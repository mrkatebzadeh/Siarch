# Copyright (C) 2026 M.R. Siavash Katebzadeh <mr@katebzadeh.xyz>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

"""Run with python3; tests real Helix/Texlab without opening a PDF viewer."""

import fcntl
import json
import os
from pathlib import Path
import pty
import re
import select
import signal
import struct
import sys
import tempfile
import termios
import time
import tomllib


def check():
    root = Path(__file__).resolve().parent
    settings = tomllib.loads((root / "languages.toml").read_text())
    texlab = settings["language-server"]["texlab"]["config"]["texlab"]
    assert "forwardSearch" in texlab, "Texlab PDF preview is not configured"
    preview = texlab["forwardSearch"]
    assert preview["executable"] == "setsid", "Detach before Zathura initializes; its own --fork hangs here"
    assert preview["args"] == ["--fork", "zathura", "--synctex-forward", "%l:1:%f", "%p"]
    assert texlab["build"]["onSave"] and not texlab["build"]["forwardSearchAfter"]

    with tempfile.TemporaryDirectory(prefix=".latex-keycheck-", dir=root) as directory:
        work = Path(directory)
        config = work / "config" / "helix"
        config.mkdir(parents=True)
        for name in ("config.toml", "helix.scm", "init.scm", "runtime"):
            (config / name).symlink_to(root / name)
        build = texlab["build"]
        (config / "languages.toml").write_text(
            "[language-server.texlab.config.texlab.build]\n"
            f"executable = {json.dumps(build['executable'])}\n"
            f"args = {json.dumps(build['args'])}\n"
            "onSave = true\nforwardSearchAfter = false\n"
            "[language-server.texlab.config.texlab.forwardSearch]\n"
            f"executable = {json.dumps(sys.executable)}\n"
            f"args = {json.dumps([str(Path(__file__).resolve()), '--viewer', *preview['args']])}\n"
        )
        project = work / "project α with spaces"
        project.mkdir()
        (project / ".texlabroot").touch()
        (project / "main.tex").write_text(
            "\\documentclass{article}\n\\begin{document}\n"
            "\\input{chapter}\n\\end{document}\n"
        )
        (project / "chapter.tex").write_text(
            "% Delete this whole line.\n% Unicode cursor: café λ 😀\nLaTeX-local keys work.\n"
        )
        (project / "notes.txt").write_text("first line\nsecond line\n")
        viewer_log = work / "viewer.jsonl"
        env = dict(os.environ, TERM="xterm-256color", XDG_CONFIG_HOME=str(work / "config"),
                   LATEX_PREVIEW_LOG=str(viewer_log))
        env.pop("CARGO_MANIFEST_DIR", None)
        pid, fd = pty.fork()
        if pid == 0:
            os.chdir(project)
            os.execvpe("hx", ["hx", "--log", str(work / "helix.log"), "main.tex"], env)
        fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 40, 140, 0, 0))
        # ponytail: local timing budget; poll LSP/build readiness if slower hosts need it.
        actions = [
            (2, b":open chapter.tex\r"),
            (3, b"ggdd"),
            (3.5, b"gg$"),
            (4, b" kv"),
            (5.5, b" kc"),
            (9.5, b" kv"),
            (11, b" fe"),
            (12, b" "),
            (13, b"\x1b"),
            (13.7, b"q"),
            (14.5, b":open notes.txt\r"),
            (16, b" kv"),
            (17, b"\x1b"),
            (17.7, b"ggdd"),
            (18.5, b":write\r"),
            (20, b":q!\r"),
        ]
        output = bytearray()
        deadline = time.monotonic() + 40
        start, stage, status = None, 0, None
        try:
            while time.monotonic() < deadline:
                if select.select([fd], [], [], 0.1)[0]:
                    try:
                        chunk = os.read(fd, 65536)
                    except OSError:
                        break
                    if not chunk:
                        break
                    output.extend(chunk)
                    if b"error[E" in output:
                        break
                    if start is None and b"Loaded 1 file." in output:
                        start = time.monotonic()
                    if b"\x1b[6n" in chunk:
                        os.write(fd, b"\x1b[1;1R")
                    if b"\x1b[c" in chunk:
                        os.write(fd, b"\x1b[?1;2c")
                if start is not None and stage < len(actions) and time.monotonic() - start > actions[stage][0]:
                    os.write(fd, actions[stage][1])
                    stage += 1
                done, child_status = os.waitpid(pid, os.WNOHANG)
                if done:
                    status = child_status
                    break
        finally:
            if status is None:
                done, status = os.waitpid(pid, os.WNOHANG)
                if not done:
                    os.kill(pid, signal.SIGTERM)
                    _, status = os.waitpid(pid, 0)
            os.close(fd)
        text = re.sub(r"\x1b\[[0-?]*[ -/]*[@-~]", "", output.decode(errors="replace"))
        text = re.sub(" +", " ", text)
        assert "error[E" not in text, text
        assert os.waitstatus_to_exitcode(status) == 0, text
        assert "Compile LaTeX first" in text, text
        assert (project / "main.pdf").read_bytes().startswith(b"%PDF-"), text
        assert not (project / "chapter.pdf").exists()
        assert (project / "chapter.tex").read_text() == "% Unicode cursor: café λ 😀\nLaTeX-local keys work.\n", text
        assert viewer_log.exists(), text
        calls = [json.loads(line) for line in viewer_log.read_text().splitlines()]
        assert len(calls) == 1, calls
        assert calls[0][:3] == ["--fork", "zathura", "--synctex-forward"], calls
        assert calls[0][-2].endswith(":" + str(project / "chapter.tex")), calls
        assert calls[0][-1] == str(project / "main.pdf"), calls
        assert "needs a LaTeX document" not in text, text
        assert "Fuzzy search" in text, text
        assert (project / "notes.txt").read_text() == "second line\n", text
    print("LaTeX-local build/preview, master resolution, URI encoding, errors, Vim, and Forest passed")


if __name__ == "__main__":
    if sys.argv[1:2] == ["--viewer"]:
        with open(os.environ["LATEX_PREVIEW_LOG"], "a") as log:
            log.write(json.dumps(sys.argv[2:]) + "\n")
    else:
        check()
