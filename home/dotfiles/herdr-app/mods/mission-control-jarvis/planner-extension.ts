// Copyright (C) 2026 M.R. Siavash Katebzadeh <mr@katebzadeh.xyz>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

import { spawn } from "node:child_process";
import { mkdir } from "node:fs/promises";
import { randomUUID } from "node:crypto";
import { join } from "node:path";
import { Type } from "@sinclair/typebox";
import { validateTask, waitStatus, worktreeTarget, workerTerminalArgs, workerSystemPrompt } from "./planner-tools.mjs";

function run(command: string, args: string[], cwd?: string): Promise<{ stdout: string; stderr: string }> {
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, { cwd, shell: false, stdio: ["ignore", "pipe", "pipe"] });
    let stdout = "";
    let stderr = "";
    child.stdout.setEncoding("utf8").on("data", (chunk) => { stdout += chunk; });
    child.stderr.setEncoding("utf8").on("data", (chunk) => { stderr += chunk; });
    child.once("error", reject);
    child.once("close", (code) => code === 0
      ? resolve({ stdout, stderr })
      : reject(new Error(`${command} exited ${code}: ${stderr || stdout}`)));
  });
}

function parseJson(text: string): any {
  try { return JSON.parse(text.trim()); } catch { return {}; }
}

export default function register(pi: any) {
  pi.registerTool({
    name: "delegate_execution",
    label: "Delegate execution",
    description: "Run requested edits in an isolated Git worktree with a separate worker.",
    parameters: Type.Object({ task: Type.String() }),
    async execute(_toolCallId: string, params: { task: string }, _signal: AbortSignal, _onUpdate: (update: any) => void, ctx: { cwd: string }) {
      try {
        const task = validateTask(params.task);
        const { stdout } = await run("git", ["rev-parse", "--show-toplevel"], ctx.cwd);
        const repoRoot = stdout.trim();
        if (!repoRoot) throw new Error("Git did not return a repository root");

        const id = randomUUID().replaceAll("-", "").slice(0, 8);
        const target = worktreeTarget(repoRoot, task, id);
        await mkdir(join(repoRoot, "..", ".herdr-worktrees"), { recursive: true });
        await run("git", ["worktree", "add", "-b", target.branch, target.path, "HEAD"], repoRoot);

        const launched = await run("hc", workerTerminalArgs(target.terminalName, target.path, task, workerSystemPrompt));
        const launchData = parseJson(launched.stdout);
        const terminal = launchData.terminal ?? launchData;
        const address = terminal.address ?? terminal.id ?? launchData.address ?? target.name;
        let status = "working";
        let output = launched.stdout.trim();
        try {
          const waited = await run("hc", ["wait", "--json", "--timeout", "300", String(address)]);
          const result = parseJson(waited.stdout);
          status = waitStatus(result);
          output = result.output ?? result.text ?? waited.stdout.trim() ?? output;
        } catch (error) {
          output = `${output}${output ? "\n" : ""}${String(error)}`;
          status = "working";
        }
        return { content: [{ type: "text", text: JSON.stringify({ address, status, output, worktree: target.path, branch: target.branch }) }] };
      } catch (error) {
        return { content: [{ type: "text", text: `Worker delegation failed: ${String(error)}` }], isError: true };
      }
    },
  });
}
