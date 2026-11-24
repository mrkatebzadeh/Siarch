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

import { basename, dirname, join } from "node:path";

const slug = (value, maxLength = 48) => value.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "").slice(0, maxLength).replace(/-+$/g, "") || "task";
const safeId = (id) => {
  const value = String(id).toLowerCase().replace(/[^a-z0-9-]+/g, "-").replace(/^-+|-+$/g, "").slice(0, 8);
  if (!value) throw new Error("id must contain letters or numbers");
  return value;
};

export const workerSystemPrompt = "Make the requested local changes only. Do not commit, push, deploy, or change external tracker state unless the user's delegated task explicitly requests that specific action.";

export function validateTask(task) {
  if (typeof task !== "string") throw new TypeError("task must be a string");
  const text = task.trim();
  if (!text) throw new Error("task must not be empty");
  if (text.length > 20_000) throw new Error("task must be at most 20,000 characters");
  return text;
}

export function waitStatus(result) {
  if (typeof result?.status === "string") return result.status;
  if (typeof result?.running === "boolean") return result.running ? "working" : "completed";
  return "unknown";
}

export function worktreeTarget(repoRoot, task, id) {
  const shortId = safeId(id);
  const name = `jarvis-${slug(validateTask(task), 16)}-${shortId}`;
  const parent = dirname(repoRoot);
  return { branch: `jarvis/${slug(validateTask(task))}-${shortId}`, path: join(parent, ".herdr-worktrees", name), name, terminalName: `jarvis-worker-${shortId}` };
}

export function workerArgs(task, workerPrompt) {
  return ["--tools", "read,grep,find,bash,edit,write", "--append-system-prompt", workerPrompt, "--", validateTask(task)];
}

export function workerTerminalArgs(name, cwd, task, workerPrompt) {
  return ["run", "--bg", "--outlive-parent", "--json", "--name", basename(name), "--cwd", cwd, "--", "pi", ...workerArgs(task, workerPrompt)];
}
