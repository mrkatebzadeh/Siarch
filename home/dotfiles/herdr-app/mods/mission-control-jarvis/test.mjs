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

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { validateTask, waitStatus, worktreeTarget, workerArgs, workerTerminalArgs, workerSystemPrompt } from "./planner-tools.mjs";

const source = readFileSync(new URL("./main.js", import.meta.url), "utf8");
assert.ok(/import \{[^}]*\bTextarea\b[^}]*\} from "gpui-base"/.test(source), "Textarea must come from gpui-base");
assert.ok(/import \{[^}]*\bTextareaState\b[^}]*\} from "gpui-base"/.test(source), "TextareaState must come from gpui-base");
assert.match(source, /const PLANNER_TOOLS = "read,grep,find,delegate_execution"/);
assert.match(source, /--tools", PLANNER_TOOLS/);
assert.match(source, /planner-extension\.ts/);
assert.match(source, /\["send", address, prompt\]/);
assert.match(source, /plannerSection\(/);
const plannerStatusFrom = Function(`${source.match(/const plannerStatusFrom = \([\s\S]*?^};/m)?.[0]}; return plannerStatusFrom;`)();
const plannerOutputFrom = Function(`${source.match(/const plannerOutputFrom = \([\s\S]*?^};/m)?.[0]}; return plannerOutputFrom;`)();
assert.equal(plannerOutputFrom({ output: "new response" }, "last good response"), "new response");
assert.equal(plannerOutputFrom({ text: "new text" }, "last good response"), "new text");
assert.equal(plannerOutputFrom({}, "last good response"), "last good response");
assert.equal(plannerStatusFrom({ status: "starting" }), "starting");
assert.equal(plannerStatusFrom({ status: "working" }), "working");
assert.equal(plannerStatusFrom({ status: "error" }), "error");
assert.equal(plannerStatusFrom({ status: "idle" }), "idle");
const plannerSection = source.match(/  plannerSection\(\) \{[\s\S]*?\n  \}/)?.[0] || "";
assert.match(source, /Return only a concise, human-readable result or status/);
assert.doesNotMatch(source, /Ask JARVIS to plan or delegate work/);
assert.doesNotMatch(source, /exec\("hc", \["read"/);
assert.match(plannerSection, /plannerWorker\.status/);
assert.doesNotMatch(plannerSection, /plannerWorker\.output/);
assert.match(plannerSection, /run\("terminal\.peek", \{ terminal: address \}\)/);
assert.doesNotMatch(plannerSection, /run\("agent\.focus", \{ terminal: address \}\)/);
assert.match(plannerSection, /\.child\("OPEN"\)/);

const agentsSection = source.match(/  agentsSection\(s\) \{[\s\S]*?\n  \}/)?.[0] || "";
assert.match(agentsSection, /const collapsed = Boolean\(this\.prefs\(\)\.agentsCollapsed\)/);
assert.match(agentsSection, /this\.setPrefs\(\{ agentsCollapsed: !collapsed \}\)/);
assert.match(agentsSection, /collapsed \? "SHOW AGENTS" : "HIDE AGENTS"/);
assert.match(agentsSection, /v_flex\(\)\.gap\(8\)\.child\(bar\)\.child\(legend\)\.child\(toggle\)/);
assert.match(agentsSection, /\.when\(!collapsed/);

assert.equal(validateTask("  Fix the failing test  "), "Fix the failing test");
assert.throws(() => validateTask(" \n "), /empty/i);
assert.throws(() => validateTask(null), /string/i);
assert.throws(() => validateTask("x".repeat(20_001)), /20,000/);
assert.equal(waitStatus({ status: "working" }), "working");
assert.equal(waitStatus({ running: true }), "working");
assert.equal(waitStatus({ running: false }), "completed");
assert.equal(waitStatus({}), "unknown");
const target = worktreeTarget("/repo/project", "../../ escape / task", "abc123");
assert.match(target.branch, /^jarvis\/[a-z0-9-]+-abc123$/);
assert.equal(target.path, "/repo/.herdr-worktrees/" + target.name);
assert.match(target.name, /^jarvis-[a-z0-9-]+-abc123$/);
assert.equal(target.terminalName, "jarvis-worker-abc123");
assert.ok(target.name.length <= 32);
assert.equal(target.path.includes(".."), false);
const task = "Fix the failing test";
const workerPrompt = "Make the requested local changes only.";
assert.match(workerSystemPrompt, /Do not commit, push, deploy, or change external tracker state/);
assert.match(workerSystemPrompt, /unless the user's delegated task explicitly requests that specific action/);
assert.deepEqual(workerArgs(task, workerPrompt), [
  "--tools", "read,grep,find,bash,edit,write",
  "--append-system-prompt", workerPrompt,
  "--", task,
]);
const terminalArgs = workerTerminalArgs(target.terminalName, target.path, task, workerPrompt);
assert.deepEqual(terminalArgs.slice(0, 6), ["run", "--bg", "--outlive-parent", "--json", "--name", "jarvis-worker-abc123"]);
assert.equal(terminalArgs[terminalArgs.indexOf("--cwd") + 1], target.path);
assert.deepEqual(terminalArgs.slice(terminalArgs.indexOf("--") + 1, terminalArgs.indexOf("--") + 3), ["pi", "--tools"]);
assert.match(terminalArgs.join(" "), /read,grep,find,bash,edit,write/);
assert.equal(terminalArgs.at(-1), task);
const policyArgs = workerTerminalArgs(target.terminalName, target.path, task, workerSystemPrompt);
assert.equal(policyArgs[policyArgs.indexOf("--append-system-prompt") + 1], workerSystemPrompt);
const definition = (name) => source.match(new RegExp(`const ${name} = \\([\\s\\S]*?^};`, "m"))?.[0];

class Builder {
  static stroke() { return new Builder(); }
  cubic_bezier_to(...point) { (this.points ||= []).push(point); return this; }
  move_to(...point) { this.start = point; return this; }
}

const circlePath = Function("PathBuilder", `${definition("circlePath")}; return circlePath;`)(Builder);
const circle = circlePath(82, 32, 5);
assert.deepEqual(circle.points.map((point) => point.slice(0, 2)), [[73, 41], [41, 73], [9, 41], [41, 9]]);

const parseSystemUsage = Function(`${definition("parseSystemUsage")}; return parseSystemUsage;`)();
assert.deepEqual(parseSystemUsage("CPU usage: 7.7% user, 12.92% sys, 80.0% idle\nPhysMem: 7403M used (1375M wired, 2821M compressor), 231M unused", "Darwin"), { cpu: 20, memory: 97 });

const quotaColorSource = source.match(/const quotaColor = \(remaining\) => .*;/)?.[0];
const quotaColor = Function("C", `${quotaColorSource}; return quotaColor;`)({ red: "red", gold: "yellow", cyan: "green" });
assert.equal(quotaColor(19.99), "red");
assert.equal(quotaColor(20), "yellow");
assert.equal(quotaColor(80), "yellow");
assert.equal(quotaColor(80.01), "green");

const gauge = source.match(/  resourceGauge\([\s\S]*?\n  \}/)?.[0] || "";
assert.match(gauge, /asset\("ring-dash"\).*spin\(24000\)/);
assert.match(gauge, /asset\("ring-dots-counter"\).*spin\(24000\)/);
assert.doesNotMatch(gauge, /orbitFrames|this\.track\(size, radius/);
const counterRing = readFileSync(new URL("./assets/ring-dots-counter.svg", import.meta.url), "utf8");
assert.match(counterRing, /<circle cx="100" cy="100" r="96"[^>]*stroke-width="3" stroke-linecap="round" stroke-dasharray="2 8"/);
assert.match(counterRing, /scale\(-1 1\)/);
assert.match(counterRing, /cx="196" cy="100"/);
assert.match(source, /return this\.section\("sec-activity", "ChatGPT"/);
