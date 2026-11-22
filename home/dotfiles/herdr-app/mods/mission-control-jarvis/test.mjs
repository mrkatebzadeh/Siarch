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

const source = readFileSync(new URL("./main.js", import.meta.url), "utf8");
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
assert.deepEqual(parseSystemUsage("CPU usage: 7.7% user, 12.92% sys, 80.0% idle\nPhysMem: 7403M used (1375M wired, 2821M compressor), 231M unused"), { cpu: 20, memory: 97 });

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
