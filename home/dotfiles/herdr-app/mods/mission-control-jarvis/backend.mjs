// J.A.R.V.I.S. data backend: one JSON snapshot of everything you're working on.
// Reads Mission Control's own config (connections + per-repo bindings), never writes it.
// Usage: node backend.mjs <appHome>
import { readFileSync, writeFileSync, mkdirSync, statSync, openSync, readSync, closeSync, readdirSync, renameSync } from "node:fs";
import { execFile, spawn } from "node:child_process";
import { join } from "node:path";
import { homedir } from "node:os";

const appHome = process.argv[2] || join(homedir(), ".config/herdr-app");
const mcDir = join(appHome, "mission-control");
const cacheDir = join(mcDir, "jarvis");

const run = (cmd, args, timeout = 30000) => new Promise((resolve) => {
  execFile(cmd, args, { timeout, maxBuffer: 16 * 1024 * 1024 }, (err, stdout, stderr) =>
    resolve({ ok: !err, stdout: String(stdout || ""), stderr: String(stderr || err?.message || "") }));
});

function readConfig() {
  try { return JSON.parse(readFileSync(join(mcDir, "config.json"), "utf8")); } catch { return { connections: {}, bindings: {} }; }
}

async function jira(config) {
  const bindings = Object.values(config.bindings || {}).filter((b) => b.jira?.connectionId);
  if (!bindings.length) return { ok: false, error: "No Jira bindings", issues: [] };
  const conn = config.connections?.[bindings[0].jira.connectionId];
  if (!conn?.site) return { ok: false, error: "Jira connection missing", issues: [] };
  const user = process.env[conn.auth?.usernameEnv || ""], token = process.env[conn.auth?.tokenEnv || ""];
  if (!token) return { ok: false, error: "Jira token not available", issues: [] };
  const projects = [...new Set(bindings.map((b) => b.jira.project).filter(Boolean))];
  const jql = `assignee = currentUser() AND project in (${projects.map((p) => `"${p}"`).join(",")}) AND (statusCategory != Done OR updated >= -7d) ORDER BY updated DESC`;
  const headers = { Accept: "application/json", "Content-Type": "application/json",
    Authorization: conn.auth?.kind === "bearer" ? `Bearer ${token}` : `Basic ${Buffer.from(`${user}:${token}`).toString("base64")}` };
  const issues = [];
  let nextPageToken;
  try {
    for (let page = 0; page < 3; page++) {
      const res = await fetch(new URL("/rest/api/3/search/jql", conn.site), { method: "POST", headers,
        body: JSON.stringify({ jql, maxResults: 100, nextPageToken, fields: ["summary", "status", "project", "priority", "updated", "issuetype"] }),
        signal: AbortSignal.timeout(20000) });
      if (!res.ok) return { ok: false, error: `Jira HTTP ${res.status}`, issues };
      const body = await res.json();
      for (const i of body.issues || []) {
        const f = i.fields || {};
        issues.push({ key: i.key, summary: f.summary || "", status: f.status?.name || "", cat: f.status?.statusCategory?.key || "new",
          project: f.project?.key || "", priority: f.priority?.name || "", type: f.issuetype?.name || "",
          updated: f.updated || null, url: `${conn.site.replace(/\/$/, "")}/browse/${i.key}` });
      }
      if (!body.nextPageToken || body.isLast) break;
      nextPageToken = body.nextPageToken;
    }
  } catch (e) { return { ok: false, error: `Jira: ${e.message}`, issues }; }
  return { ok: true, projects, issues };
}

const PR_FIELDS = `... on PullRequest { number title url body isDraft updatedAt reviewDecision repository { nameWithOwner }
  headRefName headRefOid baseRefName baseRefOid headRepository { nameWithOwner }
  reviewThreads(first: 100) { nodes { isResolved } }
  commits(last: 1) { nodes { commit { statusCheckRollup { state } } } } }`;

async function github() {
  const q = (search) => `search(query: "${search}", type: ISSUE, first: 40) { nodes { ${PR_FIELDS} } }`;
  const query = `query { mine: ${q("is:pr is:open author:@me archived:false")} reviews: ${q("is:pr is:open review-requested:@me archived:false")} }`;
  const r = await run("gh", ["api", "graphql", "-f", `query=${query}`], 30000);
  if (!r.ok) return { ok: false, error: `GitHub: ${r.stderr.trim().split("\n")[0]}`, mine: [], reviews: [] };
  let data;
  try { data = JSON.parse(r.stdout).data; } catch { return { ok: false, error: "GitHub: bad response", mine: [], reviews: [] }; }
  const map = (n) => ({ repo: n.repository?.nameWithOwner || "", number: n.number, title: n.title, url: n.url, draft: !!n.isDraft,
    updated: n.updatedAt, review: n.reviewDecision || null, ci: n.commits?.nodes?.[0]?.commit?.statusCheckRollup?.state || null,
    body: String(n.body || "").slice(0, 6000), headRef: n.headRefName || null, headSha: n.headRefOid || null,
    baseRef: n.baseRefName || null, baseSha: n.baseRefOid || null, headRepo: n.headRepository?.nameWithOwner || null,
    unresolved: (n.reviewThreads?.nodes || []).filter((t) => t && !t.isResolved).length });
  return { ok: true, mine: (data?.mine?.nodes || []).filter((n) => n?.number).map(map), reviews: (data?.reviews?.nodes || []).filter((n) => n?.number).map(map) };
}

async function codexRateLimits() {
  return new Promise((resolve) => {
    let settled = false, buffer = "", requested = false;
    const child = spawn("codex", ["app-server"], { stdio: ["pipe", "pipe", "ignore"] });
    const finish = (value) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      child.kill();
      resolve(value);
    };
    const timer = setTimeout(() => finish(null), 8000);
    child.on("error", () => finish(null));
    child.stdin.on("error", () => finish(null));
    child.stdout.setEncoding("utf8");
    child.stdout.on("data", (chunk) => {
      buffer += chunk;
      let end;
      while ((end = buffer.indexOf("\n")) >= 0) {
        const line = buffer.slice(0, end);
        buffer = buffer.slice(end + 1);
        let message;
        try { message = JSON.parse(line); } catch { continue; }
        if (message.id === 1) {
          if (!message.result || requested) { finish(null); continue; }
          requested = true;
          child.stdin.write(JSON.stringify({ method: "initialized", params: {} }) + "\n");
          child.stdin.write(JSON.stringify({ id: 2, method: "account/rateLimits/read", params: {} }) + "\n");
        } else if (message.id === 2) {
          const limits = message.result?.rateLimits;
          finish(limits ? { primary: limits.primary || null, secondary: limits.secondary || null } : null);
        }
      }
    });
    child.stdin.write(JSON.stringify({ id: 1, method: "initialize", params: {
      clientInfo: { name: "herdr-jarvis", version: "1.0.0" }, capabilities: {},
    } }) + "\n");
  });
}

// Codex activity per day and project, read incrementally from ~/.codex/sessions.
const DAY = 86400000;
const localDay = (ms) => { const d = new Date(ms); return `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-${String(d.getDate()).padStart(2, "0")}`; };

function activity() {
  const root = join(homedir(), ".codex/sessions");
  const cachePath = join(cacheDir, "codex-activity-cache.json");
  let cache = { v: 1, files: {} };
  try { cache = JSON.parse(readFileSync(cachePath, "utf8")); if (cache.v !== 1) cache = { v: 1, files: {} }; } catch {}
  const since = Date.now() - 8 * DAY;
  const seen = new Set();
  const files = [];
  const walk = (dir) => {
    let entries = [];
    try { entries = readdirSync(dir, { withFileTypes: true }); } catch { return; }
    for (const entry of entries) {
      const path = join(dir, entry.name);
      if (entry.isDirectory()) walk(path);
      else if (entry.isFile() && entry.name.endsWith(".jsonl")) files.push(path);
    }
  };
  walk(root);
  const buf = Buffer.alloc(4 * 1024 * 1024);
  for (const path of files) {
    let st; try { st = statSync(path); } catch { continue; }
    if (st.mtimeMs < since) continue;
    seen.add(path);
    let entry = cache.files[path];
    if (!entry || st.size < entry.offset) entry = { offset: 0, days: {}, cwd: null };
    if (st.size > entry.offset) {
      const fd = openSync(path, "r");
      let pos = entry.offset, carry = "";
      try {
        while (pos < st.size) {
          const n = readSync(fd, buf, 0, Math.min(buf.length, st.size - pos), pos);
          if (n <= 0) break;
          pos += n;
          const text = carry + buf.toString("utf8", 0, n);
          const lines = text.split("\n");
          carry = lines.pop();
          for (const line of lines) {
            let record; try { record = JSON.parse(line); } catch { continue; }
            if (record.type === "session_meta") entry.cwd = record.payload?.cwd || entry.cwd;
            if (record.type !== "event_msg" || record.payload?.type !== "token_count") continue;
            const t = Date.parse(record.timestamp);
            if (!(t >= since)) continue;
            const out = Number(record.payload.info?.last_token_usage?.output_tokens || 0);
            if (!(out > 0)) continue;
            const day = entry.days[localDay(t)] ||= { m: 0, t: 0 };
            day.m++; day.t += out;
          }
        }
      } finally { closeSync(fd); }
      entry.offset = pos - Buffer.byteLength(carry, "utf8");
    }
    cache.files[path] = entry;
  }
  for (const p of Object.keys(cache.files)) if (!seen.has(p)) delete cache.files[p];
  try { mkdirSync(cacheDir, { recursive: true }); writeFileSync(cachePath + ".tmp", JSON.stringify(cache)); renameSync(cachePath + ".tmp", cachePath); } catch {}

  // Map each session's working folder to its repository (worktrees resolve to their main repo).
  const repoOf = (cwd) => {
    for (let dir = cwd; dir && dir !== "/"; dir = dir.slice(0, dir.lastIndexOf("/")) || "/") {
      let st; try { st = statSync(join(dir, ".git")); } catch { continue; }
      if (st.isDirectory()) return dir;
      try {
        const gitdir = /gitdir:\s*(.+)/.exec(readFileSync(join(dir, ".git"), "utf8"))?.[1]?.trim();
        const i = gitdir?.indexOf("/.git/worktrees/") ?? -1;
        return i > 0 ? gitdir.slice(0, i) : dir;
      } catch { return dir; }
    }
    return cwd;
  };
  const days = [];
  for (let i = 6; i >= 0; i--) days.push({ date: localDay(Date.now() - i * DAY), messages: 0, tokens: 0 });
  const byDate = Object.fromEntries(days.map((d) => [d.date, d]));
  const projects = {};
  for (const e of Object.values(cache.files)) {
    const root = e.cwd ? repoOf(e.cwd) : null;
    const name = root ? root.split("/").pop() : "unknown";
    for (const [date, v] of Object.entries(e.days)) {
      const d = byDate[date]; if (!d) continue;
      d.messages += v.m; d.tokens += v.t;
      const p = projects[name] ||= { name, root, messages: 0, tokens: 0 };
      p.messages += v.m; p.tokens += v.t;
    }
  }
  return { ok: true, days, projects: Object.values(projects).sort((a, b) => b.messages - a.messages) };
}

const config = readConfig();
const [j, g, c] = await Promise.all([jira(config), github(), codexRateLimits()]);
let a;
try { a = activity(); } catch (e) { a = { ok: false, error: e.message, days: [], projects: [] }; }
const bindings = Object.values(config.bindings || {}).filter((b) => b.machine == null && b.repoRoot).map((b) => ({
  root: b.repoRoot,
  jira: b.jira?.project ? { project: b.jira.project } : null,
  github: b.github?.owner && b.github?.repo ? { repo: `${b.github.owner}/${b.github.repo}` } : null,
}));
process.stdout.write(JSON.stringify({ v: 1, at: Date.now(), bindings, jira: j, github: g, codex: c, activity: a }) + "\n");
