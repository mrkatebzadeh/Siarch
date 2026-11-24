# JARVIS Planner Output Cleanup Design

## Goal

Replace raw Pi terminal chatter in the JARVIS planner panel with a concise human-readable result or status.

## Design

- Keep the planner input, but remove the instructional `Ask JARVIS to plan or delegate work…` placeholder.
- Ask the planner to return a concise final result/status and not expose tool logs or internal progress.
- Stop rendering `hc read` terminal buffers for the planner and worker. Use structured terminal state fields (`output`/`text`) only when available; otherwise show a short status or error, never raw terminal text.
- Keep the latest worker's status and focus action, but remove its terminal transcript.
- Preserve the last useful result while a refresh has no new result.

## Alternatives

- Truncate or regex-filter the terminal buffer. Rejected because Pi's TUI and tool output formats are unstable.
- Hide all result text and show status only. Simpler, but discards useful completion summaries.
- Prefer structured result/status and fall back to a concise status. Recommended: readable output without coupling the UI to terminal rendering.

## Validation

- Add assertions that raw `hc read` output is ignored, structured result text is retained, and empty output preserves the last useful result.
- Verify the planner section no longer renders worker transcripts or the placeholder.
- Run `node test.mjs`, syntax checks, and `git diff --check`.
