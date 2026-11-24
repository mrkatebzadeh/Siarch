# JARVIS Planner and Delegated Execution Design

## Goal

Give JARVIS a persistent Pi that answers questions and plans work, but cannot execute shell commands or edit files itself.
When a user request requires execution or edits, that Pi delegates to a separate Pi worker.

## Design

- Run a persistent, named `jarvis-planner` Pi in a Herdr Core terminal so it survives the Herdr window closing and retains its session.
- Start the planner with a read-only tool allowlist (`read`, `grep`, and `find`) plus one narrowly scoped delegation tool; do not expose shell, edit, or write tools to it.
- The delegation tool starts a separate Pi worker through JARVIS's existing worktree/agent-launch path. The worker receives the requested task and works only in its isolated worktree.
- JARVIS sends prompts to the named planner, shows its response and status, and reports delegated-worker status/results. Existing `hc` terminal messaging is the transport; do not add a separate chat service.
- Questions and planning requests stay with the planner. Requests that call for command execution or file edits are delegated to a worker. Workers do not commit, push, deploy, or change external tracker state unless the user explicitly requests it.
- Provide a way to focus the planner or worker terminal for detailed interaction. If either terminal is unavailable or delegation fails, show the error in JARVIS and keep the planner's response visible.

## Alternatives

- Give one persistent Pi full tools and rely on its prompt to avoid direct edits. Rejected because this does not enforce the planner/worker boundary.
- Start a fresh Pi for each prompt. Simpler, but loses persistent context and does not provide a dedicated always-running receiver.
- Use a persistent read-only planner with a narrow delegation capability. Recommended: preserves context and permits delegation without granting the planner general execution tools.

## Validation

- Confirm the planner can answer and inspect files but cannot invoke shell, edit, or write tools.
- Verify JARVIS can send a prompt to the named planner and display its reply and running status.
- Verify a delegated task starts a separate Pi in an isolated worktree, can edit/run commands there, and returns a result to JARVIS.
- Run the JARVIS mod check and its existing tests.
