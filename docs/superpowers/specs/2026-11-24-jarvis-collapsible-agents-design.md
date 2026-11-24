# JARVIS Collapsible Agents List

## Goal

Let users hide or show the Agents list without losing the panel’s online count or status summary.

## Design

- Add a `HIDE AGENTS` / `SHOW AGENTS` control in the Agents panel body, above the agent rows.
- Keep the panel heading, online count, status bar, and legend visible in both states; only the agent rows and empty-state message are hidden.
- Persist the collapsed state as an `agentsCollapsed` boolean in the mod’s existing shared preferences, using `prefs()` and `setPrefs()`.
- When the preference is absent, default to expanded. Once changed, restore the user’s choice on subsequent app starts.
- Do not add new files, dependencies, or general-purpose collapse abstractions.

## Interaction

Clicking the control toggles `agentsCollapsed` and triggers a view update. The label reflects the next action: `HIDE AGENTS` while expanded and `SHOW AGENTS` while collapsed.

## Validation

Extend the existing Node test to cover the preference-backed toggle and ensure the agent rows are conditional on the collapsed state. Run `node test.mjs`, `node --check main.js`, and `git diff --check`. Confirm visually that the summary remains visible and the list can be hidden and restored.
