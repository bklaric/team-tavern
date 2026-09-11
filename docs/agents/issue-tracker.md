# Issue tracker: Local Markdown

Issues and specs for this repo live as markdown files in `specs/`.

## Conventions

- One feature per directory: `specs/<feature-slug>/`
- The spec is `specs/<feature-slug>/spec.md`
- Implementation issues are one file per ticket at `specs/<feature-slug>/issues/<NN>-<slug>.md`, numbered from `01`, never a single combined tickets file
- Every issue carries a `Status:` line near the top holding exactly one state from the lifecycle below
- Comments and conversation history append to the bottom of the file under a `## Comments` heading

## Issue lifecycle

There is no separate open/closed flag: the `Status:` line carries triage state and closure together, and an issue is open until it reads a terminal state.

| State                                                              | Meaning                                      |
| ------------------------------------------------------------------ | -------------------------------------------- |
| `needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`  | Open. The triage roles in `triage-labels.md` |
| `resolved`                                                          | Closed: the change landed and was verified   |
| `wontfix`                                                           | Closed: will not be actioned                 |

## When a skill says "publish to the issue tracker"

Create a new file under `specs/<feature-slug>/` (creating the directory if needed).

## When a skill says "fetch the relevant ticket"

Read the file at the referenced path. The user will normally pass the path or the issue number directly.

## When a skill says "close the issue"

1. Set `Status: resolved`, replacing whatever triage role was there. Use `wontfix` when the ticket is being dropped rather than done.
2. Append the outcome under `## Comments` at the bottom: what landed and how it was verified. For a `/wayfinder` question ticket, the answer goes under `## Answer` instead.
3. Leave the file where it is. A closed ticket stays in `specs/<feature-slug>/issues/` so its number keeps resolving; `git log` records when it closed and why.
4. Strike the ticket's number from every `Blocked by:` line that names it, deleting the line once it empties. `wontfix` does not unblock anything — re-read a dependent ticket by hand and decide whether it still stands.

## Wayfinding operations

Used by `/wayfinder`. The **map** is a file with one **child** file per ticket.

- **Map**: `specs/<effort>/map.md` (the Notes / Decisions-so-far / Fog body).
- **Child ticket**: `specs/<effort>/issues/NN-<slug>.md`, numbered from `01`, with the question in the body. A `Type:` line records the ticket type (`research`/`prototype`/`grilling`/`task`); the `Status:` line uses the same lifecycle as every other issue.
- **Blocking**: a `Blocked by: NN, NN` line near the top. A ticket is unblocked when every file it lists is `resolved`.
- **Frontier**: scan `specs/<effort>/issues/` for files that are open and unblocked; first by number wins.
- **Resolve**: append the answer under an `## Answer` heading, close the ticket as above, then append a context pointer (gist + link) to the map's Decisions-so-far in `map.md`.
