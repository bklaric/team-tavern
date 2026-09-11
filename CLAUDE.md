# TeamTavern

## Writing comments and docs in this repo

- **Present tense only.** Comments and docs describe what the code does now. No
  "this previously imported X", no "changed when we upgraded to Y", no "added in
  May 2026", no "used to be a workaround for Z". Version numbers, dates, and the
  reason a line changed belong in the git log, not in the source.
- **Explain the non-obvious present, not the past.** A comment earns its place by
  saying something the code cannot: an invariant, a constraint imposed from
  outside, a reason the obvious approach does not work here. If the sentence only
  makes sense to someone who remembers the previous version, delete it.
- **Don't leave a comment where a name will do.** Prefer renaming the binding or
  extracting the expression over narrating it.
- **Don't document degraded behavior.** If a line only makes sense in conjunction
  with a bug or an upstream layer misbehaving, it is describing a regression to
  fix, not a contract to publish.
- **No changelog sections.** Nothing in this repo needs a "Recent changes" or
  "Migration notes" heading. `git log` is the changelog.
