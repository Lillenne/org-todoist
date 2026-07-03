This branch rewrites `org-todoist--push` so it stops repeatedly rescanning the AST and recomputing headline-derived state during dirty sync.

Before this change, push generation repeatedly walked the tree and repeatedly recomputed:

- ids and parent ids
- section/project placement
- description text
- labels and inherited tags
- effort, scheduling, deadlines, comments, and assignee state

The new shape does one pre-pass with `org-todoist--collect-push-states`, producing a compact state object for each headline plus lookup tables for the old and new trees. `org-todoist--push` then diffs those states directly.

What changes:

- Adds `org-todoist--push-state-key` for stable identity, including special handling for default sections.
- Adds `org-todoist--collect-push-states` to precompute the values push generation needs exactly once per headline.
- Rewrites `org-todoist--push` to compare precomputed state instead of querying the AST repeatedly.

Measured performance impact:

- Baseline live trace `org-todoist-live-trace-20260701-093709.org`: total `24.575s`, push `11.897s`.
- After this rewrite, live trace `org-todoist-live-trace-20260701-100903.org`: total `17.117s`, push `4.257s`.

That is roughly:

- `7.64s` saved inside push generation itself.
- `7.46s` saved end-to-end on the profiled dirty-sync path.
- About a `64%` reduction in push-side wall-clock time for that workload.
