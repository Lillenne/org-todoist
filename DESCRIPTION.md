This branch contains one correctness fix: it delays writing the new Todoist sync token until after the local Org snapshot has been saved and the on-disk Org file update has succeeded.

Before this change, `org-todoist--parse-response` advanced the sync token before the local snapshot and buffer update. If Emacs was interrupted after the token write but before the file update completed, the next incremental sync could start from a token that no longer matched the visible local Org state. In practice that can cause missed remote changes and confusing recovery behavior.

What changes:

- `org-todoist--set-sync-token` moves to the end of `org-todoist--parse-response`.
- A short comment explains why the ordering matters.

Performance impact:

- No meaningful direct speedup was measured from this patch.
- Its value is correctness and crash-interruption safety, not runtime reduction.
