This branch adds AST lookup indexes and teaches `org-todoist--get-by-id` to use them safely.

Before this change, hot paths repeatedly scanned large subtrees with `org-element-map` just to find the same projects, sections, tasks, and temp ids again. That is acceptable on small files but scales poorly with large task sets.

What changes:

- Adds per-AST indexes for:
  - id
  - type+id
  - temp id
  - project nodes
  - section nodes
- `org-todoist--get-by-id` uses those indexes when available, but still falls back to a scan when necessary.
- Indexed lookups are validated against the requested subtree so duplicate ids like default sections remain subtree-safe.
- Newly created or updated nodes are tracked back into the active index.
- `org-todoist--temp-id-mapping`, `org-todoist--project-nodes`, and `org-todoist--section-nodes` use the cache as well.

Performance contribution:

- This branch removes a large class of repeated O(n) subtree scans.
- It is a structural prerequisite for the later response-path speedups and materially reduces the cost of `org-todoist--get-by-id` on large files.
- In the later live traces after the surrounding lookup work was in place, `org-todoist--get-by-id` dropped to about `0.985s` on the profiled workload. That number is not a perfectly isolated measurement for this branch alone, but it is strong evidence that the repeated-lookup problem was real and worth fixing.
