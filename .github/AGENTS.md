# AGENTS.md (.github/)

`workflows/ci.yml` runs the infra `ox-setup` action with `parquet` pinned, builds, tests
(the cram test uses the runner's `jq`), builds odoc with warnings failing, and calls the
shared `obj-magic` workflow.

- The pins must match the org dependencies in `../dune-project`.
- Check a run with `gh run list --limit 1`; at most one `gh run watch <id> --exit-status`.

See `../AGENTS.md`.
