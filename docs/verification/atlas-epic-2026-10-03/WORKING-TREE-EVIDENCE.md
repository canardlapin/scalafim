# Working-tree evidence, not SHA-bound

The files in this directory were recorded on 2026-10-03 against a dirty shared
checkout: ScalaFIM `62312f5d` plus uncommitted changes, including the
typed-atlas rework (cluster C1). They do not qualify any commit.
`acceptance.json` says so itself (`consumer_state`: "uncommitted shared
checkout"). The `consumer-input-hashes*.json` files hash that tree. The
`scalafim-*.log` runs include suites, such as `atlasWorkflowsJVM`, that exist
only in C1.

The `image4s-*` logs and patch, and `int8-regression-before.log`, are provider
evidence for image4s PR #15. They concern the image4s repository, not a ScalaFIM
commit.

The SHA-bound receipt for the INT8 change on `main` is
[`../atlas-int8-followup-20261004.md`](../atlas-int8-followup-20261004.md).

The C1-only items were removed from `main` and remain byte-identical on branch
`wip/atlas-typed-20261004`:

- `docs/verification/atlas-epic-2026-10-03.md`
- `docs/plans/atlas-typed-plan-2026-09-30.md`
- `native-source-inventory.json`
- `parity-fixture-integrity.json`
