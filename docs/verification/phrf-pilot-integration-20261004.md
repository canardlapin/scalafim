# PHRF branch recovery and canonical-main integration

Mote: `bd-01M41SAG4MA1DEZ2V6GQCAD89Z`.

## Candidate scope

The integration prerequisites and origin reconciliation were already present on
canonical main at `3f261adde7fb716cb997e290516fa49379121c81`. This candidate
starts from that revision, plus the CI-resolution receipt `4e3de63c`.

The retained PHRF branch is at `a18e32fb83fcb1051a06a26ae614582fa2f89689`.
Its repository has missing historical objects, so a complete merge/archive is
unavailable. Recovery uses the 313 explicitly scoped tracked files whose working
bytes match their HEAD Git blob IDs. The complete source hashes, modes, sizes,
and failed-archive boundary are recorded in
`phrf-pilot-integration-20261004/source-manifest.json`.

The recovered module is unpublished. Its portable preparation, scoring, and
runner contracts compile on JVM and JS; process custody and native GLMsingle
integration remain JVM-only. The build aggregates and bounded CI batches now
include the module. Existing `phrf-cmp-*` receipts describe historical author
qualification and are retained as source evidence, not new test results.

Three recovered implementation/test files have deliberate integration changes: `RidgeLss.scala`
rejects group IDs outside the trial count before arithmetic/allocation, its
suite covers `Int.MaxValue` and another huge sparse ID, and the Python late-ack
test captures the actual spawned PID instead of racing a child-written file.
One trailing space is removed from the retained S8 Markdown receipt.

Two current-main CI defects are repaired without changing numerical tolerances:
the twelve first-level law files reported by the PR #19 formatting gate are
formatted with the repository formatter; the JVM parallel-fit semantic matrix
has a ten-minute MUnit deadline. It retains all 2,048 voxels, chunk sizes, worker
counts, and explicit lifecycle deadlines. Hosted Java 17 coverage recorded
212.856 and 196.048 seconds for its two matrices.

## Qualification in progress

Completed before the review candidate:

- JVM and JS compilation of the recovered PHRF module and test sources.
- `RidgeLssSuite` and `TrialHeldOutPredictionSuite`: 15 passed on each platform.
- First-level law formatting check, test-inventory check, scenario-manifest
  validation, and first-level documentation check.
- GLMsingle converter: 13 synthetic-fixture tests passed, including real native
  recomputation and deterministic archive checks.
- Python custody: 193 passed, three skipped because the initial environment
  lacked `py-ecc`. A fresh environment from the retained custody lock is being
  checked to cover those BLS cases.

Full module gates and independent review are pending at this candidate. Logs
and actual process exit metadata are retained under
`/private/tmp/scalafim-mote-queue-evidence`.

One initial warm-server reload exhausted its three-GB heap while retaining two
large builds. That owned process was stopped before tests ran. Subsequent gates
use a fresh five-GB server; the cancelled reload is environment evidence, not
a numerical test failure.

## Explicit landing boundary

This work lands the retained S0–S9/S11 implementation and prerequisites. S10
(runner entry point, final `ScoreFeed` wiring, owner reader, freeze and custody
rehearsal) follows branch landing in the coordinator's accepted plan. No sealed
pilot outputs were inspected, no pilot was run, and no scientific pilot or
release qualification is claimed. No push is authorized.
