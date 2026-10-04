# Voxel status point lookup assessment — 2026-09-12

> **Historical record (2026-09-12).** This document describes the
> working-tree state on 2026-09-12. Its "Mixed-TR fixture correction" section
> covers work that reached main separately (`34886213`, `1eb3ffd8`), not as
> part of this change. Its verification counts and "uncommitted" statements are
> from that date. For current verification, including the fixed-effects
> allocation regression added later, see
> [`docs/verification/voxel-status-lookup-20261004.md`](../verification/voxel-status-lookup-20261004.md).

Mote: `bd-01M1WVPJ9MMPFDXAY5196E73D7`.

The finding is current. `DenseFmriFitResult.voxelStatus` materialized the full
fallback status vector for a point lookup. The private fixed-effects selector
also did so for every voxel/run pair, producing quadratic fallback-vector work.
Both point consumers now read the optional explicit status directly and default
to `Estimable`. Full-vector accessors retain their existing behavior.

The public dense lookup regression uses 4,096 voxels and 256 measured lookups,
after warm-up. JVM thread-allocation counters measured 5,042,176 bytes before
and 32,768 bytes after the repair (99.35% reduction). Its fixed 256 KiB budget
failed before and passes after; it does not use a timing threshold. Construction
and warm-up are outside the measurement. This is point-lookup allocation, not
whole-fit memory, and does not claim zero allocation or faster source-id search.

Shared tests on JVM and JS verify unknown/source voxel IDs, explicit statuses,
exclusions, malformed vectors, explicit-versus-default fixed-effects estimates
and standard errors, variance refinement, and explicit failure precedence.
The portable resource argument is removal of both per-point `Vector.fill`
materializations; the direct byte measurement is JVM-only. The full-vector
materialization API remains available intentionally.

## Mixed-TR fixture correction exposed by verification

Broader checks found seven failures caused by the old SPMG amplitude in the
mixed-TR R fixture. The fixture is regenerated using the corrected local
`fmrihrf` source, loaded explicitly before `fmridesign`; expected values come
from R design construction, `lm.fit` and full covariance pooling, not Scala
results. Run-local nuisance contrasts are now generated from the R task
covariance blocks rather than stored as untracked numeric literals in a test.

`tools/r-parity/mixed-tr-reference-lock.json` records this fixture's runtime,
package revisions/versions and hashes of the exact R and DESCRIPTION files.
The hashes include uncommitted source corrections; revision IDs are not a
clean-tree or publication claim. The shared legacy reference lock is unchanged.
Receipt finalization accepts an explicit per-fixture lock and checks source
hashes when present; corruption and existing freshness checks are tested.

## Verification

- `fitJVM/testOnly *VoxelStatus*Suite *CompactFixedEffectsSuite *FixedEffectsEstimatesSuite *InferenceSuite`: 34 passed.
- Equivalent Scala.js suites: 33 passed (allocation counter test is JVM-only).
- Receipt finalizer `--check`: passed.
- Python receipt integrity tests: 2 passed.
- Remaining direct fixture consumers (selected estimates, example and mixed-TR scenario): 11 JVM and 11 JS tests passed.
- Total: 89 Scala tests and 2 Python tests passed; `git diff --check` passed.

Tests use `-Dsbt.global.base=/private/tmp/scalafim-fruit-sbt`, with the ordinary
source dependency pins. Changes are in the working tree; no commit or
publication is claimed. Active PHRF work is outside this change.
