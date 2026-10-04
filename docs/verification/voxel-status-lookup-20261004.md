# Voxel-status point lookup (cluster C4) — readiness receipt, 2026-10-04

Branch `review/c4-voxel-status-lookup-20261004`, built from the preserved
snapshot `6a08e8f2` (`wip/voxel-status-lookup-20261004`, base `53097f3f`) with
main `54d4528e` merged in (merge `74d7ed1b`, no conflicts). This receipt covers
readiness only. It is not a review, and nothing has been landed or pushed.

## Provenance

- Mote `bd-01M1WVPJ9MMPFDXAY5196E73D7`, "Avoid full fallback status-vector
  allocation for single-voxel access" (P1, closed). Agent
  `codex-mote-sequential` did the work on 2026-09-12 and left it uncommitted
  in the canonical checkout. The canonical-tree consolidation of 2026-10-04
  preserved it as `6a08e8f2`.
- Two of the five snapshot files (`FixedEffects.scala` and
  `CompactFixedEffectsSuite.scala`) have the same blobs at `04a68798`
  ("Preserve reviewed local fit corrections as execution baseline"), which is on
  `candidate/fir-reviewed-20261003` and `estimates/response-action-evidence-v1`.
  The other three files (`VoxelStatusLookupSuite`, `VoxelStatusAllocationSuite`,
  `docs/audits/voxel-status-allocation.md`) exist only on this branch.

## Change

The change touches two source lines and nothing else:

1. `FixedEffects.fixedEffectsStatus` (private): reads
   `run.voxelStatuses.fold(Estimable)(_(voxelPosition))` instead of
   `run.resolvedVoxelStatuses(voxelPosition)`.
2. `DenseFmriFitResult.voxelStatus` (public): reads
   `voxelStatuses.fold(Estimable)(_(position))` instead of
   `resolvedVoxelStatuses(position)`.

`resolvedVoxelStatuses` returns `voxelStatuses.getOrElse(Vector.fill(V)(Estimable))`.
When no explicit statuses were stored, the old code therefore built a full
V-element vector to answer one lookup. In `FixedEffects.selectVoxels` this
happened for every voxel/run pair, which is O(R·V²) allocation in total.

## Semantics

The returned values are unchanged:

- An explicit status vector is indexed directly. A missing one means
  `Estimable` at every position, which is the same default that
  `resolvedVoxelStatuses` materializes.
- The id-to-position mapping (`voxelIndices.indexOf`), the fallback to
  `fitExclusions` for excluded ids, and `None` for unknown ids are untouched.
- In fixed effects, the residual-variance refinement still applies to
  `Estimable` (non-finite gives `NonFinite`, zero gives `ZeroResidualVariance`).
  An explicit failure status still takes precedence over that refinement.
- Validation is unchanged. Constructors still reject status vectors of the
  wrong length, so `_(position)` is always in range.
- The full-vector accessor `resolvedVoxelStatuses` remains as an intentional
  materialization API.

## Relation to C3 and FIR

- **The C3 hunk has moved into C4.** The `DenseFmriFitResult.voxelStatus` line
  is the change that the allocation suite measures. The snapshot left it inside
  C3's copy of `FmriFitResult.scala` (`wip/rrg-main-port-20261004`). Without
  that line, C4 does not stand alone: `VoxelStatusAllocationSuite` fails on main
  with 5,042,176 bytes, against a budget of 262,144. This branch therefore
  carries the one-line hunk itself. It is the same text as in both C3 and
  `04a68798`. When C3 is ported, its `FmriFitResult` diff should drop this line
  (or it will merge as a no-op if C4 lands first). The rest of C3's
  `FmriFitResult` diff (`VoxelwiseReducedRankFmriFitResult` and its exclusion
  case) does not depend on C4. C3's own `voxelStatus` on the reduced-rank result
  still uses `resolvedVoxelStatuses(position)`, which is a candidate follow-up
  for C3.
- **FIR has not superseded this change.** `04a68798` is not an ancestor of
  main. Main at `54d4528e`, which includes the FIR evidence commits
  `c7ae6263..8f967b61`, still has `resolvedVoxelStatuses(position)` in both
  consumers. `git log -S` finds the replacement text only in `04a68798`, the
  C3 snapshot and this snapshot. C4 is not redundant. If the FIR candidate later
  lands `04a68798`'s fit files, these two hunks will merge identically.
- **The audit's mixed-TR section has already landed.** The second half of
  `docs/audits/voxel-status-allocation.md` describes a mixed-TR SPMG fixture
  regeneration and `tools/r-parity/mixed-tr-reference-lock.json`. That work was
  done in the same 09-12 session, but it reached main separately (for example
  `34886213` and `1eb3ffd8`). It is not part of this cluster's diff. The audit
  text is a dated historical record, and its "Verification" counts and its
  "working tree / no commit" statements describe the 09-12 state, not this
  branch.

## What the allocation audit claims and what was re-verified

The audit claims that 256 dense point lookups over 4,096 voxels, measured after
warm-up, allocated 5,042,176 bytes before the fix and 32,768 bytes after
(−99.35%). The budget is fixed at 256 KiB and there is no wall-clock threshold.
The claim covers point-lookup allocation only. It does not claim whole-fit
memory, zero allocation or faster id search.

Re-verified on this branch (JVM thread-allocation counter):

| State | Bytes (256 lookups / 4,096 voxels) | Suite |
|---|---|---|
| Dense lookup line reverted to `resolvedVoxelStatuses` | 5,042,176 | fails |
| Branch as committed | 32,768 | passes |

Both numbers match the audit exactly. The byte measurement exists only on the
JVM, in `modules/fit/jvm/src/test/...`. The JS side has the shared behavioral
suites only.

The fixed-effects selector's O(R·V²) term is covered in two ways. The first
is an equivalence test (`CompactFixedEffectsSuite`): missing and
explicit-default statuses give identical estimates and SEs within 1e-12, and
the test also checks exclusions and precedence. The second is a JVM-only
allocation regression, `FixedEffectsStatusAllocationSuite`, added after review.
It builds a two-run runwise least-squares fit over 4,096 voxels through the
public model and fit pipeline, sets every run's `voxelStatuses` to `None`, and
warms up `FixedEffects.combine` three times. It then measures one call against
a per-voxel budget of 12,288 bytes.

| `fixedEffectsStatus` state | Bytes for one combine (4,096 voxels, 2 runs) | Per voxel | Suite |
|---|---|---|---|
| Reverted to `run.resolvedVoxelStatuses(voxelPosition)` | 174,941,944 | 42,710 | fails |
| Branch as committed | 16,247,000 | 3,966 | passes |

After the mutation check, `FixedEffects.scala` was restored. Its SHA-256
(`e0f87abb…c223a`) was the same before and after, and `git status` showed the
file unmodified.

Both allocation suites skip with `assume` when the JVM has no
thread-allocation counters, rather than failing. They report measurements only
through the assertion message.

**Residual finding, outside this cluster:**
`ResultArtifacts.scala`, in the runwise and patterned-runwise status-record
branches (lines ~179 and ~191), calls `run.resolvedVoxelStatuses(position)`
inside a per-voxel loop. That is the same O(R·V²) pattern on the artifact
export path. It predates the snapshot base (it is already present at
`53097f3f`), and the 09-12 bead did not list it. It should be tracked
separately and not added to C4.

## Gates

Gates were run with `tools/build/sbt-warm` in this worktree, one batch at a
time, with at least 60% memory free before each batch. The server was shut
down afterwards.

| Gate | Result |
|---|---|
| `fitJVM/test` | 569 passed, 0 failed (includes `VoxelStatusAllocationSuite`: 32,768 bytes) |
| `fitJS/test` | 515 passed, 0 failed |
| `fitEstimatesJVM/test` | 16 passed, 0 failed |
| `fitEstimatesJS/test` | 12 passed, 0 failed |
| `firstLevelLawsJVM/test` | 83 passed, 0 failed |
| `scalafimCompileAll` (both platforms, `-release:17`) | success, 0 warnings |
| Negative control: `fitJVM/testOnly *VoxelStatusAllocationSuite` with the dense line reverted | fails as expected (5,042,176 bytes) |
| Negative control: `fitJVM/testOnly *FixedEffectsStatusAllocationSuite` with the fixed-effects line reverted | fails as expected (42,710 B/voxel) |

### Post-review follow-up gates

The review of `d1a07520` returned APPROVE-WITH-NITS. The follow-up commit adds
`FixedEffectsStatusAllocationSuite`, changes the allocation suites to skip
with `assume` when counters are unavailable, removes the `println`, and adds a
historical-record header to the audit document.

| Gate | Result |
|---|---|
| `fitJVM/test` | 570 passed, 0 failed (both allocation suites included) |
| `fitJS/test` | 515 passed, 0 failed |
| `scalafimCompileAll` | success, 0 warnings |
