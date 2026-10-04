# Laws: chunk-invariant estimated-AR provenance (2026-10-03)

Branch `fix/laws-chunk-ar-determinism-20261003`, based on main `62312f5d`.

## Symptom

On main, both properties of `MissingResponseGeneratedLawsSuite` failed at the default
pull-request seed `sp9DAl0FY1eBBKPdxp1XHmPtWKKKG-fp4dJAL7wpvdF=`. For the estimated-GLS
engine, the whole-volume and chunked fits reported AR coefficients that differed by
1–3 ulp (for example 0.7355886724316139 against 0.7355886724316136). The suite compares
the `ArDiagnostics` provenance exactly.

## Bisection

Each candidate was tested in a detached throwaway worktree with
`firstLevelLawsJVM/testOnly *MissingResponseGeneratedLaws*`, one worktree at a time.

| Commit | Role | Result |
|---|---|---|
| `fa6bd6a9` | main-line parent of `45173e2c` | pass (2/2) |
| `45173e2c` | "Make fit preparation replayable and stream pooled GLS state" | **fail (2/2), same values** |
| `31505fd9` | AR bias-correction branch head | pass (2/2) |
| `9a8dcb74` | merge of the two lines | fail (reported) |
| `62312f5d` | current main | fail (reported) |

The first bad commit is `45173e2c`. The `9a8dcb74` merge resolution is not the cause.

## Root cause

`45173e2c` routed chunked estimated-AR GLS preparation through
`PooledGlsPreparation`. That path calls `ArEstimation.summarizeNoise` once per spatial
chunk and folds the results with `ArNoiseSummary.merge`. The whole-volume path calls
`ArEstimation.pooledAutocovariance` over every voxel. The two paths summed the same
per-element lag products in different floating-point orders:

- whole volume: one running sum over segment → column → lag → row;
- chunked: one running sum per chunk over column → segment → lag → row, then
  `left + right` across chunks.

Floating-point addition is not associative, so the pooled lag sums, and the
Yule-Walker coefficients computed from them, depended on the chunk width. The
suite's invariant, that the fit is identical whether or not it is chunked, is the
correct contract. `45173e2c` itself states that preparation is replayable and
bounded, so the code is what has to change.

## Fix

- `ExactSum` (new, `private[ar]`) accumulates doubles exactly. Every finite double is
  an integer multiple of 2^-1074, so the running total is stored as a fixed-point
  integer in 70 carry-save base-2^32 `Long` digits. Adding and merging are therefore
  associative and commutative. `value` rounds once, to nearest with ties to even.
  Non-finite inputs are added in a separate IEEE channel.
- `columnLagProductSum` is a shared kernel. For one voxel and one lag it sums the
  products in a fixed order (segments in order, rows in order, starting from 0.0).
  `summarizeNoise` and `pooledAutocovariance` both use it, then add each voxel's
  partial sum into an `ExactSum`. Because each voxel's partial sum is computed the
  same way whatever its chunk, the pooled total does not depend on how the voxels
  are partitioned or the order in which chunks are merged.
- `ArNoiseSummary` stores the exact accumulators, and `merge` adds them exactly.
  `lagSumsByRun`, the finite checks and the error ADT are unchanged.
- Inner loops do not allocate. The exact accumulation runs once per voxel, run and
  lag, not once per sample, and uses only primitive `Long`/`Double` arithmetic, so it
  is portable between JVM and JS. No dependency was added.

Whole-volume lag sums now differ from the old naive sums only by rounding.
All AR, fit and parity suites pass unchanged.

## Tests added

- `MissingResponseGeneratedLawsSuite`: the two property bodies were extracted into
  `propagatedFailures` and `maskedFailures`, and the properties are unchanged. A new
  test, `regression: estimated GLS AR provenance is bit-identical across one-voxel
  chunks`, pins the shrunk counterexample from the failing seed:
  `MissingResponseCase(Vector(14, 15), 4, 2, …, Vector(0, 3, 2, 1), 1, 883789)`.
- `ArNoiseReductionSuite`: `block summaries are bit-identical to the whole-volume
  estimator for every chunking and merge order` checks every width from 1 to 7, merged
  both forward and reversed. It compares the merged sums against
  `pooledAutocovariance` and the merged whitening coefficients against `fitNoise`
  exactly.
- `ExactSumSuite`: exact cancellation, round-to-nearest-even ties, overflow to
  infinity, independence from order and partition, and IEEE behaviour for
  non-finite inputs.

## Gates (worktree with the fix)

| Gate | Result |
|---|---|
| `firstLevelLawsJVM/testOnly *MissingResponseGeneratedLaws*` | 3/3 pass |
| `arJVM/test` | 150/150 pass |
| `arJS/test` | 148/148 pass |
| `fitJVM/test` | 391/391 pass |
| `fitJS/test` | 380/380 pass |
| `firstLevelLawsJVM/test` | 54/57: 3 pre-existing `ConditionMilestoneSuite` failures (see below); MissingResponse 3/3 |
| `firstLevelLawsJS/test` | 54/57: the same 3 pre-existing failures; MissingResponse 3/3 |
| `scalafimCompileAll` | success (exit 0); no `[warn]` or `[error]` lines apart from sbt GC notices |

### Pre-existing failures, not caused by this change

`ConditionMilestoneSuite` fails three tests on both platforms: Gaussian
"admitted 0.0 at SNR 1.0", LWU "p95 latency NaN", and the throughput receipt. The
same three failures reproduce on JVM with this fix reverted, so main's
`ArEstimation.scala` produces them as well. The suite uses a fixed, caller-supplied
AR whitening plan and never calls the pooled estimator. These failures belong to a
separate defect.

## Mutation check

1. The fixed `ArEstimation.scala` was backed up. Its SHA-256 is
   `d9279f0bbf7356cd876c18d03a6dcee4cc6a1200b8afc45fec8fb5625ad5abcf`.
2. `git checkout HEAD -- …/ArEstimation.scala` restored main's version.
   `firstLevelLawsJVM/testOnly *MissingResponseGeneratedLaws*` then failed all
   3 tests: both properties and the new seed-pinned regression test.
3. The backup was restored and its SHA-256 matched
   `d9279f0bbf7356cd876c18d03a6dcee4cc6a1200b8afc45fec8fb5625ad5abcf`. The same
   command then passed 3/3.

## Review follow-up

An Opus review of `9a59b5ff` returned APPROVE-WITH-NITS and found no correctness
defects. This follow-up commit addresses the nits. Main at `53097f3f` was merged into
the branch first, in merge commit `fe105694`.

- **Independent exact reference.** `ExactSumSuite` adds a randomized test of 600
  seeded trials across six generators: arbitrary finite bit patterns, clustered
  cancellation, constructed ties, subnormal-heavy sums, sums near `MaxValue`, and
  exact cancellation plus noise. The reference adds the terms exactly as
  `BigInteger` values in units of 2^-1074, decomposed from `doubleToRawLongBits`.
  `BigDecimal` is not used because its conversions are inexact on Scala.js.
  - The result must be at least as close as its `nextUp` and `nextDown` neighbours,
    and a tie must leave an even significand.
  - A total of magnitude at least (2^1024 − 2^970)·2^1074 units must be ±Inf.
    Exact zero must be +0.0.
- **Explicit boundary cases.**
  - Subnormal totals, including the first rounding at 2^-1021.
  - Ties at 2^1000.
  - The integer fast-path and window edges at 2^53, 2^61, 2^62, 2^63 and 2^64 units.
  - `MaxValue + ulp/2` gives ±Inf, and `MaxValue + ulp/2 − tiny` gives `MaxValue`.
  - −0.0 and exact cancellation give +0.0.
- **Allocation.** `pooledAutocovariance` with one column (the per-voxel
  `AutocorrelationDiagnostics` call) no longer creates `ExactSum` accumulators. The
  exact total of one term is that term, and a signed zero becomes +0.0, so the shortcut
  matches the accumulator bit for bit. A new `ArNoiseReductionSuite` test checks the
  raw bits against `summarizeNoise`, which always goes through `ExactSum`. It covers
  six noise voxels plus zero, constant and large-magnitude series.
- **Cleanups.**
  - `ArNoiseSummary.make` rounds each exact total once and returns the typed
    non-finite error. `merge` and `summarizeNoise` no longer round twice.
  - `ExactSum.addAll` adds the other operand's carry-save digits directly instead
    of copying it. The bound argument is in the code.
  - The digit-range comment now says the top bit is 2097.
  - In `scale`, the `exponent > 1023` branch now returns +Inf directly. That branch
    is only reachable through overflow, and it is kept so that `powerOfTwo` never
    builds an out-of-range exponent field.
- **README.** The `modules/fit` sentence on bit-identity is limited to the raw pooled
  estimator. The chunked summary path does not apply `DesignCorrected`.
- **Gale upstreaming.** Mote bead `bd-01M42FRAY1S2DS6808TAWAQXBZ` tracks moving
  `ExactSum` to `gale.numeric` as a superaccumulator and deleting the `ar` copy at
  the next `galeRevision` bump.

### Follow-up mutation check

1. The new `ExactSum.scala` was backed up. Its SHA-256 is
   `a19ec067af4334105f4c2a08a9b6341b28cab6381629161d9005b976b556eb57`.
2. The tie rule was broken by forcing the sticky bit to 1 in the rounding window.
   `arJVM/testOnly *ExactSumSuite` then failed 3 of 6 tests. One was the randomized
   BigInteger reference test: "tie not rounded to even".
3. The backup was restored and its SHA-256 matched.

### Follow-up gates (merged with main `53097f3f`)

| Gate | Result |
|---|---|
| `arJVM/test` | 153/153 pass |
| `arJS/test` | 151/151 pass |
| `fitJVM/test` | 402/402 pass |
| `fitJS/test` | 391/391 pass |
| `firstLevelLawsJVM/test` | 54/57: the same 3 pre-existing `ConditionMilestoneSuite` failures; MissingResponse 3/3 |
| `firstLevelLawsJS/test` | 54/57: the same 3 pre-existing `ConditionMilestoneSuite` failures; MissingResponse 3/3 |
| `scalafimCompileAll` | success (exit 0); warning-clean |

Main stayed at `53097f3f` for every gate. Free memory was at least 33% before each batch.
