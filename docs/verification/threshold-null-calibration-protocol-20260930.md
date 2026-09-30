# Threshold null calibration protocol (predeclared, 2026-09-30)

Mote: `bd-01M2GTMY1KYB94RGJARBB1GBK8`, packet 3. This protocol is committed
before any calibration-profile run. Its acceptance rules, conditions and seeds
may not be changed after results are seen. A revision requires a new dated
protocol, and the superseded results stay on record.

## Question

Do the implemented family-wise procedures control the family-wise error rate
(FWER) at the nominal level when the permutation null is valid? The procedures
are voxelwise maxT (`MaxT.runMap`), Westfall-Young step-down
(`WestfallYoung.stepDown`) and HierScan. The conditions include spatial
dependence, small n and between-subject heterogeneity.

## Data-generating process

- **Field and data.** A full-mask 6×6×6 grid (216 voxels). Each subject map
  is `s_i · e_i + μ`:
  - `e_i` is iid N(0, 1) per voxel. Under **smooth**, it is then replaced by
    the mean of its in-grid 3×3×3 neighbourhood, which gives spatial
    dependence.
  - `s_i = exp(0.5 z_i)` with `z_i ~ N(0, 1)`: subject-level
    heteroscedasticity.
  - `μ` is 0 under the complete null. Under the partial null it is `δ = 1.2`
    on the 2×2×2 corner block and 0 elsewhere.
- **Statistic.** The voxelwise one-sample t, mean / (sd / √n).
- **Null family.** Subject sign flips. The errors are symmetric and independent
  across subjects, so sign flips are exchangeable under H0 at every null
  voxel.
  - **Exact** (n = 6): all 64 flip vectors, including the identity,
    `NullReference.ExactEnumeration`.
  - **Monte Carlo** (n = 10): B = 99 flip vectors drawn uniformly with
    replacement from all 1,024 vectors, identity included,
    `NullReference.MonteCarlo`. The plus-one p-value is valid for this scheme
    (Hemerik & Goeman, 2018); ties with a drawn identity make it slightly
    conservative.
- **Level and alternatives.** α = 0.05, under the `Greater` and `TwoSided`
  alternatives.

## Conditions

The full cross is {exact n=6, Monte Carlo n=10} × {white, smooth} ×
{Greater, TwoSided} × {complete null, partial null}, which is 16 conditions.

## Error events per replicate

- **Complete null.** Any rejected voxel counts as an error for maxT and WY.
  For HierScan, any significant region counts.
- **Partial null.** Any rejected voxel outside the signal block counts as an
  error for maxT and WY; this is strong control. HierScan is not scored in the
  partial null. Its hits are regions, and region-level error under partial
  nulls is a separate claim not made here.

## Acceptance

In each condition, let k be the number of error replicates out of R.

1. **Every replicate completes.** A `Left` from any procedure fails the
   condition, and the count is recorded.
2. **Not liberal.** Fail if P(Binom(R, α) ≥ k) < 0.001.
3. **Not over-conservative, only where the size is exactly known.** Fail if
   P(Binom(R, size) ≤ k) < 0.001. This applies only to maxT/WY under the
   complete null, and only where the exact size is:
   - Exact, Greater: ⌊αB⌋/B = 3/64.
   - Exact, TwoSided: flip pairs ±v give tied |t|, so the size is
     2⌊αB/2⌋/B = 2/64.

   It is not applied to Monte Carlo conditions, where duplicate, negated or
   identity draws make the size approximate, nor to HierScan, whose alpha
   splitting is conservative by design, nor to partial nulls.
4. **Equivalence.** Under the complete null, maxT and WY must reject in exactly
   the same replicates, since their first steps coincide. Any disagreement
   fails.

## Profiles and seeds

- **Pull request.** R = 200 per condition, base seed 20260930. It runs on the
  JVM and on Scala.js.
- **Calibration** (`SCALAFIM_THRESHOLD_CALIBRATION=calibration`, JVM). R = 2000
  per condition, held-out base seed 7310930. That seed was not used during
  development.

A condition's seed is `base + 1000 × conditionIndex`.

## Not covered

These are recorded as unavailable, not claimed:

- FDR: no FDR method is implemented.
- First-level uncertainty propagation.
- Non-symmetric error distributions: sign flips are invalid there, and the
  protocol does not claim robustness to them.
- Adaptive HierScan priors, since only uniform priors are run.
- HierScan region-level error under partial nulls.
