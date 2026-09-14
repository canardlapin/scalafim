# Run-specific GLS qualification (2026-09-14)

Issue: `bd-01M1Z2050HGVC8FAZM07TM2K9Q`

## Qualified contract

`FitStrategy.RunwiseGeneralizedLeastSquares` is the public run-specific
coefficient estimand for AR GLS. It uses `FitEngine.GeneralizedLeastSquares`
without changing the engine label, while `FitPlan.coefficientScope` and the
returned `FitSummary` identify the coefficients as `RunSpecific`. The existing
`FitStrategy.GeneralizedLeastSquares` remains the separately named
`SharedAcrossRuns` alternative.

Each selected source run is fitted through the existing `Gls` preparation and
solve. The run result retains:

- its structural `RunCoefficientProjection`, source-column mapping, local
  coefficient axis, selected row positions, and source timepoints;
- run-bound AR coefficients, whitening segments, censor gaps, method, and
  iteration count;
- coefficients, residual variance and residual degrees of freedom;
- the full normalized coefficient covariance, including one matrix per voxel
  for voxelwise AR estimation.

Chunk preparation estimates AR from the complete selected response once. Each
voxel chunk then selects the corresponding prepared whitening plans. Merge
requires identical run structure and merges voxelwise AR and covariance
receipts in requested voxel order.

`FixedEffects.combine` consumes each run's actual normalized covariance matrix
and residual variance. Shared covariance retains the existing compact scaled
representation; voxelwise covariance is inverted separately for every
run/voxel. No diagonal or sample-count shortcut is used.

## Refused combinations

- `global=true` is rejected for runwise GLS. This slice does not pretend that
  independently prepared runs provide an across-run pooled AR estimate.
- `MissingDataPolicy.OmitRowsPerVoxel` is rejected because unlike row patterns
  do not yet have a qualified rule for run-local AR receipts.
- Public low-level partitions must cover every selected design row exactly
  once, use unique run identities, and contain every declared censored
  timepoint.
- Typed strategy validation continues to reject volume weighting and other
  unsupported GLS controls before execution.

## Numerical evidence

`RunwiseGlsSuite` contains four shared tests and therefore runs unchanged on
the JVM and Scala.js:

1. strategy identity and explicit shared-coefficient/global-pooling refusal;
2. unequal, gapped runs with planted different effects, checked against a
   test-only fixed-AR(1) reference that manually segments and whitens `X` and
   `Y`, solves the two-predictor normal equations in closed form, and compares
   coefficients, normalized covariance, residual variance, df, and
   off-diagonal covariance;
3. public structural FIR execution with unequal runs, selected gaps, physical
   FIR-bin coordinates, reordered voxels, and chunked-versus-direct equality;
4. estimated voxelwise AR with one-voxel chunks, per-run/per-voxel covariance,
   downstream dense-result adaptation, and an independent two-by-two
   full-covariance fixed-effects calculation.

Observed worktree gates:

- `sbt -Dsbt.supershell=false scalafimCompileAll`: pass, JVM and Scala.js,
  warning-clean.
- `sbt -Dsbt.supershell=false 'modelJVM/test' 'fitJVM/test'`: 28 model tests and
  335 fit tests passed.
- `sbt -Dsbt.supershell=false 'modelJS/test' 'fitJS/test'`: 28 model tests and
  323 fit tests passed.
- focused `RunwiseGlsSuite`: 4/4 JVM and 4/4 Scala.js passed after the final
  fail-closed validation changes.

## Admission boundary

This evidence qualifies the run-specific AR GLS composition, its result
adapter, chunk invariance, and covariance-aware within-participant fixed
effects. It does not admit across-run AR pooling for this estimand, patterned
missing-data GLS, a new statistical default, or downstream application-level
inference eligibility.
