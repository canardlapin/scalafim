# scalafim-fmri-fit

Portable fitting kernels for fMRI models.

The first engine is full-rank ordinary least squares over timepoints-by-voxels
response blocks. It is cross-built for the JVM and Scala.js and uses the
`scalafim-linalg` primitive array-backed matrix layer rather than Breeze.

The low-level `Ols` kernel is matrix-only. `FitPlanExecutor` is the first
fMRI-aware execution layer: it adapts `FitPlan` plus an in-memory
timepoints-by-voxels dataset into typed coefficient and residual-variance
results while preserving column names, voxel indices, and timepoint indices.
`FitEngine.RunwiseLeastSquares` executes the same OLS kernel independently per
sampling-frame block and returns per-run coefficient surfaces.

Dense OLS results carry normalized coefficient covariance, coefficient standard
errors, residual diagnostics, and t-contrast evaluation by design-column name.

## Numerical and inference behavior

OLS defaults to a rank-revealing QR solve with strict full-rank semantics. The
previous normal-equations path remains available as an explicit
`OlsSolvePolicy.NormalEquations`, but it is no longer the default. Rank-deficient
designs are rejected before fitting; ridge and minimum-norm rank policies are
represented in the public ADT but currently report typed unsupported-policy
errors rather than silently changing the estimator.

Dense `DesignMatrix` and `ResponseBlock` constructors reject non-finite values at
the boundary. Use the public constructors when accepting external data; reserve
the `.unsafe` constructors for already-validated internal paths and tests.

Contrast inference treats zero residual variance and zero contrast variance as
non-estimable. These paths now return `FitError.NonEstimableContrast` instead of
emitting non-finite t or F statistics, so callers should handle the typed error
case rather than interpreting `NaN` or infinity as a valid statistic.
