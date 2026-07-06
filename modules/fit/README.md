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
