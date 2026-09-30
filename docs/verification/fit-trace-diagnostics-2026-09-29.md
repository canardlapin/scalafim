# Bounded first-level fit traces — 2026-09-29

## Contract

`FitPlanExecutor.fitWithTraceCapture` explicitly fits one selected response block
and returns a `DenseFmriFitResult` with a trace handle attached to that same
fit. It admits only the dense OLS or dense GLS strategy (runwise GLS is a
distinct estimator even though it shares an engine label). It checks the reader's exact dataset descriptor, resolves the requested
rows and voxels against positive budgets before reading, reads once, checks the
returned axes, prepares the response, and fits OLS or AR GLS once. The handle
retains only that bounded prepared block and its design. Repeated `trace`
requests do not read or refit. A regular dense result and historical
estimate-only artifact return `Unavailable` for traces. A separate selected fit
is a new fit; its estimated shared AR coefficients must not be attributed to a
previous full-volume fit.

`FitTraceIdentity` records the dataset id, model design fingerprint where
available, selected scan/run/voxel axes, engine, response preparation and fitted AR
diagnostics. Requests must echo it exactly. The result names selected row
positions, source scan indices and run indices. `Original` is available only
when the raw selected series equals the model-prepared response exactly.
`ModelPrepared` is before temporal whitening and is explicit for weighting or
other transformations. `Transformed` applies each fitted voxel's actual AR
coefficients, initial-condition rule and contiguous whitening segments to the
observed, fitted and residual signals. OLS uses the identity transform.

The reported ACF is an uncentered normalized residual dot product at integer
source-scan lags. Pairs must be in the same contiguous run/censor whitening
segment; gaps never create artificial adjacent pairs. Zero-norm/no-pair lags
are absent. Residuals are scaled before summing squares to avoid overflow from
large but finite signals. Each request is bounded by the captured row count.

## Verification

At the tested tree, `diagnostics-tests-03.log` under the execution root
records exit 0 for:

- `fitJVM/testOnly scalafim.fmri.fit.FitTraceDiagnosticsSuite scalafim.fmri.fit.MatrixFileTraceDiagnosticsSuite`: seven passes.
- `fitJS/testOnly scalafim.fmri.fit.FitTraceDiagnosticsSuite`: six passes.
- `fitBenchJVM/compile`: warning-clean.

The synthetic cases include independent analytic OLS and known-AR GLS 2-by-2
normal-equation calculations; `y = fitted + residual` in both spaces;
OLS/GLS orthogonality; estimated shared and voxelwise AR; run/censor gaps;
chunk-size equivalence; selection budgets, source/run mismatches, read failure,
cancellation and one-read file-backed execution. These checks do not constitute
external inference calibration. The test runtime reported sbt 1.11.7, Scala
3.7.4 and Homebrew Java 25.0.1. Exact local commit and full module tests are
recorded in the handoff after completion.

Review of local commit `ca0f3602` found an untested strategy alias:
`RunwiseGeneralizedLeastSquares` shares the dense GLS engine label. The
worktree now refuses that strategy before reading and adds an independent
per-segment AR recurrence oracle for estimated shared and voxelwise fits.
Those corrections require a successor JVM/Scala.js receipt and commit; the
earlier 7/6 pass does not verify them.

## Scope

This API does not retrofit arbitrary historical fits, retain full-volume
residuals, or grant downstream PLS inference eligibility. `Original` refuses
noninvertible response preparation instead of labeling a weighted signal raw.
