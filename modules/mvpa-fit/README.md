# scalafim-fmri-mvpa-fit

Shared JVM/Scala.js composition between first-level fMRI trial readouts and the
portable MVPA operator boundary.

For each run, `RunTrialReadout` composes a prepared `TrialReadout` \(A_r\) with
its timepoints-by-features response \(Y_r\), yielding a `PatternOperator` for
\(A_rY_r\). `OneShotDataset` validates one common feature axis, stacks those
run operators by trial row, preserves run/trial identities and estimability,
and derives leave-one-run-out folds. No trial-by-feature beta matrix is required
by the operator path. Its `OperatorPatternSource` pushes each ROI feature
selection into the run's time-series columns before composing with the trial
readout, avoiding whole-feature-axis work for small MVPA regions.

`OperatorCrossnobisAnalysis` and `OperatorCrossnobisRsaAnalysis` are the
beta-free representational path. They apply the adjoint of the composed
readout/response operator to fold-local condition averages, reduce those
statistics directly to a crossvalidated condition Gram, and reuse the ordinary
labeled RDM/RSA payloads and model scorers. Temporal nuisance remains inside
the prepared `TrialReadout`; model-RDM nuisance remains an explicit
`RdmScorer.PartialPearson` control. No trial-by-feature coefficient table is
created between those two domains.

`OneShotMvpaTask` and `OneShotMvpaEngine` target the existing
`RoiOutcome`/`MvpaResult` surface through the canonical typed `MvpaTask` and
`MvpaEngine` boundaries. They accept `OperatorRoiAnalysis`; dense analyses remain
available through `RoiAnalysis.materializing` as the explicit Phase 2 parity
path.

Predictive operator ridge and soft LDA now run through `AlderOperatorRidge`
and `AlderSoftLda` in [mvpa-dataset](../mvpa-dataset/README.md). They retain the
single-fit `OperatorRidge.fit` and `SoftLda.fit` numerical kernels. Native
validation supplies identified train/test rows, hard or simplex membership,
fit audits and convergence/operator receipts. Soft LDA keeps component policy
and trial nuisance explicit and fits nuisance scope using training rows.

The earlier predictive CV analyses and universal predictive payloads were
removed in M1.12. `OneShotDataset`, the generic one-shot engine and relational
operator analyses above retain named M2.09/M3.13 migration ownership.

## Canonical contrast effect

`CanonicalEffectMvpa` is the beta-free canonical-effect path. Each
`CanonicalRunInput` combines a prepared scan-level response with a
`PreparedContrastGeometry`; for every requested ROI or searchlight, the engine
accumulates only the runwise `Z'Z` and `Z'X` moments. Training folds aggregate
their effect and residual operators and delegate the generalized-Rayleigh fit
to `multivar.CanonicalEffectProblem`. The learned frame is frozen before the
held-out run contributes its effect/residual quotient.

Temporal preparation remains a `fit` concern. `CanonicalGeometrySchedule`
accepts fixed/per-run geometry directly and requires response-learned shared
geometry to name every exact training-run scope. Results retain both an
ordinary `MvpaResult` scalar summary and typed fold payloads containing the
canonical fit, temporal receipts, regularization, Gale diagnostics, and the
explicit `RunwiseSufficientStatistics` execution mode. No `TrialReadout`,
trialwise beta matrix, or time-by-time projector is part of this API.

## Multiple contrasts and full MANOVA

`ManovaMvpa` extends the same runwise sufficient-statistic path to a typed
`PreparedManovaGeometry`. A rank-q contrast subspace produces a rank-at-most-q
feature effect without materializing coefficient maps. Training folds fit the
full generalized-root frame through `CanonicalEffectProblem.fitSpectrum`, whose
`FunctionalFrame` and `OperatorProgramFit` retain the subspace, regularization,
Gale certificate, and repeated-root clusters.

Held-out data is first compressed through that frozen training frame. Only then
is its q-dimensional generalized spectrum evaluated. Results expose Roy's
largest root, Wilks' lambda, Pillai's trace, and the Hotelling-Lawley trace as
distinct typed estimands and ordinary MVPA metrics. Repeated roots identify a
projector-valued subspace; no arbitrary axis is promoted to a scientific
result.

## Coordinate-constrained canonical effect

`NonnegativeCanonicalMvpa` estimates the distinct nonnegative canonical root
through the same `CanonicalEffectDataset` and runwise sufficient statistics.
An inspectable `NonnegativeCanonicalModelSpec` fixes residual regularization
and Gale solver policy before fold construction; version one performs no
response-selected tuning. In each outer fold all training moments are formed
and the constrained `OperatorProgram` is fitted before the held-out response is
accessed.

The constraint removes the ordinary sign gauge and makes feature coordinates
part of the estimand. Feature permutations preserve the result, but arbitrary
rotations generally do not. Fold payloads retain the nonnegative frame,
stationary-point attestation, Gale KKT/feasibility/normalization diagnostics,
temporal preparation receipts, and held-out root. The ordinary `MvpaResult`
surface exposes `MeanNonnegativeCanonicalRoot` and
`NonnegativeCanonicalCorrelation` for ROI and searchlight consumers.
