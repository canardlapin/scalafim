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

`OneShotMvpaTask` and `OneShotMvpaEngine` target the existing
`RoiOutcome`/`MvpaResult` surface through the canonical typed `MvpaTask` and
`MvpaEngine` boundaries. They accept `OperatorRoiAnalysis`; dense analyses remain
available through `RoiAnalysis.materializing` as the explicit Phase 2 parity
path.

`CrossValidatedOperatorRidgeAnalysis` is the Phase 3a native path. Passed to
`OneShotMvpaEngine`, it estimates multiclass ridge weights through the composed
\(A_rY_r\) operators without requesting a trial-by-feature beta matrix:

```scala
val ridge =
  CrossValidatedOperatorRidgeAnalysis(
    OperatorRidgeConfig(penalty = 0.5).toOption.get,
    storePredictions = true
  )

val result =
  OneShotMvpaEngine.run(dataset, featureSetPlan, response, ridge)
```

The result uses ordinary `RoiOutcome` values with an `OperatorRidge` payload
containing optional class scores and mandatory fold/solver receipts. Soft
multinomial logistic regression, fold-local feature scaling, typed nuisance
actions, and fused/cache crossover planning remain later Phase 3 work.

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
