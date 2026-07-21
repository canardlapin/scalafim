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
multinomial logistic regression, fold-local feature scaling, and fused/cache
crossover planning remain later work.

## Pulled-back soft LDA

`CrossValidatedSoftLdaAnalysis` is the operator-native discriminant path. It
adapts each training `PatternOperator` to a typed `multivar.OpTable`, constructs
hard or simplex-weighted class relations, and pulls their between/within forms
back to feature space through `OperatorAlgebra.secondOrder`. The resulting LDA
is an inspectable `OperatorProgram` fit, using Gale-backed generalized-eigen or
trace-ratio solvers. The trial-by-feature table is never requested:

```scala
val softLda =
  CrossValidatedSoftLdaAnalysis(
    SoftLdaConfig(
      withinPolicy = WithinScatterPolicy.FixedTraceScaledRidge(
        TraceRidgeFraction.unsafe(0.05)
      ),
      objective = LdaObjective.FisherRayleigh
    ),
    storePredictions = true
  )

val result =
  OneShotMvpaEngine.run(dataset, featureSetPlan, response, softLda)
```

The two nuisance domains remain explicit. Temporal nuisance is part of each
design-only `TrialReadout`; an optional `TrialNuisanceDesign` instead lives on
the resulting trial/sample axis and is subset using training rows inside every
fold. `SoftLda.crossValidate` returns typed fold receipts retaining the fitted
operator program, source provenance, exact train/test samples, and trial-level
nuisance width. The ordinary MVPA adapter exposes cross-validated probabilities
through `RoiPayload.Classification`.

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
