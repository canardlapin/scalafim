# Subject group analysis

`mvpa-group` connects source-bound MVPA coordinates to the existing `group`
engine. Subjects are the population units. Separate subject fits and frozen
task/spatial transport remain the fitting scope.

`SubjectGroupBridge` retains full component and cross-feature covariance,
physical value units, subject order, geometry evidence, uncertainty origin and
degrees-of-freedom provenance. Loading-derived uncertainty requires an explicit
homogeneous separable joint Gaussian model and a source-bound residual feature
covariance. Estimated covariance cannot become known by changing a declaration.

## Mean patterns and heterogeneity

Given an admitted `SubjectGroupInput`, declare the population, subject sampling,
conditioning, first-level nuisance and multiplicity family:

```scala
val contract = SubjectPopulationContract(
  population = "study recruitment population",
  independentSubjectSampling = "independently recruited participants",
  conditioning = "frozen discovery task coordinates and spatial measurements",
  firstLevelNuisance = "the subject-specific confirmation nuisance designs",
  multiplicityFamily = "all declared task-by-measurement cells"
)
val summary = SubjectGroupSummary.fit(input, contract,
  SubjectGroupCalculation.ApproximateMixedEffects(
    TauEstimator.PauleMandel, MetaInference.ModifiedKnappHartung,
    "declared plug-in calculation; scientific calibration is separately required"
  ))
```

The result contains mean effects, standard errors, unadjusted pointwise p-values,
Q, I² and tau², plus empirical subject means/variances. Empirical variance includes
sampling noise; tau² estimates the model's between-subject variance. For
`KnownVarianceGaussianFixedEffects`, tau² is fixed at zero and the estimand is
a common effect. That calculation accepts only known covariance sources.
Neither mode changes the original uncertainty or discards individual expression
and full covariance. Task-linked operators keep their original task axis and
operator units; they do not acquire component labels.

The summary uses an intercept-only group design. Covariate models remain
available through `input.marginalModel`; their coefficients are not silently
renamed means. Any native sample failure prevents a complete summary. The
summary-cell limit covers its eight output matrices; native solver workspace,
borrowed input, object overhead and whole-process memory are outside this limit.

## Held-out subjects

`HeldOutSubjectPrediction.freeze` wraps M4.06 `ComponentPredictionHeads` with
row-bound subject columns for shared learning, head training and assessment.
Pass the actual head-bound training exposure account and an untouched
assessment account. The supported prediction procedure is continuous
target-metric squared loss from frozen OLS full/reduced component heads.
Fitting, reduced-head refitting and loss calculation remain owned by
`ComponentConfirmation`. This adapter adds no CV or model-selection engine.

Choose `SubjectHeadTraining.SharedTraining` for a shared head trained only on
the learning cohort, or `SubjectAdaptation(subject, protocol)` for OLS heads
calibrated on separate rows/units of one new subject. In the latter case,
shared coordinates, projections, spatial support, rotation, preprocessing and
the loss metric must stay exactly frozen. The calibration subject must be
absent from shared learning, and assessment must use separate rows/units of
that subject. A native-space map or preprocessing change requires a separately
admitted prediction adapter; matching dimensions alone does not suffice.

```scala
for
  plan <- HeldOutSubjectPrediction.freeze(shared, fittedHeads,
    learningSubjectColumn, headTrainingSubjectColumn, assessmentSubjectColumn,
    SubjectHeadTraining.SharedTraining, predictionContract,
    actualHeadExposure, untouchedAssessmentExposure)
  assessed <- plan.evaluate(assessmentBrain, assessmentTargets,
    currentAssessmentExposure)
  summary <- SubjectPredictiveSummary.combine(expectedSubjects, Vector(assessed))
yield summary
```

For adaptation, evaluate each subject's separate head and combine the results
in the declared cohort order. All results must share the same learning source,
metric/family, nuisance semantics and adaptation protocol. A unit cannot span
subjects. Duplicate subjects, missing cohort members, repeated actual brain
evidence and incompatible plans refuse. Rows receive equal weight within
subjects, then subjects receive equal weight. Signed improvements and observed
subject variability remain visible. The predictor's and reducers' numeric-cell
limits are separate; they are not a whole-process allocation certificate.

This is a fixed external-cohort descriptive summary, conditional on learning
and any declared adaptation. It supplies no population p-value, confidence
interval, CV-fold standard error or prevalence claim. `populationInference`
and `prevalence` return typed unavailable results. The native marginal mean
calculations likewise supply no joint component/spatial inference, prevalence
or universal estimated-variance calibration. M5.04 owns scientific qualification.

Subject columns and exposure snapshots retain caller declarations. They can
reject contradictory identities and recorded exposure; they cannot authenticate
renamed physical observations or undisclosed external access.

Tests: `python3 tools/build/sbt-warm mvpaGroupJVM/test mvpaGroupJS/test`.
Independent numerical fixtures and source-bound execution evidence live in
[`umvpa-group-summaries-20261007`](../../docs/verification/umvpa-group-summaries-20261007/README.md).
