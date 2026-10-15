# Native unified MVPA workflows

Start with identified samples, measurements and targets. Inspect the plan and
capabilities before acquiring payloads. These paths use the current production
API and retain typed refusals; the [migration ledger](../plans/unified-mvpa-migration-ledger.md)
records the removed engine/fold/source façades.

## Classify atlas regions with held-out runs

Run the complete synthetic atlas-to-classification example:

```sh
python3 tools/build/sbt-warm 'workflowExamplesJVM/runMain scalafim.examples.workflows.runAtlasMvpaWorkflow'
```

[AtlasMvpaWorkflow.scala](../../examples/workflows-jvm/src/main/scala/scalafim/examples/workflows/AtlasMvpaWorkflow.scala)
builds nominal sample and neural axes, run-bound columns, an atlas measurement
frame and `LeaveOneGroupOutDesign`. Each visited region enters
`AlderPredictiveAdmission.nativeMeasurement` and
`AlderSwiftCentroid.crossValidate`. Results retain feature identities, tested
sample count and accuracy. The example's separable synthetic data illustrate
wiring, not real-data accuracy or a matched comparison.

## Repair an axis mismatch and change folds safely

An imported label column in reversed sample order must fail positional decode.
Inspect `AxisPage`, join by stable sample key, and create a new value identity
for the repaired column. Check key coverage and uniqueness before the join;
matching lengths alone is insufficient.

Rebind `ValidationDesign` after changing folds. Use `Diagnostics.diff` to see
that the plan identity and binding changed. `Diagnostics.explain` lists missing
capabilities using cached metadata, so diagnosing a plan should not read the
neural matrix. The executable rehearsal checks this with a source that throws
on payload access:

```sh
python3 tools/build/sbt-warm \
  'mvpaJVM/testOnly scalafim.fmri.mvpa.analysis.FoundationExtensionSuite scalafim.fmri.mvpa.analysis.DiagnosticsSuite' \
  'mvpaJS/testOnly scalafim.fmri.mvpa.analysis.FoundationExtensionSuite scalafim.fmri.mvpa.analysis.DiagnosticsSuite'
```

## Add RSA to retained relations

For observations, bind run and condition columns in `ObservationMeanPlan`,
then construct `ObservationMeanRelations`. For first-level readouts, use the
source-bound relations in `mvpa-fit`; their scoped operators expire with the
acquisition callback. A callable readout does not itself establish residual
precision or independent errors. Missing residual evidence must be reported,
or the analyst may explicitly choose a supported identity metric with its
different assumptions.

Compute ordinary geometry with `OrdinaryRelationGeometry`, or signed
crossvalidated geometry with `RelationRdm` under an admitted metric and pairing.
Retain reusable geometry with `RelationConsumers.cache`/`RetainedQuery` only
when its lifetime permits it. A new RSA model or scorer invalidates the
comparison child; it can reuse unchanged fitted relations and geometry. A
change to training scope, metric or relation provenance requires recomputation.

```sh
python3 tools/build/sbt-warm \
  'mvpaJVM/testOnly scalafim.fmri.mvpa.relation.RelationConsumersSuite' \
  'mvpaJS/testOnly scalafim.fmri.mvpa.relation.RelationConsumersSuite'
```

The suite measures source reads, retains the actual RDM, tests model-only
invalidation and rejects retention of one-shot relations. The native readout
examples and independent beta-free R oracle are exercised by
`mvpaFitJVM/test` and `mvpaFitJS/test`.

## Fit, confirm and summarize subjects

Keep discovery fitting and selection separate from confirmation. Freeze score
projections, spatial support, preparation, coordinates and nuisance semantics
before admitting a `ConfirmationDesign`. Inspect its law and exposure receipt.
The [claim matrix](../plans/unified-mvpa-claim-boundaries.md) distinguishes forward
patterns, filters, empirical diagnostics, association and predictive utility.

`RankConfirmation.run` currently exposes candidate arithmetic while
`admittedDetectableRank` refuses pending calibration. Unknown exposure,
unsupported row randomization and incomplete families likewise refuse; do not
replace those results with an unqualified p-value.

Transport each subject's task-linked estimates and complete covariance into
frozen shared coordinates. Use `SubjectGroupBridge` and
`SubjectGroupSummary.fit` for explicit known-common-effect or approximate
mixed-effects summaries. [The group module guide](../../modules/mvpa-group/README.md)
contains complete composition signatures, uncertainty assumptions, summary
budgets and separate subject-adaptation rules.

`HeldOutSubjectPrediction` keeps learning subjects, head-training subjects and
assessment subjects distinct. Combine the complete declared cohort through
`SubjectPredictiveSummary`; signed improvements and subject variability remain
visible. Its population-inference and prevalence methods return unavailable.

```sh
python3 tools/build/sbt-warm mvpaGroupJVM/test mvpaGroupJS/test
```

These are runnable examples and automated workflow contracts. They do not
constitute an independent analyst usability review. M5.06 remains open for
that review and the unresolved M4.10 prerequisite; M5.05 owns measured matched
comparisons, and M5.04 owns scientific group calibration.
