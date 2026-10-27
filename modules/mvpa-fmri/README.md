# scalafim-mvpa-fmri

Shared JVM/Scala.js composition between first-level fMRI trial readouts and the
portable MVPA operator boundary.

`IdentifiedReadoutRelations.withRuns` binds run partitions, effect and neural
axes to actual `RunReadoutRelation` providers. For each run, it composes the
prepared `TrialReadout` with the response operator directly as a typed relation
without materializing trial-by-feature beta patterns. Metadata compatibility is
checked before acquisition. Explicit scoped providers guard forward and adjoint
applications, expire before closing, and preserve acquisition/preparation
support. A one-shot declaration cannot authorize repeated relational evaluation.

Use the shared `RelationRdm` and `RelationConsumers` for signed geometry and
RSA. Pairing and metric admission are explicit; temporal nuisance belongs to
the prepared readout, and RSA model controls remain separate comparison inputs.
The independent `BetaFreeRsaAcceptanceSuite` checks a three-run literal response
fixture through this native route on JVM and Scala.js.

`RunTrialReadout.make` validates a response block and prepared trial readout,
then composes their numerical operators. `explicitPatterns(PatternCopyBudget(...))`
is a separately budgeted dense copy. Native relational consumers use the composed
operator directly. No generic one-shot dataset, task or parallel result collector
remains.

Predictive operator ridge and soft LDA are owned by standalone
`mvpa4s-predictive`, including `AlderOperatorRidge`, `AlderSoftLda` and the
single-fit `SoftLda.fit` adapter. Import `mvpa4s.predictive.*`; generic
`OperatorRidge.fit` remains in the provider core. Identified validation retains
hard/simplex membership, fit audits and convergence/operator receipts. Soft
LDA keeps component policy and training-only trial nuisance explicit.

The earlier predictive CV analyses and universal predictive payloads were
removed in M1.12. Relational operator analyses and their universal payloads
were removed in M2.09; remaining generic one-shot boundaries expire in M3.13.

## Canonical global artifacts

`CanonicalRunEvidence.fromObservations` binds each run's actual time and neural
axes, identified observations, temporal geometry schedule and explicit scoped
provider. `CanonicalRunSet.make` binds their exact order to a run partition
axis. `ObservationProduct` owns provider acquisition and expiration.

`CanonicalGlobal.fit` returns a `CanonicalArtifact[N, C]`: its neural domain
remains nominally `N`, while fitted component coordinates are existential.
`fitNonnegative` and `fitManova` return the corresponding constrained and
spectrum artifacts. They retain the upstream functional frame, regularization,
operator program, diagnostics, training runs, temporal receipts and actual
moment content identity. These are descriptive training fits.

`assess`, `assessNonnegative`, `assessManova` and `assessSigned` return separate
leave-one-run-out assessments. Every geometry schedule is resolved before the
first response access. Each training fit is frozen before its held-out response
is read. Contrast methods retain `Z'Z`, `Z'X`, contrast variance and temporal
residual moments; they do not substitute a relational noise query for the
first-level residual SSCP. MANOVA evaluates the held-out spectrum inside the
frozen training frame. Signed assessments preserve negative cross-run quotients
and the explicit run-wise sign-flip exchangeability declaration.

`NonnegativeCanonicalModelSpec` fixes residual regularization and solver policy
before folds. Its coordinate cone is part of the estimand: permutations preserve
the result, while arbitrary rotations need not. The upstream fit retains its
stationary-point and feasibility diagnostics; no global optimality is added.

The earlier canonical datasets, feature-set scanners and parallel ROI summaries
were removed in M3.01. The existing independent R fixture suites now exercise
these native artifacts, including scale/basis laws, held-out perturbations,
geometry scopes, signed reversal and degeneracy.

These canonical adapters explicitly materialize responses and neural-square
moments. Capacity and local numeric-storage admission precede provider reads.
The backend workspace remains an explicit unknown unless the caller supplies a
provider declaration; `WholeNumeric` requires known source and backend costs.
The local storage receipt is not process peak-memory or whole-brain feasibility
evidence. General eigensolvers and constrained optimization remain in pinned
Multivar/Gale.

Current migration evidence: [M3.01 native global/canonical verification](../../docs/verification/umvpa-global-canonical-20261001.md).

## Dataset adapters

Typed dataset adapters for the portable MVPA core.

Package root:

```scala
import scalafim.fmri.mvpa.dataset.*
```

Generic predictive admission and the Alder method adapters live in
[`mvpa4s-predictive`](https://github.com/canardlapin/mvpa4s/blob/main/docs/predictive.md)
(`mvpa4s.predictive`). This module feeds them dataset-backed observations.
`SoftLda.fit` and `AlderSoftLda` also live in that provider: they preserve
operator products, explicit training-only nuisance and identified validation,
without a dependency on dataset or fMRI fit types. Import `mvpa4s.predictive.*`.

See the executable
[atlas classification workflow](../../examples/workflows/shared/src/main/scala/scalafim/examples/workflows/AtlasMvpaWorkflow.scala)
for native spatial admission and a bound validation plan. The same source and
suite run on JVM and Scala.js.

`DatasetObservationEvidence` constructs nominal sample and neural domains,
identified observations, selected feature mappings and sample metadata. It
supports synchronous `DatasetSeriesReader` input, a synchronous dataset bridge,
and explicit derived pattern rows. `OpenedDatasetMvpaExecutor.observations`
performs selected reads through the opened dataset's effectful boundary.

```scala
val request =
  DatasetPatternRequest(
    selection = DataSelection(voxels = IndexSelection.indices(0, 2, 4)),
    metadata = SampleMetadataRequest.labeled(
      labels = Vector("face", "scene", "face", "scene"),
      blocks = Some(Vector("run-1", "run-1", "run-2", "run-2"))
    )
  )
val evidence = DatasetObservationEvidence.fromDataset(dataset, request)
```

Derived coefficients use `DatasetObservationEvidence.fromPatternRows`, with
an explicit `DatasetId`, row names, ordered feature indices and optional labels, runs, blocks
and item names. Estimate origins remain distinct from timepoint origins.
`categoricalLabels` returns a column bound to the actual sample domain or a
typed missing-label refusal. Validation binds run or block columns through the
native design; the dataset adapter does not construct a second fold engine.
Regional and searchlight selection uses typed spatial measurement frames.
Serialized axes bind the declared dataset and ordered sample origins or neural
mapping coordinates. Changing labels or numeric values changes value identity;
it preserves the coordinates of that population.

This module has no JVM-only dependencies and is built for both JVM and
Scala.js. JVM-specific readers should stay in `dataset` or higher-level
workflow modules; they can produce `FmriSeries` values and then use this bridge.

Run it directly with:

```sh
sbt mvpaFmriJVM/test
sbt mvpaFmriJS/test
```
