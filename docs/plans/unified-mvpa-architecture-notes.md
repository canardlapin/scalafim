# Unified MVPA Architecture: Design Notes

Status: **intake notes, Part 2; not yet an implementation plan**

Recorded: 2026-09-12

Companion: [Part 1 scientific-method notes](unified-pattern-first-mvpa-notes.md)

Synthesis: [Unified analysis PRD sketch](unified-mvpa-prd.md). Its rapid
replacement/deletion policy supersedes the compatibility-retention proposals
preserved below. It also refines coordinate versus value identity, global
result shapes, provider admission, and the three required vertical slices.

These notes preserve the proposed architectural foundation for the scientific
method in Part 1 and for ScalaFIM analysis more generally. Names and example
APIs are illustrative. Type relationships, identity rules, semantic
separations, repository boundaries, migration constraints, and laws are the
intended commitments.

## Verdict

**Keep most existing numerical and representation kernels, but replace the
architectural waist.**

The current `mvpa` package is a useful first implementation, but
`RoiAnalysis[Patterns]` should not become the lasting scientific abstraction.
It makes “run one analysis in one feature subset” the center of the system,
with an optional `FoldPlan`, a universal `Response`, and a fixed
`RoiAnalysisResult`. That center can support ordinary classification and simple
RSA, but it does not naturally unify:

- predictive modeling and multiresponse regression;
- cross-validated geometry;
- first-order/univariate effects;
- canonical and global decomposition methods;
- nuisance-aware preparation;
- purpose-specific randomization and inference; or
- cross-subject measurement and group analysis.

The basis worth committing to is

\[
\boxed{
\text{identified spaces}
+
\text{typed linear evidence}
+
\text{evidence design}
+
\text{measurement frame}
+
\text{estimand}.
}
\]

The compilation path should be

```text
scientific specification
  = source + design + frame + estimand

scientific specification
  -> bind identities and required capabilities
  -> scientific plan
  -> execution plan
  -> typed result + execution receipt
```

Two scientific kernels share that architecture:

1. **Predictive analysis:** observations, targets, leakage-safe preprocessing,
   learners, tuning, validation, cross-fitting, predictions, and metrics.
2. **Relational analysis:** partitioned experimental–neural relations,
   first- and second-order queries, effect forms, RDM/RSA, crossnobis,
   MANOVA-like statistics, canonical analyses, and component decompositions.

Prediction and relational geometry are siblings. Neither should be forced to
masquerade as the other.

## Existing strengths to preserve

Replacing the center does not discard the good properties of the current
modules. Preserve:

- representation-polymorphic `PatternSource` behavior during migration;
- operator-native `PatternOperator` execution;
- explicit dense materialization rather than hidden conversion;
- isolated per-ROI/per-measurement failure;
- streaming over feature sets or measurements;
- one-shot estimation from fMRI readout operators; and
- a deliberately thin spatial adapter.

These capabilities should be re-expressed around identified evidence, typed
designs, measurements, open estimands, and typed results.

## Current abstraction seams and disposition

| Current abstraction | Structural problem | Long-term disposition |
|---|---|---|
| `RoiAnalysis[Patterns]` | Makes a single ROI computation universal; fold need is Boolean; result shape is fixed. | Compatibility façade over typed estimands and compilers. |
| `Response` | A runtime ADT must anticipate all targets, queries, and method-specific validity rules. | Axis-bound typed columns, effect queries, and model matrices. |
| `FoldPlan` | Row-position splits do not identify validation, cross-fitting, pairing, permutation, bootstrap, nested tuning, or exact-coverage OOF semantics. | Purpose-specific evidence designs backed by resample4s. |
| `FeatureSetPlan` | Elevates region/searchlight to fundamental kinds and puts the center inside generic feature support. | `MeasurementFrame`; ROI and searchlight become builders. |
| `RoiPayload` | A closed enum requires core edits for every new analysis or encourages parallel result hierarchies. | `AnalysisResult[A]`, with a method-owned typed `A`; freeze the existing enum. |
| `PatternSource.selectFeatures` | Conflates numerical storage with spatial measurement. | A typed table composed with a measurement leg. |
| `SampleIndex` | Serves partly as row ordinal and partly as semantic row identity. | Separate semantic keys from implementation ordinals. |
| `MvpaEngine` | Mixes orchestration, validation, and method dispatch. | Separate bind, compile, and execute layers; retain as façade. |

The current `FeatureSetPlan` formally distinguishes only region and searchlight,
with searchlight validity encoded by a special center field. That makes weighted
ROIs, surface patches, aligned subspaces, multiscale bases, and cross-subject
measurement bridges unnatural extensions.

`RoiPayload` already acts as a central sum type for classification, ridge, RDM,
RSA, samplewise RSA, and feature models. The canonical-effect path has escaped
the restriction by returning a typed `CanonicalEffectMvpaResult` beside the
ordinary `MvpaResult`. That is evidence that the general result model is too
narrow, not a defect of the canonical method.

Before building upward, correct the identity ambiguity: a stored sample index
cannot simultaneously be required to equal the current row ordinal and serve
as the durable identity of an observation through restriction, stacking, and
reordering.

## Identified linear evidence as the mathematical center

The reusable substrate should be Multivar’s semantic linear algebra:

- nominal `SemanticSpace` witnesses;
- path-dependent `SpaceRef.Id` types;
- primal and dual coordinates;
- typed `Lin[From, To]` maps;
- structural adjoints and composition;
- `Table[Rows, Columns]`;
- value identity and provenance; and
- representation-independent operators and numerical certificates.

Do not introduce another pattern-matrix hierarchy as the new scientific core.

### Observation evidence

For sample patterns

\[
X\in\mathbb R^{n\times p},
\]

the semantic type is

```text
X: Table[Samples, Neural]
```

The row space is an identified sample axis. The column space is an identified
neural or feature space.

### Relational evidence

For an experimental–neural relation

\[
B\in\mathbb R^{q\times p},
\]

the semantic type is

```text
B: Table[Effects, Neural]
```

Its rows may represent effects, conditions, regressors, or contrasts. For an
fMRI run, it may be produced as

\[
B_r=A_rY_r,
\]

where \(Y_r\) is time-by-neural response evidence and \(A_r\) is a typed
readout/estimator from time to experimental effects. Existing one-shot
machinery is already close to this operator form.

### Spatial analysis is measurement, not feature indexing

A measurement is a typed linear leg

\[
L_m:\mathcal N\longrightarrow\mathcal M_m.
\]

Its matrix is \(d_m\times p\), and it acts on the neural boundary:

\[
X_m=XL_m^\top,
\qquad
B_m=BL_m^\top.
\]

This covers:

- a hard ROI or searchlight;
- a weighted ROI or atlas parcel;
- a surface patch;
- a multiresolution basis;
- coherent/configuration components;
- learned dimensionality reduction;
- subject-to-common-space alignment; or
- paired measurement legs connecting different subjects.

A hard feature subset is only a sparse one-hot measurement leg. It is not the
fundamental object.

### Row restriction is also a typed linear map

A training selection induces

\[
R_{\mathrm{train}}:\mathcal S\longrightarrow\mathcal S_{\mathrm{train}},
\qquad
X_{\mathrm{train}}=R_{\mathrm{train}}X.
\]

Sample restriction and neural measurement should be checked compositions, not
unrelated calls to `selectRows` and `selectFeatures`.

The core algebra becomes

```text
source table
  -- compose on sample side --> selected evidence
  -- compose on neural side --> measured evidence
  -- close or decompose remaining boundaries --> scientific result
```

## Complete space identity

An ID, role, and dimension are insufficient for a scientific space. Two spaces
may have the same shape and label but differ in coordinate order, basis, units,
scale, preprocessing, or provenance.

An approximate descriptor is:

```scala
final case class SpaceDescriptor(
    id: SpaceId,
    dimension: Dimension,
    coordinates: CoordinateSignature,
    basis: BasisDescriptor,
    units: Option[Units],
    scale: ScaleDescriptor,
    provenance: ProvenanceSignature,
    tags: Set[SpaceTag]
)
```

The exact fields may evolve. Freeze these laws:

1. Ordered coordinates participate in identity.
2. Reordering produces a distinct space.
3. An explicit reindexing relates the original and reordered spaces.
4. Equal shape never implies space compatibility.
5. A backend or storage codec does not define the scientific space.
6. Runtime-decoded descriptors are checked against nominal Scala witnesses
   before numerical access.

`SpaceRole` may remain descriptive metadata but must not become a closed
ontology of every possible scientific axis.

## Fundamental Scala abstraction sketches

Names are provisional; type relationships and laws are the commitments.

### Identified axes

```scala
trait AxisIndex[K]:
  def size: Int
  def keyAt(ordinal: Int): K
  def ordinalOf(key: K): Option[Int]
  def signature: CoordinateSignature

final class AxisRef[K] private (
    val descriptor: SpaceDescriptor,
    val index: AxisIndex[K]
):
  sealed trait Id extends SemanticSpace
  val evidence: SpaceEvidence[Id]
```

This separates the semantic key `K`, the efficient implementation ordinal
`Int`, and the complete identity of the ordered axis. `AxisIndex` is a
capability rather than necessarily a materialized map; contiguous, affine,
hashed, sparse, and external implementations may coexist.

### Axis-bound reindexing

Resample4s already distinguishes ordered subset selection, injective
reordering, draws with possible repetition, and total permutations. Bind those
ordinal maps to scientific spaces:

```scala
final class ReindexingLeg[
    Parent <: SemanticSpace,
    Child <: SemanticSpace
] private (
    val parent: SpaceEvidence[Parent],
    val child: SpaceEvidence[Child],
    val ordinals: resample4s.core.Reindexing,
    val leg: Lin[Primal[Parent], Primal[Child]]
)
```

The child-space identity is derived from parent identity, reindexing kind, and
the exact ordinal mapping. This lawfully connects efficient ordinal machinery
to scientific identity.

### Axis-bound columns

```scala
final case class Column[S <: SemanticSpace, A] private (
    rows: SpaceEvidence[S],
    values: IArray[A]
):
  def reindex[T <: SemanticSpace](
      by: ReindexingLeg[S, T]
  ): Column[T, A]
```

Targets and metadata become ordinary typed columns:

```scala
Column[Samples, ClassLabel]
Column[Samples, Double]
Column[Samples, RunId]
Column[Samples, TrialId]
Column[Samples, SubjectId]
```

A classifier requires a categorical target, a regressor a numeric target, and
RSA an experimental query or model relation. No universal `Response` must
anticipate all three.

### Observation evidence

```scala
final case class Observations[
    S <: SemanticSpace,
    N <: SemanticSpace
](
    samples: SpaceEvidence[S],
    neural: SpaceEvidence[N],
    patterns: Table[S, N]
)

final case class Supervised[
    S <: SemanticSpace,
    N <: SemanticSpace,
    Y,
    M
](
    observations: Observations[S, N],
    target: Column[S, Y],
    metadata: Column[S, M]
)
```

Dense, sparse, matrix-view, operator, composed-readout, and disk-backed patterns
are execution representations of one scientific evidence type.

### Partitioned relational evidence

```scala
final case class Relation[
    E <: SemanticSpace,
    N <: SemanticSpace
](
    estimate: Table[E, N],
    receipt: RelationFitReceipt
)

final case class RelationSet[
    P <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace
](
    partitions: SpaceEvidence[P],
    effects: SpaceEvidence[E],
    neural: SpaceEvidence[N],
    relations: IndexedSeq[Relation[E, N]]
)
```

Residual moments, noise precision, estimability, and degrees of freedom should
be typed capabilities, not an expanding bag of options:

```scala
trait ResidualMoments[A]:
  type ResidualSpace <: SemanticSpace
  def residualForm(value: A): ...

trait NoiseNormalizer[A]:
  def normalizer(value: A): ...

trait DegreesOfFreedom[A]:
  def residualDf(value: A): Double
```

An analysis requiring noise normalization should fail to compile or bind when
that capability is absent. A simple effect query should not fabricate it.

### Measurement legs and frames

```scala
trait Measurement[N <: SemanticSpace]:
  type Local <: SemanticSpace
  def id: MeasurementId
  def local: SpaceEvidence[Local]
  def leg: Lin[Primal[N], Primal[Local]]
  def descriptor: MeasurementDescriptor

final case class MeasurementFrame[N <: SemanticSpace](
    source: SpaceEvidence[N],
    measurements: Vector[Measurement[N]]
)
```

Builders live in domain modules:

```scala
VoxelFrame.rois(...)
VoxelFrame.searchlights(...)
SurfaceFrame.geodesicSearchlights(...)
AtlasFrame.weightedParcels(...)
AlignmentFrame.commonSpace(...)
BasisFrame.multiscale(...)
```

A searchlight center belongs in `MeasurementDescriptor`, where it supports
scattering a result back into an image. It does not belong in generic feature
support.

### Evidence designs remain semantically distinct

There should be no universal fold-plan replacement.

A validation design binds a resample4s split plan to an identified sample space
and preserves its coverage witness:

```scala
final case class ValidationDesign[
    S <: SemanticSpace,
    Cov <: resample4s.core.Coverage
](
    samples: SpaceEvidence[S],
    plan: resample4s.core.Plan[
      resample4s.core.Split[resample4s.core.Selection],
      Cov
    ],
    receipt: PlanReceipt
)
```

A cross-fit design additionally guarantees that every output row was prepared
without its own target. Exact-once coverage should remain a type-level
capability.

A pairing design is an identified graph over partitions, not a train/test
split:

```scala
final case class PairingEdge[P <: SemanticSpace](
    left: PartitionKey[P],
    right: PartitionKey[P],
    weight: Double,
    independence: IndependenceClaim,
    generalizesOver: GeneralizationAxis
)

final case class PairingDesign[P <: SemanticSpace](
    partitions: SpaceEvidence[P],
    edges: Vector[PairingEdge[P]],
    reducer: EdgeReducer
)
```

A cross-run crossnobis pairing is not leave-one-run-out validation, even when
both reuse related index machinery.

A randomization design binds lawful transformations to sample identity and an
exchangeability structure:

```scala
final case class RandomizationDesign[S <: SemanticSpace](
    samples: SpaceEvidence[S],
    exchangeability: ExchangeabilityStructure[S],
    randomizations: Plan[Permutation, ?],
    statisticReduction: RandomizationReducer
)
```

Permutation, bootstrap, sign flip, and validation should be distinct designs
sharing resample4s reindexing and deterministic random streams, not cases in a
weak common enum.

### Open estimands and compiler instances

Use concrete estimand values with result-dependent types:

```scala
trait Estimand:
  type Result
  def identity: EstimandIdentity

trait Compile[S, D, F, E <: Estimand]:
  type Bound
  def bind(
      source: S,
      design: D,
      frame: F,
      estimand: E
  ): Either[BindError, Bound]
```

Representative estimands:

```scala
final case class Classification[Y](workflow: ..., metrics: ...)
    extends Estimand:
  type Result = ClassificationEstimate[Y]

final case class Crossnobis(query: EffectQuery, noise: NoisePolicy)
    extends Estimand:
  type Result = LabeledRdm

final case class Rsa(models: Vector[ModelRdm], comparison: RdmComparison)
    extends Estimand:
  type Result = RsaEstimate

final case class ContrastEffect(query: EffectContrast)
    extends Estimand:
  type Result = EffectMap

final case class CanonicalEffect(spec: CanonicalSpec)
    extends Estimand:
  type Result = CanonicalEstimate
```

Adding an estimand requires its own result type, a compiler instance for
compatible source/design/frame combinations, and numerical plus semantic laws.
It must not require a new `RoiPayload`, `Response`, `AnalysisKind`, or registry
case.

### Scientific plans and execution plans are separate

```text
entry façade
  -> pure scientific specification
  -> bound scientific plan
  -> compiler
  -> execution plan
  -> numerical kernel
```

The scientific plan records:

- source and complete space identities;
- estimand and design semantics;
- measurement-frame identity;
- scientific normalizations and transformations; and
- requested output boundaries.

The execution receipt records:

- backend and storage representation;
- materializations performed;
- numerical precision;
- solver and convergence/stopping evidence;
- parallel schedule and chunking; and
- random seeds.

Switching from a dense to an operator-native path must preserve scientific-plan
identity while changing the execution receipt.

### Typed results retain local failure

```scala
final case class MeasurementValue[A](
    measurement: MeasurementId,
    outcome: Either[MeasurementFailure, A]
)

final case class AnalysisResult[A](
    plan: ScientificPlanIdentity,
    values: Vector[MeasurementValue[A]],
    receipt: ExecutionReceipt
)
```

This keeps the current engine’s valuable local-failure behavior without
closing the payload ontology. `A` might be a classification estimate,
regression estimate, labeled RDM, RSA estimate, effect form, canonical result,
or component fit.

Generic metrics are views rather than the scientific result:

```scala
trait Summarize[-A]:
  def metrics(value: A): MetricRecord
```

## Two kernels, one architecture

### Predictive kernel

Predictive analysis consumes

```text
Observations + typed target + validation/cross-fit design
```

and owns training/evaluation roles, leakage-safe preparation, tuning and nested
validation, fitted predictors, OOF predictions, metrics, and optional refit.

Alder should own this lifecycle. It already distinguishes:

- target-blind `Transform`;
- leakage-aware `FeatureMap`;
- terminal `Learner`;
- `Use.Train`, `Use.Validation`, `Use.Test`, and `Use.Refit`; and
- `Prepared.Reusable` versus `Prepared.LearnerReady`.

A measured ScalaFIM table should compile into an Alder workflow:

```text
measured local observations
  -> Alder Data adapter
  -> resample4s-backed validation plan
  -> target-blind transforms
  -> optional own-target-free, cross-fitted feature maps
  -> learner
  -> predictions
  -> metrics
```

Reuse Alder’s resample4s adapter rather than creating another fold
interpretation. ScalaFIM needs a batched/matrix-backed data adapter, stable
sample-key to Alder `RowId` conversion, matrix-native learner adapters where
row-wise extraction would waste work, and explicit materialization policy.

Boundary: `analysis-core` must not depend on Alder. Relational geometry, fixed
linear queries, and operator-native sufficient-statistic methods should not be
forced through a supervised example interface.

### Relational kernel

Relational analysis consumes

```text
RelationSet + pairing design + experimental/neural query
```

Its central second-order observable is

\[
\boxed{
\mathscr E_{LR}(H,K)
=
\operatorname{tr}\left(H^\top B_L K B_R^\top\right),
}
\]

where \(B_L,B_R\) are independently identified experimental–neural
relations, \(H\) closes or queries experimental space, and \(K\) closes or
queries neural space.

The rule is **unification at the compiler, specialization at the semantic
boundary**. Closing different boundaries yields different result types:

| Closed boundary | Scientific result |
|---|---|
| Neural pair | Effect form or RDM |
| Experimental pair | Neural evidence/coupling form |
| Both | Scalar statistic |
| Neither | Typed relation transport, normally operator-native |
| One first-order experimental boundary | Contrast pattern or univariate effect |
| Open form passed to a decomposition | Canonical or component result |

This allows univariate effects, RSA, coupling, and global decompositions to
share evidence transport without pretending to have identical outputs.

Crossform supplies the ontology and laws to port: identified left/right
experimental and neural spaces, ordered partition pairing,
independence/generalization declarations, forward/adjoint evidence transport,
first- and second-order queries, effect and measurement forms, open-boundary
results, compiler/kernel separation, and reversal/adjoint/direct-query laws.

Do not port Crossform’s entire R class hierarchy, R dispatch, universal plan
registry, storage codecs as scientific types, or every feature before the
basic Scala vertical slice works.

“One fit, many questions” applies only when a relation fit genuinely holds the
sufficient statistics under the same preparation and noise model. It must not
claim that changed prediction targets, target-aware preparation, covariance
estimation, or training never require refitting.

## Placement of method families

| Method family | Primary evidence | Design | Scientific operation |
|---|---|---|---|
| Classification/regression | `Observations[S,N]` | Validation, cross-fit, nested tuning | Fit learner and evaluate predictions |
| Univariate contrast | `RelationSet[P,E,N]` | Fixed, partition reduction, or group sampling | First-order closure \(c^\top B\) |
| Crossnobis/RDM | `RelationSet[P,E,N]` | Independent ordered partition pairing | Neural closure \(B_LKB_R^\top\) |
| RSA | Same as RDM | Pairing plus model query | Contract effect form with model \(H\) |
| ER-RSA | Left/right relation sets | Ordered cross-relation pairing | Rectangular effect form |
| Connectivity/coupling | Relation set | Pairing | Experimental closure \(B_L^\top H B_R\) |
| MANOVA/canonical effect | Relation moments | Training/evaluation partitioning when required | Generalized eigenproblem over effect/residual forms |
| PCA/PLS/CCA/global components | Observation table, relation table, or open form | Fit design when data-dependent | Decompose a declared open object |
| Permutation inference | Any statistic-producing plan | Randomization design | Rerun/update sufficient statistics under lawful reindexing |

Unification means identified spaces, typed observations/relations, explicit
closure of scientific boundaries, and compilation to valid sufficient
statistics and numerical paths. It does not mean “everything is
classification” or “everything is RSA.”

## Repository ownership boundaries

### Multivar

Own the semantic linear algebra:

- `SemanticSpace`, `SpaceRef`, and `SpaceEvidence`;
- `Lin`, `Table`, primal/dual orientation, adjoints, and composition;
- representation-independent operators;
- numerical certificates; and
- fitted-decomposition capabilities.

Strengthen complete space identity before adopting this as the common
substrate. Treat Multivar methods as kernels/capabilities, not a universal fit
lifecycle. Do not create another generic linear-algebra repository now. Keep
ScalaFIM’s dependency narrow and consider later extraction of the semantic core
only after another independent consumer proves the need.

### resample4s

Own finite ordinal reindexing and resampling:

- selection and reordered injection;
- bootstrap/repeated draws;
- permutations;
- repeated plans;
- exact and exact-once coverage;
- deterministic, domain-separated seed derivation; and
- plan receipts.

ScalaFIM adds the scientific binding from an ordinal plan to an identified
sample/partition axis and derives typed restriction legs.

### Alder

Own predictive fitting discipline:

- dataset-use roles;
- leakage-safe preparation;
- own-target-free, cross-fitted target-aware transformations;
- learners;
- validation, tuning, and refit;
- fitted-artifact audit; and
- model capabilities.

Do not copy this lifecycle into ScalaFIM classifiers.

### Crossform

Supply the relational ontology and laws, not a literal port of its R surface.

### ScalaFIM

Own the neuroimaging domain:

- fMRI time series/design to typed observations or relations;
- trial readouts and one-shot operator construction;
- estimability and temporal-preparation scope;
- voxel, surface, and atlas spaces;
- ROI/searchlight/basis/alignment frame builders;
- fMRI-specific bind-time validation;
- concise scientific façades;
- image/surface result scattering; and
- domain-specific execution receipts.

MVPA-specific estimands remain in ScalaFIM. Reuse generic PCA, CCA, operator,
and numerical capabilities from Multivar/Gale rather than reimplementing them
inside a domain module.

## Conceptual module direction

```text
                         Multivar semantic core
                                  |
            +---------------------+---------------------+
            |                                           |
    ScalaFIM evidence core                         resample4s
  axes, columns, observations,                          |
  relations, frames, plans/results                      |
            |                                           |
            +---------------------+---------------------+
                                  |
                   +--------------+--------------+
                   |                             |
          relational analysis            predictive analysis
       Crossform-derived laws             Alder integration
                   |                             |
                   +--------------+--------------+
                                  |
                       fMRI evidence adapters
                     time series, GLM, readouts
                                  |
                    voxel/surface frame builders
                                  |
                            `mvpa` façade
```

Possible logical modules:

| Module | Responsibility |
|---|---|
| `analysis-core` | Axes, columns, observations, relations, measurements, scientific plan/result identities |
| `analysis-design` | Axis-bound resample4s plans; validation, cross-fit, pairing, and randomization designs |
| `analysis-relational` | Effect/neural queries, forms/contractions, RDM/RSA/crossnobis, canonical/component adapters |
| `analysis-predict` | Alder adapters, learners/metrics, validation/tuning façades |
| `fmri-evidence` | fMRI series/design/readout to observations/relations; temporal preparation and estimability |
| `fmri-frame` | Voxel, surface, atlas, ROI, searchlight, basis, and alignment frames |
| `mvpa` | Concise user API and compatibility adapters |

Do not immediately perform a large physical module split. First enforce the
dependency direction inside the existing source tree and complete two vertical
slices. Split modules only after the boundaries prove themselves.

Current module names may evolve semantically:

- `mvpa-fit` is primarily fMRI evidence construction;
- `mvpa-spatial` should become frame construction;
- `mvpa-dataset` should become source and axis adaptation; and
- `mvpa` should remain an ergonomic façade rather than the ontology.

## Current-type migration ledger

| Current type | Decision |
|---|---|
| `PatternOperator` | Keep as implementation/backend adapter; expose it through typed `Table`. |
| `PatternMatrix` | Keep temporarily as dense compatibility storage; migrate scientific identity to spaces. |
| `PatternSource` | Keep as migration façade; ultimately use representation-independent tables plus measurement composition. |
| `FeatureSet` | Adapt to a sparse selection measurement. |
| `FeatureSetPlan` | Adapt to a `MeasurementFrame`; stop extending its kind enum. |
| `Response` | Deprecate as primary API; replace with typed columns and queries. |
| `Fold`/`FoldPlan` | Deprecate internally; translate to axis-bound resample4s designs. |
| `RoiAnalysis` | Retain only as source-compatible façade. |
| `RoiPayload` | Freeze; add no new cases. |
| `MvpaEngine` | Make a façade over bind/compile/execute. |
| `MvpaStream` | Re-express as streaming `MeasurementValue[A]`. |
| `RoiOutcome` | Preserve local failure, generalized to measurements. |
| `MetricVector` | Retain as summarization/rendering view, not the scientific result. |
| `OneShotDataset` | Rebuild as fMRI relation/observation evidence while keeping operator-native execution. |
| `CanonicalEffectMvpa` | Move sufficient-statistic logic into the relational compiler; return `AnalysisResult[CanonicalEstimate]`. |

During migration, `PatternSource` remains useful because it separates dense and
operator representations. The key change is to stop asking the source itself
to understand ROIs and searchlights.

## Illustrative public façade

The scientific core can remain rigorous without making common usage
ceremonial.

Predictive example:

```scala
val result =
  Mvpa
    .classify(dataset, target = _.condition)
    .across(Searchlights.radius(6.mm))
    .validate(LeaveOneRunOut)
    .using(
      Standardize()
        .andThen(LinearSvm(c = 1.0))
    )
    .score(Accuracy, BalancedAccuracy, ConfusionMatrix)
    .run
```

This lowers to an observation table, target column, measurement frame,
validation design, Alder workflow, and classification estimand.

Relational example:

```scala
val relations =
  FmriEvidence
    .fromRuns(runs)
    .estimate(design)
    .effects(effectSpace)
    .withResidualMoments

val result =
  Mvpa
    .crossnobis(relations)
    .across(Searchlights.radius(6.mm))
    .pair(CrossRun.allOrdered)
    .noise(ShrunkResidualPrecision())
    .run
```

One compatible relation source may support several typed queries:

```scala
val rdm = relations.across(rois).estimate(Crossnobis())
val rsa = relations.across(rois).estimate(Rsa(models))
val contrast = relations.across(rois).estimate(ContrastEffect(memoryVsControl))
val canonical = relations.across(rois).estimate(CanonicalEffect())
```

The result types remain distinct even when a compiler reuses the same runwise
relations and residual moments.

## Migration sequence captured in Part 2

These phases preserve architectural direction only. Do not schedule them until
all intake parts have been reconciled.

### Phase 0: freeze the accidental public center

- Add no new `RoiPayload` cases.
- Add no new meanings to `FoldPlan`.
- Add no analysis-specific fields to `FeatureSet`.
- Designate `RoiAnalysis` and `MvpaEngine` as compatibility APIs.
- Write a short architecture constitution covering identity, composition,
  leakage, and result laws.

This prevents the migration target from moving.

### Phase 1: spaces, axes, and legs

Implement:

1. enriched `SpaceDescriptor`;
2. `AxisRef[K]`;
3. axis-bound `Column[S,A]`;
4. `ReindexingLeg`;
5. `Measurement` and `MeasurementFrame`; and
6. adapters from current pattern matrices/operators, feature sets/plans, and
   folds.

Acceptance demonstration: current dense and operator analyses produce
identical results through the adapters.

### Phase 2: one complete predictive vertical slice

Move one linear classifier with leave-one-run-out validation onto:

```text
AxisRef
+ Observations
+ MeasurementFrame
+ resample4s
+ Alder
+ AnalysisResult[ClassificationEstimate]
```

Include fold-scoped standardization, exact sample-keyed OOF predictions,
deterministic receipts, dense/operator-backed input, and searchlight result
scattering. Make the old classification constructor delegate to this path.

### Phase 3: one complete relational vertical slice

Implement relation sets, pairing designs, measurement frames, typed
experimental/neural queries, and a Crossform-derived compiler. Port one-shot
crossnobis, RDM, and RSA over the same fitted relations.

Acceptance demonstrations:

1. direct scalar contraction equals materialize-then-query;
2. dense and operator-native paths agree;
3. pair reversal satisfies the transpose law;
4. pairing semantics participate in scientific-plan identity; and
5. one compatible relation fit answers several typed queries.

### Phase 4: canonical and global methods

Move canonical-effect sufficient-statistic methods onto the relational
plan/compiler system. Add Multivar-backed decompositions of observation tables,
relation tables, effect forms, and neural measurement forms. Let real use
reveal further common abstractions rather than presupposing them.

### Phase 5: deprecate the old ontology

After both vertical slices stabilize:

- deprecate `Response`, `FoldPlan`, and direct `RoiAnalysis` use;
- freeze `RoiPayload`;
- make `MvpaResult` a façade-rendered view of typed results; and
- require new methods to use estimands and compiler instances.

## Executable architectural laws

These should be release gates, not prose aspirations.

| Area | Required laws |
|---|---|
| Space identity | Equal dimension is insufficient; reorder fails without explicit reindexing; serialized and nominal descriptors agree. |
| Reindexing | Identity is neutral; composition associative; nested selection equals composed restriction; draw/permutation kinds are not silently widened. |
| Linear maps | Adjoint law; composition associativity within tolerance; sparse selection leg equals explicit slicing. |
| Measurement | Identity measurement is a no-op; hard selection agrees with current `FeatureSet`; frame order cannot alter measurement identity. |
| Dense/operator parity | Measured tables, sufficient statistics, and final results agree across explicit and operator-native paths. |
| Validation | Claimed train/assessment evidence is disjoint; exact-once coverage yields one OOF value per row; repeated exact plans are not called exact-once. |
| Leakage | Target-blind stages cannot inspect targets by type; target-aware preparation is own-target-free; learned preparation is training-scoped. |
| Pairing | Ordered edges stay ordered; reversal transposes forms; reducer weights are explicit; independence is not inferred from different edge IDs. |
| Evidence transport | Forward and adjoint contractions agree; direct query equals complete-form contraction. |
| Capabilities | Symmetry and PSD follow from construction or certification, not approximate numerical appearance. |
| Provenance | Scientific identity excludes backend/schedule; execution receipt records them; seed derivation is deterministic and domain-separated. |
| Extensibility | A new estimand and typed result compile without editing a central enum. |

## Architectural anti-goals

Do not build:

- a giant `Analysis[Any, Any]`;
- one closed `AnalysisKind` enum;
- one universal `Response`;
- one plan type conflating validation, partition pairing, and randomization;
- a universal result payload;
- local ordinals masquerading as semantic sample identity;
- dimension-only space compatibility;
- searchlight policy inside generic feature support;
- hidden materialization;
- another leakage protocol parallel to Alder;
- another resampling engine parallel to resample4s;
- an eager Kronecker representation of second-order transport;
- a plugin/codec registry in the mathematical core; or
- an unqualified “one fit, many questions” claim when preparation, target,
  covariance/noise estimation, or training has changed.

## Reconciliation with the Part 1 method

Part 1 specifies a new scientific method. Part 2 specifies the platform on
which that method should be expressible without creating another exceptional
source, plan, or result hierarchy.

Likely correspondences:

- The whole-brain identity measurement is the natural default frame for the
  pattern-first fit. Query ROIs are later measurement legs over the fitted
  neural space, but model-specific covariance restriction must not degrade into
  generic cropping.
- Part 1’s \(Y\) is an axis-bound target table or set of typed columns. Its
  feature basis, metric, scaling, and block weights participate in space and
  scientific-plan identity.
- \(A\), \(C\), \(\Psi\), support envelopes, rotations, and sufficient
  summaries belong in a typed fitted artifact with explicit neural, target, and
  component spaces.
- The anatomical adjacency graph for spatial regularization belongs to the
  identified neural-domain/basis contract, not to generic ordinals or a
  searchlight kind.
- Discovery, nested tuning, cross-fitting, independent confirmation, and
  randomization need purpose-specific designs even when they share resample4s
  maps.
- Haufe patterns, conditional-information maps, local predictions, and
  component/rank confirmations should be typed estimands or fitted-artifact
  capabilities, not new `RoiPayload` cases.
- Subject-to-common-space mappings are alignment/measurement legs, but group
  analysis must retain subject identity, uncertainty, target-coordinate
  compatibility, and heterogeneity rather than average arbitrarily rotated
  loading columns.
- Model comparisons must preserve scientific-plan identity and execution
  receipts so backend differences cannot silently become scientific
  differences.

One key design question remains for synthesis: the pattern-first method is
predictive, but its fitted forward operator and component forms also support
relational and decomposition queries. The final architecture must decide
whether it compiles as a predictive estimand exposing a typed relational
artifact, as a shared fitted-evidence object with predictive heads, or through
another capability-preserving boundary. It must not collapse the predictive
and relational kernels merely to make this one method appear unified.

## Commitment statement

ScalaFIM analysis is founded on identified finite scientific spaces and typed
linear evidence. Observation tables and partitioned experimental–neural
relations are the two primary evidence sources. Spatial localization is
represented by composable measurement legs. Validation, cross-fitting,
partition pairing, and randomization are explicit, distinct designs backed by
resample4s. Predictive estimands compile through Alder; relational estimands
compile through a Crossform-derived evidence-pairing engine. Multivar’s
semantic `Lin`/`Table` algebra is the common mathematical substrate. Results
are typed by their scientific output and carry separate scientific-plan
identity and execution receipts. `mvpa` remains a concise façade, not the
ontology.

The first architectural proof should be deliberately small but decisive: one
leave-one-run-out classifier and one operator-native crossnobis/RSA workflow
must use the same identified axes, measurement frame, plan/result shell, and
provenance model without exceptions or parallel result systems. If those two
scientifically distinct analyses fit cleanly, the foundation is credible.

## Decisions deferred until intake is complete

- exact public names and syntax;
- the concrete representation of complete space identity;
- how far nominal/path-dependent types should extend across serialization;
- the first resample4s and Alder versions/capabilities that can satisfy the
  laws without local duplicates;
- the minimal Crossform law subset for the first relational compiler;
- the physical module split and timing after logical boundaries are proven;
- the fitted-artifact boundary by which the pattern-first method spans
  prediction, forward relations, decomposition, and confirmation without
  conflating the two kernels;
- compatibility duration and source/binary deprecation policy; and
- the final implementation phases after all scientific and architectural
  intake parts have been reconciled.

## Source and qualification

- Primary source: user-supplied Part 2 architectural verdict, received
  2026-09-12.
- This record is architecture intake, not proof that current Multivar,
  resample4s, Alder, or ScalaFIM APIs already satisfy the described contracts.
- No implementation, dependency migration, compatibility commitment, or
  benchmark result is claimed here.
