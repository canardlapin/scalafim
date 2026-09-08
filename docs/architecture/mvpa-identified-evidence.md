# MVPA identified-evidence architecture

Status: ratified; vertical slices accepted; clean-break deletion pending

Date: 2026-08-24

Mote epic: `bd-01M0SV0ZFCE9154M7S9BBR3ZQ9`

The executable acceptance matrix for this decision is
[`mvpa-architecture-acceptance.md`](mvpa-architecture-acceptance.md).

## Decision

ScalaFIM MVPA has one architectural waist:

```text
identified scientific spaces
  + typed linear evidence
  + evidence design
  + measurement frame
  + open estimand
```

A scientific specification is bound and compiled before numerical execution:

```text
source + design + frame + estimand
  -> bind identities and required capabilities
  -> ScientificPlan
  -> compile
  -> ExecutionPlan
  -> AnalysisResult[A] + ExecutionReceipt
```

Two sibling kernels share that waist:

1. The predictive kernel fits and evaluates workflows over observations and
   typed targets.
2. The relational kernel asks first- and second-order questions of partitioned
   experimental-neural relations.

Neither kernel is encoded as a special case of the other. Prediction is not a
kind of representational geometry, and representational geometry is not a
learner with an unusual metric.

At the end of the epic there is one public ScalaFIM artifact and vocabulary,
`scalafim-fmri-mvpa` in `scalafim.fmri.mvpa`. The current fit, dataset, and
spatial MVPA artifacts are absorbed and removed. Internal source files may be
organized by kernel or domain, but users do not cross adapter namespaces or
choose among competing execution hierarchies.

## Complete scientific identity

Shape is not identity. An axis is identified by the ordered scientific values
it denotes and by the interpretation of its coordinates.

The canonical descriptor records at least:

```scala
final case class AxisIdentity(
    id: AxisId,
    size: AxisSize,
    coordinates: CoordinateSignature,
    basis: BasisDescriptor,
    units: Option[Units],
    scale: ScaleDescriptor,
    provenance: ProvenanceSignature
)
```

The representation may evolve, but these rules are fixed:

- ordered coordinates participate in identity;
- a coordinate permutation creates a different axis;
- an explicit reindexing leg relates the parent and child axes;
- equal dimensions never establish compatibility;
- storage layout, backend, and codec do not define a scientific axis;
- decoded runtime identity is checked against its nominal Scala witness before
  numerical access;
- implementation ordinals are never exposed as semantic sample or feature IDs.

An axis reference binds semantic keys to one ordered axis:

```scala
trait AxisIndex[K]:
  def size: Int
  def keyAt(ordinal: RowOrdinal): K
  def ordinalOf(key: K): Option[RowOrdinal]
  def coordinateSignature: CoordinateSignature

final class AxisRef[K] private (...):
  sealed trait Id extends multivar.core.SemanticSpace
  val evidence: multivar.core.SpaceEvidence[Id]
```

`SampleId`, `FeatureId`, `PartitionId`, and effect-coordinate keys are semantic
keys. `RowOrdinal` is private implementation machinery. A local ordinal may be
serialized only as part of an explicit reindexing receipt whose parent identity
is also present.

## Typed reindexing

ScalaFIM binds resample4s reindexings to identified axes. The ordinal map and
its scientific parent and child identities travel together.

```scala
final class ReindexingLeg[
    Parent <: SemanticSpace,
    Child <: SemanticSpace
] private (
    val parent: SpaceEvidence[Parent],
    val child: SpaceEvidence[Child],
    val reindexing: resample4s.core.Reindexing,
    val leg: Lin[Primal[Parent], Primal[Child]]
)
```

The child identity is derived from the parent identity, the exact ordinal map,
and the reindexing kind. Selection, injective reordering, draw with repetition,
and total permutation remain distinct. No API widens one into another merely
because each can be represented by integers.

Axis-bound columns use the same leg as evidence:

```scala
final case class Column[S <: SemanticSpace, A] private (
    rows: SpaceEvidence[S],
    values: IArray[A]
):
  def reindex[T <: SemanticSpace](by: ReindexingLeg[S, T]): Column[T, A]
```

Targets, run IDs, trial IDs, nuisance groups, subject IDs, and other metadata
are columns. Restricting evidence without restricting its columns by the same
leg is not representable.

## The two evidence sources

### Observations

Observational evidence is a sample-by-neural table:

```scala
final case class Observations[
    S <: SemanticSpace,
    N <: SemanticSpace
](
    samples: SpaceEvidence[S],
    neural: SpaceEvidence[N],
    patterns: Table[S, N]
)
```

A supervised source adds a typed target and whatever typed metadata its
workflow needs:

```scala
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

Dense, sparse, mapped, disk-backed, and operator-native storage are
representations of the same scientific table. They are not separate public
analysis hierarchies.

### Partitioned relations

A relation is an effect-by-neural estimate, commonly produced from one fMRI
partition as `B = A Y`, with a typed fit receipt:

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
    relations: Column[P, Relation[E, N]]
)
```

Residual moments, noise normalization, estimability, and degrees of freedom
are explicit capabilities. An estimand that needs one cannot bind without it;
an estimand that does not need it is not forced to fabricate an optional field.

## Measurement frames

Spatial localization and feature transformation are linear measurement legs:

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

For a sample table `X` and relation `B`, the same leg gives:

```text
X_m = X L_m^T
B_m = B L_m^T
```

A hard ROI is a sparse one-hot leg. Searchlights, surface patches, weighted
parcels, multiscale bases, and declared subject alignments use the same model.
A center is descriptive metadata for scattering a result, not a field required
by a generic feature subset.

The public measurement algebra contains only fixed scientific measurements.
Data-dependent projections are deliberately not representable as measurements:
the library will add them only when a validation or fit compiler can issue an
unforgeable, design-owned capability proving the fit scope. Until then, a
learned projection remains an internal workflow artifact and cannot enter a
measurement frame.

## Distinct designs

There is no universal split plan. Designs share resample4s ordinal machinery,
seed derivation, coverage evidence, and receipts, but keep different scientific
semantics and types.

### Validation design

A validation design assigns training, validation, assessment, and optional
refit roles. Coverage capabilities state whether out-of-fold outputs cover a
population and whether they cover it exactly once.

### Cross-fit design

A cross-fit design additionally proves that each output row was prepared
without its own target. Target-aware transformations can bind only through this
design or a stronger one.

### Pairing design

A pairing design contains ordered partition edges, weights, an explicit
independence claim, the generalization axis, and a reducer. A cross-run pairing
is not a train/test split, even when both happen to use two sets of ordinals.

### Randomization design

A randomization design contains the exchangeability structure, the lawful
permutation, draw, or sign-flip family, deterministic random streams, and the
statistic reduction. It composes with a statistic-producing scientific plan;
it does not masquerade as validation.

### Fit design

Data-dependent global decompositions receive an explicit fit/evaluate design
even when no prediction metric is requested. This is the scope boundary that
prevents a full-dataset PCA, CCA, or feature map from silently entering held-out
evaluation. Learned measurements remain outside the public algebra until this
design boundary can authorize them without a caller-forgeable receipt.

## Open estimands and typed results

An estimand owns its result type and is compiled through an open typeclass:

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

Adding an estimand requires its result type, compatible compiler instance, and
laws. It never requires editing a central analysis or payload enum.

Results preserve local measurement failure without erasing scientific type:

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

Classification estimates, out-of-fold predictions, labeled RDMs, RSA
estimates, contrast forms, canonical estimates, and component fits remain
different `A`s. Generic metric records are derived views, not the scientific
result container.

## Predictive kernel

The predictive kernel consumes observations, typed targets, and a validation,
cross-fit, nested-tuning, or fit design. Alder owns the lifecycle rules:

- target-blind transforms;
- target-aware cross-fitted feature maps;
- learners and fitted predictors;
- training, validation, test, and refit roles;
- tuning and out-of-fold prediction assembly;
- fitted-artifact audit.

ScalaFIM owns matrix-backed data conversion, neural measurement, fMRI metadata
binding, neuroimaging-specific validation, and typed result scattering. It does
not duplicate Alder's leakage protocol. Matrix-native learners and batching may
be added behind the lifecycle without changing scientific identity.

## Relational kernel

The irreducible second-order observable is:

```text
E_LR(H, K) = tr(H^T B_L K B_R^T)
```

`B_L` and `B_R` are independently identified experimental-neural relations;
`H` queries or closes experimental space; `K` queries or closes neural space.
The compiler chooses sufficient statistics and numerical representation. It
does not eagerly form a Kronecker transport.

Closing boundaries yields different result types:

| Boundary operation | Scientific result |
|---|---|
| first-order experimental closure | contrast pattern or univariate effect |
| close neural pair | effect form or labeled RDM |
| close experimental pair | neural evidence or coupling form |
| close both pairs | scalar statistic |
| decompose a declared open form | canonical or component result |

Crossnobis, RDM, RSA, ER-RSA, univariate effects, MANOVA-like statistics,
canonical effects, and relation-based components share compilation and laws.
They do not share a universal output payload.

## Scientific plan and execution receipt

Scientific identity includes:

- complete source-axis identities;
- the estimand and every normalization that changes it;
- design semantics, coverage, pairing, independence, and generalization;
- fixed measurement-frame identity;
- requested output boundaries.

Execution identity includes:

- dense, sparse, operator, or sufficient-statistic representation;
- materializations performed and their reason;
- backend, precision, solver, tolerances, and convergence evidence;
- chunking, parallel schedule, device, and memory policy;
- deterministic random-stream receipts.

Changing backend, chunking, or materialization does not change the estimand.
Changing pairing, normalization, fit scope, coordinates, or target definition
does.

Materialization is explicit in the execution plan and receipt. A public method
cannot silently turn an operator or disk-backed table into a dense matrix.

## Repository ownership

| Repository | Owns | Does not own |
|---|---|---|
| Multivar | semantic spaces, primal/dual orientation, `Lin`, `Table`, adjoints, composition, solver and decomposition capabilities | fMRI axes, ROI/searchlight builders, validation lifecycle |
| resample4s | finite ordinal reindexing, selections, injections, draws, permutations, coverage witnesses, deterministic plans and receipts | sample meaning, partition independence, exchangeability claims |
| Alder | predictive preparation, leakage-safe fitting, tuning, assessment, refit, prediction assembly and fitted-artifact audit | relational geometry and fMRI evidence construction |
| crossform | relational ontology and executable laws used as the scientific reference | ScalaFIM runtime classes, R dispatch, storage codecs |
| ScalaFIM MVPA | identified neuroimaging axes, observations and relations from fMRI data, designs bound to those axes, frames, estimands, compilers, typed results and image/surface scattering | duplicate linear algebra, resampling, or predictive lifecycle engines |

Crossform is a design and differential-oracle source, not a runtime dependency
or class hierarchy to port. rMVPA remains a named parity reference for shared
estimands, not an API template.

## Release laws

The following are executable release gates on JVM and Scala.js:

- same dimension is insufficient for axis compatibility;
- identity reindexing is neutral and composition is associative;
- coordinate reordering fails without an explicit leg;
- dense selection and a sparse one-hot measurement agree;
- evidence and every bound column undergo the same reindexing;
- semantic sample identity survives restriction, reordering, stacking, fold
  selection, prediction, and result assembly;
- dense, operator, and sufficient-statistic paths agree within declared
  tolerance;
- exact-once validation produces one output per sample and weaker coverage is
  not relabeled as exact-once;
- target-aware preparation is own-target-free and training-scoped;
- ordered pairing edges remain ordered and reversal transposes the form;
- direct relational queries equal complete-form contraction;
- symmetry and positive-semidefinite capabilities arise from construction or a
  certificate, never from observing approximately symmetric numbers;
- scientific plan identity excludes backend and scheduling while the execution
  receipt records them;
- a new estimand compiles without editing a central enum.

## Vertical slices

The architecture is admitted only after two unlike workflows use the same
waist without exceptions.

### Predictive slice

Leave-one-run-out linear classification over an ROI and a searchlight frame,
with fold-scoped standardization, exact out-of-fold prediction assembly,
noncanonical sample IDs, dense/operator parity, deterministic receipts, and
result scattering.

### Relational slice

Operator-native crossnobis and RSA over the same runwise relation source, with
ordered cross-run pairing, residual-noise capability, direct-versus-materialized
contraction parity, pair-reversal laws, multiple compatible queries from one
relation fit, and result scattering.

Canonical/global methods migrate only after both slices pass.

## Clean-break migration rules

- New development targets this ontology only.
- There is no compatibility package, deprecated forwarding façade, public type
  alias, dual result hierarchy, or runtime adapter registry.
- A temporary bridge must be package-private, branch-local, linked to a Mote
  deletion ticket, and removed in the same epic.
- Old tests are rewritten around scientific laws or independent estimands;
  tests whose sole purpose is source compatibility are deleted.
- Existing numerical kernels may be retained after their inputs, outputs, and
  fit scopes are recast in the new model.
- The cutover is atomic at the public boundary. The old and new public
  ontologies are never released together.

## Final deletion gate

The epic cannot close until all of the following are true:

1. The fit, dataset, and spatial MVPA artifacts and their module directories no
   longer exist.
2. No production, test, example, build, README, or current documentation file
   refers to the retired source, fold, feature-set, per-region analysis, result,
   or payload ontology.
3. No compatibility or legacy namespace exists.
4. Both vertical slices and the generated law court pass on JVM and optimized
   Scala.js.
5. Independent parity fixtures name their estimand, generator, source version,
   tolerance, and accepted non-equivalence boundaries.
6. Clean downstream consumers compile against only the unified artifact.
7. The zero-cruft verifier passes in final mode.

The historical migration record may retain retired names. It is not linked as
current API documentation and is the sole text-search exemption.
