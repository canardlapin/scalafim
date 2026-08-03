# Response, Representation, Archive, and Dataset Architecture

Status: accepted architectural north star; Phases 0 through 10 implemented

Date: 2026-07-26

Scope: `response`, `latent`, `archive`, `dataset`, their interop modules, and
runtime assembly

Implementation status: the Phase 0 contract, Phase 1 response kernel, Phase 2
archive revision/resource split, their
[Track-A receipt/capability integration](response-representation-archive-track-a.md)
and the
[Phase 3 typed temporal-DCT plan](response-representation-archive-phase-3.md)
and
[Phase 4 LNA binding](response-representation-archive-phase-4.md)
and the
[Phase 5 response registry/runtime](response-representation-archive-phase-5.md)
are implemented. The
[Phase 6 dataset attachment](response-representation-archive-phase-6.md)
and the
[Phase 7 Zarr format-independence proof](response-representation-archive-phase-7.md)
and the
[Phase 8 module-boundary extraction](response-representation-archive-phase-8.md)
and the
[Phase 9 normalized manifest](response-representation-archive-phase-9.md)
and the
[Phase 10 law and release audit](response-representation-archive-phase-10.md)
are also implemented.

## 1. Executive Summary

ScalaFIM needs one coherent way to compose:

- scientifically addressable fMRI datasets;
- exact and approximate response representations;
- durable, verifiable archive formats;
- local, remote, JVM, and browser-backed reads.

These are not three implementations of one interface. They are three
orthogonal interpretations of the same scientific response:

- `dataset` determines which response and which output-domain observations the
  caller means;
- `latent` defines how that response is represented and reconstructed;
- `archive` defines how representation artifacts are stored, located,
  verified, and recovered;
- explicit interop modules translate between those interpretations;
- the application runtime chooses which formats, representations, and stores
  are trusted and supported.

For one coherent acquisition, the semantic object is:

```text
Yₖ : Timeₖ × Sampleₖ → Double
```

A study is an indexed family of such response fields:

```text
𝒴 = { Yₖ } for k in acquisition identity
```

Runs may differ in duration, timing, mask, geometry, surface or volume domain,
calibration, and acquisition identity. ScalaFIM must not manufacture one global
rectangular response or one continuous elapsed-time axis where none exists.

The architectural design rule is:

> Selection travels downward. Data, evidence, and provenance travel upward.

The target execution path is:

```text
DatasetSelection
  ↓ semantic resolution
ResolvedDatasetRead
  ↓ acquisition-local selection
ResolvedResponseSelection
  ↓ representation compilation
DecodePlan[ResponseBlock]
  ↓ collect typed logical reads
LogicalPayloadRead[V]
  ↓ archive binding
PayloadPlan[V]
  ↓ layout and physical planning
PhysicalReadPlan
  ↓ effectful execution
Observed[V] + ReadReceipt
  ↓ evaluate the pure decode plan
ReadResult(ResponseBlock, Provenance, ReadReceipt)
```

This document defines the target architecture, product requirements,
acceptance laws, implementation sequence, evolution policy, and decision gates.
It does not declare the target module graph already implemented.

## 2. Relationship to Existing Plans

This PRD governs future inter-module responsibility and composition.

It does not erase or retroactively invalidate:

- [`neuroarchive-dataset-bridge.md`](neuroarchive-dataset-bridge.md), which
  records the implemented and verified bridge through the current
  `DatasetBackend`, LNA, latent, and Zarr types;
- [`neuroarchive-data-access.md`](neuroarchive-data-access.md), which defines
  NeuroArchive data-access goals, publication, identity, and remote addressing;
- [`neuroarchive-zarr-core.md`](neuroarchive-zarr-core.md), which defines the
  generic Zarr kernel and canonical dense-BOLD profile.

When these documents overlap:

- this PRD owns boundaries among response, representation, archive, dataset,
  interop, and runtime;
- format plans own their wire formats, physical layouts, conformance subsets,
  and format-specific performance gates;
- the implemented bridge plan remains the current-state behavioral
  baseline until the corresponding phase is complete.

The current bridge is therefore a baseline to wrap and migrate, not a failed
prototype to replace in one rewrite.

Phase 0 must name the exact committed baseline that this program migrates. The
record includes:

- the Git commit;
- the Scala version;
- the module graph;
- the committed bridge implementation and fixtures;
- any prerequisite commits that live on another branch when this PRD is
  accepted.

A passing uncommitted working tree is useful evidence, but it is not a durable
migration baseline.

## 3. Problem Statement

The live system has useful implementations, but its dependency structure
conflates responsibilities:

- `latent` depends on `archive` and contains LNA persistence codecs;
- `archive` contains quantization, temporal DCT, basis embedding, and
  reconstruction behavior that changes numerical meaning;
- `dataset` depends on both `archive` and `latent`;
- dataset backends inspect representation and archive details;
- LNA and Zarr reach `FmriDataset` through different privileged paths;
- selected reconstruction does not by itself prove bounded physical IO;
- provenance and physical read evidence are not consistently separated;
- synchronous JVM and asynchronous browser paths do not share one explicit
  resource/effect contract.

These couplings make several desirable operations harder than they should be:

1. Add a new representation without modifying archive or dataset core.
2. Store the same representation in LNA/HDF5 and Zarr.
3. Inspect and verify an archive whose representation is unsupported.
4. Run the same typed reconstruction plan against memory, HDF5, Zarr, or a
   traced test interpreter.
5. State honestly whether a selected read performed bounded physical IO.
6. Test representation fidelity separately from archive persistence fidelity.
7. Keep dataset hierarchy semantics identical across dense, latent, local, and
   remote sources.

The system needs explicit seams, not a universal codec or inheritance tree.

## 4. Product Vision

The mature user experience is:

```scala
runtime
  .openDataset(location, dataset, acquisition)
  .use: dataset =>
    dataset.read(selection)
```

The runtime performs:

```text
location
  → physical archive driver
  → structurally valid OpenArchive
  → explicitly installed representation family
  → archive/representation binding
  → ResponseSource
  → checked acquisition attachment
  → FmriDataset
```

After opening, scientific consumers do not branch on:

- LNA versus Zarr;
- DCT versus Haar versus BOLDZip;
- local file versus HTTP;
- dense versus reconstructed values.

They read an attached response source through dataset semantics and receive:

- the requested response values;
- their exact resolved selection;
- reconstruction provenance and contract;
- an execution receipt describing actual IO and integrity work.

Source abstraction must hide incidental mechanics without hiding scientific
meaning, approximation, or physical-access evidence.

## 5. Goals

### G1. Orthogonal module ownership

Archive, representation, and dataset responsibilities are separately testable
and connected only through small explicit adapters.

### G2. One response-domain contract

Dense, represented, generated, local, and remote response sources satisfy one
typed contract over a coherent acquisition field.

### G3. Pure, typed lowering

Scientific selections lower through inspectable typed plans before external
effects occur.

### G4. Open representation extensibility

An application can add a namespaced representation family without modifying a
closed enum or central archive dispatch function.

### G5. Stable, inspectable archives

Unknown representations do not make an archive structurally unreadable.
Archive validation, inventory, copying, and integrity checks remain possible.

### G6. Format-independent representation semantics

The same representation decoder can operate over memory, LNA/HDF5, Zarr, or a
future store by changing bindings and payload interpreters, not mathematical
code.

### G7. Honest partial access

Logical selection, planned payload demand, physical locality guarantees, and
observed reads are distinct and visible.

### G8. Scientific traceability

Every reconstructed result retains the representation contract and derivation
provenance. Every execution can report a separate read receipt.

### G9. Cross-platform truth

Portable contracts, plans, laws, and mathematical kernels compile and pass on
both JVM and Scala.js. Platform IO remains behind explicit interpreters.

### G10. Incremental migration

The architecture can be introduced through vertical slices while current LNA,
Zarr, dataset, model, and fit workflows continue to work.

## 6. Non-Goals

This PRD does not authorize:

- one `CompressionCodec` that encodes responses, writes archives, opens
  datasets, and performs selections;
- `ArchiveBackend extends LatentResponse` or
  `LatentResponse extends DatasetBackend`;
- arbitrary code loading based on file contents;
- a global mutable representation or codec registry;
- dispatch on Scala class names;
- untyped `Map[String, Any]` payload exchange;
- automatic resampling or coordinate transformation during dataset attachment;
- treating every study as one dense global matrix;
- type-level arithmetic over file dimensions learned only at runtime;
- replacing the existing Zarr kernel with a new archive-specific array engine;
- requiring every direct dense source to pass ceremonially through latent
  representation machinery;
- distributed execution, Spark integration, repository catalogs, or mutable
  archive publication as part of the first migration;
- renaming every artifact or package before the seams have been proven;
- changing validated numerical behavior merely to satisfy the new structure.

## 7. Users and Primary Use Cases

### 7.1 Scientific dataset consumer

A model or fitting workflow selects runs, timepoints, voxels, surfaces, masks,
or events and reads response values without knowing their storage or
representation.

### 7.2 Representation author

An author implements DCT, Haar, HRBF, shared dictionary, transported space, or
BOLDZip semantics once, including validation, selected reconstruction, and a
declared reconstruction contract.

### 7.3 Archive format author

An author implements LNA/HDF5, Zarr, directory-object, or in-memory archive
drivers without importing DCT, Haar, HRBF, BOLDZip, or dataset hierarchy logic.

### 7.4 Application assembler

An application explicitly chooses supported archive drivers, representation
families, bindings, resource limits, and optional discovery mechanisms.

### 7.5 Archive inspector

A tool opens, inventories, validates, copies, or audits an archive even when it
cannot reconstruct the represented response.

### 7.6 JVM and browser reader

The same semantic selection and pure planning rules execute through
platform-appropriate resource and IO interpreters without blocking Scala.js.

### 7.7 Test and instrumentation author

A test executes a representation plan against an in-memory or recording
payload interpreter and proves selection, ordering, locality, integrity, and
commutation laws.

## 8. Governing Principles

### 8.1 Selection down, evidence up

Downward values become more concrete:

```text
scientific query
→ response-domain selection
→ logical representation reads
→ archive payload reads
→ physical object and byte reads
```

Upward values accumulate evidence:

```text
bytes and integrity evidence
→ typed logical payloads
→ reconstructed response block
→ scientific provenance
→ attached dataset result
```

### 8.2 Typeclasses for canonical semantics; values for choices

Good typeclass candidates have one coherent meaning:

- logical-payload scalar evidence such as `PayloadScalar[A]`;
- `Representation[R]`;
- `ManifestCodec[Header]`;
- representation-specific decode-path consistency.

Ordinary explicit values express policy:

- representation registry;
- archive driver registry;
- representation encoder;
- archive format;
- DCT rank;
- quantization policy;
- residual policy;
- compression level;
- resource limits.

Givens must not become a hidden dependency-injection system.

### 8.3 Runtime dimensions remain runtime values

Opaque indices and domain identities prevent accidental interchange. The
design does not pretend that dimensions parsed from a file are compile-time
constants.

### 8.4 Plans precede effects

Metadata validation, selection resolution, reconstruction planning, payload
binding, and layout planning are pure whenever their required metadata is
already available.

Effects enter at:

- resource acquisition and release;
- file, HTTP, or object-store reads;
- writes and publication;
- streaming;
- verification of payload contents;
- external dependency resolution.

### 8.5 Archive reads recover logical payloads exactly

Everything below the logical-payload boundary is lossless relative to the
declared logical payload value. Any transformation that changes numerical meaning or
reconstruction fidelity belongs above that boundary.

### 8.6 Versioned formats are explicit

Container-specific formats and eager paths remain behind factual, versioned
adapters. No deprecated alias, historical naming layer, or closed central
dispatch is admitted before the first release.

## 9. Ownership Matrix

The following test settles responsibility:

| Question | Owner |
| --- | --- |
| Does it define the output response domain or response block? | `response` |
| Does it own executable volume geometry? | `image` |
| Does it own mesh geometry or topology? | `surface` |
| Does it change numerical meaning or reconstruction fidelity? | `latent` |
| Does it losslessly map logical scalars to bytes or bytes to bytes? | `archive` |
| Does it determine which acquisition, timepoints, samples, masks, or events are requested? | `dataset` |
| Does it map representation slots to archive payload identities? | archive/representation interop |
| Does it attach a response source to acquisition metadata? | `dataset` |
| Does it validate concrete geometry against a response-domain reference? | dataset/domain adapter |
| Does it choose installed implementations and policy? | runtime |

Consequences:

- rank reduction belongs to `latent`;
- quantization belongs to `latent` when it changes logical numerical values;
- residual omission and residual interpretation belong to `latent`;
- signal-level delta coding belongs to `latent` when reconstruction must
  understand it;
- exact delta coding may live below the payload boundary only when archive
  execution restores the original logical payload before returning it;
- gzip, Zstandard, Blosc, byte order, checksums, chunking, and sharding belong
  to archive or its storage kernel;
- subject/session/task/run resolution belongs to `dataset`;
- dataset coordinates resolve to output response samples, not coefficient
  coordinates;
- lowering logical `coefficients`, `basis`, or `residuals` slots to LNA or
  Zarr payload references belongs to interop.

## 10. Vocabulary

### Response field

One coherent calibrated real-valued field:

```text
Y : Time × Sample → Double
```

### Response schema

The identified time domain, sample domain, signal semantics, and shape of a
response field.

### Output-domain selection

A selection of time and sample indices in the decoded response field. It does
not contain representation-internal coefficient or atom coordinates.

### Representation

A mathematical account of how a response is encoded and reconstructed,
including approximation and validation semantics.

### Logical payload

A typed dense block or domain value named by a representation slot such as
`coefficients`, `basis`, `offsets`, `mask`, or `residual-events`.

### Archive payload

A typed logical value referenced by an archive manifest and recoverable through
an archive payload plan.

### Physical object

A file, HDF5 dataset, Zarr chunk or shard, byte range, or object-store key used
to recover archive payloads.

### Archive revision

Pure immutable identity and metadata for one persisted object graph.

### Open archive

A resource-backed capability for executing reads against an archive revision.

### Reconstruction contract

The exact or approximate relationship promised between a representation and
the response it reconstructs.

### Provenance

Durable derivation evidence describing what a value is and how it was
produced.

### Read receipt

Execution-specific evidence describing what logical and physical reads
actually occurred.

## 11. Target Module Graph

The logical target is:

```text
                       response
                  /   |      |      \
      response-laws latent dataset   archive
                     ^       ^         ^
                     |       |         |
                  linalg  geometry   format modules
                         adapters
                        /        \
                     image      surface

             response + latent + archive
                         |
              interop-archived-response
                         |
                       runtime

archive
  ├── archive-lna
  ├── archive-zarr
  └── archive-object-store
```

Normative dependency rules:

```text
response ─────▶ no internal scientific module

response-laws ▶ response
latent  ──────▶ response + linalg
dataset ──────▶ response
archive ──────▶ response vocabulary only where the manifest must name it

geometry-domain adapters
  ├───────────▶ dataset + image
  └───────────▶ dataset + surface

archive-lna  ─▶ archive
archive-zarr ─▶ archive + zarr

interop-archived-response
  ├───────────▶ response
  ├───────────▶ latent
  └───────────▶ archive

runtime ──────▶ selected APIs, implementations, and interop modules
```

The desired production-source test is:

> If a main-source file imports two principal domains, it belongs in their
> interop module or at the runtime composition root.

Integration tests, examples, and runtime wiring may deliberately import
multiple domains.

The exact artifact names may be staged. The architectural requirements are:

- `response` does not import image, surface, graph, archive, latent, or dataset;
- `latent` core no longer imports archive;
- `dataset` core no longer imports archive or concrete representation
  families;
- `archive` core no longer executes scientific reconstruction;
- implementation modules do not create dependency cycles.

The live Phase 8 graph records two secondary-domain edges that are outside the
prohibited principal-domain coupling: `latent -> image + locus-kernel` for
existing HRBF geometry and ordered-mask mathematics, and
`archive-lna -> image` for the established owned matrix wire values in the LNA
schema. Cross-domain integration still lives in
`interop-archived-response`; these edges do not put archive policy in latent,
representation dispatch in dataset, or reconstruction in generic archive.

## 12. Response Kernel

### 12.1 Responsibility

The response kernel owns only vocabulary and laws that require one meaning
across dense sources, representations, archives, and datasets:

- axis kinds and typed indices;
- runtime domain identities;
- response schemas;
- response-local selections;
- owned response blocks;
- read capabilities;
- response-source planning and execution boundaries;
- reconstruction contracts and result envelopes.

It does not own:

- archive digests or integrity algorithms;
- dataset subject/session/task hierarchy;
- DCT, Haar, HRBF, transport, or BOLDZip models;
- HDF5, Zarr, NIfTI, BIDS, HTTP, or filesystem mechanics;
- fitting or model semantics.

### 12.2 Axis-safe indices

Illustrative API:

```scala
package scalafim.response

sealed trait TimeAxis
sealed trait SampleAxis

opaque type AxisIndex[A] = Int

object AxisIndex:
  def fromInt[A](
      value: Int
  ): Either[IndexError, AxisIndex[A]] =
    Either.cond(
      value >= 0,
      value,
      IndexError.Negative(value)
    )

  private[response] def unsafe[A](
      value: Int
  ): AxisIndex[A] =
    value

  extension [A](index: AxisIndex[A])
    inline def value: Int = index
```

This prevents time indices from being passed as sample indices. It does not
prevent two different sample domains from sharing the same integer range, so
runtime domain identity is also required.

### 12.3 Domain-tagged ordered indices

```scala
opaque type DomainId[A] = String

final class OrderedIndices[A] private (
    val domain: DomainId[A],
    private val raw: IArray[Int]
):
  def size: Int = raw.length

  def apply(position: Int): AxisIndex[A] =
    AxisIndex.unsafe(raw(position))
```

Requirements:

- values preserve request order;
- bounds are checked against the referenced domain;
- duplicate behavior is explicit;
- primitive index storage is created only through companion factories;
- no caller outside `response` constructs `Array[AxisIndex[A]]` or supplies a
  `ClassTag[AxisIndex[A]]`;
- physical planners may sort, coalesce, or deduplicate internally only when
  result assembly restores requested semantics.

The first dataset adapter preserves current behavior and rejects duplicate
scientific selections. Lower-level response and storage planners may support
ordered gathers with duplicates. A later public duplicate-selection feature
requires a separate decision and semantic review.

### 12.4 Response schema

Illustrative shape:

```scala
final case class ResponseSchema(
    id: ResponseSchemaId,
    time: TimeDomain,
    samples: SampleDomain,
    signal: SignalSchema
)
```

`TimeDomain` must represent identified finite acquisition-local time:

- regular sampling with origin, interval, count, and units;
- explicit ordered coordinates with units;
- no implicit continuity across acquisitions.

`SampleDomain` must represent at least:

- volumetric voxels with a volume-space identity, optional mask identity, and
  ordering;
- surface vertices with surface identity, topology identity, and ordering.

The first implementation may support only the live volumetric path, but the
kernel must not define “sample” as “coefficient” or bake in rank-four storage.

`SampleDomain` carries finite identity, cardinality, ordering, and
content-verifiable geometry or topology references. It does not contain
`NeuroSpace`, mesh arrays, graph values, or coordinate-resolution algorithms.
Image and surface modules retain those concrete values. Dataset-domain adapters
compare them with the references during attachment.

`SignalSchema` records:

- units;
- calibration already applied at the response boundary;
- missing/non-finite policy;
- decoded response values are `Double`.

Quantized, float32, integer, or other stored scalar types remain logical
payload concerns. A future complex response requires a separate reviewed
response contract rather than a scalar parameter threaded through every
dataset and runtime type.

### 12.5 Resolved response selection

```scala
final case class ResolvedResponseSelection(
    schema: ResponseSchemaId,
    timepoints: OrderedIndices[TimeAxis],
    samples: OrderedIndices[SampleAxis]
)
```

The schema identity is mandatory. A dimensionally compatible selection
resolved for schema A must not be accepted by schema B.

### 12.6 Owned response block

The kernel must not expose Breeze or Gale matrices as its permanent public
contract. It owns one specialized row-major response block. It does not define
`Tensor2[A]` or a general tensor framework:

```scala
final class ResponseBlock private (
    private[response] val ownedRowMajor: NArray[Double],
    val rows: Int,
    val columns: Int,
    val selection: ResolvedResponseSelection
):
  inline def apply(row: Int, column: Int): Double =
    ownedRowMajor(row * columns + column)

object ResponseBlock:
  def copyFromRowMajor(
      values: NArray[Double],
      selection: ResolvedResponseSelection
  ): Either[ResponseShapeError, ResponseBlock]

  private[response] def fromOwnedRowMajor(
      values: NArray[Double],
      selection: ResolvedResponseSelection
  ): Either[ResponseShapeError, ResponseBlock]
```

Required invariant:

```text
rows    = selected timepoint count
columns = selected sample count
```

The implementation must:

- have explicit primitive-buffer ownership;
- support zero-copy adoption at trusted internal boundaries;
- expose no public mutable buffer or ownership-preserving `copy`;
- keep hot row-major loops allocation-conscious;
- copy when adapting to a public matrix whose mutable buffer can escape;
- permit zero-copy transfer only when the source relinquishes ownership;
- cross-compile to JVM and Scala.js.

The existing `DMat` establishes useful row-major and cross-platform evidence,
but its public mutable buffer prevents it from satisfying this ownership
contract unchanged.

### 12.7 Response source

Normative semantics:

```scala
trait ResponseSource[F[_]]:
  type Plan

  def schema: ResponseSchema
  def capabilities: ReadCapabilities

  def plan(
      selection: ResolvedResponseSelection
  ): Either[ReadPlanningError, Plan]

  def execute(
      plan: Plan
  ): EitherT[F, ReadError, ReadResult]
```

A convenience `read` may compose `plan` and `execute`.

Multi-source composition uses an explicit dependent pair:

```scala
sealed trait PlannedRead[F[_]]:
  type P
  val source: ResponseSource[F] { type Plan = P }
  val plan: P
  def summary: ReadPlanSummary
```

This lets a composite retain heterogeneous source-specific plans without
casts, `Any`, string association, or an untyped common plan.

Requirements:

- planning is pure;
- execution is effectful;
- direct dense sources may define specialized plans;
- represented sources may use `DecodePlan`;
- callers need not know the source’s plan type;
- plans remain inspectable through source-specific diagnostics or a common
  summary;
- resource lifetime is external to immutable schemas and selections.

Typelevel notation expresses the required effect and error semantics. The
exact dependency admission is decided in the effect-boundary gate, but any
alternative must preserve resource safety, typed operational errors, and
JVM/Scala.js behavior without blocking.

## 13. Dataset Semantics

### 13.1 Dataset owns scientific identity and query resolution

`FmriDataset` is the pure scientific description and query compiler. It owns:

- subject, session, task, run, acquisition, echo, and space identity;
- event and acquisition metadata;
- coordinate and mask resolution;
- run-local segmentation;
- cross-run assembly policy.

`OpenedDataset[F]` pairs that description with checked, resource-backed
response sources:

```scala
trait OpenedDataset[F[_]]:
  def dataset: FmriDataset

  def plan(
      query: DatasetRunQuery,
      selection: DataSelection
  ): Either[DatasetError, ResolvedDatasetRead]

  def execute(
      plan: ResolvedDatasetRead
  ): EitherT[F, DatasetError, DatasetReadResult]
```

Effects stay at this opened execution boundary. `FmriDataset`, dataset queries,
model descriptions, and fit plans do not acquire an `F[_]` parameter.
Convenience `read` methods may compose pure planning with `execute`.

Neither type owns:

- coefficient coordinates;
- basis selection;
- representation dispatch;
- archive manifest validation;
- physical layout or byte codecs.

### 13.2 Study and acquisition scope

A study-level query resolves to one or more acquisition-local reads:

```scala
final case class ResponseRead(
    key: ResponseKey,
    selection: ResolvedResponseSelection
)

final case class ResolvedDatasetRead(
    reads: NonEmptyVector[ResponseRead],
    assembly: AssemblyPolicy
)

final case class DatasetReadResult(
    segments: NonEmptyVector[ReadResult],
    assembly: AssemblyPolicy
)
```

`ResolvedDatasetRead` is an internal compiler IR. It does not replace the
existing public `DataSelection`, `DatasetRunQuery`, or typed dataset keys.

### 13.3 Output-domain resolution

Dataset resolution ends at:

```text
world/scientific coordinate
→ output response sample index
```

It must not produce:

- coefficient indices;
- dictionary atom indices;
- latent rows;
- transported coordinates;
- residual-stream offsets.

Those translations are representation semantics and occur during decode-plan
compilation.

### 13.4 Checked attachment

Canonical attachment shape:

```scala
def attach[F[_]](
    dataset: FmriDataset,
    source: ResponseSource[F],
    acquisition: AcquisitionContext
): ValidatedNec[
  AttachmentIssue,
  OpenedDataset[F]
]
```

Attachment validation is pure. The returned opened value retains the source
only after its schema and acquisition context agree.

Attachment validates:

- acquisition and run identity;
- time cardinality and time-coordinate correspondence;
- sample-domain identity;
- geometry;
- mask identity and sample ordering;
- scalar units and calibration assumptions;
- event referential integrity.

If source and acquisition orderings differ by an explicit bijection,
attachment may construct a checked `SampleAlignment`.

If they require interpolation, resampling, or coordinate transformation, the
caller must create a new explicitly transformed source with its own
provenance before attachment. Attachment never silently resamples.

### 13.5 Multi-acquisition assembly

The default result remains segmented.

Grouping, concatenation, and reduction are distinct:

- grouping reorganizes segments without changing values;
- block concatenation changes layout while retaining boundaries;
- reduction changes values under caller-supplied policy.

No operation fabricates continuous elapsed time across runs or sessions.

### 13.6 Current dataset surfaces

The first adapter must:

- accept current `DataSelection`;
- reuse current subject/session/task/run query types;
- preserve exact-grid coordinate semantics;
- preserve duplicate rejection at the public dataset boundary;
- preserve `SegmentedFmriSeries` behavior;
- adapt `ResponseBlock` to the current `FmriSeries` and matrix types;
- retain checked `FmriDataset.open` as the explicit synchronous construction
  path;
- retain synchronous `seriesEither` only for current synchronous or already
  materialized sources;
- never block an asynchronous or browser source behind a synchronous facade;
- keep `FmriModel`, `FitPlan`, and numerical kernels effect-free;
- pass `OpenedDataset[F]` or a narrower dataset-reader capability to effectful
  model, fit, and MVPA executors.

The synchronous facade and the effectful opened API must not share a name in a
way that hides which operations can perform effects.

## 14. Representation Semantics

### 14.1 Public abstraction

The module may remain named `latent`, but the durable public abstraction is:

```text
ResponseRepresentation
```

or:

```text
Representation
```

This includes identity, low-rank, sparse, transported, dictionary, residual,
and other structured response representations that are not all naturally
described as latent variables.

### 14.2 Pure model, materialized value, opened source

Effects do not belong in the mathematical model.

```scala
final case class RepresentedResponse[R, P](
    model: R,
    payloads: P
)

type MaterializedRepresentation[R] =
  RepresentedResponse[R, LogicalPayloadBundle]

type OpenedRepresentation[F[_], R] =
  RepresentedResponse[R, LogicalPayloadInterpreter[F]]
```

The exact aliases may change, but these states must remain distinguishable:

- pure representation metadata and semantics;
- owned logical payload values ready for persistence;
- resource-backed logical payload access;
- type-erased consumer-facing `ResponseSource`.

### 14.3 Representation typeclass

Illustrative API:

```scala
trait Representation[R]:
  def key: RepresentationKey

  def describe(
      model: R
  ): RepresentationDescriptor

  def validate(
      model: R
  ): ValidatedNec[RepresentationIssue, Unit]

  def compile(
      model: R,
      selection: ResolvedResponseSelection
  ): Either[
    RepresentationError,
    DecodePlan[ResponseBlock]
  ]
```

The representation owns:

- mathematical invariants;
- output response schema;
- conversion from output-domain selection to internal coordinates;
- logical payload requests;
- reconstruction kernel;
- exact or approximate reconstruction contract;
- decode-path consistency used to compare selected, partitioned, in-memory, and
  archive-backed executions of the same represented value.

It does not own:

- HDF5 or Zarr paths;
- archive payload IDs;
- object-store keys;
- chunking;
- byte compression;
- integrity checks.

### 14.4 Representation keys

Representation identities are open and namespaced:

```scala
final case class RepresentationKey(
    namespace: String,
    name: String,
    majorVersion: Int
)
```

Example:

```text
org.scalafim / temporal-dct / 1
```

They are not a closed Scala enum.

The following versions remain distinct:

- representation schema version;
- archive format version;
- object schema version;
- encoder implementation version;
- decoder implementation version.

### 14.5 Typed logical reads

Logical request/result association must be encoded in types:

```scala
final case class LogicalSlot[V](
    role: LogicalPayloadRole,
    expected: LogicalValueConstraint[V]
)

sealed trait LogicalPayloadRead[V]

object LogicalPayloadRead:
  final case class SliceOf[V](
      slot: LogicalSlot[V],
      slice: PayloadSlice
  ) extends LogicalPayloadRead[V]
```

The design must not return a heterogeneous `PayloadBatch` requiring casts or
string lookups. `V` may be a checked double matrix, integer block, mask,
sparse-event value, or another representation-owned carrier. The response
kernel does not mandate a universal `Tensor[A]`; archive and Zarr layers may
continue to use their own primitive shape-and-slice carriers below this typed
logical boundary.

### 14.6 Decode plan

`DecodePlan[A]` is:

- pure;
- typed;
- inspectable;
- compositional;
- applicative-first;
- executable by an interpreter from logical reads to an effect.

Phase 3 selected a project-owned typed applicative AST after proving Scala 3
typing, JVM/Scala.js behavior, request collection, and the explicit
data-dependent barrier. Its semantic nodes are:

```text
Pure[A]
Request[A](LogicalPayloadRead[A])
Map[A, B](DecodePlan[A], A => B)
Zip[A, B](DecodePlan[A], DecodePlan[B])
Dependent[A, B](DecodePlan[A], A => DecodePlan[B])
```

The public construction and execution surface is:

```scala
DecodePlan.read(request)
DecodePlan.pure(value)
DecodePlan.map2(...)
DecodePlan.map3(...)
DecodePlan.inspect(plan)
DecodePlan.runApplicative(plan, interpreter)
```

`Dependent` is permitted only when later logical reads truly depend on earlier
payload contents. It is counted during inspection, rejected by applicative
execution, and accepted only by the separately named sequential interpreter.
Typed request/result association never uses casts, `Any`, or string-result
lookups.

### 14.7 Dense identity representation

Dense BOLD is a valid identity representation:

```text
selected output slice
→ selected dense logical payload slice
→ identity reconstruction
```

This lets archived dense responses participate in representation registries
and laws.

Direct dense sources such as in-memory matrices or NIfTI readers may implement
`ResponseSource` directly when representation machinery adds no value.

### 14.8 Encoders and codecs

Use:

- `RepresentationEncoder`;
- `RepresentationDecoder`.

Use `RepresentationCodec` only where the two form one meaningful lawful pair.
Learned shared dictionaries may require fitted external state for encoding
while decoding requires only persisted descriptor and basis.

Encoder policy is explicit:

```scala
encoder.encode(
  response,
  DctEncodingPolicy(
    rank = rank,
    quantization = quantization,
    residuals = residualPolicy
  )
)
```

Rank, quantization, residual, and fitting policy are not givens.

## 15. Archive Semantics

### 15.1 NeuroArchive is a stable format family

NeuroArchive is:

- a logical object model;
- a family of physical container bindings;
- a validation, integrity, and publication contract.

It is not a runtime that discovers and executes arbitrary scientific code.

A file declares what it contains. A trusted application runtime supplies how
installed representation families are interpreted.

### 15.2 Logical manifest

The logical object model must describe:

- archive and object schema versions;
- object type;
- optional representation key;
- typed payload roles;
- payload scalar types and shapes;
- content-addressed external dependencies;
- provenance;
- integrity;
- physical layout references;
- publication state.

Illustrative shape:

```scala
final case class ObjectKey(
    objectType: ObjectTypeId,
    schemaMajor: Int,
    representation: Option[RepresentationKey]
)

final case class ArchiveManifest(
    format: ArchiveFormatKey,
    key: ObjectKey,
    representation: Option[PersistedRepresentation],
    attributes: CanonicalValue,
    payloads: Vector[PayloadDescriptor],
    integrity: IntegrityManifest
)

final case class PersistedRepresentation(
    key: RepresentationKey,
    descriptor: CanonicalValue,
    outputSchema: CanonicalValue
)
```

The archive may record a representation key without understanding its
mathematics.

Illustrative serialized meaning:

```text
container:
  lna-hdf5@1
object:
  org.scalafim/fmri-response@2
representation:
  org.scalafim/temporal-dct@1
payloads:
  coefficients -> payload-01
  basis        -> payload-02
  offsets      -> payload-03
physical encoding:
  little-endian -> gzip -> crc32c
```

The container and physical encoding describe recovery of logical payloads.
The representation identifier describes how a trusted installed decoder may
interpret those payloads as a response.

### 15.3 Revision versus opened resource

An archive revision is pure:

```scala
final case class ArchiveRevision(
    id: ArchiveRevisionId,
    manifest: ArchiveManifest,
    publication: PublicationStatus,
    provenance: ArchiveProvenance
)
```

An open archive is resource-backed:

```scala
trait OpenArchive[F[_]]:
  def revision: ArchiveRevision
  def payloads: PayloadExecutor[F]
  def validateContents: EitherT[F, ArchiveError, ArchiveValidation]
```

Drivers open resources safely:

```scala
trait ArchiveDriver[F[_]]:
  def open(
      location: ArchiveLocation
  ): Resource[F, OpenArchive[F]]
```

Immutable revision values never secretly own HDF5 handles, HTTP sessions,
memory mappings, or caches.

### 15.4 Structural inspection of unknown representations

Opening proceeds in layers:

```text
open physical container
→ parse manifest
→ validate archive structure and publication
→ expose revision and payload inventory
→ optionally resolve a representation family
```

If no representation family is installed:

- archive opening succeeds when archive structure is valid;
- manifest and payload inventory remain inspectable;
- integrity validation remains available;
- opening as a response fails with `UnsupportedRepresentation`;
- the error reports the found key and installed supported keys.

Unknown representation and corrupt archive are different outcomes.

### 15.5 Container algebra

The archive API deals in manifests, payload plans, validation, and resources:

```scala
trait PayloadExecutor[F[_]]:
  def execute[A](
      plan: PayloadPlan[A]
  ): EitherT[F, ArchiveError, Observed[A]]
```

```scala
final case class Observed[A](
    value: A,
    receipt: ReadReceipt
)
```

Physical drivers include:

- LNA/HDF5;
- NeuroArchive Zarr;
- directory/object layout;
- in-memory fixtures.

Drivers return `OpenArchive`, not `LatentResponse` or `FmriDataset`.

### 15.6 Two independent dispatches

Physical and logical dispatch remain separate:

```text
URI, signature, or explicit format
→ ArchiveDriver
→ OpenArchive

manifest ObjectKey
→ installed archived-response family
→ ResponseSource
```

An `.lna` suffix does not determine DCT semantics. A Zarr root does not imply
dense BOLD semantics without manifest refinement.

### 15.7 Publication

Publication states are explicit:

```scala
enum PublicationStatus:
  case Staging
  case Published(rootDigest: ContentDigest)
  case Incomplete(reason: IncompleteReason)
```

Ordinary verified response opening accepts only a complete published revision.
Inspection and recovery APIs may open staging or incomplete revisions while
preserving that status.

### 15.8 Physical layers

Vocabulary is normative:

| Layer | Examples | Contract | Owner |
| --- | --- | --- | --- |
| Representation transform | DCT rank reduction, Haar, HRBF, transport, BOLDZip | Exact or declared approximation | `latent` |
| Numerical transform | Quantization, coefficient scaling, sparse residual encoding | Exact or declared numerical error | normally `latent` |
| Scalar serialization | Float32 little-endian, Int16 encoding | Exact scalar round trip | `archive` |
| Byte compression | gzip, Zstandard, Blosc | byte-exact decode | `archive` |
| Integrity | CRC32C, SHA-256 | corruption detection or identity | `archive` |
| Layout | chunks, shards, object partitioning | coverage and reconstruction laws | `archive` |
| Store | HDF5, filesystem, HTTP, object store | resource and consistency semantics | adapter |

CRC, chunking, and sharding are not codecs. Lossy quantization is not a byte
codec.

### 15.9 Format-family profiles

Normalize terminology:

```text
NeuroArchive
  logical object model and format family

LNA-HDF5
  one physical container and profile binding

NeuroArchive-Zarr
  another physical container and profile binding
```

The same logical DCT representation may be persisted through either binding
without changing DCT reconstruction semantics.

The generic Zarr kernel and a NeuroArchive profile remain distinct:

- the generic kernel may support an extensible physical codec-provider
  architecture;
- a canonical dense-BOLD profile may require one constrained codec and layout
  pipeline;
- profile validation rejects unsupported pipelines even when the kernel could
  execute them;
- adding Zstandard later requires a new admitted profile or profile version,
  not silent relaxation of an existing canonical profile.

## 16. Archive/Representation Interop

### 16.1 Responsibility

Interop owns:

- persisted representation header schema;
- mapping logical slots to archive payload roles;
- descriptor parsing and migration;
- binding logical slices to archive payload slices;
- representation-specific archive write layout choices;
- creation of an archive-backed response source.

It does not own:

- reconstruction mathematics;
- generic archive verification;
- dataset subject/session/task resolution.

### 16.2 Typed lowering

```scala
trait LogicalPayloadBinding:
  def lower[V](
      request: LogicalPayloadRead[V]
  ): Either[BindingError, PayloadPlan[V]]
```

The archived logical-read interpreter composes:

```text
LogicalPayloadRead[V]
→ LogicalPayloadBinding.lower
→ PayloadPlan[V]
→ PayloadExecutor.execute
→ Observed[V]
```

Receipts from independent logical reads accumulate without changing
representation code.

### 16.3 Representation-specific binding

Illustrative API:

```scala
trait ArchivedRepresentationBinding[R]:
  def key: ObjectKey

  def encoder:
    ArchiveEncoder[MaterializedRepresentation[R]]

  def opener[F[_]: Async]:
    ArchiveOpener[F, OpenedRepresentation[F, R]]
```

Example explicit values:

```text
DctLnaV2
DctZarrV1
HaarLnaV2
BoldZipLnaV1
DenseBoldZarrV1
```

There is no unconstrained globally canonical
`ArchiveEncoder[DctModel]`. The chosen persistence binding is an explicit
value.

### 16.4 Narrow representation envelope

Representation-family parsing receives only an archive-neutral projection:

```scala
final case class RepresentationEnvelope(
    key: RepresentationKey,
    descriptor: CanonicalValue,
    payloads: PayloadCatalog,
    outputSchema: CanonicalValue
)
```

It must not receive the entire archive manifest by default. Representation
code should not gain accidental access to publication policy, unrelated
objects, container metadata, or physical layouts.

After descriptor parsing and installed-family lookup, binding receives a
capability-limited `ArchiveResponseAccess[F]` containing only location,
revision identity, the verified published digest, the typed payload executor,
and content validation. It does not expose `ArchiveRevision` or
`ArchiveManifest`.

### 16.5 Explicit immutable registry

Dynamic logical dispatch is assembled explicitly:

```scala
final class ArchivedResponseRegistry[F[_]] private (
    entries: Map[
      RepresentationKey,
      ArchivedResponseFamily[F]
    ]
)
```

Construction:

```scala
ArchivedResponseRegistry.build(
  DenseBoldZarrV1.family,
  DctLnaV2.family,
  HaarLnaV2.family,
  HrbfLnaV1.family,
  BoldZipLnaV1.family
)
```

Requirements:

- duplicate keys fail deterministically at construction;
- resolution is independent of registration order;
- the registry is immutable;
- it is passed as an ordinary dependency;
- tests may construct a one-entry registry;
- a file cannot cause arbitrary code loading;
- optional `ServiceLoader`-style discovery may produce candidate entries only
  in an outer optional module.

The registry is application policy, not typeclass evidence.

## 17. Runtime Composition

Illustrative runtime:

```scala
final case class ScalafimRuntime[F[_]](
    archives: ArchiveDrivers[F],
    responses: ArchivedResponseRegistry[F]
):
  def openResponse(
      location: ArchiveLocation
  ): ArchiveResource[F, ResponseSource[F]]

  def openDataset(
      location: ArchiveLocation,
      dataset: FmriDataset,
      acquisition: AcquisitionContext
  ): RuntimeResource[F, OpenedDataset[F]]
```

`RuntimeResource` is a `Resource` over an `EitherT` error channel that
distinguishes archive opening from a non-empty chain of attachment issues. The
dataset description and acquisition claim are separate arguments so identity,
run, and schema drift can be reported rather than made unrepresentable by
construction.

`openResponse` performs steps 1 through 9 below. `openDataset` reuses that
path and adds checked dataset attachment as step 10:

1. physical driver selection;
2. resource-safe archive opening;
3. archive structural validation;
4. publication validation;
5. representation-envelope decoding;
6. installed-family lookup;
7. representation validation;
8. archive binding;
9. construction of a response source;
10. checked dataset attachment.

A format-specific `DatasetArchiveOpener` may exist as ergonomic sugar only
when the archive contains complete acquisition context. It must delegate to
the same response opening and attachment path.

## 18. Planning Pipeline

### 18.1 Dataset plan

Input:

```text
DatasetRunQuery + DataSelection
```

Output:

```text
ResolvedDatasetRead
```

Responsibilities:

- identity filtering;
- coordinate resolution;
- run-local segmentation;
- event and time validation;
- assembly policy.

### 18.2 Decode plan

Input:

```text
typed representation model + ResolvedResponseSelection
```

Output:

```text
DecodePlan[ResponseBlock]
```

Responsibilities:

- validate selection against output schema;
- lower output samples to representation-internal coordinates;
- request typed logical slots;
- reconstruct exactly the requested block;
- carry the reconstruction contract and decode-path consistency.

### 18.3 Payload plan

Input:

```text
LogicalPayloadRead[V] + archive binding
```

Output:

```text
PayloadPlan[V]
```

Responsibilities:

- resolve payload identities;
- validate logical value and shape expectations;
- map logical payload slices to archive payload slices;
- remain independent of object-store execution.

### 18.4 Physical read plan

Input:

```text
PayloadPlan[V] + physical layout metadata
```

Output:

```text
PhysicalReadPlan
```

Responsibilities:

- chunks, shards, datasets, object keys, and byte ranges;
- physical codec programs;
- resource-limit checks;
- deterministic coalescing;
- planned read statistics.

### 18.5 Execution

Execution:

- acquires resources;
- performs bounded requests;
- reverses physical codecs;
- verifies integrity;
- reconstructs exact logical payload values;
- emits actual read evidence;
- evaluates the pure reconstruction program.

Each lowering stage must be independently inspectable and testable.

### 18.6 Write path

The write path mirrors the separation of the read path:

```text
Response field
→ explicit RepresentationEncoder
→ representation descriptor + typed logical payloads
→ explicit archive/representation binding
→ ArchiveWritePlan
→ physical layout planner
→ scalar serialization
→ byte compression
→ integrity tagging
→ create-only object writes
→ publication validation
→ ArchiveWriteReceipt
```

Requirements:

- representation policy is explicit;
- archive format and binding are explicit;
- logical payloads are validated before physical planning;
- physical serialization and compression are lossless relative to logical
  payloads;
- publication occurs only after all required payloads and integrity evidence
  are complete;
- write receipts distinguish logical representation identity from physical
  revision identity;
- interrupted writes do not appear as published archives;
- the same materialized representation can be offered to multiple archive
  bindings.

Illustrative use:

```scala
val represented =
  DctEncoder(policy).encode(response)

val receipt =
  runtime.writeArchive(
    location,
    represented,
    binding = DctLnaV2
  )
```

No archive writer chooses DCT rank, quantization, or residual policy.

## 19. Partial Access and Read Honesty

### 19.1 Logical selection is not physical locality

These claims are separate:

1. The source can return only selected output values.
2. The representation plan requests selected logical payload regions.
3. The archive binding maps them to bounded payload slices.
4. The physical interpreter avoids unrelated whole-object reads.

A source must not infer claim 4 from claim 1.

### 19.2 Capabilities

```scala
enum PhysicalLocality:
  case Resident
  case WholeObject
  case WholePayload
  case ChunkBounded
  case ByteRangeBounded

final case class AxisLocality(
    axes: SelectionAxes,
    guarantee: PhysicalLocality
)

final case class ReadCapabilities(
    localityByAxes: NonEmptyVector[AxisLocality]
)
```

Capabilities describe guaranteed behavior of the assembled source for a
specific selected axis or combination of axes. A source may be chunk-bounded
for sample selection, whole-payload for time selection, and resident for
metadata.

`PhysicalLocality` is not a total strength ordering. `WholeObject` and
`WholePayload`, for example, are incomparable without layout evidence. The
capability model therefore defines an explicit conformance relation:

```text
receipt conforms to declared axis-locality under the opened layout
```

It does not compare enum ordinals.

### 19.3 Receipts

```scala
final case class ReadReceipt(
    logical: LogicalReadSummary,
    physical: PhysicalReadSummary,
    integrity: IntegrityEvidence,
    fallbacks: Chain[ReadFallback]
)
```

Receipts describe what actually occurred:

- logical slots and slices requested;
- payloads touched;
- objects, chunks, shards, and byte ranges touched;
- logical and physical bytes;
- cache hits;
- checksums verified;
- whole-payload fallbacks;
- decoder and physical codec versions when relevant.

Existing Zarr `ExecutionReceipt` remains authoritative for Zarr mechanics and
is adapted into the common receipt rather than duplicated or discarded.

### 19.4 Capability truth

A source advertising `ChunkBounded` for an axis set must not issue reads
outside the chunks covering that selection unless the touched chunk itself
spans the payload. A `ByteRangeBounded` claim is checked against the covering
ranges. Whole-object and whole-payload claims are checked against manifest and
layout identities rather than a synthetic ordering.

Any fallback must be visible in the receipt. It either satisfies a declared
coarser capability for that axis set or fails an explicit strict-read policy;
it does not silently preserve the original claim.

## 20. Provenance and Identity

### 20.1 Separate identities

The following identities are not substitutes:

- dataset/acquisition identity;
- response schema identity;
- representation identity;
- archive revision identity;
- logical payload identity;
- encoder implementation and configuration;
- decoder implementation.

### 20.2 Reconstruction contract

```scala
enum ReconstructionContract:
  case Exact
  case DeterministicBounded(bounds: ErrorBounds)
  case ValidatedScientific(
    profile: ScientificProfileId,
    report: ValidationReportRef
  )
```

Structured residual policy, omitted residuals, calibration, and other
scientifically meaningful conditions must be visible in the descriptor or
contract.

### 20.3 Decode-path consistency

The reconstruction contract compares a decoded representation with the
scientific response from which it was encoded. It does not compare two
execution paths for the same represented value.

Each representation therefore declares a separate path-consistency policy:

```scala
enum DecodeConsistency:
  case ExactBits
  case UlpBounded(maxUlps: Int)
  case AbsoluteRelative(
    absolute: Double,
    relative: Double
  )
```

Selection, partition, ordering, memory-versus-archive, and interop commutation
laws use this policy. Exact representations may still use a bounded policy
when two lawful kernels accumulate floating-point terms in a different order.
The policy must be tighter than the representation's scientific reconstruction
error and justified by executable fixtures.

### 20.4 Derivation provenance

Provenance is a DAG:

```scala
final case class ProvenanceNode(
    id: ProvenanceId,
    operation: ProvenanceOperation,
    parents: Vector[ProvenanceId],
    evidence: ProvenanceEvidence
)
```

New composition creates a node referencing all parent roots. It does not
flatten or overwrite history into a string.

### 20.5 Read result

```scala
final case class ReadResult(
    block: ResponseBlock,
    provenance: Provenance,
    receipt: ReadReceipt
)
```

The same response read twice may have identical provenance and different
receipts because one read was cached or used another physical layout.

## 21. Validation and Error Model

### 21.1 Layered validation

Expected metadata faults accumulate within their domain:

```text
ValidatedNec[ArchiveIssue, ValidArchiveManifest]
ValidatedNec[RepresentationIssue, ValidRepresentation]
ValidatedNec[AttachmentIssue, ValidDatasetAttachment]
```

Operational failures remain in the effect:

- unreadable object;
- permission failure;
- failed HTTP request;
- checksum mismatch discovered during reading;
- missing payload;
- failed external-basis resolution;
- resource-limit violation.

### 21.2 Required ordering

Canonical opening order:

```text
open container resource
→ parse archive syntax
→ validate structure and publication
→ migrate archive manifest
→ extract narrow representation envelope
→ resolve installed representation family
→ parse and migrate representation descriptor
→ validate representation
→ bind logical payloads
→ validate dataset attachment
→ construct `OpenedDataset[F]`
```

### 21.3 Error taxonomy

The public model must distinguish:

- invalid dataset query or selection;
- schema/domain mismatch;
- attachment incompatibility;
- unsupported archive format;
- corrupt archive structure;
- incomplete publication;
- unsupported representation;
- malformed representation descriptor;
- invalid representation model;
- missing or incompatible logical payload;
- missing external dependency;
- physical IO failure;
- integrity failure;
- decode/reconstruction failure;
- resource-limit refusal.

These errors may be folded into a high-level runtime error for convenience,
but the original typed category and cause remain inspectable.

## 22. Functional Requirements

### RSP-001: Coherent response scope

One `ResponseSource` represents one coherent acquisition response field.

### RSP-002: Domain identity

Every resolved response selection names the schema and axis domains against
which it was resolved.

### RSP-003: Shape and order

Every response block proves its row/column shape and preserves the requested
time and sample order. Decoded values are row-major `Double` values over
time-by-sample.

### RSP-004: Source planning

Every response source exposes pure planning separately from effectful
execution, and multi-source composition retains each source with its
path-dependent plan in a typed `PlannedRead`.

### RSP-005: Minimal owned block

`ResponseBlock` owns its primitive buffer directly. The response kernel does
not introduce a generic tensor structure, expose mutable storage, or claim
zero-copy when ownership remains shared.

### DATA-001: Output-domain resolution

Dataset resolution stops at output response indices and never emits
representation-internal coordinates.

### DATA-002: Checked attachment

Dataset attachment validates timing, sample domain, geometry, mask/order,
identity, and calibration assumptions.

### DATA-003: Segmentation

Cross-acquisition reads remain segmented unless explicit assembly is requested.

### DATA-004: Public API continuity

Typed dataset queries and `DataSelection` remain supported directly through
the neutral dataset adapters.

### DATA-005: Pure description and effectful opening

`FmriDataset`, model descriptions, and fit plans remain pure.
`OpenedDataset[F]` owns effectful reads. Asynchronous and browser sources never
receive a blocking synchronous facade.

### REP-001: Open representation families

New representation families use namespaced versioned keys and do not require a
central enum edit.

### REP-002: Typed decode planning

Representation decoding compiles to a typed plan over typed logical payload
requests.

### REP-003: Selection-aware reconstruction

Each migrated representation either implements selected reconstruction or
explicitly advertises and receipts its coarser fallback.

### REP-004: Reconstruction contract

Each representation declares exact, bounded, or scientifically validated
reconstruction semantics.

### REP-005: Explicit encoding policy

Representation encoders receive rank, quantization, residual, fitting, and
other numerical policy as explicit values.

### REP-006: Decode-path consistency

Each representation separately declares how selected, partitioned,
in-memory, and archive-backed executions of the same represented value must
agree.

### ARC-001: Unknown representation inspection

A valid archive with an unknown representation can be opened for structural
inspection and integrity verification.

### ARC-002: Revision/resource split

Pure archive revision metadata is distinct from resource-backed open handles.

### ARC-003: Exact logical payload recovery

Archive execution returns logical typed payloads exactly as declared. Numeric
payload equality uses raw scalar bits unless a format profile explicitly
declares and tests canonical normalization.

### ARC-004: Independent physical dispatch

Physical container selection is independent of logical representation
selection.

### ARC-005: Publication safety

Ordinary response opening rejects incomplete or staging revisions.

### ARC-006: Transactional write planning

Archive writing lowers validated logical payloads through deterministic
physical plans and publishes only after required objects and integrity evidence
are complete.

### INT-001: Explicit bindings

Each representation/archive pairing is an explicit binding value.

### INT-002: Deterministic registry

Registry construction rejects duplicate representation keys and lookup is
deterministic.

### INT-003: Narrow envelope

Representation families receive only the manifest projection required for
descriptor parsing and payload binding.

### INT-004: Multiple persistence bindings

One materialized representation may be persisted through multiple explicit
archive bindings without changing its mathematical model or logical payload
values.

### OBS-001: Capability declaration

Every response source declares physical locality by selected axis or axis
combination.

### OBS-002: Execution receipt

Every archive-backed read can return logical, physical, integrity, and fallback
evidence.

### OBS-003: Provenance preservation

Dataset results preserve reconstruction contract and derivation provenance.

### RUN-001: Explicit assembly

Applications explicitly construct driver and representation registries.

### RUN-002: Resource safety

Archive and response lifetimes are represented by a resource-safe abstraction.

### RUN-003: No file-directed code loading

Manifest contents select only among already installed trusted entries.

### RUN-004: Concrete runtime opening

The migration assigns `ScalafimRuntime.openResponse` and
`ScalafimRuntime.openDataset` to named delivery phases and tests their full
assembly order.

## 23. Non-Functional Requirements

### NFR-001: JVM and Scala.js

All portable schemas, plans, bindings, laws, and representation kernels compile
and test on both platforms.

### NFR-002: Warning cleanliness

New and migrated modules compile warning-clean under repository scalac options.

### NFR-003: Allocation discipline

Hot response reconstruction and result assembly use owned primitive buffers,
row-major indexing, primitive index storage, and allocation-conscious loops.
Opaque indices do not force boxed arrays outside their companion factories.

### NFR-004: Determinism

Given identical metadata, selection, registry, and policies:

- pure plans are equal;
- registry resolution is equal;
- output ordering is equal;
- logical payload identities are equal.

Physical compression bytes need not be cross-implementation identical unless a
format profile explicitly promises it.

### NFR-005: Bounded resources

Open, planning, decoding, and execution paths accept explicit limits for
metadata, rank, shapes, payload bytes, objects, requests, concurrency, and
decoded buffers.

### NFR-006: No hidden blocking

Scala.js and remote IO do not expose blocking synchronous facades.

### NFR-007: No hidden global state

Registries, caches, stores, credentials, and retry policy are explicit
dependencies.

### NFR-008: No pre-release shims

This unreleased system does not retain deprecated aliases or historical API
names. The current LNA 2 pipeline, normalized manifest adapter, and dataset
bridges use factual names and explicit dependencies.

### NFR-009: Independent verification

Format conformance and scientific parity rely on independent fixtures where
available, not only Scala round trips.

### NFR-010: Frozen migration baseline

Every phase runs a numerical regression corpus frozen from the exact committed
Phase-0 baseline. The corpus covers all live reconstruction and archive
transform routes, including valid compositions.

## 24. Architectural Laws

### 24.1 Selection law

For a valid selection `s`:

```text
decode(representation, s)
≈
decode(representation, All).select(s)
```

Agreement is governed by the representation's `DecodeConsistency`, not its
scientific reconstruction contract.

### 24.2 Partition law

For ordered disjoint selections `a` and `b`:

```text
decode(representation, a ++ b)
≈
assemble(
  decode(representation, a),
  decode(representation, b)
)
```

Agreement is governed by `DecodeConsistency`.

### 24.3 Ordering law

```text
result.selection.timepoints == request.timepoints
result.selection.samples    == request.samples
```

Internal sorting, coalescing, or deduplication must be reversed during result
assembly. Values at each returned position agree with a direct read of the
corresponding requested index under `DecodeConsistency`.

### 24.4 Shape law

```text
result.rows    == requested timepoint count
result.columns == requested sample count
```

### 24.5 Domain law

A selection resolved for schema A cannot be read against schema B even when
dimensions match.

### 24.6 Representation law

For an encoder/decoder pair:

```text
decode(encode(Y)) ≈ Y
```

The approximation is governed by the declared reconstruction contract.

### 24.7 Persistence law

Archive round trip preserves the representation artifact exactly:

```text
open(write(R)).model == R.model
open(write(R)).logicalPayloads == R.logicalPayloads
```

For floating payloads, exactness compares raw IEEE scalar bits, including
signed zero and NaN payloads, unless the admitted format profile explicitly
declares canonical normalization. Scala collection or matrix `==` is not
sufficient evidence. This remains exact when `R` is a lossy representation of
`Y`.

### 24.8 Binding law

For every valid logical payload request `q`:

```text
execute(lower(q)) == logicalPayload(q)
```

### 24.9 Interop commutation law

For every selection `s`:

```text
openArchive(writeArchive(R)).reconstruct(s)
≈
R.reconstruct(s)
```

Agreement is governed by `DecodeConsistency`.

### 24.10 Dataset bridge law

For a single-acquisition dataset query `q`:

```text
datasetFromResponse(source).read(q)
≈
source.read(resolve(q))
```

with attachment provenance added and values compared under
`DecodeConsistency`.

For a multi-acquisition query:

```text
openedDataset.read(q)
≈
assemble(
  traverse(resolve(q).reads)(read corresponding source),
  resolve(q).assembly
)
```

The default assembly preserves acquisition segments.

### 24.11 Dense backend law

For exact dense storage:

```text
denseBackend.read(s) == denseValue.select(s)
```

subject only to declared scalar calibration at the response boundary.

### 24.12 Locality law

Every instrumented receipt conforms to the capability declared for the
selected axis set under the opened physical layout. `ChunkBounded` and
`ByteRangeBounded` sources obey their declared covering units for strict subset
selections. No law relies on a total ordering of locality enum cases.

### 24.13 Hierarchy law

Reads spanning acquisition boundaries remain segmented unless an explicit
assembly policy requests another layout.

### 24.14 Provenance law

Every derived value creates a provenance node referencing all parent roots.
No operation removes parent evidence.

### 24.15 Publication law

Only complete published revisions open through the ordinary verified response
API. If execution fails after any proper prefix of a write plan, the target is
absent, `Staging`, or `Incomplete`; it is never observable as `Published`.

### 24.16 Registry law

Every `(namespace, name, majorVersion)` has at most one installed family in one
registry. Resolution is deterministic and registration-order independent.

### 24.17 Migration-baseline law

Every completed phase reproduces the frozen Phase-0 numerical regression corpus under
each fixture's declared raw-bit, ULP, or absolute/relative comparator.

## 25. Versioning and Evolution Policy

### 25.1 Read admitted formats; write the canonical model

When a normalized manifest is introduced:

- the current LNA 2 manifest is adapted by the pure
  `LnaArchiveManifestAdapter`;
- each admitted container format remains readable through its versioned
  driver;
- canonical writers emit the normalized model only after conformance gates
  pass;
- container-specific writers keep factual profile names.

### 25.2 Factual bindings and registries

Cross-domain code uses explicit names such as:

```text
LnaPipelineRepresentationFamily
LnaArchiveManifestAdapter
DatasetResponseSource
LatentArchiveDatasetBackend
```

LNA construction uses representation-specific codecs. LNA recognition uses an
explicitly supplied immutable `LatentArchiveRegistry` assembled from
independent binding values. It rejects invalid or duplicate ownership and
ambiguous matches. There is no central codec facade or deprecated forwarding
alias.

### 25.3 No silent fallback

Fallback from selected to whole-payload or whole-run reconstruction:

- is declared in capabilities;
- appears in receipts;
- preserves result semantics and order;
- never claims bounded IO;
- can be forbidden by an explicit strict-read policy.

### 25.4 Pre-release API replacement

ScalaFIM has not published this API surface. When a pre-release API is
superseded, update repository callers, examples, documentation, and tests, then
remove the old symbol in the same change. Do not add forwarding aliases,
deprecation windows, or historical naming layers.

Compatibility policy for a future published API is outside this program and
must be decided from the actual release contract rather than assumed during
development.

## 26. Delivery Plan

After Phase 0, Phases 1 and 2 may proceed concurrently. Their common
capability, receipt, and resource-lifetime contracts are frozen by the Phase-0
decisions; Phase 1 owns the response-facing vocabulary while Phase 2 owns
archive-native revision and resource separation. The Track-A completion gate
waits for both phases and verifies their integration before Phase 3 begins.

### Phase 0 — Freeze the contract and inventory the live seams

Deliver:

- this PRD accepted as the north star;
- the normative
  [Phase 0 baseline and decision record](response-representation-archive-phase-0.md);
- exact committed baseline: Git commit, Scala version, module graph, bridge
  commit, and prerequisite branch commits;
- inventory of every production file importing two principal domains;
- inventory of every live numerical reconstruction route, LNA transform and
  valid transform composition, representation family, and manifest variant;
- frozen numerical regression fixtures for current quant, delta, explicit, temporal
  DCT, temporal Haar, shared-basis, transport, BOLDZip, and dense paths;
- provisional artifact mapping and normative dependency edges;
- decision records D1 through D4 and D6 through D10;
- explicit record that `ResponseBlock` owns primitive row-major `Double`
  storage directly and no response-level tensor framework will be introduced.

Acceptance:

- current bridge tests remain green;
- frozen fixtures have recorded provenance, stable fixture digests, and
  declared raw-bit, ULP, or absolute/relative comparators;
- no source is moved;
- every existing cross-domain responsibility has a named target owner;
- no unresolved decision is hidden inside a migration ticket;
- the only decision intentionally left for a later phase is D5 artifact naming.

### Phase 1 — Establish the response kernel

Deliver:

- the normative
  [Phase 1 implementation record](response-representation-archive-phase-1.md);
- axis-safe indices and domain identities;
- primitive-backed ordered indices built only through companion factories;
- response schema, neutral sample-domain references, and response-local
  selection;
- owned non-generic `ResponseBlock`;
- `ReadResult`, initial provenance reference, and read receipt;
- `ResponseSource` planning/execution contract and typed `PlannedRead`;
- axis-keyed locality capabilities and their receipt-conformance relation;
- adapters for current `ResponseBlockSource` and `DatasetBackend`.

Acceptance:

- current in-memory and NIfTI selected reads are unchanged;
- requested order, shape, duplicate, and domain laws pass;
- equal-shaped wrong-domain selections fail;
- index arrays remain primitive on JVM and Scala.js;
- no public mutable response buffer or ownership-preserving `copy` exists;
- matrix adapters copy unless ownership is transferred and the source no
  longer retains the buffer;
- no archive or latent logic moves yet;
- JVM and Scala.js tests pass.

Stop gate:

If the response kernel must import image geometry, surface, graph, archive,
dataset hierarchy, or concrete representation types, redesign before
proceeding.

### Phase 2 — Split archive revision from opened resources

Deliver:

- the normative
  [Phase 2 implementation record](response-representation-archive-phase-2.md);
- pure `ArchiveRevision`;
- resource-backed `OpenArchive[F]`;
- `PayloadExecutor[F]`;
- structural versus content validation;
- adapters over current eager LNA/HDF5 and Zarr open paths;
- initial common receipt adaptation.

Phase 2 depends on the Phase-0 contract, not on completion of Phase 1. It may
retain archive-native receipt evidence during parallel implementation; the
Track-A gate performs final adaptation to the response-facing capability and
receipt types.

Acceptance:

- valid unknown-representation archives remain inspectable;
- incomplete publication is distinct from unsupported representation;
- eager HDF5 reads report `WholePayload`;
- Zarr receipts preserve existing object/range accounting;
- mixed-axis capability claims conform to instrumented receipts;
- resource closure is tested on success, failure, and cancellation.

### Track A completion gate — response/archive integration

Implemented by the
[Track A integration record](response-representation-archive-track-a.md).
The adapter lives in `dataset`, the existing module that can see both
principal vocabularies. It preserves the ordered archive-native receipt while
deriving a response-facing logical receipt, capability set, and opened layout.
One combined-axis archive receipt or separate time and sample receipts must
cover the resolved response selection exactly.

The gate mechanically verifies that `response` imports no image, surface,
graph, archive, latent, or dataset type; no response tensor or public mutable
buffer exists; archive unknown-representation and resource laws still hold;
and all JVM/Scala.js tests plus the frozen numerical regression corpus remain
green.

### Phase 3 — Prove one typed representation plan in memory

Start with temporal DCT or Haar.

Deliver:

- the normative
  [Phase 3 implementation record](response-representation-archive-phase-3.md);
- typed logical payload slots;
- typed `DecodePlan[ResponseBlock]`;
- in-memory logical payload interpreter;
- selected reconstruction from output-domain selections;
- reconstruction contract;
- representation-specific decode-path consistency.

Acceptance:

- selection, partition, ordering, shape, and domain laws pass;
- those laws use decode-path consistency rather than reconstruction error;
- the plan can be inspected without executing;
- coefficient and basis results cannot be accidentally exchanged;
- independent reads can be collected and evaluated applicatively;
- the frozen numerical regression corpus remains green;
- JVM and Scala.js behavior agrees.

Stop gate:

If the chosen plan encoding requires casts, `Any`, string-result association,
or pervasive representation-specific branching, redesign it.

### Phase 4 — Bind the representation to LNA

Deliver:

- the normative
  [Phase 4 implementation record](response-representation-archive-phase-4.md);
- one explicit DCT/Haar LNA binding;
- logical-slot to LNA-payload lowering;
- typed payload plans over the current store;
- archived response source;
- archive write plan and write receipt;
- archive binding round-trip and interrupted-publication fixtures.

Acceptance:

- in-memory and LNA interpreters produce equivalent selected blocks;
- persistence preserves representation model and logical payload scalar bits
  exactly unless an admitted profile declares canonical normalization;
- failure after any proper prefix of the write plan never exposes a published
  archive;
- whole-payload physical fallback is receipted honestly;
- mathematical code imports no LNA/HDF5 types;
- the frozen numerical regression corpus remains green.

### Phase 5 — Introduce explicit representation registry

Implementation record:
[response-representation-archive-phase-5.md](response-representation-archive-phase-5.md).

Deliver:

- namespaced representation keys;
- immutable duplicate-rejecting registry;
- narrow representation envelope;
- `LnaPipelineRepresentationFamily` for the current generic LNA pipeline;
- unsupported-representation error and inspection path;
- `ScalafimRuntime.openResponse`.

Acceptance:

- registry law passes;
- resolution does not depend on insertion order;
- test runtimes can install one family;
- file contents cannot load code;
- runtime assembly closes archive and response resources on success, failure,
  and cancellation;
- generic LNA pipeline fixtures still open.

### Phase 6 — Attach response sources to datasets

Deliver:

- pure `FmriDataset` scientific description and query compiler;
- checked `OpenedDataset[F]` attachment;
- acquisition context and attachment validation;
- adapters from current `SamplingFrame`, `VoxelDomain`, and dataset metadata;
- dataset read lowering to `ResolvedResponseSelection`;
- provenance and receipt propagation into results;
- `ScalafimRuntime.openDataset`;
- explicit synchronous execution policy and effectful model, fit, and MVPA
  executor boundary;
- factual `DatasetError.AdapterFailure` reporting for archive and latent
  adapters.

Acceptance:

- time, geometry, mask, ordering, and schema mismatches are typed failures;
- dataset does not map requests to latent coefficient coordinates;
- segmentation and current indexed-query behavior are preserved;
- `FmriModel`, `FitPlan`, and numerical kernels do not acquire an `F[_]`
  parameter;
- asynchronous and Scala.js sources have no blocking synchronous facade;
- downstream model, fit, and MVPA compilation and behavior gates pass;
- `LatentResponseDatasetBackend` becomes a thin adapter or disappears.

Stop gate:

If effect types spread through pure scientific descriptions or numerical
kernels, if browser reads require blocking, or if attachment cannot preserve
current segmentation and exact-grid semantics, redesign before proceeding.

### Phase 7 — Prove format independence with Zarr

Deliver:

- binding of the same migrated representation to NeuroArchive Zarr or a
  typed in-memory Zarr fixture;
- dense identity family for canonical dense BOLD;
- receipt adaptation from the existing Zarr executor;
- shared cross-backend law suite.

Acceptance:

- the same logical decoder runs against memory, LNA, and Zarr;
- only binding and executor code differ;
- dense Zarr obeys exact dense-backend laws;
- browser and JVM pure plans agree;
- axis-keyed bounded-read claims match receipts;
- injected write failure never exposes a published Zarr revision.

Stop gate:

If one logical decoder cannot run unchanged through memory, LNA, and Zarr with
only binding and executor differences, do not begin module extraction or
manifest normalization. Revisit the logical-payload boundary.

### Phase 8 — Extract module boundaries

Deliver:

- archive-specific representation code moved to interop;
- scientific transform execution moved out of archive core;
- dataset archive/representation dispatch moved to interop or runtime;
- build edges updated;
- executable production-source import and build-edge guards;
- module READMEs and module-relations map updated.

Acceptance:

```text
archive core:
  no DCT, Haar, HRBF, transport, or BOLDZip reconstruction branches

latent core:
  no HDF5 paths, Zarr paths, object keys, gzip settings, or checksums

dataset core:
  no imports from scalafim.archive or scalafim.latent
  no LNA, Zarr, DCT, Haar, HRBF, transport, or BOLDZip dispatch
```

`DatasetError` no longer exposes archive- or latent-owned public cases unless a
documented unreleased-API exception applies. All new and affected modules pass
JVM and Scala.js gates.

Stop gate:

If the target edges require a dependency cycle, hidden cross-domain import,
or ownership exception that is not recorded in the PRD, stop before changing
the manifest.

### Phase 9 — Normalize the manifest

Deliver:

- namespaced object and representation identities;
- typed payload roles;
- first-class narrow representation descriptor and output-schema envelope;
- archive/representation version separation;
- pure `LnaArchiveManifestAdapter`;
- admitted deterministic canonical manifest encoding;
- format-neutral transactional canonical writer, with physical sinks remaining
  in their container modules.

Acceptance:

- LNA and canonical fixtures read;
- canonical writer emits only the admitted normalized form;
- unknown representations remain structurally validatable;
- manifest migration is pure and fixture-tested;
- logical payload identity is stable across physical layouts;
- raw numeric bits are preserved unless an admitted profile explicitly
  declares normalization;
- failure after every proper canonical-write prefix never appears published;
- format-specific external conformance and the frozen Phase-0 numerical corpus
  still pass.

### Phase 10 — Lock the architecture and remove pre-release central dispatch

Deliver:

- reusable representation law suites;
- archive binding and persistence law suites;
- dataset attachment and hierarchy law suites;
- access-honesty suites with instrumented interpreters;
- corruption and unknown-representation fixtures;
- removal of superseded central dispatch and forwarding aliases.

Acceptance:

- `sbt compileAll` passes warning-clean;
- `sbt testAll` passes;
- focused JVM and Scala.js gates pass for every affected module;
- downstream model, fit, MVPA, and group scenarios remain green;
- Phase-8 dependency and import guards remain green;
- the frozen Phase-0 numerical regression corpus remains green.

### Program-level stop policy

When a phase stop gate triggers:

- do not begin its dependent phase;
- preserve the last verified implementation path;
- record the failed assumption, evidence, and affected architectural law;
- revise this PRD and its tracker dependencies before resuming;
- do not redefine success by dropping format independence, resource safety,
  numerical parity, or dependency-boundary requirements.

In particular, failure of the Phase-7 memory/LNA/Zarr proof blocks all of
Track D.

## 27. Test Strategy

### 27.1 Response-kernel shared tests

- axis types cannot be interchanged at compile time;
- negative and out-of-bounds indices fail;
- ordered-index factories retain primitive storage on JVM and Scala.js;
- schema identity mismatch fails despite equal shapes;
- block shape is checked;
- order is preserved;
- owned buffers do not expose mutable aliases;
- no response-level tensor abstraction or public mutable storage is introduced;
- adapters preserve current matrix orientation.

### 27.2 Representation shared tests

- validation accumulates independent descriptor faults;
- selection, partition, ordering, shape, and domain laws;
- selection, partition, ordering, and backend agreement use declared
  decode-path consistency;
- encode/decode fidelity alone uses the reconstruction contract;
- plan inspection identifies exact logical slots and slices;
- in-memory interpreters cannot scramble typed results;
- the frozen corpus covers explicit, DCT, Haar, shared-basis, transport,
  BOLDZip, quant, delta, dense, and admitted transform compositions.

### 27.3 Archive shared tests

- structural validation is independent of installed representations;
- publication states are enforced;
- payload scalar and shape expectations are checked;
- malformed references and duplicate IDs fail;
- physical transforms round-trip raw logical payload bits exactly unless a
  profile declares canonical normalization;
- injected failures at every publication boundary never yield `Published`;
- resource limits fail before unsafe allocation or IO.

### 27.4 Interop tests

- representation headers parse and migrate;
- logical slots bind to correct payload roles;
- binding law;
- persistence law;
- interop commutation law;
- external dependencies are content-verified;
- duplicate registry keys fail;
- unknown representation differs from corruption.

### 27.5 Dataset tests

- attachment alignment checks;
- output-domain coordinate resolution;
- current `DataSelection` behavior;
- run-local selection translation;
- segmentation and assembly laws;
- single-acquisition and multi-acquisition bridge laws;
- pure `FmriDataset` and effectful `OpenedDataset[F]` boundaries;
- no synchronous facade over an asynchronous source;
- provenance and receipt propagation;
- no implicit resampling.

### 27.6 Physical interpreter tests

- eager HDF5 reports whole-payload access;
- hyperslab HDF5 reports touched regions when implemented;
- Zarr reports objects, ranges, chunks, shards, bytes, and fallbacks;
- mixed time/sample locality claims are checked independently;
- checksums and corruption errors are precise;
- browser and JVM request traces are equivalent for shared fixtures;
- all resources close on success, failure, and cancellation where supported.

### 27.7 Independent fixtures

Use:

- existing R fmrilatent parity fixtures;
- Phase-0 fixtures captured from every current reconstruction and LNA transform
  route, with fixture digests and declared comparators;
- Python Zarr fixtures;
- nibabel/NIfTI fixtures;
- cross-backend Scala fixtures only as supplementary evidence;
- corrupted-manifest and interrupted-publication fixtures.

## 28. Success Metrics

The architecture is successful when:

1. A DCT or Haar response reconstructs the same selected block through memory,
   LNA/HDF5, and Zarr under its declared decode-path consistency.
2. Dataset core contains no archive-format or representation-family dispatch.
3. Latent core contains no archive-format or byte-codec concepts.
4. Archive core can inspect and verify an unknown representation.
5. Archive persistence preserves representation artifact bits exactly unless
   an admitted profile declares canonical normalization.
6. Interrupted writes never appear published.
7. Read receipts distinguish logical demand from physical bytes and fallbacks.
8. Axis-keyed capability claims are enforced by instrumented law suites.
9. Direct dense sources remain efficient and satisfy the same response laws.
10. `FmriDataset` and model/fit plans remain pure while opened readers remain
    honestly effectful.
11. All portable behavior is verified on JVM and Scala.js.
12. Every phase preserves the frozen numerical regression corpus.
13. Existing model and fit workflows require no source-specific branching.

No compression-ratio or throughput target is defined by this architecture
alone. Format-specific plans and benchmark receipts remain authoritative for
those claims.

## 29. Risks and Mitigations

### Risk: the response kernel becomes a dumping ground

Mitigation:

- admit only vocabulary requiring one meaning across at least two domains;
- reject archive integrity, dataset hierarchy, and representation algorithms;
- require a module-boundary review for every new dependency.

### Risk: typed plans become difficult to use or compile

Mitigation:

- prove one representation vertically;
- hide the program encoding behind constructors;
- measure compilation and allocation;
- prohibit casts and untyped batches;
- retain a narrow data-dependent escape hatch.

### Risk: effect abstraction expands the shared dependency surface

Mitigation:

- keep effects out of values and pure plans;
- confine effectful reads to `ResponseSource[F]`, `OpenedDataset[F]`, and
  runtime interpreters;
- test the existing Cats Effect dependency at the proposed shared
  JVM/Scala.js boundary;
- consider a smaller equivalent only if that spike falsifies the default;
- require real browser and resource-lifetime evidence;
- do not force fitting kernels to adopt an effect type.

### Risk: abstraction harms numeric performance

Mitigation:

- make `ResponseBlock` the specialized primitive owned buffer rather than
  introducing a tensor layer;
- fuse reconstruction and result assembly where lawful;
- inspect plans before execution;
- benchmark the vertical slice against current selected reconstruction;
- keep direct dense fast paths.

### Risk: manifest normalization forks LNA and Zarr semantics

Mitigation:

- define one logical representation envelope;
- keep container details in bindings;
- require the same representation law suite across formats;
- write new manifests only after cross-format proof.

### Risk: pre-release shims become permanent hidden architecture

Mitigation:

- delete deprecated aliases before release;
- name current formats and adapters factually;
- receipt all coarse fallbacks;
- install representations through immutable registries;
- forbid central codec dispatch.

### Risk: provenance volume becomes unmanageable

Mitigation:

- use content-addressed nodes and references;
- separate durable provenance from per-read receipts;
- permit summarized receipts without losing integrity evidence;
- keep logging and telemetry outside pure plans.

### Risk: dataset and response time models duplicate each other

Mitigation:

- settle one authoritative response-time representation in the time-domain
  decision gate;
- adapt or extract current `SamplingFrame`;
- forbid silent conversions with different origins or precision.

### Risk: sample geometry creates a dependency cycle

Mitigation:

- keep only identity, cardinality, ordering, and verifiable geometry/topology
  references in `response`;
- retain concrete volume geometry in `image` and mesh topology in `surface`;
- validate them through dataset-domain adapters;
- stop Phase 1 if `response` must import image, surface, or graph.

### Risk: migration preserves tests but changes validated numerics

Mitigation:

- freeze every live reconstruction and transform route in Phase 0;
- record exact fixture provenance and comparator policy;
- run the corpus after every phase;
- treat unexplained drift as a stop condition rather than updating goldens.

### Risk: JVM and Scala.js implementations diverge

Mitigation:

- share plans, descriptors, and law fixtures;
- compare request traces;
- keep platform differences in interpreters;
- reject synchronous browser facades.

## 30. Decision Gates

Phase 0 resolves D1 through D4 and D6 through D10 with focused records and
required spikes. D5 remains intentionally deferred until module extraction.
No implementation slice may decide one of these questions incidentally.

### D1. Effect and resource surface

Resolved before either parallel Phase 1 or Phase 2 work begins:

- default to Cats Effect `Resource` plus `EitherT`;
- use a smaller project-owned equivalent only if the shared Scala.js spike
  falsifies that default while preserving the same laws.

This is not a green-field library-choice gate. Cats Effect 3.5.4 is already a
cross-platform dependency of `image`, and BIDS JVM code already uses
`Async`, `Resource`, and `EitherT`. The unresolved question is whether admitting
that surface to shared response/archive APIs preserves acceptable Scala.js
linking, cleanup, cancellation, and dependency cost.

The record also fixes the API boundary:

- `FmriDataset`, model descriptions, and fit plans remain pure;
- `ResponseSource[F]`, `OpenedDataset[F]`, and runtime interpreters own
  effects;
- current synchronous facades apply only to synchronous or materialized
  sources.

Evidence required:

- JVM and Scala.js compilation;
- cancellation/resource cleanup behavior;
- impact on current synchronous adapters;
- absence of blocking browser facades.

### D2. Owned response-block storage

Before Phase 1:

- `ResponseBlock` is the only response data structure;
- it stores row-major primitive `Double` values directly;
- the implementation chooses `Array[Double]`, `NArray[Double]`, or a hardened
  internal carrier;
- no response-level `Tensor2[A]` or general tensor framework is introduced;
- adapter and ownership-transfer rules are explicit.

Current evidence narrows this gate. `image.DMat` is already a cross-platform,
row-major `NArray[Double]` value with `fromRowMajorOwned`, so there is no need
to invent a tensor abstraction. Its public mutable `data` field means it
cannot satisfy the response ownership contract unchanged. The remaining
choice is the private primitive carrier and the exact copy/transfer behavior
of adapters.

Evidence required:

- primitive buffer adoption;
- row-major indexing;
- no public mutable-buffer escape or ownership-preserving `copy`;
- copies at adapters whose destination buffer can escape;
- JVM/Scala.js behavior;
- no Gale/Breeze exposure in the kernel;
- selected reconstruction benchmark.

### D3. Decode-program encoding

Resolved in Phase 0 before Phase 3:

- free applicative;
- custom typed applicative AST;
- another typed inspectable representation.

Evidence required:

- typed heterogeneous requests without casts;
- batching and request inspection;
- compile-time and runtime cost;
- data-dependent escape hatch;
- Scala.js output size and behavior.

### D4. Time-domain authority

Before Phase 1:

- extract general acquisition sampling from current `SamplingFrame`;
- or adapt it into a response-owned `TimeDomain`.

Evidence required:

- exact origin and precision preservation;
- regular and explicit coordinates;
- multi-block behavior;
- no dependency cycle through HRF/design.

### D5. Artifact names

Resolved in Phase 8:

- `response` is the neutral response-kernel artifact;
- `archive` is the format-neutral archive API artifact;
- `archive-lna` owns the versioned LNA 2 schema and physical HDF5 driver;
- `archive-zarr` owns the NeuroArchive Zarr profile;
- `interop-archived-response` owns representation/archive lowering, LNA
  pipeline reconstruction, archive-aware dataset adapters, and runtime
  assembly.

The graph is acyclic, domain package names follow their owned values, every
artifact has an ownership README, and an executable guard checks both physical
source placement and prohibited build edges.

### D6. Manifest canonical-value representation

Resolved in Phase 0 before any target manifest or representation envelope is
implemented:

- canonical JSON-like value;
- typed descriptor codec boundary;
- preservation of exact integers and deterministic rendering.

Evidence required:

- JVM/Scala.js parity;
- schema migration;
- unknown-field policy;
- no untyped map leakage into representation kernels.

### D7. Duplicate selection semantics

Resolved in Phase 0 before exposing the Phase-1 response selection:

- continue public rejection;
- or support ordered gathers with duplicates.

The initial dataset adapter preserves current rejection. Storage planners may
already support duplicate destination positions internally.

### D8. Receipt aggregation

Resolved in Phase 0 before either Phase-1 common receipt work or Phase-2
receipt adaptation begins:

- explicit receipt accumulator;
- writer-style internal interpreter;
- treatment of caching and retries.

Evidence required:

- no loss or double counting;
- deterministic logical summaries;
- honest distinction between planned and actual reads.

### D9. Sample-domain geometry authority

Resolved in Phase 0 before Phase 1:

- `response` owns finite sample identity, cardinality, ordering, and
  content-verifiable geometry/topology references;
- `image` owns concrete volume geometry;
- `surface` owns concrete mesh geometry and topology;
- dataset-domain adapters validate and resolve coordinates.

Evidence required:

- no response dependency on image, surface, or graph;
- exact volume-space and mask compatibility;
- surface identity and topology mismatch tests;
- no implicit resampling or coordinate transformation.

### D10. Axis-keyed locality and conformance

Resolved in Phase 0 before the Phase-1 capabilities API:

- capabilities are keyed by selected axis or axis combination;
- `WholeObject` and `WholePayload` remain incomparable without layout
  evidence;
- receipt conformance is defined against the opened layout;
- strict-read policy controls undeclared fallback.

Evidence required:

- mixed time/sample source fixtures;
- whole-payload, chunk-cover, and byte-range receipts;
- no ordinal comparison of locality cases;
- no loss or double counting during fallback, retries, or caching.

## 31. Rejected Designs

### Universal codec

Rejected:

```scala
trait CompressionCodec:
  def encodeDense(...)
  def writeArchive(...)
  def readSelection(...)
  def decodeRepresentation(...)
  def openDataset(...)
```

It combines unrelated responsibilities because they occur in one workflow.

### Principal-module inheritance

Rejected:

```text
ArchiveBackend extends LatentResponse
LatentResponse extends DatasetBackend
```

Substitutability does not hold.

### Global plugin discovery

Rejected as core behavior. Optional discovery may produce explicit registry
entries outside archive semantics.

### Closed representation enum

Rejected because external representation modules must participate without
editing archive core.

### Untyped payload batch

Rejected because request/result association, scalar type, and shape become
runtime casts.

### Generic response tensor

Rejected:

```text
ResponseBlock[A](Tensor2[A])
```

The decoded fMRI response contract is row-major time-by-sample `Double`.
Scalar and rank genericity belong to logical payloads and physical arrays, not
the response block.

### Effect-parameterized scientific dataset

Rejected:

```text
FmriDataset[F, A]
```

It spreads storage effects and irrelevant response scalar parameters through
model descriptions and numerical kernels. `FmriDataset` remains pure;
`OpenedDataset[F]` owns effectful reads.

### Concrete geometry in the response kernel

Rejected because volume geometry belongs to `image` and mesh topology belongs
to `surface`. The response kernel carries verifiable domain references and
ordering only.

### One scalar locality claim

Rejected because physical locality can differ by selected axis and
`WholeObject` is not globally stronger or weaker than `WholePayload`.

### Dataset-resolved latent coordinates

Rejected because coefficient, atom, transport, and residual domains are
representation internals.

### Selected result implies bounded IO

Rejected because an eager source can select after reading a whole payload.

### One study-wide response matrix

Rejected because acquisitions can differ in time, geometry, mask, ordering,
space, and signal semantics.

### Implicit encoder or persistence policy

Rejected because multiple ranks, formats, quantization policies, and residual
policies are legitimate runtime choices.

## 32. Definition of Done

This PRD is implemented only when:

- the exact committed migration baseline and frozen numerical corpus are
  recorded;
- the target dependency rules hold in `build.sbt`;
- `response` imports no image, surface, graph, archive, latent, or dataset
  module;
- response-kernel laws pass on JVM and Scala.js;
- `ResponseBlock` directly owns primitive row-major `Double` storage without a
  general tensor API or public mutable alias;
- at least one nontrivial representation has typed selected decoding;
- that representation runs through memory, LNA/HDF5, and Zarr bindings;
- cross-path agreement uses decode-path consistency separately from
  reconstruction fidelity;
- unknown-representation archives remain inspectable;
- archive persistence and representation fidelity are tested separately;
- interrupted LNA, Zarr, and canonical-manifest writes never appear published;
- checked dataset attachment replaces format-specific scientific opening
  logic;
- `FmriDataset` and model/fit plans remain pure while
  `OpenedDataset[F]` owns effectful reads;
- `ScalafimRuntime.openResponse` and `openDataset` satisfy RUN-001 through
  RUN-004;
- provenance and receipts remain distinct and reach the user-facing result;
- axis-keyed capability claims are enforced by observed-access tests;
- Phase-8 import and build-edge guards reject archive/latent dependencies from
  dataset core;
- no deprecated alias or historical response/archive name remains;
- every phase preserves the frozen numerical corpus;
- module READMEs and `docs/module-relations.md` reflect the live graph;
- `sbt compileAll` and `sbt testAll` pass warning-clean.

Until then, implementations and documentation must distinguish:

- current verified behavior;
- current cross-domain adapters;
- completed migration phases;
- aspirational target architecture.

## 33. Final Architectural Contract

For each acquisition:

```text
Yₖ : Timeₖ × Sampleₖ → Double
```

`dataset`:

```text
keeps scientific identity and query compilation pure;
OpenedDataset[F] attaches response sources and executes resolved reads
```

`latent`:

```text
compiles output-domain selections into typed
logical payload reads and reconstruction plans
```

`archive`:

```text
lowers payload plans into verified physical reads
against a durable published revision
```

`interop`:

```text
binds representation slots to archive payload identities
without moving mathematics into storage or hierarchy into decoding
```

`runtime`:

```text
explicitly assembles trusted formats, representation families,
stores, limits, and cross-domain adapters
```

The product succeeds when these modules feel like parts of one compiler
without becoming one god object: scientific intent lowers toward bytes, and
typed values, integrity evidence, reconstruction contracts, and provenance
rise back toward the user.
