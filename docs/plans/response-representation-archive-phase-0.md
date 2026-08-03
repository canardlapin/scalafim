# Response/Representation/Archive Phase 0 Record

Status: complete and repository-wide verified on 2026-07-26  
Issue: `bd-01KYAA8KS1B9F0D6SZ00GCRESJ`  
Governing plan: [response-representation-archive-architecture.md](response-representation-archive-architecture.md)

This record freezes the input to the response/archive convergence. It is not a
description of the target as though it already existed. The baseline sections
describe committed behavior at the start commit; the decision sections are
normative for Phases 1 and later.

## 1. Exact migration baseline

| Item | Frozen value |
| --- | --- |
| Repository | `canardlapin/scalafim` |
| Branch at capture | `main` |
| Start commit | `7c590372dd3cea733f5835b9ca1bb6f62e92292f` |
| Start subject | `merge: complete finite indexed-space migration` |
| Scala | `3.4.2` |
| sbt | `1.10.5` |
| Governing-plan commit | `7afc0ee19167eeb8f2acc4380e7733e3e1f0951e` |
| Committed dataset bridge | `b9d96e2a23e24a57beeb36b978b14935c81071f5` |
| LNA manifest family | `LNA R v2.0` |
| Zarr profile | `neuroarchive-zarr-0.1` |

Commits `4d2ddddf5b7a7183a16235bb15eb50abcbfe75ed` (the
Scala 3.7.4 experiment) and `b6a3af2af4dad57413e80ebce0ddf33633d704a9`
(frame4s extraction) are ancestors of the start commit. They are not unmerged
prerequisites. The build at the start commit authoritatively selects Scala
3.4.2. No worktree-only prerequisite is part of this convergence.

The historical receipt at
`docs/benchmarks/receipts/response-archive-phase0-baseline-2026-07-26.json`
records file hashes, commands, environment evidence, and the frozen corpus
definition. A dirty checkout never changes the meaning of the baseline:
the start commit plus the receipt's path hashes do.

## 2. Live dependency graph

The relevant start-commit edges are:

```text
image
  └── cats-effect 3.5.4

archive
  └── image

latent
  ├── archive
  └── locus-kernel

archive-zarr
  ├── archive
  └── zarr

dataset
  ├── image
  ├── hrf
  ├── archive
  ├── latent
  ├── bids
  └── locus modules

dataset-zarr
  ├── dataset
  ├── archive-zarr
  ├── image
  └── bids
```

These are facts about the migration input, not approved target edges. The
normative target remains:

```text
response                       no internal scientific dependencies
latent                         response + linalg
dataset                        response
archive                        response vocabulary only when persisted
archive-lna                    archive
archive-zarr                   archive + zarr
geometry adapters              dataset + image or dataset + surface
interop-archived-response      response + latent + archive
runtime                        selected implementations and interop
```

Artifact names remain provisional under D5. Dependency direction does not.

## 3. Cross-domain seam inventory

### 3.1 Mechanical two-principal-domain guard

At the start commit, the import guard identifies the following twelve
production files. “Principal domain” means the response, representation,
dataset, or archive responsibility, including an explicit import of a
file's own principal package.

| Current file | Current responsibility | Target owner |
| --- | --- | --- |
| `dataset-zarr/jvm/.../FmriDatasetZarr.scala` | Opens a Zarr archive and constructs a scientific dataset | runtime composition with a directly named synchronous adapter |
| `dataset-zarr/jvm/.../NiftiCanonicalImporter.scala` | Reads NIfTI/image data and publishes the canonical Zarr profile | format importer assembled by runtime |
| `dataset-zarr/jvm/.../ZarrResponseBlockSource.scala` | Binds archive reads, image geometry, and dataset blocks | archived-response interop plus volume-domain adapter |
| `dataset-zarr/shared/.../CanonicalBoldSampling.scala` | Converts persisted acquisition timing into dataset/HRF timing | response-time adapter at the dataset/archive boundary |
| `dataset/jvm/.../FmriDatasetLna.scala` | Decodes LNA representation families while opening a dataset | runtime/archived-response dataset adapter |
| `dataset/jvm/.../LnaDataset.scala` | Opens HDF5, resolves BIDS metadata and shared bases, then builds a dataset | JVM LNA dataset adapter |
| `dataset/shared/.../DatasetError.scala` | Exposes archive and latent errors from dataset core | dataset-only error ADT; translate foreign failures at adapters |
| `dataset/shared/.../LatentArchiveDatasetBackend.scala` | Reconstructs LNA/latent values behind `DatasetBackend` | archived-response interop adapter |
| `latent/shared/.../BoldZipLatentArchiveCodec.scala` | Binds BOLDZip semantics to LNA payloads | archived-response interop |
| `latent/shared/.../ExplicitLatentArchiveCodec.scala` | Binds explicit latent semantics to LNA payloads | archived-response interop |
| `latent/shared/.../SharedBasisLatentArchiveCodec.scala` | Binds shared-basis semantics and image masks to LNA | archived-response interop plus geometry adapter |
| `latent/shared/.../TransportLatentArchiveCodec.scala` | Binds transport semantics to LNA payloads | archived-response interop |

The abbreviated paths above expand under `modules/`; the receipt carries the
exact repo-relative paths.

### 3.2 Complete forbidden-edge inventory

The broader edge inventory is also frozen so that a file cannot evade the
two-domain guard merely by omitting an explicit self-package import.

- `latent -> archive` occurs in ten main-source files:
  `BoldZipLatentArchiveCodec`, `ExplicitLatentArchiveCodec`,
  `LatentArchiveCodec`, `LatentArchiveDescriptors`,
  `LatentArchivePayloads`, `LatentEncoder`, `RadialBasis`,
  `SharedBasisEncoder`, `SharedBasisLatentArchiveCodec`, and
  `TransportLatentArchiveCodec`.
- `dataset -> archive` occurs in four main-source files:
  `FmriDatasetLna`, `LnaDataset`, `DatasetError`, and
  `LatentArchiveDatasetBackend`.
- `dataset -> latent` occurs in those four files plus
  `LatentResponseDatasetBackend`.
- Scientific reconstruction currently lives in archive core at
  `LnaPipeline.reconstruct`; this moves behind representation/interoperability
  ownership rather than remaining an archive operation.
- The two privileged scientific opening paths are the JVM LNA opener and the
  canonical Zarr opener. Both become explicit runtime assemblies over the same
  response contract.

Phase 8 must replace this prose inventory with an executable main-source
import and build-edge guard. Until then, every listed responsibility has the
owner named above.

## 4. Numerical and format inventory

### 4.1 Live representation families

`LatentArchiveKind` is the closed inventory of currently implemented LNA
bindings:

```text
Explicit
TemporalDct
TemporalHaar
SharedBasis
Transport
BoldZip
```

HRBF/radial-basis persistence is a `SharedBasis` representation with a radial
basis artifact; it is not a seventh archive family.

### 4.2 LNA transforms and valid compositions

The live transform kinds are `Quant`, `Basis`, `Embed`, `Delta`, `Temporal`,
and named `Custom` transforms. The implemented reconstruction routes are:

| Route | Persisted logical form | Current inverse |
| --- | --- | --- |
| Dense identity | dense matrix | direct response block/matrix |
| Quant | quantized values + scale/offset | dequantize |
| Delta | first values + delta stream | cumulative decode |
| Delta then quant | first values + quantized delta stream | dequantize, then cumulative decode |
| Basis then embed | coefficients + loadings/basis | matrix reconstruction |
| Temporal DCT then embed | DCT coefficients + temporal basis/loadings | explicit latent reconstruction |
| Temporal Haar | typed custom descriptor and latent payloads | Haar inverse and explicit reconstruction |
| Explicit latent | basis + loadings + optional offset | explicit reconstruction |
| Shared basis | coefficients + external content-addressed artifact | resolve artifact, then reconstruct |
| HRBF shared basis | radial atoms encoded as a shared-basis artifact | resolve artifact, then reconstruct |
| Transport | analysis coefficients + decoder/transform payloads | transport reconstruction |
| BOLDZip | multiresolution BOLDZip payload family | BOLDZip reconstruction |

`LnaPipeline.invertPlan` walks descriptors in reverse. It does not license
arbitrary compositions: a second materializing transform is rejected, a
`Basis` or `Temporal` descriptor requires a downstream matrix, and unknown
custom transforms are unsupported unless a representation binding owns them.

### 4.3 Manifest and publication variants

The live durable variants are:

- LNA manifest `LNA R v2.0`;
- shared-basis registry `scalafim-lna-basis-registry-0`;
- NeuroArchive Zarr profile `neuroarchive-zarr-0.1`;
- Zarr publication receipt version `1`.

These remain independently inspectable migration inputs. Phase 9 introduces a
normalized manifest; Phase 0 does not rewrite any of them.

## 5. Frozen migration corpus

The executable corpus is
`modules/latent/shared/src/test/scala/scalafim/latent/ResponseArchiveMigrationBaselineSuite.scala`.
It uses one deterministic, finite matrix unless a representation requires a
special shape. Each route is constructed through the public encoder/archive
API and decoded through the current production reconstruction path.

| Case id | Route | Shape | Comparator |
| --- | --- | --- | --- |
| `dense` | direct dense | 4 x 4 | raw IEEE-754 bits |
| `lna-quant` | 16-bit quant | 4 x 4 | raw IEEE-754 bits |
| `lna-delta` | delta | 4 x 4 | raw IEEE-754 bits |
| `lna-delta-quant` | delta then 16-bit quant | 4 x 4 | raw IEEE-754 bits |
| `lna-basis-embed` | identity basis/embed | 4 x 4 | raw IEEE-754 bits |
| `latent-explicit` | explicit basis/loadings/offset | 3 x 4 | raw IEEE-754 bits |
| `lna-temporal-dct` | archive-core DCT route | 4 x 4 | abs `1e-12`, rel `1e-12` |
| `latent-temporal-dct` | latent-codec DCT route | 4 x 4 | abs `1e-12`, rel `1e-12` |
| `latent-temporal-haar` | latent Haar route | 4 x 4 | abs `1e-12`, rel `1e-12` |
| `latent-shared-basis` | non-orthogonal shared basis | 3 x 3 | abs `1e-12`, rel `1e-12` |
| `latent-hrbf-shared-basis` | Gaussian radial shared basis | 3 x 3 | abs `1e-12`, rel `1e-12` |
| `latent-transport` | identity transport decoder | 4 x 4 | raw IEEE-754 bits |
| `latent-boldzip` | identity-detail BOLDZip | 4 x 4 | raw IEEE-754 bits |

JVM output at the start baseline supplies the golden raw bits. Scala.js is
bit-identical for every raw-bit case. Its DCT accumulation differs from JVM
only in low-order bits and is checked against the frozen JVM values with the
declared absolute/relative comparator. Goldens may change only through a
separately reviewed numerical-change decision; a migration phase must not
“refresh” them to make a regression pass.

## 6. Phase 0 decisions

### D1 — Effects and resources

Decision: admit Cats Effect 3.5.4 `Resource` and `EitherT` to shared
response/archive boundaries.

- `FmriDataset`, scientific metadata, model descriptions, and fit plans remain
  pure.
- `ResponseSource[F]`, `OpenedDataset[F]`, archive resources, and runtime
  interpreters own effects.
- A synchronous `FmriDataset.open` facade remains only for sources that are
  already materialized or whose interpreter is explicitly synchronous. There
  is no synchronous browser facade over asynchronous IO.
- Errors remain typed values inside the effect. `EitherT[F, DomainError, A]`
  is an implementation convenience, not the persisted model.
- Acquisition and release are bracketed by `Resource`; cancellation must run
  finalizers on JVM and Scala.js.

Evidence: Cats Effect is already a cross dependency of `image`, so the target
`response` stack does not introduce a second effects library. The shared
`CatsEffectSharedBoundarySuite` exercises release after success, failure, and
cancellation on both platforms. Its Scala.js link is also the admission test.
Linked output size is recorded in the receipt; Phase 1 must repeat the
measurement for the real response artifact.

### D2 — Owned response block

Decision: no tensor data structure is needed.

`ResponseBlock` is a final, non-generic, time-by-sample value that directly
owns one row-major primitive `Double` buffer. It exposes shape, checked scalar
access, selected immutable views or copies, and explicit adapters. It does not
expose its mutable carrier.

The initial carrier may be `NArray[Double]` because it is primitive on both
targets. Construction has two paths:

- public constructors validate and copy any buffer whose ownership is not
  transferred;
- a package-private `fromRowMajorOwned` adopts a freshly allocated buffer
  under a documented transfer contract.

`copy` must allocate a distinct carrier. Adapters copy whenever the
destination can expose or retain the buffer. Index arrays are constructed
through opaque-index companion factories, never as generic
`Array[AxisIndex[A]]`.

`image.DMat` proves the row-major cross-platform representation, but its
public `data` field permits mutation and aliasing. It is evidence, not the
response type. Gale and Breeze types do not enter the response API. Scalar
genericity remains in payload slots and physical arrays where float and
integer encodings actually differ.

### D3 — Decode program

Decision: use a project-owned typed applicative AST, not an untyped request
batch and not a public dependency on a generic free-applicative encoding.

The Phase 3 core has these semantic nodes:

```text
Pure[A]
Request[A](PayloadRequest[A])
Map[A, B](DecodePlan[A], A => B)
Zip[A, B](DecodePlan[A], DecodePlan[B])
```

`PayloadRequest[A]` is a GADT whose result type is fixed by its payload slot.
The interpreter can inspect and batch independent `Request` leaves before
execution. Request/result association never uses a public cast or
`Map[String, Any]`.

A separately named `Dependent`/sequential node is the only data-dependent
escape hatch. It is a batching barrier, is visible during inspection and
costing, and is not used by the first representation unless an actual
dependent read requires it. The normal representation contract remains
applicative.

The Phase 0 typing spike must compile on JVM and Scala.js, demonstrate
heterogeneous typed requests, inspect leaves without executing them, and
include a negative compile-time case showing that two payload result types
cannot be exchanged.

### D4 — Time authority

Decision: `response` owns a neutral `TimeDomain`; dataset/HRF code adapts the
current `SamplingFrame` into it.

`TimeDomain` represents ordered blocks. A block is either:

- regular: sample count, exact origin seconds, exact step seconds, and exact
  precision; or
- explicit: a non-empty ordered coordinate vector and exact precision.

Time scalars preserve finite IEEE-754 `Double` values exactly. Regular
construction requires positive step and precision and non-negative origin.
Explicit construction requires finite, strictly increasing coordinates.
Block identity and local/global origin semantics are explicit.

The `SamplingFrame` adapter preserves `blockLens`, `tr`, `startTime`, and
`precision` raw bits. Its current default origin of `TR / 2` is not silently
changed. The response module does not import HRF or design; the adapter lives
at the dataset-time boundary. Multi-acquisition concatenation retains block
boundaries rather than synthesizing one study-wide clock.

### D6 — Canonical manifest values

Decision: introduce a closed canonical value ADT with exact scalar semantics:

```text
Null
Boolean
String
Int64
Float64Bits
Array[CanonicalValue]
Object[CanonicalKey, CanonicalValue]
```

`Float64Bits` preserves negative zero, infinities, and NaN payload bits.
Objects reject duplicate keys and render keys in canonical UTF-8 byte order.
Arrays preserve order. Integer values never round through `Double`.

Typed descriptor codecs are the only representation-facing API. Decoding a
known descriptor returns the typed value plus retained unknown fields so a
read/write cycle does not discard forward-compatible data. An unknown
representation remains a raw canonical descriptor in the archive envelope.
No `Map[String, Any]`, platform JSON AST, or stringly typed scalar leaks into
representation kernels.

### D7 — Duplicate selections

Decision: public dataset and response selection rejects duplicate time or
sample indices initially, preserving current dataset behavior.

Lower storage and reconstruction planners may use ordered gathers containing
duplicates. Such gathers are internal typed plans, preserve destination
order, and are accounted once per physical access but once per logical output
position. Supporting public duplicates later requires an explicit API change
and laws; it is not inferred from storage capability.

### D8 — Receipt aggregation

Decision: use an explicit immutable `ReceiptAccumulator`, interpreted
internally with writer-style composition where convenient.

- A plan estimate and an execution receipt are distinct values.
- Receipt fragments are appended in deterministic traversal/attempt order.
- Physical byte and object totals derive from observed successful accesses,
  not requested ranges.
- Retries retain every attempt and count bytes actually transferred.
- Cache hits record the logical demand and cache evidence but add zero remote
  bytes; cache fills record their physical read once.
- Summaries may normalize repeated object/range evidence only after preserving
  the detailed fragments from which totals are derived.
- Combining a fragment twice is never treated as idempotent implicitly.

This choice makes loss, double counting, fallback, and retry behavior
testable without placing telemetry or mutable logging inside pure plans.

### D9 — Sample-domain geometry authority

Decision: `response.SampleDomain` owns only finite identity, cardinality,
ordering, and content-verifiable references.

It may carry a volume-space digest, mask/order digest, surface digest, or
topology digest as opaque references. It never contains an affine, voxel
coordinate implementation, mesh, graph, or resampler.

`image` remains authoritative for volume geometry and mask indexing.
`surface` remains authoritative for mesh geometry and topology.
Dataset-domain adapters resolve references and require exact compatibility
unless an explicit scientific transform/resampling plan is supplied.
Consequently `response` has no dependency on image, surface, or graph.

### D10 — Axis-keyed locality

Decision: locality is an opened-layout conformance relation, not an ordered
enum.

A source declares selectable axes and a claim for each supported axis or axis
combination. A claim denotes the set of physical accesses allowed for a
resolved logical selection under one opened layout. Examples include
resident, exact byte ranges, chunk cover, whole logical payload, and whole
physical object.

Claim `A` is stronger than claim `B` only when both refer to the same opened
layout/axis selection and every access permitted by `A` is permitted by `B`.
`WholePayload` and `WholeObject` are therefore incomparable without evidence
that maps the payload to objects. No ordinal or pattern-match ordering is
valid.

The default execution policy is strict: if the interpreter cannot honor the
declared claim, it fails before undeclared fallback. An explicit
`AllowFallback` policy may permit a weaker path, but the receipt records the
planned claim, observed accesses, and fallback reason. Conformance tests use
instrumented whole-payload, chunk-cover, and byte-range sources with mixed
time/sample locality.

### D5 — intentionally deferred

Only artifact names remain open. D5 is resolved before Phase 8 using the
already-fixed dependency rules. It may not reopen D1-D4 or D6-D10.

## 7. Acceptance and continuation rule

Phase 0 is complete only after:

- the frozen corpus passes on JVM and Scala.js;
- the shared Cats Effect and decode-typing spikes pass on both platforms;
- current dataset bridge suites pass;
- the historical receipt validates;
- `sbt compileAll` and `sbt testAll` pass warning-clean;
- the active mote contains the commands and results.

No production source is moved in this phase. A failed numerical comparator,
resource-finalization law, bridge test, or target-graph decision stops the
migration. Once this gate is green, RRA 1 and RRA 2 may start in parallel
against this record.

## 8. Verification evidence

The Phase 0 gate passed on 2026-07-26 with Oracle OpenJDK 22 in headless mode.
Headless mode is required by the Java2D backend in the non-interactive test
environment; it does not change production code or numerical behavior.

The authoritative commands were:

```text
sbt compileAll
sbt -Djava.awt.headless=true testAll
python3 tools/response_archive/capture_phase0_baseline.py --stdout --run-corpus
python3 tools/response_archive/capture_phase0_baseline.py --check --live-sources
```

The first two commands passed warning-clean across the complete JVM and
Scala.js project aliases. The corpus runner passed all thirteen historical
routes on both platforms. Raw-bit routes were identical across platforms; the
two temporal-DCT routes differed only within their declared absolute and
relative `1e-12` comparator.
