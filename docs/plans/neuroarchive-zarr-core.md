# Scala Zarr Core — a reusable kernel, delivered NeuroArchive-first

Status: normative architecture for the first storage implementation\
Library direction: `scala-zarr`\
First domain profile: `neuroarchive-zarr-0.1`\
Reviewed against the live ScalaFIM tree and Zarr specifications on 2026-07-20

This document sharpens the Zarr portion of
[`neuroarchive-data-access.md`](neuroarchive-data-access.md). The parent plan
defines the archive system, BIDS compatibility, scientific metadata, migration,
and later layouts. This document owns the narrower question:

> What is the smallest genuine Zarr implementation ScalaFIM should own so that
> it can grow into a useful cross-platform Scala library, while solving the
> BOLD acquisition problem first?

## Decision

Build a small but genuine Scala 3 implementation of **Zarr v3 array mechanics**,
then define **NeuroArchive Zarr 0.1** as its first strict domain profile.

The reusable kernel is rank-generic at runtime. Shape, coordinates, regular
chunk grids, regions, gathers, shard indexes, and copy plans work for any
finite rank accepted by Zarr metadata. Rank four and `[t,z,y,x]` belong only to
`CanonicalBold`, one validator and adapter above the kernel.

The governing rule is:

> General in dimensionality and composition; narrow in implemented Zarr
> capabilities and initial workloads.

This is not a promise to implement every Zarr feature. It is a refusal to bake
today's BOLD shape into otherwise reusable array, chunk, and transport code.

The owned core will implement:

- exact Zarr v3 group and array metadata parsing, with deterministic rendering
  for metadata it writes;
- validation from spec-shaped metadata into an executable array descriptor;
- arbitrary finite-rank shape, coordinate, chunk-key, region, and ordered-
  gather algebra;
- pure direct-chunk and indexed-shard read planning;
- `sharding_indexed` index parsing, encoding, and range planning;
- a small extensible codec-capability boundary with independently decodable
  chunks;
- filesystem and HTTP range interpreters on the JVM;
- Fetch/HTTP Range reading on Scala.js;
- a deterministic create-only local writer on the JVM;
- Python differential fixtures and JVM/Scala.js request-trace conformance
  tests.

The NeuroArchive layer will implement:

- validation of one rank-four canonical BOLD array with dimensions exactly
  `[t,z,y,x]`;
- stored-scalar calibration, spatial and temporal semantics, BIDS identity,
  immutable publication receipts, and stricter missing-object rules;
- ordered timepoint-by-voxel requests through the existing dataset API;
- faithful NIfTI/BIDS import and export.

The first core release will not implement:

- Zarr v2;
- every data type, grid, codec, extension, or storage transformer;
- mutable arrays, resize, append, partial shard updates, or concurrent writers;
- S3 authentication, retries, cache eviction, catalogs, permissions, or job
  scheduling;
- dataframe semantics, visualization pyramids, or virtual arrays;
- compression algorithms themselves.

This is the useful Oderskyan move: separate a small lawful algebra from a
precise domain profile. It is not a license to encode arbitrary runtime rank in
match types or make users perform type-level arithmetic. Dynamic Zarr metadata
is validated once; the kernel then carries rank agreement and bounds in
always-valid values. Neuroimaging adds stronger typed meaning only after that
generic validation succeeds.

## Why this is the right-sized lift

A full Zarr implementation remains too large and too unstable a dependency
surface for ScalaFIM to own immediately. It would require every extension
point, codec family, dtype, store behavior, mutation rule, and compatibility
quirk. None of that is necessary to solve the fMRI access problem.

Arbitrary rank is different. Once coordinates are represented as checked
primitive vectors, the algorithms for chunk intersection, key formation,
linearization, shard lookup, and copying are already loops over axes. Hard-
coding four fields saves little code and creates a redesign as soon as we need
multi-echo BOLD, a time-by-voxel matrix, a surface array, a pyramid level, or an
ordinary non-neuroimaging fixture.

The first release is tractable because it makes seven strong reductions:

1. Zarr v3 only.
2. Regular chunk grids only.
3. A small initial scalar and codec set, compiled through explicit
   capabilities.
4. Direct chunks plus indexed sharding; no general storage transformers.
5. Create-only whole-array writing; no mutation or resize.
6. Storage code returns owned raw bytes; scientific interpretation stays in
   profile adapters.
7. NeuroArchive 0.1 exercises one immutable canonical BOLD publication before
   broader compatibility work is promoted.

The expected implementation is roughly 5,000–9,000 production lines across the
generic kernel, platform readers, writer, NeuroArchive profile, publication,
and adapters, with at least as much conformance and property-test code. That is
a planning estimate, not a quota. Rank-generic geometry is not the expensive
part; codec breadth, mutation, store breadth, and compatibility quirks are. We
hold those behind explicit gates.

## Two constitutions: library subset and domain profile

The reusable kernel and the NeuroArchive profile have different obligations.
A generic reader may understand arrays that are not valid NeuroArchive data;
the profile must never weaken ordinary Zarr semantics.

| Concern | Scala Zarr 0.1 kernel | NeuroArchive Zarr 0.1 profile |
| --- | --- | --- |
| Zarr version | Format 3 metadata, long-form v3.0-compatible extension objects when writing | Same |
| Rank | Any finite runtime rank, including rank zero; zero-length array dimensions are modeled | Exactly four, non-empty, named `t,z,y,x` |
| Hierarchy | Root array or group with array/group children | One group containing `canonical` plus profile documents |
| Grid | Regular grid only | Regular grid only |
| Chunk keys | Default encoding with `/` separator initially; representation does not assume rank four | Default `/` only |
| Physical layouts | Direct chunks and indexed shards | Direct for fixtures/local data; start-indexed shards for remote publication |
| Shard index | Rank-generic parser/planner models `start` and `end`; first remote writer emits `start` | `start`, little-endian `uint64`, CRC32C protected |
| Scalar types | Capability set begins with `uint8`, `int16`, `int32`, `float32`, `float64`; metadata can diagnose other core/extension types | Same five types in 0.1 |
| Byte order | Declared by the bytes codec | Little-endian canonical storage for multibyte scalars |
| Fill | Ordinary Zarr fill semantics | Explicit and representable; publication receipt distinguishes absence from failed upload |
| Compression | Explicit codec capabilities; bytes/gzip/CRC32C baseline | One required portable chain; fast chain only after gates |
| Missing objects | Decode as fill according to Zarr | Missing receipt-listed objects are invalid publication |
| Mutation | Read plus create-only whole-array write | Immutable after publication |
| Metadata authority | Zarr metadata owns array storage | NeuroArchive manifest owns scientific meaning; Zarr metadata owns array storage |

### Supported and rejected Zarr features

The raw parser recognizes extension-shaped metadata without pretending it can
execute every extension. `ArrayDescriptor.compile` admits the first executable
subset; `CanonicalBold.validate` imposes the stricter profile:

| Feature | 0.1 disposition |
| --- | --- |
| Groups and arrays | Required |
| `dimension_names` | Optional and rank-matched in the kernel; required as `["t","z","y","x"]` by canonical BOLD |
| Regular chunk grid | Required |
| `bytes` | Supported and required as the array-to-bytes stage for the initial numeric dtypes |
| `crc32c` | Supported generically; required by NeuroArchive inner chunks and shard indexes |
| `gzip` | Supported but optional for an ordinary array; required portable NeuroArchive compressor |
| `blosc` with `cname=zstd` and byte shuffle | Optional fast capability after Z5 |
| `sharding_indexed` | Supported generically; required with a start index for remote NeuroArchive publication |
| `transpose` | Metadata preserved but execution deferred; rejected in the canonical profile |
| Storage transformers | Rejected |
| Unknown codecs or must-understand extensions | Typed unsupported-feature error |
| Other core, variable-length, structured, complex, or extension dtypes | Parsed and reported as unsupported until a capability is supplied |
| Non-regular grids | Rejected |
| Missing dimension names | Accepted by the kernel; rejected for canonical BOLD |
| Zarr v2 metadata | Rejected |

The first portable NeuroArchive writer chain is:

```text
stored scalar array
-> bytes(endian = little)
-> gzip(level = 1)
-> crc32c
```

Gzip here is chunk-local, not whole-acquisition gzip. It is selected as the
first conformance baseline because it does not require a native compressor and
is defined by the accepted Zarr codec specifications. A fast production chain
using Blosc/Zstandard and shuffle is admitted only after benchmark, JVM
packaging, Python differential, and corruption gates pass. Codec choice is
recorded in metadata; neither chain changes the logical payload identity.

## The first NeuroArchive revision

The generic kernel can open an ordinary Zarr root array or hierarchy. It knows
nothing about acquisition revisions, catalogs, BIDS, or publication receipts.
The first NeuroArchive use of that kernel is intentionally boring:

```text
<acquisition-revision>/
├── zarr.json
├── neuroarchive.json
├── publication.json
└── canonical/
    ├── zarr.json
    └── c/
        └── <t>/<z>/<y>/<x>   # rank-four profile instance of a generic key
```

- `zarr.json` at the root declares a Zarr group and contains only the profile
  link and non-scientific attributes.
- `neuroarchive.json` is the scientific manifest: acquisition identity,
  source BIDS reference, axes, scalar calibration, geometry, time semantics,
  payload identity, and provenance.
- `canonical/zarr.json` describes the physical Zarr array.
- `publication.json` is written last and proves that the immutable revision is
  complete.

The institutional catalog maps a logical acquisition/release to this immutable
root. It is not inside the store. A later masked layout, pyramid, or latent
product is another revision-bound payload or store, not an excuse to broaden
the canonical reader before the need exists.

### Publication is stricter than ordinary Zarr

Ordinary Zarr permits a missing chunk to mean fill. That is unsafe for a
scientific publication because a failed upload is observationally identical to
intentional fill.

The 0.1 writer therefore materializes every expected outer chunk object. An
indexed shard may use the standard absent-inner-chunk sentinel for an
intentionally all-fill inner chunk, because that decision is protected by the
shard index checksum. A missing shard or direct chunk listed by the publication
receipt is always an error.

Listing the bucket or directory is never required. The receipt contains the
complete expected object inventory.

## Scientific semantics stay above Zarr

The canonical logical tensor is:

```text
BOLD[t, z, y, x]
```

The order is normative, not inferred. In C order, `x` is the fastest-changing
axis, which matches the useful sequential organization of ordinary NIfTI
volumes while putting time first in the named logical model.

The array stores the original scalar representation, normalized only to the
profile's little-endian byte order. Calibration remains explicit:

```text
physical value = stored value * scale + offset
```

The 0.1 scalar set matches the useful initial NIfTI boundary:

```text
uint8 | int16 | int32 | float32 | float64
```

Import must preserve integer values and floating-point bit patterns across
endian normalization. It must not eagerly convert an `int16` acquisition to
`float32` or `float64`. The dataset adapter applies scale and offset once while
assembling the requested result.

The NeuroArchive manifest, not Zarr attributes, owns:

- the source BIDS dataset, relative path, parsed entities, and artifact digest;
- acquisition and payload identifiers;
- stored dtype, physical dtype, scale, offset, and signal units;
- shape and named-axis meaning;
- voxel-to-world affine;
- preserved qform/sform matrices, codes, quaternion/qfac source fields, and
  their export policy;
- repetition time, acquisition times where explicit, slice timing, and time
  units;
- canonical logical payload hash and derivation/provenance.

The array attributes contain only stable links such as profile ID and payload
ID. A generic Zarr reader can still read numbers; a NeuroArchive reader is
required to interpret them scientifically.

## The Scala 3 shape

There are three deliberate layers:

1. `ZarrNodeMetadata` is a spec-shaped parse result. It preserves extension
   names/configuration and can represent metadata this release cannot execute.
2. `ArrayDescriptor` is a rank-generic, executable Zarr array. Rank agreement,
   dtype support, grid validity, codec composition, fill representation, and
   resource bounds have been checked.
3. `CanonicalBold` is a NeuroArchive refinement. It proves rank four, exact
   axis names and order, positive acquisition dimensions, approved codecs,
   calibration, and scientific-manifest agreement.

Do not encode dynamically discovered rank as `Axes <: Tuple`. That surface
looks precise but forces existential tuples, casts, or generated match types at
every open boundary. The generic kernel uses small runtime-rank values with
strong construction invariants. Domain adapters may then expose genuinely
typed axes when the domain supplies those facts.

The following is directional API design, not code to paste unchanged:

```scala
opaque type Rank = Int

final class Shape private (private val dimensions: IArray[Long]):
  def rank: Rank
  def axisLength(axis: Int): Long
  def elementCount: Either[ArithmeticOverflow, Long]

object Shape:
  def from(dimensions: IterableOnce[Long]): Either[ShapeError, Shape]
  val scalar: Shape

final class Coordinate private (private val indices: IArray[Long]):
  def rank: Rank

final class Region private (val origin: Coordinate, val extent: Shape)

object Region:
  def within(
      array: Shape,
      origin: Coordinate,
      extent: Shape
  ): Either[SelectionError, Region]

opaque type CoreDataTypeName = String

enum ExecutableDataType(val byteWidth: Int):
  case UInt8 extends ExecutableDataType(1)
  case Int16 extends ExecutableDataType(2)
  case Int32 extends ExecutableDataType(4)
  case Float32 extends ExecutableDataType(4)
  case Float64 extends ExecutableDataType(8)

enum DataTypeMetadata:
  case Core(name: CoreDataTypeName)
  case Extension(name: ExtensionName, configuration: JsonObject)

final case class RegularGrid private (
    arrayShape: Shape,
    chunkShape: Shape
)

enum PhysicalLayout:
  case Direct(
      grid: RegularGrid,
      codecs: ExecutableCodecPipeline
  )
  case Sharded(
      outerGrid: RegularGrid,
      innerChunkShape: Shape,
      codecs: ExecutableCodecPipeline,
      index: ExecutableIndexCodec
  )

final case class ArrayDescriptor private (
    path: ZarrPath,
    shape: Shape,
    dimensionNames: IArray[Option[DimensionName]],
    dtype: ExecutableDataType,
    fill: StoredScalar,
    layout: PhysicalLayout,
    chunkKeys: ChunkKeyEncoder
)

final case class CanonicalBold private (
    payloadId: PayloadId,
    array: ArrayDescriptor,
    axes: BoldAxes,
    calibration: ScalarCalibration,
    geometry: VoxelGeometry,
    timing: AcquisitionTiming
)
```

Important properties are established by smart constructors:

- a `Shape` has finite runtime rank and non-negative dimensions; rank-zero
  scalar arrays and zero-length dimensions are represented correctly;
- chunk dimensions are positive, have the array rank, and yield a checked
  finite decoded size; the rank-zero grid has one scalar chunk;
- coordinates, regions, chunk coordinates, and shard coordinates agree in
  rank before planning begins;
- coordinate and extent arithmetic is checked for overflow;
- every non-empty region lies within its array; empty regions produce an empty
  plan rather than a fake chunk read;
- inner chunk dimensions divide shard dimensions;
- chunk and shard decoded sizes fit configured limits;
- fill is representable by the stored dtype;
- codec stage kinds compose from array to bytes exactly once and respect the
  installed capability set;
- `dimension_names`, when present, have the array rank;
- `CanonicalBold` additionally proves `[t,z,y,x]`, positive lengths, a start-
  indexed publication layout, and scientific-manifest agreement.

The future convenience API may expose `ZarrArray[A]` after the caller supplies
or obtains `ElementType[A]` evidence. The storage and planner core does not
depend on that facade, and primitive results must not be forced through boxed
`Array[Any]`. Initial low-level reads return a closed `PrimitiveBlock` ADT or
profile-owned raw scalar blocks.

`Shape`, `Coordinate`, and their chunk counterparts have value equality, copy
input arrays at public construction, and never expose mutable backing storage.
Hot planners use package-private primitive `Array[Long]` plus `while` loops.
The public API remains lawful and inspectable; the inner loop remains
allocation-disciplined on both JVM and Scala.js.

### A useful standalone surface

The kernel must be useful without importing a NeuroArchive package. Its first
public surface should feel approximately like this:

```scala
val opened = JvmZarr.openArray(
  store = FileStore.readOnly(root),
  path = ZarrPath("signals/bold"),
  capabilities = JvmZarrCapabilities.portable,
  limits = ReadLimits.default
)

val block = opened.flatMap(_.readRegion(region))
val points = opened.flatMap(_.readPoints(coordinates))

val browser = BrowserZarr.openArray(
  store = HttpStore(baseUrl),
  path = ZarrPath("signals/bold"),
  capabilities = BrowserZarrCapabilities.portable,
  limits = ReadLimits.default
)
```

`JvmZarr.create` accepts an executable descriptor and chunk provider. Metadata
inspection and pure planning remain available separately for tools that do not
want to execute I/O. `NeuroArchiveZarr.openCanonical` and
`ZarrResponseBlockSource` are consumers of this surface, not privileged paths
inside it.

### JSON is syntax, not the domain model

Zarr shapes, byte counts, and offsets cannot pass through a JSON AST that
stores every number as `Double`. The existing LNA codec is suitable for its
current bounded fields, but it must not be reused blindly for Zarr integers.

`zarr` needs a compact RFC 8259 syntax layer with:

- exact number lexemes and checked `Long`/`BigInt` decoding;
- duplicate-key rejection;
- correct escape and Unicode surrogate handling;
- bounded nesting, string length, array length, and total input size;
- deterministic object-key ordering when rendering profile-owned metadata;
- typed path-aware parse and decode errors.

This may be a small owned parser or a dependency selected by a focused spike.
The acceptance criterion is exact integers and JVM/Scala.js parity, not a
preference for owning lexical analysis. Typed Zarr decoding and profile
validation remain ours either way. There will be no macro-derived public JSON
schema and no untyped map passed into the rest of the system.

## Pure planning is the center

The storage module has a pure center and thin effectful edges:

```text
array selection
-> logical inner-chunk demand
-> shard/object demand
-> byte-range plan
-> store interpreter
-> checksum/decode
-> copy program
-> primitive result block
-> optional domain interpretation
```

### Two requests, one chunk algebra

The first implementation serves two materially different requests:

1. A rectangular region of any rank.
2. An ordered batch of element coordinates of the same rank as the array.

They lower to the same logical chunk demand:

```scala
final class CoordinateBatch private (
    val rank: Rank,
    val count: Long,
    private val rowMajorCoordinates: IArray[Long]
)

enum ArraySelection:
  case Region(value: scalafim.zarr.Region)
  case Points(value: CoordinateBatch)

final case class ChunkDemand(
    chunk: InnerChunkCoord,
    copy: CopyProgram
)

enum CopyProgram:
  case WholeChunk(destinationOffset: Long)
  case Region(source: ChunkRegion, destination: OutputRegion)
  case Gather(sourceOffsets: IArray[Int], destinationOffsets: IArray[Int])
```

`Points` and `Gather` are essential. Sorting coordinates to improve reads is
allowed only inside the plan; destination offsets restore the caller's exact
order, including duplicates. The NeuroArchive adapter lowers selected
timepoints and dense voxel indices into ordered rank-four points without
teaching the kernel what a voxel or timepoint means.

Slices, strides, orthogonal indexing, and named-axis selection can later be
convenience syntax lowered into `Region` or `Points`. They are not new storage
primitives.

Planner laws:

- every requested element is produced exactly once;
- no unrequested element is exposed in the result;
- output ordering equals the scientific selection ordering;
- every demanded chunk intersects at least one requested element;
- repeated planning is deterministic;
- all products, offsets, and byte ranges use checked arithmetic;
- empty requests produce a typed empty result without store access;
- rank mismatches and out-of-bounds requests fail before store access.

### Direct and sharded layouts normalize to inner chunks

Downstream copy/decode code sees logical inner chunks only. Physical layout is
resolved separately:

```text
inner chunk coordinate
  -> direct object key

inner chunk coordinate
  -> outer shard coordinate
  -> shard object key
  -> entry coordinate in shard index
```

For direct arrays, the interpreter reads each demanded chunk object. For
sharded arrays, reading is necessarily two-phase:

1. Compute and range-read the fixed-size index prefix or suffix for each
   touched shard.
2. Verify/decode the index, resolve demanded inner entries, coalesce nearby
   data ranges within explicit limits, then range-read those bytes.

The API must represent those phases rather than fabricate byte offsets before
the shard index exists:

```scala
final case class ShardIndexPlan(reads: Vector[IndexRangeRead])

def resolveIndexes(
    plan: ShardIndexPlan,
    indexes: Map[ShardCoord, ShardIndex]
): Either[ZarrError, ShardDataPlan]
```

### Start-indexed shards are a profile choice, not a kernel assumption

The Zarr sharding specification permits an index at either the start or end of
a shard. NeuroArchive 0.1 writes and requires `index_location = "start"`.

That gives the profile reader a fixed prefix request without first issuing
`HEAD`, knowing the full object length, or relying on suffix-range behavior. It
works well for ordinary HTTP and S3. The generic model retains both standard
index locations. End-indexed execution is enabled only on stores that can
provide a checked object length or a correctly validated suffix range; it is
not valid for NeuroArchive 0.1 publication.

Within a shard, the writer emits inner chunks in C-coordinate order and uses
the standard all-`uint64`-maximum entry only for intentional fill chunks. The
Zarr specification permits other physical ordering; the NeuroArchive profile
chooses one deterministic ordering.

### Plans carry evidence

Planning returns diagnostics as data:

```scala
final case class ReadPlanStats(
    requestedElements: Long,
    requestedLogicalBytes: Long,
    touchedInnerChunks: Int,
    touchedShards: Int,
    indexBytes: Long,
    plannedRangeReads: Int,
    plannedBytes: Long,
    readAmplification: Double
)
```

The interpreter adds actual requests, bytes, elapsed decode time, and cache
status in an execution receipt. Core planners do not log, schedule, retry, or
touch a global cache.

## Narrow effects, explicit ownership

The current `ResponseBlockSource` is synchronous and returns `Either`.
The browser is necessarily asynchronous. We should not distort either side by
blocking Scala.js or by introducing Cats Effect into shared scientific
modules.

The shared kernel therefore owns immutable requests, staged plans, decoded
indexes, codec programs, and result assembly, but no universal effect type.
The same plan has two thin execution surfaces:

- JVM execution is synchronous and returns `Either`, so
  `ZarrResponseBlockSource` meets the live dataset contract directly;
- Scala.js execution returns `Future[Either[ZarrError, A]]` over Fetch and
  browser codec facilities.

This is deliberate duplication of orchestration edges, not algorithms. Both
interpreters consume the same plan and must produce the same request trace and
decoded result fixtures. If a tiny shared effect abstraction later removes
real duplication, it must be justified by code, not installed pre-emptively.

Directional JVM capabilities:

```scala
trait ObjectReader:
  def read(key: StoreKey, range: ByteRange): Either[StoreError, OwnedBytes]
  def readAll(key: StoreKey, limit: ByteCount): Either[StoreError, OwnedBytes]

trait ImmutableObjectWriter:
  def putIfAbsent(
      key: StoreKey,
      bytes: ByteSource,
      length: ByteCount
  ): Either[StoreError, ObjectReceipt]
```

The Scala.js reader has the corresponding asynchronous range operation and no
writer in 0.1. `ByteRange`, `StoreKey`, `OwnedBytes`, errors, and receipts are
shared values.

`OwnedBytes` controls ownership of a primitive `Array[Byte]`; codecs may use an
unsafe accessor only inside the storage package. No public `ByteBuffer`,
`ucar.ma2.Array`, AWS request type, Java stream, or zarr-java class crosses the
capability boundary.

Initial interpreters:

- shared in-memory fixtures with JVM and Scala.js interpreters;
- JVM confined filesystem reader/writer;
- JVM JDK HTTP range reader;
- Scala.js Fetch/HTTP Range reader;
- an HTTP fixture server that records every JVM and Scala.js request.

S3 is not a special array model. Public or presigned S3 objects already use
HTTP. Credentialed bucket access comes later as a small JVM-only interpreter
using the AWS SDK, after the range contract is proven. The SDK, credentials,
retry policy, and multipart upload state stay out of `zarr` shared
types.

Concurrency is an explicit interpreter setting. JVM uses a bounded executor;
Scala.js limits in-flight promises. The implementation must not use the global
common fork-join pool or hidden parallel collections.

## Codecs: own the composition, delegate the algorithms

The library owns:

- validation of the allowed codec chain;
- exact expected decoded length;
- codec configuration parsing/rendering;
- buffer ownership;
- CRC32C verification;
- error mapping and resource limits;
- differential fixtures.

The library does not write gzip, Zstandard, or Blosc algorithms. The shared
layer compiles metadata into an explicit `CodecProgram`; platform executors
supply the algorithms:

```scala
trait JvmChunkCodec:
  def decode(
      encoded: OwnedBytes,
      expectedDecoded: ByteCount,
      limits: DecodeLimits
  ): Either[CodecError, OwnedBytes]

  def encode(decoded: OwnedBytes): Either[CodecError, OwnedBytes]
```

The Scala.js executor has the equivalent asynchronous contract. The baseline
browser path uses the platform gzip-stream capability behind this boundary and
fails with `UnsupportedCodecCapability` when it is unavailable; no browser
global is assumed during metadata validation. Bytes layout and CRC32C remain
small pure shared implementations.

Every inner chunk is independently decodable. Prediction or delta coding may
occur inside an inner chunk in a future profile; a chunk must never depend on
bytes from a previous chunk.

The first executable sharding index pipeline is little-endian bytes plus
CRC32C and is not compressed. Its encoded prefix or suffix length can therefore
be calculated from the rank-generic shard/inner grid before any network
request. NeuroArchive fixes the location to the prefix.

## Deterministic create-only writing

The first generic writer is local, streaming, rank-generic, and create-only. It
accepts an `ArrayDescriptor` plus a `ChunkProvider`; it does not know about
NIfTI, BIDS, acquisition revisions, or scientific hashes, and it does not need
a mutable Zarr API.

For each shard it:

1. Visits arbitrary-rank inner chunk coordinates in canonical C order.
2. Obtains the exact raw scalar block from the provider.
3. Normalizes byte order without converting dtype.
4. Encodes each non-fill inner chunk independently into a temporary spool.
5. Records offset and length entries; marks intentional fill entries.
6. Builds and checksums the fixed index.
7. Writes `index ++ encoded-inner-chunks` to a temporary shard object.
8. Atomically moves the complete shard into the staging revision.

For an ordinary Zarr hierarchy it writes deterministic Zarr metadata only
after payload objects are durable, then atomically moves the completed staging
directory to the requested root.

The NeuroArchive publisher wraps that writer with stronger rules. It supplies
the NIfTI-backed chunk provider, writes the scientific manifest and
`publication.json`, atomically renames the completed revision, and permits an
external catalog pointer to advance only after the receipt exists.

Two identities remain distinct:

- **logical payload hash**: SHA-256 over dtype, shape, and canonical raw scalar
  bytes in `[t,z,y,x]` order; stable across chunking and compression;
- **physical revision identity**: manifest plus exact object keys, lengths,
  SHA-256 digests, storage layout, codec settings, writer version, and source
  logical payload hash.

Compression libraries are not required to produce byte-identical output across
languages. A pinned writer implementation must produce byte-identical objects
for repeated writes with the same plan. Cross-language tests compare decoded
values, metadata semantics, and hashes, not compressed byte identity.

## Opening is a checked operation

Generic `Zarr.openArray` is not a constructor around a URI. It performs bounded
validation:

1. Resolve and read the requested `zarr.json` without listing the store.
2. Decode spec-shaped metadata with exact integers and bounded JSON.
3. Check array rank, shape, dimension names, regular grid, key encoding, fill,
   and codec composition.
4. Compile dtype and codecs against explicit platform capabilities.
5. Enforce decoded chunk, shard-index, and request limits.
6. Return an immutable `OpenedArray` containing an `ArrayDescriptor` and store
   location.

A missing ordinary Zarr chunk is interpreted as fill. Missing metadata is an
open error. No payload chunk is fetched merely to open the array.

`NeuroArchiveZarr.openCanonical` performs the additional publication checks:

1. Read `publication.json` and check profile/receipt version.
2. Read and digest-check `neuroarchive.json` and both `zarr.json` documents.
3. Use the generic opener to compile `canonical`.
4. Refine the descriptor into `CanonicalBold`.
5. Cross-check dtype, shape, axes, calibration, payload ID, and logical hash
   declarations against the scientific manifest.
6. Validate that the receipt inventory contains every expected outer object.
7. Return an immutable `OpenedCanonicalBold` value.

Object digests are verified when objects are read, so opening does not download
the acquisition. Inner CRC32C detects corruption in a partial shard range; the
object SHA-256 can be fully verified by an audit or after the whole shard has
entered a trusted cache.

### Error algebra

The generic library error ADT should distinguish at least:

```scala
enum ZarrError:
  case InvalidJson(path: JsonPath, detail: String)
  case InvalidMetadata(path: JsonPath, detail: String)
  case UnsupportedVersion(found: Int)
  case UnsupportedExtension(name: String)
  case UnsupportedCodec(name: String)
  case UnsupportedCodecCapability(name: String, platform: String)
  case UnsupportedDType(name: String)
  case InvalidShape(detail: String)
  case RankMismatch(expected: Rank, actual: Rank, subject: String)
  case InvalidGrid(detail: String)
  case InvalidCodecChain(detail: String)
  case OutOfBounds(detail: String)
  case ArithmeticOverflow(operation: String)
  case InvalidShardIndex(detail: String)
  case ChecksumMismatch(subject: String)
  case MissingObject(key: StoreKey)
  case ObjectLengthUnavailable(key: StoreKey)
  case RangeIgnored(key: StoreKey, requested: ByteRange)
  case ResourceLimit(limit: String, actual: Long, maximum: Long)
  case Store(cause: StoreError)
```

Profile errors such as `InvalidCanonicalAxes`, `ManifestArrayMismatch`, and
`IncompletePublication` live in `NeuroArchiveZarrError`, which may wrap a
`ZarrError`. An ordinary Scala Zarr user should never need an archive-domain
error type.

`StoreError` separately represents `Unauthorized`, `Forbidden`, `NotFound`,
`Transient`, and permanent transport failures. Public operations return
`Either`; exceptions from filesystem, HTTP, or codec libraries are caught and
mapped at their adapter boundary.

### Resource limits

Every open/read receives an explicit `OpenLimits`/`ReadLimits` value. Defaults
are public named values, never hidden `given` configuration. Limits include:

- metadata bytes and JSON nesting depth;
- decoded string/array/object sizes;
- maximum rank and maximum coordinate-batch size;
- maximum decoded inner chunk and shard size;
- maximum shard-index entries and index bytes;
- maximum ranges, objects, and concurrent requests per operation;
- maximum encoded and decompressed bytes;
- maximum coalescing gap and range length;
- maximum permitted response when a server ignores `Range`.

The HTTP reader requires a valid `206` and matching `Content-Range`. If a server
returns `200` for a non-whole-object request, the reader fails with
`RangeIgnored` after a small bounded response rather than silently downloading
an entire shard.

Filesystem keys are validated relative paths and resolved beneath a fixed
store root. Absolute paths, `..`, empty segments, encoded separators, and
platform-specific escape forms are rejected before IO.

## Integration with live ScalaFIM

The live checkout already contains the scientific seam this storage profile
needs:

- [`ResponseBlockSource`](../../modules/dataset/shared/src/main/scala/scalafim/dataset/ResponseBlockSource.scala)
  defines bounded selected reads and preserves timepoint/voxel order;
- [`NiftiResponseBlockSource`](../../modules/dataset/jvm/src/main/scala/scalafim/dataset/io/NiftiResponseBlockSource.scala)
  proves the synchronous `Either` boundary and bounded positional-read shape;
- [`FitChunkPlan` and `FitChunkProgram`](../../modules/fit/shared/src/main/scala/scalafim/fmri/fit/FitChunks.scala)
  already describe ordered fit work.

The module shape is:

```text
zarr             --> no ScalaFIM domain module
archive-zarr     --> zarr
archive-zarr     --> archive
dataset-zarr     --> archive-zarr
dataset-zarr     --> dataset
dataset-zarr     --> bids
dataset-zarr     --> image
zarr-s3          --> zarr               optional adapter later
```

Each arrow points from a consumer to its dependency:

- `zarr` is an extraction-ready cross-project module under
  `scalafim.zarr`. Shared owns exact metadata, runtime-rank geometry, planning,
  shard indexes, codec programs, primitive blocks, and fixtures. JVM owns
  filesystem/JDK HTTP, synchronous codec adapters, and create-only writing. JS
  owns Fetch, typed-array/`ArrayBuffer` adaptation, asynchronous codec
  execution, and read-only opening. It has no dependency on `archive`,
  `dataset`, `bids`, or `image`.
- `archive-zarr` is a cross-project profile adapter depending on `zarr` and
  `archive`. Shared owns canonical-BOLD refinement and manifest agreement; JVM
  owns NIfTI-backed publication and full audit. Scala.js may open and read an
  already published profile through the generic browser reader.
- `dataset-zarr` is a thin adapter depending on `dataset`, `archive-zarr`,
  `image`, and `bids`; shared owns selection lowering where portable, JVM owns
  `ZarrResponseBlockSource`, NIfTI import, and BIDS/NIfTI export.
- `zarr-s3`, if needed, is also a cross-project module whose concrete
  SDK adapter lives under `jvm`; it depends on `zarr`, with the AWS SDK
  confined to the JVM configuration. No lower module depends on it.

The module name and package follow repository policy while keeping the
dependency seam required for a later standalone `scala-zarr` extraction. We do
not publish or rename it merely to validate the architecture.

`ZarrResponseBlockSource.readBlock` does exactly four scientific things:

1. Validate/map selected timepoints and dense voxel indices into ordered
   rank-four `ArraySelection.Points`.
2. Execute the storage plan to obtain checked raw chunks.
3. Assemble the requested matrix in the caller's order.
4. Apply dtype decoding, scale, and offset into the dataset's primitive
   `Double` result.

Fit, model, design, and linalg code never branch on Zarr. The same
`FitChunkProgram` can consume NIfTI, Zarr, latent reconstruction, or an in-memory
source through the existing dataset boundary.

## zarr-java and ndarray.scala

Neither library should define the ScalaFIM API.

`zarr-java` is useful as:

- an independent JVM differential oracle;
- a fixture generator/reader beside Zarr-Python;
- a temporary escape hatch for a codec not yet behind the narrow capability.

It is not the runtime foundation because its monolithic transitive
numeric/cloud/codec surface, Java array model, mutable operations, execution
policy, and JVM-only boundary conflict with the extraction-ready Scala 3 and
Scala.js kernel. Genericity is no longer the objection; dependency and
execution ownership are. Its current 0.1.3 release still contains an S3
suffix-range defect for end-indexed shards: pull request 80 merged the fix
to `main` on 2026-07-20, after the 0.1.3 release, while issue 79 remains open.
That is evidence for independent request-trace tests and the start-indexed
NeuroArchive choice, not a reason to reject the project.

The executable boundary is `tools/verify_zarr_java_oracle.py`, backed by an
isolated Gradle project under `tools/zarr-java-oracle`. It pins zarr-java 0.1.3
and the official `netcdfAll` 5.9.1 release asset by SHA-256, but contributes no
dependency edge to `build.sbt`. zarr-java reads the Scala-published
NeuroArchive scalar matrix; a shared JVM/Scala.js suite reads an exact fixture
written by zarr-java. This makes the oracle reciprocal while keeping its heavy
JVM graph visibly outside the candidate standalone Scala library.

The Laserson Lab `ndarray.scala/zarr` code is useful design archaeology. It is a
Scala 2-era Zarr v2 reader whose open path eagerly sequences/decompresses
chunks. It does not supply the Zarr v3 sharding and bounded-range substrate
needed here.

## Conformance is the product

Self-round trips are necessary but insufficient. The generic compatibility
matrix is part of the library; the scientific round-trip matrix is part of the
profile.

### Shared JVM/Scala.js laws

- exact JSON number, duplicate-key, Unicode, malformed-input, and limit cases;
- rank/shape/coordinate/chunk-key round trips at ranks 0, 1, 2, 3, 4, 5, and a
  higher adversarial rank;
- zero-length array dimensions, scalar chunks, and rank-mismatch rejection;
- checked multiplication and offset overflow;
- region coverage with no gaps or duplicates;
- ordered gather preservation, including adversarial ordering;
- direct/sharded logical-demand equivalence;
- shard-coordinate and index-entry mapping;
- shard index encode/decode, fill sentinel, checksum, truncation, overlap, and
  out-of-bounds entries;
- deterministic metadata rendering;
- resource-limit rejection before allocation.

### JVM and Scala.js store and codec tests

- exact JVM filesystem range reads and root confinement;
- JVM and Scala.js HTTP `206`/`Content-Range` validation;
- servers that ignore ranges, truncate bodies, or return inconsistent lengths;
- JVM and browser gzip inverse fixtures and decompression-bomb bounds;
- CRC32C corruption at index and inner-chunk levels;
- bounded concurrency and deterministic request ordering where promised;
- repeated JVM writer runs produce byte-identical Zarr objects;
- the JVM and Scala.js readers emit equivalent staged request traces.

### Independent cross-language oracles

1. Zarr-Python writes direct and start/end-indexed sharded arrays at multiple
   ranks for every 0.1 dtype, edge shape, fill case, and corruption case; Scala
   reads full arrays, regions, and ordered points wherever the declared
   capability applies.
2. Scala writes the same cases; Zarr-Python verifies metadata and values.
3. The tool-scoped zarr-java 0.1.3 oracle reads Scala fixtures and the shared
   Scala suite reads its SHA-pinned compatible fixture.
4. Nibabel verifies exported NIfTI stored values, calibrated values, affine,
   qform/sform, dimensions, and timing metadata.
5. JVM and Scala.js HTTP recorders prove a subset read consists of the required
   index prefix/suffix and demanded inner ranges, never an unrelated full shard
   request.

The critical performance assertion is observable:

```text
requested 30 MB logical selection
-> bounded number of shard index prefix reads
-> bounded inner-chunk ranges
-> no complete acquisition and no unrelated complete shard download
```

### Repository gates

Every portable slice passes both platforms. The browser reader is a real 0.1
feature, not merely a shared-code compile target. JVM-only filesystem/writer
adapters receive JVM tests; Fetch and browser codecs receive Scala.js tests:

```text
sbt zarrJVM/test zarrJS/test
sbt archiveZarrJVM/test archiveZarrJS/test
sbt datasetZarrJVM/test datasetZarrJS/test
sbt datasetJVM/test datasetJS/test
sbt imageJVM/test imageJS/test
sbt fitJVM/test fitJS/test
sbt testAll
```

The aliases are added with the modules. Warning-clean compilation is part of
each gate.

## Implementation sequence

### Z0 — kernel constitution and exact metadata

Deliver:

- this design accepted as the library/profile boundary;
- raw JSON/Zarr metadata model and exact-integer syntax boundary;
- group and arbitrary-rank array metadata validation;
- compilation against explicit dtype/grid/codec capabilities;
- positive/negative rank 0, 1, 2, 4, and 5 fixtures generated by Zarr-Python;
- explicit unsupported-feature and unsupported-capability errors.

Gate: Python metadata is classified exactly; unsupported Zarr is rejected for
the right typed reason; JVM and Scala.js tests pass.

### Z1 — runtime-rank shape, chunk, key, and demand algebra

Deliver:

- `Rank`, `Shape`, `Coordinate`, `Region`, `CoordinateBatch`, and checked
  arithmetic with value semantics;
- scalar arrays, zero-length dimensions, and rank-mismatch behavior;
- arbitrary-rank regular grids and default chunk keys;
- direct and sharded layout invariants;
- region and ordered-gather to inner-chunk planners;
- object/shard grouping and planner statistics;
- property/law suites over multiple ranks on JVM and Scala.js.

**Stop gate:** if the same planner cannot express a 2D region, a 5D region, and
arbitrary rank-four `ResponseBlockSource` ordering without rank-specific
branches, redesign before any IO implementation.

### Z2 — shard indexes and portable codecs

Deliver:

- arbitrary-rank shard-index encode/decode and start/end index plans;
- shared bytes/CRC32C plus JVM and Scala.js gzip adapters;
- exact decoded-length enforcement;
- Python-to-Scala direct and sharded fixtures;
- Scala-to-Python in-memory shard fixtures at multiple ranks.

Gate: every supported dtype/edge/fill/corruption fixture passes independently.

### Z3 — bounded JVM and Scala.js reading

Deliver:

- memory interpreters on both platforms;
- confined JVM filesystem and JDK HTTP readers;
- Scala.js Fetch/HTTP Range reader;
- two-phase shard execution and bounded range coalescing;
- request/execution receipts;
- generic `OpenedArray` region/point reads;
- equivalent JVM and Scala.js HTTP request-trace tests.

**Stop gate:** 2D, 5D, and realistic BOLD ROI/time selections must avoid full
unrelated shards and full arrays with measured amplification inside the
declared gate. If they do not, change chunks/shards or the plan before building
a writer.

### Z4 — deterministic generic local writer

Deliver:

- arbitrary-rank streaming `ChunkProvider`;
- deterministic direct and sharded create-only writer with local staging;
- ordinary Zarr missing-chunk/fill behavior;
- Scala-to-Python cross-read suite at multiple ranks;
- interrupted-write tests.

Gate: repeated writes are byte-identical for the pinned writer, incomplete
staging roots are not exposed as completed writes, and Python reads the
completed stores.

### N0 — NeuroArchive canonical-BOLD profile and publication

Deliver:

- `CanonicalBold` refinement over `ArrayDescriptor`;
- exact `[t,z,y,x]` axes, scalar calibration, geometry, and timing agreement;
- immutable revision publisher around the generic writer;
- `publication.json`, logical payload hash, and physical object receipts;
- strict missing-object and interrupted-publication tests;
- JVM and Scala.js opening of a published read-only fixture.

Gate: no rank-four assumptions exist in `zarr`; invalid scientific refinements
fail in `archive-zarr`; interrupted writes never open as published.

### N1 — faithful NIfTI/BIDS and dataset bridge

Deliver:

- streaming NIfTI raw-scalar import for the declared dtype set;
- geometry/timing/source-identity capture;
- faithful NIfTI/BIDS export and validator invocation;
- nibabel/neuroimaging oracle matrix;
- `ZarrResponseBlockSource` over the generic point planner;
- fit-program scenario using Zarr without whole-run materialization.

Gate: stored values, calibrated values, geometry, ordering, and BIDS export all
pass independent oracles.

### Z5 — measured compatibility and production capabilities

Only after Z0–Z4 and N0–N1:

- benchmark Blosc/Zstandard plus shuffle against portable gzip;
- select documented chunk/shard defaults from the workload corpus;
- add end-indexed execution wherever the store capability and request traces
  make it safe;
- promote additional core dtypes and indexing conveniences from real users;
- add a credentialed S3 interpreter under the JVM side of its cross-project
  adapter if deployment needs it;
- add operational diagnostics required by the repository runtime.

Masked `time x voxel`, pyramids, Parquet, catalogs, caches, mutation, and
virtual arrays remain parent-plan or future-library milestones, not hidden Z5
work.

Z5 outcome (2026-07-20):

- [`canonical-balanced-0.1`](../benchmarks/zarr-z5.md) was promoted in the
  NeuroArchive profile with inner chunks `[16,24,32,32]` and start-indexed
  shards `[64,72,96,96]`; it is not a generic Zarr default.
- Blosc/Zstandard with shuffle materially beat gzip on the deterministic BOLD
  corpus, but were not promoted because the JVM dependency and Scala.js
  capability gates are not both satisfied. The portable 0.1 chain remains
  little-endian bytes, gzip level 1, and CRC32C.
- generic start/end-indexed execution remains enabled; NeuroArchive remains
  start-indexed only.
- execution receipts now distinguish length requests, index bytes, data bytes,
  requested logical bytes, and actual read amplification.
- NIfTI import can publish directly with the measured sharded profile while
  retaining the existing explicit direct-layout path.
- credentialed S3 remains unimplemented because no deployment requires it;
  public and presigned objects use the checked HTTP range interpreter.
- the initial five dtypes remain the promoted set. No user workload justified
  broadening the scalar or indexing surface in 0.1.

## Scope verdicts

These decisions keep the implementation coherent:

| Temptation | Verdict | Reason |
| --- | --- | --- |
| Wrap zarr-java as the public backend | Reject | Imports a monolithic dependency graph, JVM-only model, mutable surface, and execution policy into the kernel |
| Reimplement every Zarr v3 feature | Reject | Solves an ecosystem problem rather than the declared fMRI problem |
| Own a rank-generic v3 parser/planner/shard writer | Keep | These mechanics are small enough to reuse and are the behavior the library must prove |
| Own compressor algorithms | Reject | Mature implementations already exist and can sit behind a tiny capability |
| Model arbitrary rank with match types | Reject | Dynamic metadata still needs runtime validation; runtime-rank values are simpler and honest |
| Hard-code rank four in the kernel | Reject | Saves little and blocks multi-echo, analytical matrices, surfaces, and ordinary Zarr use |
| Refine generic arrays into domain-typed axes | Keep | Prevents accidental time/spatial exchange without complicating wire parsing |
| Put the whole study in one Zarr root | Reject | Couples permissions, revisioning, publication, and object counts unnecessarily |
| Add mutation in 0.1 | Defer | It greatly expands store and concurrency obligations; NeuroArchive publication remains immutable |
| Require start-indexed shards in NeuroArchive | Keep | Enables one predictable prefix read and removes suffix/length branches from the profile |
| Erase end-indexed shards from the generic model | Reject | It is standard Zarr and can be executed later without redesigning metadata/plans |
| Make browser reading a later rewrite | Reject | Scala.js is a founding constraint and should exercise the same planner now |
| Add masked layout immediately | Defer | Canonical correctness and actual workload measurements come first |
| Implement S3 protocol/authentication | Reject | HTTP range semantics are the array concern; AWS policy belongs in an adapter |
| Implement a transparent cache | Reject | Cache lifecycle belongs to Eidolon/runtime; core emits stable identities and receipts |

## Definition of the first real success

The core is successful when this scenario is true and independently observed:

1. Zarr-Python writes direct and sharded 2D, 4D, and 5D arrays.
2. JVM and Scala.js read the same regions/points and issue equivalent bounded
   range requests.
3. Scala writes arbitrary-rank fixtures that Zarr-Python reads correctly.
4. Import a BIDS NIfTI BOLD run without converting its stored scalar dtype.
5. Refine its generic descriptor into canonical `[t,z,y,x]` BOLD.
6. Publish one immutable, start-indexed, sharded NeuroArchive Zarr revision.
7. Serve that store from an ordinary HTTP server and open it in JVM and
   Scala.js readers.
8. Ask `ResponseBlockSource` for non-contiguous timepoints and voxels.
9. Observe only metadata, shard-index prefixes, and the necessary inner ranges.
10. Receive calibrated values in exactly the requested order.
11. Run a fit chunk without materializing the selected whole run.
12. Export the revision to validator-clean BIDS/NIfTI.
13. Have Python/nibabel independently verify array values and geometry.
14. Corrupt or remove one demanded publication object and receive a typed
    integrity error.

That is enough to replace `.nii.gz` as the computational substrate while
retaining NIfTI/BIDS as the exchange and archival view. Everything beyond it
must earn its way into a later library capability or domain profile.

## External specifications and implementation evidence

- [Zarr format 3 core specification](https://zarr-specs.readthedocs.io/en/latest/v3/core/)
  defines the metadata, regular grid, dimension names, chunk-key encoding,
  codec pipeline, runtime-rank array model, and store model used by the kernel.
- [Indexed sharding codec](https://zarr-specs.readthedocs.io/en/latest/v3/codecs/sharding-indexed/)
  defines inner chunks, `uint64` offset/length indexes, fill sentinels, index
  checksums, and start/end index placement.
- [Zarr codec specifications](https://zarr-specs.readthedocs.io/en/latest/v3/codecs/)
  define the accepted bytes, gzip, CRC32C, Blosc, and sharding codecs.
- [Zarr data-type specifications](https://zarr-specs.readthedocs.io/en/latest/v3/data-types/)
  define the fixed numeric dtype names admitted by 0.1.
- [WHATWG Compression Standard](https://compression.spec.whatwg.org/)
  defines the browser `DecompressionStream` gzip capability used by the
  Scala.js adapter when present.
- [Blosc codec specification](https://zarr-specs.readthedocs.io/en/latest/v3/codecs/blosc/)
  defines the optional Zstandard/shuffle capability to benchmark after the
  portable path works.
- [zarr-developers/zarr-java](https://github.com/zarr-developers/zarr-java)
  is retained as an implementation oracle, not the ScalaFIM domain API.
- [zarr-java issue 79](https://github.com/zarr-developers/zarr-java/issues/79)
  and [pull request 80](https://github.com/zarr-developers/zarr-java/pull/80)
  document the released suffix-range edge case considered by the start-index
  decision.
- [Laserson Lab ndarray.scala Zarr code](https://github.com/lasersonlab/ndarray.scala/tree/master/zarr)
  is historical Scala design evidence, not a v3 backend candidate.
