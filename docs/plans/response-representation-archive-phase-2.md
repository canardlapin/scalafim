# Response/Representation/Archive Phase 2 Record

Status: complete and focused JVM/Scala.js gates verified on 2026-07-26  
Issue: `bd-01KYAA8MDBRHYQW74FDVHVFA3Z`  
Governing plan: [response-representation-archive-architecture.md](response-representation-archive-architecture.md)  
Baseline: [Phase 0 record](response-representation-archive-phase-0.md)

Phase 2 separates immutable archive identity from live resources and physical
execution. It is additive over the existing LNA and NeuroArchive Zarr APIs:
current callers retain their old entry points, while new code can inspect a
pure revision and execute a typed payload plan through a scoped opened
archive.

## 1. Pure revision envelope

`ArchiveRevision` contains only an id, logical manifest, publication state,
and provenance. It cannot own an HDF5 handle, object reader, HTTP session,
cache, or finalizer. Publication is one of `Staging`, `Published(rootDigest)`,
or `Incomplete(reason)`.

The new logical envelope includes:

- checked object-type, representation, payload, role, scalar-type, and
  revision identities;
- payload descriptors with positive runtime-rank shapes;
- a closed `CanonicalValue` algebra for null, Boolean, string, `Int64`, raw
  `Float64` bits, arrays, and canonically ordered objects;
- duplicate-key rejection and UTF-8 byte ordering for canonical objects;
- integrity entries that may reference only declared payloads.

Raw floating bits preserve negative zero, infinities, and NaN payloads.
Unknown representation keys remain ordinary inspectable manifest data.
`requireRepresentation` reports `UnsupportedRepresentation` only during
logical resolution. That result is independent of an incomplete publication
state, which has its own value and error case.

## 2. Resource and error boundary

`ArchiveDriver[F]` returns an `ArchiveResource[F, OpenArchive[F]]`.
`ArchiveResource` is a Cats Effect `Resource` whose effect is
`EitherT[F, ArchiveError, *]`; acquisition and use therefore retain a typed
archive error channel without sacrificing finalization. `OpenArchive[F]`
exposes:

- its pure `revision`;
- the successful structural validation performed during open;
- `PayloadExecutor[F]`;
- an independent `validateContents` action.

Shared tests prove release after success, typed failure, and fiber
cancellation on both the JVM and Scala.js. The Zarr adapter repeats that law
against a real store-provider resource rather than only a synthetic core
fixture.

## 3. Typed payload execution

`PayloadPlan[A]` retains its result type through
`PayloadExecutor.execute[A]`. The eager LNA adapter uses three fixed-result
plans:

- `LnaDoubleMatrixPlan`;
- `LnaDoubleVectorPlan`;
- `LnaIntMatrixPlan`.

No cast, `Any`, scalar type parameter, or general tensor carrier participates
in dispatch. A plan for an integer matrix produces a statically typed
`Payload.IntMatrix`. Wrong physical variants return `ArchiveError`, and a plan
owned by another driver returns `UnsupportedPayloadPlan`.

The HDF5 driver loads and closes the current jHDF store during resource
acquisition. Subsequent typed execution is intentionally reported as
`WholePayload`, with logical payload bytes calculated from the declared scalar
width and shape. It does not fabricate chunk or byte-range locality.

## 4. Ordered receipt evidence

Phase 2 produces an archive-native receipt; the subsequent
[Track-A integration gate](response-representation-archive-track-a.md)
preserves that receipt beside a response-facing logical summary. A
`PayloadPlanSummary` declares an axis-keyed locality claim, while
`ReceiptAccumulator` appends immutable physical observations in execution
order. Combining or appending the same fragment twice counts it twice;
aggregation is never implicitly idempotent.

Physical observations distinguish:

- resident payloads;
- whole logical payloads;
- whole physical objects;
- exact object byte ranges;
- object-length observations.

Physical byte totals are derived from successful observations with checked
overflow. Conformance is defined against the opened claim for the same axis
set; it never assigns an ordinal strength to `WholePayload`, `WholeObject`,
chunk cover, or byte ranges. Mixed time/sample claims are executable law
fixtures.

## 5. Zarr adapter

`ZarrArchiveDriver[F]` uses the existing portable asynchronous
`AsyncNeuroArchiveZarr` path. The caller supplies a resource-backed
`AsyncObjectReader` and an explicit platform codec runtime. JVM tests use the
JVM asynchronous gzip adapter; Scala.js tests use the browser gzip runtime.

The adapter instruments the actual object-reader boundary. It reserves an
observation slot when a physical request begins and fills that same slot when
the future completes, so completion timing cannot reorder evidence. For the
frozen start-indexed shard fixture, a full mixed-axis read records exactly:

```text
canonical/c/0/0/0/0  range 0   length 132  bytes 132
canonical/c/0/0/0/0  range 132 length 248  bytes 248
```

The archive receipt and the generic Zarr receipt both report 380 physical
bytes. Content validation reads every publication-listed object and verifies
its stored SHA-256 digest. It does not mark the logical payload digest as
verified: the current profile stores that digest as manifest identity, while
object validation establishes only the publication-listed physical hashes.

Detailed attempt order is currently guaranteed by requiring
`maxConcurrentRequests = 1` in receipt-bearing `ZarrPayloadPlan` values. A
caller asking for parallel requests receives a checked planning error. This
constraint is deliberate: relaxing it requires plan-indexed deterministic
attempt ordering that preserves retries, not post-hoc sorting or plan-derived
fiction.

## 6. Executable evidence

The focused source state passed:

```text
archiveJVM/test       53 passed
archiveJS/test        35 passed
archiveZarrJVM/test   17 passed
archiveZarrJS/test    15 passed
```

The law suites cover canonical raw-bit semantics, unknown representations,
publication/error separation, immutable receipt ordering, mixed-axis
conformance, typed LNA execution, exact Zarr request and byte evidence,
structural versus content validation, deterministic Zarr planning, and
resource release on success, failure, and cancellation.

The focused gates above ran again after the final semantic audit. The
repository-wide `compileAll testAll` alias and the frozen Phase 0
compatibility-corpus verifier also passed on the closing source state; the
exact commands and results are recorded on the issue.

## 7. Track A integration

`ArchiveResponseReceiptAdapter` now performs the final common adaptation in
`dataset`, which already depends on both `archive` and `response`. It accepts
either one combined-axis receipt or separate time and sample receipts,
validates identity conversion, sums physical bytes and cache hits with checked
overflow, constructs the response opened-layout evidence, and runs response
receipt conformance before returning.

For `ChunkBounded`, the archive plan's physical object cover becomes the
response summary's chunk-cover identity. This is a summary, not a replacement:
the returned `AdaptedArchiveReceipt` retains the native receipt with its exact
ordered object/range attempts. Non-resident archive receipts must now contain
at least one physical observation, closing the empty-evidence hole found
during integration.
