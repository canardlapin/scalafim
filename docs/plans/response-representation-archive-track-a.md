# Response/Representation/Archive Track A Integration Record

Status: implemented; JVM/Scala.js integration laws verified on 2026-07-26  
Issue: `bd-01KYAA8JE3V2BJBGABQ580FJ2Q`  
Slices: RRA 0, RRA 1, and RRA 2  
Governing plan: [response-representation-archive-architecture.md](response-representation-archive-architecture.md)

Track A closes the contract-freeze, response-kernel, and archive-resource
tranche. Its final responsibility is not another principal-domain API. It is
the explicit translation between the response-facing logical receipt and the
archive-native physical receipt.

## 1. Interop ownership

`ArchiveResponseReceiptAdapter` lives in `dataset`. That is the current lowest
shared module that already depends on both `response` and `archive`.
Consequently:

- `response` remains independent of image, surface, graph, archive, latent,
  and dataset;
- `archive` remains unaware of response schemas and output selections;
- the adapter receives a resolved response selection and one or more completed
  archive receipts;
- the result retains both the common response evidence and the ordered native
  evidence.

No general tensor, response scalar parameter, mutable response buffer,
runtime cast, or untyped payload dispatch was introduced.

## 2. Exact adaptation contract

The adapter returns `AdaptedArchiveReceipt`:

```text
response selection
+ ordered native archive receipts
        |
        +-- checked axis coverage
        +-- checked identity conversion
        +-- checked byte/cache aggregation
        +-- response capability and opened-layout construction
        +-- response receipt conformance
        |
        v
common ReadReceipt + capabilities + layout + unchanged native receipts
```

Axis coverage is exact. A time-only or sample-only logical read requires the
matching archive axis. A mixed logical read accepts either one combined
time-and-sample receipt or exactly one time receipt plus one sample receipt.
Duplicate or unrelated axis claims fail before a common receipt is returned.

Archive identity values pass through the checked response identity
constructors. Physical bytes and cache hits come from completed native
receipts and use overflow-checked addition. Logical bytes derive from the
resolved output shape, not from physical transfer.

## 3. Locality mapping

The mapping is structural rather than ordinal:

| Archive claim | Response summary evidence |
|---|---|
| `Resident` | no physical units, zero physical bytes |
| `WholePayload` | touched and covering payload identity |
| `WholeObject` | touched and covering object identities |
| `ChunkBounded` | archive object-cover identities represented as payload-scoped chunk covers |
| `ByteRangeBounded` | exact object and byte-range covers |

The common `ChunkBounded` summary does not pretend that a shard-range attempt
is a newly observed logical chunk. The native receipt remains beside the
summary and retains every ordered whole-object, object-length, and byte-range
observation, including repeats. Zarr's own execution receipt also remains
authoritative for Zarr mechanics.

The integration audit found one native-law gap: a non-resident archive receipt
could previously contain no observations and still conform vacuously.
Archive conformance now rejects empty evidence for `WholePayload`,
`WholeObject`, `ChunkBounded`, and `ByteRangeBounded`.

## 4. Tensor decision

Track A confirms that a general tensor abstraction is neither required nor
desirable for the response contract:

- public response values are always calibrated `Double`;
- `ResponseBlock` directly owns primitive row-major `Double` storage;
- representation-specific scalar diversity remains in typed archive payload
  plans;
- the existing image matrix is an adapter target, not the response kernel's
  storage abstraction.

This keeps hot response kernels primitive and cross-platform without imposing
rank, scalar, or ownership genericity on every downstream signature.

## 5. Executable evidence

The new shared integration suite runs unchanged on JVM and Scala.js. It covers
all five locality forms, exact output selection order and logical bytes,
repeated physical attempts, combined versus split axis claims, native receipt
preservation, and wrong-axis rejection. The archive suite separately proves
that non-resident empty evidence is rejected.

Focused closing evidence:

```text
ArchiveResponseReceiptAdapterSuite JVM   3 passed
ArchiveResponseReceiptAdapterSuite JS    3 passed
ArchiveResourceSuite JVM                 4 passed
ArchiveResourceSuite JS                  4 passed
```

The final Track A closure also requires the complete dataset/archive suites,
the repository-wide `compileAll testAll` aliases, the frozen Phase 0
numerical regression corpus, import-boundary inspection, type-discipline inspection,
and `git diff --check`. Those exact results are lodged on the issue.
