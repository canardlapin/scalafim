# Response/Representation/Archive Phase 7 Record

Status: complete; repository-wide JVM/Scala.js gates passed  
Issue: `bd-01KYAA8P1ZX4C3N49WPBG9055J`  
Governing plan: [response-representation-archive-architecture.md](response-representation-archive-architecture.md)  
Baseline: [Phase 6 record](response-representation-archive-phase-6.md)

Phase 7 proves that representation semantics are independent of archive
format. One temporal-DCT `DecodePlan[ResponseBlock]` now runs unchanged through
memory, LNA/HDF5, and Zarr. It also adds the canonical dense-BOLD Zarr family
and makes bounded-read claims axis-specific and receipt-checked.

## 1. One logical decoder, three bindings

`ArchivedTemporalDctSource[F]` owns representation compilation and pure plan
evaluation. It depends on a narrow internal `TemporalDctArchiveBinding[F]`
that supplies:

- an interpreter for the existing typed `TemporalDctRead` requests;
- an opened physical layout;
- axis-keyed read capabilities;
- adaptation of native archive receipts to response receipts.

The LNA and Zarr bindings implement this contract without changing the
temporal-DCT model, request algebra, decode plan, or `ResponseBlock` result.
The Zarr binding uses checked float64 arrays for basis, loadings, and optional
offset values. Exact logical selections are lowered through the generic Zarr
`ChunkPlanner` and `ShardPlanner`; the representation layer never addresses a
chunk, shard, object key, or byte range.

The shared `TemporalDctCrossBackendSuite` compiles one selection once, inspects
the same decode program, and executes it through:

```text
owned in-memory values
LNA typed payload plans
portable AsyncZarr arrays
```

Agreement is checked with the representation's `DecodeConsistency`, not its
reconstruction-error contract.

## 2. Axis-keyed locality and receipt truth

`ReadPlanSummary` now carries `Vector[AxisLocality]`, matching
`ReadCapabilities`, instead of collapsing a read to one locality scalar.
This matters for a temporal-DCT Zarr layout whose:

- sharded basis is `ByteRangeBounded` for time selection;
- direct loadings and offsets are `ChunkBounded` for sample selection.

The Zarr binding groups native receipts by logical axis, preserves exact
objects and ranges, sums observed bytes and cache hits with overflow checks,
and emits no fallback when execution matches the declared plan. Invalid or
coarser physical behavior cannot be relabelled as the requested locality:
`ReadResult.make` checks every claim against the opened layout.

Receipt-bearing `ZarrPayloadPlan` execution remains serial
(`maxConcurrentRequests = 1`) so physical attempt order is deterministic.
Both chunk-bounded and byte-range-bounded constructors retain their declared
locality and exact covering evidence.

## 3. Canonical dense-BOLD family

`DenseBoldRevisionMetadata` projects a checked NeuroArchive Zarr canonical-BOLD
revision into the narrow archive representation envelope
`org.scalafim/dense-bold@1`. The archive-Zarr module remains independent of the
response kernel: it publishes canonical array metadata, calibration, time and
sample schema facts, payload identity, and immutable object inventory as
canonical archive values.

`DenseBoldZarrRepresentationFamily` admits only that exact envelope. Its source
then:

- reconstructs the neutral response schema and rejects descriptor drift;
- lowers ordered response selections to canonical `[t,z,y,x]` points;
- reads direct chunks or start-indexed shard ranges through the existing
  portable Zarr executor;
- applies primitive-scalar calibration at the response boundary;
- returns an owned row-major `Double` `ResponseBlock`;
- adapts native observations into conforming axis-keyed receipts.

Selected-versus-whole agreement is exact. No general tensor or
scalar-generic response abstraction was introduced.

## 4. Publication and focused executable evidence

The existing canonical writer remains create-only and transactional. The JVM
crash-injection law
`provider interruption and direct fill omission never publish a revision`
proves that a failed provider or incomplete direct fill leaves no published
target. Portable opening also classifies a revision without its publication
receipt as incomplete, and Scala.js rejects a publication whose immutable
inventory is missing an object.

The focused implementation state has passed:

```text
archive-zarr JVM               18 passed
archive-zarr JS                16 passed
interop archived response JVM  18 passed
interop archived response JS   13 passed
compileAll                      passed
testAll                         passed
Phase 0 numerical corpus        valid
git diff --check                clean
```

These suites run the same portable Zarr plans on both platforms, prove exact
request/range evidence, exercise mixed-axis locality, check dense laws, and
run the unchanged temporal-DCT decoder through all three backends.

Repository-wide `compileAll` and `testAll` verify the permanent
`interop-archived-response -> archive-zarr` edge and every aggregated JVM and
Scala.js target. The frozen Phase 0 corpus passed without regeneration.

Static production scans found no general tensor, old scalar-locality summary,
LNA-specific name on the backend-neutral temporal-DCT plan, unchecked cast,
warning suppression, or archive-Zarr import of the response kernel. The only
matched `null` text is a descriptor error message for the canonical null
value; no null reference was introduced.
