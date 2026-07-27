# Response/Representation/Archive Phase 4 Record

Status: complete; repository-wide JVM/Scala.js gates verified on 2026-07-26  
Issue: `bd-01KYAA8N28KT4STPR5XXSA1KYR`  
Governing plan: [response-representation-archive-architecture.md](response-representation-archive-architecture.md)  
Baseline: [Phase 3 record](response-representation-archive-phase-3.md)

Phase 4 binds the typed temporal-DCT representation to LNA-HDF5 without
putting archive concepts into the mathematical implementation. The binding is
owned by the new `interop-archived-response` cross-project.

## 1. Typed lowering

The pure LNA payload plans now live in archive shared code:

- `LnaDoubleMatrixPlan`;
- `LnaDoubleVectorPlan`;
- `LnaIntMatrixPlan`.

The JVM driver remains their physical interpreter. Moving the plan values to
shared code permits Scala.js to compile and inspect the same logical binding
without pretending that browser HDF5 execution exists.

`ArchivedTemporalDctSource[F]` interprets the Phase 3 GADT directly:

```text
BasisRows       -> LnaDoubleMatrixPlan -> TemporalBasisValues
LoadingRows     -> LnaDoubleMatrixPlan -> SpatialLoadingValues
OffsetEntries   -> LnaDoubleVectorPlan -> SampleOffsetValues
```

The eager LNA driver must read each physical payload whole. Selected logical
rows are applied only after that read. Each native receipt therefore reports
`WholePayload`; the response receipt aggregates the exact touched payloads,
physical bytes, cache hits, and content-validation evidence without claiming
bounded IO.

The three reads execute through the same applicative `DecodePlan` as the
in-memory interpreter. Their native receipts accumulate in plan order and are
retained beside the common response `ReadResult`.

## 2. Stored representation model

The LNA descriptor retains the existing typed DCT parameters and payload
roles. The interop profile adds a deterministic length-framed model
fingerprint covering:

- representation instance and response-schema identities;
- exact time-domain coordinates and units;
- neutral sample-domain identity, kind, references, and ordering;
- signal semantics;
- DCT shape, normalization, centering, and ridge raw bits;
- reconstruction contract;
- decode-path consistency.

Opening as a response requires a published
`org.scalafim/temporal-dct@1` revision, the exact expected fingerprint, float64
payload shapes, and the declared embedded-dependency mode. Legacy temporal-DCT
archives remain structurally inspectable but do not silently acquire a model
they never stored.

The profile has no external basis dependency: temporal basis, spatial
loadings, and optional sample offsets are embedded and content-verified by
the LNA root checksum.

## 3. Publication plan

`TemporalDctLnaWritePlan` exposes ordered steps:

```text
BeginStaging
WritePayload(...)
WriteManifest
Publish
```

The current jHDF backend already writes a temporary file, closes it, and
performs one atomic move. The interop executor preserves that contract:
payload and manifest preparation remain invisible, and the physical store is
invoked only for `Publish`. Immutable destinations are create-only at the
interop boundary.

The crash-injection fixture stops after every proper prefix. Every case returns
an `Absent` visibility receipt and proves that the destination path does not
exist. A successful execution reopens the archive, requires its SHA-256 root
checksum, and returns `Published(digest)`.

## 4. Executable evidence

The completed source state passed:

```text
interop-archived-response JVM   4 passed
interop-archived-response JS    1 passed
archive JVM                    53 passed
archive JS                     35 passed
compileAll testAll              passed
frozen Phase 0 corpus           valid
git diff --check                clean
```

The shared law verifies write-plan order, embedded dependency declaration,
model fingerprint presence, and raw logical payload bits. JVM fixtures prove
real HDF5 publication/opening, selected in-memory-versus-LNA exact-bit
agreement, three honest native whole-payload receipts, raw-bit persistence
including negative zero, and every proper interrupted write prefix.

The frozen corpus was checked without regenerating live-source expectations.
Static production scans also found no general tensor abstraction, unchecked
casts, untyped result maps, warning suppressions, partial `.get`, or `null` in
the Phase 4 implementation. The temporal-DCT mathematical implementation has
no archive, LNA, or HDF5 dependency.
