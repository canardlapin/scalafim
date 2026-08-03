# Response/Representation/Archive Phase 5 Record

Status: complete; repository-wide JVM/Scala.js gates verified on 2026-07-26  
Issue: `bd-01KYAA8NCV89JFF3ZAX6NFQ1N0`  
Governing plan: [response-representation-archive-architecture.md](response-representation-archive-architecture.md)  
Baseline: [Phase 4 record](response-representation-archive-phase-4.md)

Phase 5 makes logical representation dispatch and physical archive dispatch
explicit application policy. It also delivers the first complete
`ScalafimRuntime.openResponse` path.

## 1. Namespaced identity and immutable registries

`RepresentationKey` now accepts only:

```text
namespace/name@positive-major
```

It exposes the namespace, name, and major version as checked components.
`ArchivedResponseRegistry[F]` and `ArchiveDrivers[F]`:

- copy and sort installed entries at construction;
- reject duplicate representation keys or driver identifiers
  deterministically;
- resolve independently of registration order;
- remain immutable ordinary dependencies;
- perform no global mutation or file-directed code discovery.

A runtime test installs exactly one response family. An unknown, otherwise
valid representation returns `UnsupportedRepresentation` with the sorted
installed keys. It does not invoke an installed family.

## 2. Narrow persisted envelope

Representation descriptor parsing receives only:

```text
RepresentationEnvelope(
  key,
  descriptor,
  payloads,
  outputSchema
)
```

Families do not receive publication policy, provenance, unrelated container
metadata, or the entire archive manifest. After lookup, execution receives a
capability-limited `ArchiveResponseAccess[F]`: location, revision identity,
published root digest, typed payload executor, and content-validation effect.
Missing descriptor values are represented as canonical `Null`, allowing valid
unknown and generic LNA archives to remain structurally inspectable. An installed
family decides whether its descriptor is valid.

The temporal-DCT LNA profile now stores two deterministic, length-framed
records:

- a response schema preserving domain identities, regular or explicit time
  coordinates by raw bits, volume or surface references, sample ordering,
  units, calibration, and non-finite policy;
- a representation descriptor preserving instance identity, DCT parameters,
  ridge raw bits, reconstruction contract, and decode-path consistency.

`TemporalDctLnaRepresentationFamily` reconstructs the checked model from those
records and then uses the Phase 4 typed binding. The stored Phase 4 fingerprint
remains an independent exact-model check.

## 3. Runtime assembly and the LNA pipeline

`ScalafimRuntime.openResponse` executes this order:

```text
installed driver selection
-> resource-safe physical open
-> structural coverage validation
-> publication validation
-> narrow envelope projection
-> installed-family lookup
-> family descriptor validation
-> archive binding
-> response-source construction
```

Staging or incomplete publication fails before family lookup. Structural
corruption remains distinct from an unsupported representation. The archive
resource encloses the response-family resource, so release order is response
then archive.

`LnaPipelineRepresentationFamily` owns the generic
`org.scalafim/lna-pipeline@2` representation. It requires an explicit platform
opener; the JVM HDF5 opener requires one LNA run and delegates its eager read
to `LnaPipeline`. It does not become the canonical temporal-DCT binding or
expose raw LNA state through the generic family contract.

## 4. Focused executable evidence

```text
archive JVM                    53 passed
archive JS                     35 passed
interop-archived-response JVM  13 passed
interop-archived-response JS    8 passed
compileAll testAll              passed
frozen Phase 0 corpus           valid
git diff --check                clean
```

The focused laws cover key parsing, duplicate rejection, insertion-order
independence, one-family assembly, unsupported-versus-corrupt classification,
publication-before-lookup ordering, release on success, typed failure,
family-acquisition failure, and cancellation. JVM integration fixtures open a
typed temporal-DCT archive without a caller-supplied model and open a generic
LNA archive through `LnaPipelineRepresentationFamily`.

The frozen corpus was checked without regenerating live-source expectations.
Static production scans found no general tensor abstraction, unchecked cast,
untyped `Any` carrier, warning suppression, or `null`. Representation-family
production code has no `OpenArchive`, `ArchiveRevision`, or `ArchiveManifest`
access; only typed revision identity is present in the capability-limited
handle.
