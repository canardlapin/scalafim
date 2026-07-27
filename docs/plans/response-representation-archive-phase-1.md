# Response/Representation/Archive Phase 1 Record

Status: complete and repository-wide verified on 2026-07-26  
Issue: `bd-01KYAA8M2XMHHBPASHKK1871E9`  
Governing plan: [response-representation-archive-architecture.md](response-representation-archive-architecture.md)  
Baseline: [Phase 0 record](response-representation-archive-phase-0.md)

Phase 1 establishes a storage-neutral response kernel and a typed bridge from
the dataset API. It deliberately does not move archive or
latent behavior. The public response value is one specialized, owned,
row-major `Double` block; there is no general tensor abstraction.

## 1. Module boundary

The new cross-project `response` module has no internal ScalaFIM dependency.
Its shared API uses Cats Core, Cats Effect, and `NArray`; it compiles for the
JVM and Scala.js. `dataset` depends on `response` only to host dataset
adapters. No dependency points from `response` back into dataset, image,
surface, graph, archive, latent, or a concrete storage module.

The response kernel owns:

- opaque, axis-indexed `AxisIndex[A]` and `DomainId[A]` values;
- primitive-backed `OrderedIndices[A]`, constructed only by checked companion
  factories;
- regular or explicit time domains and neutral volume/surface content
  references;
- response-local ordered selections;
- `ResponseBlock`, whose private primitive `Double` carrier is always owned;
- `ResponseSource[F]`, its path-dependent plan, and the existentially packed
  `PlannedRead[F]`;
- typed provenance, logical read summaries, physical evidence, and integrity
  evidence;
- axis-keyed locality capabilities and receipt conformance against an opened
  payload/object/chunk layout;
- `DecodeConsistency`, which is distinct from scientific reconstruction
  accuracy.

## 2. Ownership and selection rules

`ResponseBlock` is not a case class. Public construction copies its input, and
the only public bulk accessor returns another copy. Package-internal adoption
is available for kernels that allocate a fresh row-major buffer and transfer
exclusive ownership. Matrix adapters must copy whenever their source retains
or exposes the buffer.

Selections preserve requested order. The initial public duplicate policy is
rejection, matching the current dataset API; the factory can represent an
explicit duplicate-allowing gather for future internal planners. Both the
domain identity and full-domain cardinality are checked before a selection is
accepted. A same-sized foreign domain fails by identity, while drift under the
same identity fails with an axis-specific cardinality error.

Regular time construction verifies the origin, positive interval, count, and
finite final coordinate. Explicit time coordinates preserve raw IEEE-754 bits
and must be finite and strictly increasing. Sample-domain references carry
identity and ordering evidence only. Concrete affine, mask, mesh, and topology
validation remains in image/surface-aware adapters. Time-domain and response
schema equality include units and raw-bit-preserving coordinates; bounded
coordinate access reports a typed index error.

## 3. Dataset adapters

`DatasetResponseSource[F]` wraps an `FmriDataset`,
`DatasetBackend`, or `ResponseBlockSource` without changing those synchronous
contracts. It maps response sample ordinals through the dataset's full voxel
ordering, retains requested order, rejects duplicates, and copies the exposed
`DMat` buffer into an owned `ResponseBlock`. Attachment checks the complete
neutral volume-space, mask, and ordering references, not cardinality alone.

The synchronous dataset API cannot report actual byte ranges or chunk touches.
Its adapter therefore declares coarse `WholePayload` locality and records
physical-byte evidence as explicitly unavailable; it does not fabricate
bounded-I/O claims.
NIfTI parity is exercised through the existing JVM source test. Archive and
latent dispatch remain untouched for later phases.

## 4. Executable evidence

The shared response law suite covers:

- requested order, shape, ownership, duplicate policy, wrong-domain rejection,
  and cardinality drift;
- whole versus partitioned dense reads;
- heterogeneous path-dependent planned reads;
- exact-bit, ULP-bounded, and absolute/relative decode consistency;
- mixed time/sample locality conformance, including touched-versus-cover
  validation for payloads, objects, chunks, and byte ranges;
- provenance DAG validation;
- time-grid overflow and neutral sample references.

Platform-specific suites prove that ordered index storage remains an
`Array[Int]` on the JVM and an `Int32Array` on Scala.js, while response values
remain an `Array[Double]` and `Float64Array`, respectively. An external-package
compile check proves that the owned carrier and adoption constructor are not
public.

The dataset suites cover schema adaptation, selected-read parity, explicit
coarse receipt evidence, and the real JVM NIfTI path. The Phase 0 regression
corpus remains the numerical stop gate and was revalidated without
regenerating its goldens.

## 5. Verification

The final source state passed:

```text
responseJVM/test     16 passed
responseJS/test      15 passed
datasetJVM/test      87 passed
datasetJS/test       60 passed
compileAll           passed
testAll              passed end to end
Phase 0 corpus       receipt and JVM/Scala.js digests valid
```

The test launcher used Java 22, isolated temporary sbt/Coursier caches, and
headless AWT for the repository's Java2D suites. The response main sources have
no `scalafim.*` import, warning suppression, cast, blocking facade, or general
tensor API. A final Scala type-discipline review found no remaining weakened
type, sentinel state, partial production `.get`, or untyped plan composition.

## 6. Track A integration

Phase 1's response-facing receipt vocabulary is now connected to Phase 2's
archive-native evidence by the
[Track A integration record](response-representation-archive-track-a.md).
The adapter is deliberately outside `response`: `response` remains
internal-dependency-free, while `dataset` translates identities, axis claims,
layout cover, physical byte totals, and ordered evidence at its existing
interop boundary.

The common response summary retains exact logical selection order and checked
byte totals. The accompanying native receipt remains available for retries,
individual Zarr byte ranges, object-length probes, and other physical details
that the common locality summary intentionally does not duplicate.
