# locus-kernel

`locus-kernel` is ScalaFIM's dependency-free calculus of finite indexed
spaces. It is cross-compiled for the JVM and Scala.js.

The module owns:

- `SpaceKey`, `FiniteSpace`, existential `SomeFiniteSpace`, and typed `Point`;
- unordered extensional `Region` values;
- ordered, duplicate-free `Selection` values;
- exact `TotalMap` values and validated injection, surjection, and bijection
  evidence;
- sparse Boolean `Relation` values and validated reflexive and symmetric
  evidence.

## Identity

A `SpaceKey` identifies the ordered semantic elements of a space. It is not a
shape or geometry fingerprint. Two spaces are compatible only when both their
keys and sizes agree. Phantom types reject ordinary cross-space composition at
compile time; the runtime identity check remains necessary for existential,
deserialized, or accidentally reused phantom types.

Points carry local ordinals. Public constructors obtain or validate them
against a `FiniteSpace`; points are not implicitly created from `Int`.

## Ordering

`Region[S]` is an unordered subset of `FiniteSpace[S]`. Its equality depends
only on runtime space identity and membership. Its domain-order iterator is an
operational convenience and is not semantic ordering.

`Selection[S]` explicitly stores an observable order and rejects duplicate
points. Use it for extraction rows, matrix axes, display order, and
serialization order.

## Exact maps

`TotalMap[X, Y]` denotes an exact, total point map. Pullback, existential
image, universal image, and image-supported universal image implement the
indexed Boolean logic of finite spaces. Interpolation, partial transforms,
multi-candidate correspondences, and probabilistic membership are not total
maps.

Public array constructors and exporters copy their buffers. The initial
private region representation is a sorted primitive array, selected after the
shared benchmark fixture compared sparse and dense membership/Boolean
operations against immutable `BitSet` on both supported platforms. The
representation is intentionally not part of the API.

The JVM suite additionally records current-thread allocation for warmed region
union and intersection loops. These numbers are diagnostic receipts, not
machine-independent thresholds:

```sh
sbt locusKernelJVM/test
sbt locusKernelJS/test
```

## Relations

`Relation[X, Y]` stores a finite set of target points for each source point.
It supplies identity, composition, converse, union, relational image, and
all-related-inside erosion. Identity relations retain their diagonal loops.
Evidence such as reflexivity or symmetry is produced only by validating the
relation.

The `graph` module depends on this kernel and adapts its keyed `VertexBasis`
to a locus space. Graph conversion keeps loop handling explicit because
ScalaFIM graphs remain simple and loopless.
