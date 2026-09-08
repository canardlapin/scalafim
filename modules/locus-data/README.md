# locus-data

`locus-data` provides ScalaFIM-specific quotient and aggregation operations
over finite domains from standalone
[`locus4s`](https://github.com/canardlapin/locus4s). It cross-compiles for the
JVM and Scala.js and depends on the locus4s core and data projects plus Cats
Kernel.

The module provides:

- `DomainFactory`, which restores a live, unforgeable domain owner from a
  persistent domain id and size;
- locus4s `Field[S, A]`, `VectorField[S, A]`, and restricted `Section` views;
- supported `Parcellation[X, P]` quotients with typed parcel points;
- compact `locus4s.NeighborhoodSystem[C, S]` values consumed directly;
- one-pass `foldMapBy` aggregation using
  `cats.kernel.CommutativeMonoid`.

`IndexedField` is not the lazy execution/provenance type
`scalafim.spatial.Field`; storage, chunking, caches, interpolation, and IO stay
in their existing modules.

A parcellation stores one optional parcel point per ambient point. Background
is visible only as `None`, and construction rejects unused parcel points.
Names, atlas ids, colors, network terms, and display order are separate fields
indexed by the parcel space. Partition equality up to relabeling is distinct
from label-field equality.

Aggregation scans the supported ambient points once. Means should accumulate
a mergeable `(sum, count)`-like state and divide only at presentation time.
The exact hierarchy-fusion law applies to lawful commutative monoids; ordinary
IEEE floating-point addition is not claimed to be exactly associative.

## Minimal example

Each restored domain has an abstract owner type. Callers cannot choose or
forge that type, while the persistent id supports serialization and semantic
comparison:

```scala
import scalafim.locus.*

val voxelDomain =
  DomainFactory.restore(SpaceKey.unsafe("sub-01:native:bold"), 6).toOption.get
type NativeVoxels = voxelDomain.S
val voxels: FiniteSpace[NativeVoxels] = voxelDomain.space

val parcelDomain =
  DomainFactory.restore(SpaceKey.unsafe("atlas:demo:parcels"), 2).toOption.get
type Parcels = parcelDomain.S
val parcelAxis: FiniteSpace[Parcels] = parcelDomain.space

val left =
  Region.fromOrdinals(voxels, Vector(0, 1, 2)).toOption.get
val requestedOrder =
  Selection.fromOrdinals(voxels, Vector(2, 0, 1)).toOption.get

val parcels =
  Parcellation
    .fromAssignments(
      voxels,
      parcelAxis,
      Vector(Some(0), Some(0), Some(0), Some(1), Some(1), None)
    )
    .toOption.get

val field =
  VectorField.fromValues(voxels, Vector(10, 11, 12, 20, 21, 0)).toOption.get
val section = field.restrict(left)
val orderedValues = section.gather(requestedOrder).toOption.get.toVector
```

`orderedValues` is `Vector(12, 10, 11)`: ordering comes from
`Selection`, never from the set semantics of `Region`.
