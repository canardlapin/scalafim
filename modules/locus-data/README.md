# locus-data

`locus-data` adds data and quotient constructions over the dependency-free
finite spaces in `locus-kernel`. It cross-compiles for the JVM and Scala.js and
depends only on `locus-kernel` plus Cats Kernel.

The module owns:

- pure random-access `IndexedField[S, A]` values and restricted `Section`
  views;
- supported `Parcellation[X, P]` quotients with typed parcel points;
- `Searchlight[S]` center policy over a locus endorelation;
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

The phantom type identifies one semantic domain, while the runtime key protects
existential and deserialized boundaries:

```scala
import scalafim.locus.*

sealed trait NativeVoxels
sealed trait Parcels

val voxels =
  FiniteSpace.make[NativeVoxels](SpaceKey.unsafe("sub-01:native:bold"), 6)
    .toOption.get
val parcelAxis =
  FiniteSpace.make[Parcels](SpaceKey.unsafe("atlas:demo:parcels"), 2)
    .toOption.get

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
  IndexedField.fromValues(voxels, Vector(10, 11, 12, 20, 21, 0)).toOption.get
val section = field.restrict(left).toOption.get
val orderedValues = section.valuesIn(requestedOrder).toOption.get.toVector
```

`orderedValues` is `Vector(12, 10, 11)`: ordering comes from
`Selection`, never from the set semantics of `Region`.
