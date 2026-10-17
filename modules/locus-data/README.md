# locus-data

`locus-data` provides ScalaFIM-specific quotient operations
over finite domains from standalone
[`locus4s`](https://github.com/canardlapin/locus4s). It cross-compiles for the
JVM and Scala.js and depends on the locus4s core and data projects.

The module provides:

- `DomainFactory`, which restores a live, unforgeable domain owner from a
  persistent domain id and size, or creates an ephemeral derived domain;
- supported `Parcellation[X, P]` quotients with typed parcel points;
- `Searchlight[S]` center policy over a locus endorelation.

Callers use locus4s names directly (`FiniteDomain`, `Index`, `Region`,
`Selection`, `locus4s.data.VectorField`, `Section`); this module re-exports
none of them. `VectorField` is not the lazy execution/provenance type
`scalafim.spatial.Field`; storage, chunking, caches, interpolation, and IO stay
in their existing modules.

There is no process-wide domain registry. Every `DomainFactory.restore` mints an
independent owner. Two restores of the same key share persistent identity but
not a runtime owner, so code that combines them checks `left.align(right)` and
transports regions, selections and maps through the resulting
`DomainAlignment`. A composition that genuinely needs one shared live owner
threads its own `locus4s.DomainRegistry` value. Derived supports, such as mask
active sets, are ephemeral, and size or display name is never identity.

A parcellation stores one optional parcel point per ambient point. Background
is visible only as `None`, and construction rejects unused parcel points.
Names, atlas ids, colors, network terms, and display order are separate fields
indexed by the parcel space. Partition equality up to relabeling is distinct
from label-field equality.

Aggregation is owned by `locus4s.data.Aggregation`; the unused ScalaFIM loop
has been removed. For an existing ScalaFIM parcellation, construct a provider
`PartialMap.fromOptionalTargetOrdinals(parcellation.ambient,
parcellation.parcels, parcellation.assignmentOrdinals)` and call provider
`Aggregation.foldMapByChecked(mapping, field)(empty)(contribution)(combine)`.
The checked entrypoint rejects foreign runtime owners and accepts the provider's
representation-neutral `Field`. It skips background and traverses increasing
source order, including when no targets exist.

Provider aggregation scans the supported ambient points once. Means should accumulate
a mergeable `(sum, count)`-like state and divide only at presentation time.
The exact hierarchy-fusion law applies to lawful commutative monoids; ordinary
IEEE floating-point addition is not claimed to be exactly associative.

## Minimal example

Each restored domain has an abstract owner type. Callers cannot choose or
forge that type, while the persistent id supports serialization and checked
alignment:

```scala
import locus4s.{FiniteSpace, Region, Selection}
import locus4s.data.VectorField
import scalafim.locus.{DomainFactory, Parcellation}

val voxelDomain =
  DomainFactory.restore("sub-01:native:bold", 6).toOption.get
type NativeVoxels = voxelDomain.S
val voxels: FiniteSpace[NativeVoxels] = voxelDomain.space

val parcelDomain =
  DomainFactory.restore("atlas:demo:parcels", 2).toOption.get
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
val section = field.restrictChecked(left).toOption.get
val orderedValues = section.gather(requestedOrder).toOption.get.toVector
```

`orderedValues` is `Vector(12, 10, 11)`: ordering comes from
`Selection`, never from the set semantics of `Region`.
