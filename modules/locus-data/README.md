# locus-data

`locus-data` provides ScalaFIM-specific domain and aggregation adapters
over finite domains from standalone
[`locus4s`](https://github.com/canardlapin/locus4s). It cross-compiles for the
JVM and Scala.js and depends on the locus4s core and data projects plus Cats
Kernel.

The module provides:

- `DomainFactory`, which restores a live, unforgeable domain owner from a
  persistent domain id and size through an explicitly owned immutable
  `DomainRegistry`;
- compatibility aliases for locus4s `IndexedField[S, A]` values and restricted
  `Section` views;
- `Searchlight[S]` center policy over a locus endorelation;
- a `cats.kernel.CommutativeMonoid` adapter for locus4s one-pass
  `PartialSurjection` aggregation.

`IndexedField` is not the lazy execution/provenance type
`scalafim.spatial.Field`; storage, chunking, caches, interpolation, and IO stay
in their existing modules.

Standalone locus4s is the sole owner of parcel-assignment storage and
validation through `PartialSurjection[X, P]`. Background is visible only as
`None`, and upstream construction rejects unused parcel points. Names, atlas
ids, colors, network terms, and display order remain separate fields indexed
by the parcel domain. Partition equality up to relabeling is distinct from
label-field equality.

Aggregation scans the supported ambient points once. Means should accumulate
a mergeable `(sum, count)`-like state and divide only at presentation time.
The exact hierarchy-fusion law applies to lawful commutative monoids; ordinary
IEEE floating-point addition is not claimed to be exactly associative.

## Minimal example

Each restored domain has an abstract owner type. Callers cannot choose or
forge that type, while the persistent id supports serialization and semantic
comparison. The returned registry snapshot belongs to a concrete dataset,
archive session, workflow, or application service and is threaded through
subsequent restorations in that scope:

```scala
import scalafim.locus.*
import locus4s.{DomainRegistry, PartialSurjection}

val voxelDomain =
  DomainFactory
    .restore(
      DomainRegistry.empty,
      SpaceKey.unsafe("sub-01:native:bold"),
      6
    )
    .toOption.get
type NativeVoxels = voxelDomain.S
val voxels: FiniteSpace[NativeVoxels] = voxelDomain.space

val parcelDomain =
  DomainFactory
    .restore(
      voxelDomain.registry,
      SpaceKey.unsafe("atlas:demo:parcels"),
      2
    )
    .toOption.get
type Parcels = parcelDomain.S
val parcelAxis: FiniteSpace[Parcels] = parcelDomain.space

val left =
  Region.fromOrdinals(voxels, Vector(0, 1, 2)).toOption.get
val requestedOrder =
  Selection.fromOrdinals(voxels, Vector(2, 0, 1)).toOption.get

val parcels =
  PartialSurjection
    .fromOptionalTargetOrdinals(
      voxels,
      parcelAxis,
      Vector(Some(0), Some(0), Some(0), Some(1), Some(1), None)
    )
    .toOption.get

val field =
  IndexedField.fromValues(voxels, Vector(10, 11, 12, 20, 21, 0)).toOption.get
val section = field.restrict(left)
val orderedValues = section.gather(requestedOrder).toOption.get.toVector
```

`orderedValues` is `Vector(12, 10, 11)`: ordering comes from
`Selection`, never from the set semantics of `Region`.

`DomainRegistry` is a pure immutable value. Concurrent effects must serialize
updates in an application-scoped reference and retain the returned snapshot;
branching an old snapshot intentionally creates independent live owners. No
registry is process-global, and dropping the owning resource releases the
scope. Ephemeral derived domains are never retained by a registry.
