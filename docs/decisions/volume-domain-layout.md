# ADR: canonical volume-domain layout

- Status: accepted
- Date: 2026-08-22
- Issue: `bd-01KZ6BDWJDSZ34YD0QY573N1V0`
- Providers: image4s `31bc8f87d8349fd3296496979c95eeb3ec11ae21`;
  locus4s `58c9739be51345ad9adc4bc9c9e7335023254ec9`

## Context

ScalaFIM image containers historically number a three-dimensional coordinate
`(x, y, z)` with the first axis fastest:

```text
legacy = x + nx * (y + ny * z)
```

image4s-locus exposes one versioned grid-domain layout,
`row-major-last-axis-fastest/v1`:

```text
canonical = ((x * ny) + y) * nz + z
```

The two orders differ on a non-degenerate grid. Treating an integer from one as
an integer in the other silently changes the represented voxel. Ravel's
physical storage is not evidence for either logical convention.

## Decision

ScalaFIM adopts one persistent voxel domain: the image4s-locus
`GridDomain` with `row-major-last-axis-fastest/v1`. This is decision A from
the migration issue.

Legacy first-axis-fastest ordinals remain an explicitly named boundary format,
not a second persistent voxel domain. Transition mechanism B is mandatory:
`VolumeOrdinalBridge` builds a checked locus4s `Bijection` by converting
every legacy ordinal to a lattice coordinate and asking image4s-locus for the
canonical ordinal at that coordinate. No conversion may infer order from an
array or reinterpret an integer in place.

A volume-domain persistent key comes from the complete image4s grid record plus
the versioned image4s-locus layout. ScalaFIM creates persistent image4s grids
with a reproducible id containing the spatial shape and raw affine bits;
image4s also retains and validates the complete frame, shape, and affine
record. Runtime `hashCode` is not persistent identity.

Dataset id, acquisition id, run length, atlas name, and display metadata do not
participate in voxel-domain identity. They remain metadata or identify their
own non-spatial domains. Equal exact grids share a voxel owner only through an
explicit caller-owned `DomainRegistry` scope.

## Boundary inventory

| Artifact family | Boundary layout | Migration |
| --- | --- | --- |
| `NeuroVol.linear`, `VoxelIndexSet`, `VoxelRegion`, `VoxelSelection` | legacy first-axis-fastest | transport through `VolumeOrdinalBridge` |
| locus regions, selections, fields, assignments | canonical image4s layout | use the authoritative `FiniteSpace` |
| dataset `VoxelIndex` and readable masks | legacy first-axis-fastest | convert when building selections and active-to-full injections; convert back on read plans |
| atlas label volumes | legacy first-axis-fastest | expose labels as a canonical indexed field before constructing parcel assignments |
| threshold compact `volumeIndex` | legacy first-axis-fastest | convert the selection before constructing its injection |
| `ROIVolWindow.parentIndex` and members | legacy first-axis-fastest | convert centers and relation rows before locus searchlight construction |
| persisted ordinal vectors | declared by `VolumeOrdinalRecord.layout` | accept the two known versions, persist canonical, reject unknown layouts |

NIfTI, Zarr, archive, and fixture payloads that expose image-container ordinals
remain legacy at their existing API boundary. A future persisted locus artifact
must carry the exact grid record and layout; a bare ordinal vector is
insufficient.

## Rejected alternative

Permanent support for a second provider-level
`FirstAxisFastestV1` grid-domain layout was rejected. The admitted
image4s-locus revision defines only the last-axis-fastest layout, and no
provider-independent use case justifies enlarging its persistent identity
surface. Legacy compatibility is fully represented by the checked bijection
and versioned artifact record.

## Verification contract

The migration court uses an asymmetric `2 x 3 x 5` grid with a hard-coded
30-entry mapping table. Shared tests run on JVM and Scala.js and cover:

- coordinate formulas and coordinate round trips;
- both directions of the certified bijection;
- ordered selection and region transport;
- exact-grid and same-size foreign-grid rejection;
- canonical record round trips, legacy record conversion, and unknown-layout
  rejection;
- atlas assignment fibers, dataset masks and selection order, threshold
  injections, and searchlight center/member order.

Bridge construction is linear in voxel count and allocates two integer
permutations through locus4s certification. Point conversion is constant time.
Callers should retain the bridge with the owning resource instead of rebuilding
it inside a numeric loop.
