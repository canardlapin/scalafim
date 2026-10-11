# Surface mesh equality and observed sample tally (2026-09-30)

Mote: `bd-01M37FQGV8ZPT30X37TA4MM8R7`, items (3) and (4) of the fsLR audit
follow-up (`docs/audits/group-volume-fslr-mapping.md`).

This packet leaves three items untouched:

- Item (1), duplicate lookup engines: `VolumeToSurfaceOperator.scala` has
  uncommitted edits by another agent in the shared checkout.
- Item (2), GIFTI frame metadata.
- Item (5), the consumer-pin compile check.

## Changes

- **(3) Structural mesh equality.** `TriangleMesh` is a case class over
  `Array` fields, so its generated `==` compared array references.
  `equals` and `hashCode` are now structural:
  - equality requires the same ordered faces and bitwise-equal coordinates,
    so `-0.0 ≠ 0.0`;
  - the hash is cached.

  `SurfaceGeometry` equality becomes structural with it. So does
  `SurfaceVertexMapping`'s `field.geometry == sourceGeometry` check: a field
  on a separately loaded, identical surface is now accepted.
- **(4) Observed sample tally.**
  - `VolumeSurfaceSampler` records a `SurfaceSampleTally` as it samples.
    Every requested point falls in exactly one category: outside the volume,
    masked, non-finite, or accepted as finite. The constructor enforces this.
  - `SurfaceProjectionReceipt` carries the tally and derives
    `requestedSamples`, `acceptedSamples` and `rejectedSamples` from it.
    `requestedSamples` is now observed rather than recomputed from the path,
    and `acceptedSamples` no longer counts non-finite values.
  - The GPU projector (`ThreeVolumeProjector`) reports the same categories.
    It takes one midpoint sample per vertex and applies no mask. A valid
    vertex whose value reads back non-finite counts as non-finite.
  - Sampled values, aggregation and per-vertex `sampleCounts` are unchanged.

## Finding for the surface owner (not changed here)

`sampleCounts` includes non-finite samples, so `SurfaceProjectionPolicy`
(`minimumSamples`) can mark a vertex *qualified* even when its only sample is
NaN. Its value then stays NaN rather than taking the fill value. `Average`
aggregation likewise propagates one NaN sample into the vertex value.

Whether non-finite samples should be excluded from aggregation and
qualification is a behaviour decision for the surface owner. The receipt now
at least reports them.

## Evidence

Branch `surface/sampling-tally-20260930`, isolated worktree, base `d9c08120`.

| Gate | Result | Log SHA-256 prefix |
| --- | --- | --- |
| `surfaceJVM/test surfaceViewJVM/test` | 165/165, 59/59 | `95ac4a9cc5039c00` |
| `surfaceJS/test surfaceViewJS/test surfaceViewThreeJS/test` | 131/131, 59/59, 14/14 | `d76369cf4b47e18d` |
| `scalafimCompileAll` | success, 0 warnings | `a78a1e9f55dea96b` |
| Mutation check: NaN samples counted as accepted | tally test fails; source restored | `44da5d97532c27dc` |

New tests:

- `TriangleMeshEqualitySuite`: separately built equal meshes are equal and
  hash alike; coordinates, face order and signed zero each distinguish
  meshes; `SurfaceGeometry` equality is structural; a mapping accepts a field
  on a reloaded identical geometry.
- `SurfaceSamplingSuite`: tally cases for in-volume, fractional-ribbon,
  masked, out-of-volume and NaN samples, plus tally invariant refusal.
- `SurfaceProjectionNetworkSuite`: a NaN voxel is rejected, not accepted, in
  the receipt; the masked receipt reports `tally.masked`.

**Superseded 2026-10-10.** The owner decided both questions: non-finite samples
neither count toward `minimumSamples` nor enter any reducer. See row 4b of
`volume-surface-gap-matrix-20261010.md`.
