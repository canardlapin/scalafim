# Labelled atlas voxel-cell boundary distances

`AtlasBoundaryQuery.queryEither` measures Euclidean anatomical millimetres to
the exposed faces of labelled voxel cells. A voxel occupies its affine image of
the closed grid cube centred on its integer index with half-width0.5. A face is
exposed if the adjacent cell has a different label, no label, or lies outside
the atlas. Same-label interior faces do not count. A shared interface belongs
to both labels. Distance is unsigned both inside and outside a labelled region.

The query requires the exact atlas coordinate-space identity and performs no
registration or alias normalization. The full affine includes obliquity and
shear. It returns the nearest boundary witness for every region within the
inclusive finite search radius, sorted by distance then region ID. Distinct
regions with equal distances or equal text names remain separate. Within one
region, exactly tied faces choose voxel(z,y,x), axis, then negative/positive side.
The witness includes the original voxel face and the nearest anatomical point.
No hit means no labelled boundary within the radius; it is not a zero distance.

Inverse-affine row norms bound the candidate voxel cells intersecting the world
ball. The candidate count must fit the explicit voxel budget before traversal;
the default is one million. Cancellation is checked before admission, for each
visited voxel and before returning. No partial result is returned on failure.
Face distance uses a feasible orthogonal plane projection and four segment
projections. Faces with normalized Gram determinant at most1e-12 are refused
as numerically degenerate, even if the affine is algebraically invertible.

The shared suite has independent box, edge/corner, rotated anisotropic and
oblique-plane distance oracles, equal-label versus different-label interfaces,
zero-radius ties, no-label/outside cases, budget and mid-query cancellation.
JVM and Scala.js gates are required before adoption. This query does not change
the older labelled-centre `AtlasQuery`; its Int narrowing overflow is tracked
separately as `bd-01M2BAAYDG9CJRD60YF01MG3B3`.

Implementation issue `bd-01M2B8MAZ0H5PVGD4YES7N36RC`; provider prerequisite is
scene-codec head`4d00cc0` above admitted`a3ee73b`. This candidate is unadmitted
until both platform gates and downstream app boundary evidence pass.
