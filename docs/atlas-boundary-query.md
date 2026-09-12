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
inclusive finite search radius with an explicit arithmetic envelope, sorted by
measured distance then region ID. Distinct
regions with equal distances or equal text names remain separate. Within one
region, exactly tied faces choose voxel(z,y,x), axis, then negative/positive side.
The witness includes the original voxel face and the nearest anatomical point.
No hit means no labelled boundary within the radius; it is not a zero distance.

Inverse-affine row norms bound candidate cells intersecting the world ball.
Each multiply/add in the inverse application is outward rounded, using the
relative point (point minus forward translation). The stored inverse B is checked
against the forward linear A by outward intervals for R=I-A B. With rho=||R||∞,
the Neumann bound ||A^-1-B||∞ <= ||B||∞ rho/(1-rho) pads the grid centre and
inverse-row norms. Nonfinite bounds, rho>=1/8, or discovery uncertainty above
0.01 voxel are refused. Discovery residual and padding are reported separately.

The distance arithmetic envelope is gamma(128) times an outward world-coordinate
scale, divided by a lower bound on the minimum normalized face Gram determinant
(sin² theta, not its square). Here gamma(n)=n*u/(1-n*u), u=ulp(1)/2; the scale
includes the query-point norm and an affine bound over atlas cell corners. The
envelope covers the coordinate and normalized face arithmetic chain and its
conditioning. Candidate bounds and final radius admission both use radius plus
this envelope. Each hit retains measured distance, distanceErrorBoundMm and
nearestPointErrorBoundMm, plus DefinitelyWithin or NumericallyBorderline admission.
The same envelope covers Euclidean arithmetic error of the emitted anatomical
nearest point on its selected face, including the final world-coordinate addition.
It does not promise a unique witness across distinct exactly equidistant faces;
the recorded face and deterministic face-order rule retain that identity.
Ordering uses measured
distance. Borderline admission is numerical uncertainty, not a scientifically
expanded radius. This replaces the disproven final-only one-ulp guarantee.

The query refuses envelopes above max(1e-12 mm, 1e-8 times the smallest voxel
spacing). That explicit representability floor preserves zero-radius interface
queries without allowing broad uncertainty at extreme coordinate magnitudes.
The candidate count must fit its explicit budget before traversal, default one
million. Cancellation is checked before admission, for each
visited voxel and before returning. No partial result is returned on failure.
Face distance uses a feasible orthogonal plane projection and four segment
projections. Faces with normalized Gram determinant at most1e-12 are refused
as numerically degenerate, even if the affine is algebraically invertible.

The shared suite has independent box, edge/corner, rotated anisotropic and
oblique-plane distance oracles, equal-label versus different-label interfaces,
zero-radius ties, no-label/outside cases, budget and mid-query cancellation.
It retains the independent 80-digit Decimal counterexample where the old face
formula overestimated a radius by77 ulps, verifies inclusion inside the exposed
envelope and exclusion beyond it, tests inverse residual admission/refusal,
and covers translated inverse-dot cancellation and materiality refusal.
JVM and Scala.js gates are required before adoption. This query does not change
the older labelled-centre `AtlasQuery`; its Int narrowing overflow is tracked
separately as `bd-01M2BAAYDG9CJRD60YF01MG3B3`.

Implementation issue `bd-01M2B8MAZ0H5PVGD4YES7N36RC`; provider prerequisite is
scene-codec head`4d00cc0` above admitted`a3ee73b`. This candidate is unadmitted
until both platform gates and downstream app boundary evidence pass.
