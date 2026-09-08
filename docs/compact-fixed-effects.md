# Compact fixed-effects uncertainty

Combining independently fitted runs must retain their full coefficient covariance.
For run r and voxel v, let C_r be the selected normalized coefficient covariance,
s_rv² the residual variance, and b_rv the coefficient vector. ScalaFIM uses

    P_rv = inverse(C_r) / s_rv²
    P_v  = sum_r P_rv
    b_v  = solve(P_v, sum_r P_rv b_rv)
    Cov(b_v) = inverse(P_v)

This is joint inverse-covariance weighting. Off-diagonal entries, shared
intercepts, coefficient identities, and run provenance remain part of the
estimator. Shared estimands and voxels must be usable in every contributing run
under the current `RequireAllRuns` policies. Zero or non-finite residual
variance does not mean infinite precision: the existing availability policy
excludes that voxel from the combination.

## Storage and access

A run's precision field retains one square base matrix and one positive scalar
per voxel. Combined covariance retains those fields and computes a small matrix
when a voxel is requested. Construction validates every summed precision by
Cholesky factorization and checks its inverse for finite values, using bounded
working storage. Coefficients and standard errors are still materialized as
predictor-by-voxel matrices. The solve for coefficients uses the Cholesky factor;
it is not replaced by multiplication by an explicitly formed inverse.

For R runs, V voxels, and P shared coefficients, retained structured precision
storage is R × (P² + V) doubles instead of R × V × P². Combined covariance
references the same precision fields rather than retaining another V × P²
doubles. Weighted coefficient sufficient statistics, coefficients, standard
errors, voxel identities, and other fit data require additional storage.

Use `coefficientCovariance.matrixForVoxelPosition(v)` for checked indexed access.
`selectVoxelPositions` and `mergeByVoxel` preserve compact views. A selected view
retains its parent; selecting fewer voxels does not necessarily release parent
storage. Repeated access recomputes covariance, trading CPU work for retained
memory. Callers that repeatedly need one matrix can retain that matrix locally.

`coefficientCovariance.materialize(maximumMatrices)` explicitly refuses a logical
matrix count above the caller's limit. The legacy `.matrices` accessor still
materializes all matrices and can consume substantial memory. Its limit is a
matrix count, not a byte budget.

`retainedDoubleCount` and `retainedPrecisionDoubleCount` count retained numeric
doubles. They exclude temporary matrices, object overhead, and integer axes.
Shared references may be counted more than once, so sums across objects are
conservative and are not measurements of heap usage. They do not establish a
whole-analysis memory budget.

## Compatibility

`FixedEffectsRunContribution.precisionByVoxel` is now an immutable
`IndexedSeq[DMat]`, rather than a `Vector[DMat]`. Existing vector arguments remain
usable. Code requiring a vector result must explicitly materialize with
`.toVector`; downstream binaries must be rebuilt. No binary compatibility claim
is made for this change. Arbitrary supplied precision sequences are validated
and snapshotted before forming a compact inverse-sum field.

## Verification contract

`CompactFixedEffectsSuite` compares the public run-combination API with the
independent mixed-TR R fixture, including full correlated covariance and standard
errors. Analytic structured fields check storage growth and off-diagonal values;
legacy vector contributions, disjoint folds, voxel availability, and bounded
materialization exercise compatibility and refusal behavior. Both JVM and
Scala.js runs are required. A full-size consumer run is a separate resource gate;
small numerical tests do not establish cohort-scale performance.

An opt-in covariance-field stress probe uses four units, three runs, 206,086
voxels, and 17 coefficients with analytically invertible correlated precisions:

```sh
sbt 'fitJVM/Test/runMain scalafim.fmri.fit.CompactCovarianceScaleProbe'
```

It validates all covariance fields, retains the four fields simultaneously,
checks sampled matrices against an independent analytic inverse, and prints a
JSON receipt. These are matrix fields at a real MNI common-mask cardinality, not
a simulated neuroimaging dataset. The probe excludes coefficient fitting,
weighted-coefficient statistics, image decoding, and group analysis.
