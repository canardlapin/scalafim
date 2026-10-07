# M4.06 independent implementation review

Reviewer: `/root/atlas_test_isolation`, 2026-10-07.

**Verdict:** no remaining blocking implementation findings in the reviewed
component procedures. This review covers the M4.06 procedure contract and
independent deterministic oracle reasoning. M4.08 family correction and M4.09
scientific release remain unavailable.

## Exact reviewed main source

| File | SHA-256 |
| --- | --- |
| `ComponentInference.scala` | `31ed07db6356111817df362fd79e4d875505ffe0a83326d2f257a7b256ecfc3d` |
| `ComponentConfirmation.scala` | `3f9ea8658754ced1bd8372b549cbe430a7c2d35f699c2fcb284ce946e1972325` |

Both files are under
`modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/pattern/`.
The [source manifest](source-manifest.json) and
[integration receipt](integration-gates.json) identify the final executed
test sources and build inputs.

## Scientific and binding review

- Association uses a separately declared, design-bound spherical joint Gaussian
  score law over independent rows. Huh–Jhun residual coordinates and pinned
  Multivar rank-one CCA preserve the nuisance-adjusted paired association.
  The provider does not recenter these prepared coordinates. Common seed,
  residual row count and distinct candidate IDs yield common actions across
  the named association members.
- Incremental prediction retains discovery-fitted full and separately refitted
  reduced heads. Its estimand is conditional expected reduced-minus-full loss,
  given discovery, the fitted heads and actual confirmation predictors. Known
  response covariance is declared separately; a voxel covariance or Gaussian
  label alone does not supply this reference.
- With metric `M` and fixed predictions `f` and `r`, the row difference is
  `(f-r)ᵀ M (2Y-f-r)`. Equal row weights within each independent unit and equal
  unit weights determine the affine contrast. The complete component mean
  covariance is `Aᵀ Γ A`, including cross-target and within-unit terms.
  Claimed independent units require zero cross-unit covariance.
- Known covariance must bind the actual source identities and ordered rows and
  targets, be symmetric and positive definite, and precede confirmation
  exposure. Estimated, unknown, marginal or misbound laws refuse. Source,
  exposure and aggregate storage refusals precede source projection.
- The fixed nominal one-sided 5% test and two-sided 95% interval use explicit
  R-qualified critical values. They are actual candidate reference calculations
  without a private CDF or quantile approximation. Zero contrast variance is
  explicitly unavailable. P-values for this Gaussian route, corrected-family
  inference and admitted C1 results remain typed unavailable.
- The complete ordered `2r` family binds both procedures to the same plan and
  confirmation sources. A failed member cannot silently produce a complete
  family result. Caller declarations do not authenticate physical source origin
  or undisclosed external exposure.

## Resolved numerical finding and independent controls

The reviewer identified loss cancellation in the initial inference mean.
For `f=1`, `r=0`, `Y=10⁸` and unit metric, subtracting separately rounded
squared losses gives `200000000`; the exact affine difference is `199999999`.
The production mean now uses the stable residual-sum affine difference.
Public-pipeline controls retain this positive-offset case, the negative
improvement case and identical-head zero-variance refusal. Full and reduced
loss diagnostics remain available separately.

The reviewed controls also cover independent R mean/covariance/interval
expectations for full target covariance, unequal unit sizes and known
within-unit dependence; covariance/source/order/exposure/budget refusals;
common actions; and incomplete-family and release boundaries.

The finite-group control independently recursively enumerates all 720
permutations and calculates normalized scalar dot products from the prepared
score inputs. Its oracle does not call the production QR/SVD statistic or
generic inference reducer. Because numerical ties can differ by a few ULPs,
the test uses strict/inclusive tolerance brackets. This establishes numerical
agreement within those brackets, **not exact mathematical-tie qualification**.
The inclusive identity/plus-one receipt checks do not remove that limitation.

## Execution evidence and remaining admission boundary

The reviewer ran no builds, Scala tests or calibration simulations. Parent
execution evidence reports 15 focused controls and 11 existing component
arithmetic tests passing on each platform, followed by full `mvpaJVM/test`
and `mvpaJS/test` with 433 passes each. Commands, logs, exact hashes and results
belong to the integration receipt; these are engineering and deterministic
oracle evidence.

The frozen known-truth protocol's independent datasets, seed quarantine,
fixed transform budgets, type-I/coverage/power decision bands and complete
failure receipts have not been established by this review or those gates.
M4.09 remains responsible for scientific admission. The generic portable
Normal CDF/quantile capability gap remains explicit; this slice implements
only the fixed critical-value Gaussian reference.
