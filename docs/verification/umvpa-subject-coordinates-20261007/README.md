# Commensurate subject coordinates — M5.01

Source freeze: 2026-10-07. Ticket `bd-01M2BNHSTYKBBD12JS8XXC9X7K`.
The source manifest records the exact local base, source hashes, independent
oracle and command. The focused suite passed **17 JVM + 17 JS** tests.
Full integrated `mvpaJVM/test` and `mvpaJS/test` each passed **433 tests**,
with zero failures, errors or warnings. Exact commands, source hashes and
retained raw-log hashes are in `integration-gates.json` and `focused-gates.json`.

## Binding and scientific scope

`SubjectCoefficientEstimate` describes coefficients **B** in
`X = Y Cs Bᵀ`, with explicitly supplied full joint covariance of row-major
`vec(B)` (feature first, component second). Scalar standard errors do not
provide the required component or cross-feature covariance. No diagonal
approximation is invented from `VoxelLoadingResult`. M5.02 owns the adapter
from a qualified confirmation procedure into this explicit boundary.

The estimate retains the C1 design, row-bound subject column, ordered feature
and task identities, brain/target evidence, coefficient/covariance value
identities, uncertainty receipt, and known/estimated/approximate/unknown df
provenance. A row-bound column naming multiple subjects is refused.

Shared coordinates, stability diagnostics, and the exact spatial measurement
map have frozen discovery accounts. Spatial selection binds its actual
measurement semantic id, source discovery, shared coordinates, and training
exposure. Confirmation/unknown exposure, wrong ordering, wrong spatial
endpoints, different discovery, and shared-discovery overlap refuse before
transport kernels. These account bindings cannot authenticate deliberately
renamed physical units or undeclared foreign exposure.

Stable component comparison requires both local and shared discovery axis
stability declarations. With `Cs R = Cshared`, coefficients become
`Bcommon = M B R⁻ᵀ`. For feature-major vectorization, the covariance map is
`M ⊗ R⁻¹`. The adapter uses Gale QR/solve/SVD, checks task projection residual
and conditioning, and never optimizes agreement of confirmation coefficients.

When axes are unstable but a discovery subspace is stable, the explicit
`TaskLinkedForwardOperator` result is `M B Csᵀ` on the original ordered task
axis. Its covariance map is `M ⊗ Cs`. It carries an operator label and does
not manufacture one-to-one component names. A completely unstable local
solution remains refused.

Physical coefficient, covariance and task-operator value units have their own
receipt. Anatomical coordinate units such as mm never become beta value units.
Operator units explicitly name the original target-value denominator. This
slice performs no implicit unit conversion. The smart unit constructor takes
coefficient/operator labels and a receipt; it derives both covariance labels
as their squared units, so an unrelated covariance unit cannot be supplied.

Covariance validation uses Gale eigenvalues only and accepts supported singular
PSD covariance without jitter or clipping. Mean and covariance transport use
two bounded block stages; no full Kronecker map is materialized. The owned-cell
estimate excludes provider internal scratch/object overhead. The scalar-product
policy bounds the deterministic planned domain matrix products, **excluding**
Gale spectral/factorization and arbitrary measurement-provider work. Neither
estimate is a whole-process memory or elapsed-time certificate.

## API composition

Given identified discovery snapshots, untouched training accounts, the subject
C1 design, a row-bound subject column, and an admitted native-to-common
`MeasurementLeg`, the stages are:

```scala
for
  valueUnits <- SubjectCoordinateValueUnits("BOLD-percent/latent-score",
    "BOLD-percent/stimulus-amplitude", "source value-unit convention")
  sharedStability <- SubjectAxisStability.freeze(globalDiscovery,
    globalExposure, SubjectStabilityKind.StableAxes, "discovery diagnostic")
  shared <- SharedTaskCoordinates.freeze(globalDiscovery, globalExposure,
    commonFeatures, sharedStability, valueUnits)
  localStability <- SubjectAxisStability.freeze(subjectDiscovery,
    subjectDiscoveryExposure, SubjectStabilityKind.StableAxes, "subject diagnostic")
  spatial <- SubjectSpatialAlignment.freeze(subjectDiscovery, shared,
    measurement, spatialSelectionExposure)
  source <- SubjectCoefficientEstimate.bind(subjectColumn, nativeFeatures,
    confirmationDesign, coefficients, fullJointCovariance,
    brainEvidence, targetEvidence, coefficientSource, covarianceSource,
    df, uncertaintyReceipt, localStability, valueUnits)
  result <- SubjectCoordinates.transport(source, shared, spatial,
    SubjectComparisonKind.SharedComponentCoefficients)
yield result
```

`spatialSelectionExposure.reference.result` must name
`measurement.descriptor.semanticId`; the account must name subject discovery.
Freeze these discovery selections before estimating confirmation coefficients;
bind the supplied full joint covariance only after its producing procedure
has supplied its own uncertainty/source receipt. The result preserves source metadata and df. It provides arithmetic
commensurability, not a population model, coverage qualification, prevalence
claim, or scientific release.

## Independent oracle

`oracle.R` uses base R `solve`, `kronecker` and ordinary matrix multiplication,
independently of the production block implementation. The original joint
covariance is formed from a supplied integer triangular factor `A Aᵀ`.
`expected.tsv` records every mean and covariance cell for an oblique shear,
signed rotation, gauge change and a nonorthogonal **3×2** anatomical map.
The Scala suite retains the independent numeric constants with explicit
tolerances and discriminating covariance-drop controls.

The exact integer fixture has `B=[[2,3],[5,7]]`,
`R=[[1,1],[0,1]]`, `M=[[1,2],[-1,1]]`. The transformed mean is
`[[-5,17],[-1,4]]`; covariance is:

```text
 33 -11  18 -4
-11  33  -4  6
 18  -4  21 -8
 -4   6  -8  9
```

The first R startup inherited an unsupported Darwin locale and emitted an
LC_CTYPE warning; generation succeeded. The recorded repeat explicitly uses
`LC_ALL=C LANG=C`, retains its log/session, and produces the same numeric table.

## Acceptance evidence and remaining consumers

| Contract | Evidence |
| --- | --- |
| Equal dimensions do not prove commensurability | Wrong ordered task, native/common endpoint, and row-bound subject refusal controls |
| Discovery-only task/spatial selection and stability | Exact snapshot/map accounts; holdout contamination and borrowed-account controls |
| Full uncertainty transport | Independent R integer shear/rotation and all covariance cells; cross-feature/component-drop controls |
| Unequal anatomical dimensions and singular uncertainty | Nonorthogonal 3×2 map and rank-deficient task operator; no jitter/clipping |
| Unstable axes use an explicit alternative | Local/shared stability guards and task-operator mean/covariance gauge invariance |
| Identities, units, df and subject variation survive | Source/unit/df controls, distinct subject effects and physical units independent of anatomical mm |
| Refusals do not re-read original brain observations | Real source spy with positive application control; metadata precedence over zero budgets |

Focused results: `mvpaJVM/testOnly *SubjectCoordinatesSuite`
and `mvpaJS/testOnly *SubjectCoordinatesSuite` (the matching class is
`scalafim.fmri.mvpa.analysis.SubjectCoordinatesSuite`), **17 passed each**, exit 0 and no compiler
warnings. Reported task times are 9 s JVM and 23 s JS (warm invocations 9.6/23.6 s).
These are focused task observations, not performance qualification.

An initial test compile failed because the suite package could not access the
existing scoped plan-id fixture constructor. The final suite uses the same
analysis package as existing confirmation fixtures; no production visibility
was broadened. Its initial failure log is preserved separately in the gate
receipt and must not be read as passing evidence.

Independent exact-source review accepted with no blocking findings; see
`independent-review.md`. Full `mvpaJVM/test` and `mvpaJS/test` on the integrated
M4/M5 source and the retained raw-log archive are complete; see
`integration-gates.json` for exact source and log hashes.
No upstream capability
or access gap was found for this bounded transport; covariance-provider and
group-model admissions remain the separate M5.02/M5 gates.

The application spy observes the original confirmation brain source: rejected
metadata/budgets do not re-read it, and a direct application is the positive
control. It does not count arbitrary measurement-provider internal operations.
The test class is `scalafim.fmri.mvpa.analysis.SubjectCoordinatesSuite`, matching
the existing confirmation fixtures' package for the scoped plan identity API.
