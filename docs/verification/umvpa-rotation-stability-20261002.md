# Explanatory rotations and training-block stability, 2026-10-02

Mote `bd-01M2BNH659B05P8TRTDKCYFS48`. Baseline
`02728e8cfca9eff82bfe8ebde9a7af3a9003b418`. This draft uses the explicitly
local Multivar checkout `/private/tmp/multivar-umvpa-rotations-20261002` over
`f74d631720d65147c51496dcbdd37c01912de1cb`. Upstream publication and the
ScalaFIM pin bump are pending; local integration is not published-pin closure.

## Coordinate laws

Brain and target display axes are separate nominal coordinate systems, each
binding its original component descriptor and actual transform digest. The
native adapter retains the original artifact and predictive object. With
independent invertible transforms B and T, it stores `A B`, `C T` and the
middle relationship `B^-1 T^-T`; the effective forward map remains `A C^T`.
The potentially large neural-by-target map is not retained. Its explicit
materializer has an output-cell ceiling, not a whole-call peak certificate.

Raw filters transform as `P A B` and raw scores as `B^T u`. Calibrated
filters and scores transform as inverse-transpose and inverse, respectively.
An actual SPD covariance of the original factor coefficients, or an explicit
whitened factor-identity assertion, is required. These inverse-transform
coordinates differ from raw filter scores, which transform by transpose. Rotated covariance is `B^-1 Phi B^-T`; correlations
use that covariance and stable sequential square-root division. Actual
converged Gale SVD checks inverses and condition limits. Supplied transforms
have a separate method tag; they are not mislabeled as optimized varimax.

Independent nonsymmetric/shear, scale and sign-permutation arithmetic checks
include correlated score covariance, raw/calibrated scores, intercept-aware
encoding, continuous Gaussian decoding and categorical priors/probabilities.
A two-dimensional cofactor oracle supplies Gaussian predictions. Near-singular
transforms, non-SPD covariance, nonfinite scores and owned/output budgets
refuse. Prediction heads delegate to the original admitted object, preserving
priors, intercepts and scientific fit identity.

## Upstream rotation method

Multivar owns the statistical rotation adapter over Gale decompositions.
Varimax uses Kaiser or no row normalization, an SVD polar update, actual
coordinate fixed-point stopping and a polar midpoint for criterion plateaus.
A zero polar criterion has explicit termination, fixing the rank-one Kaiser
cycle. Iteration exhaustion, unacceptable objective decrease and nonconverged
SVD return typed errors. A converged local solution does not establish a
nonconvex global optimum or neural-source identification.

Promax uses the power-four target after the normalized varimax baseline,
Gale pivoted QR least squares, actual SVD inverse/condition limits and
right-column normalization. Its target residual and varimax criterion have
separate receipts. Score correlation is conditional on the explicitly whitened
original basis, not estimated covariance from unseen data.

An independent R scalar angle optimization supplies the stricter local varimax
criterion oracle; separate R QR and inverse arithmetic supply promax transform
and factor correlations. The original R stats::varimax output is retained;
its earlier relative stopping differs from the final coordinate stopping.
No fixture tolerance was loosened to hide that difference. Rank-one Kaiser
and two-row cycle boundaries have explicit tests. The earlier expert's
zero-polar counterexample was repaired; no new peer verdict is claimed for
this author's subsequent midpoint implementation. Independent executable
oracles and actual cross-platform gates remain separate evidence.

## Descriptive training sensitivity

`PatternTrainingStability.run` requires the actual discovery training rows,
matching neural/target coordinates, the frozen discovery identity and a
training exposure snapshot. Native leave-one-group-out units select whole
identified training blocks. A refit receives reindexed observations and
multivariate targets plus a binding naming the exact selected child axis.
Returned artifacts must bind that child axis and the original feature domains.
The adapter does not supply confirmation rows to the refit or optimize a
rotation using confirmation evidence. Foreign confirmation designs, unknown
external exposure and refit-count budgets refuse before the callback.

Each requested unit remains present, including failed refits and comparison
failures. A failure does not discard later groups. Exposure attempts are
retained with declared assurance; caller-provided numerical fits and their
private workspace remain the numerical owner's resource responsibility.
Snapshot identity is not external synchronization or physical-origin
authentication.

Upstream `SubspaceAgreement` reuses the existing orthonormal range extraction
and Gale's converged compact cross-basis SVD. It computes principal angles
without a dense ambient-square projector. It separately reports absolute
fixed-axis cosines: signs are ignored, component order is retained, and no
favorable matching/rotation is searched. If component descriptors differ,
native fixed-axis correspondence is unavailable. Rank changes retain actual
left/right dimensions. Numerically deficient generators refuse rather than
padding a basis or comparing invented axes. The existing range extractor's
numerical support threshold remains part of the capability boundary.

Actual native structured fits on two leave-one-training-block-out populations
exercise the refit path. A subsequent 45-degree within-plane gauge rotation
preserves both fitted subspaces while lowering fixed-axis cosines. Independent
3D geometry, sign/scale and permutation fixtures distinguish these laws.
This is descriptive sensitivity of a declared training procedure, not a
confidence interval, voxel p-value, selection-frequency significance,
source-identification or group-population result.

Upstream owned-array ceilings conservatively include concurrent polar midpoint
factors: `12 p r + 24 r^2` for varimax and `20 p r + 48 r^2` for promax.
Returned Gale factors count; private solver workspace remains excluded.

## Native checks and provenance

`gate149-rotation-stability-focused.log` passes 10 scoped tests on each platform.
The two ordinary refit workflows return `ScenarioResult` verdicts under the
existing Pass-only CI policy. Additional trust-boundary cases retain failed
refits, reject a returned artifact on unselected discovery rows, and exercise
actual one-component refits with `(2, 1)` dimensions and no invented fixed-axis
correspondence.

`gate150-rotation-stability-final.log` passes all 372 owning-module tests on each
platform. That invocation then ran unrelated `fit`, `dataset`, and `spatial`
module tests successfully, but exited 1 on this author's nonexistent
`estimatesIOJVM` target. It did not run the full compile. The complete failure
log is retained; it is not represented as an all-command pass.

The corrected `gate151-rotation-stability-final-consumers.log` exits 0 for
`mvpaFit{JVM,JS}/test` (45 each), `mvpaDataset{JVM,JS}/test` (108 each),
`mvpaSpatial{JVM,JS}/test` (20 each), `mvpaArtifactsJVM/test` (13),
`mvpaArtifactsJS/test` (5), and `scalafimCompileAll` (126.8 seconds).
Together, owning and relevant consumer suites pass 558 JVM and 550 JS tests.
There are no Scala source warnings in these two native logs. The cold local
build's pre-existing linops4s build-DSL streams warning and Java 25 archive-child
Unsafe warnings are retained separately; this is not a claim that every
transitive tool prints no warnings.

The 421-entry `gate151-local-rotation-sources.json` covers build configuration,
all five affected native modules and all local Multivar source/test trees.
Every entry was compared with the checked files after completion: zero
mismatches. This binds local-override evidence, not committed dependency pins.
Evidence directory: `/private/tmp/scalafim-umvpa-finish-evidence-20261001`.

SHA-256 receipts:

- Owning/failure log: `390ff5428f5634ae7adedc455d2ec83a8fdc5890cb05c8d6b27e709aa9012275`.
- Corrected consumer/compile log: `99042cd957d5f44c7ae0aebf2c258e23d20da2eb7606825202849542936a5555`.
- Source manifest: `9f0945d0d1ffc7b895c2b11955987669ce052700af1a435e73dbbfc5e7bd1208`.
- Independent R script: `010cae8246a342d77315f5cc0648742e737b4ee8efa513e1c496de5798c7b282`.
- Independent R output: `5b98c762dc0a8a65e600bb60dbb9d21a12c2527270245b8dc4432c6c99bf5e67`.

## Upstream checks and existing documentation blocker

`upstream-rotation-stability-final.log` passes `compileAll` and `testAll`:
core 593 tests per platform, IR 48 JVM/46 JS, inference 70 per platform
(711 JVM and 709 JS total). Scala compilation is warning-clean.
The same invocation's default `smokeCheck` then exhausts the launcher's
1 GiB heap in Scaladoc; `mimaCheck` is not reached.

A separate 8 GiB retry reaches an existing Scala 3.7.4 Scaladoc
`SignatureBuilder.content()` null-pointer exception. The unchanged published
revision `f74d631720d65147c51496dcbdd37c01912de1cb` reproduces that exact
exception in a separate clone and retained
`upstream-scaladoc-published-baseline.log`. Temporary package-filter diagnostic
runs establish a baseline model-family rendering issue; filtered builds are
not documentation qualification or changes to the repository's default gates.
Default `smokeCheck` therefore remains blocked by pre-existing Scaladoc, not
accepted as passing.

`upstream-rotation-stability-binary-smoke.log` exits 0 for an explicitly
binary-only local publication/consumer-resolution check: session-local
`packageDoc / publishArtifact := false` on the three JVM artifacts, followed
by `smokeCheck` and `mimaCheck`. The consumer has no source-module `dependsOn`.
These session settings are not committed, do not qualify the documentation,
and do not replace the failed default gate. MiMa runs but its previous-artifact
sets are empty, so no released binary-compatibility conclusion follows.
`upstream-rotation-stability-surface.log` exits 0 with the ordinary public API
snapshot unchanged. Private solver workspace, peak RSS, hosted builds,
calibration, new peer approval and upstream publication remain unqualified.

The concrete upstream draft is local commit
`e1146d1835d19cbdedaf65df02af6bad6e691e64` on
`work/umvpa-rotation-stability-20261002`; only four new shared source/test files
are committed. Final whitespace cleanup removed one empty line at the end of
`SubspaceAgreement.scala`: Git's ignored-blank-lines diff proves no other source
change. The original manifest/log hashes above remain the original receipts.
`m402-committed-sources.json` records all 421 current source hashes; the committed
upstream gate records a fresh required compile/test run. No root pin is changed
and M4.02 remains open pending upstream publication and pin qualification.
