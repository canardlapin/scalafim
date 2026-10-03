# fsLR route through the pointwise inverse

Native ticket `bd-01M3WCQD1MFW1WRTJP6C6A2ZFS`; provider request reframe4s
`bd-01M3WRE7BEJFR3G67BBEBH2NGE`. Base: reviewed reconciliation
`c603045413faef00ebb0b55b1decca5b3b55de6c`
([handoff](fslr-current-main-20261001.md)).

## What changed

`FrameBridge.displacement(map, PointMapUse.Inverse(policy))` now admits the
MNI152NLin6Asym -> MNI152NLin2009cAsym bridge that the reconciliation refused.
Each vertex is solved where it lies by reframe4s `PointwiseInversion`
(reframe4s `5f7152aada60335935843ddc968162f615337415`). The declared affine
stages are composed and removed analytically. Only the displacement is
iterated, starting from identity. No inverse is sampled on a lattice or
interpolated between queries.

- A converged vertex is `PointMapOutcome.Inverted(point, residualMm, iterations)`.
- Anything else is `InverseUnsolved(failure, iterations, bestResidualMm)`,
  with a typed `InverseFailure`: max iterations, divergence, left support, or
  failure. An unsolved vertex is never placed or sampled; it is reported as
  `VertexCoverage.BridgeUnavailable`.
- The route discloses
  `BridgeExactness.Approximate(NumericalBridgeMethod.PointwiseFixedPoint(policy))`.
  `BridgePlacement.inverseSummary` aggregates the per-vertex evidence.
- Maps the provider cannot invert this way (an affine before the displacement,
  several displacements, affine only) are refused at admission with
  `ReferenceError.PointwiseInverseUnsupported`.
- The placements are numerical estimates with measured residuals. They are not
  an exact inverse, and they do not certify invertibility.

An earlier candidate that inverted on a 1 mm lattice was rejected. It failed a
frozen round-trip budget (0.081 mm against 0.05 mm). Measured against an
all-vertex oracle on consumer fixtures, it moved about 0.36% of cortical
vertices to a different nearest voxel, with value changes up to 4 SD and sign
flips. It is removed.

## Anatomy frame

The fsLR midthickness is declared with `FrameBasis.PublisherMethods`, citing
Van Essen et al. 2012 (doi:10.1093/cercor/bhr291), Materials and Methods
p. 2245. Each of 69 subjects was aligned by FLIRT (affine) to FSL 4.1.7
`MNI152_T1_1mm`, which is TemplateFlow MNI152NLin6Asym, and the surfaces were
averaged. The TemplateFlow asset is byte-identical to the Conte69 v2 32k
midthickness. This is the publisher's declaration. It is not an asset-specific
registration proof, and an average matches no single anatomy. Numerical
qualification of the bridge is separate from this basis.

## Independent oracle

`tools/transform/generate_fslr_inverse_oracle.py` solves all 64,984 fsLR 32k
vertices with SimpleITK 2.5.6 against the original TemplateFlow HDF5
composite. It uses fixed-point iteration from identity, run to 1e-10 mm. All
vertices converge within 33 iterations, inside the displacement support. The
script first reproduces the earlier 2,500-vertex-per-hemisphere fixture. That
fixture stopped at 12 iterations, so each vertex is checked against a bound
derived from its own recorded residual. The output bytes are deterministic.
`RealInverseOracleSuite` checks every solution against ScalaFIM's exact
declared map, to within 1e-6 mm.

## Frozen budgets and results

The budgets were recorded on the native ticket before the bridge was built.
P1-P3 repeat quantities first seen in a direct solver assessment, so they are
not blind; they are re-measured through the production bridge.

| Budget | Result |
| --- | --- |
| P1 every vertex converged, residual <= 1e-8 mm | 32,492 / 32,492 per hemisphere; max 1.0e-8 mm; <= 30 iterations |
| P2 placement vs oracle <= 1e-6 mm | max 1.50e-8 (L), 1.53e-8 (R) mm |
| P3 no nearest-voxel disagreement, 1 mm and 2 mm 2009c grids | 0 and 0 |
| P4 consumer fixtures: no coverage, value or sign disagreement | 0 across 15 maps x 2 hemispheres |
| P5 medial wall exact, no unavailable vertex, display identity | 2,796 / 2,776 wall vertices exact; 0 unavailable; identity holds |
| P6 JVM admission + placement <= 30 s | 8.7 s (both hemispheres, GM mapping included) |
| P6 Scala.js real-field placement <= 120 s | 31.0 s (Node 26, fastopt); load + SHA-256 + decode 12.5 s |

The first P6-JVM run measured 37.9 s. Its timer wrongly included loading and
hashing the 200 MB field under heavy host load. The scope correction was
recorded before re-measuring, and the limit is unchanged. The real field
places identically on Scala.js and the JVM.

## Running the gates

```sh
SCALAFIM_REQUIRE_REAL_ASSETS=1 sbt "surfaceJVM/testOnly scalafim.surface.reference.RealPointwiseInverseSuite scalafim.surface.reference.RealInverseOracleSuite"
SCALAFIM_JS_REAL_TIMING=1 sbt "surfaceJS/testOnly scalafim.surface.reference.RealPointwiseJsSuite"
sbt "surfaceJVM/Test/runMain scalafim.surface.reference.FslrBridgeDisagreement pointwise <spec.json> <out.json>"
```

`FslrBridgeDisagreement` takes `scalafim.fslr-qualification-input/1` specs
(see `FslrQualification`). Consumer fixtures are supplied by the consumer;
ScalaFIM does not commit them.

## Remaining limits

- Consumer qualification belongs to the consumer: source-volume release,
  cohort, statistic identity, coverage presentation, picks and exports.
- The anatomy frame rests on the publisher's methods, as stated above.
- The pre-existing `TemplateSphereFilesSuite` can exceed munit's 30 s timeout
  under heavy host load. It is unrelated to this change.
