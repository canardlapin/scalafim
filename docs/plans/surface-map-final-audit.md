# Surface map requirement audit

Status: complete locally, 2026-09-08. This audit preserves the scope of
[surface-map-semantics.md](surface-map-semantics.md). Each row identifies its
contract and direct evidence. The
[final receipt](../benchmarks/receipts/surface-map-final-2026-09-08.json) records
current source hashes, cross-platform gates, native artifacts, and cleanup.

## Facewise data and association (Stage 1)

| Requirement | Implementation and direct checks | Evidence state |
| --- | --- | --- |
| 1. Owned, frame-major face fields with exact ordered topology | `SurfaceFaceFieldSuite`: ownership, frame bounds, reordered faces, reversed winding, hemisphere mismatch and coordinate-only morphs | JVM/JS conformance passes |
| 2. Separate association and value kind; scalar/label/mask/RGBA constructors | `SurfaceLayer`, `SurfaceFaceLayerSuite`; explicit face field constructors and face scalar presentation checks | JVM/JS conformance passes |
| 3. Compiled association, resource identity, revision migration and unknown-revision rejection | `SurfaceCompiler`, `SurfaceSceneCodec`, `SurfaceFaceLayerSuite`, scene compatibility suites | JVM/JS conformance passes; current scene revision 6 |
| 4. Original face, vertex, surface and barycentric pick/readout | `SurfaceFaceLayerSuite`, raster/Three face suites, native ray probe | 28 JavaFX native ray cases pass; all 252 cortical WebGL stages pass |
| 5. Constant face display without scientific topology changes; normals and expanded bytes | `SurfaceFaceRasterSuite`, `ThreeSurfaceFaceSuite`, JavaFX face and approximation suites | Native analytic probes and final JavaFX cortical cases pass |
| 6. Ordered mixed face/vertex composition, opacity and blending | `SurfaceFragmentCompositionSuite`, `SurfaceFragmentEvaluator`, native fragment probes | Shared/raster JVM/JS passes; final existing WebGL harness passes |
| Native acceptance and retained updates | Interior palette checks; time/style/camera upload receipts; morph provenance | Final JavaFX cortex: 60 static cases, 86 unlit updates, 9,344 native picks; 252 cortical WebGL stages and 16,128 native picks pass |

## Interpolation and JavaFX feasibility (Stage 2)

| Requirement | Implementation and direct checks | Evidence state |
| --- | --- | --- |
| Explicit color, face-constant, nearest and scalar alternatives | `SurfaceMapInterpolation`, typed layer constructors and compiler payloads | JVM/JS conformance passes |
| Preserve legacy vertex-color operation order and migrated scenes | Revision compatibility suites and independent example viewport mask oracle | Both example suites pass; explicit Fill reproduces the old image hash |
| Discrete labels/masks cannot become interpolated scalars through a flag | Separate scalar constructors; nearest policy and label constructors | Shared constructor and scene tests pass |
| Perspective-correct interpolation before visibility/mapping; nonfinite contributors | `SurfaceScalarInterpolationSuite`, `SurfaceScalarRasterSuite`: analytic coordinates, nonlinear mapping, clipping, invalid values and extreme finite arithmetic | JVM/JS conformance passes |
| Original-ID tie rule and categorical membership | `SurfaceNearestSuite`, `SurfaceNearestRasterSuite`, native nearest probes | Analytic native baselines pass; all 252 cortical WebGL stages pass |
| Inspectable scalar mappings; opaque callbacks retain supported sample evaluation only | Mapping metadata and unsupported adjustment tests; Three float-boundary admission tests | JVM/JS conformance passes; unsupported combinations reject before upload |
| A: constant face texture regions | JavaFX constant unlit path and original-face picking | Final cortical matrix and repeated benchmarks pass |
| B: nearest six-cell partition and source provenance | Area/ownership/tie regressions; native pixels, morphs and actual rays | Final JavaFX cortical and repeated benchmark evidence passes |
| C: scalar lookup exploration, finite envelopes and texel/filter errors | Isolated lookup prototype and scalar-fragment review; clamp-before-interpolation counterexample | Lookup is not promoted as a general supported path; bounded fallback is explicit |
| D: threshold/ramp partition, coincident edges, original face IDs, explicit update cost | `SurfaceMappingPartition`, `SurfaceFragmentApproximationSuite`; native depth/threshold probes | Final JavaFX cortical and repeated resource admission passes, including explicit depth-budget rejections |
| E: ordered scalar/folding composition, declared lighting and approximation/resource limits | `SurfaceFragmentEvaluator`, normal-cone and blend interval tests; five-mode native cortical matrix | Final JavaFX static/update evidence passes; all 252 cortical WebGL stages pass |
| Native sizes/projections/lighting/picks/morphs/uploads and truthful capability limits | 32,768 and 163,842 vertex synthetic benchmark matrices, anatomical corpus, analytic probes | 870 rendered stages, 55,680 native picks, 18 checked cold rejections; no interactive-latency guarantee |

### Two-axis lookup decision

A two-axis texture can encode an unlit scalar overlay and scalar underlay, with
one field in each texture coordinate. It does not remove the sampled lookup
limitations measured in Stage 2C: fixing the underlay reduces the construction
to the one-axis case, whose sharp-band and piecewise fixtures failed their native
budget. An RGBA8 table also costs `4 * width * height` base-level bytes (64 MiB
at 4096 by 4096), before mipmaps and driver storage.

The same two coordinates cannot encode general declared lighting. Two locations
with identical overlay/underlay values can have normals `(0, 0, 1)` and `(1, 0, 0)`.
For a composed gray value of 100, ambient 0.4, diffuse 0.6, and light `(0, 0, 1)`,
the required output is respectively 100 and 40. A single lookup entry cannot
supply both. Native Phong lighting is not a substitute for the declared
world-space model. The implementation therefore uses the evaluated per-face
geometric/atlas path, with interval color bounds and explicit resource limits.
The two-axis option was considered but is not advertised as an admitted general
compositor; it remains a possible narrower unlit optimization.

## Mapping descriptors (Stage 3)

Intaglio is pinned to `55658abfbbfed0c9a36ab612b38bd8f0677bc158`. The current
resolved dependency checkout has that exact HEAD. Its `ScalarMappingSuite`
covers independent asymmetric continuous/split tail values, ramp knots, every
endpoint inclusion convention and immediate neighbors, split-gap edges,
invalid/hidden/saturated identity, visibility before clamping, legacy behavior,
checked overrides, invalid constructors, extreme finite limits, canonical keys,
and affine changes of units. The upstream core/SVG receipt records 527 passing
JVM/Scala.js tests at the pinned revision.

| Numbered requirement | Consumer evidence |
| --- | --- |
| 1. Validated limits, ramps, visibility, hidden/invalid and out-of-range states | `SurfaceScalarMappingSuite` tabulated vertex/face colors and checked overrides |
| 2. Continuous/diverging/split alternatives with asymmetric center and valid tails | Pinned upstream `ScalarMappingSuite`; scalar raster split and nonlinear fixtures |
| 3. Inside/outside intervals, endpoint inclusion, legacy open band, clamp versus hide | Pinned upstream endpoint/legacy tests; native shader edge fixture |
| 4. One effective evaluator/identity for overrides, shader, legend, serialization and keys | Mapping identity, camera/opacity invariance, stale legend, scene restoration and publication identity tests |
| 5. Opaque colorizers retain supported behavior without invented limits/legends | Explicit metadata and adjustment rejection tests |

The article supplies conceptual semantics. These fixtures do not claim numerical
parity with an unpinned historical script or palette.

## Legends and publication (Stage 4)

| Requirement | Direct evidence | Evidence state |
| --- | --- | --- |
| 1. Continuous/split/categorical/manual alternatives; effective layer binding | `SurfaceLegendSuite`: all alternatives, missing/hidden/opaque layer rejection, category completeness and shared effective scales | JVM/JS conformance passes |
| 2. Quantity, units, limits/ticks, threshold/gap/saturation and missing keys; matching normalization | Intaglio scalar legend piecewise oracle; surface override/tick tests | Pinned upstream and consumer tests pass |
| 3. Intaglio drawing, manuscript/poster presets, shared versus incompatible mappings | `SurfaceLegendPublicationSuite`; 12 native SVG layout cases and eight exact opaque swatch pixels | Dated publication evidence passes; source implementation hashes remain unchanged |
| 4. Mapping/legend/policy persistence and stale identity prevention | `SurfaceSceneLegendSuite`: revision-6 round trips, threshold/palette tampering, camera invariance and revision-5 compatibility | JVM/JS conformance passes |
| 5. Legends explicitly represent unlit mapping colors | Public guide and publication explanatory text distinguish lighting, opacity and composition | Implemented and documented |
| Visual acceptance of long labels/units, asymmetric and missing-data legends | 12 manuscript/poster gallery artifacts with native text bounds and collision checks | Published receipt and reviewed images retained |

## Final integration evidence

The [post-benchmark integration receipt](../benchmarks/receipts/surface-map-benchmark-integration-2026-09-08.json)
records the earlier 596 conformance tests, 23 JVM and 17 Scala.js example tests,
`surfaceViewAdmissionJVM` (96 cases, three repetitions each), and all four
`surfaceViewVisualQaJVM` probes. The
[final JavaFX cortical receipt](../benchmarks/receipts/surface-cortical-final-javafx-2026-09-08.json)
adds the complete anatomical reruns after the geometric-normal fix.

The refreshed final build passes 601 conformance tests and 23 JVM / 17 Scala.js
example tests. The existing browser harness passes all 48 admission cases, 64
composition cases, 28 scalar-edge cases, four clipping cases, and the analytic
nearest-lighting regression (152 pixels, maximum channel error 1). Interior checks now
require complete projected pixel footprints; antialiased boundary differences
are retained separately. Renderer fixes preserve fractional viewport alignment,
keep scalar/normal interpolation inside partially covered triangles, and match
the reference legacy lighting of generated nearest corners.

The cortical facewise perspective probe exposed a native depth precision
failure: two contributing faces differed by about 2.4 depth-buffer steps, and
the GPU selected the farther face for one sample. The renderer now conditions
the occupied geometry interval, shared across all slots. Independent tests cover
screen coordinates, close-depth separation, clipping, eye crossings, and
translated multi-slot bounds. The final full matrix includes the formerly
failing 1024-pixel facewise case, which passes 86,262 strict pixels with zero
channel error. The receipt retains the original geometry and an independent
screen-space depth calculation.

The [final cortical WebGL receipt](../benchmarks/receipts/surface-cortical-webgl-2026-09-08.json)
passes all 20 configurations and 252 stages: 10,592,698 strict interior pixels,
maximum channel error 3, minimum mask IoU 0.9927939174, and 16,128 native picks
with maximum barycentric error 0.000176584. All original-ID and retained-upload
invariants pass. The [native gallery](../visual-qa/surface-cortical-webgl/README.md)
contains 40 cortical images plus the existing harness image; selected views of
all five modes were inspected.

The final existing browser harness passes all 48 admission, 64 composition,
28 scalar-edge and four clipping cases. Two focused software checks (lit scalar
morph and lit layered white geometry) pass without antialiasing. A final
production check rejects the known four-sample SwiftShader depth defect before
surface upload. Every browser run records resource closure, and the ownership
audit finds no automated browser processes. No claim extends this evidence to
all devices or to a complete software-renderer matrix.

All numbered requirements and required gates are closed. Earlier receipt
“remaining” fields describe dated intermediate states and are superseded here.
No new public file-format codec, areal resampling, statistical threshold
estimation or automatic face-to-vertex conversion was part of this plan. The
browser corpus transport uses existing JVM readers and checks all four external
source checksums. The current ScalaFIM slice is not committed or published.
