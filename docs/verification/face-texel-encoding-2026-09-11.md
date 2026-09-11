# Flat per-face texel encoding on stock OpenJFX 24.0.1 — 2026-09-11

Spike D for bead `bd-01M2847N1K11Y5Y1NT2S7G8PMG`. The question: can a flat
per-face colour, sampled from a texture at a coordinate that is constant over
each face, render PLS Neuro's cortical maps faithfully on **stock** OpenJFX
24.0.1 (macOS ES2), where the per-face tile atlas needs a patched runtime?

**Answer. GO.** Constant texture coordinates make stock sampling exact: every
planar and cortical frame is byte-identical on stock and patched runtimes and
for 2x2 and 1x1 texel blocks. Every residual difference from the flat raster
reference is a rasterization tie within 0.002 px of a face edge, so the
tie-aware gate passes where the literal gate cannot. Colour, cutoff and
map-value updates take 11.5–13.0 ms median on 4 worker threads, excluding
snapshot readback. Textures take 8.8 MB, a quarter of the atlas. Picks are
exact. The cost is visible faceting from 2x camera zoom.

**User decision.** `FaceScalarMean` is the default rule, labelled as face means.
`FaceScalarMaxMagnitude` is a declared display option ("show every
suprathreshold vertex"). Adoption is a separate bead: it must pass ScalaFIM's
native gates and shares Spike B's pin decision. No application code changed.

## Mechanism

Each scientific face owns a 2x2 texel block in one diffuse image per surface,
row-major in a power-of-two width (2048 x 542 and 2048 x 534 for the bilateral
cortex's 276,566 and 272,452 faces). All three corners of a face address the
block centre, the shared corner of four equal texels, so the texture coordinate
is constant over the face:

- zero screen-space derivatives select the base mip level under the mipmaps
  that stock ES2 forces on diffuse maps;
- multisample extrapolation of a constant is the same constant;
- there is no tile padding.

Bilinear filtering returns the block colour for any coordinate error below half
a texel. The 1x1 variant (`FaceTexelFlatSingle`, 1024 x 271 and 1024 x 267)
addresses texel centres exactly and rendered byte-identical frames.

Points and normals remain the scientific vertices, and native faces are the
plan's faces, so picks need no remapping. Lighting is per-pixel JavaFX Phong
from scene lights and interpolated vertex normals. After the first build, a
colour, cutoff or map-value change rewrites only the image: per-face values,
rule evaluation on a fork-join pool (`scalafim.faceTexel.threads`), block writes
into a heap mirror, and one bulk upload of the dirty rows. Updates never change
the `TriangleMesh`, as tests and the benchmark checks confirm.

## Declared face-flat modes

A plan that shows face colours never claims vertex interpolation.

- `SurfaceMapInterpolation.FaceScalarMean` and `FaceScalarMaxMagnitude`
  (`SurfaceFaceReduction`), created with `SurfaceLayer.faceFlatScalar`, reduce a
  layer's vertex samples to one value per face before mapping. The fragment
  evaluator, compiler resource keys, fragment approximation, legend and the
  reference raster implement both.
- **Mean** is the centroid value, the mean of the three samples. Any non-finite
  sample makes the face missing, exactly as the interpolated reference hides a
  face with a missing corner.
- **Max-magnitude** is the finite sample of largest `|value|`, keeping its
  sign. Non-finite samples are excluded, and a face is missing only when all
  three are. On an exact magnitude tie between opposite signs the positive
  sample wins. With the transparent band below the cutoff, a face is coloured
  exactly when one of its finite corners is at or beyond the cutoff.
- The curvature underlay always uses the mean, so the sulcal/gyral step
  follows the sign of the face's mean curvature. Below-cutoff and missing faces
  show that underlay, never grey. The two rules composite over the neutral base
  as the evaluator does.
- Scene documents carry an optional layer key `faceReduction` (`mean` or
  `max-magnitude`), from revision 7 on and emitted only when declared. Documents
  without face-flat layers are byte-unchanged, and readers that do not know the
  key refuse the document rather than misread it. Admission requires the new
  `SurfaceBackendFeature.FaceFlatScalar` (plus `FragmentComposition`), never
  `ScalarInterpolation`. Restore compares the declared reduction.
- Capabilities: the face-texel backend (`javafx-scene3d-face-texel-v1`) declares
  `FaceFlatScalar` and `FragmentComposition`, not `ScalarInterpolation`. The
  raster declares `FaceFlatScalar`; three.js refuses face-flat layers.
- Refusals: `FaceTexelFlat` refuses vertex-scalar, vertex-colour and
  nearest-vertex layers. `ScalarLutInterpolated` refuses face-flat layers. The
  final-colour atlas encodings refuse both.

## Inputs and provenance

- **Scenes.** The hashed scenes retained by the scalar lookup spike
  (`plsneuro-fixtures/spike-lut-1/inputs`, all 17 SHA-256 sums verified): real
  beta and FIR on the admitted bilateral inflated bundle at 1350 x 762, with
  549,018 faces and 274,513 vertices (101,359 faces touch a missing sample),
  and FreeSurfer sulc. They were not regenerated from the 2026-09-11 results.
- **Cutoffs.** Round 1 used each scene's retained cutoff (50th percentile of
  `|value|`). Round 2 used the application default, the 90th percentile of
  `|finite value|` pooled over hemispheres: beta 0.0042008, FIR 0.0024270.
  Both rounds use a saturated onset at the cutoff and the scene's 98th
  percentile symmetric range.
- **Runtimes.** Stock: OpenJFX 24.0.1+4 macOS ARM64 ES2, Coursier jars on the
  module path, no `--patch-module`, with material and shader class origins
  asserted. Patched: the same plus the runtime-25 compatibility JAR (SHA-256
  `6025860c…7194`, no colour mipmaps, centroid UVs). JDK 22.
- **Build.** `sbt -Dscalafim.intaglio.build=file:///Users/bbuchsbaum/code/scala/plsneuro-fixtures/intaglio-55658ab-lut`.
- **Machine.** Apple M3 Max, 14 cores (10 performance, 4 efficiency), shared
  with other sessions; timings are single-machine medians.
- **Evidence.** `plsneuro-fixtures/spike-face-1` (round 1) and `spike-face-2`
  (round 2), each with `criteria.json`, `SHA256SUMS`, images, logs, commands and
  tools. Both `criteria.json` files are stored beside this report as
  `face-texel-encoding-2026-09-11-round1.json.gz` and `-round2.json.gz`.

## References and gates

All references come from the portable raster's face picks and the declared
rule, never from the native texture, its coordinates or its sampler.

- **Point reference:** the rule colour of the face hit by each pixel centre
  (MSAA off).
- **MSAA reference:** the mean over the standard 4-sample positions
  (0.375, 0.125), (0.875, 0.375), (0.125, 0.625), (0.625, 0.875), in image rows.
  These are taken from a 4 x 4 sub-pixel raster whose sample centres include
  them. The pattern was chosen on a prespecified calibration case (patched
  noisy-underlay-256 with MSAA on): RMS 0.414, against 2.841 for its vertical
  mirror, 1.833 for a 16-sample box and 5.960 for the point reference.
- **Bands:** Chebyshev distance 0, 1, 2 and 3 or more pixels from point-reference
  colour steps greater than 6 levels. Error is the maximum absolute RGB channel
  difference in 8-bit levels, over pixels whose 16 sub-samples all hit a face,
  eroded by one pixel.
- **Literal gate:** RMS of at most 1 and at most 0.1% of pixels more than 2
  levels off, in every band. MSAA off is scored against the point reference;
  MSAA on against the MSAA reference.
- **Tie-aware gate** (the reviewer's method, in
  `spike-face-2/tools/face_metrics2.py`): a pixel more than 2 levels off passes
  only when a sample lies within **0.002 px** of a reference face edge and the
  native colour is within 2 levels of a tie outcome.
  - MSAA off: the tie outcomes are the colours of the pixel centre's three
    edge-adjacent faces and its 16 sub-sample faces.
  - MSAA on: each of the 4 samples within 0.002 px of an edge may take any of
    its edge-adjacent faces, and every combination is averaged.
  - The pixel's error becomes that of the best tie outcome; bands and the gate
    are then recomputed. The literal gate is always reported beside it.
- **Edge distance:** from each sample to the nearest edge of its picked face,
  through each surface's world-to-pixel map. That map is an affine
  least-squares fit to the raster's own picks and is exact for orthographic
  views; the maximum residual is 1.3e-4 px on the cortex and 4.8e-6 px planar.
- **Footprint check (round 1):** with MSAA off, the native pixel must be within
  2 levels of one of its 16 sub-sample face colours. With MSAA on, it must lie
  within the per-channel hull of those colours, widened by 2 levels.

## Round 1 — mechanism on stock (branch head `3b3519f`)

Encoding `FaceTexelFlat` (2x2) unless noted; mean rule at the scene cutoff.

| Criterion | Measured | Verdict |
|---|---|---|
| 1 Fidelity, literal gate | Cortex beta, MSAA off: contour band RMS 3.38 with 0.104% over 2 levels; 1 px band 0.015 / 0.002%; bands at 2 px and beyond exactly 0. MSAA on: contour band 1.59 / 0.29%, away band 0.011. FIR is the same (3.35 / 0.104%; 1.58 / 0.295%). Planar: 8/24 cases pass in each MSAA mode; ramp-256 RMS 0.033 off and 0.265 on | Fail, identically on stock and patched |
| 1 Cause | Every cortex pixel over 2 levels with MSAA off (163 beta, 168 FIR) lies within 0.0019 px of a face edge, where only 1.8% of pixels are. Footprint check leaves 36/5 (beta off/on) and 31/2 (FIR) unexplained, all in the contour band; planar leaves 2 in all 96 images | Rasterization ties, not sampling |
| Identity | All 96 planar and 9 cortex frames (including lit) byte-identical stock vs patched; 48 planar and 4 cortex frames identical 2x2 vs 1x1 | Pass |
| 2 Visual and faceting | Crops in `spike-face-1/images`. Faceting against the vertex-interpolated scalar raster (informational): RMS 41.8 overall, 6.35 at 3 px and beyond | Informational |
| 3 Latency (render + snapshot including readback, 14 threads) | Serial implementation 52 ms (43 ms of CPU colour evaluation). Parallel implementation, beta: palette, cutoff and map 14.7 / 14.4 / 14.2 ms unlit and 17.1 / 16.2 / 16.5 lit; 1x1 13.6 / 13.9 / 13.0; FIR 20.8 / 18.2 / 20.4 (p90 up to 35). Upload plus mipmaps about 3.5 ms, of which mipmap regeneration is about 2 ms | Pass (parallel) |
| 4 Memory | Textures 8,814,592 bytes (1x1: 2,203,648) against 35,258,368 for the adaptive atlas; mesh arrays 30,745,104 against 40,193,412 | Pass |
| 5 Picks | 1,200/1,200 same face in all 19 runs; existing 1,200-face atlas suites pass | Pass |
| 6 Missing and below-cutoff show the underlay | Rule checked against the recipe at all 549,018 faces (maximum difference 0); tests; images | Pass |
| 7 Module tests | surfaceViewJVM 151/151, surfaceViewJS 151/151, surfaceViewJavafxJVM 50/50 | Pass |

A control scored patched `AdaptiveAffineOpaque` against its own vertex-colour
raster with the same bands. It passes with MSAA off (contour band 0.64 /
0.002%). With MSAA on there is no 4-sample vertex-colour reference, so no
comparison was made.

## Round 2 — rules, declared modes, gates (branch head `be9a028`)

### Rule statistics at the 90th-percentile cutoff

A suprathreshold vertex is finite and mapped opaque. A cluster is a connected
set of same-sign suprathreshold vertices. A vertex is shown if an incident face
shows an overlay colour of its sign. Area is 3D inflated surface area coloured
by flat faces, compared with the region where the vertex-interpolated scalar
reaches the cutoff (exact per-triangle clipping). Jaccard is the same-sign
overlap divided by the union.

| Scene, rule | Vertices hidden | Clusters hidden | Single-vertex clusters hidden | Coloured area vs interpolated | Jaccard |
|---|---:|---:|---:|---:|---:|
| beta, mean | 457 / 22,793 | 156 / 679 | 107 / 135 | 0.986 | 0.866 |
| beta, max-magnitude | 0 | 0 | 0 | 1.490 | 0.671 |
| FIR, mean | 480 / 22,795 | 180 / 773 | 112 / 142 | 0.980 | 0.854 |
| FIR, max-magnitude | 0 | 0 | 0 | 1.527 | 0.655 |

At the scene cutoff, these definitions give 1,126 / 113,955 hidden vertices and
414 / 1,880 hidden clusters for beta with the mean rule. The reviewer counted
998 / 113,843 and 387 / 1,872; the suprathreshold counts differ, so the
threshold test differs slightly. Crops of both rules beside patched Adaptive
and the interpolated raster:
`spike-face-2/images/modes-{beta-lateral,beta-medial,fir-lateral}-q0.9-BALANCED.png`.
The max rule's blobs are fatter and polygonal; the mean rule tracks the
interpolated extent.

### Criteria

| Criterion | Measured | Verdict |
|---|---|---|
| Tie-aware gate, cortex | 8/8 images pass (beta and FIR, mean and max, MSAA off and on). MSAA off: every pixel over 2 levels is an explained tie (32–45 per image); contour band RMS 0–0.008. MSAA on: 109–137 per image, of which 1–2 stay unexplained, each with a sample 0.0021–0.0023 px from an edge; contour band RMS 0.27–0.34, over 2 levels at most 0.0034% | Pass |
| Literal gate, cortex | Contour band RMS 3.08–3.56 with MSAA off and 1.55–1.63 with MSAA on | Fail (ties) |
| Tie-aware gate, planar | 24/24 cases per rule and MSAA mode. Unexplained pixels in total: mean off 0, on 13; max off 0, on 8. Literal gate: mean 8/24 and 8/24, max 14/24 and 8/24 | Pass |
| GPU test | `JavaFxFaceTexelGpuSuite` requires uniform-band pixels to be exact, every other mismatch to be a tie within 0.002 px taking the colour of a face sharing a vertex, and 2x2 frames to equal 1x1. It passes on JavaFX 21.0.5 (the sbt test runtime) and on stock and patched 24.0.1 (`JUnitCore` under the probe launcher). It is skipped, with its reason, only if the toolkit or 3D support is unavailable | Pass |
| Identity | Stock equals patched (beta mean, MSAA off and on). Round 2 mean at the scene cutoff equals round 1 byte for byte (natives and flat reference). Planar: round 2 mean equals round 1, 96/96; stock equals patched, 96/96; max rule 2x2 equals 1x1, 48/48 | Pass |
| Picks, memory, first show | 1,200/1,200 in every run. Textures 8.8 MB vs 35.3 MB; mesh arrays 30.7 vs 39.7 MB. First show 639–754 ms, Adaptive 709–789 ms | Pass |
| Module tests | surfaceViewJVM 158/158, surfaceViewJS 158/158, surfaceViewRasterJVM 30/30, surfaceViewRasterJS 30/30, surfaceViewThreeJS 28/28, surfaceViewJavafxJVM 55/55 | Pass |

### Camera zoom

Beta was rendered at 1x, 2x, 4x and 8x by scaling the orthographic camera about
the view centre, not by upscaling pixels. The renderers were patched Adaptive,
stock `FaceTexelFlat` with each rule, and the interpolated raster
(`spike-face-2/images/zoom-beta-{left,right}-q0.9-BALANCED.png`).

| Zoom | Mean face area | Face side | Facets |
|---:|---:|---:|---|
| 1x | 1.0 px² | 1.4 px | Not visible |
| 2x | 4.1 px² | 2.9 px | First visible: sawtooth contour and underlay boundaries |
| 4x | 16.5 px² | 5.8 px | Clearly visible |
| 8x | 66 px² | 11.5 px | Obvious |

Patched Adaptive has smooth fills but blurs contours from 4x.

### Latency

Each row is the median of 20 updates after 4 warm-ups, on stock `FaceTexelFlat`
with the mean rule, beta, MSAA on, 90th-percentile cutoff. Latency is the render
call plus the snapshot after the update, minus an empty-scene readback
baseline of 2.5–3.0 ms. The worker pool and `-XX:ActiveProcessorCount` were
limited together.

| Configuration | Palette | Cutoff | Map values |
|---|---:|---:|---:|
| 4 threads | 12.0 ms | 11.5 ms | 13.0 ms |
| 8 threads | 8.7 ms | 8.5 ms | 9.9 ms |
| 14 threads | 8.1 ms | 8.3 ms | 8.7 ms |
| 4 threads, max rule | 11.8 ms | 11.4 ms | 13.0 ms |
| 4 threads, lit | 12.0 ms | 11.7 ms | 12.6 ms |
| 4 threads, 1x1 texels | 10.1 ms | 10.3 ms | 11.6 ms |
| 4 threads, patched runtime | 9.3 ms | 9.1 ms | 10.5 ms |

Phase medians at 4 threads:

| Phase | Time |
|---|---:|
| Per-face values, cached (palette, cutoff) | 0.22 ms |
| Per-face values, recomputed (map change) | 1.55 ms |
| Colour evaluation | 6.1 ms |
| Texel write | 0.6 ms |
| Upload plus mipmaps, stock | 3.0–3.2 ms |
| Upload plus mipmaps, patched | about 1.0 ms |
| Repeat snapshot (draw plus readback) | 4.0 ms |

Mipmap regeneration therefore costs about 2 ms. Colour evaluation dominates.

## Platform runs out of scope

Windows D3D, Linux ES2 and JavaFX 26/27 Metal were not run. A run on any of
them must measure:

1. Runtime identity: `javafx.runtime.version`, Prism pipeline, GPU and driver,
   and material and shader class origins.
2. Sampler behaviour for constant texture coordinates: whether diffuse maps get
   mipmaps, and which level a constant coordinate samples, for 2x2 and 1x1
   blocks. Also non-power-of-two heights, and textures of at least 2048 x 542.
3. `JavaFxFaceTexelGpuSuite` must pass: uniform bands exact, every other
   mismatch a tie within 0.002 px.
4. MSAA sample-pattern recalibration on that pipeline, before any MSAA-on
   scoring. The D3D and GL/Metal standard positions may differ.
5. Literal and tie-aware gates for beta and FIR, both rules, MSAA off and on,
   against the same raster references.
6. Byte identity between 2x2 and 1x1 blocks, and against any runtime patch.
7. Update phases at 4 and 8 threads (per-face values, evaluation, texel write,
   upload plus mipmaps, readback baseline). The median must be 25 ms or less,
   excluding readback.
8. First show time, texture and mesh bytes, and peak RSS.
9. 1,200 native picks against the raster.
10. Device scale above 1: snapshot size and pick coordinates.

## Limitations

- **Coverage:** macOS ARM64 ES2 only, on one shared machine. Two real scenes
  from one result; the faces are about one pixel at the shared bilateral
  camera. Lit rendering was visual and benchmarked, not gated.
- **Tie threshold:** the 0.002 px threshold is the reviewer's. With MSAA on, 1–2
  pixels per cortex image and 21 planar pixels in total sit just beyond it
  (0.0021–0.0023 px on the cortex) and are reported as unexplained.
- **Face-flat is a different presentation contract.** It is not
  interpolation. Faceting is visible from 2x camera zoom. The mean rule hides
  small clusters, most of them single-vertex. The max rule overstates coloured
  area by about half.
- **Latency:** colour evaluation scales with worker threads. A 4-core machine
  still meets 25 ms here, but slower GPUs, drivers or pipelines need
  measurement.
- **Adoption:** open under a separate bead. It must pass ScalaFIM's native
  gates and the provider pin decision shared with Spike B.
