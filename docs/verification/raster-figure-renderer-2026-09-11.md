# Reference raster as figure renderer and off-gate cortex path — 2026-09-11

Spike B for bead `bd-01M277KNZYE5RD49J49Q9PEW8T`. The question: can the
`reference-raster` interpreter (`SurfaceRasterizer`) be PLS Neuro's
publication-figure renderer, and the labelled non-interactive cortex path on
machines outside the qualified JavaFX gate?

**Answer.** Yes for figures and for the labelled non-interactive fallback.
Figures should render at 2x supersampling with a box filter. Output is
byte-deterministic everywhere we looked. The slowest figure took 1.4 s,
against a 30 s budget. At 2x the output is closer to an area-integrated
reference than the qualified JavaFX render is. A labelled *interactive*
fallback needs the banded parallel variant; single-threaded it is borderline.

No application code changed. All measurement code is test-scope spike code
under `modules/surface-view-raster/jvm/src/test/scala/scalafim/surface/view/raster/spike/`.

## Inputs and provenance

- **Scenes.** Four exported compiled plans: beta (Component 1, condition
  estimate) and FIR (Component 1, FIR bin 4.00-6.00 s), each lit and unlit.
  Each has the admitted bilateral inflated bundle: 549,018 faces and 274,513
  vertices, with 46,605 vertices lacking values. All use a binary
  sulcal/gyral underlay, the robust overlay recipe (98th-percentile symmetric
  range, 50th-percentile cutoff, transparent below the cutoff) and a missing
  colour of rgb(135,143,145). All use an orthographic shared bilateral camera,
  which shows the left hemisphere laterally and the right medially. Lit scenes
  use the default directional light (ambient 0.35, diffuse 0.65).
- **Result files.** The source `.pls-result` files under `/private/tmp` were
  swept overnight. The regenerated
  `plsneuro-fixtures/real-usable-beta-20260911` fixture did not exist during
  this spike. The plans embed the geometry, final vertex colours, cameras and
  per-size fitted slots, so no re-projection was needed.
- **Plan producer and interpreter.** The PLS Neuro frozen closure compiled the
  plans: ScalaFIM `fd992a0` plus candidate patch `ed388817…`. The spike
  `SpikePlanCodec` serialised them, and the `a3ee73b` rasterizer rendered
  them. A suite test verifies that a codec round-trip leaves pixels unchanged.
- **JavaFX oracle.** The previous worker rendered the oracle images with the
  qualified launch: patched OpenJFX 24.0.1+4 ES2, compatibility JAR
  `openjfx-24.0.1-affine-atlas-compat.jar`, `AdaptiveAffineOpaque`, JDK 22,
  macOS ARM64. Repeat snapshots were identical. They were not re-rendered here.
- **Build.** `sbt -Dscalafim.intaglio.build=file:///Users/bbuchsbaum/code/scala/plsneuro-fixtures/intaglio-55658ab-raster`.
  That directory is a copy of the qualified 55658ab override, not a git
  checkout. Its source-tree SHA-256 is `c30537c8…00be1ed`. The first matrix,
  from the previous worker, had resolved Intaglio core from an sbt staging
  clone at `dbbafc79`. A full rerun on the override classpath reproduced
  every pixel and PNG digest. `SpikeSuite` passes 2 of 2.
- **Machine.** Apple M3 Max: 14 cores (10 performance, 4 efficiency), 36 GiB,
  JDK 22+36 aarch64 with ParallelGC. The machine was shared with 85 sessions,
  and load average was 9.7-12.6 during the runs. Treat times as contended
  upper estimates; parallel medians show the most noise.

Outputs, logs, receipts and diff images are in
`/Users/bbuchsbaum/code/scala/plsneuro-fixtures/spike-raster-1/`. The
consolidated receipt `spike-b-receipt.json` is also stored beside this file
as `raster-figure-renderer-2026-09-11.json.gz`.

## Thresholds

| Threshold | Measured | Verdict |
|---|---|---|
| Figures: deterministic at 1350x762, 2700x1524 and 3600x2032 | Every pixel and PNG digest was identical in every configuration listed under Determinism | Pass |
| Figures: within tolerance | Provider `SurfaceVisualQaPolicy.NativeBackend` passes at all three sizes for all four scenes. Pixelwise agreement within 2 levels does not pass; see Agreement | Pass under the provider policy |
| Figures: time under 30 s | 3600x2032 at 2x: median 0.58-0.89 s, cold 0.61-1.00 s, heap-limited worst 1.38 s | Pass |
| Labelled non-interactive fallback: 1 s or less at 1350x762 | Cold first render 398-454 ms single-threaded; warm 167-266 ms | Pass |
| Labelled slow interactive: 250 ms or less at 1350x762 | Warm single-thread median 190-240 ms, one warm run at 266 ms. With 2-14 threads, 24-51 ms. Cold first frame 316-454 ms | Marginal single-threaded; pass with 2 or more threads after warm-up |

Figure times cover render, box filter and PNG encoding, after a 66-73 ms
scene load. The interactive row covers render time only. The plan recompile
after a camera action and the image upload were not measured.

## Timing

Render only, 1350x762, warm. Values are median and minimum in milliseconds;
cold is the first render in each JVM.

| Scene | 1 thread | 2 | 4 | 8 | 14 | Cold, 1 thread |
|---|---:|---:|---:|---:|---:|---:|
| beta lit, JVM a | 190 / 167 | 51 / 48 | 43 / 39 | 45 / 35 | 101 / 35 | 404 |
| beta lit, JVM b | 240 / 182 | | | | 27 / 24 | 454 |
| beta unlit | 205 / 168 | | | | 40 / 35 | 398 |
| FIR lit | 218 / 190 | | | | 47 / 34 | 437 |
| FIR unlit | 213 / 179 | | | | 57 / 33 | 414 |

Figure path for beta lit: warm median total in milliseconds for JVM a and
JVM b, with cold in parentheses.

| Output | k | 1 thread | 14 threads |
|---|---:|---|---|
| 1350x762 | 1 | 267 / 264 (507) | 90 / 89 |
| 1350x762 | 2 | 282 / 285 (318) | 124 / 99 |
| 1350x762 | 4 | 464 / 454 (536) | 171 / 178 |
| 2700x1524 | 1 | 502 / 459 (762) | 285 / 293 |
| 2700x1524 | 2 | 592 / 570 (785) | 320 / 323 |
| 3600x2032 | 1 | 631 / 622 (1102) | 457 / 476 |
| 3600x2032 | 2 | 815 / 886 (1000) | 760 / 582 |

FIR lit at 3600x2032 with k = 2 takes 867 ms single-threaded and 565 ms with
14 threads. PNG encoding with ImageIO dominates at large sizes: about 55 ms at
1350 wide, 195 ms at 2700 and 316-349 ms at 3600. Render time is under 0.5 s
in every figure configuration. For comparison, the qualified JavaFX snapshot
took 415-608 ms for the first frame and 24-58 ms for warm snapshots at 2700
and 3600 wide.

## Memory

- **1x renders.** All three sizes complete deterministically under
  `-Xmx448m` single-threaded; peak heap used is 221-444 MiB.
- **3600x2032 at 2x.** The render canvas is 7200x4064. It needs more than
  1 GiB of heap single-threaded, failing at 1 GiB and passing at 1.5 GiB. With
  14 threads it needs more than 1.5 GiB, passing at 2 GiB. Both failures are
  `OutOfMemoryError: Java heap space`.
- **Working set.** The interpreter holds about 36 bytes per rendered pixel:
  colour, depth, pick identities and three barycentric floats. A 4x figure at
  3600 wide, 14400x8128 pixels, would need about 4.2 GiB, so it requires tiling.
- **Resident size.** Maximum resident size was 1.6-3.4 GB under 6-14 GiB heap
  limits. That reflects lax collection, not a requirement.

## Determinism

Pixel and PNG SHA-256 digests were identical across all of these:

- the cold run plus three warm runs within one JVM;
- a second JVM;
- the sequential interpreter and the banded parallel variant at 2, 4, 8 and
  14 threads;
- heap limits from 448 MiB to 14 GiB;
- the staging and override Intaglio classpaths;
- the 1x supersample path compared with the render-only harness.

The 2x output at 1350x762 is byte-identical to a box filter of the 2700x1524
render. All three sizes share identical normalized fitted slots.

## Agreement with the qualified JavaFX render

The metric is the per-pixel maximum absolute RGB channel error. Interior means
foreground pixels whose four neighbours are also foreground; edge means the
remaining foreground. The raster is point-sampled at pixel centres, while
JavaFX uses balanced multisampling with centroid UV interpolation.

| Beta lit, raster vs JavaFX | Interior mean | p95 | Within 2 levels | Edge p95 | Mask IoU | Centroid (px) |
|---|---:|---:|---:|---:|---:|---:|
| 1350x762, 1x | 3.58 | 17 | 68.8% | 118 | 0.9960 | 0.071 |
| 1350x762, 2x | 2.56 | 13 | 76.8% | 64 | 0.9994 | 0.036 |
| 2700x1524, 1x | 1.61 | 8 | 84.1% | 113 | 0.9980 | 0.057 |
| 2700x1524, 2x | 1.37 | 7 | 87.0% | 64 | 0.9997 | 0.015 |
| 3600x2032, 1x | 1.13 | 6 | 88.9% | 104 | 0.9985 | 0.048 |
| 3600x2032, 2x | 1.01 | 5 | 90.7% | 64 | 0.9998 | 0.017 |

FIR agrees to within about 0.2 levels of these values. Unlit scenes show about
50% larger interior means: 5.5-5.8 at 1350 wide with 1x.

The NativeBackend limits are mask IoU of at least 0.90, centroid distance of
at most 3 px, and mean interior error of at most 24. All 12 JavaFX pairs at
1x pass them, as do all figure-size pairs at 2x.

The appearance document's two-level budget is not met pixelwise. That budget
was defined against a footprint oracle on planar fixtures. The document calls
its cortical raster-vs-JavaFX figures diagnostic. The comparison below shows
why that budget cannot apply between two cortical renderers.

### Anti-aliasing against an 8x supersampled reference

Each candidate is compared with the raster rendered at 10800x6096 and box-filtered
to 1350x762, which approximates the area-integrated image.

| Beta lit, vs 8x reference | Interior mean | p95 | Maximum | Within 2 levels | Edge p95 |
|---|---:|---:|---:|---:|---:|
| Qualified JavaFX | 2.49 | 13 | 68 | 78.1% | 44 |
| Raster 1x | 2.07 | 10 | 145 | 77.9% | 112 |
| Raster 2x | 0.61 | 2 | 42 | 96.7% | 42 |
| Raster 4x | 0.18 | 1 | 26 | 99.8% | 16 |

The other three scenes show the same ordering. Interior mean ranges are:

| Candidate | Interior mean |
|---|---:|
| JavaFX | 2.49-3.90 |
| Raster 1x | 2.07-3.52 |
| Raster 2x | 0.61-0.81 |
| Raster 4x | 0.18-0.21 |

Edge p95 is 40-44 for JavaFX, 111-112 at 1x, 37-42 at 2x and 15-16 at 4x.
The 4x raster differs from JavaFX by 2.48-3.89 interior mean. That matches
JavaFX's own distance from the reference, so the residual disagreement is
JavaFX multisample error, not raster error.

Visual inspection confirms this. At 1x the raster shows stair-stepping on
overlay and curvature boundaries. The 2x output is at least as smooth as
JavaFX, and 4x is indistinguishable from the reference. The crop sheet is
`crops/beta-lit-1350-aa-crops.png`.

Diff images, with error amplified four times and mask disagreement in red,
are `compare/*-diff.png` for 1x against JavaFX and `compare-aa/*-diff.png`
for 2x against JavaFX and each candidate against 8x. In the 1x diffs the
error sits on colour boundaries: binary curvature and overlay edges. Flat
regions agree.

## Feature parity

| Feature | Evidence | Status |
|---|---|---|
| Lighting | Directional vertex Lambert lighting in both renderers. Lit scenes agree better than unlit ones: interior mean 3.6 against 5.5 at 1350 wide. The app pin's rasterizer applies the same lighting, but its capability set omits `Lighting` and its caveat still says lighting is not applied | Parity; the app pin's metadata is stale |
| Binary curvature underlay | Pure-class pixel IoU at 1350 / 3600 wide on unlit scenes: sulcal 0.86 / 0.95, gyral 0.79 / 0.92 | Parity; differences are anti-aliasing mixing |
| Missing vertices | Pure missing-colour pixel IoU 0.94 / 0.98 | Parity |
| Cutoff transparency | Both renderers composite the same final layers, and the underlay shows through transparent overlay. Positive overlay IoU 0.88 / 0.96; negative 0.70 / 0.88 for the small blue class | Parity |
| Camera pairing | The shared bilateral camera, left lateral and right medial, matches JavaFX with centroid offset of at most 0.07 px. The `a3ee73b` rasterizer supports per-surface paired cameras (`PerSurfaceCameras`, plan revision 8), but no real-cortex paired scene was rendered or compared. The app pin's rasterizer and compiler lack paired cameras | Shared camera: parity. Paired cameras: provider only, unmeasured on cortex |
| Device scale | Normalized fitted slots are identical at all sizes, so 2700x1524 is exactly the 2x device-scale version of 1350x762. The interpreter renders physical pixels; the caller supplies logical size times output scale | Parity |
| Silhouette | At 1x the raster mask is about one pixel thinner than JavaFX's multisampled coverage: 1,108 JavaFX-only pixels at 1350 wide. At 2x the mask IoU is 0.9998 | Resolved by supersampling |
| Chrome and legends | The rasterizer ignores `plan.chrome`. `SurfacePublication.decorate` and `compose` place vector chrome over a raster image and exist at both `fd992a0` and `a3ee73b`. Not exercised here | Provider path exists |

## Application wiring proposal

This is a proposal only; no application code changed.

**Provider.** The app's ScalaFIM pin `fd992a0` already contains
`surface-view-raster`. The candidate patch already modifies its viewport fit.
It is not in the app's project list. Wiring it needs four provider-side steps:

1. Add `surfaceViewRasterJVM` to the application closure: `build.sbt` and
   `docs/provider-contract.json` projects.
2. Correct the pin rasterizer's `Lighting` capability and its stale caveat.
3. Promote the banded parallel interpreter from spike test scope to
   `surface-view-raster/jvm`, if the slow-interactive state is wanted.
4. Add a plan content digest API.

Paired lateral views need the plan-revision-8 provider regardless of renderer.

**What crosses the boundary.** The boundary is in process. The app compiles
the current viewer state with `SurfaceCompiler.compile(model, controller.state)`,
the same plan type it hands to `JavaFxSurfaceBackend.render`. It adds the
physical pixel size (logical size times device scale), the supersample factor
k, and `SurfaceRasterStyle`. No serialisation is needed; `SpikePlanCodec` is
only a test vehicle. The plan carries meshes, final RGBA layers, camera
packets, fitted slots, lighting, clipping and draw passes. The
`SurfaceInspection` snapshot from `ResultSurfaceView.snapshot()` identifies the
view state that produced it.

**Off-gate labelled state.** Today, `SurfaceRenderer.configuration()`
(`SurfaceRenderer.scala:35`) returns legacy JavaFX encoding when the switch is
absent. It returns a refusal when an enabled runtime is not the qualified
one. `ResultSurfaceView.mount` (`ResultSurfaceView.scala:460`) then throws into
the "Unable to render surface" status. The proposal has three parts:

- `SurfaceRenderer` returns a choice: `QualifiedJavaFx(config)` or
  `ReferenceRaster(reason)`. The reason names the failed gate check: OS or
  architecture, pipeline, component origin or hash, or the absent switch.
- For `ReferenceRaster`, `mount` renders `prepared.plan` on its `LatestWork`
  worker. The viewport size times `Screen.outputScaleX` becomes a
  `WritableImage` in an `ImageView`. The pane carries a persistent label such
  as "Reference renderer: not interactive. <reason>." Pick-dependent controls
  are disabled. Navigation actions re-render; that is the slow-interactive
  state if the parallel interpreter is adopted.
- The same choice serves the standalone GIFTI viewer, which also calls
  `SurfaceRenderer.create()`.

Legacy JavaFX encoding has documented colour-gate failures. It should survive
only behind an explicit developer flag.

**Export.** Add `ResultSurfaceView.exportFigure(path, logicalSize, deviceScale, k)`.
Surface it through the existing figure-export pattern:
`TaskResultPane.exportFigure(key)` and `exportSvg` (`TaskResultPane.scala:456`),
with a cortical key that writes PNG. The export uses the raster on every
machine, including gated ones, so figures are identical everywhere. The steps:

1. Compile the current state.
2. Optionally apply `SurfacePublication.decorate`.
3. Render at k times the physical size and box-filter.
4. Optionally apply `SurfacePublication.compose` for the chrome.
5. Encode the PNG and write a receipt beside it.

**Receipt.** It extends `SurfacePublicationReceipt`:

- **interpreter:** id `reference-raster`, plan revision, sequential or
  banded-parallel variant with thread count, and the ScalaFIM and Intaglio
  revisions with patch hash;
- **scene:** a plan content SHA-256 plus camera key, mesh and layer resource
  keys, and timepoint; also the result artifact hash, item, coordinate, the
  display recipe with range, cutoff, palette, missing colour and underlay,
  and the `SurfaceInspection`;
- **output:** logical width and height, device scale, pixel width and height,
  supersample factor, filter (box, round half up), background, pixel
  SHA-256, PNG SHA-256, and publication preset and chrome;
- **timing:** compile, render, filter, encode and total nanoseconds;
- **runtime:** Java version, architecture, available processors and maximum
  heap;
- **gate state:** whether JavaFX was qualified, and the reason if not.

`SurfaceRasterSupersampleSpike` already writes the interpreter, scene digest
and keys, output, timing, digest and runtime parts of this shape.

## Recommendation

1. **Figures.** Adopt `reference-raster` with 2x supersampling as the default.
   Keep 1x only as a draft preview. Use 4x at 1350 wide and below. Beyond
   that, 4x needs tiling or heaps above 4 GiB.
2. **Figure acceptance.** Gate the figure renderer against its own 8x
   reference: interior mean at most 1.0 and p95 at most 3 at 2x, observed
   0.61-0.81 and 2-3. Keep the provider NativeBackend policy against JavaFX as
   a cross-check. Do not use pixelwise two-level agreement with JavaFX.
3. **Off-gate path.** Adopt the labelled non-interactive raster path now. It
   meets the 1 s budget with margin.
4. **Slow interactive.** Defer until the parallel interpreter is productised.
   Recompile and upload costs must also be measured. Single-threaded it
   straddles 250 ms on this contended machine.

## Limitations

- Scenes come from exported plans, not a fresh `.pls-result`, and there are
  only two results. The JavaFX oracle images are the previous worker's.
- Paired per-surface cameras were not rendered on real cortex.
- The legend and title chrome path was not exercised.
- Interactive costs beyond rendering were not measured.
- Timings come from a heavily shared machine. JavaFX device-scale snapshots
  resize the subscene rather than applying an output scale.
- This report has not had an independent review.
