# Paired anatomical surface cameras

This candidate lets each visible surface choose an anatomical viewpoint while
sharing navigation. It fixes the missing representation behind a bilateral
viewer showing left lateral and right medial when the intention is two lateral
faces. It changes display transforms, never scientific positions, normals,
sampling coordinates, topology or identities.

```scala
val viewpoints = Map(
  leftId -> SurfaceViewpoint.Lateral(CorticalHemisphere.Left),
  rightId -> SurfaceViewpoint.Lateral(CorticalHemisphere.Right)
)
val paired = SurfaceViewer.reduce(model, state,
  SurfaceViewerAction.SetSurfaceViewpoints(viewpoints))
val fitted = paired.flatMap(value => SurfaceViewer.reduce(model, value,
  SurfaceViewerAction.FitCamera))
```

Use `Medial` for paired medial views. The map is replaced atomically and every
key must identify a model surface. Unmentioned surfaces inherit the shared
camera viewpoint. Viewpoints are independent of slot order and survive focus
changes that hide a surface.

Projection, zoom, aspect, world-coordinate pan and orbit remain shared. Orbit
retains the existing world-Z yaw and local-right pitch convention relative to
each anatomical viewpoint. `ResetCamera` resets shared pan/orbit/zoom, preserving
projection, aspect and anatomical viewpoints. `SetViewpoint` explicitly returns
to one common anatomical viewpoint and clears all overrides. There are no
independent per-surface zoom/pan/orbit actions in this policy. A later UI may
choose another explicit navigation policy; it must not silently change this one.

`FitCamera` contains the displayed bounds under every effective camera, with
one shared projection and physical scale. Ordinary navigation and geometry
updates do not trigger an implicit fit. Render-plan revision 8 adds
`surfaceCameras` and `cameraFor(surface)`. Camera packets must reference visible
surfaces, contain finite 4x4 matrices and share a projection. Camera keys are
stable by value and include the visible per-surface orientations; recoloring
alone does not change those keys or the mesh resources. Buffer receipts include
the added camera matrices, including after network attachment.

The JavaFX display transform is `G^-1 L S`, where `G` is the existing shared
camera transform, `L` is viewport placement and `S` is the surface camera. The
existing native mesh pick path therefore keeps its original coordinates.
Three selects the effective camera for each draw and ray construction, computes
the shared depth interval using the effective views, and clears overrides on a
return to the common camera. Older custom Three runtimes refuse the new
capability before mutation. Reference raster projection and pick buffers use
the same effective camera.

Scene-document revision 7 saves the viewpoint map and camera aspect ratio. The
latter was previously omitted from serialization and restoration. Revisions 1–6
remain readable and encode their original field sets; earlier revisions refuse
new state they cannot represent. Camera capabilities are inferred from saved
viewpoint maps, so an incapable renderer cannot admit a paired document merely
because `requiredFeatures` omitted the new feature.

## Qualification boundary

The candidate is based on recovery commit
`066be6c11960de26a8825811f12241af6e5120a1`, plus the exact 16-file retained-atlas
snapshot. It is isolated from ScalaFIM main and from the app's admitted backport.
Its build uses the existing Intaglio override at
`55658abfbbfed0c9a36ab612b38bd8f0677bc158`; the default dependency pin is not
qualified by these results. No scientific scalar/fragment contract is adopted
in PLS Neuro by this candidate.

Shared tests check anatomical directions, original mesh buffers and IDs, shared
navigation, focus/reset, invalid-input refusal, fitted margins/aspect under both
projections and orbit, stable camera keys, and old/new document restoration.
The two-sided raster fixture independently distinguishes outer and inner faces
in both hemispheres. Its old-global-camera negative control exposes the wrong
right face. JavaFX affine-transform checks compare pixel coordinates at wide,
tall and doubled dimensions under both projections and orbit. Three command and
legacy-runtime tests cover camera propagation, clearing, reuse and pre-mutation
refusal. JVM and Scala.js runs are recorded separately in the consumer receipt.

The next qualification uses the same asymmetric two-sided fixture on retained
native backends. Seventeen sequential stages cover paired lateral/medial,
orbit, portrait resize, slot order, white geometry, morphing, frame and opacity
updates, fit, single-surface focus, return to bilateral, reset, global viewpoint,
restored paired viewpoints, complete near/far clipping and restoration.

| Native runtime | Stages | Original-ID hits | Misses | Maximum sampled channel error | Maximum barycentric error |
| --- | ---: | ---: | ---: | ---: | ---: |
| JavaFX 24.0.1 ES2 | 68 | 44,354 | 255,914 | 0 | 3.984e-7 |
| Three r185, Chromium 151, ANGLE Metal | 136 | 88,708 | 511,828 | 0 | 2.980e-8 |

JavaFX runs orthographic/perspective with disabled/balanced antialiasing, using
the existing qualified no-mipmap/centroid compatibility component. A hidden
`SubScene` is snapshotted without constructing or showing a `Stage`. Test-only
reflection into `SubScene.pickRootSG` constructs the native camera ray and
traverses actual meshes; the ordinary controller resolves original identities.
The test-only `--add-opens` is not a new production runtime requirement. Runtime
classpaths are frozen and verified before and after execution.

WebGL runs both projections, actual 0/4-sample antialiasing and logical-to-device
ratios 1/2 on Apple M3 Max through ANGLE Metal. It reads the native framebuffer
and calls the normal backend ray-pick API. Every context and the owned browser
are closed; the final process audit is clean. JavaFX device-scale variation and
OS-input interaction are not established by this harness.

Both compare against the independent reference raster's expected face, vertex,
barycentric coordinates and color. The initial outer/inner face identities also
have an analytic check. Hit samples are safely inside one visible face and avoid
nearest-vertex ties; miss samples have an empty 5x5 neighborhood. This is a
camera, visibility and picking gate with constant face colors, not a substitute
for the separate affine-atlas edge-color oracle or a lighting qualification.

Camera-only updates and recoloring reuse geometry in the measured Three path.
Reordering still uploads both meshes; focus uploads the visible mesh and return
to bilateral uploads both. Thus layout transitions do not yet meet the intended
resource-reuse behavior. A horizontal pair also wastes space in portrait panes.
Keep those defects open alongside partial depth/clipping, native save/reopen,
curved cortex, lighting, end-to-end latency and actual app adoption. Unit tests
already cover scene-document restoration; native save/reopen is a separate gate.
No standard-template or final cortical-appearance claim follows from this fixture.

The consumer archive `output/brain-display/paired-cameras-32` contains exact probe
sources, the linked browser bundle, native/reference images, reference queries,
runtime receipts, measurements and the incremental patch over candidate 31.
The implementation files that passed the prior 400 tests are unchanged. Only
these two inert probes and this QA record are added or updated in this slice.

## Responsive packing increment (10 September 2026)

The recovered paired-camera and retained-atlas candidate is preserved as
`6a7e17beb42c5d056e2596809be627cca8f026b5` on the local
`surface/paired-responsive-20260910` branch. This preservation commit does not
establish upstream landing or a new runtime qualification.

Bilateral compilation now selects `SurfaceViewportFit.Pack`, which maximizes
common tile size across ordered rows and columns at the camera's physical aspect
ratio. Portrait panes can stack the hemispheres; wide panes retain a horizontal
pair. Single views retain the existing `Contain` behavior. `Fill` and explicit
whole-group `Contain` remain available to callers. Packing changes only viewport
rectangles: camera fit, anatomical margins, mesh positions, scientific identities
and per-surface viewpoints are retained. All backends consume the shared resolver
again on resize.

The independent layout fixtures check exact portrait/wide results, the analytic
maximum for two tiles, physical aspect, non-overlap, centering, reordered identity,
partial rows and single-view focus. Camera fixtures retain their projected-corner
containment and equal-scale checks. New JVM/JS execution, native framebuffer/pick
checks and real cortical consumer review must qualify this increment separately
from the archived paired-camera evidence. These gates were pending at the
implementation commit; the following section records the subsequent execution.


### Responsive candidate qualification

Fresh detached checkout `618e03cf52ebcb9bd4a4930d406867d040217653` passed
412 tests: surface-view 140 on each platform, raster 29 on each platform,
JavaFX 46 and Three.js 28. The explicit Intaglio override remains
`55658abfbbfed0c9a36ab612b38bd8f0677bc158`; all 106 tracked files in that source
snapshot were compared with the exact Git blobs before execution. The ordinary
Intaglio pin is not qualified by this override.

The frozen JavaFX 24.0.1 ES2 compatibility runtime passed all 68 hidden native
stages: 47,210 ray hits and 251,058 misses, maximum tested interior channel error
0, and maximum barycentric error 2.960806659846327e-7. These cover the two
projections and antialiasing modes through paired views, portrait resize,
reordering, geometry morph, frame/opacity changes, fit/focus/reset, and full
clip/restore. No Stage or OS input was used. The portrait comparison was visually
inspected: both ordered triangle fixtures now occupy stacked, substantially
larger viewports instead of a small centered horizontal row.

Evidence is retained in the PLS Neuro workspace under
`output/brain-display/paired-cameras-37/evidence.json`, with source hashes,
test reports, build logs, frozen-runtime input hashes and native images/rays.
The native-frame archive SHA-256 is
`72a6d36d3ffad48c72462492854851f8ea8972676983281df7940e0d14b86118`.

This is a synthetic camera/layout/pick gate with constant face colors and
interior sampling. New WebGL framebuffer execution, curved cortical color and
lighting review, normal interaction latency, native save/reopen and partial
clipping remain separate. The consumer backport must retain its admitted color
semantics: the native branch also contains retained-atlas/face/scalar features
that are not implicitly admitted by paired-camera adoption. Neither canonical
ScalaFIM main nor the PLS Neuro provider patch was updated by this increment.
