# Mapping, linked views, and publication evidence

This is bounded P4/P5 evidence, not a pass for the frozen 151-checkpoint matrix.
Open [the gallery](index.html) for paired images and retained failures.

The candidate adds explicit display-sRGB/linear-light interpolation, reference
curvature controls, compatible threshold-edge overlays, discrete palettes and
plus-lighter blending, bivariate effect/confidence keys, world clipping, and
ordered translucent raster composition. Canvas and JavaFX raster hosts preserve
image/pick coherence. Flat-map hosts share scientific vertex selection and retain
map-space polygon/lasso outlines through resizing. Playback documents preserve
fractional time and source bindings; composition figures share the displayed
layers' mappings and selection anchors.

| Evidence | Observed result | Limit |
| --- | --- | --- |
| Sixteen mapping/visibility specimens | All fixed central-color regions pass; native full-frame 15/16 pass | Mixed-gradient native max channel error 3 versus limit 2. Raster lacks antialiasing and fails the strict silhouette gate. |
| Four alpha overlap/reorder/zero-alpha specimens | Raster colors, ordering and picks pass | Native Three.js translucency returns a typed refusal; this is the explicit raster-host route. |
| Actual JavaFX visibility snapshots | Twenty images equal their published raster pixels | Validates actual host publication, not native Scene3D or cortical performance. |
| Browser flat-map sequence | Seven stages pixel-identical to SurfViewJS; polygon/lasso/selection/time/resize checks pass | The reference fill is uniform, not scalar-painted. |
| JavaFX flat-map sequence | Actual interaction assertions pass; silhouettes agree closely | Strict pixel comparisons fail: Canvas/JavaFX text rasterization and color rounding differ. |
| Linked four-view browser sequence | Eight checkpoints: silhouette IoU 1, max channel error 1; restore is pixel-exact | Synthetic plane and expansion, not cortical inflation or performance qualification. |
| Actual JVM linked sequence | ImageView snapshots equal published raster; known vertex pick, time/morph/selection, nondefault camera restore and actual resize-back pass | Confirms host integration, not an independent oracle for the shared raster/compiler. |
| Publication | Real 1200×900 and 2400×1800 figures, transparent background, four markers, shared scalar/bivariate keys | Renderer uses 96/192 PPI for identical physical layout; PNG files do not embed a resolution metadata chunk. |

The linked replay uses real Three.js and Canvas hosts. Flat-map selection changes
all four markers; a 3D pick changes the flat-map selection. Interpolated playback,
pause, morph endpoints/midpoint, a nondefault camera pan, save/restore and resume
are exercised. Restoration renders the saved navigation, including its camera
links. Temporal readouts are checked directly from fixture numbers. The reference
uses a 256-entry sampled palette; the candidate's continuous ramp differs by at
most one output byte at these checkpoints.

The linked replay creates four marker packets. After its intentional pan, the
first selected vertex lies outside that viewport, so only three markers remain
visible in the endpoint/restored/resumed plates. The publication specimens use
a different selection and show all four markers. Figure panel labels describe
the synthetic plane views; they do not imply anatomical frontal/inferior views.

The figure previews ending in `-white.jpg` composite the original transparent PNG
over white for inspection. The original PNGs remain unmodified. Their receipts
record mapping keys and requested dimensions. `figures/export-checks.json`
checks transparent corners and all four exact-color selection glyphs at both
scales; glyph diameter doubles from approximately 10 to 20 output pixels.

## Reproduction

Candidate/provider revisions and source hashes are recorded in
`source-binding.json`. The upstream Intaglio candidate is
`13747a3a8bf75fd788ff68e1c979d6c84f740605`. Following explicit user approval,
that reviewed commit was published to `canardlapin/intaglio` main and its remote
SHA verified. The original captures and gates use the explicit
`-Dscalafim.intaglio.build=/private/tmp/intaglio-surface-display` override;
publication does not change that historical provenance. The separate
`publication-verification.json` receipt records verification through the normal
remote pin, with the local override absent.

After compiling `surfaceViewThreeJS/fastLinkJS`, run:

```sh
node tools/surface-view/parity/run-visibility-proof.mjs \
  --candidate-root "$PWD" \
  --scalafim-module "$PWD/modules/surface-view-three/js/target/scala-3.7.4/scalafim-surface-view-three-fastopt/main.js" \
  --out /private/tmp/visibility-proof

node tools/surface-view/parity/run-flatmap-proof.mjs \
  --candidate-root "$PWD" \
  --scalafim-module "$PWD/modules/surface-view-three/js/target/scala-3.7.4/scalafim-surface-view-three-fastopt/main.js" \
  --reference-bundle /private/tmp/visibility-proof/reference-source-build/reference.mjs \
  --out /private/tmp/flatmap-proof

node tools/surface-view/parity/run-linked-proof.mjs \
  --candidate-root "$PWD" \
  --scalafim-module "$PWD/modules/surface-view-three/js/target/scala-3.7.4/scalafim-surface-view-three-fastopt/main.js" \
  --reference-bundle /private/tmp/visibility-proof/reference-source-build/reference.mjs \
  --out /private/tmp/linked-proof
```

The runners use the installed SurfViewJS Playwright browser with ANGLE Metal,
audit browser ownership, and close their contexts/browser/server. They do not
attach to the user's Chrome profile. The local reference checkout is pinned in
provenance, including its bundle hash. Browser module/bundle caches are omitted
from this evidence directory.

The actual JVM probes are `JavaFxVisibilityProbe`, `JavaFxFlatMapProbe`, `JavaFxLinkedViewProbe`, and
`SurfaceFigurePublicationProbe` (the last in `surfaceViewExamplesJVM/Test`).
Raw gate logs and actual exit codes are retained under `verification/`.

## Adverse evidence and review

The earlier linked reference mistakenly supplied two palette entries as color
stops. SurfViewJS treats those as a discrete lookup, producing a nearly solid
blue field. That run remains under `adverse/scalafim-linked-proof-34`; correcting
the reference input did not change tolerances. Earlier orbit-input mismatch,
compile failures and their corrected reruns are also retained. The surviving
native mixed-gradient, raster antialiasing and JVM flat-map text differences
remain visible in the current results.
The final lasso capture also has overlapping, partly clipped ROI labels on both
the reference and candidate. Pixel agreement does not remove that shared
legibility limitation.

Independent source review found and prompted fixes for a stale temporal cache
on restore, legends bound through legacy layout instead of displayed instances,
and a replay that discarded restored navigation. Regression tests and the
corrected actual replay cover those cases. Review scope and exact bindings are
recorded in `review.json`.
