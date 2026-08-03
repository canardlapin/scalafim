# Geodesic cortical reveal lens parity plate

This gate renders a local pial-to-inflated cortical reveal through JavaFX
Scene3D and Scala.js/Three.js. It demonstrates a display operation that a flat
surface viewer cannot provide: open one folded neighborhood to expose its
topology while thresholded scientific data, selection, and vertex identity
remain attached.

## Shared contract

- Corpus pial: `lh.pial.gii`, SHA-256
  `b425b4914362af4aaf43bb5d022afd39a0c5f6ec4601e353631cdd737884c951`
- Corpus inflated: `lh.inflated.gii`, SHA-256
  `5a6d3f1fc87ab588133bc1656ef2e30d8cdbd89a45effeba6a3bf210433f0520`
- Geometry: left fsaverage5, 10,242 vertices and 20,480 ordered faces
- View: left lateral `(-1, 0, 0)`, orthographic scale 105, unlit
- Camera: canonical pial center with scale-aware eye distance outside the
  world-space bounding sphere
- Center: vertex 3074, selected deterministically as a strongly displaced,
  lateral, suprathreshold vertex
- Lens: spatially fixed pin, 12 mm aligned-target core, 64-step discrete
  harmonic collar, and fixed outer boundary at 48 mm
- Active neighborhood: 900 vertices
- Stages: folded `0.00`, opening `0.35`, reveal `0.70`, full lens `1.00`
- Underlay: orientation-invariant umbrella-Laplacian curvature
- Overlay: fixed-seed topology-smoothed Gaussian data, display window `[-4,4]`
- Threshold: the open interval `(-2.33, 2.33)` is transparent; both tails and
  exact boundaries remain visible
- Threshold tails: 115 negative and 88 positive vertices
- Raster dimensions: 600 by 450 pixels per stage

The geodesic mask and natural deformation are computed once when the lens is
pinned. Target translation at the pin is removed, the core retains aligned
target displacements, and the collar solves the graph Laplace equation between
the core and fixed exterior. Stage compilation reuses that immutable primitive
field. Every stage must retain the exact topology key, layer keys, camera key,
selected vertex, and layer values; only the geometry revision changes.

The final field has zero triangle inversions over the complete continuous
animation interval (checked from each triangle's exact quadratic orientation),
minimum/maximum full-opening triangle-area ratios `0.0403/1.9541`, maximum edge
strain `0.6572`, and p95 edge strain `0.0454`. The high maximum is deliberately
reported rather than hidden; the 95th percentile shows that the strong strain
is localized. Warm and cool projected rings distinguish the fully constrained
core from the fixed outer boundary, while a pin marker makes the spatial anchor
visible.

## Acceptance checks

Both live backends are compared with the same deterministic CPU raster for all
four stages. Each stage requires at least 8,000 foreground pixels, mask IoU at
least 0.90, centroid distance at most 3 pixels, mean interior channel error at
most 40, and a direct-orientation advantage of more than 3 channels over
horizontal flip, vertical flip, and 180-degree rotation alternatives.

The captured natural-lens run is substantially tighter: JavaFX and WebGL both
exceed 0.995 mask IoU; centroid distances stay below 0.05 pixels; mean interior
channel error is at most 1.221 for JavaFX and below 0.464 for WebGL;
orientation margins remain above 40 channels. JavaFX performs one 245,808-byte
position/normal update and zero atlas updates for every nonzero stage. Warm
JavaFX geometry updates settle to `0.17 ms` in this run after a `1.47 ms` first
update. The one-time fixture build, which includes Dijkstra, the harmonic solve,
quality analysis, overlay preparation, and four plan compilations, measured
`0.65 s` on the JVM and `0.37 s` in Scala.js. The browser presentation uses four
independent retained runtimes so all stages remain visible at once; each canvas
therefore records one cold geometry and color upload.

The plate also guards a failure mode that ordinary silhouette thresholds did
not expose reliably: a fixed four-unit camera eye can lie inside a
millimetre-space cortex and display its medial wall from a nominal lateral
view. The canonical bounding-sphere camera and live lateral plates make that
anatomical contract explicit.

## Reproduce

Compile and run the portable gates on both platforms:

```bash
env COURSIER_CACHE="$PWD/.coursier-cache" sbt \
  -Dsbt.boot.directory="$PWD/.sbt-boot" \
  -Dsbt.global.base="$PWD/.sbt-global" \
  -Dsbt.ivy.home="$PWD/.ivy2" \
  'surfaceViewJVM/test' \
  'surfaceViewJS/test' \
  'surfaceViewExamplesJVM/test' \
  'surfaceViewExamplesJS/test' \
  'surfaceViewExamplesJS/fastLinkJS'
```

Run the native plate with a live display:

```bash
sbt 'surfaceViewExamplesJVM/Test/runMain \
  scalafim.surface.view.javafx.JavaFxCorticalLensProbe \
  /private/tmp/scalafim-surface-lens-javafx.png'
```

Serve `/Users/bbuchsbaum/code` over HTTP and open:

```text
http://127.0.0.1:8000/scala/scalafim/examples/surface-view/browser/lens.html
```

The page sets `data-scalafim-cortical-lens="pass"` only after the four live
canvases pass structural, threshold-tail, locality, orientation, and pixel
checks with no runtime errors.
