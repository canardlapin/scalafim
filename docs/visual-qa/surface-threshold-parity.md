# Thresholded cortical surface parity plate

This gate renders one checksum-pinned `fsaverage5` left pial mesh through the
JavaFX Scene3D and Scala.js/Three.js backends. It exists to make anatomical
orientation, overlay thresholding, layer composition, and camera-only resource
reuse directly inspectable instead of relying on a semantic unit test alone.

## Shared contract

- Corpus: `surfviewjs/tests/data/fsaverage5-lh-pial.gii`
- SHA-256: `b425b4914362af4aaf43bb5d022afd39a0c5f6ec4601e353631cdd737884c951`
- Geometry: 10,242 vertices and 20,480 faces
- Views, in plate order: ventral `(0, 0, -1)`, left lateral `(-1, 0, 0)`,
  posterior `(0, -1, 0)`, and dorsal/top `(0, 0, 1)`
- Projection: orthographic, scale 105
- Lighting: unlit
- Underlay: orientation-invariant signed umbrella-Laplacian curvature,
  standardized and clipped to the conventional `[-1, 1]` gray window
- Overlay: fixed-seed xorshift32/Box-Muller Gaussian data, five rounds of
  topology-neighbor smoothing, then population standardization
- Seed: `324508639` (`0x13579bdf`)
- Display window: `[-4, 4]`, blue-to-red
- Threshold: values strictly inside `(-2.33, 2.33)` are transparent; both
  boundary values and both tails remain visible
- Raster dimensions: 600 by 450 pixels per view

The portable fixture compiles four plans from one model and state. The mesh and
layer resource keys must remain identical across plans; only the camera key may
change.

## Acceptance checks

Each live backend is compared with the deterministic raster backend for every
view. The hard structural gate requires at least 8,000 foreground pixels,
foreground-mask IoU at least 0.90, and centroid distance at most 3 pixels. Mean
interior RGB error must be at most 40 channels. In addition, the direct raster
must beat horizontal flip, vertical flip, and 180-degree rotation alternatives
by more than 3 mean channels. This texture-sensitive orientation margin catches
an inside/outside or upside-down camera even when the cortical silhouette is
too symmetric for mask IoU alone. JavaFX self-illumination textures and WebGL
output undergo different color-space conversion, so exact colorimetry is still
diagnostic; the plates remain the final visual color QA.

After the cold ventral render, the three camera-only JavaFX plans must perform
zero geometry and zero layer uploads. The browser creates one renderer per
canvas so all four views remain visible together; its upload counts are
therefore cold-load counts for each canvas.

## Reproduce

Use the repository-local dependency caches in restricted environments:

```bash
env COURSIER_CACHE="$PWD/.coursier-cache" sbt \
  -Dsbt.boot.directory="$PWD/.sbt-boot" \
  -Dsbt.global.base="$PWD/.sbt-global" \
  -Dsbt.ivy.home="$PWD/.ivy2" \
  'surfaceViewExamplesJVM/test' \
  'surfaceViewExamplesJS/test' \
  'surfaceViewExamplesJS/fastLinkJS'
```

Run the JavaFX probe with a live display:

```bash
sbt 'surfaceViewExamplesJVM/Test/runMain \
  scalafim.surface.view.javafx.JavaFxSurfaceThresholdParityProbe \
  /private/tmp/scalafim-surface-threshold-javafx.png'
```

Serve `/Users/bbuchsbaum/code` over HTTP and open:

```text
http://127.0.0.1:8000/scala/scalafim/examples/surface-view/browser/threshold-parity.html
```

The browser page sets `data-scalafim-threshold-parity="pass"` only after all
four native canvases pass the structural comparison and the exact threshold and
tail contracts.
