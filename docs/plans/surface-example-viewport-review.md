# Example viewport checksum diagnosis

The old example checksum described `Fill` viewports: two logical square
hemisphere panels were stretched across a 320×180 canvas. The earlier
aspect-preserving compiler instead centers two 160×160 panels, leaving ten
pixels above and below. The stale expected checksum was unrelated to scalar,
facewise, or legend mapping changes.

The diagnostic changes only `plan.viewportFit` to `SurfaceViewportFit.Fill`.
Every mesh, camera matrix, layer color, opacity, threshold, and scientific ID
stays identical. This reproduces the old checksum and shaded-pixel count exactly:

| Viewport policy | Image checksum | Shaded pixels |
| --- | ---: | ---: |
| Current aspect-preserving default | -748488703 | 11342 |
| Explicit legacy `Fill` | 895258625 | 12720 |

An independent geometric oracle checks all 57,600 pixels for each policy.
The unit triangle projects to normalized limits ±2/3. In each 160-pixel-wide
half, the contained mask has integer pixel indices `x=27..132`, `y=37..142`,
and `y >= x+10`. The stretched mask has `y=30..149` and its diagonal obeys
`8*(2*y+1) >= 9*(2*x+1)`, using doubled pixel-center coordinates. Both masks
match the exported PNGs exactly, independently of rasterizer traversal and
edge-function implementation.

![Current square viewport result](../visual-qa/surface-example-viewport/contained.png)

![Explicit stretched viewport result](../visual-qa/surface-example-viewport/legacy-fill.png)

The shared regression now checks the default receipt, retains the old checksum
for explicit `Fill`, and checks both complete analytic masks on JVM and
Scala.js. No production rendering code or fixture values changed for this fix.

Reproduce the diagnostic artifacts with:

```sh
sbt 'surfaceViewExamplesJVM/Test/runMain scalafim.examples.surfaceview.SurfaceExampleViewportProbe target/surface-example-viewport'
```

The [receipt](../benchmarks/receipts/surface-example-viewport-2026-09-07.json)
records exact source/artifact hashes and the final example-suite results.
