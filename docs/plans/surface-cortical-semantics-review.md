# Cortical surface semantics acceptance

The static fsaverage6 cortical matrix passes **60 native JavaFX cases**: five
map modes, two projections, two lighting modes, and three viewport sizes. Maximum
same-face interior color error is four channel values. One pixel is classified
separately as a native subpixel occlusion difference, with geometric and color
proof recorded below. Cortical retained-update, larger performance, and WebGL
acceptance remain open in the [surface-map plan](surface-map-completion.md).

## Fixture and publication figures

The external, checksum-pinned left fsaverage6 surface has 40,962 vertices and
81,920 faces. Pial, white, and inflated coordinates share the exact ordered
topology. The underlay reads FreeSurfer `lh.sulc`; a separate curvature mode
uses the existing standardized umbrella-Laplacian display estimator. The fixture
rejects changed topology, reordered sample indices, and nonfinite underlay data.

The overlay is deliberately synthetic: `4*sin(y/25)*cos(z/30)`, evaluated at
original pial coordinates in millimetres. Its display window is [-4, 4], its
open hidden interval is (-1, 1), and its opacity is 0.8. Categorical examples
use three synthetic anterior-posterior bands, split at y = -40 and 0 mm.
Face categories use mean face y; nearest categories use original vertex y.
These are display conventions, not anatomical parcel labels.

Five 2400-by-1400 reference publication figures have been rendered and visually
reviewed. They have mapping-derived legends, explicit synthetic-data labels,
and the correct left-lateral orientation mark:

- [Scalar interpolation](../visual-qa/surface-cortical-semantics/Scalar-publication.png)
- [Sulcal-depth underlay](../visual-qa/surface-cortical-semantics/Layered-publication.png)
- [Curvature underlay](../visual-qa/surface-cortical-semantics/CurvatureLayered-publication.png)
- [Nearest vertex samples](../visual-qa/surface-cortical-semantics/Nearest-publication.png)
- [Facewise samples](../visual-qa/surface-cortical-semantics/Face-publication.png)

Matching SVG exports are stored as `.svg.gz` beside the PNGs; gunzip them to
open the SVG. The PNGs were rendered with CairoSVG and inspected for complete
geometry, readable labels, legend separation, and orientation. They show the
portable reference renderer, not native JavaFX acceptance.

## Native findings

The matrix uses orthographic and perspective projections at 384, 768, and
1024 pixels. All five modes exercise both unlit and directional lighting. Antialiasing is
disabled. Native ray intersections retain original face, nearest-vertex, and
barycentric provenance.

The cortical fixture exposed three implementation defects that analytic examples
did not reveal:

1. Flat-color atlas tiles used varying texture coordinates. Minification could
   mix unrelated tiles, producing categorical errors up to 85 channel values.
   Constant-color triangles now use constant texture coordinates, and color
   updates restore varying coordinates when needed. The categorical matrix
   now has zero interior color error. UV uploads are accounted separately.
2. Default orthographic depth handling retained unscaled millimetre coordinates.
   The 384-pixel view clipped nearly the whole cortex. Default and explicit
   depth ranges now use the compiled projection; the silhouette comparison
   improved from approximately 0.009 to 1.0 IoU in that case. Four large-coordinate
   native pick cases supplement the existing 24-case regression.

3. Lighting makes categorical corner colors vary. The legacy atlas then
   reintroduced filtering leakage: nearest-sample interior error reached 26.
   The opt-in backend now approximates the already composed and shaded corner
   colors, preserving legacy operation order. One channel value is reserved
   inside the requested budget for shaded-corner rounding, so lit legacy maps
   require a budget of at least two. Duplicated render corners share geometric
   subdivision edges without merging their colors or scientific IDs. This
   conformity step also closes the cracks exposed by the first implementation.
   The final lit categorical matrix has maximum interior error four. Reviewed
   [nearest-sample native](../visual-qa/surface-cortical-semantics/Nearest-ortho-lit-768-javafx.png)
   and [reference](../visual-qa/surface-cortical-semantics/Nearest-ortho-lit-768-reference.png)
   images, and [facewise native](../visual-qa/surface-cortical-semantics/Face-ortho-lit-768-javafx.png)
   and [reference](../visual-qa/surface-cortical-semantics/Face-ortho-lit-768-reference.png)
   images, show the final shaded displays.

The initial strict 1024-pixel lit curvature comparison found an isolated dark
pixel at (234, 309): native gray 85 versus reference gray 119. Original face 7689
is returned by both the reference and native CPU ray intersection. Rasterizing
the generated mesh gives gray 120. The raw discrepancy of 34 is retained in the
receipt; the geometric coverage test below explains it without increasing the
color budget. The same-face interior comparison remains bounded by four.

The [native image](../visual-qa/surface-cortical-semantics/CurvatureLayered-ortho-lit-1024-javafx.png)
and [reference image](../visual-qa/surface-cortical-semantics/CurvatureLayered-ortho-lit-1024-reference.png)
preserve the raw pixel discrepancy.

## Isolated native subpixel reproduction

A diagnostic pass encodes each generated triangle's ID into its texture without
changing geometry. The actual GPU pixel identifies generated triangle 298238,
original face 10567, with gray 85. That triangle is nearer than the face returned
by the exact ray intersection, but its projected edge misses the exact pixel
center by approximately 0.0006 pixels. Rounding its projected vertices to a
1/256-pixel grid changes coverage to include the pixel.

`JavaFxSubpixelCoverageProbe` reproduces the behavior with just two plain JavaFX
triangles and a two-pixel texture. It uses no surface compiler, geometric
approximation, face atlas, or reference rasterizer. The native ray follows the
analytic triangle-edge test; the native framebuffer follows the quantized
coverage model. Moving the nearer triangle by 0.01 pixels to either side gives
controls away from the ambiguous edge. The three-case receipt records both
coverage decisions. The 1/256 grid describes this observed native configuration;
it is not asserted as a portable guarantee for all graphics drivers.

```sh
sbt 'surfaceViewExamplesJVM/Test/runMain scalafim.surface.view.javafx.JavaFxSubpixelCoverageProbe'
```

The acceptance check now classifies a pixel only when the GPU ID pass identifies
a different, nearer triangle; its exact projected triangle excludes the pixel
center; rounding vertices to the observed grid includes it; and its constant
cell color matches the native pixel within one channel value. Every outlier
above the broad color guard must have this evidence. No fixed list of pixel
coordinates or face IDs is accepted. Same-face interior color checks remain
unchanged, and unlit categorical colors still have a one-channel budget.

The curvature case has one classified pixel. Its largest per-axis vertex shift
is 0.001479 pixels; the native triangle depth is 0.239915 versus reference depth
0.247521, and its native cell-color error is zero. Unit tests reject ordinary
interiors, distant edges, non-nearer triangles, wrong colors, degenerate geometry,
and nonfinite input. The three-case native reproduction independently checks the
classification against actual ray and framebuffer behavior.

## Resource costs and verification limits

With a four-channel approximation budget, the observed derived geometry costs are:

| Display | Derived triangles | Accounted generated bytes |
| --- | ---: | ---: |
| Scalar, unlit | 120,032 | 47,103,744 |
| Sulcal underlay, unlit | 145,992 | 57,257,024 |
| Curvature underlay, unlit | 667,186 | 261,566,480 |
| Sulcal underlay, lit | 1,831,840 | 718,087,424 |
| Scalar, lit | 1,966,074 | 770,701,392 |
| Curvature underlay, lit | 2,340,358 | 917,452,720 |
| Nearest samples, lit | 2,155,148 | 844,841,824 |
| Facewise samples, lit | 1,585,034 | 621,340,880 |

The lit curvature case needs a three-million-triangle limit and a 1 GiB byte
budget. Accounted bytes cover generated output primitive buffers, provenance, and
base atlas textures; JVM object and graphics-driver overhead are additional.
These measurements establish cost, not interactive-performance admission.

Following these changes, `surfaceViewConformance` passed 593 tests,
the example suites passed 17 JVM and 14 Scala.js tests, and the expanded
`surfaceViewVisualQaJVM` passed all four probes. The native ray probe passed
28 cases with 30,479 hits and 49,448 misses; maximum barycentric error was below
9e-8. The earlier separate 24-case depth/resize/restoration probe remains the dated
depth baseline; the current visual alias also exercises native clipping and restoration.

Remaining acceptance includes cortical morph/timepoint/threshold updates and their costs,
the established larger benchmark sizes, actual cortical WebGL checks, and final
integrated gates. The [receipt](../benchmarks/receipts/surface-cortical-coverage-2026-09-08.json) records the final matrix, geometric coverage
evidence, exact source hashes, resource costs, and validation log. The
[earlier strict failures](../benchmarks/receipts/surface-cortical-semantics-2026-09-08.json)
remain archived as historical evidence.

Run the native matrix with the external corpus:

```sh
sbt 'surfaceViewExamplesJVM/Test/runMain scalafim.surface.view.javafx.JavaFxCorticalMapProbe /path/to/fsaverage6 /tmp/cortical-semantics Layered,CurvatureLayered,Nearest,Face,Scalar 3000000'
```

`publication-only` as the fifth argument exports figures without starting JavaFX.
For diagnosis, `native ortho-lit-1024` as the fifth and sixth arguments selects
one camera/lighting/size case. A prefix such as `perspective` selects its six cases. A failing case retains its images and diagnostics
and exits unsuccessfully; it is never added to the passing-case JSON.
