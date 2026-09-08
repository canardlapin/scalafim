# JavaFX native ray-picking acceptance

The native intersection probe passes 24 analytic cases on OpenJFX 21.0.5,
macOS arm64, JDK 25.0.1. It checks 25,061 surface hits and 46,668 misses;
2,886 misses are inside the original surface but removed by near/far clipping.
Every checked hit retains the original surface, face, and nearest-vertex IDs.
The maximum difference from reference barycentric coordinates is
`8.856300026671704e-8`, below the declared `1e-5` budget.

`JavaFxNativePick` is a test-only reflection adapter to
`SubScene.pickRootSG(double, double)`. The installed OpenJFX bytecode confirms
that this method computes the effective camera's pick ray, traverses the scene
root, and returns the native intersection chooser's result. The probe supplies
screen coordinates, not an intersected node, face, or point. Production code
does not depend on this private API. This is native ray-intersection evidence;
it does not test operating-system mouse-event delivery. Other OpenJFX versions
or module-path execution require separate validation of the test adapter.

The reference rasterizer supplies original-mesh IDs and perspective-correct
barycentric coordinates. Sampling excludes original-face edges, clipping edges,
and nearest-vertex ties using explicit weight margins and a 3-by-3 neighborhood.
Background and clipped-region misses are checked separately. Exact seam/tie
rules remain the responsibility of the shared semantic suites.

Coverage comprises face-constant, nearest-sample, and bounded scalar modes at
96 and 192 pixels under orthographic and oblique perspective projection.
The scalar cases deliberately use a 64-pixel atlas limit to exercise chunk-local
intersections; one case checks picks across 445 distinct chunks. Twelve further
cases keep the same backend and controller while changing timepoint, threshold,
geometry morph fraction, near clipping, far clipping, and then restoring the
depth range while resizing to 256-by-192. Morph and scalar updates therefore
exercise native intersections after the derived mesh is rebuilt.

Run this probe directly:

```sh
sbt 'surfaceViewJavafxJVM/Test/runMain scalafim.surface.view.javafx.JavaFxNativePickProbe'
```

It is also included in `surfaceViewVisualQaJVM`. The expanded alias passes all
four probes, and `surfaceViewJavafxJVM/test` passes all 22 tests. No shared or
Scala.js implementation changed in this slice. The
[receipt](../benchmarks/receipts/surface-native-picking-2026-09-08.json) records
individual cases, source hashes, and the validation log. These analytic cases
close the constructed-pick evidence gap; cortical-scale feature and performance
acceptance remains a separate requirement in the
[completion audit](surface-map-completion.md).
