# Native affine-atlas color gate

This optional JavaFX gate compares native pixels with the original triangle
geometry and opaque vertex colors. It does not read native atlas UVs, tiles,
midpoint weights or the reference surface rasterizer. `plan.json` fixes the
fixtures, controls and two-channel-level budget before the recorded runs.

The Scala test main `scalafim.surface.view.javafx.JavaFxAffineColorProbe` writes
48 PNG/binary pairs for one encoding: diagonal and RGB triangles, two dense
ramps, front/oblique geometry, original/cyclic/shuffled faces, and AA off/on.
The binary stores original positions, colors, indices and outer boundary.
The planar fixtures use known parallel-camera pixel coordinates, isolating
atlas lowering from application camera fitting. No Stage is shown.

## Generate fixtures

Compile `surfaceViewJavafxJVM/Test/compile`. The recorded qualification uses
exact Intaglio `55658abfbbfed0c9a36ab612b38bd8f0677bc158` via the existing
`scalafim.intaglio.build` override because the current default pin is
incoherent with existing surface APIs. This gate does not fix that pin.

Run the test main in a separate JVM with the intended JavaFX runtime. For the
recorded JavaFX 24.0.1 macOS ES2 positive runs, use the externally supplied,
matching no-mipmap/centroid compatibility JAR. This repository does not ship
or enable that runtime patch. The classpath must contain the compiled test,
main and provider dependencies, excluding the ordinary build's JavaFX JARs:

```sh
java --module-path "$SCALAFIM_FX24_MODULES" \
  --add-modules javafx.graphics \
  --enable-native-access=javafx.graphics \
  --patch-module "javafx.graphics=$SCALAFIM_FX24_COMPAT_JAR" \
  -Dprism.order=es2 \
  "-Dprobe.expectedOrigin=$SCALAFIM_FX24_COMPAT_URL" \
  -cp "$SCALAFIM_TEST_CLASSPATH" \
  scalafim.surface.view.javafx.JavaFxAffineColorProbe \
  /tmp/affine-color/AdaptiveAffineOpaque-patched AdaptiveAffineOpaque
```

The expected origin is the exact `CodeSource.getLocation.toString` URL of the
patch JAR (for example `file:/absolute/path/compat.jar`). Both material and
shader origins are asserted. Repeat for `AffineMidpointOpaque` and
`LegacyTriangle`. The latter is a required failing encoding control, even
with the corrected sampler. For the stock sampler control, omit
`--patch-module`, set the expected origin to the selected graphics JAR URL,
and generate `AdaptiveAffineOpaque-stock`. Use separate output directories.
A successful native process only certifies fixture generation; the pixel
oracle must still pass. Reject renderer errors even when the process exits 0.

## Check the pixels

The Python tools require NumPy and Pillow. Given the four generated folders:

```sh
python3 tools/surface-view/javafx-affine-color/check-oracle.py \
  /tmp/affine-color/AdaptiveAffineOpaque-patched
python3 tools/surface-view/javafx-affine-color/oracle.py \
  /tmp/affine-color/AdaptiveAffineOpaque-patched
python3 tools/surface-view/javafx-affine-color/oracle.py \
  /tmp/affine-color/AffineMidpointOpaque-patched
```

`oracle.py` writes `color-report.json` and exits 0 only if all 48 fixtures and
all 32 permutation comparisons pass. The legacy-encoding control must fail
its diagonal cases; stock JavaFX must fail its dense-ramp cases. Their oracle
commands must exit 1, and their named failures must be inspected explicitly.
An unrelated failure does not establish a discriminating control.

`check-oracle.py` checks the generic triangle/pixel reference against separate
closed-form diagonal, RGB and linear-ramp fields; its residual is below
`6e-14` in the recorded run. Cached geometric references are written beside
the fixture folders in `oracle-cache`, keyed by the complete input binary,
oracle source hash and NumPy version. Algorithm changes invalidate old entries.

## Meaning and limits of the gate

The oracle enumerates vertices of each original triangle intersected with a
pixel square: contained triangle vertices, contained pixel corners and edge
intersections. Affine color extrema occur at these vertices. It combines all
contributing faces and checks all pixel squares strictly inside the outer
quadrilateral. Internal edges are included and reported separately. Partial
outer silhouette/background coverage is excluded.

With MSAA, the footprint bounds allow covered interpolation locations and
sample mixtures. They are a conservative envelope, not an exact sample-layout
prediction. Without AA, the gate also compares pixel-center barycentric
colors. The prospective tolerance of two 8-bit channel levels covers the
integration's quantization budget; it is an empirical acceptance threshold,
not a universal hardware theorem. Cyclic and packing-equivalent images also
must agree within two levels.

On JavaFX 24.0.1 macOS ES2, both corrected encodings pass all 48 fixtures and
32 permutation checks. Their worst footprint excess is 1.223 levels and
worst non-AA center error is 1.452. Legacy encoding reaches 21.483 center
error and 21 levels of permutation dependence. Stock JavaFX reaches 17.625
footprint excess and 18 levels of permutation dependence.

These gates do not qualify lighting, actual device scaling, perspective,
cortical occlusion, multilayer alpha, fragment-scalar semantics, ray picking
or packaging. Dense cortical resource/appearance evidence is separate. Full
source/runtime hashes, failed controls and screenshots are retained in the
PLS Neuro consumer's `output/brain-display/gpu-color-23` evidence bundle.

## Production-width and lighting extension

`production-plan.json` extends the frozen unlit gate without changing its
two-level budget. The Scala main accepts two additional optional arguments:
`maxTextureSize` (default 256) and `Unlit`, `Default`, or `Soft` (default Unlit).
For example append `4092 Soft` after the encoding argument above, using a
new output directory. Run all six combinations of 4096/4092 and the three
lighting settings, plus the original unlit 256 case and the two 4096/Unlit
negative controls. Each invocation still writes 48 PNG/binary pairs.

`fixture-config.json` records the selected settings. Binary version 23 remains
unchanged for unlit output. Lit version 24 appends original Float vertex
normals and five Double values (ambient, diffuse, direction x/y/z) after the
original triangle indices. The independent Python oracle derives rounded
shaded original vertex RGB from those inputs; it does not consume native
composited colors. The same `oracle.py <folder>` command checks each mode.
`check-oracle.py` checks the unlit closed-form fields; run it on an Unlit
folder, not on the varying-normal fixtures.

All 288 adaptive cases and 192 permutation comparisons pass on the recorded
patched runtime. Widths are genuinely 4096 and 4092 for the dense cases, with
heights 128 and 132. This is not a maximum-area or device-scale sweep. Both
required negative controls fail their named criteria. The original 256-wide
unlit PNGs and binaries remain byte-identical. See
[the qualification report](../../../docs/visual-qa/javafx-affine-atlas.md) for
metrics and the remaining native UV-update performance limit. Soft lighting
is a QA comparison, not a changed public or consumer default.
