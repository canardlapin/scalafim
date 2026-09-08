# Three.js fragment shader implementation

The Three.js adapter now evaluates inspectable scalar mappings and ordered mixed
layers in a fragment shader. Scalar values interpolate before visibility,
split-gap handling, ramp evaluation, opacity, and blending. World-space lighting
uses the normalized interpolated normal after composition. Legacy all-vertex
color scenes retain their earlier renderer.

Scalar samples use an affine encoding over the complete finite envelope of
covered, referenced data and mapping boundaries. Unused vertices and zero padding
for appended network geometry cannot dilute a narrow scalar range's precision.
Vertex values are not clamped to display limits. Native
high-precision floats are still finite precision: mappings whose distinct
boundaries collapse in that encoding are rejected before uploads. Nonfinite
contributing samples carry a separate interpolated validity flag. Scientific
layer coverage excludes appended network faces independently of invalid colors.

Fragment meshes use fixed per-face render corners, preserving original face and
vertex IDs. Nearest-sample scenes retain their existing fixed partition and
original-triangle raycasting proxy. Scalar, threshold, and opacity changes update
attributes/materials without geometric subdivision. Camera-only updates reuse
all layer buffers. Morphs update positions and normals without replacing topology.

Shaders operate on unlit sRGB display bytes and round at the same layer and
composition boundaries as the portable evaluator. They use `ShaderMaterial`
without tone mapping or an additional output color conversion. Attributes are
reused on data changes; unused attribute slots remain owned by their geometry
until disposal. The renderer checks native attribute/varying limits before any
commands are applied. The implementation permits at most eight layers and 1,024
ramp stops per layer, subject to the actual device limits.

The first native matrix covers scalar-only, color/scalar, face/scalar, and
nearest/scalar scenes with all four blend modes, opacity 0.7, lighting on/off,
and orthographic/oblique perspective cameras. All 64 configurations pass the
declared two-channel-value budget; the largest measured difference is one.
Together these cases check 49,192 native pixels. The final conformance matrix
passes 548 tests across ten JVM/Scala.js project targets.
Samples exclude predeclared mapping/triangle/nearest boundaries affected by
native coverage and antialiasing. Exact endpoint and tie semantics remain the
shared reference contract.

An additional 28 native cases check split and asymmetric diverging mappings,
hidden out-of-range values, NaN and both infinities, and a narrow scalar range
around 100,000,000 with an attached network tube. Each runs with orthographic and
oblique perspective cameras, unlit and with varying-normal directional lighting.
All 93,728 checked pixels pass, with maximum channel error one. Network comparisons
exclude a two-pixel domain silhouette band: MSAA mixes tube and base colors there,
even when a narrow triangle's center has large barycentric weights. The four
network cases retain 490 or 166 interior tube pixels each.

Four native depth-clipping cases check both near and far planes with both camera
projections. Each verifies over 100 retained and 100 removed pixels and their
pick results; retained scalar colors differ by at most one channel value. This
exposed an ignored public near/far setting, now applied by the shared compiler to
the depth coefficients of the projection matrix. Independent JVM/JS tests check
eye-space endpoint projection, invalid ranges, and camera identity. Default
projection bytes and mesh/layer keys remain unchanged.

Native update checks preserve scientific picks through timepoint, threshold,
camera, and white-to-pial morph changes. Timepoint and threshold updates each
upload 1,152 layer bytes without geometry uploads; a camera change uploads zero
bytes; the morph updates 864 position/normal bytes without layer uploads or
topology replacement. A rejected precision-unsafe map leaves every framebuffer
byte, backend resource key, and upload counter unchanged.

The standalone browser harness now supports its documented NoModule exports as
well as CommonJS exports. The native test uses an isolated Playwright Chromium
151.0.7922.34 with Three.js 0.185.1. No system Chrome profile is used.

The admission benchmark now picks at normalized coordinates `(0.55, 0.53)`.
Its former center ray crosses degenerate latitude/longitude poles, including an
incomplete last row on the 163,842-vertex fixture. An independent ray/triangle
calculation finds two ordinary interior intersections at the offset for both
pinned sizes. This changes the benchmark probe point, not production picking.
The full browser harness passes all 48 admission cases, both anatomical profiles,
GPU volume projection, face/nearest checks, and the fragment probes, with no
browser errors.

![Native mixed nearest and scalar map](../visual-qa/surface-three-fragments.png)

Exact source hashes, native cases, update receipts, and test output are recorded
in the [implementation receipt](../benchmarks/receipts/surface-three-fragments-2026-09-07.json).
This implements the shader path; it does not complete the surface-map plan's
remaining legends, publication/persistence integration, cortical acceptance, or
final combined gates.
