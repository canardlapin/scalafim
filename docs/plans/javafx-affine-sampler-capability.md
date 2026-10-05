# Per-map affine-data sampling capability for JavaFX

Mote: `bd-01M241Y5FPND8TPHTJQVHJWW7K`.

This is a concrete upstream capability proposal, not adoption of the diagnostic
JavaFX patch or approval of a stock runtime. The existing stock-Metal rejection
is retained in `docs/verification/javafx-stock-metal-decision-20260929.md`.

## Required binding and behavior

Introduce an explicit sampler policy on each diffuse/self-illumination map
binding. Keep the existing policy as the default. An affine-data policy must:

1. Use linear interpolation at the base level with no generated/selected mip
   levels. Adjacent atlas tiles represent unrelated scientific triangles;
   averaging across their boundaries is not a lawful reduction of the field.
2. Use centroid texture-coordinate interpolation for multisample rasterization,
   keeping covered samples inside the original triangle's affine extension.
3. Apply the policy per map binding. Reusing the same Image on an ordinary
   material must not silently change that material's filtering.
4. Include the complete sampler policy in texture/material/shader cache identity
   and maintain it through PixelBuffer mutations and retained texture uploads.
5. Return an explicit unsupported capability when the active ES2, D3D or Metal
   implementation cannot honor the policy. A version string or SCENE3D flag
   must not grant capability or select a silent approximation.

JavaFX 24's documented [PhongMaterial API](https://openjfx.io/javadoc/24/javafx.graphics/javafx/scene/paint/PhongMaterial.html)
exposes image-map bindings; the published material and
[Image API](https://openjfx.io/javadoc/24/javafx.graphics/javafx/scene/image/Image.html)
do not document per-binding mipmap or centroid controls. The need for a new
public capability is an inference from that API surface and the independently
retained sampler failures, not a claim about every newer runtime.

The diagnostic JavaFX 24 two-class override changes ES2 filtering process-wide.
It is an experimental positive control and cannot implement the required
per-binding ownership or deployment contract. It is not shipped by ScalaFIM.

## Scientific and adapter contract

The recovered adapter consumes current V1 render-plan geometry and already
composited opaque vertex colors. Palette mapping, cutoff, missingness, blend and
display opacity occur before lowering. Adaptive cyclic anchors and bounded
midpoint children represent that same field; they must not average source
triangles or smooth statistical values. Child picks resolve to the original
face before computing original barycentrics and vertex identity.

The default `LegacyTriangle`/`NativePhong` behavior remains selected. The
experimental `WorldVertexLambert` policy explicitly shades original vertex
colors using anatomical world normals before atlas interpolation; it is
separate from native per-pixel Phong lighting. Its lit fixtures retain original
RGB, Float normals and the declared Double light parameters for the independent
oracle. Equality between these two lighting policies is not claimed.

Production `JavaFxSurfaceBackend.create` refuses affine encodings with the
typed `SamplerUnqualified` outcome. Package-scoped diagnostic hosts and the
public encoding probe permit controlled experiments without minting product
approval. Runtime receipts record actual Prism initialization, module sources,
hashes, fallback and mutation arguments; runtime-only preflight remains
insufficient for product qualification.

## Prospective acceptance

Use the unchanged `tools/surface-view/javafx-affine-color/plan.json` and
`production-plan.json`, their independent original-triangle pixel oracle, and
the existing two-channel-level budget. Required runs include both affine
encodings, cyclic and shuffled atlas packing, AA off/on, front/oblique geometry,
production widths 4096/4092 and explicit world-space lighting. Retain legacy
encoding and stock-sampler negative controls with their named failure criteria.

Then bind dense bilateral cortex, palette/cutoff/opacity/map/lighting/camera/morph
updates, original pick identities and resource measurements to the exact
candidate and runtime. Qualify each claimed platform independently. Bind any
PLS Neuro consumer run to exact inputs and the executed dependency closure;
historical images and already-mapped RGB cannot establish original numeric
beta/FIR mapping or threshold fidelity. No visual, performance, packaging or
consumer admission follows merely from a passing planar color oracle.
