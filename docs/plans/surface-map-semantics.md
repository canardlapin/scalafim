# Surface map semantics and legends

Status: complete locally, 2026-09-08. All four stages and their native acceptance
checks are complete. Typed facewise data, explicit interpolation, effective
Intaglio mappings, and mapping-derived legends are implemented. JavaFX's admitted
workaround is an opt-in bounded geometric/atlas compositor with explicit error
and resource limits; Three.js uses native fragment shaders and guarded depth
precision. Scene migrations and original scientific IDs are preserved.

The [completion record](surface-map-completion.md),
[requirement-by-requirement audit](surface-map-final-audit.md), and
[final source/evidence receipt](../benchmarks/receipts/surface-map-final-2026-09-08.json)
record the passing JVM/Scala.js gates, anatomical native matrices, repeated
JavaFX resource admission, publication legends, tested device limits, and
browser cleanup. ScalaFIM changes remain uncommitted in this checkout.

The implementation slices below retain their historical validation baselines.
Their earlier capability limitations are superseded by the current audit above.

### First implementation slice

- Added owned `SurfaceFaceField[A]`, explicit vertex/face layer association,
  face scalar/label/mask/RGBA constructors, and face-aware selections/readouts.
- The shared compiler lowers face samples to separate render corners, preserving
  original face order, vertex IDs, and normals. All three backends use this
  representation. Render profiles account for the expanded buffers and ID map.
- Scene revision 2 persists association and optional face selection; revision 1
  still decodes as vertex data. Face documents infer a `FacewiseData` requirement.
- Existing color composition remains at render corners. General per-fragment
  evaluation and mixed-policy composition belong to Stage 2. This slice does
  not claim to implement those semantics through corner precomposition.
- Initial JavaFX framebuffer check: 32,220 interior pixels, maximum channel
  error 0. Initial actual WebGL check (Playwright Chromium 151.0.7922.34): 196
  interior samples, maximum channel error 0, original pick IDs on both faces,
  and zero geometry uploads on timepoint change. Analytic fixtures provide this
  evidence; a new cortical facewise benchmark remains pending.
- The surface conformance matrix passed all 435 tests. The general vertex-only
  GIFTI example fails its old image checksum on both JVM and JS (expected
  `895258625`, obtained `-748488703`). An isolated JVM build of the surface source
  snapshot taken before these edits reproduces exactly the same failure. The
  example's expected image has been left unchanged; broad example acceptance is
  therefore not reported green.

The common corner representation prioritizes one verified rendering contract.
JavaFX's constant-UV optimization and scalar lookup-texture workarounds
remain follow-up work. The nearest-sample partition is implemented below.

The [local validation receipt](../benchmarks/receipts/surface-facewise-2026-09-07.json)
records source hashes, platform checks, native images, and the reproduced
pre-existing example failure. It includes the final JVM/JS source-map and memory
accounting checks after the full conformance run.

### Second implementation slice: nearest vertex samples

- Vertex constructors accept `SurfaceVertexInterpolation.Color` (legacy default)
  or `NearestSample`; face constructors retain constant face semantics.
- A fixed six-triangle partition per original face carries original face IDs,
  sample owners, and barycentric provenance. All backends consume it. Original
  picks survive subdivision and morphs; exact reference ties choose the lowest
  original vertex ID. Three.js picking now follows this barycentric rule rather
  than Euclidean distance to a corner. A native probe also exposed the existing
  orthographic ray bug; rays now unproject the compiled clip segment, independent
  of Three.js camera class. Three.js raycasts original triangles sharing the
  live position buffer, because native ray tests can miss a display-only seam.
- Nearest and face-constant layers compose per region. Mixing nearest and legacy
  color interpolation on one surface is explicitly rejected before allocation,
  including hidden layers, until general fragment composition is implemented.
- Scene and plan revision 3 record the policy. Scene revisions 1 and 2 migrate
  to their original vertex-color behavior; capability admission infers nearest
  sampling requirements.
- The portable oracle compares subdivided raster output with picks from the
  original mesh under orthographic and oblique, clipped perspective views.
  Native JavaFX and WebGL evidence is recorded in the
  [nearest-sample receipt](../benchmarks/receipts/surface-nearest-2026-09-07.json).
  All 455 conformance tests passed, followed by final targeted checks. JavaFX
  checked 29,238 interior pixels (maximum channel error 1); WebGL checked 179
  (error 0), plus 174 oblique perspective picks verified by reprojection.
  Scalar lookup textures and threshold splitting are still unimplemented.

### Third implementation slice: inspectable scalar mappings

- Intaglio now owns validated sequential, continuous diverging, and split-tail
  scales; piecewise ramps; explicit visibility endpoints; and separate hidden,
  invalid, and out-of-range states. Its legacy colorizer adapter preserves the
  open transparent-band contract. Extreme finite display limits normalize
  without overflowing their interval width.
- ScalaFIM pins provider commit `c6b55d066a8f4c27b0acc3c34fc04737bfaba0bd`
  from the dedicated `codex/scalar-mapping-descriptors-20260907` branch. This
  branch starts at the previous consumer pin, preserving its toolchain and
  dependency baseline; it is not a claim of a merged release on Intaglio main.
- Vertex and face scalar layers compile one effective descriptor after checked
  window/threshold overrides. Mapping identity participates in resource keys.
  Scene revision 4 guards restoration against changed mapping metadata while
  keeping revisions 1–3 readable. The descriptor identity accompanies external
  model bindings; the scene does not yet carry a standalone mapping codec.
- The [implementation review](surface-scalar-mapping-review.md) records the
  provider and consumer checks, source patches, and a coordinate-valued
  [ellipsoid plate](../visual-qa/surface-scalar-mapping.png). It compares
  continuous scaling, two hidden bands, and split tails with asymmetric limits.
  The plate uses unlit nearest-vertex samples, not scalar fragment interpolation.
- This supplies Stage 2's mapping prerequisite. JavaFX scalar lookup textures,
  threshold splitting, general fragment composition, and automatic legends
  remain unimplemented. The prior example checksum failure remains open.

### Fourth implementation slice: scalar fragment reference

- `SurfaceLayer.interpolatedScalar` requires an inspectable mapping and keeps
  raw owned `Double` samples. The raster interpolates original-triangle values
  after clipping and perspective correction, then maps and composites them.
  Single-layer unlit scenes are admitted; additional layers on the same surface,
  including hidden layers, and lighting are rejected before packing.
- Raw values participate in resource identity even when mapped vertex colors
  coincide. Memory profiles include scalar buffers. Scene and plan revision 5
  record scalar interpolation; scene revisions 1–4 remain readable.
- Analytic pixel tests use the coordinate field `1 + 3*x` and invert a known
  perspective projection independently of the raster's picks. Tests include
  clipping, thresholds, nonlinear ramps, split scales, invalid samples,
  saturation, exact constants, and cancellation at extreme finite values.
- JavaFX and Three.js production paths explicitly reject scalar fragment plans.
  A separate JavaFX experiment replaces mesh texture coordinates with affine
  scalar coordinates over the full data envelope and samples an opaque lookup
  strip. Its results and current limitations are recorded in the
  [reference and feasibility review](surface-scalar-fragments-review.md).
  This experiment does not establish general multilayer, lighting, or sharp
  threshold support. The final 72-case probe checked 1,036,494 interior pixels:
  affine grayscale passed all 24 cases within two channel values, while
  piecewise and threshold mappings failed the overall budget (maximum errors
  13 and 108 respectively). All 491 surface conformance tests passed. The
  next JavaFX experiment is geometric partitioning at mapping boundaries.

Surface maps should preserve whether observations belong to vertices or faces,
state how samples become triangle-interior colors, and generate legends from
the same mapping that colors the surface.

The motivating reference is Winkler's
[Displaying vertexwise and facewise brain maps](https://brainder.org/2013/07/28/displaying-vertexwise-and-facewise-brain-maps/).
It separates vertexwise/facewise data, scaling/visibility ranges, central-gap
scaling, and matching colorbar export. The interpolation contracts below are
our proposed extension, informed by the current ScalaFIM renderer code.

## Source baseline before the first slice

- `SurfaceLayer.scala` validates every payload against vertex count times frame
  count and colorizes samples before rendering. Meshes already expose `FaceId`.
- `SurfaceCompiler.scala` allocates vertex color buffers; selections and layer
  descriptions currently resolve through vertices.
- `SurfaceLayerPacket` contains packed colors without an association or
  interpolation policy. Render-plan revision is 1.
- Raster and Three.js interpolate colors; JavaFX builds interpolated triangle
  texture tiles. None of these paths currently evaluates a scalar mapping at
  each fragment from interpolated scalar samples.
- Pinned Intaglio revision `596b398af380079e4b251535230d0bc03cd88c51`
  provides a two-endpoint scalar ramp, clamped window normalization, and a
  transparent open interval whose boundary values remain visible.
- `SurfacePublicationSpec` takes caller-authored labeled color swatches.

These observations describe the pre-implementation working tree, which included existing
uncommitted changes. Recheck relevant diffs before implementation and keep each
implementation slice separate from unrelated work.

## Ownership and compatibility

| Owner | Responsibility |
| --- | --- |
| `surface` | Typed face fields and exact ordered mesh association; reuse existing geometry, topology, and identity machinery. |
| Intaglio core | Inspectable scalar mapping, validation, evaluation, and renderer-neutral legend specification/drawing. |
| `surface-view` | Typed layers, interpolation choices, effective mapping compilation, selection/readouts, resource identity, capabilities, scene persistence, and publication integration. |
| Raster / Three.js / JavaFX adapters | Interpret admitted plans while preserving their semantics and original scientific IDs. |

Keep existing vertex constructors and revision-1 scene appearance compatible.
New constructors should make categorical/discrete intent explicit. Do not infer
association from array length, implicitly convert face fields to vertices, or
silently replace an unsupported interpolation policy. Generic display math
belongs upstream in Intaglio; generic mesh machinery belongs with its existing
owner rather than acquiring a second implementation here.

## Stage 1: typed facewise layers and an end-to-end reference path

1. Add a validated face-field API keyed by `FaceId` and exact ordered topology,
   with owned payload storage and explicit frame count. Equal counts alone are
   insufficient for compatibility. Coordinate-only morphs retain association;
   changed face order, winding, or hemisphere requires rejection or an explicit
   remapping operation outside rendering.
2. Introduce typed vertex/face layer alternatives and constructors for scalar,
   label, mask, and packed-color data. Association and value kind are separate
   concepts, but constructors must prevent invalid combinations.
3. Extend the compiled payload algebra for vertex colors and face colors. Bump
   the plan revision, include association in cache identity, and update backend
   admission. Add explicit migration of old scene documents to vertex color
   interpolation; reject unknown revisions.
4. Add face-capable selection/readouts. Preserve the picked surface, original
   face, nearest vertex, and barycentric coordinates. Face layers read the face
   sample; existing vertex readouts retain their named nearest-vertex behavior.
5. Implement constant face color in the portable raster, then native adapters.
   Three.js may expand render vertices at face boundaries; JavaFX may use
   constant-color texture tiles. Retain original face/vertex mappings for picks,
   reuse original vertex normals when smooth lighting is requested, and report
   expanded buffer cost. Do not change scientific topology to satisfy a backend.
6. Support mixed face and vertex layers in deterministic layer order. Compose
   at the location required by each layer's semantics, with matching opacity
   and blend behavior. Existing vertex precomposition is not a general solution
   for mixed associations.

Acceptance: two adjacent triangles with different face values have no interior
color bleeding under unlit rendering; shared vertices do not force equal colors.
Reordered-topology inputs fail. Picks recover original IDs after native buffer
expansion. Timepoint/style changes upload no geometry; camera changes upload no
layer data. Coordinate morphs preserve data and selections. Both JVM and JS
shared tests pass before native visual admission.

## Stage 2: explicit interpolation policies

Use a small closed policy algebra, constrained by layer kind and association:

| Policy | Meaning | Initial support |
| --- | --- | --- |
| Vertex color interpolation | Map each vertex, then interpolate colors; preserve existing appearance. | All three backends. |
| Face constant | Evaluate one face sample; keep base color constant across the face. | All three backends. |
| Nearest vertex sample | Select the largest barycentric weight, then evaluate that sample; use a documented original-ID tie rule. | Raster and Three.js; prototype JavaFX geometric partition below. |
| Vertex scalar interpolation | Interpolate finite scalar samples with perspective-correct barycentric weights, then apply visibility and color mapping. | Raster and Three.js for inspectable mappings; prototype JavaFX lookup texture and boundary splitting below. |

Keep lighting independent of association/interpolation. Discrete labels and
masks must not acquire scalar interpolation through a permissive flag. Existing
legacy color-interpolated label scenes remain reproducible; new discrete APIs
offer nearest-sample behavior explicitly. Nearest-sample boundaries are a stated
display convention, not inferred anatomical parcel borders.

For scalar interpolation, specify invalid-sample behavior: initially, an
interior involving a non-finite contributing sample yields the mapping's
invalid color, with no implicit renormalization over remaining vertices.
Thresholds apply after scalar interpolation in this mode. Opacity/blending then
operate on the evaluated color. Preserve the existing operation order in the
legacy color mode and record the mode in scene documents and receipts.

Implement a reference evaluator and capability validation first. Arbitrary
`Colorizer` functions can still be evaluated at samples, but cannot be assumed
serializable or executable in GPU shaders. Per-fragment scalar mode requires
the inspectable mapping introduced in Stage 3. Run the JavaFX feasibility work
below before settling its supported modes. Until a path passes admission, reject
it before resource allocation rather than relabeling an approximation.

Acceptance: use an analytic triangle crossing a threshold and a nonlinear
piecewise color mapping to prove the two interpolation orders differ as expected.
Test perspective correction, invalid samples, exact threshold boundaries,
categorical color membership, mixed-layer composition, and deterministic ties.
Native probes sample triangle interiors away from antialiased boundaries; exact
boundary rules remain shared reference tests. Test unsupported-mode rejection.

### JavaFX feasibility work: texture lookup and geometric boundaries

The limitation is narrower than a blanket inability to interpolate scalars.
JavaFX supports independent point, normal, and texture-coordinate indices in
[`TriangleMesh`](https://openjfx.io/javadoc/21/javafx.graphics/javafx/scene/shape/TriangleMesh.html)
and image maps in
[`PhongMaterial`](https://openjfx.io/javadoc/21/javafx.graphics/javafx/scene/paint/PhongMaterial.html).
Our current adapter already uses per-face UVs and a self-illumination atlas.
The following are proposed lowerings using those public APIs, not claims of
verified native support.

**A. Constant face colors.** Give all three corners the same UV at the center
of a padded constant-color swatch. Point and normal indices can remain shared;
JavaFX does not require duplicating positions to introduce a texture seam.
Verify oblique/minified views for filtering leakage. Face association itself
has no apparent JavaFX API blocker.

**B. Nearest vertex samples.** Split the reference triangle at its three edge
midpoints and centroid into six triangles. The two subtriangles touching each
original corner occupy exactly its largest-barycentric-weight region. Give
them that corner's mapped color. This is a geometric representation of the
requested partition, rather than a higher-resolution sampled texture boundary.
Preserve original barycentric coordinates at generated vertices and a render-face
to original-face map for picks, morphing, and normals. Fixed subdivision allows
data/color updates without rebuilding topology. Budget for six times as many
render faces, and distinguish boundary antialiasing from interior color bleeding.

**C. Scalar lookup texture.** Encode the scalar with an affine transform in
`u`; let texture interpolation carry that scalar to each fragment and sample a
one-dimensional color lookup strip. Start with one opaque, unlit scalar layer.
Store values over a finite data envelope, not a clamped display window: vertex
clamping before interpolation changes saturation and threshold locations.
Apply display limits, split-tail mapping, and visibility in the lookup function.
Handle constant fields and non-finite faces explicitly. Changing mapping style
can update only the texture; changing scalar samples updates UV data. Account
for UV uploads separately from scientific geometry changes.

This preserves scalar interpolation in principle, but the finite lookup texture
approximates general mappings, and filtering can blur discontinuities. Probe
texel-center placement, padding, perspective correctness, minification, color
quantization, and texture-size limits on the actual JavaFX pipeline. Do not
assume a public nearest-filter or arbitrary shader control. Define an explicit
error budget before admitting a sampled mapping path.

**D. Sharp thresholds and split points.** Since an interpolated scalar is affine
over a triangle, each threshold or piecewise-ramp knot cuts it along a straight
line in barycentric space. Partition render triangles at those lines, use
separate padded texture regions on opposite sides of discontinuities, and keep
the hidden region displaying the underlay. This can represent threshold geometry
without a finite-texture transition band. It does not by itself prove exact
color, lighting, transparency, or multilayer composition. Preserve coincident
positions at seams and original face provenance. Data/threshold changes can
alter derived topology: declare that cost in a separate admission path rather
than violating the existing no-geometry-upload contract invisibly.

**E. General layered scenes.** Our current adapter composites and lights vertices
before making atlas tiles. A scalar lookup strip cannot directly reproduce
arbitrary independently varying layers and baked lighting. Explore a two-axis
lookup for the common scalar-overlay/curvature-underlay case (`u` and `v` carry
the two fields), and per-face atlas evaluation for general composition. Both
need sampling-error and memory bounds. Smooth lighting adds another varying
quantity; native Phong shading must not silently replace the declared world-space
lighting model. Avoid coplanar overlay meshes unless depth, transparency, and
blend-order behavior are explicitly demonstrated.

Feasibility order: A, B, unlit C, then D, followed by the cortical multilayer and
lighting case E. Keep any approximating path explicitly named and opt-in. Reject
unsupported combinations even if a simpler configuration works.

Exploration evidence (2026-09-07): a deterministic Python arithmetic probe with
seed 20260907 checked 60,000 interior samples of B: zero incorrect nearest-corner
assignments and total subtriangle area equal to the original area. Affine scalar
encoding/decoding differed from direct interpolation by at most `1.33e-15`.
For samples `(-2, 0, 4)`, barycentric weights `(0.1, 0.2, 0.7)`, and display
window `[-1, 1]`, correct post-interpolation normalization gives `1.0`, while
interpolating clamped vertex coordinates gives `0.8`. These are arithmetic
feasibility checks, not JavaFX pixel or performance evidence. Promote them to
shared regression tests during implementation, with analytic boundary cases.

Native prototype acceptance must include unlit/declared-lit, orthographic/
perspective, magnified/minified views, sharp bands, categorical seams, two-layer
composition, exact interior picks, morph updates, and texture/derived-mesh upload
receipts. Use both small fixtures and the existing cortical benchmark sizes.
Decide supported capability combinations from those results, not API availability
alone. The final plan must publish the resulting error and resource budgets.

## Stage 3: inspectable continuous and split scalar mappings

Implement upstream in Intaglio, test JVM and JS, then update ScalaFIM's immutable
dependency pin. Stage 2's scalar-fragment path depends on this stage's descriptor.

1. Define validated mapping values with color limits, a piecewise color ramp,
   visibility selection, hidden color, invalid color, and explicit out-of-range
   behavior. Keep invalid, hidden, and saturated states distinguishable even
   when callers deliberately choose the same color for them.
2. Provide continuous diverging and split-tail alternatives. Continuous mode
   retains the hidden interval's share of the scale; split mode maps the negative
   and positive visible intervals onto their respective ramps. Allow asymmetric
   limits with an explicit center; validate interval order and nonzero widths.
3. Support showing inside or outside a visibility interval, with explicit
   endpoint inclusion. Preserve the existing transparent-band open-interval
   contract through a compatibility adapter. Define clamping and hiding as
   distinct out-of-range choices rather than coupled boolean switches.
4. Expose a pure evaluator and canonical descriptor identity. Compile layer
   window/threshold overrides into one effective descriptor, used by raster,
   supported shader lowering, legends, resource keys, and scene serialization.
5. Keep existing arbitrary colorizers working in their supported modes. Do not
   invent scalar limits or legends for a function lacking inspectable metadata.

Acceptance: independent tabulated expectations at limits, center, threshold
edges and their immediate neighbors, tail midpoints, saturation, NaN, and
infinities. Verify asymmetric scales and invalid constructors. A coordinate-valued
ellipsoid demonstrates fixed-window threshold changes and retained-gap versus
split-tail scaling. Historical article examples are conceptual fixtures unless
the original script and palette are pinned and checked; do not claim numerical
parity from screenshots.

## Stage 4: legends derived from effective mappings

1. Add a typed legend alternative alongside existing manual swatches: continuous
   bar, split bar, categorical entries, and explicit manual legend. Automatic
   scalar legends reference a layer's effective mapping after presentation
   overrides, rather than accepting independently editable limits or colors.
2. Carry quantity label, optional units, limits, ticks, threshold boundaries,
   omitted intervals, and saturation indicators. Distinguish hidden data from
   invalid/missing data in optional explanatory keys. Render ticks through the
   same normalization as the surface mapping.
3. Generate Intaglio chrome from the descriptor. Support manuscript and poster
   presets, shared legends only for identical effective mappings, and separate
   legends for incompatible layer scales. Reject misleading shared-legend requests.
4. Persist mapping/legend identity and interpolation policy in scene documents
   and publication receipts. Threshold/window changes update both surface and
   legend; camera changes leave mapping/legend identity unchanged.
5. Label legends as representing unlit mapping colors. Lighting, layer opacity,
   and compositing can change displayed pixels and must not be interpreted as
   a new scalar-to-color calibration.

Acceptance: sampled bar colors equal the effective evaluator's output before
lighting/compositing; tick positions match analytic normalization. Test scene
round trips and stale-legend prevention after window/threshold changes. Render
and inspect continuous, split, categorical, asymmetric, and missing-data legends
at publication sizes, including long quantity names and units, for clipping and
overlap. Preserve manual swatch output for existing callers.

## Delivery order and completion evidence

1. Land Stage 1 as a complete facewise slice, including all three backends and
   revision migration. Keep its packet design extensible to Stage 2 payloads.
2. Run JavaFX feasibility A/B alongside Stage 2 policy types, compatibility
   behavior, and discrete reference/native paths. Advertise only admitted capabilities.
3. Land Stage 3 in Intaglio, pin it in ScalaFIM, then complete Stage 2 scalar
   interpolation in raster and Three.js. Run JavaFX feasibility C/D/E, admit
   demonstrated combinations, and verify rejection of all remaining combinations.
4. Land Stage 4 and update `docs/surface-viewer.md`, examples, capability tables,
   and conformance receipts to describe the final supported combinations.

Each slice runs the relevant shared suites on both JVM and JS. At integration,
run `sbt surfaceViewConformance`,
`sbt surfaceViewExamplesJVM/test surfaceViewExamplesJS/test`, and the JVM native
admission/visual-QA aliases. Extend the existing browser harness with the same
new fixtures and verify actual WebGL pixels and picks; linking alone is not
visual proof. Follow the browser ownership/audit lifecycle in `AGENTS.md`.

Add a realistic cortical map plate alongside the analytic fixtures. Record
artifact revisions, backend capabilities, numerical/semantic checks, and native
visual results separately. Active scenarios return one `ScenarioResult`, default
to clean `Pass`, and declare any caveats under the scenario harness policy.
An unavailable native environment leaves that admission pending. A supported
feature is complete only when its promised platform and native checks pass;
explicit rejection proves a capability boundary, not rendering support.

No new file-format import/export, areal resampling, statistical threshold
estimation, or automatic face-to-vertex conversion is included in this plan.
