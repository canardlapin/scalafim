# Affine JavaFX face atlas

`JavaFxAtlasEncoding.AffineMidpointOpaque` is an opt-in native adapter encoding
for already-composited opaque vertex colours. It subdivides each render-plan
triangle into four midpoint children and places their four 2 × 2 tiles in one
4 × 4 atlas block. Nominal texture pixels per source face remain 16.

```scala
val atlas = JavaFxAtlasConfig.make(
  encoding = JavaFxAtlasEncoding.AffineMidpointOpaque,
  maxRenderedFaces = 4000000
).toOption.get
val backend = JavaFxSurfaceBackend.create(atlas)
```

This is an **encoding policy**, not proof of a faithful stock-JavaFX sampler.
It requires no mipmaps and centroid UV interpolation when MSAA is enabled.
The public JavaFX material API currently cannot express those requirements.
The default encoding remains unchanged. The mode refuses nonopaque final
colours, unsupported block/texture sizes and expanded geometry over its
configured limit before native mesh allocation. The default rendered-face
budget is 4,000,000; receipts count actual child geometry and buffer updates.

For each child with corner colours p, q, r, its fourth texel is q + r - p.
The midpoint construction guarantees this texel is an original colour or
edge average, so no clamped/nonlinear extension is needed. Original packed
channels are averaged before the only rounding step. Synthetic geometry and
normal midpoints stay inside the JavaFX adapter; the scientific render plan
is unchanged. Morphs recompute midpoint attributes. Native child faces map
back to the source packet face, after which existing original scientific
face/vertex and barycentric provenance applies.

## Evidence, 2026-09-09

The current native build's default Intaglio pin lacks APIs used by its existing
surface code. Qualification used the exact Intaglio commit recorded by the
prior surface gate, `55658abfbbfed0c9a36ab612b38bd8f0677bc158`, through the
existing `scalafim.intaglio.build` override. Ordinary clean-checkout build
qualification remains open until the native dependency pin is reconciled.

With that dependency closure:

- JavaFX: 32 tests pass, including six new encoding regressions.
- Shared surface-view: 128 JVM and 128 Scala.js tests pass.
- The native probe verifies 1,200 child picks across atlas chunks retain
  original face, vertex and barycentric identity. It passes on native
  JavaFX 21.0.5, and again on the consumer's JavaFX 24.0.1 with the diagnostic
  compatibility module patch. No Stage is shown. These are explicit
  PickResult dispatch checks, not OS ray-picking coverage.
- An independent sampler reads the actual uploaded positions, indices, UVs
  and atlas pixels. Saturated/random RGB cases, vertices, diagonal edges and
  cyclic winding-preserving order pass a 0.50001-channel bound. Its legacy
  control still reproduces 255 -> 233.75 diagonal darkening.
- Chunk offsets, winding, buffer counts, resource refusal, colour updates,
  constant/varying UV updates and midpoint morphs have direct checks.

Exact sources, commands, failed controls and passing logs are retained in the
PLS Neuro consumer at `output/brain-display/affine-21`. The current native
source changes are uncommitted; no consumer dependency pin adopts them yet.

## Compatibility module and initial adaptive feasibility

An isolated two-class patch for JavaFX 24.0.1 disables ES2 colour mipmaps and
uses centroid texture-coordinate varyings. The Java launcher's
`--patch-module=javafx.graphics=<compatibility.jar>` loads both overrides
from the asserted JAR. The public-JavaFX MSAA ramp gives 0.329018 / 0.410830 /
0.391206 gray-level RMS at 8,192 / 32,768 / 131,072 triangles. This is a
tested deployment mechanism on macOS ES2, not production packaging or
cross-platform qualification. Its mipmap policy is process-wide for affected
Phong maps; a general per-texture policy needs cache/material plumbing.

The four-child encoding is a general bounded fallback. A cheaper candidate
keeps one original triangle whenever one cyclic choice of p, q, r has
q + r - p inside the RGB cube. Rotate only the texture-corner assignment and
write an affine continuation; scientific winding and geometry need not change.
All 549,018 faces in one exported, exact unlit beta cortical colour state meet
this condition. Only 1,548 need a rotated anchor; no subdivisions are needed
for that state. That first scan established arithmetic feasibility. The implementation below
now handles adaptive layout; broad palette/update performance remains open.

Remaining qualification includes atlas packing invariance,
actual native colour frames with edge-specific error, obliquity/device-scale
sweeps, lighting, ray picks, dense geometry/update latency and memory, and
supported-platform runtime admission. Do not close the broader appearance
issue on the strength of the current bounded tests.


## Adaptive implementation — 2026-09-10

Select `JavaFxAtlasEncoding.AdaptiveAffineOpaque` for the checked single-face
encoding with midpoint fallback. Original vertices stay shared per chunk;
only fallback faces append three edge midpoints. Every source face retains
one 4 × 4 block, so this version reduces geometry but not atlas allocation.
Both affine modes have the same opaque final-colour and sampler contract.
The encoding's `facesPerSource` is an upper bound in adaptive mode; native
chunk counts and build/upload receipts report actual geometry.

Cyclic anchor changes update only UVs and pixels. Split/collapse changes are
detected before mutation and the regular backend compiles a complete scene
replacement before swapping it in. Direct `updateColors` on a probe returns
`AtlasLayoutChanged` when a replacement is needed. The face budget uses the
actual adaptive count; rejection leaves the old native view/pick plan intact.
Do not treat a face budget as a bound on Prism's additional heap/GPU storage.

Qualification now passes 38 JavaFX, 128 shared JVM and 128 shared JS tests
with the same exact Intaglio override. The new independent sampler includes
saturated/random RGB, cyclic anchors and diagonal edges; mixed one/four-child
chunks verify original identity and winding. Native update probes pass 3,300
explicit picks after split/collapse/anchor changes on both JavaFX 21.0.5 and
patched 24.0.1. No Stage or OS-input automation is involved.

A full bilateral unlit beta fixture renders 549,018 faces with 274,513 position
vertices and 37.70 MiB of native mesh buffers under a 2 GiB heap. Guarded
1200 × 800 MSAA snapshots verify nonblank content and visible colour inversion
followed by exact framebuffer restoration. Full midpoint subdivision creates
2,196,072 faces and 201.06 MiB of mesh buffers. Its 2 GiB run exhausted JavaFX
heap and produced a black image despite a zero process exit; those timings
are rejected. A 4 GiB guarded rerun succeeds. Runtime resource policy still
needs qualification before production use.

Two guarded affine images differ by 1.800 channel RMS on common nonwhite
pixels including edges (maximum 44). This is pairwise native agreement, not
analytic colour fidelity. A reference accounting for MSAA coverage and
interpolation positions remains necessary; do not infer colour certification
from the CPU encoding bound or a successful snapshot alone.

Exact follow-up source, commands, images, failed/passing runs and input hashes
are retained in the consumer's `output/brain-display/adaptive-22`, with the
full report in `docs/verification/javafx-cortical-appearance.md`. Current
source and runtime overrides remain uncommitted/unadopted. Lighting, broader
colour states, GPU edge/packing tests, ray picks and runtime packaging remain.


## Native GPU color gate — 2026-09-10

The optional `JavaFxAffineColorProbe` and
[portable independent oracle](../../tools/surface-view/javafx-affine-color/README.md)
now establish the planar, opaque, unlit color contract on exact JavaFX 24.0.1
macOS ES2. The protocol freezes a two-channel-level budget, diagonal/RGB
fixtures, two ramp densities, front/oblique planes, original/cyclic/shuffled
faces, and AA off/on before results. Atlas size is capped at 256 for this
isolated gate; larger production atlases remain a separate qualification.

Both affine encodings pass all 48 cases and 32 order/packing comparisons.
For each, 1,838,544 fully covered pixel instances include 732,618 internal-edge
instances. The reference evaluates original colored triangle/pixel
intersections, independent of native UVs, atlas texels and midpoint tables.
Its conservative MSAA footprint bound has worst excess 1.223; without AA,
maximum pixel-center error is 1.452. Equivalent order/packing changes differ
by at most one channel level. Closed-form fields independently validate the
reference within 5.685e-14.

The required controls discriminate: legacy padding fails diagonal cases with
up to 21.483 center error and 21 levels of ordering dependence; stock sampling
fails dense ramps with 17.625 footprint excess and 18 levels of packing
dependence. All results, including failed controls, are retained. The actual
sbt-compiled fixture reproduces the initial 192 PNGs and source binaries byte
for byte; the affected JavaFX suite passes 38 tests with the same exact
Intaglio override. The probe uses JDK image export, adding no Swing dependency.

Exact evidence is in the consumer's `output/brain-display/gpu-color-23`.
The gate excludes partial outer silhouette/background coverage and does not
predict exact multisample positions. It does not qualify lighting, perspective,
large-atlas precision, actual device scaling, cortical occlusion, ray picks or
packaging. The native candidate and runtime patch remain unadopted.

## Production-width atlas and lighting qualification — 2026-09-10

The adaptive encoding passes **288/288** cases at maximum texture widths
4,096 and 4,092, with unlit, existing directional (35% ambient / 65% diffuse),
and softer directional (72% / 28%) settings. All 192 cyclic/packing comparisons
pass within one channel level. The same prospective two-level budget and
original-triangle pixel oracle apply; internal edges remain included.
`production-plan.json` was frozen before rendering (SHA-256
`5fa5b64a9af6ac24907b502fcc34152df17a1a0aa4d35b3e76dc66fcf0b02e4f`).
The dense fixture actually creates 4096 × 128 and 4092 × 132 textures. This
extends width and non-power-of-two precision evidence, not every maximum
texture-area or device-scale combination.

| Lighting, either width | Maximum footprint excess | Maximum non-AA center error | Maximum permutation difference |
|---|---:|---:|---:|
| Unlit | 1.222223 | 1.451389 | 1 |
| Existing directional | 0.913842 | 1.263883 | 1 |
| Softer directional | 0.681946 | 0.930394 | 1 |

Values are 8-bit channel levels. Lit binary fixtures retain original RGB,
Float vertex normals and Double light parameters. Python independently
computes the world-normal Lambert factor and rounds shaded original vertex
colors before applying the affine pixel oracle. It never reads native atlas
colors or texture coordinates. The varying normal field is deliberately
specified on planar geometry; it does not claim geometric-normal generation.
Four axis-normal checks additionally cover back-facing normals, ambient-only
contribution, zero contribution and saturation. The new implementation's
original 256-wide unlit output is byte-identical for all 48 PNG/binary pairs.
All 38 affected native JavaFX tests pass using the recorded exact Intaglio
override; no main/shared renderer source changed during this qualification.

Required controls still discriminate at width 4096. Legacy encoding passes
24/48 cases and fails the diagonal fixture, with 21.483 maximum non-AA center
error. Stock JavaFX passes 39/48 and fails dense ramp cases, with 24.089 maximum
footprint excess and 25 levels of permutation dependence. Fixture generation
itself succeeds in both controls; those are color-gate failures, not launch
failures. An initial sandbox ES2 initialization failure is separately retained.

The retained real bilateral cortex also stays at **549,018 rendered faces and
274,513 position vertices** under all three lighting settings. Independent
RGB/normal calculations confirm no fallback subdivisions for the original
and inverted colors. Guarded snapshots verify visible color changes and exact
restoration. Instrumented and original snapshots are byte-identical for each
lighting mode. All six native cortical processes use the same version-pinned
compatibility JAR and 2 GiB heap, show no Stage, and exit successfully.

Visual review favors softer lighting: it retains shape cues while exposing
the inferior cortex and reducing broad dark regions. It is a candidate
presentation setting, not a changed application default. These images retain
a shared bilateral camera (left lateral / right medial) and broad white
margins; they do not certify final viewer layout or strong-overlay legibility.

The update experiment exposes a performance limit. Warm original-RGB
inversion/restoration under default lighting takes 293–325 ms: 26–53 ms in
`backend.render`, then 264–291 ms in the subsequent snapshot. It uploads
13,176,432 UV bytes each time without changing native topology. The comparable
unlit updates take 38–49 ms and upload no UVs. Softer-lighting warm updates take
303–355 ms. These are a few sequential measurements, not frame-rate percentiles
or UI-interaction proof. The location of the delay is consistent with expensive
native work after UV mutation, not yet a driver-level attribution.

Independent counts explain why UVs change: default lighting gives 44,727
constant/varying face transitions and 1,539 anchor changes during inversion;
softer lighting gives 46,530 and 1,465. Removing constant-face specialization
alone cannot remove the necessary anchor changes. Native follow-up
`bd-01M24EQFARZKYMZ71BJP91C9XS` owns this performance investigation; it must
preserve color fidelity, picks and bounded failure behavior. The next consumer
step remains exact provider adoption and reproducible runtime packaging, with
this update limit explicit. No production runtime patch is installed here.

Exact sources, commands, native input hashes, controls, images and measurements
are retained in `output/brain-display/gpu-color-24` in the PLS Neuro consumer.
