# Qualification input decisions, 2026-10-15

The original manifest and frozen-inputs.json are retained byte for byte. This
record specifies additional cases before their candidate images are scored.

- Every C01 view, projection and declared material profile is a required
  subcase of its parent checkpoint. Controlled-lit anatomy is supplementary
  evidence with its own status; it cannot stand in for a declared subcase.
- Where manifest viewport prose conflicts with the numeric frozen input,
  capture both sizes as named variants. The numeric frozen case remains
  required. Resizing is an actual action whose endpoint is the action's size.
- C10 window-change originally requests ticks [-2, 0, 3] outside its changed
  range [-1, 2]. Retain that request as a negative validation specimen, and add
  a mandatory positive specimen with ticks [-1, 0, 2]. Typed rejection alone
  does not prove the visible key changes with the surface mapping.
- Presence is checkpoint-specific: a hidden crosshair, disabled node glyphs,
  absent halo or inactive linked marker requires an absence check, not an
  empty mask accepted as evidence of presence.

The reference orthographic adapter is a bounded advanced public-field client.
SurfViewJS's own viewer-fit tests replace its public camera, and fitToView has
an orthographic branch. Our adapter installs all surfaces first, disposes the
old controls, assigns OrthographicCamera and calls public setupControls() to
attach render-scheduler listeners. It then applies explicit frozen pose/bounds.
It fixes useShaders:false because the composer retains the constructor camera.
Resize uses public resize followed by explicit bounds and handleResize.
No subsequent addSurface, automatic fitting, or orthographic serialization is
claimed. Source support is not runtime qualification: event-driven motion,
projection, picking, resize and disposal still require actual-host evidence.

Endpoint captures and action replay have separate statuses. Missing host
actions, unsupported elements, uncalibrated comparisons, or incomplete native
routes fail qualification; they are never promoted by matching screenshots.

## Observed contract conflicts

C07 declares `depthWrite:true` and `alphaDiscard:1/255`. SurfViewJS's public
`SurfaceConfig` has no such controls. Its CPU compositor sets depth writes from
the final opacity and uses a fixed alpha discard of `0.5/255`. The first endpoint
adapter omitted that audit. Subsequent receipts record the actual materials and
fail the original frozen material contract when they differ. The original
translucent crossing images are retained: whole-object reference sorting and
ScalaFIM's per-pixel fragment sorting produce different interiors. Neither an
antialiasing envelope nor a looser color tolerance can qualify this difference.
Analytical transparency capability must have a distinct verdict; it cannot be
called full reference pixel parity.

C02's frozen volume vertices fall exactly on voxel centres. Its nearest/linear
images therefore cannot prove fractional interpolation. Independent affine-field,
boundary, nonfinite and mask probes supplement those images. ScalaFIM's linear
sampling uses the closed voxel-centre hull and positive-weight contributors;
SurfViewJS's rendering layer additionally clamps within the half-voxel exterior
and can propagate zero-weight NaNs. Those boundary policies remain distinct.
The reference adapter uses an explicit finite fill sentinel outside the frozen
source range, retaining valid zero values. Custom palettes use the public preset
registry, with mount-owned names removed on disposal.

C09 asks for signed colors and radius directly proportional to absolute weight.
A single reference ConnectivityLayer instead looks up abs(weight) and scales
radius as base * (0.3 + 0.7 * normalizedWeight). The original captures retain
that mismatch. The revised reference client first uses public getFilteredEdges
for threshold/top-N selection, then creates public constant-color, fixed-radius
ConnectivityLayers per selected edge. This expresses the declared signed
presentation without changing the reference implementation. Coincident opaque
endpoint spheres may repeat across layers. Receipts include selected edges,
actual colors and configurations. ScalaFIM explicitly selects Overlay depth
policy, matching reference connectivity's depthTest=false; nodes paint after
edges. This is a public-client composition contract, not a claim that one
reference layer provides signed mapping. Neither route's tube tessellation is
assumed pixel-identical without measurement.

The per-edge C09 client assigns monotonically increasing public Three object
renderOrder values after attachment: all edges in source order, then nodes.
This prevents separate ConnectivityLayers from acquiring unintended depth
sorting within the transparent render queue. Actual object names and orders
are retained in the capture receipts.

C10's exact viewer key contract exceeds the pinned reference API. The public
viewer.exportPNG({colorbar:true, colorbarLayer:id}) route renders a scalar key
with automatic geometry and endpoint/threshold-boundary ticks. It does not
accept the frozen independent 240x160 key size or arbitrary [-2,0,3] ticks.
LabelLayer and TwoDataLayer do not provide this scalar-key interface; no public
bivariate key route exists. renderAtlasFigureSVG provides categorical swatches
in a separate atlas-plate export route, not the frozen viewer route. Preserve
all original C10 contracts as unmet; separately identified public-export
evidence must not be counted as their replacement. Adding the missing reference
capabilities would require a new upstream revision and a newly pinned matrix.
This is a source-level finding at fb60f3705d51d2d74e69ddf71df7000ee9eb5b06,
independently reviewed against the viewer, figure overlay, preset and control
implementations.

C05's halo is a semantic mismatch, not merely an antialiasing difference. The
pinned reference draws a full-width white stroke at 0.6 opacity after its opaque
black core (separate Three render queues), with round endcaps and depth writes
disabled. Overlapping short boundary segments accumulate translucent white.
ScalaFIM's documented halo consists of opaque nonoverlapping side bands around
a crisp core, with butt caps and opaque depth writes. Changing a color or
tolerance cannot reconcile these behaviors. Preserve the failed halo images;
exact compatibility requires an explicit cross-backend stroke policy or an
upstream reference correction and new pin. The discriminating next fixture is
one short edge and two overlapping edges, measuring center, flank, cap and
overlap separately.

C06/C08 baseline annotation anchors were measured directly in the actual
reference marker world transforms, against an independent source-vertex morph
oracle. Eleven captured candidate sphere centers match within 1e-4 world units.
The three morphed reference cases remain at their base vertices: the midpoint
error reaches 4.235901260375975 and the fully expanded error 8.47180252075195
world units. Nonmorphed reference cases pass. These baseline images and
annotation-centers.json receipts are retained; the discrepancy cannot be
classified as an antialiasing error.

A separate opt-in reference client (--reference-follow-morph) now synchronizes
ordinary noninstanced markers from the public Mesh.getVertexPosition result,
updates the exported AnnotationRecord.position and marker.position in mesh-local
coordinates, and requests rendering. Native reference behavior remains the
default. All eleven measured centers pass for this client route, including
base, midpoint and full expansion. The geometry-change native pixel maximum
falls from 168 to 3; strict parity still fails. This is checkpoint client
synchronization, not native automatic following, annotation:moved event
equivalence, instanced support or continuous-animation qualification. Baseline
images and receipts remain separate and unchanged.
