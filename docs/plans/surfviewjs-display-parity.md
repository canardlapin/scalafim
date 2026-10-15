# Core SurfViewJS display parity

Status: implementation in progress; core parity unqualified, 2026-10-14. Mote: `bd-01M4XAQ87FRRSD48EV0CPK05FP`.
The user requested core display parity, recovery of useful retained work where
possible, and visual QA with side-by-side comparisons as proof. This document
defines that work; it does not claim that recovery or parity has passed.

## Outcome and scope

ScalaFIM should reproduce the core scientific displays available in SurfViewJS
through public APIs: anatomy, layered maps, bilateral and multiple views,
parcel/ROI outlines, markers, legends, clipping, translucent surfaces, time and
geometry changes, basic networks, linked flat maps, and publication figures.
Scientific identities and display decisions must survive picking, interaction,
export, and scene restoration.

The target includes Scala.js/Three.js and the JVM display path. The portable
raster provides an independent rendering implementation, but shares upstream
mapping/compiler inputs with native ScalaFIM renderers. It is not an independent
oracle for those shared decisions. A raster snapshot does not
establish native JavaFX interaction or performance parity. A core case that
cannot run acceptably on the intended JVM host remains incomplete until an
explicit implementation, including a qualified fallback if needed, satisfies
its output and interaction contract. Freeze a matrix of required product routes
(browser viewer, JVM viewer, publication export) separately from actual
renderers/providers before qualification. A production JVM fallback can satisfy
its product route only through its real host and interaction/resource budgets;
it cannot count as a JavaFX-native pass. Keep native failures visible alongside
the fallback result; diagnostic constructors and test-only overrides are excluded.

Core parity means matching scientific content and visible elements with
measured rendering tolerances. It does not require identical widget styling or
GPU algorithms. React bindings, plugins, parcel puzzles, force-directed layouts,
standalone HTML packaging, cinematic effects, and all material presets are
outside this plan. True vector atlas silhouettes are an extension; core figures
may contain raster surfaces with vector text, outlines, and legends.

## Audited baseline

- ScalaFIM main: `45c71b9bfcca484f507d72cccb53682348492237`, clean before this plan.
- SurfViewJS: `fb60f3705d51d2d74e69ddf71df7000ee9eb5b06`, in
  `~/code/jscode/surfviewjs`. Unrelated local settings and untracked concept
  documents/output are excluded from the reference.
- Retained ScalaFIM work: `c14b36ac624c3d38e767af7bdbd62addd2c1cd47`, present on
  retained branches including `codex/scalajs-gifti-ingestion`, absent from main.

The initial assessment inspected source and tests. Fresh baseline captures now
exist in [the observed-default gallery](../visual-qa/surfviewjs-parity/baseline-20261014/index.html):
C01--C03 use the same asymmetric mesh, 512px viewport, scalar endpoints and
explicit camera in SurfViewJS, unchanged ScalaFIM Three.js, actual JavaFX, and
portable raster. Browser silhouette IoU is 1 and camera matrices agree within
1e-7; color and lighting differences are substantial. These are smoke captures,
not full-family passes. Every formal baseline verdict remains failure.
Historical receipts and closed Motes describe their recorded revisions only.

| Display element | Audited baseline main | Action |
| --- | --- | --- |
| Meshes, anatomical cameras, scalar/label/mask/RGBA layers, curvature, blending | Implemented | Preserve and compare actual displays. |
| Time frames, geometry families/morphing, picks/readouts, scene documents | Implemented | Exercise transitions and restoration, not only initial frames. |
| Volume-projected maps and filtered network edges | Implemented with explicit sampling/backend limits | Reuse qualified sampling; compare values and display. GPU algorithm identity is not required. |
| Multiple surfaces in one scene; four/six-view figures | Only `Single`/`Bilateral` layouts, with one plan camera | Add per-view cameras, surface instances, and composition. |
| Parcel/ROI outlines | `SurfaceParcels` extracts boundaries; no dedicated display primitive | Add connected contours, width, color and halo. |
| Markers and crosshairs | `SurfaceAnnotations.layer` colors selected vertices | Add visible glyphs, projected labels, selection and hover rendering. |
| Fragment threshold edges, inspectable mappings, automatic legends | Main precolors vertices and offers manual legend swatches | Recover retained work, then qualify compatible display policies. |
| World clipping | Raster supports it; native backends reject it | Implement native interpretation or an admitted JVM rendering path. |
| Surface translucency | Three.js flattens onto opaque gray and emits alpha 1 | Preserve surface alpha and define depth/composition semantics. Layer opacity is insufficient. |
| Bivariate effect/confidence maps | Static RGBA can be supplied manually | Add paired-field mapping and a matching 2D key. |
| Basic connectome presentation | Signed line/tube edges exist | Add node glyphs, weight-dependent width/color, correct composed-view placement. |
| Interactive flat maps | No dedicated flat-map host | Add 2D picking, ROI drawing and linked selection/time. |
| Publication output | Snapshot plus Intaglio chrome exists | Add multi-panel capture and shared mapping-derived legends. |

ScalaFIM anchors: `SurfaceLayer`, `SurfaceViewer`, `SurfaceCompiler`,
`SurfaceRenderPlan`, `SurfaceDynamics`, `SurfaceNetwork`, `SurfacePublication`,
`ThreeSurfaceProgram`, `ThreeJsRuntime`, and `JavaFxSurfaceProgram`. SurfViewJS
counterparts: `NeuroSurfaceViewer`, `MultiLayerNeuroSurface`, `OutlineLayer`,
`annotations.ts`, `TwoDataLayer`, `ConnectivityLayer`, `FlatMapView`,
`LinkedBrainWorkspace`, and the report/figure APIs.

SurfViewJS `ColorMap.getColor` hides both endpoints of an active threshold band;
the currently pinned Intaglio `ThresholdBand` hides only its interior.
SurfViewJS uses a discrete color lookup where ScalaFIM's current ramp
interpolates. Its sharp threshold path is limited to eligible topmost plain
`DataLayer` overlays, not every layer. Compatibility must be explicit without
silently changing existing ScalaFIM scenes. The captured reference also converts
sRGB vertex colors into linear light before interpolation, then encodes the
framebuffer as sRGB. ScalaFIM's legacy path interpolates display RGB. Preserve
that legacy behavior and add an explicit color interpolation contract for the
compatible path. Curvature defaults differ in polarity and range: reference
positive curvature is dark, with brightness/contrast/smoothness controls; the
legacy ScalaFIM helper defaults to negative-dark and a [-1,1] ramp. Match explicit
underlay parameters and retain both defaults as named behavior. A suspected reference bug needs an
independent oracle and recorded disposition, not automatic imitation.

## Recovery before new implementation

`c14b36ac` contains facewise/nearest/scalar interpolation, fragment composition,
mapping-derived legends, camera fitting, and native QA work. It also includes
GIFTI/sampling changes and extensive dated evidence. Its parent already changed
Intaglio to `55658abfbbfed0c9a36ab612b38bd8f0677bc158`; cherry-picking the commit
alone does not bring that prerequisite. Main pins
`596b398af380079e4b251535230d0bc03cd88c51`.

A non-mutating `git apply --check` of the commit's source/example/build delta
failed on 11 paths: build wiring, JavaFX controller/program/probe/native-pick
helper, raster, Three.js runtime/README, shared compiler/view types, and
sampling. This is patch applicability evidence, not a three-way merge verdict.
The [recovery ledger](../verification/migration-recovery-ledger-20261005.md)
requires current-contract ports for retained development.

Use an isolated current-main worktree. Audit the historical delta and later
fixes on its retained line; port coherent pieces rather than replacing modules
with an old tree. Preserve typed camera/cursor APIs, GIFTI declarations,
sampling tally/tie behavior, geometry persistence, and the JavaFX
`SamplerUnqualified` production guard. Reconcile required Intaglio APIs and its
pin independently of unrelated historical dependency bumps. Keep dated receipts
unchanged. Review and land a new exact candidate after its own gates pass.
The user's merge authorization is retained. Recovery, composition and annotation
work have since been locally merged; the checkpoints below record those revisions.
Independent recovery review found that restored V1 scenes would change camera
framing under the recovered fit formula. The port now needs an explicit legacy
framing policy, V7 persistence, and genuine main-V1 matrix/viewport regression
fixtures before landing. V2--V6 historical documents lacked an aspect field;
non-square historical aspect cannot be reconstructed from absent data.

## Implementation sequence

Each stage produces an inspectable candidate and records its exact SHA and
verdict in Mote. Historical issues provide evidence, not current acceptance.

| Stage | Changes and ownership | Exit evidence |
| --- | --- | --- |
| P0 — baseline and recovery | Pin assets/tools; capture baseline C01–C03 below; port useful `c14b36ac` display code and required Intaglio APIs/pin. Own affected surface modules/build/docs only. | Current APIs preserved; affected JVM/JS tests and compilation pass; native before/after captures; independent conflict/dependency review. |
| P1 — comparison harness | Add test-only manifest, public-API adapters and runner under `tools/surface-view/parity/` and `examples/surface-view/`. Expand the baseline into the full gallery. | Every required case produces artifacts or an explicit failure; deliberate corruptions are rejected. |
| P2 — views and scenes | Add view identity, per-view cameras, repeated surface instances and several surfaces in one scene to shared state/plan/codec and backends. | C01/C04 pass: four-view figures, bilateral placement, linked/independent navigation, correct instance picks; scientific world coordinates unchanged. |
| P3 — outlines and annotations | Add typed contours, marker/label primitives, parcel/ROI lowering, crosshairs, network node glyphs and basic edge styles. | C05/C06/C09 pass at multiple zooms and through morphs; borders lie between parcels; glyphs have stable IDs and declared size units. |
| P4 — mapping and visibility | Finish compatible threshold/palette/color-interpolation and curvature policies, continuous/categorical/bivariate legends, native clipping and surface alpha; extend admission and persistence. | C02/C03/C07/C10 pass: endpoint equality, invalid values, overlap/occlusion, clipping picks and changing legends. |
| P5 — linked views and figures | Add flat-map host/linking, polygon/lasso ROI, publication panels/chrome and complete dynamic controls/export/restore. | C08/C11/C12 pass, including two export sizes and interaction sequences; figures share viewer mappings/selections. |
| P6 — admission and landing | Run the fixed gallery on the exact final candidate and declared browser/JVM environments; inspect all plates and adverse cases. | Every required case/product route passes semantics, visuals, interaction and resource gates; actual backend verdicts remain explicit; independent review accepted; local landing recorded separately from push/hosted CI. |

P0 builds the minimal reference capture harness before changing rendering. P1
expands that harness rather than starting another system. After P1, visual
inspection is part of every stage, not deferred to P6.

The P4 source review fixes several compatibility decisions before implementation:

- Preserve existing scenes with an explicit legacy byte-color pipeline. Add an
  opt-in display-sRGB/linear-light pipeline, with a document migration that keeps
  old scenes on the legacy policy. Alpha never receives a color transfer.
- For ordinary vertex stacks, compose display colors at vertices, decode RGB,
  interpolate and light in linear space, then encode once. Decoding the final
  fragment byte color would produce a different result. Reference threshold
  edges use a separate linear-space overlay stage.
- Curvature is independently parameterized by brightness, contrast, smoothness
  and polarity. The reference formula is
  `brightness - clamp(curvature / smoothness, -.5, .5) * contrast`, clamped to
  the display range. Keep the existing curvature factory's output unchanged.
- Reference additive blending is plus-lighter; current Intaglio additive uses
  an additive channel blend inside source-over. Add a distinct operator rather
  than changing existing `Additive` semantics.
- Controlled material profiles explicitly disable specular and emissive terms.
  The reference's default emissive color `0x0a0a0a` at intensity `.2` otherwise
  adds two byte levels to nominally zero RGB channels. Observed-default captures
  retain that behavior.
- Any initially unsupported pipeline/backend combination returns a typed
  refusal. Existing JavaFX sampling admission remains in force. A production
  fallback requires its own visual, interaction and resource evidence.

Independent material probes now cover constant RGB/gray, three curvature
values, half-light gray, half-opacity white over black, and a black/white
gradient. On the declared Metal reference environment, all ten specimens have
zero repeat-capture difference and zero interior error against their analytic
oracles. This is bounded reference calibration, not candidate display parity.

Generic mappings, legends and 2D composition belong in Intaglio. Surface
association, scientific IDs, anatomical layout, mapping policy and state belong
in `surface-view`. Native objects remain in their backends. Add small typed plan
primitives rather than exposing Three.js types or encoding every element as
vertex colors.

## Fixed display gallery

Use an asymmetric analytic mesh and the checksum-pinned bilateral fsaverage5
pial corpus already present in SurfViewJS. It remains external: data-specific
redistribution terms have not been established. Curvature, labels, scalar/time
fields and network weights are deterministic synthetic inputs. The morph target
is a 1.2 expansion around a declared hemisphere anchor with unchanged topology;
it tests display interpolation and identity, not anatomical inflation. Record
provenance and availability. Missing required assets fail rather than skip.

| Case | Required visible result and operation |
| --- | --- |
| C01 — anatomy | Left/right lateral and medial, dorsal/ventral; orthographic/perspective; curvature; transformed geometry; orbit/pan/zoom/resize. |
| C02 — scalar maps | Sequential/signed maps, asymmetric ranges, threshold equality and adjacent values, zero-width disabled threshold, NaN/masks, eligible sharp threshold edges. |
| C03 — composition | Curvature plus two maps; normal/additive/multiply, visibility, opacity, reordering and time/style changes. Check each resulting state. |
| C04 — layouts | Anatomical bilateral scene, split hemispheres, four-view lateral/medial and six-view figures, repeated surface views, linked and independent cameras. |
| C05 — parcels/ROI | Categorical fill, continuous parcel values, exterior ROI border, parcel borders/halo, ignored/medial-wall labels, selected parcel highlight/pick. |
| C06 — annotations | Multiple markers, selected/hover marker, crosshair/label, foreground/occluded cases; persistence through camera/geometry changes. |
| C07 — clipping/alpha | Axis/oblique planes, flipped keep-side, hidden-part pick rejection, overlapping translucent surfaces/internal geometry, zero-alpha regions and empty/all-hidden layer stacks. |
| C08 — dynamics | Time frames/interpolated playback; pial/synthetic-expansion endpoints and midpoint; pause/resume; persistent maps, picks and annotation IDs. The historical checkpoint name `inflated` identifies this explicitly synthetic display target. |
| C09 — networks | Signed weighted edges, node glyphs, weight-dependent width/color, threshold/top-N changes, bilateral/composed placement. |
| C10 — keys | Continuous/diverging/categorical and effect/confidence 2D mapping/key; threshold/window/palette changes update map and legend together. |
| C11 — flat maps | Equal-scale 2D geometry, hover/pick, polygon/lasso ROI, linked 3D marker/time. Current SurfViewJS paints uniform triangle fills; scalar-painted flat maps are an extension needing a separate oracle. State synchronization and painted output are distinct checks. |
| C12 — publication/restore | Four panels, title/orientation/shared legend, transparent background, high-resolution export, restored camera/layers/time/selection and scientific readouts. |

Exercise volume projection in C02/C08 with independently known sampled values.
Freeze interpolation, depth samples, reducers, affine and fill rules. A render
comparison must not conceal different sampled fields by adjusting color ranges.

Before implementing each stage, expand its case families into required manifest
rows: fixture, product route, actual renderer, camera/material profile, viewport,
initial state, action/checkpoint, expected element masks and metric policy. P1
freezes the complete matrix before P2–P5. Require one uniquely identified receipt
per row; missing or duplicate rows fail. The final verdict is the conjunction of
all required rows and independent visual review, not a sample of gallery images.

The numeric input contract is in `tools/surface-view/parity/frozen-inputs.json`:
151 checkpoints across 28 rows, generated from reviewed fixture and checkpoint
sources with input hashes. This freezes inputs, not a passing result. Missing
route adapters, masks, calibration, and interaction evidence remain failures.
Run `freeze-inputs.mjs` only after reviewing any contract change; do not regenerate
inputs to accommodate a candidate's output.

SurfViewJS `OutlineLayer` draws mesh edges whose endpoint labels differ. Core
pixel comparison uses that explicit compatibility policy, with edge identities
independently computed from fixture topology and labels. A contour connecting
edge midpoints is a different geometry policy and must not be mislabeled as the
same reference output. ROI exterior cases collapse the selected parcels into
one binary region before generating boundaries, so internal selected-parcel
boundaries are excluded.

The reference's sharp threshold edge is also a distinct display policy: it
interpolates shown/hidden signs, placing the boundary halfway along an edge
between a shown and a masked vertex. It does not locate the scalar isocontour.
Keep this compatibility policy separate from ScalaFIM's scientifically sampled
`VertexScalar` interpolation. Neither may silently replace the other.

## Visual proof protocol

### Inputs and capture

Every fixture records mesh coordinates/topology and hashes, frame declaration,
data/seed, palette, thresholds/endpoint policy, layer order, alpha, camera
eye/target/up/projection, viewport, clipping, lights, background, dimensions,
device-pixel ratio, and selected IDs. Camera inputs also state vertical/horizontal
FOV convention, orthographic bounds, near/far planes, aspect handling, viewport
origin/extent and coordinate conversion; record the actual resulting matrices.
Independently project asymmetric landmarks in square, wide and tall viewports,
before and after resize. An absent public camera seam is a capability failure;
adapters cannot fix it through private state or distorted geometry.

Use three named material profiles: observed defaults (document differences),
controlled unlit mapping, and controlled lit anatomy. The latter two carry
explicit matching contracts; observed defaults are not silently normalized into
a parity claim. Record normal generation/interpolation, culling, light frame,
material response, alpha discard/premultiplication, depth writes and ordering.
Orbit folded anatomy and overlapping translucent geometry to test changes in
shading and front/back visibility. Normalization cannot remove a tested feature.

Capture real public viewer APIs and actual framebuffer/native snapshots. Pin
SurfViewJS, its lockfile and Three.js; record candidate/provider SHAs,
browser/GPU/OS/JDK/JavaFX, color-space/tone-mapping settings, antialiasing and
fonts. Do not independently auto-fit, crop, register, recolor or align images
after capture. Resolve coordinate conventions explicitly in the adapters.

Use one installed Playwright backend and a task-owned browser, following the
repository's ownership audit before and after execution. Do not automate the
user's system Chrome profile. Dispose browser/native resources on completion.
Historical native failures remain explicit; diagnostic overrides cannot count
as production acceptance.

### Artifacts for every case and checkpoint

Produce SurfViewJS, ScalaFIM Three.js, ScalaFIM JVM and reference-raster PNGs;
a same-scale side-by-side plate; alpha overlay or blink comparison; error
heatmap; and machine-readable semantic/pixel/resource receipts. Include enlarged
crops of threshold edges, outlines, markers and legends. Time/morph/interaction
cases require named checkpoints and a short matched sequence, not one favorable
frame. Export and live display are separate checkpoints.

Proposed gallery: `docs/visual-qa/surfviewjs-parity/<candidate>/`, with a local
HTML index and artifact hashes. Store bulky raw captures/logs in a
content-addressed bundle and retain its location/digest in the receipt. Failed
comparisons remain available.

### Acceptance

Use the [scenario contract](scenario-parity-harness.md): each required matrix row
has one `ScenarioResult`, retaining product route and actual backend identity.
Missing outputs/required routes, stale artifacts and missing core elements cannot
become `Pass`. Core completion requires clean `Pass`; optional capabilities and
nonrequired native-provider results are reported separately.

1. **Scientific semantics:** exact original surface/vertex/parcel identities,
   frame and layer order, categorical/visibility decisions; numeric readouts
   within declared operation-specific bounds. Test boundaries and immediate
   floating-point neighbors. Declare original-face tie rules for native fill
   and ray differences. Calculate expected identities, visibility, sample values,
   mapped colors and landmark projections directly from fixture inputs without
   candidate mapping/compiler functions. Corrupting shared ScalaFIM mapping must
   fail this independent oracle even if every ScalaFIM renderer agrees.
2. **Pixels and geometry:** measure foreground coverage/IoU, landmark placement,
   interior color error, threshold/ROI contour distance, alpha and occlusion.
   Initial calibration targets: IoU >= 0.995, landmark/contour displacement <=
   one output pixel, and max channel error <= 2/255 for controlled unlit interior
   samples. These are proposed targets, not measured guarantees. Derive
   case-specific antialiasing envelopes from reference repeatability and analytic
   fixtures; freeze them before evaluating candidates. Relaxation requires
   evidence/review. No aggregate score can excuse a missing marker, swapped
   hemisphere or wrong threshold region. Use per-element expected masks for each
   hemisphere, threshold island, contour, marker, label and legend, generated
   independently or frozen from reference artifacts. Report presence/coverage
   and local errors; never compare only intersecting surviving foreground or use
   candidate-derived exclusion masks. Bound and record antialiasing exclusions.
   Deleting the smallest marker or a complete thin contour must fail automatically
   even when global IoU exceeds 0.995.
3. **Appearance:** independently inspect every plate at native size and its
   crops for anatomy, shading, borders, labels, legends, spacing, clipping,
   alpha and publication legibility. Lit comparisons require a declared
   lighting/color contract and separate bounds; unlit success does not prove
   shaded-anatomy parity. Record reviewer, exact SHA, case, finding and verdict.
4. **Interaction/resources:** replay deterministic pick, orbit, resize,
   threshold/order, time, morph, ROI and restore actions. Check state/readouts
   and pixels. Keep current upload invariants; set latency/memory budgets on
   declared hardware from baseline measurements before implementation. Preserve
   stricter existing consumer budgets. A slow snapshot is not interactive proof.

Validate the comparator with deliberate hemisphere swaps, camera shifts, missing
outlines/markers, changed threshold equality, stale legends, reversed layers and
broken alpha/occlusion. Each relevant case must fail. Background agreement and a
generous global mean are insufficient. Distinguish reference/candidate mismatch
from violation of an independent analytic oracle.

## Checks and current evidence

Existing commands are below. Run tests in bounded batches on memory-limited
hosts; use the constituent alias targets rather than accumulating every JS suite
in one JVM. Prefer `python3 tools/build/sbt-warm` for repeated local gates and
preserve raw logs/exit status. An Intaglio pin change also needs affected design
and image-view consumer tests on both platforms.

```text
sbt surfaceViewConformance
sbt surfaceViewExamplesJVM/test surfaceViewExamplesJS/test
sbt scalafimCompileAll
sbt surfaceViewAdmissionJVM
sbt surfaceViewVisualQaJVM
sbt surfaceViewExamplesJS/fastLinkJS surfaceViewThreeJS/fastLinkJS
```

The visual aliases are prerequisites, not the new comparison harness. P1 must
implement/document one reproducible capture/comparison command before claiming
gallery results; a proposed command is not an installed command.

| Evidence | State at plan creation |
| --- | --- |
| Source/API comparison | Completed at the two audited SHAs. |
| Retained scalar/legend source | Located at `c14b36ac`; not in main. |
| Direct source-patch applicability | Fails on 11 paths; no changes applied. |
| Current-base recovery, fresh tests/native comparisons | Pending. |
| Side-by-side gallery and adversarial comparison checks | Pending. |
| Display parity qualification | Pending; no passing claim. |

The recovery/composition/annotation work below supersedes this initial evidence
table. Next: land the reviewed mapping/linked-figure candidate after its Intaglio
dependency is published, then perform the complete P6 qualification matrix.

## Recovery landing checkpoint

Recovery source `423991bd70439ae1700db29c5d7790432f44255a` passed its affected
platform tests and final native visual/resource gates and received independent
source review. The [recovery gallery and evidence](../visual-qa/surfviewjs-parity/recovery-20261014/README.md)
retain the earlier aggregate compile and JavaFX failures alongside the successful
focused reruns. The new harness is a P1 foundation; 151 frozen checkpoints are
not 151 passing receipts. Qualification additionally needs trusted reference,
environment and artifact-role bindings before the matrix can admit a pass.
P2 composition source `e2df2a9e83b9792b06837881aeab8ccd4f263d47` passed focused
JVM/JS gates, independent source review, 3,161 native JavaFX pick checks, and
nine native Three.js browser checks. The [composition gallery](../visual-qa/surfviewjs-parity/composition-20261014/README.md)
retains paired native/raster images, camera-isolation images, raw logs and
provenance. This establishes view/instance rendering and resource behavior;
it is not the full SurfViewJS C04 cortex appearance comparison. The complete P6
matrix remains outstanding; later checkpoints record P3–P5 work.

## Annotation landing checkpoint

P3 source `68dc4732fa30a26130d64a210da5c272800738af` passed shared surface-view
193 tests on each JVM/JS, raster 41 on each, JavaFX 51, and Three.js 41. Native
probes pass 3,824 JavaFX picking checks and 29 browser checks, including network
line/tube glyphs, alpha edges and both transparent-label submission orders.
Independent source review accepted the correction after the label-overlap defect
was reproduced and fixed. The [annotation gallery](../visual-qa/surfviewjs-parity/decorations-20261014/index.html)
contains same-scale native/raster comparisons and retains the adverse browser
filtering and JavaFX label-overlap captures. This qualifies the bounded synthetic
annotation implementation, not the full C05/C06/C09 SurfViewJS cortex matrix.
P4 mapping/visibility and P5 linked figures are underway; final core parity remains
unqualified. Upstream Intaglio display primitives are a local candidate, not yet
a published dependency.

## Mapping and linked-view checkpoint

The isolated P4/P5 candidate now implements the core mapping/visibility and
linked-figure APIs. Its [gallery and reproduction notes](../visual-qa/surfviewjs-parity/mapping-linked-20261014/README.md)
retain side-by-side captures, exact input/module hashes, raw logs, independent
review, and failures. The evidence is deliberately narrower than the stage exit
criteria and does not admit a full C02/C03/C07/C08/C10/C11/C12 family pass.

- Mapping/visibility: sixteen fixed interior-color regions pass against
  SurfViewJS. Fifteen of sixteen native full frames pass; the mixed-gradient
  case has max channel error 3 versus limit 2. Raster full frames retain
  antialiasing/silhouette failures. Four translucent overlap/reorder/zero-alpha
  cases pass through the raster route; native Three.js explicitly refuses
  translucency. Twenty actual JavaFX snapshots equal the raster pixels they
  publish. Native JavaFX Scene3D admission remains unchanged.
- Linked browser replay: all eight time/morph/selection/camera/restore checkpoints
  have silhouette IoU 1 and max channel error 1. Restored surface, marker and
  flat-map captures are pixel-identical. Temporal source restoration invalidates
  stale same-ID cache entries. The replay restores and renders saved navigation,
  rather than reconstructing fixture cameras.
- The actual JVM linked-host replay also passes: fixed vertex pick, fractional
  playback, synchronized selection, morphing, nondefault camera restore and
  real 384→512 resize-back. Every ImageView snapshot equals its published raster;
  restored and resize-back images are byte-identical to the saved endpoint.
- Flat maps: all seven browser stages are pixel-identical to the reference,
  including polygon/lasso ROI, resize, time and selection. The JVM host passes
  actual interaction checks but retains strict text/color pixel failures.
  Both reference and candidate have overlapping labels in the lasso specimen.
- Publication: actual 1200×900 and 2400×1800 transparent figures preserve
  physical layout, camera aspect, four selection glyphs and scalar/bivariate
  mapping-derived keys. Legends bind to displayed composition instances, not
  legacy single/bilateral layout. Panel labels describe the synthetic plane;
  the expansion is not anatomical inflation.

Current gates pass: shared surface-view 230 tests on each JVM/JS, raster 49 on
each, Three.js 50, JavaFX 51, examples 12 JVM/11 JS, and connectivity 2 on each.
Intaglio core passes 712 JVM/703 JS; its design consumers pass 447 JVM/446 JS and
image-view passes 42 on each. `scalafimCompileAll` passes warning-clean on both
platforms using the explicit reviewed local Intaglio override. Updating Intaglio
also required the existing design graphics domain annotation to become
`DiscreteDomain[String]`.

At the original capture checkpoint, Intaglio
`13747a3a8bf75fd788ff68e1c979d6c84f740605` was independently reviewed and
locally merged, with remote publication pending authorization. The ScalaFIM
candidate pin targets that exact revision. P0–P3 were already locally merged at
`53feab621013d0c148a5b5d2567408a153354218`. The subsequent publication and
verification checkpoint below supersedes that dependency blocker.

Full frozen-matrix qualification, cortical interaction/resource budgets, strict
raster antialiasing, JVM/browser glyph equivalence, and the remaining native
mixed-gradient pixel discrepancy are still open P6 evidence/implementation work.

## Published dependency verification checkpoint

With explicit user approval, reviewed Intaglio
`13747a3a8bf75fd788ff68e1c979d6c84f740605` was fast-forward pushed to
`canardlapin/intaglio` main; a fresh remote read confirmed the exact SHA.
ScalaFIM candidate `221948858884d7bc3d9175a9fb0330211ffed96f` then resolved
that public source pin into a clean sbt dependency clone with
`scalafim.intaglio.build` absent. The original visual captures retain their local
override provenance.

The default-pin gates pass: 833 JVM tests and 830 Scala.js tests across the
affected surface viewers, examples, connectivity, design and image-view
consumers, plus `scalafimCompileAll` and `surfaceViewThreeJS/fastLinkJS`.
The rebuilt browser module has SHA-256
`78e61d3954506bceaaaa7a4f95a077c5d390559aee18b424757859cae134015c`,
identical to the module used for the retained visual comparisons. Raw logs and
exact command/exit receipts are linked from
[the publication verification record](../visual-qa/surfviewjs-parity/mapping-linked-20261014/publication-verification.json).
Upstream sbt settings/lint warnings are retained; no ScalaFIM source compiler
warnings were emitted. The earlier design JS count was a documentation error:
both the original consumer log and this rerun report 446 passing tests.

This removes the dependency-publication blocker for the authorized local merge.
It does not qualify the full P6 matrix or constitute ScalaFIM remote publication.

Local landing completed by fast-forwarding main from `53feab62` to
`7ec313d769f9fd494d67618e6bf4109491652eab`, including the P4/P5 implementation,
paired evidence and publication receipts. Existing immutable Mote operations
were preserved byte-for-byte. ScalaFIM has not been pushed; the parity epic
remains open for the P6 work listed above.

## Qualification execution checkpoint, 2026-10-15

The user authorized the remaining five workstreams, including final ScalaFIM
publication. The isolated candidate reconciles local `606b49b3` with upstream
`1616c630` at merge `dc101ea0`, retaining the published Intaglio pin. The scene
codec accepts canonical two-sided thresholds and the legacy V8 representation,
rejects mixed representations, and retains strict unknown-field validation.

The new `tools/surface-view/parity/run-qualification.mjs` is an endpoint capture
runner, not a passing qualification gate. Its contract loader checks the frozen
source hashes and all 151 parent checkpoints, expands declared view/projection
and profile combinations, and records input ambiguities in
`tools/surface-view/parity/qualification-errata.md`. Unsupported actual-host
routes and missing replay/mask evidence remain failures. The original frozen
inputs are unchanged.

The first twelve controlled-unlit C01 browser captures use both external
fsaverage5 hemispheres (20,484 vertices and 40,960 triangles). Camera matrices
agree with the public reference within Float rounding. All native silhouettes
have IoU 1; seven native images retain strict channel-error failures at isolated
fold/depth boundary pixels. Raster images retain antialiasing failures. These
observations do not qualify the full C01 family, other profiles, interactions,
or JavaFX native rendering.

The opaque compositor now avoids collecting and sorting hidden fragments while
preserving exact scientific picks, clipped/partitioned samples, same-instance
duplicate tolerance, complete coplanar tie ordering, and transparent-background
behavior. Translucency, nonempty decorations and explicit capacity checks keep
their checked collection path. Independent source review accepted the bounded
change. Focused gates pass on both JVM and Scala.js, including ten new raster
regressions; actual JavaFX tests and example linking also pass.

The twelve repeated cortex captures are byte-identical to the previous raster
images. Warm render time decreased from 591–873 ms to 169–295 ms in the retained
fast-optimized browser build; this remains above the proposed 100 ms fallback
target. Browser heap deltas are diagnostic allocation observations, not retained
memory or leak qualification. The next check isolates repeated vertex-color
composition. Resource budgets and native GPU-completion latency still require
qualification. No P6 family is marked complete, and this candidate has not yet
been pushed.

## Expanded qualification checkpoint, 2026-10-15

The frozen matrix expands to 636 variants across 151 parent checkpoints. This
checkpoint adds evidence; it does not declare full-family completion. The
reference runs from a clean isolated fb60f370 checkout because the user's
original SurfViewJS checkout has unrelated edits. Its external fsaverage5
hemispheres remain local, checksum-bound inputs.

Production improvements include checked explicit perspective/orthographic
frusta and up vectors, an explicit Three depth-precision policy, linear affine
volume sampling, normal-directed contour offsets, matching 12-by-12 marker
sphere geometry, network node visibility/depth policy, and consistent declared
overlay order in rendering and picking. Opaque raster work now avoids hidden
fragment collections, repeated eligible vertex-color composition, and redundant
compositing at undecorated pixels. Translucency, transparent backgrounds,
explicit capacity limits and scientific provenance retain checked paths.

Six native volume endpoints are byte-identical to the public reference, with
independent affine-field readouts. Six actual multi-panel browser endpoints
pass ray-derived surface/view identity, resize-back and disposal checks; strict
fold/edge pixels remain failures. Revised sphere geometry brings the multiple,
hover and selected annotation cases to maximum channel error 3 (strict limit
2), while geometry-change differences remain under investigation.

The latest actual JavaFX batch captured 54/54 cases. Every fallback ImageView
snapshot equals its published raster, five warm images agree, resize-back is
exact and disposal releases the presented frame. The actual loaded JVM
artifacts are hashed in runtime.json. Native JavaFX refuses these controlled
profiles explicitly; fallback success does not imply native admission. Warm
raster medians span 69-93 ms for volumes, 512-840 ms for multi-panel cortex,
104-505 ms for contours, 109-160 ms for annotations, 58-79 ms for dynamics and
116-190 ms for networks. The 100 ms fallback target therefore remains unmet
for several realistic workloads.

A browser batch was interrupted by ENOSPC. Its 23 completed raster images are
byte-identical before/after the sparse-decoration optimization. Seventeen
completed contour cases improved from 384-914 ms to 223-689 ms warm medians.
The incomplete capture and its failure log are retained; it is not counted as
a completed batch. The runner now supports bounded offset/limit batches and
retains exact runner source beside hashes.

A separate realistic-cortex lifecycle probe completed 20 same-page
create/render/resize/dispose cycles. All owned WebGL contexts and host resources
were released, warm uploads were zero and warm resource counts were stable.
Retained JS heap grew 482,512 bytes against a fixed 8 MiB budget after warmup;
reference calibration had to stay within an independently fixed 2 MiB range.
This measures JS heap and explicit lifecycle release, not detached backing
stores, GPU memory, native RSS, warmup retention or long-run leak freedom.

Independent review identified two material contract conflicts. C05's reference
halo is a full-width translucent stroke over its core, while ScalaFIM's
documented halo uses separate side bands. C10's custom ticks, separate key
dimensions and categorical/bivariate viewer keys exceed the pinned public
reference route. Exact failures are preserved in qualification-errata.md;
these cannot be repaired through broader pixel tolerances. Supported public
exports and additional ScalaFIM capabilities need separate evidence.

Remaining qualification includes independent per-element masks and mutation
controls, calibrated edge envelopes, full actual-action replay, the uncaptured
variants, publication/key routes, remaining visual discrepancies and resource
budgets. Source improvements may be reviewed independently, but the P6 clean
Pass and full display-for-display parity claim remain open.

The follow-up C06/C08 anchor probe now distinguishes geometry from pixels:
all eleven captured ScalaFIM sphere centers match the independent morphed
source-vertex oracle within 1e-4 world units. Three actual SurfViewJS marker
transforms remain at base vertices, producing up to 4.24/8.47 world-unit error
at midpoint/full expansion. Original failures remain retained.


## Reviewed evidence closeout, 2026-10-15

Production `37df543f` received independent bounded source review. Final
`scalafimCompileAll` passes warning-clean after the documented exact pinned
provider preparation. The nearest-only fsLR qualification consumer now explicitly
rejects an unexpected interpolated contribution; fixture tests pass on JVM and JS.
Affected display gates and all 48 Node qualification tests pass; exact commands,
counts, retained failures and source identities are in the
[qualification gallery](../visual-qa/surfviewjs-parity/qualification-20261015/README.md).

The explicit public-client morph update passes all eleven measured anchor checks
and reduces the geometry-change native maximum channel error from 168 to 3.
The original stale-marker reference behavior remains retained. All 54 current
JVM fallback captures pass their own host lifecycle/presentation checks; 53 have
runtime-bound reference comparisons and all retain strict pixel differences.
The missing crosshair-label adapter is reported separately. Six browser flat-map
endpoints are exact; six JVM static semantic checks pass but their strict pixel
comparisons fail. Independent review corrected an overbroad C11 status: complete
qualification remains incomplete, including reference calibration, linked-marker
anchor parity, physical event replay and per-element masks.

The ledger accounts for all 636 expanded variants with 162 distinct paired
endpoints across historical/current builds. This is coverage accounting, not
162 qualified variants. No P6 family is marked complete. The candidate may be
published as a draft for review; the full-matrix merge gate remains unsatisfied.

### Rounded contour compatibility and warm reuse (2026-10-15)

The [new comparison appendix](../visual-qa/surfviewjs-parity/rounded-contours-20261015/README.md)
adds opt-in rounded, pixel-width contour strokes with translucent full-width
halos. Six source-defined synthetic specimens pass interior geometry, alpha,
overlap and occlusion checks on native Three.js and the raster backend; strict
full-frame edge comparisons still fail. Actual native picks and cleanup pass.
JavaFX explicitly refuses the native policy and uses its raster host.

Native decoration reuse preserves byte-identical cortex captures and reduces
median render-plus-PNG time from 172–188 ms to 86–90 ms in three fixed views.
Raster latency remains above target. Corrected scalar-key exports check range,
palette interiors and threshold metadata while retaining visible publication
and transparency differences. These supplementary receipts do not complete a
frozen matrix family or waive the P6 merge gate. Draft PR #61 contains the
reviewable candidate; final source and runtime hashes accompany the appendix.
