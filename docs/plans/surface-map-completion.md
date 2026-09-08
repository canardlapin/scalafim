# Surface map plan completion audit

Status: complete locally, 2026-09-08. All four stages of
[surface-map-semantics.md](surface-map-semantics.md) are implemented and have
passed their required acceptance checks. The
[requirement audit](surface-map-final-audit.md) maps each numbered requirement to
its implementation and direct evidence. The
[final receipt](../benchmarks/receipts/surface-map-final-2026-09-08.json) binds the
current source and build hashes to the final gates and retained native evidence.
Changes in this ScalaFIM checkout remain uncommitted.

| Plan area | Completed behavior | Acceptance evidence |
| --- | --- | --- |
| Stage 1: facewise data | Owned, frame-major fields with exact ordered topology; typed scalar, label, mask and packed-color constructors; original IDs and face readouts | JVM/JS topology, ownership, migration and composition tests; native face and cortical pixel/pick checks |
| Stage 2: interpolation | Explicit legacy color, face constant, nearest sample and scalar policies; perspective-correct scalar evaluation, invalid contributors, ordered mixed layers and declared lighting | Analytic raster/native composition, nonlinear and boundary oracles; full cortical WebGL and bounded JavaFX matrices |
| Stage 2: JavaFX feasibility | Constant swatches, six-cell nearest partition, evaluated scalar lookup limitations, and opt-in bounded geometric/atlas composition | Native minification, clipping, normals, morphs and original-ray probes; explicit approximation/resource rejection; repeated benchmarks at both requested sizes |
| Stage 3: mappings | Continuous/diverging/split descriptors, independent visibility and scaling, checked overrides, canonical identity and opaque-colorizer boundaries | Pinned Intaglio core/SVG: 527 JVM/JS tests; consumer mapping, native shader and scene identity tests |
| Stage 4: legends | Continuous/split/categorical/manual legends, units and missing keys, effective mapping binding, shared-scale checks and scene revision 6 persistence | JVM/JS publication/persistence tests; 12 native SVG layout cases, eight exact opaque swatch pixels, manuscript/poster gallery |
| Cross-platform integration | Portable contracts, backend admission and runnable examples | 601 conformance tests; 23 JVM and 17 Scala.js example tests |
| Native JVM integration | Raster/JavaFX admission, visual rendering, interactions and actual camera rays | 96 admission cases with three repetitions each; all four visual QA probes; 28 native ray cases |
| Native WebGL integration | Scalar and mixed layers, network geometry, picking, clipping, retained uploads and lifecycle | 48 admission cases, 64 composition cases, 28 scalar-edge cases, four clipping cases and the independent nearest-lighting regression |
| Cortical validation | Five display modes, orthographic/perspective, both lighting states, sizes, time/style updates, morphs and resize | WebGL: 252 stages, 16,128 picks; JavaFX: 60 static cases and 86 unlit retained-update cases, 9,344 picks |
| Documentation and provenance | Capability limits, host commands, current completion records, source/artifact hashes and native galleries | Final receipt, [viewer guide](../surface-viewer.md), [WebGL gallery](../visual-qa/surface-cortical-webgl/README.md) and dated receipts below |

## Native results and limits

The [cortical WebGL receipt](../benchmarks/receipts/surface-cortical-webgl-2026-09-08.json)
records all 20 configurations on Chromium 151 / Apple M3 Max Metal: 10,592,698
checked interior pixels, maximum channel error 3, minimum mask IoU 0.9927939174,
and 16,128 original-ID native picks. Camera, unchanged-state and resize actions
upload no resources; frame, opacity and threshold changes upload no geometry.
All test-owned native resources and browser processes are closed.

The renderer fixes fractional viewport alignment, keeps interpolated attributes
inside partially covered triangles, preserves legacy nearest-corner lighting,
and conditions the occupied native depth interval. The last correction resolves
a measured perspective facewise error while preserving screen positions, reached
clipping planes, cross-slot depth comparisons and the scientific camera.
Antialiased occlusion and display-cell boundaries retain their raw differences
separately from the strict interior budget.

A measured Chromium SwiftShader multisample depth defect is rejected by a typed
startup capability check. Two focused cortical checks pass on a fresh software
WebGL2 context without antialiasing. This is a tested fallback, not an all-device
or complete software-matrix guarantee; the portable raster remains available.

The [final JavaFX cortical receipt](../benchmarks/receipts/surface-cortical-final-javafx-2026-09-08.json)
records maximum strict channel error 4 and minimum mask IoU 0.9999714245. Scalar
and mixed composition use `createApproximate` with explicit color, triangle,
byte and subdivision limits. Unsupported combinations fail before allocation.
The one- and two-axis lookup investigations do not establish a general supported
lookup compositor; the bounded geometric/atlas path is the admitted workaround.

Repeated JavaFX admission includes [32,768 vertices](../benchmarks/receipts/surface-map-benchmark-32768-2026-09-08.json)
and [163,842 vertices](../benchmarks/receipts/surface-map-benchmark-163842-2026-09-08.json):
870 rendered stages and 55,680 native picks. The larger workload also records 18
checked cold rejections for lit scalar subdivision depth. These are explicit
budget outcomes, not a general vertex-count limit. Some large lit categorical
updates take several seconds; rendering acceptance does not promise interactive
latency. Accounted generated bytes exclude JVM objects, driver allocations and
render targets.

## Earlier evidence retained

- [JavaFX bounded lowering](surface-bounded-javafx-review.md), [cortical semantics and reference plates](surface-cortical-semantics-review.md), and [native viewport integration](../surface-viewer.md#javafx).
- [Three.js fragment semantics](surface-three-fragments-review.md) and [independent native depth admission](../benchmarks/receipts/surface-three-depth-admission-2026-09-08.json).
- [Publication legend acceptance](surface-publication-legend-review.md), [legend foundation](surface-legend-review.md), and [example checksum diagnosis](surface-example-viewport-review.md).
- [Post-benchmark JVM integration](../benchmarks/receipts/surface-map-benchmark-integration-2026-09-08.json).

Earlier receipts describe their dated source snapshots; their historical
“remaining” fields are superseded by this audit. Intaglio's publication commit
`55658abfbbfed0c9a36ab612b38bd8f0677bc158` is pinned and verified live on its dedicated
branch. This work does not claim an Intaglio main-branch release, exact edge
pixels on every device, new areal resampling, or implicit face-to-vertex conversion.
