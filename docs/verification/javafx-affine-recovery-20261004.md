# JavaFX affine recovery and sampler proposal

Mote `bd-01M241Y5FPND8TPHTJQVHJWW7K` is resolved as a bounded recovery and
explicit upstream capability proposal. The exact code candidate is
`3bb30729c2511d9d110db217851c5d366aabeafa`, on the queue worktree above
`36151a6705281f9f9523d18bc7e39df059aff674`. Its canonical integration base is
`06ecaac9a6158e1a04133cba05bb6dbacb9550b4`. This receipt does not land the queue
branch, install an OpenJFX fork, change a dependency pin, or approve a product
runtime. Canonical main advanced independently to `41815b7042f60e6b09b07e136b9969d42302ab0a`
during qualification; those changes were left intact.

The recovered implementation adapts historical `066be6c1` to current V1
already-composited vertex-color packets and the current provider pins. It does
not import the newer scalar/fragment or prepared-render-plan API family. The
existing `LegacyTriangle`/`NativePhong` default remains selected. Affine
midpoint and adaptive encodings preserve the original opaque vertex field,
cyclic anchoring, original face/vertex identity and barycentric readouts.
Resource counts use checked arithmetic before allocation. Layout-changing
updates construct a complete replacement before swapping it into the mounted
scene; refused updates retain the old view and authoritative picking plan.

`WorldVertexLambert` is an explicit experimental vertex-lighting estimand,
separate from native per-pixel Phong lighting. Production affine backend
creation returns typed `SamplerUnqualified`. The concrete
[per-map sampler proposal](../plans/javafx-affine-sampler-capability.md)
requires base-level linear sampling without mipmaps, centroid MSAA texture
interpolation, policy-aware cache identity and explicit unsupported outcomes.
The retained process-wide two-class JavaFX patch is a diagnostic positive
control, not a per-binding public implementation or production dependency.

## Executed checks

The [machine-readable receipt](javafx-affine-recovery-20261004.json) binds source
hashes, the clean candidate, all 64 immutable executed classpath entries,
runtime/module hashes, commands, terminal exits, raw log hashes, input hashes,
framebuffer reports and descriptive update measurements. Full evidence remains
under `/private/tmp/scalafim-mote-queue-evidence`.

Java 17 on macOS arm64 passed the complete `surfaceViewConformance` constituent
gates in bounded batches: 722 tests passed and two existing opt-in Scala.js
timing tests were skipped. The skips require `SCALAFIM_JS_TIMING=1` and
`SCALAFIM_JS_REAL_TIMING=1`; they are not native or performance qualification.
`scalafimCompileAll` passed for both JVM and Scala.js without compiler warnings.
The final native suite passed 35 tests, including combined geometry/color/lighting
updates, checked overflow, resource refusal and independent atlas samples.
Unchanged shared/viewer suites were carried forward by an explicit source-tree
comparison. An initial SurfaceJS test compilation exhausted the 3 GB warm
server heap after 280 SurfaceJVM tests passed; its fresh 4 GB retry passed
223 tests with the two declared skips. That compiler failure is retained.

Fresh hidden native runs used Java 25.0.1 and exact JavaFX 24.0.1 macOS arm64
modules on observed `com.sun.prism.es2.ES2Pipeline`, with fallback disabled and
material/shader origins asserted. The Apple driver reports `2.1 Metal - 88`;
that string does not establish Prism Metal admission. No visible Stage or
browser was started.

The unchanged independent original-triangle/pixel oracle passed 384/384
analytical cases and 256/256 cyclic/packing comparisons: both affine encodings
at width 256, plus adaptive at widths 4096/4092 with Unlit/Default/Soft lighting.
Cases cover diagonal/saturated RGB, sparse/dense ramps, front/oblique planes
and AA off/on. The frozen two-channel-level budget was unchanged. Worst
footprint excess was 1.222223; worst AA-off center error was 1.451389.
The closed-form oracle check retained residual below 5.685e-14.

The named negative controls fail correctly. Patched legacy encoding passes
24/48 cases, with diagonal/RGB failures and eight permutation failures; its
worst AA-off center error is 21.482833. Stock sampling with adaptive encoding
passes 39/48, with dense-ramp failures and nine permutation failures; its worst
footprint excess is 24.088769. Their oracle CLI exits are 1, distinct from the
successful native image generation exits. Historical stock-Metal rejection
remains separately preserved and is not reversed.

All six beta/FIR × Unlit/Default/Soft dense consumer-fixture runs pass at the
same exact candidate and frozen closure. They consume digest-checked original
PLS exported V1 geometry and original Double beta/FIR arrays: 274,513 vertices
and 549,018 source faces. Finite vertex mapping is independently checked
against the original export. Each run verifies 32 updates/restorations over
palette, cutoff, opacity, map, morph, lighting, camera and background: every
change alters pixels and every restoration is pixel-exact. Actual native ray
picks agree at 1,200 initial and 1,200 restored sampling locations per run,
including original source face, vertex, surface, barycentrics and numeric layer
readouts. Maximum barycentric error is 7.994e-6 against the unchanged 1e-3 bound;
16 selection readouts per run also agree. These picks revisit initial/restored
locations; they do not sample every altered state.

Separate native synthetic dispatch probes pass 3,300 adaptive and 1,200 midpoint
child picks, including split/collapse/anchor changes and refusal preservation.
They also check mounted snapshot resizing, externally resized/bound viewports,
and incremental versus cold combined updates. These dispatch counts are not
additional actual ray-pick counts.

## Qualification limits

The dense 2 GB diagnostic failed with Prism PNT allocation OOM and a subsequent
restoration failure; it remains rejected. The six passing dense runs used
6 GB. This is diagnostic feasibility, not a deployment heap guarantee. Receipt
timings and FX-thread allocation counts are descriptive shared-host measurements;
retained mesh/atlas bytes exclude Prism internals, total process heap and GPU
allocations. Ordinary update performance is owned by the next retained-atlas
Mote and is not admitted here.

The consumer evidence is exact PLS numeric/plan fixture consumption by this
ScalaFIM harness, not a packaged interactive PLS Neuro application admission.
Current V1 maps nonfinite scalars to transparent, while the historical exporter
used an invalid-value gray; finite agreement does not establish identical
invalid-value presentation. The planar color oracle does not supply a complete
cortical occlusion/pixel golden. No Windows/Linux native, perspective/device
matrix, stock runtime, sustained performance, deployment or visual-product
approval is inferred.

An independent read-only expert review approved code SHA `3bb30729` for this
bounded proposal/refusal outcome, verified the 64 frozen classpath hashes and
six cortex receipt/log digests, and found no remaining code blocker. Its
conditions—final planar binding and required conformance/compile gates—passed.
Native execution was author-run; the reviewer did not rerun the native tests.
