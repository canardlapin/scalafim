# Volume-to-surface engine parity, 2026-09-30

Mote: `bd-01M37FQGV8ZPT30X37TA4MM8R7`, remaining item (1).
Base: `7989608d32ed8283ed11f5ff3df54c7c7761a23e`.
Branch: `surface/engine-parity-20260930`.
Worktree: `/private/tmp/scalafim-volume-surface-parity-20260930`.

This is a bounded engine-qualification slice. GIFTI metadata, actual group-space-to-fsLR admission, provider/consumer publication, and real anatomical/display asset correspondence remain separate work.

## Contracts

- The surface sampler performs nearest-voxel point lookup. Nearest, Average, and Mode name depth reducers. The spatial compiler supports nearest or trilinear interpolation and averages valid depth samples; its corresponding eager comparator therefore uses Average for multi-depth paths.
- Trilinear interpolation normalizes retained in-grid/in-mask corners. Coverage is the sum of their original weights, before normalization, averaged over all requested depth points. Fractional-depth samples do not establish voxel-overlap ribbon weights.
- Uncovered spatial sparse rows evaluate to zero; eager projection uses its declared fill policy. Coverage and sample counts distinguish that zero from an observed zero.
- GPU projection supports one midpoint, nearest lookup, no source mask, and minimumSamples=1. Values use float32. Finite double values outside float32 range and arbitrary double-value equality are outside this qualification.
- The existing nonfinite policy remains: nonfinite samples count toward minimumSamples and propagate through aggregation, while the tally reports them separately from accepted finite samples.

## Frozen independent specimens

`VolumeToSurfaceParitySuite` checks a multiaffine polynomial against its analytic value on an explicitly transformed oblique, anisotropic, shifted grid; an impulse with a hand-calculated corner weight; masked-corner and volume-edge normalization/coverage; and public compiled nearest/multi-depth and trilinear routes. Grid-to-world expectations are written independently of the lookup conversion.

`ThreeVolumeProjectorSuite` uses a 3 x 2 x 4 asymmetric ramp (x + 10y + 100z), half-voxel offsets of 1e-8, exact lower and upper support boundaries, valid zero, NaN, and +/-2^32 coordinates on multiple axes. The injected test runtime independently uses WebGL's x + width*(y + height*z) texture layout; it is a transport oracle, not native shader evidence. A test-only browser export invokes the actual production API with Three.js and WebGL for 13 axis, boundary, and far-outside vertex observations.

## Reproduced discrepancies

The unchanged projector failed all three transport regressions. Its non-cubic ramp returned 1 where the expected value was 102, and a NaN texel appeared at the wrong vertex. ScalaFIM canonical storage is Z-fast; WebGL 3D texture storage is X-fast. The old direct upload used the wrong layout. Fractional coordinate upload also rounded double coordinates into float32 before nearest selection, changing lookup at half-voxel ties and the upper support edge.

Baseline log: `/private/tmp/scalafim-execution-20260929/logs/volume-surface-parity-before.log`, exit 1, 0/3 passed. Two separate project-load failures preceded this executed regression; the runner preserved each retry log and metadata. They are infrastructure failures, not projection-test evidence.

The actual production API also failed the frozen browser baseline: Chromium 140.0.7339.186, Three.js r185, ANGLE SwiftShader, 7 value/count/quality mismatches across 10 vertex observations, no console/page errors. Receipt: `/private/tmp/scalafim-engine-parity-20260930/webgl-baseline.json`; linked baseline SHA256: `408d5e929c5c456a7e3b6a53ed315ea95714463344c0e6802a3fcfd087278d50`. This is software WebGL evidence, not hardware-GPU admission. Initial harness attempts failed to load the test export; they ran no shader qualification and are not included in the baseline result.

After the X-fast upload repair alone, the axis and zero/NaN regressions passed, while the half-voxel test still returned 11 where 10 was required. Log: `/private/tmp/scalafim-execution-20260929/logs/volume-surface-parity-axis-only.log`, exit 1, 2/3 passed. This separates the two discrepancies.

The axis/tie repair resolves nearest indices in double precision before uploading integer texel coordinates, uses the shared midpoint arithmetic convention, and packs source texels into X-fast order. The shader fetches the chosen integer texel. A subsequent boundary audit reproduced Long-to-Int wrapping in all three nearest kernels: coordinates such as +/-2^32 were accepted as in-grid. The original eager and spatial kernels already had this defect, and the first GPU tie fix inherited it. `volume-surface-wide-before.log` (exit 1, GPU 3/4 passing) and `volume-surface-wide-before-jvm.log` (exit 1, spatial/eager 4/7 passing) independently demonstrate the rejected far-outside samples being read and assigned coverage one. All three nearest boundaries now retain Long indices until finite/in-grid checks succeed, then narrow only proven valid CPU indices. This affects coordinate admission, not the source-value NaN/minimumSamples policy.

## Validation

The tested source commit is `dbcce3043a809547d6103764de2f1d3f385f20e1`. At the original candidate `9ba0f367` freeze, its receipt was a documentation-only followup; production/test sources and module README remained identical to that source commit. At that freeze, no local-main integration or remote publication had occurred. The later integration evidence is recorded below.

| Gate | Result | Raw log and metadata |
| --- | --- | --- |
| `surfaceJVM/test` + `spatialJVM/test` | 166 + 224 = 390 passed; exit 0 | `/private/tmp/scalafim-execution-20260929/logs/volume-surface-parity-rev2-jvm.log` and `.meta.json` |
| `surfaceJS/test` + `spatialJS/test` + `surfaceViewThreeJS/test` | 132 + 199 + 18 = 349 passed; exit 0 | `/private/tmp/scalafim-execution-20260929/logs/volume-surface-parity-rev2-js.log` and `.meta.json` |
| `scalafimCompileAll` | exit 0, no warning/error markers | same JS log and metadata |
| Actual Three.js/WebGL production API | 13 observations, zero value/count/quality mismatches, no page/console errors | `/private/tmp/scalafim-engine-parity-20260930/webgl-rev2.json` |

The runner serialized builds through the existing shared sbt lock and bounded each invocation with a 3 GiB heap and four active processors. JVM and JS ran in separate invocations; `scalafimTestAll` was not run. The JVM invocation also produced a NoModule test bundle for the actual browser probe; that frozen bundle was copied before the subsequent JS test invocation changed linker output. The observed toolchain was sbt 1.11.7, Scala 3.7.4, Homebrew Java 25.0.1, and Node 26.7.0.

No failed or skipped test summaries or warning/error markers appear in the complete final logs. The word "ignored" occurs in ordinary test names and is not a skipped-test marker. JVM raw-log SHA256: `dfef6759b852037bd114462673dff0a592e2ab19b83eb1e7a7ad311777510bc0`. JS/compile raw-log SHA256: `56910f8575ecb3afd3c4a35f6c6ae8ed067047f5a30e3c5b13fdadb04bb61b2a`.

The final actual browser run used Chromium 140.0.7339.186, Three.js r185, and ANGLE's SwiftShader Vulkan software device, matching the baseline environment. Frozen bundle `/private/tmp/scalafim-engine-parity-20260930/rev2-main.js` SHA256: `858ba61d6fc393b5ba314c6429146a2e8a174c207441461fb2f9616bce1d2fd5`. Browser receipt SHA256: `1acbe85cc66ce069350193e38c5a1f1a7e78cc52aaa4d56aea1368246877b6ed`. Browser, context, pages, and loopback server closed in the harness's finalizer; the before/after automation guard audit was clear. This qualifies the software WebGL transport and shader path for these specimens, not hardware GPU support, rendering appearance, real native update cost, or performance.

The loaded source-provider closure contains 11 staged checkouts, all tracked-clean at capture, including both transitive Gale revisions. `/private/tmp/scalafim-engine-parity-20260930/provider-closure-rev2.json` binds their paths/HEADs to the tested source and logs; SHA256: `9866266628d0eebc0baa4b7f6791a5410000df7a312034606626e4b1d4ab3989`. No sibling-source build override was used.

`/private/tmp/scalafim-engine-parity-20260930/evidence-manifest.json` binds source-file blob IDs and SHA256s, final gate outcomes, and baseline/final artifact digests. Its SHA256 is `a32e109e53c381e2f404a0d863fa8dba4678e7b6a34af1a79b7a677bce6934f3`. Independent review is recorded separately on Fray #83 and the final Mote candidate; test passage alone does not imply review or landing authorization.

This completes the bounded engine-parity slice of item (1), subject to the separately recorded review outcome. Mote remains open for GIFTI frame-metadata admission, the historical consumer-pin compile check, and broader exact group-space/fsLR qualification. The existing source-value nonfinite/minimumSamples decision also remains separate.


## Independent review and local integration

Fray #83, seq 600, records independent **ACCEPT-WITH-NOTES** for both the source commit `dbcce304` and the receipt-only candidate `9ba0f367`. The named reviewer recorded native Mote approval on `cand-45CM72CYF12236MMTM8G3T8MC1`. No changed-kernel correctness defect was found. An independent BigDecimal oracle checked 96 adversarial points per kernel on the relevant JVM/JS paths, including ties, support boundaries, far-outside values, and saturation; it found zero mismatches. The reviewer separately checked Scala.js/JVM rounding agreement for NaN and infinities. CPU/GPU narrowing and GPU tie mutants were killed.

The qualification has a pre-existing affine-inverse limit. Spatial sampling uses `GridSpec.worldToVoxel`; eager/GPU sampling uses `SampleSpaces.coordToIndex`. Their inverses can differ by a few ulps on oblique grids. The independent review found different nearest voxels for 486 of 3000 deliberately exact half-integer ties, on both platforms. Boundary admission can also differ: at voxel `(1,1,-0.5)`, one oblique spatial inverse produced `z=-0.5000000000000002` and rejected the point, while eager produced `z=-0.5` and admitted ordinal 25 on a 3 x 4 x 5 grid. **Cross-engine nearest voxel identity is qualified on identity or exactly invertible fixtures, not every world affine.** The analytic oblique-grid trilinear test qualifies the spatial path, not universal inverse agreement between engines.

Two other non-blocking review findings remain outside this repair. A simultaneous half-down tie mutant in both CPU kernels survived their owning suites, although a surface-view consumer suite detected it; absolute owning-suite CPU tie/boundary sensitivity remains follow-up work. Extremely large finite world coordinates can overflow the spatial index conversion and throw, while eager rejects them. The texture mock remains a transport oracle; the separately captured real software WebGL evidence retains its stated limits. These findings are preserved in the broader open Mote ticket.

The proposed committed integration `2f45f1ab5fe29ebf9e554db2e9e8cd523160ed7c` merges the approved candidate into then-current main `0aa969a2`. Its production/test sources for this slice are identical to `9ba0f367`. Fresh integration gates passed:

- JVM: `set surfaceJVM / Test / parallelExecution := false`, `surfaceJVM/test`, `spatialJVM/test`: 166 + 224 = 390 passed, exit 0. Raw log `/private/tmp/scalafim-execution-20260929/logs/volume-surface-parity-integration-jvm-serial.log`, SHA256 `f0f45d21278ba30ee10be83c2b1bfb0d9d7336f0cd758c9a91c6ce471898fe38`.
- JS: `surfaceJS/test`, `spatialJS/test`, `surfaceViewThreeJS/test`: 132 + 199 + 18 = 349 passed; `scalafimCompileAll` exited 0 with no warnings/errors. Raw log `/private/tmp/scalafim-execution-20260929/logs/volume-surface-parity-integration-js.log`, SHA256 `d460a457959db7e7d22378258dfe2045ca3189a9ea6a50cf293052eee55d733a`.

Both default-scheduling JVM attempts timed out in the unchanged `TemplateSphereFilesSuite` nested-prefix fixture; each passed the other 165 surface tests and stopped before spatial tests. Their full logs and metadata are retained as `volume-surface-parity-integration-jvm.log` and `volume-surface-parity-integration-jvm-retry.log`. The successful controlled run changed only process-local suite scheduling. Every assertion and the original 30-second per-test deadline remained intact; the same fixture passed in 11.116 seconds. This establishes the sequential-suite gate, not reliability of that fixture under default parallel scheduling on a contended host.

The integration provider closure contains 14 tracked-clean staged checkouts, reflecting current main's additional provider graph. `/private/tmp/scalafim-engine-parity-20260930/integration-provider-closure.json` binds them to the tested integration/log; SHA256 `ee923b56f7bafaf6200c950f5f61789e188f702f1152749b7d1e4c8d098082c2`. The concurrent main commit `43616621` changes only an unrelated dataset test fixture and its evidence; it is preserved in the final integration, with no production/build or this slice's test changes. This README/receipt amendment states review limits and does not change compiled sources.

The user requested a local merge if ready. Exact landing reachability and authority are recorded in the native Mote candidate; no remote push was requested. The broader ticket remains open for metadata/consumer qualification and the recorded remaining numerical/test gaps.
