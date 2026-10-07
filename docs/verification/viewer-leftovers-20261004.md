# Viewer leftovers (cluster C7) readiness receipt — 2026-10-04

Readiness step for landing preserved cluster `C7-viewer-leftovers` on main. Not
landed; an independent review follows.

## Provenance

- Snapshot: `wip/viewer-leftovers-20261004` at `5675d776`, which captures
  uncommitted canonical-checkout files on top of main `53097f3f`. The triage
  record is `dirty-triage.json`, cluster `C7-viewer-leftovers`. Every file is
  classified `UNIQUE` / `LANDABLE-AFTER-REVIEW`, with mtimes of 2026-09-12.
- Review branch: `review/c7-viewer-leftovers-20261004`, created from the
  snapshot. Merging main `53097f3f43fa125cd166a99268f192f4a5b12311` was a no-op
  because main had not moved. For the review round (see "Review round 1"
  below), main `36240e396de9906a11fd31e6aa7a151f9162d017` (PHRF prerequisites
  landed) was merged in without conflicts.
- Original authors: the 2026-09-12 core-triage Codex agents. Their sources were
  already committed to main, but these files were left uncommitted.
  - `codex-core-fruit` worked on bd-01M1X52T0SJK92JXQAW9N17X4V, "Align
    reference raster lighting capability metadata with implemented shading"
    (closed). It produced the raster Lighting flag, its test and the README.
  - `codex-next-five` worked on bd-01M0NS3ERVZDN9GXY9W7K5X873, "Add validated
    viewer model updates for layer order and colorizers" (closed). It produced
    the image-view README and `ViewerModelUpdateSuite`. The
    `ViewerModelUpdate` source, `ViewerSession.validateModel`,
    `CanvasViewerController.updateModel` and its Canvas test reached main in
    `7989608d`.
  - `codex-next-five` also worked on bd-01M0TGTH208WXGQ2CZJZM7RWRQ,
    "Accelerate SurfaceWorldLink.nearestVertex with an owner-safe spatial
    index" (closed). It produced the surface-view README section and
    `SurfaceWorldIndexBenchmark`. `SurfaceWorldIndex` and
    `SurfaceWorldLink.prepare` reached main in `7989608d`.
- Fray search found no related threads.

## Files and what they assert

| File | Kind | Content |
| --- | --- | --- |
| `modules/surface-view-raster/shared/src/main/scala/scalafim/surface/view/raster/SurfaceRasterizer.scala` | **Code** (capability metadata) | Adds `SurfaceBackendFeature.Lighting` to `SurfaceRasterizer.capabilities`. Replaces the caveat "lighting is not applied by the reference raster backend" with the precise semantics: world-space vertex normals, ambient/diffuse Lambert shading, lit vertex colours interpolated, and no per-pixel normals, specular highlights or shadows. |
| `modules/surface-view-raster/shared/src/test/.../SurfaceRasterizerSuite.scala` | Test | New case "advertised lighting produces ambient and directional Lambert pixels". It asserts the capability is advertised. On a known +Z-normal triangle it checks exact pixels for five cases: unlit (unchanged), front light at 0.25 + 0.5 (×0.75), back light (ambient only, ×0.25), tangent light (ambient only) and saturating 0.75 + 0.75 (clamped to ×1). The expected values come from the known geometry, not from renderer normals. |
| `modules/surface-view-raster/README.md` | Docs | States the same lighting semantics and limits. |
| `modules/image-view/shared/src/test/.../ViewerModelUpdateSuite.scala` | Test (new) | Four tests: (1) `ReorderLayers` partial promotion keeps the order of unmentioned layers; empty reorder is the identity; duplicate and unknown ids, and `ReplaceLayer` of an unknown id, return typed errors. (2) After a reorder the composited colours follow the new order, with 9 cache hits, 0 source reads and 0 colorized pixels. (3) Typed `ReplaceLayer` changes pixels with no stale colorizer-bound sample reuse: 6 hits, 3 misses, 1 source read, readouts unchanged. (4) `ViewerSession.validateModel` refuses an unsupported window or timepoint instead of silently dropping it. |
| `modules/image-view/README.md` | Docs (new) | Documents `ViewerModelUpdate`, Canvas `updateModel` adoption and `session.validateModel`. All three APIs exist on main (`Layer.scala`, `Interaction.scala`, `CanvasViewerHost.scala`). |
| `modules/surface-view/README.md` | Docs | Documents prepared `SurfaceWorldIndex` snapshot semantics: explicit re-preparation after mutation, original vertex ids, inclusive radius, lowest-id ties and the full affine metric. These match `SurfaceWorldIndex.scala`. Also records the 2026-09-12 synthetic benchmark table, explicitly labelled as one local warmed run and not a gate. |
| `modules/surface-view/jvm/src/test/.../SurfaceWorldIndexBenchmark.scala` | Test-scope tool (new) | A plain `object` with `main`, not a munit suite. It is compiled by `surfaceViewJVM/Test/compile` but not run by `test`; run it on demand with `sbt "surfaceViewJVM/Test/runMain scalafim.surface.view.SurfaceWorldIndexBenchmark"`. It checks with `require` that the index agrees with a primitive scan on 1000 queries, then reports build, query and allocation figures. It is JVM-only because it uses `com.sun.management.ThreadMXBean`. It was not re-run in this pass. |

The Lighting flag is therefore a source change. It does not change the
rasterization algorithm, because `lightFactor` has applied ambient/diffuse
shading since `149686d6` (2026-07-22). It does change the advertised backend
capabilities that backend admission consumes, so `surfaceViewExamples*` was
gated as well.

## Gates (readiness pass on main `53097f3f`; superseded by review round 1 below)

Each run used `tools/build/sbt-warm` in the review worktree, one batch at a
time, with at least 30% memory free beforehand. All runs were warning-clean
(no `[warn] --` or `[error]` lines).

| Target | Result |
| --- | --- |
| `surfaceViewRasterJVM/test` | 12/12 passed |
| `surfaceViewRasterJS/test` | 12/12 passed |
| `imageViewJVM/test` | 42/42 passed |
| `imageViewJS/test` | 42/42 passed |
| `imageViewCanvasJS/test` | 8/8 passed |
| `surfaceViewJVM/test` | 68/68 passed (benchmark compiled; not executed by `test`) |
| `surfaceViewJS/test` | 68/68 passed |
| `surfaceViewExamplesJVM/test` | 12/12 passed |
| `surfaceViewExamplesJS/test` | 11/11 passed |
| `scalafimCompileAll` | success in 699 s, 0 warnings, 0 errors (JavaFX viewer modules included) |

The first `surfaceViewJVM/test` attempt deadlocked in the sbt client/server
terminal handshake: munit's `RichLogger.flush` was blocked in
`NetworkTerminal.isColorEnabled`. It was a harness problem, not a test failure.
It was rerun with `TERM=dumb` and stdin from `/dev/null`, shutting down the
server after each batch.

The JavaFX viewer modules (`imageViewJavafxJVM`, `surfaceViewJavafxJVM`) are
compiled by `scalafimCompileAll`. Their tests and probes need a display and were
not run.

## Edits made in the readiness pass (commit `78950a6b`)

- No edits to the cluster's own files. Every test passed against main as
  snapshotted.
- Added `NEWS.md` (main has none). It holds the C9 header and the two C9
  entries that describe these items: the prepared world-coordinate pick index
  and validated viewer layer reorder/replacement.
- Added this receipt.

## Review round 1 (Opus review of `78950a6b`: CHANGES-REQUIRED)

The review confirmed the lighting semantics against the code (world-space
normals, Lambert, Gouraud interpolation) and the `ViewerModelUpdate` cache
counts. These changes were made in response.

1. **Blocker: JDK 17 compile.** `SurfaceWorldIndexBenchmark` called
   `Thread.currentThread().threadId()`, which is a JDK 19+ API. CI pins JDK 17
   (`.github/workflows/first-level.yml`), and `surfaceViewJVM/test` compiles
   test sources. The benchmark now pattern-matches
   `com.sun.management.ThreadMXBean` on `isThreadAllocatedMemorySupported`,
   following the other allocation probes, and calls
   `getCurrentThreadAllocatedBytes()` (JDK 14+). The deprecated `getId()` is not
   used. All measurements stay on the calling thread.
   - Evidence: no JDK 17 is installed locally. `/usr/libexec/java_home -V`
     lists 22, 20, 8 and 7. Instead, `javac 22 --release 17` (which checks
     against the JDK 17 API signatures in `ct.sym`) compiled a Java probe using
     the same calls (`isThreadAllocatedMemorySupported`,
     `isThreadAllocatedMemoryEnabled`, `setThreadAllocatedMemoryEnabled`,
     `getCurrentThreadAllocatedBytes`). A control calling
     `Thread.currentThread().threadId()` failed with "cannot find symbol".
     API availability on 17 was therefore observed against JDK 17 signatures.
     The Scala compile itself ran on JDK 22 and was not observed on a JDK 17
     runtime.
   - Out of scope, observed: `modules/mvpa/jvm/src/test/.../RsaAllocationProbe.scala`
     on main also calls `threadId()`. It is not part of C7 and was left
     unchanged.
2. **Stale doc.** `docs/benchmarks/surface-viewer.md` said the reference raster
   "declares lighting unsupported". It now describes the supported per-vertex
   Lambert semantics and their limits.
3. **Test gap: world-space versus object-space normals.** Added
   `SurfaceRasterizerSuite` "lighting uses world-space normals under a rotated
   surface-to-world affine". The +Z triangle is rotated +90° about X, so the
   world normal is −Y, and it is viewed from `Posterior`, which faces it.
   - A light along the world normal (0, −1, 0) yields front-lit (×0.75)
     `(150, 90, 60)`.
   - A light along the old object normal (0, 0, 1) yields ambient-only (×0.25)
     `(50, 30, 20)`.
   - Mutation check: `SurfaceCompiler.packMesh` was temporarily changed to
     compute normals from the untransformed local coordinates
     (`frame.coordinateAt`). `surfaceViewRasterJVM/test` then failed exactly
     this test (13 total, 1 failed, 12 passed; assertion at line 168). After
     restoring, `SurfaceCompiler.scala` has SHA-256
     `48da0fe6060dace16243124c50389b873a09c0a435a776b45173038311e871fe`,
     identical to before the mutation, and `git status` is clean for that path.
4. **Nits.**
   - `ViewerModelUpdateSuite` now asserts the typed
     `Left(ImageViewError.TimepointOutOfBounds(1, 1))` instead of `isLeft`.
   - It also adds a threshold-rejection check for the image-view README claim:
     a mask layer given a `DisplayThreshold` yields
     `ThresholdUnsupported(b.id)`, because `MaskColorizer` defines no
     thresholding.
   - Rewrapped the over-long line in `modules/surface-view/README.md`.
   - Added a `NEWS.md` entry for the raster `Lighting` capability, since it is
     a user-visible admission change. This entry has no C9 counterpart.
   - Documented the light-direction convention (the vector `d` points from the
     surface toward the light) and the normal flip under a reflecting
     (negative-determinant) surface-to-world affine. Both are now separate
     `SurfaceRasterizer.capabilities` caveats and are stated in the
     surface-view-raster README.

### Gates after review round 1

All batches ran in the review worktree with sbt-warm (`TERM=dumb`, stdin
`/dev/null`). Memory was at least 30% free before each batch, and the server
was shut down after each. There were no `[warn] --` or `[error]` lines.

| Target | Result |
| --- | --- |
| `surfaceViewRasterJVM/test` | 13/13 passed |
| `surfaceViewRasterJS/test` | 13/13 passed |
| `imageViewJVM/test` | 42/42 passed |
| `imageViewJS/test` | 42/42 passed |
| `imageViewCanvasJS/test` | 8/8 passed |
| `surfaceViewJVM/test` | 68/68 passed |
| `surfaceViewJS/test` | 68/68 passed |
| `surfaceViewExamplesJVM/test` | 12/12 passed |
| `surfaceViewExamplesJS/test` | 11/11 passed |
| `scalafimCompileAll` | success in 336 s, 0 warnings, 0 errors (JavaFX viewer modules compiled, not run) |

### NEWS.md and C9

C9 (`wip/core-triage-docs-20261004`) also creates `NEWS.md`, so landing both
will produce an add/add conflict. Resolve it by taking C9's file, which already
contains the two shared entries verbatim, and then adding this cluster's
`Lighting` entry under `## Unreleased`.
