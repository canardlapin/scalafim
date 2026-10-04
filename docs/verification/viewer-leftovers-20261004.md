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
  because main had not moved.
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

## Gates

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

## Edits made in review

- No edits to the cluster's own files. Every test passed against current main
  as snapshotted.
- Added `NEWS.md` (main has none). It holds the C9 header and only the two C9
  entries that describe these items: the prepared world-coordinate pick index
  and validated viewer layer reorder/replacement. C9
  (`wip/core-triage-docs-20261004`) has no lighting entry, so none was added.
  Because C9 also creates `NEWS.md`, landing both will produce an add/add
  conflict. Resolve it by keeping C9's full file, which already contains these
  two entries verbatim.
- Added this receipt.
