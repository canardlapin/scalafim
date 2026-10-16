# Surface display evidence, 2026-10-15

[Open the side-by-side gallery](index.html). **Full display parity is not qualified.**
The reviewed production change is `37df543fc821a28e1fdad861a0fead6ceeaf08eb`,
on reconciled base `dc101ea05a327e06641b9fc7064f822c4a9608e3`.
The additional nearest-only fixture consumer guard and evidence harness are
reviewed separately. Captures bind the actual built modules, dirty source
hashes, reference source and, for the latest JVM batch, loaded runtime artifacts.
Historical captures deliberately retain their earlier source identities.

## What the evidence establishes

- Six native volume endpoints match SurfViewJS exactly, with independent affine-field readouts.
- The six multi-panel browser endpoints pass independent ray-derived surface/view identity, resize-back and disposal checks.
- Matching sphere geometry reduces several native annotation images to maximum channel error 3; the strict limit is 2. An explicit public-client update fixes stale reference morph anchors in all 11 measured cases, without changing the original native reference receipts.
- All 54 actual JavaFX fallback captures equal their published raster pixels and pass stable warm images, resize-back and disposal checks. Fifty-three have runtime-bound reference comparisons; all retain strict pixel failures. The remaining crosshair-label reference adapter is unavailable. Native JavaFX explicitly refuses these controlled profiles.
- Six browser flat-map endpoints are byte-exact. Six JVM static semantic captures pass; six JVM image comparisons fail. The revised C11 report remains **incomplete**, not a family pass. Original overbroad receipts are preserved as adverse evidence and superseded by `flatmap/reviewed-flatmap-qualification.json`.
- Twenty same-page realistic-cortex lifecycle cycles pass explicit resource release and the fixed retained-JS-heap budget. This does not measure GPU memory, native RSS, detached backing stores, warmup retention or long-run leak freedom.
- Full repository JVM/JS compile is warning-clean. Affected module gates and 48 Node tests pass; see [exact check receipts](checks.json), including the original failed prerequisite run and two existing skipped JS benchmarks.

The [coverage ledger](coverage.json) accounts for all 636 expanded variants
(151 parent checkpoints), including supplementary profiles and the declared
positive legend erratum. **162 distinct variants have paired endpoint images
across historical/current builds; none has complete frozen-matrix qualification.**
The dedicated C11 six-endpoint probe is supplementary and is not counted as
24 fully exercised C11 variants. Raw logs, failures and source hashes live in
the content-addressed local bundle described by [artifact-manifest.json](artifact-manifest.json).
External cortex input coordinates are not redistributed in that bundle.

## Remaining acceptance failures

1. C05 halo compositing differs semantically; C07 reference material/depth behavior conflicts with the frozen policy. These need explicit compatibility decisions and implementation, not looser global tolerances.
2. C10 custom viewer keys exceed the pinned public reference route; the crosshair-label adapter and complete C12 publication/restore matrix remain unqualified. Earlier publication and linked-view evidence remains separately recorded in `../mapping-linked-20261014/`.
3. Full physical-action replay, independent per-element masks and mutation controls, and reference-calibrated edge envelopes are unfinished. The uncaptured variants and exact final-source full matrix remain open.
4. Realistic fallback medians exceed the proposed 100 ms target for several workloads (latest JVM multi-panel 512–840 ms, contours 104–505 ms, networks 116–190 ms). Strict raster antialiasing, JVM glyphs, native fold/edge pixels and remaining transparency differences still fail.
5. Hosted CI and a merge satisfying the canonical P6 gate are separate from local compilation and source review.

## Reproduction

Use the repository's installed sbt and SurfViewJS Playwright dependencies, the
clean pinned SurfViewJS commit `fb60f3705d51d2d74e69ddf71df7000ee9eb5b06`,
and the external GIFTI files/digests in `tools/surface-view/parity/manifest.json`.
Follow the browser ownership audit in `AGENTS.md`; all captures here used one
owned installed Playwright backend, ANGLE Metal, DPR 1. Source inputs, cameras,
color policies and viewport sizes are recorded, without image registration.

```sh
bash tools/prepare-pinned-dependencies.sh
python3 tools/build/sbt-warm surfaceViewExamplesJS/fullLinkJS
node --test tools/surface-view/parity/*.test.mjs
node tools/surface-view/parity/run-qualification.mjs \
  --reference-root /path/to/clean/pinned/surfviewjs \
  --module examples/surface-view/js/target/scala-3.7.4/scalafim-examples-surface-view-opt/main.js \
  --row '^C02-BR-volume' --limit 6 --out /tmp/surface-volume-capture
```

The generic runner's default exit 0 means **capture completed**. Its results
still say qualification failed. Add `--require-qualified` for fail-closed
qualification gating. Use `--offset`/`--limit` for bounded batches; `--prepare-only`
writes checked inputs for the actual JVM probe, and `--jvm-results` compares its
retained captures while binding `runtime.json`. `--reference-follow-morph` is a
separately declared public-client route; it is not native automatic behavior.
The flat-map runner exits nonzero while its required qualification checks remain
missing. Exact commands and runtimes are in the retained log metadata.
