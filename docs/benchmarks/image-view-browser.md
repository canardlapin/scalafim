# Image-view browser benchmark

This opt-in benchmark measures the production orthogonal viewer through a real
browser `CanvasRenderingContext2D`. Its timings are diagnostic receipts, not
portable CI thresholds. Ordinary JVM and Scala.js tests enforce the scientific
and orientation contracts independently.

## Workloads

The deterministic benchmark entry point is
`scalafim.image.view.canvas.BrowserBenchmark`. It records:

- cold compilation, sampling, colorization, Canvas upload, and draw;
- warm redraw with all sampled slices, rasters, and native image sources cached;
- axial scrolling, where sagittal and coronal rasters must remain cached;
- display-window changes, which recolorize without volume reads or resampling;
- nonlinear dense-field pullback scrolling;
- source-read counts, viewer/Canvas work receipts, and a final canvas checksum.

The affine workload uses a `160 x 192 x 128` volume with anatomy and overlay
layers on a `960 x 720` canvas. The nonlinear workload uses an `80 x 80 x 64`
volume and a deterministic smooth displacement field.

## Run

Link the test-only browser entry point as a standalone script:

```sh
sbt 'set imageViewCanvasJS / Test / scalaJSLinkerConfig ~= (_.withModuleKind(org.scalajs.linker.interface.ModuleKind.NoModule))' 'imageViewCanvasJS/Test/fullLinkJS'
```

Serve the repository root and open the benchmark page:

```sh
python -m http.server 8765 --bind 127.0.0.1
```

Navigate to
`http://127.0.0.1:8765/tools/image-view-browser-benchmark.html`. The JSON
receipt is shown beside the rendered viewer and is also available as
`window.scalafimImageViewBenchmark`.

## Acceptance contract

Before timings are interpreted, the receipt must show:

- cold source reads equal visible source-backed layers times cold repetitions,
  not that count times three anatomical panels;
- warm redraw has zero sampled pixels, zero colorized pixels, and zero uploaded
  bytes;
- axial scrolling misses one raster per visible layer and reuses the other two
  planes;
- window changes have zero source reads and zero sampled pixels;
- nonlinear scrolling misses only the moved plane and reuses the other two;
- the canvas checksum is non-empty and stable for the same linked artifact.

## Structural results

The implementation now makes one source-frame read per visible layer and
timepoint during a cold three-plane compilation, rather than one read per
plane. Nonlinear pullback planning transforms one complete display row per
mapping call, reducing mapping calls from `width * height` to `height` without
changing sampled pixels. Scalar colorization transfers one owned packed-pixel
array into the raster, and Canvas draw profiling accumulates primitive counters
without allocating an intermediate profile for every command.

Nearest, linear, and cubic slice interpolation now read the primitive volume
buffer by linear index after the kernel's existing bounds check; linear and
cubic sampling therefore avoid respectively eight and 64 varargs index paths
per output pixel. Canvas raster creation writes one endian-correct 32-bit word
per pixel on little-endian browsers and retains the byte-wise implementation as
a portable fallback. Dense nearest/linear mappings reuse row-sized primitive
coordinate buffers, cache the field-grid inverse, and sample directly into
primitive coordinates; cubic and arbitrary morphisms retain the typed generic
fallback. JVM and Scala.js differential tests compare every fast path with its
checked or typed reference behavior.

## First live browser receipt

The first live receipt was recorded on 2026-07-21 in Chrome 150 on macOS with
no CPU or network throttling. The complete representative JSON receipt is
preserved at
[`receipts/image-view-browser-chrome150-2026-07-21.json`](receipts/image-view-browser-chrome150-2026-07-21.json).
Every structural contract passed and repeated executions produced checksum
`5c73b3ad` after the harness was made to clear the complete device canvas at
the start of each run.

The following values are the medians of five same-browser benchmark medians,
after the linked artifact and JavaScript engine were warm:

| Scenario | Median time |
| --- | ---: |
| Cold render | 60.1 ms |
| Warm redraw | 0.1 ms |
| Axial scroll | 146.7 ms |
| Window recolor | 7.8 ms |
| Nonlinear scroll | 79.0 ms |

These are diagnostic local measurements, not release thresholds. The work
receipts explain the large interaction timings: axial scrolling samples and
colorizes 61,440 pixels, while nonlinear scrolling maps, samples, and
colorizes 6,400 pixels. Warm redraw performs none of that work.

## Follow-up optimization decisions

Two proposed optimizations were evaluated against the live receipt:

1. Replacing immutable LRU updates with a compilation-local mutable
   thaw/freeze transaction was slower. In sequential same-browser trials, the
   cold, axial-scroll, and nonlinear medians increased by approximately 4%,
   7%, and 32%, respectively. Copying the small caches into and out of mutable
   maps cost more than the immutable updates it replaced. The experiment was
   reverted.
2. Replacing the complete `SliceLayer` in the model-scoped cache key with only
   its `LayerId` produced no stable end-to-end improvement and was also
   reverted, retaining the stronger cache identity.

Static raster versus dynamic decoration splitting was measured before adding
any multi-canvas lifecycle. Across five 1,000-iteration batches, median warm
scene compilation was 0.067 ms and median cached Canvas drawing was 0.030 ms.
The entire warm path is therefore about 0.10 ms; a split could recover only a
fraction of that and would not reduce sampling on scroll. It is not justified
for the current viewer.

A Chrome performance trace reported a 1 ms document response and an 8.884 s
render delay because this diagnostic page deliberately runs the complete
synchronous benchmark before replacing its output text. That LCP is a harness
effect, not a viewer load metric.
