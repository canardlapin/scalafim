# Rounded contours and retained qualification gaps

[Open the side-by-side gallery](index.html). **Full display parity remains
unqualified.** This appendix adds bounded evidence to the earlier
[636-variant ledger](../qualification-20261015/coverage.json); it does not turn
the six new primitive specimens into completed matrix variants.

## Implemented and checked

`SurfaceContourMode.RoundedStrokes` is an opt-in mode with pixel widths, round
endpoint caps, opaque cores, and translucent full-width halos. All cores precede
halos; halo overlap follows declared edge order. Opaque scientific geometry and
ordinary opaque decorations occlude strokes, and overlay elements remain last.
Native picks preserve the visible decoration's identity. Unsupported arbitrary
ordered-alpha geometry, translucent scientific bases, and eye-plane crossings
are rejected. Native JavaFX refuses this policy; its raster host is the fallback.
Existing side-band behavior remains the default.

The six synthetic specimens use orthographic 240×240 and perspective 480×320
outputs, single/overlapping strokes, and an opaque occluder. Masks come from
source endpoints and camera equations with a fixed 1.5-pixel exclusion around
stroke edges, before capturing either implementation. Missing-contour and
wrong-occlusion mutations must fail each required mask. Wrong dimensions,
truncated buffers, and nonfinite comparisons are rejected. Three identical
reference captures precede candidate capture. There is no image alignment.

[All six primitive checks pass](primitives/rounded-contour-proof.json): native
maximum interior channel error is 0–1, raster error is 0. **All six strict
full-frame comparisons still fail.** These checks establish interior core,
halo, cap, overlap, and occlusion behavior; they do not calibrate antialiasing or
qualify the complete C05 matrix. An additional
[native pixel/pick proof](native-proof/decoration-proof.json) checks actual
WebGL blending, reversed order, occlusion, and clearing decorations.

The [actual JVM ImageView host](jvm/comparisons.json) passes the same six interior
masks and captures all three cortex inputs. All nine JVM raster images equal
their browser-raster counterparts byte-for-byte. Published snapshot pixels,
warm repeat stability, resize-back, and disposal checks pass; every native
JavaFX attempt reports the intended ordered-depth-policy refusal. Strict
reference full-frame comparisons remain failed on all nine. The JVM comparator
verifies frozen oracle hashes and reference admission and requires all nine
cross-platform byte equalities. [Three negative controls](jvm/negative-controls.json)
reject a changed cortex pixel, changed oracle bytes, and a failed reference repeat.

## Warm rendering

Three.js now reuses native decorations only when the plan and lowered geometry
are the same objects and the output size is unchanged. Validation and resource
compilation still run. Cleanup invalidates reuse before native mutation;
switching to a plain frame forces drawing. Failed cleanup, failed drawing,
camera changes, and equal-but-distinct geometry are regression-tested.

The [three-cortex comparison](cortex/cache-comparison.json) uses unchanged inputs
at 600×450 with zoom 1, 1.5, and 2. All reference, native, and raster PNGs are
byte-identical before and after caching. Median native render plus synchronous
PNG readback fell from **188.2/181.3/171.6 ms to 85.6/85.8/89.6 ms**, respectively,
using five retained warm samples each. Logical draw submissions fell from two
to one; this is not a count of actual GL draw calls. Warm scientific uploads
remain zero. This is a bounded local measurement, not sustained-interaction or
GPU-memory qualification. Browser raster medians remain **845.6–1346.3 ms**.
Actual JVM cortex render plus snapshot medians are **266.1–536.4 ms**, also
above the proposed 100 ms fallback target.

## Scalar-key evidence

The supplementary [reference exports](scalar-key/reference/scalar-key-qualification.json)
and [JVM publication exports](scalar-key/jvm/candidate.json) use dorsal unlit
views and the same declared 256-entry byte palette. Both output sizes retain
the range and threshold interval. Endpoint and interior palette checks pass;
reference export restores its original dimensions. The original edge-on
fixture, two-entry palette, and erroneous midpoint expectation are retained in
the local adverse-evidence archive.

These are different publication layouts and retain different surface-alpha
appearance. [All raw full-frame comparisons fail](scalar-key/comparisons.json).
Custom ticks, custom key sizes, categorical/bivariate viewer export, calibrated
glyph/edge masks, and the complete C10/C12 matrix remain unqualified.

## Evidence and reproduction

[Source review](source-review.json), [check receipts](checks.json), and
[artifact hashes](artifact-manifest.json) distinguish passing checks from
retained failed attempts. The browser module hash is
`b57a48d9514a6a7a8a5f8585b49fa58f50f8bb63a2a0a845ba80ec3cd8b6191f`;
its build receipt binds individual source files. The reference is pinned at
`fb60f3705d51d2d74e69ddf71df7000ee9eb5b06`. Runtime, helper, module, and input
hashes accompany the primitive proof. External cortex coordinates remain
outside the committed gallery and retained archive.

Run `tools/surface-view/parity/run-rounded-contour-proof.mjs` with
`--reference-root`, `--reference-bundle`, `--scalafim-module`, `--build-receipt`,
and a fresh `--out` directory. For the cortex rerun, use
`run-qualification.mjs --row C05-BR --checkpoint halo --limit 3 --rounded-contours`
with the same pinned reference and module. A successful capture command is not
a full qualification pass. Frozen baseline inputs and strict thresholds have
not been overwritten.

The remaining P6 gates include the uncaptured matrix/actions, independent masks
and reference edge calibration, transparency compatibility, full publication
and restore evidence, fallback latency, long-run resource bounds, and hosted
checks on the final candidate. Draft PR #61 remains separate from merge.
