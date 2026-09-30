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

`ThreeVolumeProjectorSuite` uses a 3 x 2 x 4 asymmetric ramp (x + 10y + 100z), half-voxel offsets of 1e-8, exact lower and upper support boundaries, valid zero, and NaN. The injected test runtime independently uses WebGL's x + width*(y + height*z) texture layout; it is a transport oracle, not native shader evidence. A test-only browser export invokes the actual production API with Three.js and WebGL for the axis and boundary fixtures.

## Reproduced discrepancies

The unchanged projector failed all three transport regressions. Its non-cubic ramp returned 1 where the expected value was 102, and a NaN texel appeared at the wrong vertex. ScalaFIM canonical storage is Z-fast; WebGL 3D texture storage is X-fast. The old direct upload used the wrong layout. Fractional coordinate upload also rounded double coordinates into float32 before nearest selection, changing lookup at half-voxel ties and the upper support edge.

Baseline log: `/private/tmp/scalafim-execution-20260929/logs/volume-surface-parity-before.log`, exit 1, 0/3 passed. Two separate project-load failures preceded this executed regression; the runner preserved each retry log and metadata. They are infrastructure failures, not projection-test evidence.

The actual production API also failed the frozen browser baseline: Chromium 140.0.7339.186, Three.js r185, ANGLE SwiftShader, 7 value/count/quality mismatches across 10 vertex observations, no console/page errors. Receipt: `/private/tmp/scalafim-engine-parity-20260930/webgl-baseline.json`; linked baseline SHA256: `408d5e929c5c456a7e3b6a53ed315ea95714463344c0e6802a3fcfd087278d50`. This is software WebGL evidence, not hardware-GPU admission. Initial harness attempts failed to load the test export; they ran no shader qualification and are not included in the baseline result.

After the X-fast upload repair alone, the axis and zero/NaN regressions passed, while the half-voxel test still returned 11 where 10 was required. Log: `/private/tmp/scalafim-execution-20260929/logs/volume-surface-parity-axis-only.log`, exit 1, 2/3 passed. This separates the two discrepancies.

The final repair resolves nearest indices in double precision before uploading integer texel coordinates, uses the shared midpoint arithmetic convention, and packs source texels into X-fast order. The shader fetches the chosen integer texel. Spatial compiler production code is unchanged.

## Validation

Pending final execution and source-bound review. Raw logs and metadata will be listed here after gates complete.
