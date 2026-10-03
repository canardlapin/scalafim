# surface-view-three

Scala.js interpreter for the shared `SurfaceRenderPlan`. A host injects its
Three.js module namespace and canvas, so the library does not impose an npm
bundler or leak mutable Three.js types through its public API.

`ThreeVolumeProjector` admits one midpoint point per vertex, nearest-voxel
lookup, and `minimumSamples=1`. It selects the voxel in double precision before
uploading integer coordinates and packs the source into WebGL's X-fast texture
order. Output values use float32; exact equality with arbitrary double-valued
CPU output is not promised. It applies no source mask. Unsupported paths and
reducers return an explicit error before creating WebGL resources.

`Nearest`, `Average`, and `Mode` in the surface sampling plan name depth
reducers. The shared surface sampler uses nearest-voxel point lookup. The
spatial compiler separately supports nearest and trilinear interpolation,
normalizes retained in-grid/in-mask trilinear corners, and averages valid
depth samples. Its coverage records retained corner mass before normalization;
an uncovered sparse row evaluates to zero, while eager projection uses its fill
policy. Fractional-depth sampling is point sampling along a surface pair.

Cross-engine nearest-voxel identity is qualified on identity or exactly
invertible grid fixtures. The spatial compiler uses `GridSpec.worldToVoxel`,
while eager and GPU sampling use `SampleSpaces.coordToIndex`. Their affine
inverses can differ by a few ulps on oblique grids, selecting different voxels
at exact half-voxel ties or admitting different samples at the support boundary.
This evidence does not establish universal voxel identity across those inverse
paths for every world affine.

The engine-parity evidence and remaining qualification boundaries are recorded
in [the numerical receipt](../../docs/verification/volume-surface-engine-parity-20260930.md).

The production project uses the repository-wide CommonJS setting. To build the
standalone real-browser acceptance probe as a classic script:

```text
sbt 'set surfaceViewThreeJS / scalaJSLinkerConfig ~= (_.withModuleKind(org.scalajs.linker.interface.ModuleKind.NoModule))' surfaceViewThreeJS/fastLinkJS
```

The checked-in harness resolves the linked output and the local SurfViewJS
Three.js installation relative to `~/code`; serve `~/code` and open
`/scala/scalafim/modules/surface-view-three/browser/index.html`.

See [`docs/surface-viewer.md`](../../docs/surface-viewer.md) for the shared
scientific contract, host lifecycle, feature gates, and example application.
