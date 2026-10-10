# Volume-to-surface engines and GIFTI frame metadata: current gap matrix

Date: 2026-10-10. Mote: `bd-01M37FQGV8ZPT30X37TA4MM8R7`. Base: `origin/main`
`8e85ef61`, branch `work/v2s-engine-reconcile`. Parent audit:
`docs/audits/group-volume-fslr-mapping.md`.

This receipt replaces repeating the five historical findings with their
status on current main. "Resolved on main" cites source and a test that runs in
the ordinary suites. "Resolved here" was changed on this branch, with a
regression that failed before the change. "Open" is not resolved and says why.

## Matrix

| # | Historical finding | Current status | Evidence |
|---|---|---|---|
| 1a | Spatial `VolumeToSurfaceOperator` trilinear path untested against an independent oracle | Resolved on main (`aa2af562`) | `VolumeToSurfaceParitySuite` "trilinear interpolation reproduces a closed-form multiaffine polynomial on an oblique anisotropic shifted grid" (`modules/spatial/shared/src/test/scala/scalafim/spatial/VolumeToSurfaceParitySuite.scala:30`), impulse/mask/partial-support test (`:51`) |
| 1b | GPU (`ThreeVolumeProjector`) float32 nearest ties and texture axis order | Resolved on main for the transport and shader specimens (`aa2af562`); hardware GPU is not qualified | `ThreeVolumeProjectorSuite` texture-axis (`modules/surface-view-three/js/src/test/scala/scalafim/surface/view/three/ThreeVolumeProjectorSuite.scala:79`), float32 half ties (`:86`), far-outside (`:119`). The texture mock is a transport oracle (N4). Software WebGL (SwiftShader) evidence is in `volume-surface-engine-parity-20260930.md`. It was not rerun here |
| 1c | Long-to-Int index wrap in the eager, spatial and GPU nearest kernels | Resolved on main (`dbcce304`) | `VolumeToSurfaceParitySuite.scala:138,146,152` far-outside tests. Eager bound check is at `modules/surface/shared/src/main/scala/scalafim/surface/SurfaceSampling.scala:220` |
| N1 | Spatial `GridSpec.worldToVoxel` (image4s `Affine.inverse`) and eager/GPU `SampleSpaces.coordToIndex` (inverse matrix product) can choose different voxels at exact oblique half ties: 486/3000 in review | **Open**, owner decision needed | Both inverses are still present: `VolumeToSurfaceOperator.scala:234` vs `SurfaceSampling.scala:216` and `modules/image/shared/src/main/scala/scalafim/image/SampleSpaces.scala:292`. Cross-engine voxel identity is qualified only on identity or exactly invertible grids (engine-parity receipt). Fixing this means choosing one canonical inverse, which is image4s/Gale geometry. That work belongs upstream, so it was not attempted here |
| N2 | Owning CPU suites did not detect simultaneous half-down mutants | Resolved on main (`d462806e`) | Absolute half-up/boundary tests per axis: `VolumeToSurfaceParitySuite.scala:158` and `modules/surface/shared/src/test/scala/scalafim/surface/SurfaceSamplingSuite.scala:264`. Mutation kills are recorded in `volume-surface-ties-consumer-pin-20260930.md` |
| N3 | A finite world point whose voxel image overflows makes the spatial conversion throw, while eager rejects it | **Resolved here** | Reproduced on base: `VolumeToSurfaceParitySuite` "nearest and trilinear spatial weights reject world points whose voxel image overflows" threw `IllegalArgumentException: voxel coordinate x coordinate must be finite; got Infinity`. `GridSpecFrameSuite` "world-to-voxel returns Left…" failed the same way when the fix was reverted. Fix: `SpatialCoordinates.finiteImage` (`modules/image/shared/src/main/scala/scalafim/image/SpatialCoordinates.scala:198`) makes the three `Either`-returning point conversions return `GeometryError.NonFiniteCoordinate`. The spatial lookup already maps `Left` to empty weights. The eager overflow test also pins `SurfaceSampleTally(4, 3, 0, 0, 1)` |
| 2a | GIFTI codec drops `DataSpace`/`TransformedSpace`/`GeometricType` | Resolved on main | `GiftiCoordinateDeclaration` (`modules/surface/shared/src/main/scala/scalafim/surface/gifti/GiftiCoordinates.scala:109`, built by `fromPointSet` at `:122`) keeps every transform in file order with its typed spaces and structure/type metadata. `FrameDeclarationSuite` and `DeclaredSurfaceReaderSuite` test it |
| 2b | GIFTI placement silently applies the first transform (`headOption`) whatever its target, unknown or ambiguous | **Resolved here** (C6 re-port) | Base `GiftiSurfaceCodec.transformMatrix` used `pointSet.transforms.headOption`. Placement now goes through `GiftiPlacementResolver` (`GiftiPlacement.scala:69`, called at `GiftiSurfaceCodec.scala:67`). `DeclaredGiftiSurface.placement` (`GiftiCoordinates.scala:145`) names the original transform by index. `GiftiPlacementReaderChecks` runs 23 cases through both platforms' public readers (`GiftiPlacementReaderSuite` on JVM, `GiftiPlacementReaderJsSuite` on JS). A mutant that restores the first-transform fallback fails 2 of them |
| 2c | GIFTI coordinates lose precision (Float64 payloads unsupported) | **Resolved here** (C6 re-port) | `NIFTI_TYPE_FLOAT64` reader extension (`GiftiPayloadDecoder.scala:27,62,129`). Integer and byte APIs refuse it. Covered by `GiftiFloat64Suite` (width overflow, short/long/misaligned, ASCII coercion) and `GiftiFloat64Reader{,Js}Suite` (ASCII, base64, gzip and zlib in both endians, exact to 0.0) |
| 3 | `TriangleMesh` lacks structural equality; `SurfaceGeometry ==` compares array identity | Resolved on main (`fa2193ab`) | `TriangleMesh.equals/hashCode` (`modules/surface/shared/src/main/scala/scalafim/surface/TriangleMesh.scala:18,30`); `TriangleMeshEqualitySuite.scala:8,15,20` |
| 4a | `SurfaceProjectionReceipt.requestedSamples` computed, not observed; `acceptedSamples` counts non-finite values | Resolved on main (`fa2193ab`) | `SurfaceSampleTally` with a checked partition invariant (`modules/surface/shared/src/main/scala/scalafim/surface/SurfaceSampling.scala:46`), carried on CPU and GPU (`ThreeVolumeProjector.scala:197`); `SurfaceSamplingSuite.scala:224,240` |
| 4b | `minimumSamples` can qualify a vertex whose only sample is NaN; Average propagates NaN | **Open**, owner decision (never made) | Per-vertex `sampleCounts` still include non-finite samples by documented contract (`SurfaceSampling.scala:65-66`), and qualification is `count >= minimumSamples` (`modules/surface-view/shared/src/main/scala/scalafim/surface/view/SurfaceVolumeProjection.scala:89`). Changing this alters a scientific policy, so it is not done here |
| 5 | `spatialJVM` fails to compile at consumer pin `7c3ff0a` against Gale `83cac90` (`DMatBuilder.writeLinear`) | Historical; current graph compiles | The historical pin failure was reproduced and archived in `volume-surface-ties-consumer-pin-20260930.md`. On this branch `scalafimCompileAll` exits 0 on both platforms. No downstream consumer was rebuilt or repinned |

## C6 reconciliation (wip `8242a97e`)

The retained branch `migration/20261005/branches/wip/gifti-float64-placement-20261004`
(`8242a97e`, base `53097f3f`) cherry-picks cleanly. Its strict default fails
current main. With it applied as-is, `surfaceJVM/test` gave 308 total and 3
failed, all in `DeclaredSurfaceReaderSuite`, with "multiple transforms require
an explicit target". The Workbench-style `DeclaredGiftiFixture` has two
identity transforms (blank, then Talairach→Talairach), and the wip default
refused it. The re-port therefore makes these changes:

- It builds placement on current declarations instead of the wip's parallel
  `GiftiDecodedSurface`/`sourceTransforms`. `GiftiTargetSpace` is the
  recognized subset of `GiftiDeclaredSpace`, so `Unknown` and `Other` cannot
  be a placement target. `GiftiPlacement.Transformed(index, system, target)`
  points into `GiftiCoordinateDeclaration.coordinateSystems`.
  `DeclaredGiftiSurface` gains `placement`, and its constructor is now
  `private[surface]` so placement always agrees with
  `geometry.surfaceToWorld`.
- It changes one default. Under `Unambiguous`, identity-only declarations stay
  `NativeCoordinates` and are not refused. That covers a nibabel `UNKNOWN`
  identity and the Workbench pair. No choice among identity matrices can change
  coordinates, and `NativeCoordinates` claims no frame. A single transform with
  a recognized target is still applied and keeps its provenance. Several
  transforms of which any is not identity are refused, as is a single
  non-identity transform with an unknown or missing target. Both differ from
  base main, which applied the first matrix.
- `declared`/`readDeclared*` take a `GiftiTransformSelection`, which defaults
  to `Unambiguous`. `Target(space)`, `Transform(index)` and `NativeCoordinates`
  are explicit.
- The Float64 reader extension and its fixtures are ported unchanged.

## Gates (this branch)

All gates ran through `python3 tools/build/sbt-warm` on the final source.

| Gate | Result |
|---|---|
| `imageJVM/test spatialJVM/test surfaceJVM/test` | exit 0. image 386/386, spatial 229/229, surface 313 total (301 passed, 12 real-asset skips) |
| `imageJS/test spatialJS/test surfaceJS/test` | exit 0. image 356/356, spatial 204/204, surface 258 total (256 passed, 2 skips) |
| `surfaceViewJVM/test` / `surfaceViewJS/test surfaceViewThreeJS/test` (consumers) | exit 0. 70/70 / 70/70 and 18/18 |
| `scalafimCompileAll` | exit 0, with no compiler warnings or errors. The only `[warn]` lines are Intaglio's unused doc-tag build keys and a GC notice |
| Wip as-is, `surfaceJVM/test` | exit 1, 3/308 failed (see above) |
| N3 before the fix: `VolumeToSurfaceParitySuite`, `GridSpecFrameSuite` | each new test failed with `IllegalArgumentException … got Infinity`. Both pass after the fix |
| Placement mutant (first-transform fallback restored) | 2/23 `GiftiPlacementReaderSuite` tests fail. The source was restored byte-exact |

## Not qualified here

- **Hardware GPU.** No browser/WebGL harness is part of this repository, and
  nothing was run on this host's GPU. The software-WebGL receipt from
  2026-09-30 is not re-executed. GPU parity rests on the JS transport-mock
  suite (N4).
- **The real TemplateFlow fsLR asset.** The local
  `~/Library/Caches/templateflow/tpl-fsLR/tpl-fsLR_den-32k_hemi-L_midthickness.surf.gii`
  is an empty placeholder (SHA-256 `e3b0c442…`). With
  `SCALAFIM_REQUIRE_REAL_ASSETS=1`, the real-asset test fails its digest check.
  Without that variable it is skipped. Its new expectation, `Transformed(0, …,
  Talairach)` for the single Talairach transform, has not been executed.
- **N1** (canonical inverse, upstream geometry) and **4b** (non-finite
  qualification policy) are owner decisions.
- **Downstream consumers.** There is no rebuild of external consumers (for
  example PLSNeuro) at a new pin.
