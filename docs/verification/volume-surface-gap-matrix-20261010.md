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
| N1 | Spatial `GridSpec.worldToVoxel` (image4s `Affine.inverse`) and eager/GPU `SampleSpaces.coordToIndex` (inverse matrix product) can choose different voxels at exact oblique half ties: 486/3000 in review | **Declared caveat** `volume-surface.cross-engine-inverse-ties` (owner decision 2026-10-10: do not patch here, fix upstream in image4s) | Both routes use the same inverse matrix, bitwise. They differ only in summation order. image4s `Affine.applyUnchecked` starts from the translation, and `SampleSpaces.transformCoordinates` (`modules/image/shared/src/main/scala/scalafim/image/SampleSpaces.scala:525`) adds it last. Reproduced on this branch with the parity suite's oblique affine. World `(11.75, 3.625, 7.0)` (true voxel `(0, 3.5, 0.5)`) maps to y = `3.4999999999999996` on the spatial route and `3.5` on the eager route. On a 3×4×5 grid, one engine samples `(0, 3, 1)` and the other reports the point outside the volume. Half-integer sweeps differed in 46, 169 and 154 of 480 points for three oblique affines. See the caveat below and the upstream draft `image4s-n1-issue-draft.md` |
| N2 | Owning CPU suites did not detect simultaneous half-down mutants | Resolved on main (`d462806e`) | Absolute half-up/boundary tests per axis: `VolumeToSurfaceParitySuite.scala:158` and `modules/surface/shared/src/test/scala/scalafim/surface/SurfaceSamplingSuite.scala:264`. Mutation kills are recorded in `volume-surface-ties-consumer-pin-20260930.md` |
| N3 | A finite world point whose voxel image overflows makes the spatial conversion throw, while eager rejects it | **Resolved here** | Reproduced on base: `VolumeToSurfaceParitySuite` "nearest and trilinear spatial weights reject world points whose voxel image overflows" threw `IllegalArgumentException: voxel coordinate x coordinate must be finite; got Infinity`. `GridSpecFrameSuite` "world-to-voxel returns Left…" failed the same way when the fix was reverted. Fix: `SpatialCoordinates.finiteImage` (`modules/image/shared/src/main/scala/scalafim/image/SpatialCoordinates.scala:198`) makes the three `Either`-returning point conversions return `GeometryError.NonFiniteCoordinate`. The spatial lookup already maps `Left` to empty weights. The eager overflow test also pins `SurfaceSampleTally(4, 3, 0, 0, 1)` |
| 2a | GIFTI codec drops `DataSpace`/`TransformedSpace`/`GeometricType` | Resolved on main | `GiftiCoordinateDeclaration` (`modules/surface/shared/src/main/scala/scalafim/surface/gifti/GiftiCoordinates.scala:109`, built by `fromPointSet` at `:122`) keeps every transform in file order with its typed spaces and structure/type metadata. `FrameDeclarationSuite` and `DeclaredSurfaceReaderSuite` test it |
| 2b | GIFTI placement silently applies the first transform (`headOption`) whatever its target, unknown or ambiguous | **Resolved here** (C6 re-port) | Base `GiftiSurfaceCodec.transformMatrix` used `pointSet.transforms.headOption`. Placement now goes through `GiftiPlacementResolver` (`GiftiPlacement.scala:69`, called at `GiftiSurfaceCodec.scala:67`). `DeclaredGiftiSurface.placement` (`GiftiCoordinates.scala:145`) names the original transform by index. `GiftiPlacementReaderChecks` runs 23 cases through both platforms' public readers (`GiftiPlacementReaderSuite` on JVM, `GiftiPlacementReaderJsSuite` on JS). A mutant that restores the first-transform fallback fails 2 of them |
| 2d | Identity-only GIFTI declarations (nibabel `UNKNOWN` identity, Workbench blank + Talairach identity pair) | **Owner-accepted decision (2026-10-10)**: they stay unplaced (`GiftiPlacement.NativeCoordinates`) under the default `Unambiguous` selection, rather than being refused or assigned a frame. A single identity with a recognized target is still placed and keeps its provenance | Implemented by the C6 re-port (see the reconciliation below). Cases "unknown identity stays native and claims no frame" and "identity-only workbench pair stays native" (`modules/surface/shared/src/test/scala/scalafim/surface/gifti/GiftiPlacementFixtures.scala:40,41`) run through both platforms' public readers via `GiftiPlacementReaderChecks` |
| 2c | GIFTI coordinates lose precision (Float64 payloads unsupported) | **Resolved here** (C6 re-port) | `NIFTI_TYPE_FLOAT64` reader extension (`GiftiPayloadDecoder.scala:27,62,129`). Integer and byte APIs refuse it. Covered by `GiftiFloat64Suite` (width overflow, short/long/misaligned, ASCII coercion) and `GiftiFloat64Reader{,Js}Suite` (ASCII, base64, gzip and zlib in both endians, exact to 0.0) |
| 3 | `TriangleMesh` lacks structural equality; `SurfaceGeometry ==` compares array identity | Resolved on main (`fa2193ab`) | `TriangleMesh.equals/hashCode` (`modules/surface/shared/src/main/scala/scalafim/surface/TriangleMesh.scala:18,30`); `TriangleMeshEqualitySuite.scala:8,15,20` |
| 4a | `SurfaceProjectionReceipt.requestedSamples` computed, not observed; `acceptedSamples` counts non-finite values | Resolved on main (`fa2193ab`) | `SurfaceSampleTally` with a checked partition invariant (`modules/surface/shared/src/main/scala/scalafim/surface/SurfaceSampling.scala:46`), carried on CPU and GPU (`ThreeVolumeProjector.scala:197`); `SurfaceSamplingSuite.scala:224,240` |
| 4b | `minimumSamples` can qualify a vertex whose only sample is NaN; Average propagates NaN | **Resolved here** (owner decisions 2026-10-10: NaN is not an observation for qualification, and every reducer skips non-finite samples) | `SurfaceSampleResult.sampleCounts` now counts finite samples only, and the new `nonFiniteCounts` field counts non-finite in-mask samples per vertex (`modules/surface/shared/src/main/scala/scalafim/surface/SurfaceSampling.scala`). The CPU projection (`SurfaceVolumeProjection.materialize`) and the GPU projector (`ThreeVolumeProjector`) both qualify on finite samples. Both report the typed partition `SurfaceVertexTally(vertices, qualified, nonFiniteOnly, insufficient)` on `SurfaceProjectionReceipt.vertexTally`, so a NaN-only vertex stays visible as `nonFiniteOnly`. `SurfaceProjectionResult.nonFiniteCounts` carries the per-vertex counts. `SurfaceRoute` already admitted only finite voxels through its admission mask, so it is unchanged. Regressions: `SurfaceProjectionNetworkSuite` "a vertex whose only sample is non-finite…" and "non-finite ribbon samples do not count…", `SurfaceSamplingSuite` "per-vertex finite and non-finite counts…", and `ThreeVolumeProjectorSuite` "valid zero and non-finite texels…" (GPU against the CPU oracle). Before the change, old-API probes of the two projection cases failed 2/2 on base sources, and the GPU expectation `(true, false, false)` failed 1/4. Reducers (second decision): `Nearest`, `Average` and `Mode` reduce finite samples only, and a vertex with no finite sample reduces to NaN (contract in `SurfaceSampleAggregation`'s scaladoc). `Nearest` is the first finite sample in the path's declared order. Each sample's voxel is chosen by rounding half up per axis, and declared order is total, so no other tie rule is needed. `Mode` ties go to the smallest value. `SurfaceVertexSample.acceptedSampleIndices` now lists finite samples only. The GPU path (midpoint + `Nearest`, one sample) already matched, because its single texel is used exactly when it is finite. It now refuses (`InvalidPlan`) any volume with a finite value that overflows float32, which it would otherwise have dropped as an infinite texel while the CPU kept it. Regressions that failed before: `SurfaceSamplingSuite` "Average reduces over finite samples only" and "Nearest takes the first finite sample…", `SurfaceProjectionNetworkSuite` "a qualified vertex's value is reduced from its finite samples only", and `ThreeVolumeProjectorSuite` "GPU refuses finite volume values that float32 cannot represent…". Pins that already passed before: the `Mode` NaN test (boxed NaN keys never compare equal, so NaN could not win), "every reducer returns NaN…", and the GPU/CPU NaN-fill parity test |

| 5 | `spatialJVM` fails to compile at consumer pin `7c3ff0a` against Gale `83cac90` (`DMatBuilder.writeLinear`) | Historical; current graph compiles | The historical pin failure was reproduced and archived in `volume-surface-ties-consumer-pin-20260930.md`. On this branch `scalafimCompileAll` exits 0 on both platforms. No downstream consumer was rebuilt or repinned |

## Declared caveats

These follow the `ScenarioCaveat` shape in `docs/plans/scenario-parity-harness.md`.

| id | kind | severity | owner | follow-up | detail |
|---|---|---|---|---|---|
| `volume-surface.cross-engine-inverse-ties` | `AlgorithmDivergence` | `Actionable` | image4s (geometry) | Upstream issue drafted in `docs/verification/image4s-n1-issue-draft.md`. It asks for one canonical world→voxel routine with documented tie-breaking. When it lands, route `SampleSpaces.coordToIndex` and `GridSpec.worldToVoxel` through that routine and delete this caveat | On oblique grids, the spatial engine and the eager/GPU engines can choose different nearest voxels, or disagree on grid admission, at exact half-voxel ties. Cross-engine voxel identity is qualified only on identity or exactly invertible grids |

No active scenario in `docs/scenarios/manifest.json` compares the spatial
engine with the eager or GPU engines on an oblique grid. No scenario policy
changes, therefore. A future cross-engine scenario on oblique grids must
declare this caveat id and admit `PassWithCaveats` only through an explicit
policy. The cross-engine parity tests in `VolumeToSurfaceParitySuite` stay
restricted to identity or exactly invertible grids. They are not loosened.

### 4b output-value change

Projected values change for every vertex that mixes finite and non-finite
samples. Before, `Average` returned NaN, and `Nearest` returned NaN when the
first in-mask sample was NaN. Both now return the reduction of the finite
samples. A qualified vertex therefore always carries a finite value. A vertex
with only non-finite samples is still NaN in `SurfaceSampleResult.values`, and
the projection marks it `nonFiniteOnly` and fills it. `SurfaceRoute` output is
unchanged, because its admission mask already excluded non-finite voxels. The
spatial `VolumeToSurfaceOperator` is a data-independent linear operator. It
has neither a reducer nor `minimumSamples`, so it was not changed: a NaN in the
input vector still propagates through `forward`, as for any sparse matrix
product.

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
| 4b follow-up: `surfaceJVM/test spatialJVM/test surfaceViewJVM/test` | exit 0. surface 314 total (302 passed, 12 real-asset skips), spatial 229/229, surface-view 73/73 |
| 4b follow-up: `surfaceJS/test spatialJS/test surfaceViewJS/test surfaceViewThreeJS/test` | exit 0. surface 259 total (257 passed, 2 skips), spatial 204/204, surface-view 73/73, surface-view-three 18/18 |
| 4b follow-up: `scalafimCompileAll` | exit 0, with no `[warn]` or `[error]` lines |
| 4b before the change (base sources, old-API probes) | `surfaceViewJVM` 2/2 probes failed: NaN-only midpoint vertex qualified, ribbon NaN counted toward `minimumSamples = 3`. `surfaceViewThreeJS` GPU quality `(true, false, false)` failed 1/4 |
| 4b reducers: `surfaceJVM/test spatialJVM/test surfaceViewJVM/test` | exit 0. surface 318 total (306 passed, 12 skips), spatial 229/229, surface-view 74/74 |
| 4b reducers: `surfaceJS/test spatialJS/test surfaceViewJS/test surfaceViewThreeJS/test` | exit 0. surface 263 total (261 passed, 2 skips), spatial 204/204, surface-view 74/74, surface-view-three 20/20 |
| 4b reducers: `scalafimCompileAll` | exit 0, with no `[warn]` or `[error]` lines |
| 4b reducers before the change | `SurfaceSamplingSuite` 2/23 failed (Average, Nearest got NaN). `SurfaceProjectionNetworkSuite` 1/11 failed (expected 101.0, got NaN). `ThreeVolumeProjectorSuite` 1/6 failed (GPU returned `Right`, with the float32-overflow vertex tallied `nonFiniteOnly` while the CPU qualified it) |
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
- **N1** is a declared caveat that waits on the upstream image4s change. It is not
  patched here.
- **Spatial linear operator and NaN input.** `VolumeToSurfaceOperator.forward`
  propagates NaN like any matrix product. It has no reducer or
  `minimumSamples`, so the 4b decisions do not reach it.
- **Hardware GPU, 4b.** The GPU qualification change is tested only through the
  JS texture-transport mock. It has not run against a real WebGL driver.
- **Downstream consumers.** There is no rebuild of external consumers (for
  example PLSNeuro) at a new pin.
