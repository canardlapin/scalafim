# Group-volume → fsLR mapping: audit and route admission

Ticket: `bd-01M35BHNDCHM6YXCKYX0544TP3` (ScalaFIM). Consumer ticket:
PLSNeuro `bd-01M2421N7AZQX1AK5BEMVSB84K`.
Originally written on `surface/group-fslr-qualification-20260923` (commits
`efbfff1`, `d741dff`, based on the consumer pin `7c3ff0a`, which predates the
image-API refactor). This version records the port onto current `main`
(`integration/main-catchup-20260923`): `SomeScalarVolume`/`SomeMaskVolume`,
`SampleSpaces`, image4s `Affine[D3]` and grid runtime ownership. The per-vertex
receipt API that PLSNeuro uses was ported from `7c3ff0a` alongside it. Findings
in §1 were re-checked against `main`; §3 counts were re-measured after the port.

## Summary

- **The existing kernel.** It performs nearest-voxel point lookup. It carries no
  reference identity, and nothing checked that a volume and a surface share a
  coordinate frame.
- **What this work adds.** A typed admission layer
  (`scalafim.surface.reference`) now binds five things before any value is
  mapped:
  - an exact template frame;
  - an exact voxel grid;
  - the mesh family, density, hemisphere and ordered topology;
  - a medial-wall mask;
  - the frame the anatomy is expressed in.

  The layer refuses routes it cannot justify, and it executes through the
  existing kernel rather than a new one.
- **The PLSNeuro route is refused, correctly.** The route in question maps
  `MNI152NLin2009cAsym` res-2 group results to fsLR 32k. It cannot be qualified
  with the assets that exist today (see "Qualification status" below).
- **Real-data qualification.** This is item 6 of the ticket. It has not been run
  and is blocked on the prerequisites listed below.

## 1. Inventory of existing machinery (re-checked on `main`)

There are three sampling implementations.

| Engine | Location | Lookup | Depth handling | Masks | Reference checks |
|---|---|---|---|---|---|
| A (primary) | `surface/SurfaceSampling.scala` (`VolumeSurfaceSampler`) | nearest voxel, `Math.round` (ties up), support `[-0.5, dim-0.5)` | per-path point lookups reduced by `Nearest`/`Average`/`Mode` | volume mask only (must share the volume grid's runtime owner) | none |
| B (sparse operator) | `spatial/VolumeToSurfaceOperator.scala` | nearest or trilinear (renormalised corners) | 1/#valid per row, fractional coverage | volume and vertex mask | domain ids only |
| GPU | `surface-view-three/js/.../ThreeVolumeProjector.scala` | nearest in float32, same support | midpoint only | — | none |

`VolToSurfMorphism` and `SurfaceVolumeProjection` are thin wrappers over engine A.
PLSNeuro's `SurfaceProjection` calls engine A directly (`Midpoint` + `Nearest`).

### Numerical contracts of engine A

- **Point lookup.** World coordinates come from `surfaceToWorld` (an image4s
  `Affine[D3]`), then the sample space's `coordToIndex` (the inverse of the
  grid's `indexToFrame`), then round to the nearest voxel. There is no
  interpolation.
- **Depth paths.**
  - `Midpoint`: white + ½·(pial − white).
  - `FractionalThickness`: lookups at declared fractions.
  - `NormalLine`: offsets along the unit white→pial vector. This is not a
    surface normal, and it collapses to the midpoint when white equals pial.
- **Averaging over depth is not ribbon mapping.** It combines point lookups with
  equal weight. There is no voxel-overlap weighting anywhere in A. Engine B's
  trilinear path is also point interpolation.
- **Duplicate voxel hits.** Each hit counts separately in `Average` and `Mode`.
- **Nonfinite voxel values.** These are *included*: they raise the count, poison
  `Average`, and may be returned by `Nearest`/`Mode`.
- **Labels versus continuous values.** They are not distinguished. `Mode` uses
  exact `Double` equality, and nothing prevents `Average` on a label volume.
- **Receipts.** `inspectVertex` receipts reconstruct values and counts exactly;
  this is tested (`SurfaceSamplingReceiptSuite`). Bulk `sample` and
  `inspectVertex` share one kernel through an `observe` callback.

### Defects found

**Fixed here:**

- `nearestGrid` narrowed `Math.round`'s `Long` with `.toInt` before the bounds
  check.
  - A coordinate at index 2³²+1 aliased onto voxel 1.
  - A nonfinite index rounded to voxel 0 (`Math.round(NaN) = 0`, and
    `Math.round(-Infinity).toInt = 0`). On `main`, image4s affines are finite by
    construction, but a finite vertex can still overflow to world `-Infinity`,
    whose index is (−∞, NaN, NaN).
  - Both were accepted as valid samples. The index is now checked for finiteness
    and range-checked as a `Long`.
  - Regression test: `SurfaceSamplingSuite` "coordinates beyond Int range or
    nonfinite never alias onto a voxel". The old NaN-translation case is
    unconstructible on `main`, so the nonfinite case uses a vertex at
    x = −1e308 under a ×10 `surfaceToWorld`. With the fix reverted, the
    Int-range cases and the overflow case each fail independently
    (mutation-checked).
- `SurfaceVolumeProjection.scalarLayer` built the layer on the *sampling*
  geometry and used the display geometry only for a compatibility check.
  - The layer now carries the display geometry.
  - Regression: `SurfaceProjectionNetworkSuite` asserts `layer.geometry eq
    inflated`. It fails without the fix (mutation-checked on `main`, where the
    defect was still present).

**Recorded, not changed:**

- Engines B and GPU duplicate engine A's lookup contract. GPU tie parity in
  float32 is untested, and B's trilinear path is untested.
- `GiftiXmlParser` reads `DataSpace`/`TransformedSpace` into `GiftiTransform`,
  but `GiftiSurfaceCodec` keeps only the matrix and drops them (and
  `GeometricType`). A GIFTI file's own frame declaration therefore cannot reach
  admission today.
- `TriangleMesh` has no structural `equals`, so `SurfaceGeometry ==` is
  reference equality on arrays. This affects `SurfaceVertexMapping` and engine
  B's `geometry == plan.surfaces.white`.
- `SurfaceProjectionReceipt.requestedSamples` is computed from the path rather
  than observed. Its `acceptedSamples` includes nonfinite values.
- At `7c3ff0a`, `spatialJVM` did not compile against its pinned Gale `83cac90`
  (`DMatBuilder.writeLinear` missing). That no longer applies: on `main`,
  `spatialJVM/test` compiles and passes (§3).

### Reference identity before this change

- `SampleSpaces` frames all carry the constant metadata label
  `"scalafim-space"` and persistent frame id `scalafim-ras-d3`. Grids get a
  persistent key from frame, dims and the affine's exact bits, which identifies
  a voxel grid but not a template.
- `SurfaceGeometry` has no family, density or frame field.
- There is no medial-wall type.
- `SurfaceKind` has no very-inflated case.
- Typed template ids (`atlas.SpaceIdOf`, `StandardSurface.FsLR32k`) live in
  `atlas`, which `surface` cannot see.
- PLSNeuro's only space check was a string comparison between a manifest
  `template` field and the BIDS `space` entity.

## 2. Route admission API (`modules/surface/.../reference`)

| Type | Admits / refuses |
|---|---|
| `TemplateId`, `TemplateRelease`, `TemplateFrame` | Exact TemplateFlow identifier plus release (and optional cohort). Refuses family names such as `MNI`, `MNI152`, `ICBM152` and `Talairach`. Any `MNI*`/`ICBM*` name must be one of the exact TemplateFlow variants (case-sensitive allowlist), so `MNI2009` or a lowercased variant is refused. Frames compare exactly: no aliases. |
| `VolumeReference` | Frame + the admitted image4s 3-D grid (dims + voxel-to-world `Affine[D3]` in RAS mm; spaces with non-spatial axes are refused) + optional analysis support (`SomeMaskVolume`) on the identical grid. Grid identity is the same live grid *or* the same persistent grid key (frame, dims, bitwise-equal affine), so a separately constructed identical space is admitted and a one-ulp affine change is not. |
| `StandardCorticalMesh` | Family + density + vertices per hemisphere (`FsLR32k` = fsLR, 32k, 32492). |
| `MedialWallMask` | Cortex flags bound to an ordered `SurfaceMeshDomain`. Requires at least one cortical vertex. |
| `CorticalMeshReference` | Mesh + cortical hemisphere + ordered topology anchor + medial wall. Refuses a wrong vertex count, a non-cortical hemisphere, or a mask from another domain. |
| `SamplingAnatomy` | `Midthickness` or `WhitePial` geometry in a **declared** frame, on the reference's exact face order. Refuses inflated or other display shapes, swapped white/pial, and rewound topology. |
| `DisplaySurface` | Inflated / very-inflated / sphere / anatomical display on the same ordered domain. Display coordinates never sample. |
| `FrameBridge` | Explicit `Affine[D3]` from one frame to another, with declared evidence. Finite, homogeneous, invertible and immutable by construction; `Affine.fromRowMajor` copies its input, so later caller mutation cannot change an admitted bridge (tested). Nonlinear warps are not representable. It is composed *after* each surface's own `surfaceToWorld` (`surfaceToWorld.andThen(bridge)`); a composition that image4s rejects is refused as `BridgeComposition`. |
| `SurfaceRoute.admit` / `select` | Refuses mesh or hemisphere mismatch, a frame mismatch without a bridge, reversed or unrelated bridges, a bridge where frames already agree, and depth methods on midthickness-only anatomy. `select` prefers same-frame anatomy and returns every refusal when none is admissible. |
| `AdmittedSurfaceRoute.map` / `inspect` | Takes `SomeScalarVolume[Double]`. Refuses a volume on any other grid (`SourceGridMismatch`), and non-integral categorical values (`NonIntegralLabel`, reported by `VoxelCoord`). Excludes nonfinite and unsupported voxels *before* aggregation. Coverage is `Mapped` / `MedialWall` / `NoSupport`. Per-lookup evidence (`Included` with weight, `NonFinite`, `OutsideSupport`, `OutsideGrid`) is reported in source world millimetres. |
| `MappedSurfaceValues.onDisplay` | Carries the identical value/coverage object onto any display shape admitted against the *same* mesh reference (same mesh, domain and medial wall): no resampling, and vertex ids are preserved. Accessors return `None` for vertices outside the domain. |

### Methods

- **`MidthicknessNearest`.** One nearest-voxel lookup, either at the midthickness
  vertex or at the white/pial midpoint.
- **`DepthNearest(fractions)`.** Requires white and pial. It takes nearest-voxel
  lookups at the declared depths.
  - For **continuous** values it takes their mean.
  - For **categorical** values it takes the modal label, with ties going to the
    smallest label.
  - Neither variant is a volume-weighted ribbon method, and the disclosure does
    not claim one.
- **Value semantics** stay a declared `ValueSemantics` enum on the request
  rather than a `SomeLabelVolume` input. The shared kernel reads `Double`
  values, and categorical routes admit integral-valued floating-point atlases,
  refusing non-integral voxels with a typed error; a `SomeLabelVolume[Int]`
  entry point would need a second kernel instantiation.

## 3. Evidence

All new tests are independent of the kernel. Vertices are placed at chosen
continuous voxel indices through the forward affine, and the volume values encode
their own voxel. Expected values therefore come from construction, not from the
kernel's inverse affine.

The suite is `SurfaceRouteSuite`, with 19 tests (18 ported unchanged in
expectation, plus one for persistent-key grid identity). Canonical ordinals on
`main` are C-order, so fixtures place values by voxel coordinate rather than by
a hand-written linear index. They cover the following
fixtures and behaviours:

- **Oblique, anisotropic, shifted grid.** Rotation about z by 30° and about x by
  20°; spacing 2/3/1.5 mm; translation (−7.25, 4.5, 11).
- **Asymmetric ramp and impulse.** Zero is treated as data.
- **Partial depth coverage** and **outside-grid** vertices.
- **Exact .5 ties.**
  - 1.5 → 2.
  - −0.5 is inside the grid.
  - `dim−0.5` is outside.
  - 3.49 → 3.
  - −0.51 is outside.
- **Medial wall.**
- **Nonfinite and unsupported voxel values.**
  - NaN and +Inf are excluded and disclosed.
  - An all-NaN volume gives `NoSupport`.
  - A support-mask exclusion is disclosed.
- **Evidence reconstruction.** Σ weight·value equals the mapped value for every
  vertex. Continuous weights are 1/n. Categorical weights are 1/k on the k lookups
  carrying the modal label and 0 on the rest; this is tested explicitly.
- **Categorical mode and ties**, plus refusal of non-integral labels.
- **Grid mismatch at execution**, and **grid identity by persistent key**: a
  twin space with the same dims and affine (distinct live grid) maps
  identically and admits its support mask; a one-ulp translation change is
  refused.
- **Rigid bridge.** It reproduces the same-frame result exactly, with anatomy
  carrying its own non-commuting `surfaceToWorld`. Reversing the composition
  order fails the test (mutation-checked).
- **Every admission refusal.**
- **fsLR 32k count, hemisphere and medial-wall binding** at full 32 492-vertex
  size.
- **Identity of inflated and very-inflated values and coverage.**
- **The PLSNeuro source grid being refused** against fsLR anatomy declared in
  `MNI152NLin6Asym`.

Re-measured on 2026-09-23 after the port, on
`integration/main-catchup-20260923` with the dependency pins in `build.sbt`.
(The pre-port counts on `7c3ff0a` were 151/151/91/124/151/35 and are not
comparable: the module test sets differ between the two lines.)

| Target | Result |
|---|---|
| `surfaceJVM/test` | 140/140 |
| `surfaceViewJVM/test` | 49/49 |
| `atlasJVM/test` | 90/90 |
| `spatialJVM/test` | 140/140 |
| `surfaceJS/test` | 114/114, including all 19 route tests |
| `surfaceViewJS/test` | 49/49 |
| Route + sampling suites under Scala.js `FullOpt` | 36/36 (19 route, 13 sampling, 4 receipt) |

`scalafimCompileAll` is warning-clean. Both kernel regressions and the bridge
composition order were mutation-checked on the ported code: each reverted fix,
and `bridge.andThen(surfaceToWorld)` in place of
`surfaceToWorld.andThen(bridge)`, fails its test.

## 4. Qualification status: `MNI152NLin2009cAsym` res-2 → fsLR 32k

**Status: refused. Not qualified.**

The source is PLSNeuro group results on a grid of 97×115×97 at 2 mm, with origin
(−96.5, −132.5, −78.5), in `MNI152NLin2009cAsym`. Four things block
qualification:

1. **No real fsLR 32k assets are present.** Every `tpl-fsLR` GIFTI in the local
   TemplateFlow cache (`~/Library/Caches/templateflow`) is 0 bytes. The files
   affected are:
   - midthickness, inflated, very-inflated and sphere;
   - `desc-nomedialwall_dparc`.

   The templateflow4s cache holds no fsLR files at all.
2. **TemplateFlow fsLR has no white/pial pair.** Only `MidthicknessNearest` is
   therefore possible from it. `DepthNearest`, and any future ribbon method,
   needs a corresponding white/pial pair that is explicitly pinned.
3. **The anatomy's frame is undeclared.** TemplateFlow's fsLR surfaces derive
   from HCP pipelines, whose coordinates are conventionally FSL MNI152, i.e.
   ≈`MNI152NLin6Asym`. That is *not* `MNI152NLin2009cAsym`. The file names do
   not declare a frame, and ScalaFIM's GIFTI codec drops `dataSpace`. The frame
   must be declared from asset provenance, not assumed.
4. **No exact bridge exists.** 6Asym ↔ 2009c is a nonlinear warp.
   - TemplateFlow ships it as an ANTs `.h5`, and it is 0 bytes in the local
     cache.
   - ScalaFIM has no warp-field volume→surface capability.
   - `FrameBridge` deliberately admits only exact affines.

Any one of the following would open an exact route; each must be qualified
separately:

- **(a)** Group results produced in `MNI152NLin6Asym` itself: a same-frame
  route.
- **(b)** fsLR 32k anatomy registered to `MNI152NLin2009cAsym`, with its
  provenance recorded: a same-frame route.
- **(c)** A new, separately qualified nonlinear bridge capability that uses the
  exact TemplateFlow transform.

A generic `MNI` alias will never be used.

### Frozen budgets for the eventual real-data run

These are declared before any real data is evaluated and must not be relaxed
after it.

- **Values.**
  - Over cortical vertices, admitted `MidthicknessNearest` values must be
    identical (|Δ| ≤ 1e-9·max(1,|v|)) to an independent implementation
    (Connectome Workbench `-volume-to-surface-mapping -enclosing` on the same
    anatomy and volume).
  - Disagreements are permitted only at vertices whose continuous index lies
    within 1e-6 of a .5 voxel boundary (tie convention). They must be counted
    and reported, and must not exceed 0.1 % of cortical vertices.
- **Coverage.**
  - `MedialWall` equals the admitted mask's medial count exactly.
  - `NoSupport` must be ≤ 3 % of cortical vertices for a whole-brain analysis
    support. Every such vertex must be explained by a receipt showing
    `OutsideSupport`, `NonFinite` or `OutsideGrid`.
- **Display identity.** Values, coverage and vertex ids must be bitwise
  identical between inflated and very-inflated. This is structural: the same
  object.
- **Picks.** For 20 sampled vertices per hemisphere, the receipt voxel must equal
  the independent implementation's voxel, and the pick must link to that source
  voxel.
- **Resources.**
  - One 32 492-vertex hemisphere from a 97×115×97 volume: ≤ 500 ms warm on the
    JVM and ≤ 2 s on Scala.js `FullOpt`.
  - ≤ 64 MB additional heap per mapped volume and hemisphere.
- **Scope.** Beta and FIR fixtures (`plsneuro-fixtures/real-usable-{beta,fir}-20260911`)
  are mapped only through an admitted route. Original group-volume statistics
  are retained unchanged. Cortical display is derived presentation, not
  surface-native inference.
