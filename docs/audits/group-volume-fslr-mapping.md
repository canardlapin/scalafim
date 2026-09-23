# Group-volume → fsLR mapping: audit and route admission

Ticket: `bd-01M35BHNDCHM6YXCKYX0544TP3` (ScalaFIM). Consumer ticket:
PLSNeuro `bd-01M2421N7AZQX1AK5BEMVSB84K`.
Base: consumer pin `7c3ff0a` (branch `surface/warm-update-20260921`), which is
**not** an ancestor of `main`. The per-vertex receipt API used by PLSNeuro exists
only on the pinned branch, so this work is on
`surface/group-fslr-qualification-20260923`, based on `7c3ff0a`.

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

## 1. Inventory of existing machinery (at `7c3ff0a`)

There are three sampling implementations.

| Engine | Location | Lookup | Depth handling | Masks | Reference checks |
|---|---|---|---|---|---|
| A (primary) | `surface/SurfaceSampling.scala` (`VolumeSurfaceSampler`) | nearest voxel, `Math.round` (ties up), support `[-0.5, dim-0.5)` | per-path point lookups reduced by `Nearest`/`Average`/`Mode` | volume mask only (grid must match) | none |
| B (sparse operator) | `spatial/VolumeToSurfaceOperator.scala` | nearest or trilinear (renormalised corners) | 1/#valid per row, fractional coverage | volume and vertex mask | domain ids only |
| GPU | `surface-view-three/js/.../ThreeVolumeProjector.scala` | nearest in float32, same support | midpoint only | — | none |

`VolToSurfMorphism` and `SurfaceVolumeProjection` are thin wrappers over engine A.
PLSNeuro's `SurfaceProjection` calls engine A directly (`Midpoint` + `Nearest`).

### Numerical contracts of engine A

- **Point lookup.** World coordinates come from `surfaceToWorld`, then
  `NeuroSpace.coordToIndex` (the inverse affine), then round to the nearest
  voxel. There is no interpolation.
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
  this is tested.

### Defects found

**Fixed here:**

- `nearestGrid` narrowed `Math.round`'s `Long` with `.toInt` before the bounds
  check.
  - A coordinate at index 2³²+1 aliased onto voxel 1.
  - A NaN index (possible through a nonfinite `surfaceToWorld`) rounded to
    voxel 0.
  - Both were accepted as valid samples. The index is now checked for finiteness
    and range-checked as a `Long`.
  - Regression test: `SurfaceSamplingSuite` "coordinates beyond Int range or
    nonfinite never alias onto a voxel". It fails without the fix.
- `SurfaceVolumeProjection.scalarLayer` built the layer on the *sampling*
  geometry and used the display geometry only for a compatibility check.
  - The layer now carries the display geometry.
  - Regression: `SurfaceProjectionNetworkSuite` asserts `layer.geometry eq
    inflated`. It fails without the fix.

**Recorded, not changed:**

- Engines B and GPU duplicate engine A's lookup contract. GPU tie parity in
  float32 is untested, and B's trilinear path is untested.
- `GiftiSurfaceCodec` drops the `dataSpace`/`transformedSpace` fields and
  `GeometricType`. A GIFTI file's own frame declaration therefore cannot reach
  admission today.
- `TriangleMesh` has no structural `equals`, so `SurfaceGeometry ==` is
  reference equality on arrays. This affects `SurfaceVertexMapping` and engine
  B's `geometry == plan.surfaces.white`.
- `SurfaceProjectionReceipt.requestedSamples` is computed from the path rather
  than observed. Its `acceptedSamples` includes nonfinite values.
- `spatialJVM` does not compile at `7c3ff0a` against its pinned Gale `83cac90`:
  `DMatBuilder.writeLinear` is missing. This is pre-existing. PLSNeuro does not
  build `spatial`.

### Reference identity before this change

- `NeuroSpace` frames all carry the constant label `"scalafim-space"`.
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
| `VolumeReference` | Frame + 3-D grid (dims + finite, invertible voxel-to-world affine in RAS mm) + optional analysis support on the identical grid. |
| `StandardCorticalMesh` | Family + density + vertices per hemisphere (`FsLR32k` = fsLR, 32k, 32492). |
| `MedialWallMask` | Cortex flags bound to an ordered `SurfaceMeshDomain`. Requires at least one cortical vertex. |
| `CorticalMeshReference` | Mesh + cortical hemisphere + ordered topology anchor + medial wall. Refuses a wrong vertex count, a non-cortical hemisphere, or a mask from another domain. |
| `SamplingAnatomy` | `Midthickness` or `WhitePial` geometry in a **declared** frame, on the reference's exact face order. Refuses inflated or other display shapes, swapped white/pial, and rewound topology. |
| `DisplaySurface` | Inflated / very-inflated / sphere / anatomical display on the same ordered domain. Display coordinates never sample. |
| `FrameBridge` | Explicit finite, invertible affine from one frame to another, with declared evidence. Nonlinear warps are not representable. Admitted matrices (and `VolumeReference` affines) are owned copies, so later caller mutation cannot change them. |
| `SurfaceRoute.admit` / `select` | Refuses mesh or hemisphere mismatch, a frame mismatch without a bridge, reversed or unrelated bridges, a bridge where frames already agree, and depth methods on midthickness-only anatomy. `select` prefers same-frame anatomy and returns every refusal when none is admissible. |
| `AdmittedSurfaceRoute.map` / `inspect` | Refuses a volume on any other grid, and non-integral categorical values. Excludes nonfinite and unsupported voxels *before* aggregation. Coverage is `Mapped` / `MedialWall` / `NoSupport`. Per-lookup evidence (`Included` with weight, `NonFinite`, `OutsideSupport`, `OutsideGrid`) is reported in source world millimetres. |
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

## 3. Evidence

All new tests are independent of the kernel. Vertices are placed at chosen
continuous voxel indices through the forward affine, and the volume values encode
their own voxel. Expected values therefore come from construction, not from the
kernel's inverse affine.

The suite is `SurfaceRouteSuite`, with 18 tests. They cover the following
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
- **Grid mismatch at execution.**
- **Rigid bridge.** It reproduces the same-frame result exactly, with anatomy
  carrying its own non-commuting `surfaceToWorld`. Reversing the composition
  order fails the test (mutation-checked).
- **Every admission refusal.**
- **fsLR 32k count, hemisphere and medial-wall binding** at full 32 492-vertex
  size.
- **Identity of inflated and very-inflated values and coverage.**
- **The PLSNeuro source grid being refused** against fsLR anatomy declared in
  `MNI152NLin6Asym`.

Runs on 2026-09-23 used intaglio `8bef37e`, which is the consumer's provider pin,
for `surface-view`. The results were:

| Target | Result |
|---|---|
| `surfaceJVM/test` | 151/151 |
| `surfaceViewJVM/test` | 151/151 |
| `atlasJVM/test` | 91/91 (run before the review fixes, which touched only `reference/`) |
| `surfaceJS/test` | 124/124, including all 18 route tests |
| `surfaceViewJS/test` | 151/151 |
| Route + sampling suites under Scala.js `FullOpt` | 35/35 |

Both kernel regressions were confirmed to fail on the unfixed code.

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
