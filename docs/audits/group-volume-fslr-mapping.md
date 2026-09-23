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
  (`scalafim.surface.reference`) binds these at route admission:
  - an exact template frame for the source;
  - an exact voxel grid;
  - the mesh family, density, hemisphere and ordered topology;
  - a medial-wall mask;
  - the frame the anatomy is expressed in, taken only from digest-bound
    `FrameDeclaration`s of the surface files.

  At execution, a route maps only a `DeclaredVolume`: a volume whose frame
  declaration is bound to the exact bytes it was decoded from. The route refuses
  it unless the declared frame equals the source frame (`SourceFrameMismatch`)
  and the grid matches. Grid identity alone cannot tell templates apart: a
  `MNI152NLin6Asym` volume can sit on a grid identical to a
  `MNI152NLin2009cAsym` one. Before this change such a volume was admitted
  silently; the fresh-context review found this.

  The layer refuses routes it cannot justify, and it executes through the
  existing kernel rather than a new one.
- **The PLSNeuro route.** It maps `MNI152NLin2009cAsym` res-2 group results to
  fsLR 32k. It was first refused, correctly. After WS3 it is admissible for
  `MidthicknessNearest`: fsLR anatomy is declared in `MNI152NLin6Asym`, and a
  digest-bound TemplateFlow point map moves it into 2009c through its per-vertex
  inverse (§5). The oracle, inverse-consistency and GM-evidence gates pass.
- **Real-data qualification.** This is item 6 of the ticket. The frozen budgets
  in §4, against an independent implementation, have not been run yet.

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
  - Regression tests in `SurfaceSamplingSuite`:
    - "indices beyond Int range never alias onto a voxel" pins the `Long`
      range check.
    - "NaN indices from world-coordinate overflow never round onto voxel 0"
      pins the finiteness check. The old NaN-translation case is
      unconstructible on `main`, so a vertex at (1e308, −1e308, 0) under a ×10
      in-plane `surfaceToWorld` lands at world (+∞, −∞, 0). Every index axis is
      then NaN, and `Math.round(NaN) = 0` is in range on every axis.
  - Mutation checks: reverting the whole fix fails both tests. Removing only the
    finiteness check fails the NaN test.
  - **Correction.** The earlier port used a vertex at x = −1e308, whose index is
    (−∞, NaN, NaN). The −∞ axis is refused by the `Long` range check alone, so
    that fixture failed only when the whole fix was reverted; it did not pin the
    finiteness check.
- `SurfaceVolumeProjection.scalarLayer` built the layer on the *sampling*
  geometry and used the display geometry only for a compatibility check.
  - The layer now carries the display geometry.
  - Regression: `SurfaceProjectionNetworkSuite` asserts `layer.geometry eq
    inflated`. It fails without the fix (mutation-checked on `main`, where the
    defect was still present).

**Recorded, not changed:**

- Engines B and GPU duplicate engine A's lookup contract. GPU tie parity in
  float32 is untested, and B's trilinear path is untested.
- `GiftiXmlParser` read `DataSpace`/`TransformedSpace` into `GiftiTransform`,
  but `GiftiSurfaceCodec` kept only the first matrix and dropped them (and
  `GeometricType`). **Addressed in WS2 (§2a):** the declared read path now
  retains every coordinate system and the structure/type metadata as a
  `GiftiCoordinateDeclaration`. The codes are generic and still never yield a
  template frame.
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
| `VolumeReference` | Frame + the admitted image4s 3-D grid (dims + voxel-to-world `Affine[D3]` in RAS mm; spaces with non-spatial axes are refused) + optional analysis support (`SomeMaskVolume`) on the identical grid. Grid identity is either the same live grid, or the same frame (live owner or persistent frame key) with equal dims and an affine equal under `==` element by element. So a separately constructed identical space is admitted, and `-0.0`/`0.0` compare equal; the persistent grid key encodes raw bits and would not. A one-ulp affine change is refused. Ephemeral grids match only grids in the same live frame. The `VolumeReference` frame is a declaration about the grid; it is enforced against each volume's own digest-bound declaration at execution. |
| `StandardCorticalMesh` | Family + density + vertices per hemisphere (`FsLR32k` = fsLR, 32k, 32492). `declare` refuses a published (family, density) with any other count (fsLR 32k/164k; fsaverage 3k/10k/41k/164k), so a mis-declared mesh cannot display as the standard one. |
| `MedialWallMask` | Cortex flags bound to an ordered `SurfaceMeshDomain`. Requires at least one cortical vertex. |
| `CorticalMeshReference` | Mesh + cortical hemisphere + ordered topology anchor + medial wall. Refuses a wrong vertex count, a non-cortical hemisphere, or a mask from another domain. |
| `SamplingAnatomy` | `Midthickness(DeclaredSurface)` or `WhitePial(DeclaredSurface, DeclaredSurface)` on the reference's exact face order. Its frame is taken **only** from the surfaces' `FrameDeclaration`s, which must agree; no frame argument exists. Refuses inflated or other display shapes, swapped white/pial, rewound topology, and white/pial declared in different frames. Route disclosures carry the declarations (`anatomyDeclarations`). |
| `DisplaySurface` | Inflated / very-inflated / sphere / anatomical display on the same ordered domain. Display coordinates never sample, so a display surface carries no frame and is not digest-bound; its identity is the mesh reference's ordered topology and medial wall. |
| `FrameBridge` | Explicit `Affine[D3]` from one frame to another, with declared evidence. Finite, homogeneous, invertible and immutable by construction; `Affine.fromRowMajor` copies its input, so later caller mutation cannot change an admitted bridge (tested). Nonlinear warps are not representable. It is composed *after* each surface's own `surfaceToWorld` (`surfaceToWorld.andThen(bridge)`); a composition that image4s rejects is refused as `BridgeComposition`. |
| `SurfaceRoute.admit` / `select` | Refuses mesh or hemisphere mismatch, a frame mismatch without a bridge, reversed or unrelated bridges, a bridge where frames already agree, and depth methods on midthickness-only anatomy. `select` prefers same-frame anatomy and returns every refusal when none is admissible. |
| `AdmittedSurfaceRoute.prepare` / `map` / `inspect` | Takes a `DeclaredVolume`. `prepare` checks it once and returns a `PreparedSource` that holds the admission mask, which `map` and every `inspect` reuse on the same kernel; a pick no longer rebuilds a whole-volume mask. A `PreparedSource` from another route is refused (`ForeignPreparation`). Refuses a volume declared in another frame (`SourceFrameMismatch`), one on any other grid (`SourceGridMismatch`), and non-integral categorical values (`NonIntegralLabel`, reported by `VoxelCoord`). Mapped values carry the source declaration. Excludes nonfinite and unsupported voxels *before* aggregation. Coverage is `Mapped` / `MedialWall` / `NoSupport`. Per-lookup evidence (`Included` with weight, `NonFinite`, `OutsideSupport`, `OutsideGrid`) is reported in source world millimetres. |
| `MappedSurfaceValues.onDisplay` | Carries the identical value/coverage object onto any display shape admitted against the *same* mesh reference (same mesh, domain and medial wall): no resampling, and vertex ids are preserved. Accessors return `None` for vertices outside the domain. |

### 2a. Frame declaration (WS2)

| Type | Admits / refuses |
|---|---|
| `GiftiDeclaredSpace`, `GiftiCoordinateSystem`, `GiftiCoordinateDeclaration` (`surface.gifti`) | Every pointset `CoordinateSystemTransformMatrix` in file order: `NIFTI_XFORM_{UNKNOWN,SCANNER_ANAT,ALIGNED_ANAT,TALAIRACH,MNI_152}` or `Other(text)`, blank/absent as `None`, and the 16 row-major values (an affine view is checked on demand). Also `GeometricType`, `AnatomicalStructurePrimary` and `AnatomicalStructureSecondary` from pointset metadata, falling back to document metadata. Exposed by `GiftiSurfaceReader.readDeclaredEither` (JVM) and `readDeclared`/`readDeclaredString` (Scala.js) beside the existing readers. **There is no conversion to `TemplateFrame`.** |
| `AssetSha256`, `AssetProvenance` | 64 lowercase hex digits; template id; archive path that is normalized, relative and starts with `tpl-<template>/`; non-blank catalog revision. |
| `FrameBasis` | `Literature(doi, statement)` (bare DOI `10.NNNN/...`, non-blank statement) or `Derived(recipe, inputs)` (non-blank recipe, at least one provenance-bound input). |
| `DeclaredAsset` | `AssetProvenance` (TemplateFlow archive asset) or `DataAsset(name, sha256)` for anything else, such as a group-result NIfTI or a PLS bundle. |
| `FrameDeclaration` | `(frame, basis, asset)`. A derivation may not consume its own asset. This is the only source of an anatomy frame and of a source volume's frame. |
| `DeclaredVolume` | No public constructor. JVM `DeclaredVolumeReader.readNifti` reads the file once, refuses a digest mismatch, and decodes a private copy of exactly those bytes. There is no Scala.js NIfTI loader yet. A `private[reference]` `unsafeAssumeVerified` exists for synthetic tests only. |
| `DeclaredSurface` | No public constructor. `DeclaredSurfaceReader.read` (JVM: path; Scala.js: `Uint8Array`, copied once) hashes the exact bytes with the portable SHA-256, refuses a digest mismatch (`DigestMismatch`), decodes those same bytes, and refuses a file whose own `AnatomicalStructurePrimary`, or (for `Anatomical` geometry) `AnatomicalStructureSecondary`/type, contradicts the requested hemisphere and kind (`DeclarationConflict`). A `private[reference]` `unsafeAssumeVerified` exists for synthetic tests only. |
| `FrameEvidence` | Disconfirmation receipt: per-`Placement` (`Bridged`, `Raw`, `Reversed`, `Shifted(axis, mm)`) mean GM probability, fraction with p > 0.5, scored and excluded vertex counts; declared `FrameEvidenceThresholds` (default +0.03 over raw, +0.05 over reversed, every ±3 mm shift lower); a total `verdict` returning `Pass` or every failure (missing placement, differing vertex population, insufficient margin, shift not worse). Types and unit tests only; the real-asset evidence suite comes with the WS3 bridge. |

Hashing uses `zarr4s.PortableSha256` from zarr4s-core, which has no
dependencies, through a new `surface → zarr4s-core` edge. Its single call site
is `private[surface] SurfaceDigest.sha256Hex`, so the implementation can move
without touching the witness API. It matches the FIPS
180-2 vectors on both platforms, and the JDK's `MessageDigest` on the real
asset.

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

WS2 (frame declaration, plus review follow-ups) measured on 2026-09-23; WS3 counts follow below.

| Target | Result |
|---|---|
| `surfaceJVM/test` | 170/170. This includes `SurfaceRouteSuite` (26), `GiftiCoordinateDeclarationSuite` (5), `FrameDeclarationSuite` (7), `FrameEvidenceSuite` (5), `DeclaredSurfaceReaderSuite` (3) and `DeclaredVolumeReaderSuite` (1). The real-asset test ran against `tpl-fsLR_den-32k_hemi-L_midthickness.surf.gii` (sha256 `036a8b6c…f1af`): Talairach/Talairach, `CortexLeft`, `MidThickness`, `Anatomical`, 32 492 vertices. It is skipped, and reported as skipped, when the file is absent from `$TEMPLATEFLOW_HOME/tpl-fsLR`, `~/.cache/templateflow/tpl-fsLR` and `$SCALAFIM_FSLR_ASSET_DIR`. |
| `surfaceViewJVM` / `atlasJVM` / `spatialJVM` | 49/49, 90/90, 140/140 |
| `surfaceJS/test` | 142/142 |
| `surfaceViewJS` / `atlasJS` / `spatialJS` / `surfaceViewRasterJS` / `surfaceViewConnectivityJS` | 49/49, 62/62, 116/116, 11/11, 2/2 |
| Reference, GIFTI and sampling suites under Scala.js `FullOpt` | 73/73 |
| `FullOpt` links (`Test/fullLinkJS`) | `surfaceViewJS`, `atlasJS`, `spatialJS`, `surfaceViewThreeJS`, `mvpaSpatialJS`, and `surfaceViewExamplesJS/fullLinkJS`: all link with the new dependency |

WS3 (displacement bridge) re-measured on 2026-09-23:

| Target | Result |
|---|---|
| `surfaceJVM/test` | 202/202 after the WS3 review fixes, run locally with `SCALAFIM_REQUIRE_REAL_ASSETS=1` so the real-asset suites could not skip |
| `surfaceJS/test` | 166/166 |
| Reference, GIFTI and sampling suites under Scala.js `FullOpt` | 97/97 |
| `surfaceViewJVM` / `atlasJVM` / `spatialJVM` | 49/49, 90/90, 140/140 |
| `surfaceViewJS/test` | 49/49 |
| `FullOpt` links | `atlasJS`, `spatialJS` (`Test/fullLinkJS`), `surfaceViewExamplesJS/fullLinkJS` |

Real-asset suites skip when their inputs are absent, and are reported as
skipped. `SCALAFIM_REQUIRE_REAL_ASSETS=1` turns absence into failure; this was
checked in both directions. Counts marked as real-asset runs are local runs with
the locked cache present.

Mutation checks on the follow-ups each fail their test:
- removing the source-frame check;
- restoring persistent-key-only grid identity (signed zero);
- removing the finiteness check (see "Defects found").

`scalafimCompileAll` is warning-clean. Both kernel regressions and the bridge
composition order were mutation-checked on the ported code: each reverted fix,
and `bridge.andThen(surfaceToWorld)` in place of
`surfaceToWorld.andThen(bridge)`, fails its test.

## 4. Qualification status: `MNI152NLin2009cAsym` res-2 → fsLR 32k

**Status (after WS3): admissible for `MidthicknessNearest` through the inverse
point-map bridge; the WS3 gates pass; not yet qualified against the frozen
real-data budgets below.** Items 1, 3 and 4 below are resolved as noted; item 2
still restricts the method.

The source is PLSNeuro group results on a grid of 97×115×97 at 2 mm, with origin
(−96.5, −132.5, −78.5), in `MNI152NLin2009cAsym`. As first recorded, four things
blocked qualification:

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
   not declare a frame, and the GIFTI itself declares only the generic
   `NIFTI_XFORM_TALAIRACH` (now retained, §2a). The frame must be declared from
   asset provenance, not assumed. WS2 supplies the mechanism: a digest-bound
   `FrameDeclaration` with a literature or derived basis. **Resolved (WS3):** the
   `FrameEvidence` receipt on the locked assets passes (§5).
4. **No exact bridge exists.** 6Asym ↔ 2009c is a nonlinear warp.
   - TemplateFlow ships it as an ANTs `.h5`, and it is 0 bytes in the local
     cache.
   - ScalaFIM has no warp-field volume→surface capability.
   - `FrameBridge` deliberately admits only exact affines.

   **Resolved (WS3), §5:** `FrameBridge.displacement` applies the digest-bound
   converted `tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym` point map through
   its per-vertex inverse. Item 1 is also resolved: the locked fsLR assets are
   present in `~/.cache/templateflow` with the digests in
   `docs/audits/templateflow-mni-transform-direction.md`.

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

## 5. Displacement-field bridge (WS3)

### Types (`scalafim.surface.reference`)

| Type | Admits / refuses |
|---|---|
| `DisplacementField` | Dense component-planar field on a voxel grid (`voxelToRas: Affine[D3]`). Evaluation is ITK `DisplacementFieldTransform` with linear interpolation. With `ci = voxelToRas⁻¹·x`, the displacement is zero unless `−0.5 ≤ ci < n − 0.5` on every axis. Inside, it is trilinear over the 8 surrounding centres with neighbour indices clamped to `[0, n−1]`. Nonfinite points are never inside. The inner loop is allocation-free over primitive arrays. |
| `PointMap` | Ordered `AffineStage` / `DisplacementStage` composite; `stages(0)` is applied first. `forwardInto` has ITK semantics. `forward` returns `Mapped`, or `OutsideSupport` if any displacement stage saw its point outside its field. `inverse` is the fixed-point iteration `y ← y + (x − T(y))` from `y = x` under an `InversePolicy(toleranceMm, maxIterations)`. It returns `Converged(point, residual, iterations)` or `NonConvergent(lastIterate, residual, iterations)`, which is never admitted. It returns `OutsideSupport` when the solution lies outside a field (nothing is extrapolated), and `NonFinite` when evaluation overflows. |
| `PointMapManifest`, `DeclaredPointMap` | A `templateflow4s.point-map/1` manifest verified before use. Its bytes must match the caller's expected manifest SHA-256 before parsing (`PointMapManifest.verified`; the constructor is package-private). So an edited affine, reordered stages or swapped frames cannot pass as the declared manifest, and the digest is recorded in the bridge disclosure. Other checks: schema; quarantine (either `frames.quarantine` or `QUARANTINED` in `frames.derivation`, re-checked in `fromManifest`), refused with no override; source SHA-256 equal to the one the caller expects; frames equal to those implied by the TemplateFlow name `tpl-X/tpl-X_from-Y_mode-image_xfm.h5` (input X, output Y; otherwise `PointMapFrameMismatch`); frame ids as exact `TemplateId`s. For each displacement stage file it also checks: a plain `stage-<i>-displacement.nii` name, byte count, SHA-256, NIfTI-1 header (`sizeof_hdr`, magic, `dim = [5,nx,ny,nz,1,3,1,1]`, datatype/bitpix 64, intent 1007, `vox_offset` 352, `sform_code` 5, `scl_slope`/`scl_inter` 0/0 or 1/0), and float32 sform equal to the manifest `voxelToRas` within float32 rounding. The source becomes an `AssetProvenance` when the manifest has a catalog revision, otherwise a `DataAsset`. |
| `FrameBridge` | `transform: BridgeTransform` = `AffineMap(Affine[D3])` or `Displacement(DeclaredPointMap, PointMapUse)`. Endpoints come from the map's frames and use: `Forward` is input→output, `Inverse(policy)` is output→input. Their release is the map's catalog revision; an explicit release is required when the map has none and must agree when both exist. `ReversedBridge` / `BridgeMismatch` apply unchanged. The display and disclosure carry the use, tolerance, source digest and stage digests. |
| Route placement | Each vertex is taken through its surface's `surfaceToWorld` and then the point map (per vertex, once at admission). The placed coordinates feed the unchanged sampling kernel. A vertex whose outcome is not `Mapped`/`Converged` (on every anatomical surface) is `VertexCoverage.BridgeUnavailable`. It is skipped by the kernel (`sampleSelected`), so it is never looked up at a fabricated coordinate, and `inspect` reports its outcome(s) with no samples. `inspect` evidence carries the per-vertex outcome and residual. |
| `DeclaredPointMapReader` (JVM) | Reads `manifest.json` (ujson) and each stage file once, then verifies them through `DeclaredPointMap.fromManifest`. The Scala.js side has the shared model and verification but no directory reader. |

White/pial anatomy is refused through a point-map bridge
(`RouteRefusal.DepthThroughPointMap`), for `MidthicknessNearest` (the white/pial
midpoint) and `DepthNearest` alike. The only placement available would warp the
two endpoints and interpolate depth points between them. That is a second-order
approximation of warping each depth point. It stays refused until per-depth
warping exists. Midthickness anatomy is placed vertex by vertex.

### Evidence

- **Analytic (shared; JVM, JS, FullOpt).** Constant, linear-in-position and
  small-rotation fields reproduce their closed-form forward and inverse maps. The
  ITK border is tested exactly: −0.5 is inside, `n − 0.5` is outside, the
  half-voxel border is clamped, and beyond it the displacement is zero. Stage
  order A∘D vs D∘A is distinguishable (the field has an offset, since a uniform
  scale commutes with a homogeneous linear field). Inversion is tested for
  `NonConvergent` within a one-iteration budget and for `OutsideSupport`.
  Manifest refusals (quarantine, digests, header, sform, file name, schema,
  family-name frames) are tested, as are bridge endpoints and releases, and
  route placement against a same-frame route, including `BridgeUnavailable` and
  `ReversedBridge` for forward use.
- **Synthetic ITK oracle (shared and JVM reader).** templateflow4s's canonical
  synthetic map: an oblique 5×6×7 displacement grid, then an affine. It is
  compared against SimpleITK 2.5.6 on 120 points (interior, voxel centres,
  border, outside): max |Δ| = 5.7e-14 mm. Budget: 1e-9 mm.
- **Real forward.** The converted `2e3869a0…` map (stage `4e964918…`,
  193×229×193, then an affine) against the templateflow4s SimpleITK oracle on
  200 points: max |Δ| = 2.8e-14 mm. Budget: 1e-6 mm.
- **Real inverse.** 5 000 fsLR 32k vertices (every 13th per hemisphere),
  against SimpleITK's 12-iteration fixed-point iterates (fixture
  `scalafim.fslr-inverse-oracle/1`, sha256 `8a0fac18…`, committed gzipped):
  max |Δ| = 1.1e-9 mm. Budget: 1e-6 mm.
  - ITK has no inverse for `DisplacementFieldTransform`. The fixture is SimpleITK
    forward arithmetic under the same fixed-point scheme, so it validates forward
    arithmetic and convergence agreement, not an independent inverse method.
  - The convergence status at 1e-9 mm agrees with the fixture's residual at every
    vertex. Every vertex converges under the production policy (1e-6 mm, ≤ 50
    iterations), and none lies outside the field.
- **Real `FrameEvidence`.** 2009c res-01 GM probseg as a `DeclaredVolume`, fsLR
  32k midthickness L+R as `DeclaredSurface`s in `MNI152NLin6Asym`, cortex from
  `desc-nomedialwall` (59 412 vertices), nearest voxel through the production
  kernel. The production route with the inverse bridge maps exactly the bridged
  placement's values at every cortical vertex. `verdict = Pass` over both
  hemispheres:

  | Placement | Mean GM p | Fraction p > 0.5 |
  |---|---|---|
  | Bridged (inverse, tolerance 1e-6 mm, ≤ 50 iterations) | 0.7027 | 0.7937 |
  | Raw | 0.6702 | 0.7427 |
  | Reversed (forward map) | 0.6180 | 0.6754 |
  | Shift x −3 / +3 mm | 0.6157 / 0.6124 | — |
  | Shift y −3 / +3 mm | 0.6288 / 0.6444 | — |
  | Shift z −3 / +3 mm | 0.6262 / 0.6253 | — |

  Gate (b) asserts that every cortical vertex of both hemispheres is
  `Converged`: none is `NonConvergent`, `OutsideSupport` or `NonFinite`. It then
  checks the residuals: median 3.0e-7 mm, max 1.0e-6 mm.
- **Caveat.** Per hemisphere, the bridged-minus-raw gain is 0.038 for L but only
  0.027 for R, below the 0.03 margin. The margin was declared for the combined
  cortical population, which is how the plan measured it, and the receipt is
  therefore scored over both hemispheres. A per-hemisphere gate would fail
  on R.
- **Mutation checks.** Each of the following fails its tests:
  - reversing stage application order: the synthetic oracle, the analytic stage
    order test and the real evidence all fail;
  - replacing the border rule with `0 ≤ ci ≤ n−1`: the border, synthetic oracle
    and bridge tests fail;
  - swapping forward and inverse use in route placement: the bridge tests and the
    real evidence fail.

## 6. Real-data qualification (WS5), 2026-09-23

Each budget frozen in §4 is evaluated as written. Failures are reported as
failures.

### Setup

- **Inputs.** The PLSNeuro beta and FIR exports, `MNI152NLin2009cAsym` res-2,
  97×115×97, affine `diag(2,2,2)` with origin (−96.5, −132.5, −78.5): 3 beta
  and 12 FIR brain-direction volumes. Each is a `DeclaredVolume` with basis
  `Derived("plsneuro task-PLS brain direction export
  (tools/fslr-qualification/pls_bundle_to_nifti.py)", inputs = [bundle
  DataAsset, export-script DataAsset])`. The digests are:
  - beta bundle `64b5bc0e…`;
  - FIR bundle `c3c8576c…`;
  - export script `e6bbbb7e…`;
  - per-volume digests from `export.json`, verified on read.
- **Release (asserted).** The PLS provenance records no TemplateFlow release.
  The runner **asserts** the source frame
  `MNI152NLin2009cAsym@templateflow@d79aacb1…`, the point map's catalog
  revision, so that the frames are exact.
- **Route.**
  - fsLR 32k L and R midthickness `DeclaredSurface`s in `MNI152NLin6Asym`,
    with the WS2 statement;
  - `FrameBridge.displacement` with `Inverse(1e-6 mm, ≤ 50 iterations)` on
    the locked point map (manifest `34bdcea2…`, source `2e3869a0…`);
  - `MidthicknessNearest`, `Continuous`;
  - no support mask declared: the exports are NaN outside the PLS selection,
    so unsupported voxels are nonfinite.
- **Runner and artifacts.** `FslrQualification` (surface JVM test scope, forked
  JVM 25.0.1, `-Xmx4g`) writes to `scratchpad/qualification/final/{beta,fir}`:
  `.npy` arrays (receipts and chosen voxels for every volume) and
  `results.json`. The artifacts, all under `scratchpad/qualification/`, are:

  | Artifact | SHA-256 | Produced by |
  |---|---|---|
  | `final/beta/results.json` | `dbcf0f16…` | `FslrQualification` |
  | `final/fir/results.json` | `0e783dac…` | `FslrQualification` |
  | `heap-probe.json` | `19c897e6…` | `--probe-heap`, `-XX:+UseSerialGC -Xmn16m` |
  | `js-timing.json` | `6cf74c52…` | `RouteTimingJsSuite`; FullOpt selected with `set surfaceJS/Test/scalaJSStage := FullOpt`, confirmed by `LinkingInfo.productionMode = true` |
  | `final/beta-summary.json` | `51da32da…` | the comparison script |
  | `final/fir-summary.json` | `8c3b7c6f…` | the comparison script |

  The large data is not committed.
- **Comparison.** `tools/fslr-qualification/compare_fslr_qualification.py`,
  run with `--export`, `--assets`, `--heap` and `--js`, against the SimpleITK
  oracle `independent_fslr_mapping.py` (`oracle/beta.npz`, `oracle/fir.npz`).
  It checks the following independently:
  - medial wall against the `desc-nomedialwall` labels, vertex by vertex;
  - ties from both ScalaFIM's and the oracle's positions, on the `export.json`
    grid;
  - receipts, voxels and picks for every volume;
  - picks against the oracle's value and voxel;
  - JVM timing on median and max.

  It exits non-zero because the NoSupport rate fails.

### Shared inputs and shared contract (common mode)

- **Same export.** ScalaFIM and the oracle read the same exported NIfTI files.
  The export's linear order was inferred heuristically (x-fastest: in-mask
  fraction F = 0.976 vs C = 0.835, `export.json`). A wrong order in the export
  would affect both sides identically and would not be detected here.
- **Same inverse scheme.** The value oracle is SimpleITK forward arithmetic
  under the same fixed-point inverse scheme; ITK has no inverse for
  `DisplacementFieldTransform`.
- **Same contract.** The oracle re-implements the same declared lookup
  contract: ties round up; support is `[−0.5, dim−0.5)`; nonfinite means
  NoSupport.
- **What agreement shows.** Agreement validates the arithmetic, the placement
  and the contract's implementation. It does not validate those choices
  themselves.

### Results (beta and FIR identical in every count)

| Budget | Measured | Verdict |
|---|---|---|
| Values: identical to an independent implementation, \|Δ\| ≤ 1e-9·max(1,\|v\|) over cortical vertices | Max relative \|Δ\| = 0.0 over all 15 volumes. Coverage is identical to the oracle at every vertex (L 26 381 / 2 796 / 3 315, R 25 718 / 2 776 / 3 998 Mapped / MedialWall / NoSupport) and identical across volumes. The chosen voxel equals the oracle voxel at every cortical vertex of every volume. Placed positions differ from the oracle's by ≤ 1.9e-4 mm (L) and 4.0e-6 mm (R), because of the oracle's 12-iteration residuals. | **Not met as written.** The budget names Connectome Workbench `-volume-to-surface-mapping -enclosing`, which was not run. Against the SimpleITK oracle: exact. |
| Values: disagreements only at ties, counted, ≤ 0.1 % | 0 tie vertices from ScalaFIM's positions and 0 from the oracle's; 0 disagreements | **Pass** |
| MedialWall equals the admitted mask's medial count | Vertex-exact against the labels: L 2 796, R 2 776 (oracle equal) | **Pass** |
| NoSupport ≤ 3 % of cortical vertices, for a whole-brain analysis support | L 3 315 / 29 696 = 11.16 %; R 3 998 / 29 716 = 13.45 %. The support is the PLS selection, not whole-brain: 206 070 finite voxels. | **Fail.** The premise (whole-brain support) does not hold, and the rate exceeds 3 %. The budget is not relaxed. |
| Every NoSupport vertex explained by a receipt | Every NoSupport vertex of every volume has an `inspect` receipt in {OutsideGrid, OutsideSupport, NonFinite}. All are `NonFinite`: outside the PLS selection. | **Pass** |
| Display identity (inflated vs very-inflated) | `onDisplay` onto `tpl-fsLR` inflated (`1672da09…` L, `8237f4e7…` R) and very-inflated (`8639333c…` L, `57d574b8…` R) returns the same object (`eq`) | **Pass** |
| Picks: 20 per hemisphere; receipt voxel = oracle voxel, linked to the source | 20 evenly spaced cortical vertices per hemisphere × every volume: beta 60/60 and FIR 240/240 per hemisphere. Each receipt voxel equals the oracle voxel. Each `Included` value equals the mapped value and the oracle value. NoSupport picks agree with the oracle's coverage. | **Pass** |
| JVM ≤ 500 ms warm per hemisphere and volume | 3 warm-up runs then 7 timed. Prepare + map median ≤ 50.2 ms and max ≤ 128.8 ms over all 15 volumes × 2 hemispheres. Placement at admission is once per route: warm median 17.6–30.4 ms. | **Pass** (median and max) |
| Scala.js FullOpt ≤ 2 s | Synthetic run of the same size (`js-timing.json`): prepare + map median 183.8 ms, max 504.8 ms over 7 runs; admission + placement median 52.5 ms. The suite asserts max ≤ 2 s and production mode. An earlier run measured 171.1 / 258.5 ms. | **Met with qualification**: synthetic, because no Scala.js NIfTI or point-map reader exists |
| ≤ 64 MB additional heap per mapped volume and hemisphere | Retained after `map`: 0.50 MiB. Live heap sampled at 1 119 forced full collections during 78 prepare + map runs: max 27.8 MiB, median 1.8 MiB above the post-setup baseline (220.2 MiB). Not bounds: heap-pool peak above baseline `poolPeakAboveBaselineMiB` = 236 (238 in the previous run); young-collection bound 175.2 MiB. Both include uncollected garbage. Transient allocation is 235 MiB per prepare + map. | **Met with qualification**: sampled live heap, not a proven bound |
| Scope: mapped only through an admitted route; group statistics unchanged | Every volume goes through `SurfaceRoute.admit` → `prepare` → `map`; the volumes are read-only | **Pass (by construction; not script-evaluated)** |

### Summary

- **Not met as written.**
  - Value oracle: a deviation. The frozen budget names Connectome Workbench;
    the comparison used SimpleITK, against which the route is exact.
  - NoSupport rate: a failure. It is 11.2 % (L) and 13.5 % (R) against 3 %,
    and the budget's whole-brain-support premise does not hold for the PLS
    selection. Every NoSupport vertex has a `NonFinite` receipt.
- **Met with qualification.**
  - Scala.js timing: synthetic, same size.
  - Heap: sampled live heap, not a proven bound.
- **Met.** Ties, medial wall, NoSupport receipts, display identity, picks, JVM
  time, and scope (by construction).
- **Asserted, not recorded.** The 2009c release `templateflow@d79aacb1…` is
  asserted by the runner, not recorded in the PLS provenance.
