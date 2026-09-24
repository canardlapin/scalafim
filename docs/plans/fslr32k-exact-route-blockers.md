# Exact MNI152NLin2009cAsym → fsLR 32k route: resolving the four blockers

Planning record: 2026-09-23. Ticket `bd-01M35BHNDCHM6YXCKYX0544TP3`. This plan
defines work and gates. It does not report a completed qualification.

## Blockers and decisions

| # | Blocker | Resolution | Owner |
|---|---|---|---|
| 1 | Local fsLR/warp assets are 0-byte placeholders | Acquire real assets through templateflow4s, pinned by content digest | templateflow4s |
| 2 | TemplateFlow fsLR has no white/pial | The qualified route is **midthickness point mapping**, which needs no pair. A template-native pair is an optional second route, not a prerequisite | ScalaFIM (route); cluster job (optional pair) |
| 3 | The frame of the fsLR anatomy is undeclared | Preserve GIFTI coordinate metadata as *non-exact*. Bind the exact frame (MNI152NLin6Asym) from provenance, then require empirical frame evidence | ScalaFIM + templateflow4s |
| 4 | There is no exact 6Asym → 2009c bridge | Add a typed **displacement-field bridge** built from TemplateFlow's own 2009c→6Asym transform, inverted per vertex, and qualified against an independent implementation (nitransforms) plus a GM-map direction control | templateflow4s (conversion), ScalaFIM (bridge) |

### Evidence already in hand (scratch check, 2026-09-23)

All assets were read from the TemplateFlow S3 archive; the independent warp
implementation was nitransforms 25.1.

- **No GM map for 6Asym.** TemplateFlow's MNI152NLin6Asym directory (130 files)
  has no GM/WM/CSF probability maps. MNI152NLin2009cAsym has them.
- **Single instrument.** The 2009c res-01 `label-GM` probability map, which is
  also the target frame, is therefore used to score every candidate placement of
  the fsLR 32k midthickness. The score covers the 59 412 cortical vertices, with
  the medial wall excluded.

  | Placement | mean GM p | p > 0.5 |
  |---|---|---|
  | Raw (hypothesis: already 2009c) | 0.670 | 0.743 |
  | TemplateFlow transform applied as a point map | 0.616 / 0.618 | 0.671 / 0.675 |
  | Same transform, numerically inverted (residual ≤ 0.003 mm) | **0.705 / 0.703** | **0.798 / 0.794** |

- **Frame conclusion.** The fsLR surfaces are in MNI152NLin6Asym, and moving them
  into 2009c measurably improves the anatomical fit. The GIFTI itself declares
  only `NIFTI_XFORM_TALAIRACH`.
- **Transform hazard.**
  - Both `tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym` and
    `tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym` behave as 2009c→6Asym *point*
    maps.
  - Their affine stages are nearly identical (scale ≈0.985 in both, not
    reciprocal).
  - Their displacement fields share the 2009c grid: 193×229×193, LPS origin
    (96, 132, −78).
  - Composing them does not return to the start: the round-trip median is
    2.99 mm, about twice the one-way displacement.
  - The second file therefore does not behave as its name implies for points.
  - The route uses the first file, which behaves consistently with its name, and
    inverts it per vertex.
- **Medial wall.** `desc-nomedialwall` marks 2 796 medial vertices on L and
  29 696 + 29 716 cortical vertices across L+R.

## WS0: Land the route layer on current `main` (prerequisite)

Port the `scalafim.surface.reference` package from
`surface/group-fslr-qualification-20260923` onto
`integration/main-catchup-20260923`, which uses the post-refactor image API
(`SomeScalarVolume`, `Affine[D3]`, grid ownership). The port covers:

- the per-vertex receipts;
- the NaN/Long-narrowing fix;
- the `scalarLayer` display-geometry fix.

**Gate:** all 18 route tests plus the kernel regressions pass on JVM, JS and
FullOpt JS. Both mutation checks are repeated.

## WS1: Acquisition (templateflow4s)

1. **Content digest.**
   - Compute SHA-256 while streaming.
   - Store it in `state/*.json`.
   - Return `AssetVerification.ChecksumVerified`.
   - `cache verify` re-hashes files.
2. **Asset lock.** A committed lock file lists `{archivePath, bytes, sha256, etag,
   catalogRevision}`.
   - `fetch --lock <file>` materialises exactly those files.
   - A digest mismatch is a typed failure, never a silent refresh.
3. **Provenance on `LocalAsset`.** Add the source URL, ETag, catalog
   `archiveRevision` and SHA-256.
4. **Entity selection.** Add `hemi`, `desc`, `den`, `from`, `to` and `mode`
   builders to `TemplateQuery`, with matching CLI flags.
5. **Large bodies.** Replace the whole-body request timeout with an idle/read
   timeout. Add a Range resume from `partial/`.
6. **Quarantine.** The catalog can mark an asset as quarantined with a reason.
   `tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym` is quarantined because it
   behaves as the same point direction as its opposite-named twin (evidence
   above). Filing this upstream with TemplateFlow is a separate, explicitly
   approved step.
7. **Scope of the ScalaFIM lock.** It covers:
   - the fsLR 32k L/R midthickness, inflated, veryinflated and sphere;
   - `desc-nomedialwall` for L and R;
   - `tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5`;
   - the 2009c res-01 `label-GM` probability map, for frame evidence.

**Boundary:** ScalaFIM does not depend on the templateflow4s client. It accepts a
digest-bound provenance record, `AssetProvenance(template, archivePath,
catalogRevision, sha256)`, from whoever acquired the file.

**Gate:** templateflow4s JVM/JS suites pass. A lock round-trip test covers
fetch → verify → tamper → typed failure.

## WS2: Frame declaration (ScalaFIM `surface`)

1. **GIFTI metadata.** `GiftiSurfaceCodec` retains `DataSpace`,
   `TransformedSpace`, `GeometricType` and `AnatomicalStructureSecondary` as a
   typed `GiftiCoordinateDeclaration`.
   - `NIFTI_XFORM_TALAIRACH` / `MNI_152` map to a *generic* declaration.
   - A generic declaration can never produce a `TemplateFrame`.
2. **`FrameDeclaration(frame, basis, asset: AssetProvenance)`.** The `basis` is
   one of:
   - `Literature(doi, statement)`, e.g. Conte69 surfaces registered to FSL
     MNI152 nonlinear 6th generation;
   - `Derived(recipe, inputs)`.

   `SamplingAnatomy` requires a declaration whose asset digest matches the loaded
   file.
3. **`FrameEvidence` receipt**, reproduced in a JVM qualification suite over the
   locked assets. It uses one GM probability map in the target frame, scoring
   cortical vertices under each candidate placement:
   - the declared frame, bridged, must beat raw placement by ≥ 0.03 in mean GM p;
   - it must beat the direction-reversed placement by ≥ 0.05;
   - every ±3 mm shift of the bridged placement must lower the score.

   This is a disconfirmation test, not proof. It is recorded alongside the
   declaration.

**Gate:** codec tests on real TemplateFlow GIFTI bytes (JVM) and on synthetic
fixtures (shared). The evidence suite passes on the locked assets.

## WS3: Displacement-field bridge

1. **Direction (resolved empirically).**
   - Use `tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5`. As an
     ITK composite it maps 2009c points to 6Asym points (affine + displacement
     field on the 2009c grid).
   - Vertices move 6Asym → 2009c through its **inverse**: solve T(y) = x by
     fixed-point iteration.
   - The per-vertex residual ‖T(y) − x‖ is recorded. Non-convergence, or a point
     outside the field, is a typed per-vertex refusal (`OutsideBridge` /
     `NonConvergent`). It is never extrapolated.
   - `tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym` is quarantined (see WS1)
     until upstream clarifies it.
2. **Conversion (templateflow4s, JVM).**
   - Read the ITK HDF5 composite with a pure-Java HDF5 reader, e.g. jHDF. Note
     the files spell the parameter datasets `TranformParameters` /
     `TranformFixedParameters`.
   - Emit the ordered stages: `AffineTransform` (12 parameters + centre) and
     `DisplacementFieldTransform` (size, origin, spacing, direction, 25.6 M
     values).
   - Convert LPS→RAS explicitly.
   - Output a canonical NIfTI vector field plus a JSON stage list, both
     digest-bound to the source `.h5`.
3. **`FrameBridge.Displacement` (ScalaFIM, shared).**
   - Apply the stages in ITK order, with trilinear displacement interpolation and
     inversion as above.
   - Endpoints, direction (forward or inverse use), source digest and
     convergence tolerance are part of the type. `ReversedBridge` and
     `BridgeMismatch` still apply.
4. **Gates.**
   - **(a) Independent oracle.** SimpleITK 2.5.6 (reference ITK semantics) on
     5 000 vertices plus the committed synthetic and real oracle fixtures
     (templateflow4s). Forward map and inverse solutions must agree to
     max |Δ| ≤ 1e‑6 mm.
     - The ITK semantics being matched are: composite = A(D(x)); trilinear
       displacement interpolation; direction parameters row-major; each axis
       inside when −0.5 ≤ c < n − 0.5, with neighbours clamped to the edge;
       zero displacement outside.
     - nitransforms 25.1 is only a loose cross-check (≤ 0.05 mm on vertices).
       It interpolates with cubic B-splines in float32 and has no half-voxel
       border. On fsLR vertices it differs from ITK by a median of 0.004 mm
       and at most 0.04 mm, which changes the nearest voxel at about 120
       vertices per hemisphere.
   - **(b) Inverse consistency.** Median residual ≤ 0.001 mm and max
     ≤ 0.01 mm, measured at 0.0000 / 0.0004 mm.
   - **(c) Anatomical, on the 2009c GM map.**
     - Warped cortical vertices must reach mean GM p ≥ 0.69 and a fraction
       p > 0.5 ≥ 0.78 (measured 0.703 / 0.794).
     - Raw must stay below that (0.670 / 0.743).
     - Applying the map forward instead of inverted must *fail* (0.618 / 0.675):
       a direction control.
   - **(d) Platforms.** JVM, JS and FullOpt JS pass on synthetic analytic fields
     (constant, linear and rotational displacements with closed-form inverses).

## WS4: White/pial (no longer blocking)

The qualified route is `MidthicknessNearest` on warped fsLR midthickness. That is
the appropriate operator for smooth group statistics on a 2 mm grid. Depth and
ribbon methods stay typed-refused for this route.

If a depth or ribbon route is wanted later, qualify it separately:

- **Recommended:** a *template-native* family. Run FreeSurfer `recon-all` on
  `tpl-MNI152NLin2009cAsym_res-01_T1w` on the cluster. Register it to fsLR 32k
  through TemplateFlow's `space-fsaverage` spheres, and resample white, pial and
  midthickness with barycentric weights.
  - This yields a same-frame route: no bridge is needed.
  - It is also an independent cross-check of WS3, via per-vertex distance
    between the two midthickness derivations.
- **Not recommended:** HCP S1200 group-average white/pial. It carries
  ConnectomeDB data-use terms, and averaging collapses cortical thickness.

## WS5: Real-data qualification

This runs only after WS0–WS3 pass. It uses the budgets frozen in
`docs/audits/group-volume-fslr-mapping.md`, with one declared amendment made
before any data is evaluated.

- **Amendment.** The independent implementation becomes a nibabel/NumPy
  nearest-voxel mapper applied to SimpleITK-warped vertices
  (`tools/fslr-qualification/independent_fslr_mapping.py`), because Connectome
  Workbench is not installed locally. On 2026-09-24 the maintainer decided that
  Workbench is not used at all: on already-placed coordinates it would only
  re-check the nearest-voxel lookup. The independent check of the warp is an
  ANTs point-transform run (recorded in the audit).
- **Inputs.** TemplateFlow-only volumes in MNI152NLin2009cAsym res-2 (the
  `label-GM` probseg with its `desc-brain` mask as declared support, plus a
  derived volume with a non-whole-brain support), given to the runner as a
  declaration spec. Qualification on a consumer's own data is the consumer's
  responsibility.
- **Checks:**
  - value identity;
  - coverage (medial wall exact; `NoSupport` budgets as amended post hoc on
    2026-09-24 in the audit);
  - inflated/veryinflated identity;
  - 20 picks per hemisphere;
  - the time/heap budgets.

## Order

1. WS0 ∥ WS1 (independent repositories).
2. Then WS2.
3. Then WS3, which needs the WS1 conversion and the WS0 bridge types.
4. Then WS5.
5. WS4's template-native pair can run on the cluster in parallel with WS3, as an
   optional second route and cross-check.
