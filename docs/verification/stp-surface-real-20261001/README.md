# Real surface chain: bounded left-hemisphere qualification

This P7.07 slice samples the real ds002748 sub-01 `orig.mgz` through its matching
raw FreeSurfer white/pial surfaces, then resamples the registered vertex field
to fsaverage and fsLR32k. It also runs the single-interpolation direct route.
The portable contract covers one connected patch of 64 fsLR vertices selected
from source geometry before numerical outputs: nearest to direction `(0.2, -0.7, 0.68)`,
then sorted by original vertex ID. No query is omitted after comparison.

The native commands operate on complete subject/template surfaces. Portable
fixtures retain the exact contributing face closure: 167 subject vertices,
192 fsaverage vertices, original vertex/face IDs and ordered source faces.
These subsets are ordinary meshes, not a claimed complete stock template
domain. The volume crop is 10 x 14 x 17, starting at original voxel
`(139, 71, 74)`; every retained uint8 value and interpolation corner is exact.

## Source and coordinate admission

- Subject: `OpenNeuroDerivatives/ds002748-fmriprep`, commit
  `1d8407e467d1af0ac8933c0539bbbb0168badab6`, CC0. Public S3 payloads were matched
  to the pinned Git-annex size/digest and additional SHA256. The raw
  `lh.white`, `lh.pial` and `lh.sphere.reg` have identical ordered faces.
- fsaverage: TemplateFlow `tpl-fsaverage`, commit
  `8e53ba4f2e438758f69d11436fe0cd291a28bec6`, left 164k `desc-std` sphere.
- fsLR: TemplateFlow `tpl-fsLR`, commit
  `ca545b4721c2858decef9bbca302c1eac4d0d8bf`, left 32k `space-fsaverage` sphere.
- Both template coordinate arrays and ordered face arrays are bit-exact with
  the corresponding `resample_fsaverage` files in HCPpipelines commit
  `fb25ef4e9be44402d1d746834d18050842bb4042`. The fsLR target is already deformed
  into the fsaverage registration gauge. Thus the direct route interpolates
  subject `sphere.reg` onto that target once; the staged route inserts the
  fsaverage mesh. No extra sphere-coordinate composition is assumed.

The derivative GIFTIs were audited but are not used as scanner coordinates:
their nonidentity embedded transform targets Talairach space, and `smoothwm`
also differs from raw white. The raw surfaces use tkRAS; the matching real
MGH header defines `Norig * inverse(Torig)` explicitly. The public
`FreeSurferVolumeGeometry` model infers voxel sizes from Norig column norms.
Its inferred sizes differ from the actual MGH header delta by approximately
1.19e-7 mm because the stored direction cosines are float32. The scenario
checks exact model algebra at 1e-9 mm and actual-header placement separately
at the predeclared 5e-5 mm limit. This small distinction is retained, not hidden.

Template data are not relabelled CC0. The original HCP and FreeSurfer notices
are retained in the fixtures, along with provenance and the modified-data
notice. `NOTICE.md` includes the required prefatory notice and full terms:

> All or portions of this licensed product (such portions are the "Software") have been obtained under license from The General Hospital Corporation "MGH" and are subject to the following terms and conditions:

See the fixture `NOTICE.md` for those terms. The pinned TemplateFlow descriptions refer to a LICENSE which is not
in those repositories; the original-source notices are preserved explicitly.
No local FreeSurfer reconstruction, native FreeSurfer command or license file
was required.

## Comparisons and limits

The public `RibbonOperator` uses seven equally spaced samples on each
white-to-pial segment, with trilinear volume interpolation. Its native
reference averages seven Workbench `-volume-to-surface-mapping -trilinear`
outputs. Workbench's polyhedral `-ribbon-constrained` method is a different
estimator and is not represented by this contract.

Three native `-metric-resample BARYCENTRIC` commands produce the subject to
fsaverage, fsaverage to fsLR and direct subject to fsLR outputs. Workbench uses
closest points on radius-normalized triangles; the public ScalaFIM plan uses
radial intersections. The independent Python reference searches all full-source
faces, using a triangle-vertex matrix solve for radial intersections and
orthogonal face/edge minimization for closest points. This is independent of
the production spatial hash and Moller-Trumbore intersection code.

- Public radial ribbon/route mathematics must agree at 1e-9 intensity units.
  Selected public plan columns and weights must match the full-source face
  solution at 1e-10; nearest-vertex fallback is not accepted.
- Native closest-point outputs must agree with their own independent reference
  at the fixed 1e-3 intensity limit. This budget covers float32 surface points,
  interpolation weights, metrics and volume sampling; it does not replace the
  strict public double-precision guard. The largest native residual is
  0.000170491 intensity units, versus a source uint8 range of 0 through 255.
- Each public real route is also checked against its native output with an
  input-derived bound: half the union-support input intensity range times the
  radial/closest row-weight L1 difference, plus the maximum true-header/model
  sample difference on that support, plus the unchanged 1e-3 float32 limit.
  These bounds were written before native numerical commands. They explain
  the distinct estimators; they are not a universal Workbench parity claim.
- A separate linear field on the normalized registered sphere tests
  commutativity. If `p` is a radial triangle hit and `q` its sphere query, its
  interpolation error is bounded by `norm(gradient) * norm(p - q)`. Propagate
  first-stage distances with the nonnegative second-stage weights, then add
  the second-stage and direct-route distances. Every target has its own
  geometry-only bound. Maximum bound: 0.000192038; maximum observed analytic
  route gap: 0.0000374551. No real-field residual determines this limit.

The first attempt used the maximum face-plane sagitta over every source face.
That produced a vacuous 2.22365 analytic bound because unused thin triangles
dominated it. The final formulation uses the contributing radial-hit distances
above and was frozen before the final native rerun. The rejected attempt and
its original bound remain in the external evidence directory. Its exact generator
source is retained as `rejected-attempt1-generator.py`; SHA256
`d57caa0a82ad7410853aede8b22937c4f69cba0894317771ff150afab50c0b5b`
matches the original attempt manifest.

The real intensity staged/direct difference is descriptive: maximum 0.297963,
median 0.0601214, p95 0.247538. Two interpolations and one interpolation are
different estimators, so this is not asserted to be zero. The portable scenario
requires a clean `Pass`, rejects tkRAS-as-scanner, reversed placement, LPS grid
and source-vertex permutation mutations, and checks the frame mismatch at
compile time. Historical synthetic scenarios retain their declared caveats.

This slice does not qualify registration accuracy, all vertices/hemispheres,
arbitrary field commutativity, polyhedral ribbon overlap, template-volume
bridges or the compiled spatial graph. Those are separate contracts.

## Reproduction and evidence

Fixture root:
`modules/surface/shared/src/test/resources/scalafim/surface/oracle/real_chain/`.
`contract.json`, `input-derived-bounds.json`, `report.json`, `commands.json`
and `manifest.json` retain source identities, limits, successful command
receipts and fixture hashes. Workbench 2.2.1 reports commit
`01164ffa47f2778088bd6ff472ec9cd9a57f5b42`; its actual executable SHA256 is
`64c956c20640e48c3e9c148da7c0940c388b4162273613ba28319d673ec9e2a2`.
The official DMG SHA256 is
`a5a1372b581aa64b4667be854d884dbadb06ed44c4ecb7220a9ab4ad84c183bc`.
The session mounted it read-only, copied its bundle to temporary tooling and
detached its own mount. Ten native numerical commands and one version command
exited zero; version discovery alone is not the readiness evidence.

```sh
uv run --with numpy==2.5.3 --with nibabel==5.4.2 python \
  tools/transform/generate_surface_real_chain_oracle.py \
  --source-dir /path/to/hash-verified/admitted-inputs \
  --workbench /path/to/ConnectomeWorkbench.app/Contents/usr/bin/wb_command \
  --work-dir /new/native-evidence --output /new/portable-fixtures
sbt surfaceJVM/test
sbt surfaceJS/test
python3 tools/scenarios/validate_manifest.py docs/scenarios/manifest.json
sbt scalafimCompileAll
```

Existing output directories are rejected. Full originals and native outputs
remain outside Git in
`/private/tmp/scalafim-p707-surface-real-20261001/`, with successful qualified
outputs under `native-reviewed` and the rejected first bound under
`native-attempt1`. Portable checks do not require installed native tools.
The final isolated combined tree includes the approved concurrent main merge:
surface JVM 185/185, JS 150/150 and all-module compilation on both platforms
passed without warnings. `qualification.json` binds that exact tree to the
scientific source and raw log/exit metadata. Peer review independently reproduced
all 64 bounds; pending final delta evidence is kept distinct from approval.

The fixed native float32 limit dominates these small radial/closest estimator
differences. Native comparisons protect gross convention errors but do not
distinguish every projection algorithm. The strict independent radial
mathematics and contributor-weight gates carry the finer correctness claim.
