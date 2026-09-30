# STP completion evidence, 2026-09-29

This local, unpublished integration candidate is based on ScalaFIM
`8d0dd730b60a40b093fae0f7ff61996fa071a811`. The shared checkout contains
concurrent work. `integration.json` records actual commands, exit status and
raw-log hashes; `source-manifest.json` binds the candidate files and identifies
any differences from the shared checkout. This is not full epic qualification.

## Repairs

- Public NIfTI readers, dataset/workflow ingest and motion IO require explicit
  world evidence. Unknown D3 constructions have separate persistent tokens;
  identical geometry does not establish a common world. Derived grids retain
  their source frame. Relabelling refuses incompatible units/conventions.
- Viewer cursor state and replay retain frames. Cross-world links require a
  `WorldLink`; the unchecked surface-to-volume helper is retired. Camera bounds
  use private display coordinates.
- FSL headers with conflicting active q/sform handedness are rejected. The
  compatibility policy is explicit; positively handed native applyxfm controls
  verify the admitted storage convention.
- Surface graph compilation uses frozen endpoint topology and cached CSR
  operators. Fractional ribbon weights, masks, ROI order and adjoints have
  independent regression checks. Admission validates endpoints without
  rebuilding the normalized operator.
- Native writer acceptance covers every analytically supported query and binds
  geometry, tool, input and output identities. A stale receipt is rejected.

## Native evidence

ANTs 2.6.5 development and the FSL6 command-package closure have immutable tool
identities, actual command logs and fixture SHA256 manifests under the transform
oracle resources. The user selected smaller temporary FSL command packages;
`fsl6-Dockerfile`, package lock and provenance record the exact environment.
Its package versions must not be described as one monolithic FSL release.

`native-writer-summary.json` records six passing formats: ANTs affine tfm/mat
and field, FSL FLIRT and field, and AFNI oblique affine. The predeclared tolerance
is 0.0002 mm. Affine checks cover 11,799 supported queries per format; field
checks cover 60. FreeSurfer's three native writer formats remain unrun because
the user has no local license.

`afni-native-probe` supports cardinalized oblique placement at the sampled
nodes. Native default off-node interpolation differs from trilinear decoding;
this does not authorize a new oblique-field admission rule.

## Numerical inverse candidate

`u6-native-candidate-factor4/qualification.json` records 2/2 JVM and 2/2 JS
passing tests against unpublished reframe4s tip
`842ec9a752d76793f936d7024681d50de89117db`. ANTs uses quarter-spacing on the
fixed interior crop, FSL half-spacing. The original 0.01 mm composition gates,
full coverage and zero-divergence requirements remain intact. All 962 ANTs and
423 FSL reverse probes are explicitly checked without outside-domain filtering,
plus 12 fixed crop-interior off-node holdouts per tool. Independent native inverse
decoding is checked separately at original lattice nodes and whole-field holdouts.

The ANTs estimated reverse maximum is 0.007324 mm. Its native forward/inverse
pair has a separate approximate closure maximum of 0.01793 mm; native inverse
agreement is not the same claim as estimate composition residuals. Rejected
coarser candidates and the original ordinary-pin failure are retained. These
sampled checks do not prove a continuous-domain bound or complete large-affine
registration qualification. Ordinary ScalaFIM still pins
`9a4508351d74567147b8ea3221d82db89e5892b0`.

## Owner-local overlap patch

`locus4s-overlap.patch` applies to locus4s base
`5a61003379483c19f2aab0e8ce1b7408397da299`. Its raw log records 3/3 JVM and
3/3 JS tests, including an independent exhaustive small-set oracle, same-owner
admission and overflow-safe counts. It passed `git apply --check` in a fresh
clone. No upstream publication, foreign tracker mutation or consumer pin change
was made.

## Acceptance limits

- FreeSurfer native qualification requires a license.
- Upstream inverse/overlap publication and ScalaFIM pin updates remain open.
- Real scenario clean-Pass acceptance remains open. The cached TemplateFlow
  bridge tests did run successfully; supplied synthetic surface topology tests
  do not establish the required real fsaverage-to-fsLR commutativity.
- Geometry-only persisted metadata can reconstruct fresh unknown worlds. It
  cannot establish cross-open identity without explicit stored world evidence.
  The retired shared `scalafim-ras-d3` key requires an explicit migration.
- `halfflow-geometry-request.md` is a prepared upstream request, not a submitted
  tracker change. Housekeeping and final independent epic review remain open.
- A frozen fit writer consumer test was migrated and verified; concurrent shared
  fit edits were preserved and are not qualified by that frozen test result.

The final compile and affected JVM/JS results are recorded in `integration.json`.
Earlier failures remain alongside their passing replacement checks.
