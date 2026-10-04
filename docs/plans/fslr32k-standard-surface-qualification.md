# Standard cortical surfaces: fsLR 32k qualification

Planning record: 2026-09-23. Discovery tags: `standard-surface-qa`, `fslr-32k`.
This document defines work and acceptance gates; it does not report a completed
asset acquisition, visual comparison, projection qualification or default change.

## Outcome and decision

Establish a reproducible population-template surface for group-result inspection,
starting with TemplateFlow fsLR 32k. Compare original `inflated` and
`veryinflated` before considering additional smoothing. Deliver both a reviewable
visual decision and, separately, a qualified route for displaying real volumetric
results on the selected surface.

`VeryInflated` is the leading hypothesis, not an accepted default. The comparison
may select inflated, veryinflated, or neither. Keep the original unwarped
ds000001 sub-01 family as an identified regression fixture. Its individual anatomy
does not set the group-display quality target.

The earlier smoothing court also used a previously rejected *warped* inflated
fixture. Preserve that evidence, but supersede its appearance/default conclusions
and prevent its accidental reuse as a baseline. This is a second confound beyond
the choice of a single subject. PLSNeuro records the rejection and original-coordinate
replacement in `docs/verification/inflated-surface-family.md`. Existing mesh4s
algorithm tests and the visual suitability of their input are separate claims.

## Ownership

| Repository/module | Responsibility | Deliverable |
| --- | --- | --- |
| TemplateFlow4s catalog/client | Exact discovery, acquisition, cache, fingerprints and source provenance | Reproducible asset manifest and acquisition/cache evidence |
| ScalaFIM surface and surface-view | Decode/admit family correspondence, masks and display identities; compile/render geometry | Typed admitted family and reproducible comparison |
| ScalaFIM surface/image/spatial | Execute and qualify general coordinate transforms and volume sampling | Exact-space mapping capability and numerical/coverage receipts |
| ScalaFIM atlas | Standard-space descriptors and optional parcellation labels/provenance | Labels attached to an already identified vertex axis |
| PLSNeuro | Select a qualified family/route, compose the UI and preserve inspection provenance | Automatic group inspection, picking, persistence and export |

PLSNeuro already delegates projection to ScalaFIM. Reuse and qualify those public
APIs; do not build another mapper. Atlas does not distribute this geometry or
execute projection. Generic mesh algorithm defects belong in mesh4s only after a
minimal public-API reproduction identifies one.

## Scientific invariants

- Anatomical coordinates and display coordinates have different roles.
  Midthickness is a candidate sampling geometry; inflated/veryinflated are
  corresponding display realizations. Never apply anatomical nonlinear warps to
  the inflated display to make it appear registered.
- fsLR names a surface reference. Neither that name nor an MNI label establishes
  correspondence to a particular volume. Record the actual anatomical frame,
  template variant/version, units, voxel affine/grid and any transform chain.
- Midthickness plus a medial-wall mask does **not** by itself qualify projection.
  Declare point interpolation or another explicit sampling rule. True ribbon
  mapping additionally needs corresponding inner/outer anatomy and defined voxel
  weights; do not call averaged depth samples ribbon-constrained mapping.
- Preserve hemisphere and ordered vertex identity. Equal vertex counts, and even
  matching connectivity alone, are insufficient proof of anatomical correspondence.
  Require source provenance and explicit correspondence checks.
- A display switch preserves scalar values, coverage, vertex IDs, sampling
  coordinates and projection receipts. Medial wall, unavailable data and valid
  zero remain distinct. No vertex renumbering to implement the wall mask.
- Projected beta/FIR/statistic fields remain derived displays of the original
  volume results. This work does not admit surface-native inference, invent a
  cortical p-value, smooth statistical values, or change the fitted result.

## Execution and gates

### 1. Resolve and fingerprint the exact family

Acquire six geometries and two masks through TemplateFlow4s:

- `tpl-fsLR_den-32k_hemi-{L,R}_{midthickness,inflated,veryinflated}.surf.gii`
- `tpl-fsLR_hemi-{L,R}_den-32k_desc-nomedialwall_dparc.label.gii`

These paths are present in the local pinned catalog and the upstream
[fsLR asset repository](https://github.com/templateflow/tpl-fsLR). The inspected
catalog records archive `d79aacb1ad7d1c52e5d10ad88f48fd8af6e5ae56` and fsLR
subdataset `ca545b4721c2858decef9bbca302c1eac4d0d8bf`; execution must record the
actual selected revisions rather than silently refreshing them.

The manifest records unique queries, archive paths, source URLs, library/catalog
revisions, byte counts, payload SHA-256, cache receipts, metadata and citation/license
terms. Distinguish a locally observed digest from a digest verified against a
trusted upstream identifier. Trace the actual average-geometry construction and
coordinate-frame provenance; leave unknowns explicit. Catalog identity alone
does not qualify geometry or pin bytes returned by a mutable endpoint.

**Exit:** reproducible cold acquisition and warm offline reuse, with explicit
missing/corrupt/interrupted states. Retain payloads in managed external storage.
Existing APIs may suffice: an executable recipe and receipts can satisfy this
ticket without adding a new acquisition abstraction.

### 2. Admit corresponding geometry and mask identities

Use the existing GIFTI and surface-resource APIs. Verify 32,492 vertices per
hemisphere, triangle validity/winding, finite coordinates, source coordinate
metadata, ordered topology and family correspondence. Retain fingerprints for
each realization and the shared vertex axis. Fail visibly on mismatched inputs;
do not silently repair or smooth the source.

Represent `VeryInflated` explicitly in the display-kind model and its
serialization. The inspected primary `SurfaceKind` only has `Inflated` today.
Reconcile the actual consumer pin and existing mesh4s migration before editing;
reuse their contracts instead of inventing parallel resource identities.

Read the medial-wall label table to establish polarity and record included/
excluded counts. Declare the boundary-face policy, including faces with mixed
included/excluded vertices. Keep source topology and use a render mask; sampling,
overlay visibility and picking must honor the corresponding semantics.

**Exit:** admitted bilateral family plus JVM/JS evidence for identity preservation,
kind roundtrip, mask semantics and refusal of permuted, wrong-hemisphere,
wrong-density or mixed-family inputs. Volume-space qualification may still be
unavailable; that must not prevent a geometry-only comparison.

### 3. Run the visual comparison

Begin with plain anatomy: original coordinates and geometry-derived, unfiltered
vertex normals. Record the normal-generation method. No additional coordinate
or normal smoothing in this baseline.

The primary matrix has **24 views**: two display forms × two hemispheres ×
lateral/medial views × three fixed lights. Reuse the previous current,
anterior/posterior-mirrored and superior lights, recording their vectors and
coordinate convention. Fix material, orthographic projection, background and
one union-derived framing scale per matched view pair. Any separate fit-to-pane
plate is labeled as a usability comparison, not the controlled shape comparison.

Add a second set of controls with identical scalar arrays across both forms:
asymmetric landmarks, a smooth signed gradient, small high-contrast patches,
valid zero and explicit missingness. Generate them once from the common vertex
axis/anatomical realization, not separately from each inflated shape. Use the
same color range, thresholds and opacity. Include unlit controls to expose color
or interpolation artifacts. Apply the admitted medial-wall policy throughout.

Start without a sulcal underlay. The pinned fsLR family does not supply a complete
curvature/white/pial family; a `desc-vaavg_midthickness.shape.gii` vertex-area field
is not curvature. Any later sulcal background needs its own matching source and
scalar identity. Existing PLSNeuro curvature acceptance remains a later requirement.

Produce actual 1x and 2x renders, posterior and frontal closeups, input hashes,
scene/renderer/provider receipts, timings and the exact reproduction command.
Inspect the deterministic raster first. Compare the same assets in an independent
viewer where available, then qualify the same scene on the claimed native backend
before selecting an application default. Differences in lighting across viewers
must be documented; exact cross-renderer pixel equality is not an oracle.

**Exit:** recorded review of posterior roughness, pits/seams, recognizable cortical
landmarks, silhouette, overlay legibility, bilateral consistency, wall boundary
and pick identity; a user verdict chooses a form or rejects both. Record time and
memory on declared hardware. 32k is a reasonable candidate density, not a prior
performance pass. Roughness metrics supplement inspection, not anatomical truth
or human preference.

If neither form looks acceptable, diagnose source geometry, normal generation,
lighting and renderer behavior separately. Open/reuse a provider defect only where
the evidence points. Additional smoothing is a bounded follow-up on the admitted
template family, with the unmodified baseline retained for comparison.

### 4. Qualify the real volume-to-surface route

This proceeds alongside preparation of the visual court under the existing
ScalaFIM mapping ticket. It is already being worked on; do not duplicate its
reference types or numerical audit.

Audit `VolumeSurfaceSamplingPlan`, `VolumeSurfaceSampler`, sampling inspection
receipts and `SurfaceVolumeProjection` at the actual consumer revision. Select
one exact, useful group-volume reference first. Establish the anatomical surface
frame and qualified transform before selecting sampling/interpolation policy.
No arbitrary MNI alias or assumption that the fsLR mesh is in the volume frame.

Use independent asymmetric ramps, impulses and known transforms, including
oblique/anisotropic/shifted grids, coverage edges, wrong references, masks and
nonfinite inputs. Check contribution weights and pick footprints. Freeze
tolerances and coverage budgets before evaluating real fixtures. An independently
configured Workbench route can be a reference where its method actually matches;
its [mapping documentation](https://www.humanconnectome.org/software/workbench-command/-volume-to-surface-mapping)
distinguishes point interpolation from ribbon-constrained mapping.

**Exit:** one independently qualified, precisely named mapping route with explicit
refusals and reconstructible receipts. Switching inflated/veryinflated leaves
mapped values and coverage unchanged. Missing transform/anatomical assets block
that route, while the visual court remains useful. Add other spaces separately.

### 5. Integrate and accept the group-inspection workflow

PLSNeuro consumes exact qualified provider revisions and resolves the family
without manual file hunting. Expose loading, offline-cache, missing-resource and
unsupported-mapping states; persist family hashes, route, chosen display form,
mask, camera and source result identity through reopen/export.

Run real beta and FIR fixtures only after gate 4. Verify stable value/color meaning
across displays, physical FIR time, support/coverage, mapped pick footprints and
linked volume locations. Review native lateral/medial images at 1x/2x and realistic
pane sizes. Measure existing latency/memory budgets and asynchronous cancellation
and stale-result behavior. Reuse current surface-switching/lighting/V4 tickets;
this plan does not close their broader acceptance requirements.

**Exit:** source-bound end-to-end evidence, required provider/app gates and human
visual acceptance. Only then adopt a group-display default. Keep the old
single-subject fixture available by explicit identity for regression.

## Work records and dependencies

| Step | Repository | Mote | Status at planning |
| --- | --- | --- | --- |
| Exact assets | TemplateFlow4s | `bd-01M37DW3QJJAEQA351SCTXJPX1` | New P1 |
| Family admission | ScalaFIM | `bd-01M37DV2BCSYH88EWF5M835JC5` | New P1 |
| Visual comparison and this plan | ScalaFIM | `bd-01M37DVYQ9J6NWE1WEN2SR41CN` | New P1; depends on family admission |
| Exact-space mapping | ScalaFIM | `bd-01M35BHNDCHM6YXCKYX0544TP3` | Existing P1, doing; holder `claude-group-fslr` |
| Consumer integration | PLSNeuro | `bd-01M2421N7AZQX1AK5BEMVSB84K` | Existing P1, open |
| Optional smoothing follow-up | ScalaFIM | `bd-01M34K30H3PZ1PDZRHZH7HK7YG` | Existing P2, review; no default adopted |

Execution order: asset receipt → admitted family → visual verdict. Mapping audit
can start independently, but qualification of the actual route needs its exact
assets. Consumer completion requires both the visual and mapping gates.
Smoothing is not a prerequisite for either branch.

Only the ScalaFIM visual→admission dependency is a native blocking edge. Foreign
repository IDs are explicitly linked prerequisites in issue bodies/notes, not
cross-store dependency edges. Consequently `mote ready` alone is not proof that
external acceptance prerequisites are met. Preserve existing ticket owners and
their broader acceptance; do not duplicate a future epic or mapping ticket.

Find this work in any of the three repository roots:

```sh
mote ls --tag standard-surface-qa
mote ls --tag standard-surface-qa --tag fslr-32k --all
```

From elsewhere, use `mote --store /absolute/repository/.mote ls --tag standard-surface-qa`.

## Implementation checks and handoff

At execution, reserve exact paths in an isolated worktree as needed and record
the complete provider closure. Existing dirty primary checkouts and unlanded
migration candidates are not interchangeable baselines.

TemplateFlow4s changes require `sbt -batch compileAll testAll` and any applicable
optional-module gates. ScalaFIM changes require relevant JVM **and** JS suites;
for example, run `sbt surfaceJVM/test surfaceViewJVM/test` and
`sbt surfaceJS/test surfaceViewJS/test` as separate bounded invocations, adding
the affected raster/examples/spatial suites. Run `sbt scalafimCompileAll` for the
warning-clean compile gate. Do not run the entire ScalaFIM test aggregate in one
process. New probe entry points and commands must be recorded when implemented;
the present plan does not invent a runnable fsLR probe.

PLSNeuro integration requires its `sbt check`, the documented headless acceptance
lane and touched native lanes through `tools/run-acceptance.py`. Native/platform
and human review remain distinct from headless passes. Preserve failed attempts
and record exact commands, versions and artifact locations.

The next deliverable is the pinned eight-asset receipt and admitted-family
comparison, while the existing mapping owner continues the independent audit.
This planning change adds no runtime code, changes no defaults, runs no scientific
tests, and grants no commit, merge or publication authority.
