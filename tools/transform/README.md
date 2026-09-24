# Transform oracle generators

These scripts produce the native-tool oracles that pin ScalaFIM's transform
conventions. The plan is `docs/plans/spatial-transform-parity.md` (Phase 4),
and the tooling decision is ADR §6.

## Layout

- Fixtures live in
  `modules/transform/shared/src/test/resources/scalafim/transform/oracle/<set>/`.
- Each set has a `manifest.json` recording:
  - the generator
  - tool versions
  - the exact commands run
  - the evidence *kind*
  - the SHA-256 of every file
- Shared tests read these files through `OracleFixtures`. The JVM reads them
  from the classpath and Scala.js reads them through Node `fs`/`zlib`, so
  both platforms verify identical bytes. `OracleProvenanceSuite` checks the
  hashes on both platforms.

## Evidence kinds

| kind | meaning | example |
|---|---|---|
| `native-oracle` | output of the reference tool itself | `flirt`, `antsApplyTransformsToPoints`, `mri_info` |
| `reference-implementation` | an independent implementation of the tool's documented convention | nibabel's `MGHHeader.get_vox2ras_tkr`, fslpy's FLIRT coordinate transforms |
| `cross-implementation` | consistency with another converter | nitransforms |

A packet whose acceptance calls for a native oracle does not close on the
weaker kinds. It records the gap as a caveat instead.

## Symmetry rules

A pure translation or an axis-aligned isotropic grid can hide axis-order and
sign errors, so generators must break symmetry:

- use oblique, anisotropic, non-square grids with shear
- test at least eight asymmetric points (`oracle_common.asymmetric_points`),
  including points off the grid centre and near every face

## Running

Python generators run through `uv`, with dependencies pinned per invocation:

```sh
uv run --with nibabel==5.3.2 --with numpy python tools/transform/generate_convention_oracle.py
```

Native-tool generators need Docker with pinned images. Record the image
digest in the manifest at generation time. Candidate tags:

| Tool | Image |
|---|---|
| FSL 6.0.x | `brainlife/fsl:6.0.4-patched2` or an equivalent pinned FSL 6 image |
| ANTs 2.6.x | `antsx/ants:v2.6.2` |
| AFNI | `afni/afni_make_build:AFNI_26.1.04` |
| FreeSurfer 7.x | `freesurfer/freesurfer:7.4.1`; needs a license file on the generating machine only, mounted read-only and never committed |

`neurotransform/` holds the generators of the imported neurotransform
oracles, for provenance.
