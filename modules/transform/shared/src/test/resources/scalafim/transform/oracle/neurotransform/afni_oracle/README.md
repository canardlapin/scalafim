# Pinned AFNI conversion oracle

Every file here was produced by AFNI itself, so they are an oracle independent of
this package. A round trip through our own reader and writer could hide a shared
sign or direction error; these files cannot.

**Producer:** `afni/afni_make_build:AFNI_26.1.04`
(`Precompiled binary linux_ubuntu_16_64_glw_local_shared: Jun  2 2026 (Version
AFNI_26.1.04 'Balbinus')`), generated 2026-09-17.

## The convention these files pin

From `3dmaskdump -help`:

> `-dbox x y z` … the coordinates are in RAI/DICOM order (+x=Left, +y=Posterior,
> +z=Superior).
> `-nbox x y z` … LPI/SPM or 'neuroscience' order where the signs of the x and y
> coordinates are reversed relative to RAI/DICOM. (+x=Right, +y=Anterior,
> +z=Superior)

So AFNI's axis letters name the *negative* end of each axis: "RAI" is numerically
LPS, and RAI ↔ RAS negates X and Y, never Z.

From `3dAllineate -help`, on `-1Dmatrix_save`:

> This matrix is the coordinate transformation from base to source DICOM
> coordinates. In other terms: `Xin = Xsource = M Xout = M Xbase`.

`3dvolreg -help` states the same base-to-input convention, and both append
`.aff12.1D` when the given name does not already end in `.1D`.

## Files

| File | What AFNI command produced it |
|---|---|
| `oracle.aff12.1D` | `cat_matvec -ONELINE 'MATRIX(1,0,0,7,0,0,-1,-3,0,1,0,5)'` — a 90° rotation about the DICOM x axis plus an integer shift, written by AFNI in its own format |
| `oracle_inverse.aff12.1D` | `cat_matvec -ONELINE oracle.aff12.1D -I` — AFNI's own inversion of that matrix |
| `landmarks_source_dicom.txt` | The five landmark positions placed in the source volume, in RAI/DICOM mm, with their voxel values |
| `landmarks_base_dicom.txt` | `3dAllineate -1Dmatrix_apply oracle.aff12.1D -final NN` resampled those landmarks onto the base grid; `3dmaskdump -xyz` then reported where they landed (`i j k x y z value`, xyz in RAI/DICOM mm). **This is an image oracle**: it proves the direction of the stored matrix, not merely its arithmetic |
| `vecwarp_source_dicom.1D` | `Vecwarp -matvec oracle.aff12.1D -forward` applied to the base landmark coordinates — AFNI's own point mapping |
| `volreg_series.aff12.1D` | `3dvolreg -base 0 -1Dmatrix_save` on a synthetic four-volume series: four rows plus AFNI's real header comment, including negative zeros |

Grids are 1 mm isotropic with voxel centres on integer millimetres, and the
oracle transform maps voxel centres exactly onto voxel centres, so the landmark
correspondence is exact rather than interpolation-limited.

The generator lives in this package's tests; see
`tests/testthat/test_afni_oracle.R` for how each file is consumed.
