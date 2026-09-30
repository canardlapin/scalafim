#!/usr/bin/env python3
"""FSL Jacobian-determinant oracle (STP P6.03): a cropped block of a real FNIRT registration and FSL's own
`fnirtfileutils --jac` output on the same lattice.

Evidence kind: native-oracle. Every value is FSL 5.0.9 output (brainlife/fsl@sha256:fbd262c3..., the image used for
the vendored neurotransform FSL oracles), produced by neurotransform's inst/extdata/fsl/register_to_mni.sh
(ss_sub-1001_T1w -> MNI152_T1_2mm_brain: flirt, fnirt, convertwarp --absout, fnirtfileutils --jac --withaff). Those
full-size outputs are generated locally and not committed upstream (about 140 MB); this script crops them losslessly:

  field_abs.nii.gz     the convertwarp absolute field (source FSL scaled-voxel mm) on a BLOCK^3 sub-lattice of the
                       2 mm MNI reference; the sform/qform translation is shifted so every voxel keeps its world position
  jac.nii.gz           fnirtfileutils --jac on the same sub-lattice
  support.nii.gz       uint8, 1 where FSL's applywarp output is non-zero (inside the warped brain)
  source_header.nii.gz the source volume's header (dims, pixdim, sform/qform) with all-zero uint8 data; FSL's
                       scaled-voxel convention depends on the source dims and handedness, not on its intensities

Run (needs the locally generated neurotransform FSL outputs):
    uv run --with nibabel==5.3.2 --with numpy python tools/transform/generate_fsl_jacobian_oracle.py \
        [--neurotransform ~/code/neurotransform]
"""

from __future__ import annotations

import argparse
import os
import sys

import nibabel as nib
import numpy as np

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

BLOCK = 20

parser = argparse.ArgumentParser()
parser.add_argument(
    "--neurotransform", default=os.path.expanduser("~/code/neurotransform")
)
args = parser.parse_args()
extdata = os.path.join(args.neurotransform, "inst", "extdata")
inputs = {
    "field_abs": os.path.join(extdata, "fsl", "highres2standard_warp_abs.nii.gz"),
    "jac": os.path.join(extdata, "fsl", "highres2standard_jac.nii.gz"),
    "applywarp": os.path.join(extdata, "fsl", "highres_in_mni_applywarp.nii.gz"),
    "source": os.path.join(extdata, "afni", "ss_sub-1001_T1w.nii.gz"),
}
for path in inputs.values():
    if not os.path.exists(path):
        sys.exit(
            f"missing {path}: run neurotransform's inst/extdata/fsl/register_to_mni.sh first"
        )

field = nib.load(inputs["field_abs"])
jac = nib.load(inputs["jac"])
applied = nib.load(inputs["applywarp"])
source = nib.load(inputs["source"])
jac_data = np.asarray(jac.dataobj, dtype=np.float32)
support = np.asarray(applied.dataobj) != 0

# The BLOCK^3 window fully inside the warped brain with the widest spread of FSL determinants: the comparison then
# covers both compression and expansion. Deterministic: ties resolve to the lowest corner in C order.
best, corner = -1.0, None
nx, ny, nz = jac_data.shape
for i in range(0, nx - BLOCK + 1, 2):
    for j in range(0, ny - BLOCK + 1, 2):
        for k in range(0, nz - BLOCK + 1, 2):
            if not support[i : i + BLOCK, j : j + BLOCK, k : k + BLOCK].all():
                continue
            spread = float(
                np.std(jac_data[i : i + BLOCK, j : j + BLOCK, k : k + BLOCK])
            )
            if spread > best:
                best, corner = spread, (i, j, k)
if corner is None:
    sys.exit("no fully supported block")
i0, j0, k0 = corner
window = (slice(i0, i0 + BLOCK), slice(j0, j0 + BLOCK), slice(k0, k0 + BLOCK))


def shifted(image):
    affine = image.affine.copy()
    affine[:3, 3] = (
        image.affine[:3, :3] @ np.array([i0, j0, k0], dtype=float) + image.affine[:3, 3]
    )
    return affine


def save(data, like, name, dtype):
    affine = shifted(like)
    header = like.header.copy()
    header.set_data_dtype(dtype)
    out = nib.Nifti1Image(np.asarray(data, dtype=dtype), affine, header)
    out.set_sform(affine, int(like.header["sform_code"]))
    out.set_qform(affine, int(like.header["qform_code"]))
    out.header["pixdim"][1:4] = like.header["pixdim"][1:4]
    nib.save(out, os.path.join(OUT, name))


OUT = oc.oracle_dir("fsl_jacobian")
save(
    np.asarray(field.dataobj, dtype=np.float32)[window + (slice(None),)],
    field,
    "field_abs.nii.gz",
    np.float32,
)
save(jac_data[window], jac, "jac.nii.gz", np.float32)
save(support[window].astype(np.uint8), applied, "support.nii.gz", np.uint8)

source_header = source.header.copy()
source_header.set_data_dtype(np.uint8)
blank = nib.Nifti1Image(
    np.zeros(source.shape, dtype=np.uint8), source.affine, source_header
)
blank.set_sform(source.get_sform(), int(source.header["sform_code"]))
blank.set_qform(source.get_qform(), int(source.header["qform_code"]))
blank.header["pixdim"][1:4] = source.header["pixdim"][1:4]
nib.save(blank, os.path.join(OUT, "source_header.nii.gz"))

oc.write_manifest(
    OUT,
    generator=__file__,
    kind="native-oracle",
    tools={
        "FSL": "5.0.9 (brainlife/fsl@sha256:fbd262c385e9de22aa58bf7b6311cbd5cd96c7b4eaff151f879191e869bf224e)",
        "nibabel": nib.__version__,
        "numpy": np.__version__,
    },
    commands=[
        "neurotransform inst/extdata/fsl/register_to_mni.sh (flirt; fnirt --cout; convertwarp --absout; "
        "fnirtfileutils --jac --withaff; applywarp)",
        "uv run --with nibabel==5.3.2 --with numpy python tools/transform/generate_fsl_jacobian_oracle.py",
    ],
    notes=(
        f"Crop corner (voxel) {list(corner)} of the 91x109x91 MNI152 2 mm reference, block {BLOCK}^3, chosen as the "
        f"fully brain-supported window with the largest FSL determinant spread (std {best:.4f}). Values are copied, "
        "not recomputed. Full-size input sha256: "
        + ", ".join(f"{key} {oc.sha256_file(path)}" for key, path in inputs.items())
        + ". FSL differentiates the B-spline coefficient field analytically; a dense-field finite difference is "
        "expected to agree to about 1% (neurotransform measured median 0.36%, p99 2.3% over the whole brain). "
        "Pending: the same check against FSL 6 fnirtfileutils --jac, with the FSL 6 native set (STP P4.04)."
    ),
)
print("corner", corner, "spread", best)
