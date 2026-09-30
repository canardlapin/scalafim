#!/usr/bin/env python3
"""FreeSurfer linear-transform oracle (STP P3.06), written from FreeSurfer's own source semantics.

Semantics (FreeSurfer dev, utils/transform.cpp and utils/mri.cpp):
  * LTA src = movable, dst = reference. LINEAR_RAS_TO_RAS m_L maps src RAS -> dst RAS ("movsrc->refdst").
  * LINEAR_VOX_TO_VOX m_L = dst_ras2vox * m_L(ras) * src_vox2ras (LTAgetV2V); volume-geometry vox2ras is
    [Mdc*D | c_ras - Mdc*D*dims/2].
  * REGISTER_DAT (and register.dat files) hold the tkregister matrix R mapping reference tkRAS -> movable tkRAS,
    with Torig from MRIxfmCRS2XYZtkreg (fixed LIA axes, the volume's voxel sizes).
  * FSLREG_TYPE holds the FLIRT matrix from movable FSL coordinates to reference FSL coordinates.
  * talairach.xfm (MNI xfm) maps source RAS -> target RAS.

Evidence kind: self-consistency. The LTA, register.dat and xfm files are written by this generator from the formulas
above, so agreement shows only that ScalaFIM reads every encoding of one transform the way these formulas write it.
Two inputs come from reference implementations: Torig from nibabel's MGHHeader.get_vox2ras_tkr (MRIxfmCRS2XYZtkreg)
and the FSLREG matrix from fslpy toFlirt. nitransforms independently reads ras2ras.lta and vox2vox.lta in the
cross-implementation set (oracle/nitransforms_crosscheck). Native lta_convert / tkregister2 / mri_vol2vol outputs
remain pending (P4.02).
nitransforms is deliberately not used to write LTAs: nitransforms 25.1.0 writes the reference geometry into the LTA
`src` slot, which FreeSurfer reads as the movable volume.

Run:
    uv run --with nibabel==5.4.2 --with fslpy==3.29.1 --with numpy python tools/transform/generate_freesurfer_oracle.py
"""

from __future__ import annotations

import os
import sys

import numpy as np
import nibabel as nib
from nibabel.freesurfer.mghformat import MGHHeader
import fsl.version
from fsl.data.image import Image
from fsl.transform import flirt as fslflirt

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

OUT = oc.oracle_dir("freesurfer_linear")


def rotation(ax, ay, az):
    cx, sx, cy, sy, cz, sz = (
        np.cos(ax),
        np.sin(ax),
        np.cos(ay),
        np.sin(ay),
        np.cos(az),
        np.sin(az),
    )
    return (
        np.array([[cz, -sz, 0], [sz, cz, 0], [0, 0, 1]])
        @ np.array([[cy, 0, sy], [0, 1, 0], [-sy, 0, cy]])
        @ np.array([[1, 0, 0], [0, cx, -sx], [0, sx, cx]])
    )


def volume(shape, zooms, rot, origin, name):
    aff = np.eye(4)
    aff[:3, :3] = rot @ np.diag(zooms)
    aff[:3, 3] = origin
    img = nib.Nifti1Image(np.zeros(shape, dtype=np.float32), aff)
    img.header.set_sform(aff, code=1)
    img.header.set_qform(aff, code=1)
    path = os.path.join(OUT, name)
    nib.save(img, path)
    return path, nib.load(path)


def vox2ras(img):
    """FreeSurfer volume-geometry vox2ras, in float64 from the stored header."""
    return np.asarray(img.affine, dtype=float)


def tkreg(img):
    header = MGHHeader()
    header.set_data_shape(img.shape[:3])
    header.set_zooms(nib.affines.voxel_sizes(img.affine)[:3])
    return np.asarray(header.get_vox2ras_tkr(), dtype=float)


def volgeom_text(label, img, name):
    aff = vox2ras(img)
    sizes = nib.affines.voxel_sizes(aff)
    cosines = aff[:3, :3] / sizes
    cras = aff[:3, 3] + aff[:3, :3] @ (np.array(img.shape[:3]) / 2.0)
    fmt = lambda v: " ".join(repr(float(x)) for x in v)  # noqa: E731
    return [
        f"{label} volume info",
        "valid = 1  # volume info valid",
        f"filename = {name}",
        "volume = " + " ".join(str(int(n)) for n in img.shape[:3]),
        "voxelsize = " + fmt(sizes),
        "xras   = " + fmt(cosines[:, 0]),
        "yras   = " + fmt(cosines[:, 1]),
        "zras   = " + fmt(cosines[:, 2]),
        "cras   = " + fmt(cras),
    ]


def write_lta(name, lta_type, label, matrix, mov, ref):
    lines = [
        "# transform file written by tools/transform/generate_freesurfer_oracle.py",
        f"type      = {lta_type} # {label}",
        "nxforms   = 1",
        "mean      = 0.0000 0.0000 0.0000",
        "sigma     = 10000.0000",
        "1 4 4",
    ] + [" ".join(repr(float(v)) for v in row) for row in matrix]
    lines += volgeom_text("src", mov, "movable.nii") + volgeom_text(
        "dst", ref, "reference.nii"
    )
    lines += ["subject scalafim-oracle", "fscale 0.100000"]
    with open(os.path.join(OUT, name), "w") as f:
        f.write("\n".join(lines) + "\n")


mov_path, mov = volume(
    (40, 48, 36),
    (1.2, 0.9, 1.5),
    rotation(0.1, -0.05, 0.2),
    (-30.0, -45.5, 12.0),
    "movable.nii",
)
ref_path, ref = volume(
    (64, 56, 50),
    (1.0, 1.1, 0.95),
    rotation(-0.08, 0.12, -0.1),
    (-60.25, -70.0, -40.0),
    "reference.nii",
)

forward = np.eye(4)  # movable RAS -> reference RAS
forward[:3, :3] = rotation(0.04, -0.03, 0.06) @ np.array(
    [[1.03, 0.02, 0.0], [0.0, 0.97, 0.015], [0.0, 0.0, 1.01]]
)
forward[:3, 3] = (2.5, -3.75, 1.25)
pullback = np.linalg.inv(forward)  # reference RAS -> movable RAS

write_lta("ras2ras.lta", 1, "LINEAR_RAS_TO_RAS", forward, mov, ref)
write_lta(
    "vox2vox.lta",
    0,
    "LINEAR_VOX_TO_VOX",
    np.linalg.inv(vox2ras(ref)) @ forward @ vox2ras(mov),
    mov,
    ref,
)
R = (
    tkreg(mov)
    @ np.linalg.inv(vox2ras(mov))
    @ pullback
    @ vox2ras(ref)
    @ np.linalg.inv(tkreg(ref))
)
write_lta("registerdat.lta", 14, "REGISTER_DAT", R, mov, ref)
flirt = fslflirt.toFlirt(forward, Image(mov_path), Image(ref_path), "world", "world")
write_lta("fslreg.lta", 15, "FSLREG_TYPE", flirt, mov, ref)
write_lta("coronal.lta", 21, "LINEAR_CORONAL_RAS_TO_CORONAL_RAS", R, mov, ref)

with open(os.path.join(OUT, "register.dat"), "w") as f:
    f.write("scalafim-oracle\n0.900000\n1.500000\n0.150000\n")
    for row in R:
        f.write(" ".join(repr(float(v)) for v in row) + "\n")
    f.write("round\n")

with open(os.path.join(OUT, "talairach.xfm"), "w") as f:
    f.write(
        "MNI Transform File\n% scalafim oracle\n\nTransform_Type = Linear;\nLinear_Transform =\n"
    )
    for r in range(3):
        f.write(
            " "
            + " ".join(repr(float(v)) for v in forward[r])
            + (";" if r == 2 else "")
            + "\n"
        )

rows = []
for p in oc.asymmetric_points((40, 30, 20), count=10):
    point = np.array(p) - 20.0
    q = pullback @ np.append(point, 1.0)
    rows.append(["pullback", *point, *q[:3]])
oc.write_table(
    os.path.join(OUT, "points.tsv"), ["key", "x", "y", "z", "sx", "sy", "sz"], rows
)
oc.write_table(
    os.path.join(OUT, "matrices.tsv"),
    ["key"] + [f"m{r}{c}" for r in range(4) for c in range(4)],
    [
        ["movable_vox2ras"] + list(vox2ras(mov).ravel()),
        ["reference_vox2ras"] + list(vox2ras(ref).ravel()),
        ["movable_tkr"] + list(tkreg(mov).ravel()),
        ["reference_tkr"] + list(tkreg(ref).ravel()),
        ["forward"] + list(forward.ravel()),
    ],
)
oc.write_manifest(
    OUT,
    generator=__file__,
    kind="self-consistency",
    tools={
        "nibabel": nib.__version__,
        "fslpy": fsl.version.__version__,
        "numpy": np.__version__,
    },
    commands=[
        "uv run --with nibabel==5.4.2 --with fslpy==3.29.1 --with numpy python tools/transform/generate_freesurfer_oracle.py"
    ],
    notes=(
        "Self-consistency: the LTA/register.dat/xfm files are written by this generator from FreeSurfer transform.cpp/"
        "mri.cpp semantics (see generator docstring), not by FreeSurfer or an independent implementation. points.tsv: "
        "reference RAS points and the movable RAS point each maps to under the pullback; every file here encodes that "
        "one transform. Reference-implementation inputs: Torig from nibabel get_vox2ras_tkr; FSLREG matrix from fslpy "
        "toFlirt. Cross-implementation: nitransforms reads ras2ras.lta and vox2vox.lta in ../nitransforms_crosscheck. "
        "PENDING (P4.02): native lta_convert / tkregister2 / mri_vol2vol outputs."
    ),
)
print("wrote", OUT)
