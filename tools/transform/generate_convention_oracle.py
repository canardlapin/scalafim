#!/usr/bin/env python3
"""Convention oracles for ScalaFIM's coordinate-convention kernel (STP P2.03).

Evidence kind: reference-implementation.
  * FSL scaled-voxel ("fsl") coordinates and FLIRT world<->world conversion come from fslpy
    (fsl.data.image.Image.getAffine, fsl.transform.flirt.fromFlirt), FSL's own Python library.
  * FreeSurfer vox2ras / vox2ras-tkr for conformed (LIA, 256^3, 1 mm) volumes come from nibabel's MGHHeader.
Native mri_info / flirt outputs replace or supplement these when FreeSurfer and FSL are available (P4.02, P4.04).

Run:
    uv run --with nibabel==5.4.2 --with fslpy==3.29.1 --with numpy python tools/transform/generate_convention_oracle.py
"""

from __future__ import annotations

import os
import sys

import numpy as np
import nibabel as nib
from nibabel.freesurfer.mghformat import MGHHeader

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

from fsl.data.image import Image  # noqa: E402
from fsl.transform import flirt as fslflirt  # noqa: E402
import fsl.version  # noqa: E402

OUT = oc.oracle_dir("conventions")


def rotation(ax: float, ay: float, az: float) -> np.ndarray:
    cx, sx, cy, sy, cz, sz = (
        np.cos(ax),
        np.sin(ax),
        np.cos(ay),
        np.sin(ay),
        np.cos(az),
        np.sin(az),
    )
    rx = np.array([[1, 0, 0], [0, cx, -sx], [0, sx, cx]])
    ry = np.array([[cy, 0, sy], [0, 1, 0], [-sy, 0, cy]])
    rz = np.array([[cz, -sz, 0], [sz, cz, 0], [0, 0, 1]])
    return rz @ ry @ rx


def affine(linear: np.ndarray, offset) -> np.ndarray:
    out = np.eye(4)
    out[:3, :3] = linear
    out[:3, 3] = offset
    return out


# --- FSL cases: qform and sform deliberately disagree in code and handedness ---------------------------------------
DIMS = (7, 9, 6)
ZOOMS = (1.1, 0.8, 2.3)
radiological = affine(
    rotation(0.12, -0.08, 0.2) @ np.diag([-1.1, 0.8, 2.3]), (40.5, -21.25, -12.0)
)  # det < 0
neurological = affine(
    rotation(-0.05, 0.1, -0.15) @ np.diag([1.1, 0.8, 2.3]), (-35.0, -18.5, 9.75)
)  # det > 0

FSL_CASES = [
    # (qform, qcode, sform, scode)
    (radiological, 1, neurological, 2),  # sform wins: neurological -> x flip
    (neurological, 1, radiological, 4),  # sform wins: radiological -> no flip
    (radiological, 1, None, 0),  # qform only: radiological
    (neurological, 2, None, 0),  # qform only: neurological
    (None, 0, None, 0),  # neither: scaling fallback
]

fsl_rows = []
for index, (qform, qcode, sform, scode) in enumerate(FSL_CASES):
    data = np.arange(np.prod(DIMS), dtype=np.float32).reshape(DIMS)
    img = nib.Nifti1Image(data, None)
    img.header.set_zooms(ZOOMS)
    if qform is not None:
        img.header.set_qform(qform, code=qcode)
    else:
        img.header.set_qform(None, code=0)
    if sform is not None:
        img.header.set_sform(sform, code=scode)
    else:
        img.header.set_sform(None, code=0)
    path = os.path.join(OUT, f"fsl_case{index}.nii")
    nib.save(img, path)
    fimg = Image(path)
    selected = 2 if scode != 0 else (1 if qcode != 0 else 0)
    v2w = fimg.getAffine("voxel", "world")
    v2f = fimg.getAffine("voxel", "fsl")
    stored = nib.load(path).header
    q_stored = stored.get_qform() if qcode != 0 else np.full((4, 4), 0.0)
    s_stored = stored.get_sform() if scode != 0 else np.full((4, 4), 0.0)
    fsl_rows.append(
        [index, *DIMS, *ZOOMS, qcode, scode, selected, int(fimg.isNeurological())]
        + list(v2w.ravel())
        + list(v2f.ravel())
        + list(q_stored.ravel())
        + list(s_stored.ravel())
    )

m16 = lambda prefix: [f"{prefix}{r}{c}" for r in range(4) for c in range(4)]  # noqa: E731
oc.write_table(
    os.path.join(OUT, "fsl_cases.tsv"),
    [
        "case",
        "nx",
        "ny",
        "nz",
        "px",
        "py",
        "pz",
        "qcode",
        "scode",
        "selected",
        "neurological",
    ]
    + m16("v2w")
    + m16("v2f")
    + m16("qform")
    + m16("sform"),
    fsl_rows,
)

# FLIRT matrices between case pairs, converted to world->world by fslpy.
flirt_rows = []
pairs = [(0, 1), (1, 0), (2, 3), (3, 0), (4, 2)]
for n, (src, ref) in enumerate(pairs):
    lin = rotation(0.03 * (n + 1), -0.02 * n, 0.05) @ np.diag([1.02, 0.97, 1.05])
    lin[0, 1] += 0.04  # shear
    flirt_mat = affine(lin, (2.5 - n, -1.25 + 0.5 * n, 3.75))
    src_img = Image(os.path.join(OUT, f"fsl_case{src}.nii"))
    ref_img = Image(os.path.join(OUT, f"fsl_case{ref}.nii"))
    world = fslflirt.fromFlirt(flirt_mat, src_img, ref_img, "world", "world")
    np.savetxt(os.path.join(OUT, f"flirt_{src}_to_{ref}.mat"), flirt_mat, fmt="%.10f")
    flirt_rows.append([src, ref] + list(flirt_mat.ravel()) + list(world.ravel()))
oc.write_table(
    os.path.join(OUT, "flirt_pairs.tsv"),
    ["src", "ref"] + m16("flirt") + m16("world"),
    flirt_rows,
)

# --- FreeSurfer conformed volumes: vox2ras (Norig) and vox2ras-tkr (Torig) ----------------------------------------
fs_rows = []
for index, cras in enumerate(
    [(0.0, 0.0, 0.0), (12.3, -45.6, 7.8), (-3.25, 18.5, -22.125)]
):
    header = MGHHeader()
    header.set_data_shape((256, 256, 256))
    header.set_zooms((1.0, 1.0, 1.0))
    lia = np.array([[-1.0, 0, 0], [0, 0, 1.0], [0, -1.0, 0]])
    center = np.array(cras)
    offset = center - lia @ (np.array([256, 256, 256]) / 2.0)
    header.set_affine = None  # MGHHeader is configured through its fields
    header["Mdc"] = lia.T  # nibabel stores direction cosines column-wise transposed
    header["Pxyz_c"] = center
    norig = header.get_vox2ras()
    torig = header.get_vox2ras_tkr()
    fs_rows.append(
        [index, 256, 256, 256, 1.0, 1.0, 1.0, *cras]
        + list(np.asarray(norig, dtype=float).ravel())
        + list(np.asarray(torig, dtype=float).ravel())
    )
oc.write_table(
    os.path.join(OUT, "freesurfer_conformed.tsv"),
    ["case", "nx", "ny", "nz", "px", "py", "pz", "cr", "ca", "cs"]
    + m16("norig")
    + m16("torig"),
    fs_rows,
)

oc.write_manifest(
    OUT,
    generator=__file__,
    kind="reference-implementation",
    tools={
        "nibabel": nib.__version__,
        "fslpy": fsl.version.__version__,
        "numpy": np.__version__,
    },
    commands=[
        "uv run --with nibabel==5.4.2 --with fslpy==3.29.1 --with numpy python tools/transform/generate_convention_oracle.py"
    ],
    notes=(
        "FSL cases: qform/sform disagree in code and handedness; 'selected' is 2=sform, 1=qform, 0=scaling fallback; "
        "v2w is fslpy voxToWorld, v2f is fslpy voxel->fsl. FLIRT pairs: fslpy fromFlirt world->world. "
        "FreeSurfer: conformed LIA 256^3 1mm volumes only; nibabel's tkr matrix is float32 and hard-codes LIA, so it is "
        "a reference only for conformed volumes. Native mri_info (oblique) and flirt oracles are pending P4.02/P4.04."
    ),
)
print("wrote", OUT)
