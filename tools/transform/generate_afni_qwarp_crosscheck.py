#!/usr/bin/env python3
"""AFNI 3dQwarp displacement-field cross-check (STP P3.12): nitransforms' reading of AFNI-style `_WARP.nii` fields.

Evidence kind: cross-implementation. AFNI itself is not installed, so these files are constructed here in the layout
3dQwarp writes (`3dQwarp -help`, "STORAGE of 3D warps in AFNI"): a 5D (x,y,z,1,3) float32 NIfTI on the base grid whose
components are DICOM (LPS) millimetre displacements from each base point to the matching source point, i.e. a
pullback `source = base + d`. nitransforms 25.1.0 (`io/afni.py`, AFNIDisplacementsField) negates the x and y components
into RAS and places the lattice with nibabel's `img.affine`: the sform when its code is set, else the qform.

AFNI reads the same form (`thd_niftiread.c`, `AFNI_NIFTI_PRIORITY` defaults to 'S') but its warp code (`mri_nwarp.c`,
e.g. THD_nwarp_forward_xyz) places the field with `daxes->ijk_to_dicom`, the cardinalised version of that form. For a
cardinal (permuted, possibly flipped) sform the two coincide, so the read fields here have a cardinal radiological sform
(code 2) and an oblique qform (code 1) that ITK would pick instead. Agreement with nitransforms therefore also reflects
AFNI's placement. An oblique field (oblique_WARP.nii) is kept only as a refusal case: ScalaFIM rejects it as an
unqualified convention until 3dNwarpXYZ can pin AFNI's cardinalised placement.

Cases:
  * affine_WARP.nii: displacements affine in space, so nitransforms' cubic B-spline and ScalaFIM's trilinear
    interpolation agree off the lattice; 16 off-grid points at least 8 voxels from every face (nitransforms' mirror
    boundary spoils cubic exactness near faces).
  * smooth_WARP.nii: nonlinear displacements, compared on 12 interior lattice nodes, where nitransforms returns the
    stored value exactly.
  * oblique_WARP.nii: the smooth field on an oblique sform; no points (refusal case).

A native AFNI check (3dNwarpXYZ on these files) is pending until AFNI is available.

Run:
    uv run --with nitransforms==25.1.0 --with nibabel==5.4.2 --with numpy python tools/transform/generate_afni_qwarp_crosscheck.py
"""

from __future__ import annotations

import os
import sys

import nibabel as nib
import nitransforms
import numpy as np
from nitransforms.nonlinear import DenseFieldTransform

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

OUT = oc.oracle_dir("afni_qwarp")
SHAPE = (9, 7, 6)  # smooth field
AFFINE_SHAPE = (26, 24, 22)  # affine field: large enough for points far from every face
MARGIN = 8.0  # voxels between off-grid points and the nearest face
ZOOMS = (1.6, 2.3, 1.2)


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


def affine(linear, offset):
    out = np.eye(4)
    out[:3, :3] = linear
    out[:3, 3] = offset
    return out


# cardinal, radiological, permuted sform (code 2: not SCANNER_ANAT); an oblique qform (code 1) that ITK would choose
SFORM = np.array(
    [
        [0.0, 0.0, -ZOOMS[2], 14.5],
        [-ZOOMS[0], 0.0, 0.0, -21.0],
        [0.0, ZOOMS[1], 0.0, -6.25],
        [0.0, 0.0, 0.0, 1.0],
    ]
)
OBLIQUE_SFORM = affine(
    rotation(0.21, -0.13, 0.34) @ np.diag([-ZOOMS[0], ZOOMS[1], ZOOMS[2]]),
    (14.5, -21.0, -6.25),
)
QFORM = affine(rotation(-0.1, 0.05, 0.0) @ np.diag(ZOOMS), (-3.0, 4.0, 9.5))

i, j, k = np.meshgrid(*[np.arange(n, dtype=float) for n in AFFINE_SHAPE], indexing="ij")
AFFINE_LPS = np.array(
    [
        [0.08, -0.05, 0.03, 1.25],
        [0.02, 0.06, -0.04, -0.75],
        [-0.03, 0.01, 0.07, 0.5],
    ]
)
affine_lps = np.stack(
    [
        AFFINE_LPS[r, 0] * i
        + AFFINE_LPS[r, 1] * j
        + AFFINE_LPS[r, 2] * k
        + AFFINE_LPS[r, 3]
        for r in range(3)
    ],
    axis=-1,
)
i, j, k = np.meshgrid(*[np.arange(n, dtype=float) for n in SHAPE], indexing="ij")
smooth_lps = np.stack(
    [
        0.9 * np.sin(i / 2.2) + 0.3 * j - 0.1 * k,
        -0.7 * np.cos(j / 1.7) + 0.2 * k,
        0.5 * np.sin(k / 1.3 + i / 4.0) - 0.25,
    ],
    axis=-1,
)


def write(name, lps, sform=SFORM):
    img = nib.Nifti1Image(lps[:, :, :, None, :].astype(np.float32), None)
    img.header.set_zooms(ZOOMS + (1.0, 1.0))
    img.header.set_qform(QFORM, code=1)
    img.header.set_sform(sform, code=2)
    path = os.path.join(OUT, name)
    nib.save(img, path)
    return path


def load(path):
    return DenseFieldTransform.from_filename(path, fmt="afni")


rows = []
# affine field: off-grid points far from every face. nitransforms prefilters with mirror boundaries, which a cubic
# B-spline cannot extend linearly; the resulting error decays by ~0.27 per voxel away from a face.
affine_path = write("affine_WARP.nii", affine_lps)
field = load(affine_path)
worst = 0.0
inner = tuple(int(n - 2 * MARGIN) for n in AFFINE_SHAPE)
voxels = [tuple(MARGIN + v for v in p) for p in oc.asymmetric_points(inner, count=16, seed=31)]
assert len(voxels) == 16, len(voxels)
lps_to_ras = np.diag([-1.0, -1.0, 1.0])
for v in voxels:
    world = (SFORM @ np.append(np.array(v), 1.0))[:3]
    mapped = np.asarray(field.map([world]))[0]
    analytic = world + lps_to_ras @ (AFFINE_LPS[:, :3] @ np.array(v) + AFFINE_LPS[:, 3])
    worst = max(worst, float(np.abs(mapped - analytic).max()))
    rows.append(["affine_WARP.nii", *world, *mapped])
assert worst < 1e-4, worst

# nonlinear field: interior lattice nodes only (nitransforms interpolates off-grid with a cubic B-spline)
smooth_path = write("smooth_WARP.nii", smooth_lps)
field = load(smooth_path)
nodes = sorted(
    {
        tuple(min(max(int(round(v)), 1), n - 2) for v, n in zip(p, SHAPE))
        for p in oc.asymmetric_points(SHAPE, count=40, seed=17)
    }
)[:12]
assert len(nodes) == 12, len(nodes)
for node in nodes:
    world = (SFORM @ np.append(np.array(node, dtype=float), 1.0))[:3]
    mapped = np.asarray(field.map([world]))[0]
    rows.append(["smooth_WARP.nii", *world, *mapped])

write("oblique_WARP.nii", smooth_lps, sform=OBLIQUE_SFORM)

oc.write_table(
    os.path.join(OUT, "points.tsv"), ["key", "x", "y", "z", "sx", "sy", "sz"], rows
)
oc.write_manifest(
    OUT,
    generator=__file__,
    kind="cross-implementation",
    tools={
        "nitransforms": nitransforms.__version__,
        "nibabel": nib.__version__,
        "numpy": np.__version__,
    },
    commands=[
        "uv run --with nitransforms==25.1.0 --with nibabel==5.4.2 --with numpy python tools/transform/generate_afni_qwarp_crosscheck.py"
    ],
    notes=(
        "AFNI 3dQwarp-layout fields constructed here (AFNI not installed) and mapped by nitransforms "
        "DenseFieldTransform(fmt='afni'). points.tsv: base (reference) RAS point and nitransforms' source RAS point. "
        "The read fields have a cardinal radiological sform (code 2), which nitransforms and AFNI both use, and an "
        "oblique qform (code 1), which ITK would use. "
        f"affine_WARP.nii rows are off-grid; nitransforms' cubic result differs from the analytic affine value by at most "
        f"{worst:.2e} mm. smooth_WARP.nii rows are lattice nodes. oblique_WARP.nii (oblique sform) has no rows: AFNI's "
        "warp code places it on the cardinalised grid, which ScalaFIM refuses as unqualified. "
        "PENDING: native AFNI 3dNwarpXYZ on these files."
    ),
)
print(len(rows), "rows; worst affine deviation", worst)
