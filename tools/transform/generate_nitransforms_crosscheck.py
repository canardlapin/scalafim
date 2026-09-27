#!/usr/bin/env python3
"""nitransforms cross-check (STP P4.05, P3.12): nitransforms' reading of ITK, FSL, AFNI and FreeSurfer oracle files.

Evidence kind: cross-implementation. For each file nitransforms' `linear.load` yields an internal RAS matrix that
maps reference points to moving points (a pullback, like ScalaFIM's WorldTransform.pull). points.tsv records that
mapping at asymmetric points; the ScalaFIM suite compares its own pullback with it.

P3.12 adds: the fresh FLIRT case (different source and reference grids); the FreeSurfer RAS_TO_RAS and VOX_TO_VOX
LTAs (nitransforms converts VOX_TO_VOX through the LTA's own volume geometry, then inverts to a pullback); and a
generic oblique, sheared AFNI aff12 written here (afni_generic.aff12.1D), read once without images and once with
oblique base and source images (key suffix `#oblique`), where nitransforms applies AFNI's cardinal/real correction
(`io/afni.py`, _cardinal_rotation). Native AFNI (oblique datasets through 3dAllineate) remains pending.

Run:
    uv run --with nitransforms==25.1.0 --with nibabel==5.4.2 --with h5py==3.16.0 --with numpy python tools/transform/generate_nitransforms_crosscheck.py
"""

from __future__ import annotations

import os
import sys

import nibabel as nib
import nitransforms
import numpy as np
from nitransforms import linear

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

OUT = oc.oracle_dir("nitransforms_crosscheck")
R = oc.ORACLE_ROOT
points = [
    np.array(p)
    for p in [
        (10.0, -20.0, 30.0),
        (-35.5, 12.25, -8.0),
        (0.5, 0.25, -0.75),
        (52.0, -61.0, 17.5),
        (-7.3, 44.1, 21.9),
        (28.4, 3.3, -40.2),
        (-60.0, -15.5, 5.25),
        (15.75, -33.0, -12.5),
    ]
]

cases = []
for name in [
    "affine",
    "euler",
    "euler_zyx",
    "versor_rigid",
    "similarity",
    "scale_skew_versor",
    "translation",
    "composite_euler_affine",
]:
    cases.append((f"itk_linear/{name}.tfm", dict(fmt="itk")))
for src, ref in [(0, 1), (1, 0), (2, 3), (3, 0), (4, 2)]:
    cases.append(
        (
            f"conventions/flirt_{src}_to_{ref}.mat",
            dict(
                fmt="fsl",
                moving=os.path.join(R, f"conventions/fsl_case{src}.nii"),
                reference=os.path.join(R, f"conventions/fsl_case{ref}.nii"),
            ),
        )
    )
cases.append(
    (
        "conventions/flirt_fresh.mat",
        dict(
            fmt="fsl",
            moving=os.path.join(R, "conventions/flirt_fresh_src.nii"),
            reference=os.path.join(R, "conventions/flirt_fresh_ref.nii"),
        ),
    )
)
cases.append(("neurotransform/afni_oracle/oracle.aff12.1D", dict(fmt="afni")))
for name in ["ras2ras.lta", "vox2vox.lta"]:
    cases.append((f"freesurfer_linear/{name}", dict(fmt="lta")))


# A generic AFNI affine: oblique rotation, anisotropic scale, shear and a non-integer shift (DICOM, base -> source).
def rotation(ax, ay, az):
    cx, sx, cy, sy, cz, sz = np.cos(ax), np.sin(ax), np.cos(ay), np.sin(ay), np.cos(az), np.sin(az)
    return (
        np.array([[cz, -sz, 0], [sz, cz, 0], [0, 0, 1]])
        @ np.array([[cy, 0, sy], [0, 1, 0], [-sy, 0, cy]])
        @ np.array([[1, 0, 0], [0, cx, -sx], [0, sx, cx]])
    )


generic = rotation(0.17, -0.23, 0.11) @ np.array([[1.04, 0.06, -0.02], [0.0, 0.93, 0.05], [0.0, 0.0, 1.08]])
generic_row = np.hstack([generic, np.array([[3.7], [-5.2], [2.35]])]).ravel()
with open(os.path.join(OUT, "afni_generic.aff12.1D"), "w") as handle:
    handle.write("# 3dAllineate-style matrix written by generate_nitransforms_crosscheck.py\n")
    handle.write(" ".join(repr(float(v)) for v in generic_row) + "\n")


def oblique_image(name, linear, offset, shape):
    aff = np.eye(4)
    aff[:3, :3] = linear
    aff[:3, 3] = offset
    img = nib.Nifti1Image(np.zeros(shape, np.float32), aff)
    img.header.set_qform(aff, code=1)
    img.header.set_sform(aff, code=1)
    nib.save(img, os.path.join(OUT, name))
    return os.path.join(OUT, name)


oblique_base = oblique_image(
    "afni_oblique_base.nii", rotation(0.21, 0.0, -0.14) @ np.diag([-1.7, 1.3, 2.4]), (61.0, -72.5, -30.25), (10, 12, 8)
)
oblique_source = oblique_image(
    "afni_oblique_source.nii", rotation(-0.09, 0.26, 0.05) @ np.diag([2.2, -0.9, 1.6]), (-48.0, 55.5, -12.0), (9, 14, 11)
)
cases.append(("nitransforms_crosscheck/afni_generic.aff12.1D", dict(fmt="afni")))
cases.append(
    (
        "nitransforms_crosscheck/afni_generic.aff12.1D#oblique",
        dict(fmt="afni", reference=oblique_base, moving=oblique_source),
    )
)

rows = []
notes = []
for rel, kwargs in cases:
    try:
        xfm = linear.load(
            os.path.join(R, rel.split("#")[0]),
            **{
                k: (nib.load(v) if k in ("moving", "reference") else v)
                for k, v in kwargs.items()
            },
        )
        for p in points:
            q = np.asarray(xfm.map([p]))[0].ravel()[:3]
            rows.append([rel, *p, *q])
    except (
        Exception
    ) as error:  # record what nitransforms cannot read instead of failing the whole set
        notes.append(f"{rel}: {type(error).__name__}: {error}")

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
        "uv run --with nitransforms==25.1.0 --with nibabel==5.4.2 --with h5py==3.16.0 --with numpy python tools/transform/generate_nitransforms_crosscheck.py"
    ],
    notes="Reference RAS points and nitransforms' mapped (moving) RAS point. Unreadable by nitransforms: "
    + ("; ".join(notes) or "none"),
)
print(len(rows), "rows;", len(notes), "unreadable:", notes)
