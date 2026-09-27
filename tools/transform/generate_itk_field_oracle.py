#!/usr/bin/env python3
"""ITK displacement-field oracle (STP P3.08, P3.12): ITK's own DisplacementFieldTransform on the vendored neurotransform
itk_oracle warp (5D NIfTI, intent 1007), and on fields whose qform and sform disagree, at asymmetric interior points.

Evidence kind: native-oracle (SimpleITK = ITK: NIfTI IO, DisplacementFieldTransform, TransformPoint in LPS).

The forms_*.nii fields pin which NIfTI affine ITK's NiftiImageIO places a field with (ITK 5.4
SetImageIOOrientationFromNIfTI): the orthonormal sform when the qform code is unset or the sform code is SCANNER_ANAT,
otherwise the qform; spacing always from pixdim; no code at all gives LPS pixdim scaling at the origin; a
non-orthonormal sform with no qform is refused. forms.tsv records what ITK chose for each file.

Run:
    uv run --with SimpleITK==2.5.6 --with nibabel==5.4.2 --with numpy python tools/transform/generate_itk_field_oracle.py
"""

from __future__ import annotations

import os
import sys

import nibabel as nib
import numpy as np
import SimpleITK as sitk

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

OUT = oc.oracle_dir("itk_field")
warp_path = os.path.join(oc.ORACLE_ROOT, "neurotransform", "itk_oracle", "warp.nii.gz")

field = sitk.ReadImage(warp_path, sitk.sitkVectorFloat64)
transform = sitk.DisplacementFieldTransform(sitk.Image(field))
size = field.GetSize()

rows = []
for p in oc.asymmetric_points(size, count=12):
    # interior continuous indices -> physical LPS points, then ITK's TransformPoint
    physical = field.TransformContinuousIndexToPhysicalPoint(tuple(float(v) for v in p))
    q = transform.TransformPoint(physical)
    rows.append(["warp", *physical, *q])

oc.write_table(
    os.path.join(OUT, "points.tsv"), ["key", "x", "y", "z", "tx", "ty", "tz"], rows
)
oc.write_table(
    os.path.join(OUT, "grid.tsv"),
    ["key", "nx", "ny", "nz", "ox", "oy", "oz", "sx", "sy", "sz"]
    + [f"d{i}" for i in range(9)],
    [["warp", *size, *field.GetOrigin(), *field.GetSpacing(), *field.GetDirection()]],
)

# --- qform/sform disagreement: which affine does ITK place the lattice with? ------------------------------------------
def rotation(ax, ay, az):
    cx, sx, cy, sy, cz, sz = np.cos(ax), np.sin(ax), np.cos(ay), np.sin(ay), np.cos(az), np.sin(az)
    return (
        np.array([[cz, -sz, 0], [sz, cz, 0], [0, 0, 1]])
        @ np.array([[cy, 0, sy], [0, 1, 0], [-sy, 0, cy]])
        @ np.array([[1, 0, 0], [0, cx, -sx], [0, sx, cx]])
    )


FORM_SHAPE = (8, 6, 7)
ZOOMS = (1.4, 1.9, 1.1)


def form(linear, offset):
    out = np.eye(4)
    out[:3, :3] = linear
    out[:3, 3] = offset
    return out


QFORM = form(rotation(0.12, -0.2, 0.31) @ np.diag(ZOOMS), (11.0, -7.5, 4.25))
SFORM = form(rotation(-0.27, 0.16, -0.09) @ np.diag([-ZOOMS[0], ZOOMS[1], ZOOMS[2]]), (-19.5, 14.0, -6.75))
SCALE_MISMATCH = form(rotation(0.05, 0.1, -0.2) @ np.diag([2.0, 2.0, 2.0]), (3.0, -2.0, 1.5))
SHEARED = form(np.array([[1.4, 0.5, 0.0], [0.0, 1.9, 0.0], [0.0, 0.0, 1.1]]), (5.0, 6.0, 7.0))
fi, fj, fk = np.meshgrid(*[np.arange(n, dtype=float) for n in FORM_SHAPE], indexing="ij")
FORM_LPS = np.stack(
    [0.6 * np.sin(fi / 1.9) + 0.2 * fj, -0.5 * np.cos(fj / 1.4) + 0.15 * fk, 0.4 * np.sin(fk / 1.6 + fi / 3.0) - 0.1],
    axis=-1,
)
FORM_CASES = [
    # (name, qform, qcode, sform, scode)
    ("forms_q1_s1.nii", QFORM, 1, SFORM, 1),  # SCANNER_ANAT sform wins over a set qform
    ("forms_q1_s2.nii", QFORM, 1, SFORM, 2),  # any other sform code: the qform wins
    ("forms_q2_s4.nii", QFORM, 2, SFORM, 4),
    ("forms_q0_s2.nii", None, 0, SFORM, 2),  # no qform: the sform
    ("forms_q1_s0.nii", QFORM, 1, None, 0),
    ("forms_q0_s0.nii", None, 0, None, 0),  # neither: LPS pixdim scaling at the origin
    ("forms_q0_s1_scale.nii", None, 0, SCALE_MISMATCH, 1),  # sform directions, pixdim spacing
    ("forms_q0_s2_shear.nii", None, 0, SHEARED, 2),  # refused by ITK
]
form_rows = []
form_points = []
lps = np.diag([-1.0, -1.0, 1.0])
for name, qform, qcode, sform, scode in FORM_CASES:
    img = nib.Nifti1Image(FORM_LPS[:, :, :, None, :].astype(np.float32), None)
    img.header.set_intent(1007)
    img.header.set_zooms(ZOOMS + (1.0, 1.0))
    img.header.set_qform(qform, code=qcode)
    img.header.set_sform(sform, code=scode)
    path = os.path.join(OUT, name)
    nib.save(img, path)
    try:
        itk_field = sitk.ReadImage(path, sitk.sitkVectorFloat64)
    except RuntimeError:
        form_rows.append([name, qcode, scode, -1.0])
        continue
    origin = lps @ np.array(itk_field.GetOrigin())  # RAS
    direction = lps @ np.array(itk_field.GetDirection()).reshape(3, 3)

    def same(matrix):
        return matrix is not None and np.allclose(origin, matrix[:3, 3], atol=1e-4) and np.allclose(
            direction, matrix[:3, :3] / np.linalg.norm(matrix[:3, :3], axis=0), atol=1e-4
        )

    chosen = 2 if same(sform) else 1 if same(qform) else 0
    form_rows.append([name, qcode, scode, float(chosen)])
    itk_transform = sitk.DisplacementFieldTransform(sitk.Image(itk_field))
    for p in oc.asymmetric_points(FORM_SHAPE, count=12, seed=23):
        physical = itk_field.TransformContinuousIndexToPhysicalPoint(tuple(float(v) for v in p))
        form_points.append([name, *physical, *itk_transform.TransformPoint(physical)])

oc.write_table(os.path.join(OUT, "forms.tsv"), ["key", "qcode", "scode", "chosen"], form_rows)
oc.write_table(os.path.join(OUT, "forms_points.tsv"), ["key", "x", "y", "z", "tx", "ty", "tz"], form_points)

oc.write_manifest(
    OUT,
    generator=__file__,
    kind="native-oracle",
    tools={"SimpleITK": sitk.Version_VersionString(), "nibabel": nib.__version__, "numpy": np.__version__},
    commands=[
        "uv run --with SimpleITK==2.5.6 --with nibabel==5.4.2 --with numpy python tools/transform/generate_itk_field_oracle.py"
    ],
    notes=(
        "Input: ../neurotransform/itk_oracle/warp.nii.gz read by ITK's NIfTI IO. points.tsv: fixed-space LPS points "
        "(interior continuous indices mapped by the field's own geometry) and ITK DisplacementFieldTransform.TransformPoint "
        "(LPS). grid.tsv: ITK's origin/spacing/direction (LPS) for the field, pinning which NIfTI affine ITK used. "
        "forms_*.nii: fields written by nibabel with disagreeing qform/sform codes; forms.tsv 'chosen' is the affine ITK "
        "placed each with (2=sform, 1=qform, 0=neither: LPS pixdim scaling, -1=ITK refused the file); forms_points.tsv "
        "holds 12 off-grid LPS points per readable field and ITK TransformPoint."
    ),
)
print(len(rows), "points; grid", size)
