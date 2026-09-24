#!/usr/bin/env python3
"""ITK displacement-field oracle (STP P3.08): ITK's own DisplacementFieldTransform on the vendored neurotransform
itk_oracle warp (5D NIfTI, intent 1007) and on a 4D variant, at asymmetric interior points.

Evidence kind: native-oracle (SimpleITK = ITK: NIfTI IO, DisplacementFieldTransform, TransformPoint in LPS).

Run:
    uv run --with SimpleITK==2.5.6 --with numpy python tools/transform/generate_itk_field_oracle.py
"""

from __future__ import annotations

import os
import sys

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
oc.write_manifest(
    OUT,
    generator=__file__,
    kind="native-oracle",
    tools={"SimpleITK": sitk.Version_VersionString(), "numpy": np.__version__},
    commands=[
        "uv run --with SimpleITK==2.5.6 --with numpy python tools/transform/generate_itk_field_oracle.py"
    ],
    notes=(
        "Input: ../neurotransform/itk_oracle/warp.nii.gz read by ITK's NIfTI IO. points.tsv: fixed-space LPS points "
        "(interior continuous indices mapped by the field's own geometry) and ITK DisplacementFieldTransform.TransformPoint "
        "(LPS). grid.tsv: ITK's origin/spacing/direction (LPS) for the field, pinning which NIfTI affine ITK used."
    ),
)
print(len(rows), "points; grid", size)
