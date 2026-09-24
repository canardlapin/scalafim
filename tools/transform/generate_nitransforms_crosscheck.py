#!/usr/bin/env python3
"""nitransforms cross-check (STP P4.05): nitransforms' reading of ITK, FSL and AFNI oracle files.

Evidence kind: cross-implementation. For each file nitransforms' `linear.load` yields an internal RAS matrix that
maps reference points to moving points (a pullback, like ScalaFIM's WorldTransform.pull). points.tsv records that
mapping at asymmetric points; the ScalaFIM suite compares its own pullback with it.

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
cases.append(("neurotransform/afni_oracle/oracle.aff12.1D", dict(fmt="afni")))

rows = []
notes = []
for rel, kwargs in cases:
    try:
        xfm = linear.load(
            os.path.join(R, rel),
            **{
                k: (nib.load(v) if k in ("moving", "reference") else v)
                for k, v in kwargs.items()
            },
        )
        for p in points:
            q = np.asarray(xfm.map([p]))[0]
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
