#!/usr/bin/env python3
"""nibabel's reading of every NIfTI oracle file, to pin ScalaFIM's raw NIfTI-1 transform-container reader (STP P3.02).

Evidence kind: reference-implementation (nibabel). One row per file, keyed by path relative to the oracle root.

Run:
    uv run --with nibabel==5.4.2 --with numpy python tools/transform/generate_nifti_header_oracle.py
"""

from __future__ import annotations

import os
import sys

import numpy as np
import nibabel as nib

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

OUT = oc.oracle_dir("nifti_headers")

rows = []
for root, _, names in sorted(os.walk(oc.ORACLE_ROOT)):
    for name in sorted(names):
        if not (name.endswith(".nii") or name.endswith(".nii.gz")):
            continue
        path = os.path.join(root, name)
        rel = os.path.relpath(path, oc.ORACLE_ROOT)
        img = nib.load(path)
        hdr = img.header
        dims = list(hdr["dim"][:8].astype(int))
        data = np.asarray(img.dataobj, dtype=np.float64).ravel(order="F")
        first = list(data[:4]) + [0.0] * max(0, 4 - data.size)
        # nibabel's own qform uses the stored float32 fields widened to float64, like ScalaFIM's reader.
        qform = hdr.get_qform() if hdr["qform_code"] > 0 else np.zeros((4, 4))
        sform = hdr.get_sform() if hdr["sform_code"] > 0 else np.zeros((4, 4))
        rows.append(
            [rel]
            + dims
            + [
                int(hdr["intent_code"]),
                int(hdr["datatype"]),
                int(hdr["qform_code"]),
                int(hdr["sform_code"]),
            ]
            + [float(v) for v in hdr["pixdim"][:4]]
            + [
                float(hdr["intent_p1"]),
                float(hdr["intent_p2"]),
                float(hdr["intent_p3"]),
            ]
            + list(np.asarray(qform, dtype=float).ravel())
            + list(np.asarray(sform, dtype=float).ravel())
            + [float(v) for v in first]
            + [float(data.sum()), float(np.abs(data).max() if data.size else 0.0)]
        )

m16 = lambda p: [f"{p}{r}{c}" for r in range(4) for c in range(4)]  # noqa: E731
header = (
    ["key"]
    + [f"dim{i}" for i in range(8)]
    + [
        "intent",
        "datatype",
        "qcode",
        "scode",
        "pixdim0",
        "pixdim1",
        "pixdim2",
        "pixdim3",
        "p1",
        "p2",
        "p3",
    ]
    + m16("qform")
    + m16("sform")
    + ["v0", "v1", "v2", "v3", "sum", "absmax"]
)
oc.write_table(os.path.join(OUT, "headers.tsv"), header, rows)
oc.write_manifest(
    OUT,
    generator=__file__,
    kind="reference-implementation",
    tools={"nibabel": nib.__version__, "numpy": np.__version__},
    commands=[
        "uv run --with nibabel==5.4.2 --with numpy python tools/transform/generate_nifti_header_oracle.py"
    ],
    notes="nibabel header fields, qform/sform (zeros when the code is 0), first values in Fortran (x-fastest) order, sum and max |value| of the scaled data.",
)
print(len(rows), "files")
