#!/usr/bin/env python3
"""Writer acceptance (STP P5.03): native tools read ScalaFIM's writer goldens.

The goldens (written by scalafim.transform.WriterGoldens) all encode one sheared affine pullback, and two dense fields
sampling it on a lattice. Each check reads a golden with an independent reader and maps query points:
  * native-oracle: SimpleITK (ITK itself) for ITK text, MATLAB v4, HDF5 and the ANTs displacement field.
  * reference-implementation: fslpy for FLIRT.
Checks whose native tool is not installed are recorded as "pending-native" (FSL flirt/applywarp, AFNI 3dAllineate,
FreeSurfer mri_vol2vol/lta_convert/tkregister2); they must be run where those tools exist.

Writes <goldens>/receipt.json: golden SHA-256s, tool versions, and per-check status and max error (mm).

Run (via tools/transform/writer-acceptance.sh):
    uv run --with SimpleITK==2.5.6 --with nibabel==5.4.2 --with fslpy==3.29.1 --with nitransforms==25.1.0 --with h5py==3.16.0 --with numpy python tools/transform/writer_acceptance.py <goldens-dir>
"""

from __future__ import annotations

import hashlib
import json
import os
import sys

import numpy as np

GOLDENS = sys.argv[1]
FIXTURES = os.path.join(os.path.dirname(GOLDENS), "freesurfer_linear")
TOLERANCE = 1e-5

queries = np.loadtxt(os.path.join(GOLDENS, "queries.tsv"), skiprows=1)
points, expected = queries[:, :3], queries[:, 3:]
lps = lambda v: np.array([-v[0], -v[1], v[2]])  # noqa: E731
checks = {}
tools = {}


def record(name, tool, status, error=None, note=""):
    checks[name] = {"tool": tool, "status": status, "max_error_mm": error, "note": note}


try:
    import SimpleITK as sitk

    tools["SimpleITK"] = sitk.Version_VersionString()
    for name in ["affine.tfm", "affine.mat"]:
        transform = sitk.ReadTransform(os.path.join(GOLDENS, name))
        errors = [
            np.abs(lps(np.array(transform.TransformPoint(tuple(lps(p))))) - e).max()
            for p, e in zip(points, expected)
        ]
        worst = float(max(errors))
        record(
            name,
            "SimpleITK ReadTransform/TransformPoint",
            "pass" if worst < TOLERANCE else "fail",
            worst,
        )
    # ANTs field: ITK DisplacementFieldTransform at interior lattice points; the field samples the golden pullback.
    field = sitk.ReadImage(
        os.path.join(GOLDENS, "field_1Warp.nii"), sitk.sitkVectorFloat64
    )
    dft = sitk.DisplacementFieldTransform(sitk.Image(field))
    size = field.GetSize()
    pullback = np.array(
        json.load(open(os.path.join(GOLDENS, "pullback.json")))["pullback"]
    ).reshape(4, 4)
    errors = []
    for index in [
        (1, 1, 1),
        (size[0] - 2, 2, 1),
        (3, size[1] - 2, size[2] - 2),
        (2, 3, 2),
    ]:
        physical = field.TransformIndexToPhysicalPoint(index)
        mapped = lps(np.array(dft.TransformPoint(physical)))
        errors.append(
            np.abs(
                mapped - (pullback @ np.append(lps(np.array(physical)), 1.0))[:3]
            ).max()
        )
    worst = float(max(errors))
    record(
        "field_1Warp.nii",
        "SimpleITK DisplacementFieldTransform",
        "pass" if worst < TOLERANCE else "fail",
        worst,
    )
except ImportError:
    for name in ["affine.tfm", "affine.mat", "field_1Warp.nii"]:
        record(name, "SimpleITK", "pending-native")

try:
    import fsl.version
    from fsl.data.image import Image
    from fsl.transform import flirt as fslflirt

    tools["fslpy"] = fsl.version.__version__
    flirt = np.loadtxt(os.path.join(GOLDENS, "flirt.mat"))
    forward = fslflirt.fromFlirt(
        flirt,
        Image(os.path.join(FIXTURES, "movable.nii")),
        Image(os.path.join(FIXTURES, "reference.nii")),
        "world",
        "world",
    )
    pull = np.linalg.inv(forward)
    worst = float(
        max(
            np.abs((pull @ np.append(p, 1.0))[:3] - e).max()
            for p, e in zip(points, expected)
        )
    )
    record(
        "flirt.mat",
        "fslpy fromFlirt (reference implementation)",
        "pass" if worst < TOLERANCE else "fail",
        worst,
        "native flirt -applyxfm still pending",
    )
except ImportError:
    record("flirt.mat", "fslpy", "pending-native")

for name, tool in [
    ("affine.aff12.1D", "AFNI 3dAllineate -1Dmatrix_apply"),
    ("affine.lta", "FreeSurfer mri_vol2vol --lta / lta_convert"),
    ("talairach.xfm", "FreeSurfer / MINC xfm reader"),
    ("register.dat", "FreeSurfer tkregister2 --noedit"),
    ("field_fnirt_relative.nii", "FSL applywarp"),
]:
    checks.setdefault(
        name,
        {
            "tool": tool,
            "status": "pending-native",
            "max_error_mm": None,
            "note": "tool not installed on the generating machine",
        },
    )

goldens = {}
listed = [n.strip() for n in open(os.path.join(GOLDENS, "goldens.txt")) if n.strip()]
for name in listed + ["queries.tsv", "pullback.json", "goldens.txt"]:
    with open(os.path.join(GOLDENS, name), "rb") as f:
        goldens[name] = hashlib.sha256(f.read()).hexdigest()

receipt = {
    "goldens": goldens,
    "tools": tools,
    "tolerance_mm": TOLERANCE,
    "checks": dict(sorted(checks.items())),
}
with open(os.path.join(GOLDENS, "receipt.json"), "w") as f:
    json.dump(receipt, f, indent=1)
    f.write("\n")
failed = [k for k, v in checks.items() if v["status"] == "fail"]
print(json.dumps({k: v["status"] for k, v in checks.items()}, indent=1))
sys.exit(1 if failed else 0)
