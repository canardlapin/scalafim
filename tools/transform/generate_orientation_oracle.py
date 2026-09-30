#!/usr/bin/env python3
"""Orientation oracle (STP P2.02): nibabel axis codes and reorientation of data plus affine.

Evidence kind: reference-implementation (nibabel orientations: io_orientation, aff2axcodes, as_reoriented).

Outputs:
  axcodes.tsv      one row per affine: 16 affine entries and nibabel's aff2axcodes (letters name the positive
                   direction of each voxel axis, e.g. RAS).
  source.nii       a small anisotropic, oblique volume with a unique value per voxel.
  reoriented_<CODES>.nii  source.nii reoriented by nibabel to <CODES> (data and affine).

Run:
    uv run --with nibabel==5.4.2 --with numpy python tools/transform/generate_orientation_oracle.py
"""

from __future__ import annotations

import os
import sys

import numpy as np
import nibabel as nib
from nibabel.orientations import (
    aff2axcodes,
    axcodes2ornt,
    io_orientation,
    ornt_transform,
)

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

OUT = oc.oracle_dir("orientation")


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


def affine(linear, offset=(0.0, 0.0, 0.0)):
    out = np.eye(4)
    out[:3, :3] = linear
    out[:3, 3] = offset
    return out


zooms = np.diag([1.1, 0.9, 2.3])
permute_pir = np.array(
    [[0, 0, -1], [-1, 0, 0], [0, 1, 0]], dtype=float
)  # i->P? built below via signs
cases = [
    affine(zooms, (10, -20, 5)),
    affine(np.diag([-1.1, 0.9, 2.3]), (60, -20, 5)),
    affine(np.diag([-1.1, -0.9, -2.3]), (60, 80, 70)),
    affine(np.array([[0, 0, -2.3], [-1.1, 0, 0], [0, 0.9, 0]]), (1, 2, 3)),
    affine(np.array([[0, 1.1, 0], [0, 0, 0.9], [2.3, 0, 0]]), (-4, 5, -6)),
    affine(rotation(0.2, -0.1, 0.3) @ zooms, (3, -7, 11)),
    affine(rotation(0.5, 0.4, -0.6) @ np.diag([-1.1, 0.9, 2.3]), (-9, 8, 7)),
    affine(rotation(0.0, 0.0, np.deg2rad(40)) @ zooms),
    affine(rotation(np.deg2rad(-35), np.deg2rad(20), 0.0) @ np.diag([1.1, -0.9, 2.3])),
]
rows = []
for index, aff in enumerate(cases):
    rows.append([f"case{index}", *aff.ravel(), "".join(aff2axcodes(aff))])
with open(os.path.join(OUT, "axcodes.tsv"), "w") as f:
    f.write(
        "\t".join(
            ["key"] + [f"m{r}{c}" for r in range(4) for c in range(4)] + ["codes"]
        )
        + "\n"
    )
    for row in rows:
        f.write(
            "\t".join([row[0]] + [repr(float(v)) for v in row[1:17]] + [row[17]]) + "\n"
        )

shape = (5, 4, 3)
source_affine = affine(
    rotation(0.15, -0.1, 0.2) @ np.diag([-1.1, 0.9, 2.3]), (12.5, -30.0, 8.0)
)
data = np.arange(np.prod(shape), dtype=np.float32).reshape(shape, order="F") * 1.5 + 2.0
source = nib.Nifti1Image(data, source_affine)
source.header.set_qform(source_affine, code=1)
source.header.set_sform(source_affine, code=1)
nib.save(source, os.path.join(OUT, "source.nii"))
start = io_orientation(source_affine)
targets = ["RAS", "LPI", "PIR", "SAL", "LAS"]
for codes in targets:
    img = nib.load(os.path.join(OUT, "source.nii"))
    reoriented = img.as_reoriented(ornt_transform(start, axcodes2ornt(tuple(codes))))
    out = nib.Nifti1Image(
        np.asarray(reoriented.dataobj, dtype=np.float32), reoriented.affine
    )
    out.header.set_qform(reoriented.affine, code=1)
    out.header.set_sform(reoriented.affine, code=1)
    nib.save(out, os.path.join(OUT, f"reoriented_{codes}.nii"))

oc.write_manifest(
    OUT,
    generator=__file__,
    kind="reference-implementation",
    tools={"nibabel": nib.__version__, "numpy": np.__version__},
    commands=[
        "uv run --with nibabel==5.4.2 --with numpy python tools/transform/generate_orientation_oracle.py"
    ],
    notes="axcodes follow nibabel: letters name the positive direction of each voxel axis. reoriented_* are nibabel as_reoriented(source, target codes); sform = qform = the reoriented affine.",
)
print(len(rows), "affines;", len(targets), "reorientations")
