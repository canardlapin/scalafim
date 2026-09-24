#!/usr/bin/env python3
"""X5 oracle (STP P3.10 / P4.05): X5 files written by nitransforms, with nitransforms' own point mapping.

Evidence kind: cross-implementation. X5 is a draft BIDS specification whose reference implementation is nitransforms;
agreement means "consistent with nitransforms", not verified against an independent standard.

Semantics (nitransforms 25.1): every node maps reference (Domain) RAS points to moving RAS points (a pullback).
linear/affine: Transform is the 4x4 RAS matrix. nonlinear/densefield: Transform is (x,y,z,3) on the Domain grid,
"displacements" (y = x + d) or "deformations" (y = field). A TransformChain dataset "i/j/..." applies nodes in that
order: y = f_j(f_i(x)).

Outputs: <case>.x5, <case>.nodes.txt (h5py dump: one node per line, for Scala.js), points.tsv.

Run:
    uv run --with nitransforms==25.1.0 --with nibabel==5.4.2 --with h5py==3.16.0 --with numpy python tools/transform/generate_x5_oracle.py
"""

from __future__ import annotations

import json
import os
import sys

import h5py
import nibabel as nib
import nitransforms
import numpy as np
from nitransforms.linear import Affine
from nitransforms.manip import TransformChain
from nitransforms.nonlinear import DenseFieldTransform
from nitransforms.io.x5 import to_filename

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

OUT = oc.oracle_dir("x5")

reference_affine = np.array(
    [
        [1.2, 0.1, 0.0, -20.0],
        [-0.08, 1.1, 0.05, -30.5],
        [0.02, -0.03, 1.4, 10.25],
        [0, 0, 0, 1.0],
    ]
)
shape = (9, 8, 7)
reference = nib.Nifti1Image(np.zeros(shape, dtype=np.float32), reference_affine)

linear = Affine(
    np.array(
        [
            [0.98, 0.03, -0.02, 1.5],
            [-0.01, 1.02, 0.04, -2.25],
            [0.02, -0.03, 0.99, 0.75],
            [0, 0, 0, 1.0],
        ]
    ),
    reference=reference,
)

i, j, k = np.meshgrid(*[np.arange(n, dtype=float) for n in shape], indexing="ij")
deltas = np.stack(
    [
        0.3 * np.sin(i / 3.0) + 0.05 * j,
        -0.2 * np.cos(j / 2.5) + 0.02 * k,
        0.15 * i - 0.1 * k,
    ],
    axis=-1,
)
dense = DenseFieldTransform(
    nib.Nifti1Image(deltas.astype(np.float64), reference_affine), is_deltas=True
)
world = (reference_affine @ np.stack([i, j, k, np.ones_like(i)], axis=-1)[..., None])[
    ..., :3, 0
]
deform = DenseFieldTransform(
    nib.Nifti1Image((world + deltas).astype(np.float64), reference_affine),
    is_deltas=False,
)

cases = {
    "linear": [linear.to_x5()],
    "displacements": [dense.to_x5()],
    "deformations": [deform.to_x5()],
}
for name, nodes in cases.items():
    to_filename(os.path.join(OUT, f"{name}.x5"), nodes)
# chain: linear first, then the displacement field (y = dense(linear(x)))
chain_path = os.path.join(OUT, "chain.x5")
to_filename(chain_path, [linear.to_x5(), dense.to_x5()])
with h5py.File(chain_path, "a") as f:
    f.create_group("TransformChain").create_dataset("0", data="0/1")


def dump(path):
    lines = []
    with h5py.File(path, "r") as f:
        for key in sorted(f["TransformGroup"].keys(), key=int):
            node = f["TransformGroup"][key]
            fields = {
                "index": key,
                "type": node.attrs["Type"],
                "subtype": node.attrs.get("SubType", ""),
                "representation": node.attrs.get("Representation", ""),
                "shape": list(node["Transform"].shape),
                "transform": " ".join(
                    repr(float(v))
                    for v in np.asarray(node["Transform"]).ravel(order="C")
                ),
            }
            if "Domain" in node:
                d = node["Domain"]
                fields["domain_size"] = [int(v) for v in np.asarray(d["Size"]).ravel()]
                fields["domain_mapping"] = " ".join(
                    repr(float(v)) for v in np.asarray(d["Mapping"]).ravel(order="C")
                )
            lines.append(json.dumps(fields, default=str))
        chains = (
            [f["TransformChain"][c][()] for c in sorted(f["TransformChain"].keys())]
            if "TransformChain" in f
            else []
        )
        lines += [
            "chain " + (c.decode() if isinstance(c, bytes) else str(c)) for c in chains
        ]
    return lines


for name in list(cases) + ["chain"]:
    with open(os.path.join(OUT, f"{name}.nodes.txt"), "w") as f:
        f.write("\n".join(dump(os.path.join(OUT, f"{name}.x5"))) + "\n")

rows = []
# nitransforms interpolates dense fields off-grid with a cubic B-spline (map_coordinates order=3, NaN outside), while
# ITK/FSL (and ScalaFIM) interpolate linearly, so points are compared exactly on the lattice where both agree.
# interior lattice indices only: on the lattice edge, round-off can place a point a hair outside the field
lattice = sorted(
    {tuple(min(max(int(round(v)), 1), n - 2) for v, n in zip(p, shape)) for p in oc.asymmetric_points(shape, count=24, seed=5)}
)[:12]
on_grid = [(reference_affine @ np.append(np.array(p, dtype=float), 1.0))[:3] for p in lattice]
linear_inverse = np.linalg.inv(linear.matrix)
for name in list(cases) + ["chain"]:
    path = os.path.join(OUT, f"{name}.x5")
    if name == "chain":
        chain = TransformChain.from_filename(path, fmt="X5", x5_chain=0)
        # start where the linear stage lands exactly on the dense lattice
        queries = [(linear_inverse @ np.append(w, 1.0))[:3] for w in on_grid]
    else:
        chain = TransformChain.from_filename(path, fmt="X5", x5_chain=None)[0]
        queries = on_grid
    for q in queries:
        rows.append([name, *q, *np.asarray(chain.map([q]))[0]])
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
        "h5py": h5py.__version__,
        "numpy": np.__version__,
    },
    commands=[
        "uv run --with nitransforms==25.1.0 --with nibabel==5.4.2 --with h5py==3.16.0 --with numpy python tools/transform/generate_x5_oracle.py"
    ],
    notes="points.tsv: reference RAS points on the dense lattice and nitransforms' mapped (moving) RAS point (nitransforms interpolates off-grid with a cubic B-spline; ScalaFIM/ITK/FSL are linear). X5 is a draft spec; this is consistency with nitransforms.",
)
print(len(rows), "points")
