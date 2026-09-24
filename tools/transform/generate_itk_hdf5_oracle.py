#!/usr/bin/env python3
"""ITK HDF5 composite oracle (STP P3.07).

For each vendored neurotransform itk_oracle HDF5 file (affine.h5, affine_warp.h5, warp_affine.h5):
  * <name>.components.txt: the file decoded by h5py (index, TransformType, parameters, fixed parameters), so the
    platform-neutral interpretation is verified on Scala.js without an HDF5 reader; a JVM test checks jHDF agrees.
  * points.tsv: ITK's own TransformPoint (SimpleITK ReadTransform) at asymmetric interior LPS points.

Evidence kind: native-oracle (ITK) for points; h5py decoding for the component dumps.

Run:
    uv run --with SimpleITK==2.5.6 --with h5py==3.16.0 --with numpy python tools/transform/generate_itk_hdf5_oracle.py
"""

from __future__ import annotations

import os
import sys

import h5py
import numpy as np
import SimpleITK as sitk

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

OUT = oc.oracle_dir("itk_hdf5")
SOURCE = os.path.join(oc.ORACLE_ROOT, "neurotransform", "itk_oracle")
field = sitk.ReadImage(os.path.join(SOURCE, "warp.nii.gz"), sitk.sitkVectorFloat64)

rows = []
for name in ["affine.h5", "affine_warp.h5", "warp_affine.h5"]:
    with h5py.File(os.path.join(SOURCE, name), "r") as h5:
        group = h5["TransformGroup"]
        lines = []
        for index in sorted(group.keys(), key=int):
            node = group[index]
            kind = node["TransformType"][()]
            kind = (
                kind[0]
                if hasattr(kind, "__len__") and not isinstance(kind, (bytes, str))
                else kind
            )
            kind = kind.decode() if isinstance(kind, bytes) else str(kind)

            def values(names):
                for n in names:
                    if n in node:
                        return " ".join(
                            repr(float(v)) for v in np.asarray(node[n]).ravel()
                        )
                return ""

            lines.append(
                "\t".join(
                    [
                        index,
                        kind,
                        values(["TransformParameters", "TranformParameters"]),
                        values(["TransformFixedParameters", "TranformFixedParameters"]),
                    ]
                )
            )
    with open(os.path.join(OUT, name.replace(".h5", ".components.txt")), "w") as f:
        f.write("\n".join(lines) + "\n")

    transform = sitk.ReadTransform(os.path.join(SOURCE, name))
    # interior points of the field lattice (its LPS physical space) so every stage stays in support
    for p in oc.asymmetric_points(field.GetSize(), count=12, seed=11):
        physical = field.TransformContinuousIndexToPhysicalPoint(
            tuple(float(v) for v in p)
        )
        rows.append([name, *physical, *transform.TransformPoint(physical)])

oc.write_table(
    os.path.join(OUT, "points.tsv"), ["key", "x", "y", "z", "tx", "ty", "tz"], rows
)
oc.write_manifest(
    OUT,
    generator=__file__,
    kind="native-oracle",
    tools={
        "SimpleITK": sitk.Version_VersionString(),
        "h5py": h5py.__version__,
        "numpy": np.__version__,
    },
    commands=[
        "uv run --with SimpleITK==2.5.6 --with h5py==3.16.0 --with numpy python tools/transform/generate_itk_hdf5_oracle.py"
    ],
    notes="Inputs are ../neurotransform/itk_oracle/*.h5. Points are LPS; tx,ty,tz = ITK TransformPoint (fixed -> moving).",
)
print(len(rows), "points")
