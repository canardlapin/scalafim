#!/usr/bin/env python3
"""h5py dumps of the SimpleITK HDF5 fixtures (STP P3.12), so Scala.js runs the same interpretation checks as the JVM.

Evidence kind: native-oracle. The inputs are the SimpleITK-written files in ../itk_hdf5_simpleitk (see that manifest);
their point oracles stay in ../itk_hdf5_simpleitk/point_oracles.tsv. Each <name>.components.txt holds one line per
/TransformGroup/<i>: index, TransformType, parameters, fixed parameters (tab-separated; values space-separated, float32
datasets widened exactly to double). The legacy file's misspelled `Tranform*` dataset names are read like ITK's
current names, exactly as the JVM jHDF container reads them. The JVM suite checks that jHDF decodes every dumped file to
these dumps. malformed_missing_fixed.h5 lacks TransformFixedParameters, which its dump records as an empty cell. The
refusal of that file (a truncated file, never an implicit zero centre) belongs to the HDF5 container, which exists only
on the JVM: ITK text files legitimately carry an empty fixed-parameter line, so the platform-neutral model cannot tell
the two apart, and Scala.js has no HDF5 container to refuse it with.

Run:
    uv run --with h5py==3.16.0 --with numpy python tools/transform/generate_itk_hdf5_simpleitk_dumps.py
"""

from __future__ import annotations

import os
import sys

import h5py
import numpy as np

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

SOURCE = os.path.join(oc.ORACLE_ROOT, "itk_hdf5_simpleitk")
OUT = oc.oracle_dir("itk_hdf5_simpleitk_dumps")
NAMES = [
    "affine_only_forward_double.h5",
    "composite_affine_displacement_double.h5",
    "composite_affine_displacement_legacy_float.h5",
    "pullback_plus_one.h5",
    "pullback_minus_one.h5",
    "unsupported_bspline.h5",
    "malformed_missing_fixed.h5",
]
names_used = {}
for name in NAMES:
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

            def values(candidates):
                for candidate in candidates:
                    if candidate in node:
                        names_used.setdefault(name, set()).add(candidate)
                        return " ".join(
                            repr(float(v)) for v in np.asarray(node[candidate]).ravel()
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
    with open(os.path.join(OUT, name.replace(".h5", ".components.txt")), "w") as handle:
        handle.write("\n".join(lines) + "\n")

assert names_used["composite_affine_displacement_legacy_float.h5"] == {
    "TranformParameters",
    "TranformFixedParameters",
}
oc.write_manifest(
    OUT,
    generator=__file__,
    kind="native-oracle",
    tools={"h5py": h5py.__version__, "numpy": np.__version__},
    commands=[
        "uv run --with h5py==3.16.0 --with numpy python tools/transform/generate_itk_hdf5_simpleitk_dumps.py"
    ],
    notes=(
        "h5py decodes of ../itk_hdf5_simpleitk/*.h5 (SimpleITK-written; see that manifest). The legacy-float file "
        "stores float32 datasets under the historic 'Tranform*' names. malformed_missing_fixed's missing fixed "
        "parameters are an empty cell; refusing that file is a JVM container check (jHDF). Point oracles: ../itk_hdf5_simpleitk/point_oracles.tsv."
    ),
)
print("wrote", len(NAMES), "dumps")
