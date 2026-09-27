#!/usr/bin/env python3
"""ITK inverse displacement-field oracle (STP P6.02): ITK's own InvertDisplacementFieldImageFilter applied to the
vendored neurotransform itk_oracle warp, with ITK's own TransformPoint residual of the result.

Evidence kind: native-oracle (SimpleITK = ITK: NIfTI IO, InvertDisplacementFieldImageFilter, DisplacementFieldTransform,
TransformPoint in LPS).

The forward field d is a pullback: DisplacementFieldTransform sends a fixed point p to p + d(p). The filter returns a
field e on the same lattice such that p + e(p) is the preimage of p: TransformPoint(p + e(p)) = p. ScalaFIM's
numerical inverse of the same field (WorldTransform.Mapped.invertNumerically) estimates exactly that preimage on a
lattice with the field's geometry, so the two can be compared lattice point by lattice point.

The filter runs with EnforceBoundaryCondition off (on, it pins e to zero on the lattice faces, which is not the
inverse there) and with tolerances far below the comparison tolerance. Its convergence is not taken on trust: every
row carries the residual |TransformPoint(p + e(p)) - p| (mm) evaluated through ITK's own forward transform, whose
out-of-lattice behaviour (the half-voxel border hold) is the one ScalaFIM reads with DenseContext.itk.

Outputs: inverse.tsv (i, j, k; fixed LPS point p; ITK inverse displacement e(p), LPS; ITK residual), manifest.json.

Run:
    uv run --with SimpleITK==2.5.6 --with numpy python tools/transform/generate_itk_inverse_oracle.py
"""

from __future__ import annotations

import os
import sys

import numpy as np
import SimpleITK as sitk

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

OUT = oc.oracle_dir("itk_inverse")
warp_path = os.path.join(oc.ORACLE_ROOT, "neurotransform", "itk_oracle", "warp.nii.gz")

ITERATIONS = 500
MAX_ERROR = 1e-10
MEAN_ERROR = 1e-12

field = sitk.ReadImage(warp_path, sitk.sitkVectorFloat64)
invert = sitk.InvertDisplacementFieldImageFilter()
invert.SetMaximumNumberOfIterations(ITERATIONS)
invert.SetMaxErrorToleranceThreshold(MAX_ERROR)
invert.SetMeanErrorToleranceThreshold(MEAN_ERROR)
invert.SetEnforceBoundaryCondition(False)
inverse = invert.Execute(field)

forward = sitk.DisplacementFieldTransform(sitk.Image(field))
values = sitk.GetArrayFromImage(inverse)  # (z, y, x, 3)
size = field.GetSize()
rows = []
for k in range(size[2]):
    for j in range(size[1]):
        for i in range(size[0]):
            p = np.array(field.TransformIndexToPhysicalPoint((i, j, k)))
            e = values[k, j, i]
            residual = np.linalg.norm(
                np.array(forward.TransformPoint(tuple(p + e))) - p
            )
            rows.append([float(i), float(j), float(k), *p, *e, residual])

oc.write_table(
    os.path.join(OUT, "inverse.tsv"),
    ["i", "j", "k", "px", "py", "pz", "ex", "ey", "ez", "residual"],
    rows,
)

worst = max(row[-1] for row in rows)
oc.write_manifest(
    OUT,
    generator=__file__,
    kind="native-oracle",
    tools={"SimpleITK": sitk.Version_VersionString(), "numpy": np.__version__},
    commands=[
        "uv run --with SimpleITK==2.5.6 --with numpy python tools/transform/generate_itk_inverse_oracle.py"
    ],
    notes=(
        "Input: ../neurotransform/itk_oracle/warp.nii.gz (a 7x8x9 oblique ITK displacement field, float64 on read) "
        f"inverted by InvertDisplacementFieldImageFilter (MaximumNumberOfIterations {ITERATIONS}, "
        f"MaxErrorToleranceThreshold {MAX_ERROR}, MeanErrorToleranceThreshold {MEAN_ERROR}, EnforceBoundaryCondition "
        f"off; the filter reported max error norm {invert.GetMaxErrorNorm():.3g} and mean {invert.GetMeanErrorNorm():.3g}). "
        "inverse.tsv: one row per lattice point, x fastest: index (i, j, k), the fixed LPS point p, the inverse "
        "displacement e(p) (LPS mm), and the residual |TransformPoint(p + e(p)) - p| (mm) through ITK's "
        f"DisplacementFieldTransform of the forward field (largest {worst:.3g} mm)."
    ),
)
print(len(rows), "lattice points; largest ITK residual", worst)
