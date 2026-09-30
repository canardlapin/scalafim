#!/usr/bin/env python3
"""ITK linear-transform oracle (STP P3.03): every linear ITK parameterisation written by ITK itself, with
TransformPoint results on asymmetric points.

Evidence kind: native-oracle. SimpleITK wraps ITK; WriteTransform and TransformPoint are ITK's own code paths
(the same IO factories ANTs links against).

Each case writes <case>.tfm (text) and <case>.mat (MATLAB v4, ITK's MatlabTransformIO). points.tsv holds, per case,
input LPS points and ITK's TransformPoint output (LPS). ITK's convention: a transform read from file maps points of the
fixed (output) space to the moving space, i.e. it is the resampling pullback.

Run:
    uv run --with SimpleITK==2.5.6 --with numpy python tools/transform/generate_itk_linear_oracle.py
"""

from __future__ import annotations

import os
import sys

import numpy as np
import SimpleITK as sitk

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

OUT = oc.oracle_dir("itk_linear")
CENTER = (13.9, -21.4, 7.25)

cases = {}

affine = sitk.AffineTransform(3)
affine.SetMatrix((1.02, 0.03, -0.01, -0.02, 0.98, 0.02, 0.01, 0.0, 1.01))
affine.SetTranslation((0.15, -0.2, 0.1))
affine.SetCenter(CENTER)
cases["affine"] = affine

euler = sitk.Euler3DTransform(CENTER, 0.11, -0.07, 0.23, (1.5, -2.25, 0.75))
cases["euler"] = euler

euler_zyx = sitk.Euler3DTransform(CENTER, -0.09, 0.13, -0.17, (-0.5, 1.25, 3.0))
euler_zyx.SetComputeZYX(True)
cases["euler_zyx"] = euler_zyx

versor = sitk.VersorRigid3DTransform()
versor.SetCenter(CENTER)
versor.SetRotation((0.1, -0.2, 0.3), 0.4)
versor.SetTranslation((2.0, -1.0, 0.5))
cases["versor_rigid"] = versor

similarity = sitk.Similarity3DTransform()
similarity.SetCenter(CENTER)
similarity.SetRotation((0.3, 0.1, -0.2), -0.25)
similarity.SetTranslation((-1.0, 0.25, 2.0))
similarity.SetScale(1.07)
cases["similarity"] = similarity

ssv = sitk.ScaleSkewVersor3DTransform()
ssv.SetCenter(CENTER)
ssv.SetRotation((-0.2, 0.4, 0.1), 0.15)
ssv.SetTranslation((0.5, 1.5, -0.75))
ssv.SetScale((1.05, 0.94, 1.1))
ssv.SetSkew((0.01, -0.02, 0.03, 0.015, -0.025, 0.005))
cases["scale_skew_versor"] = ssv

translation = sitk.TranslationTransform(3, (4.5, -3.25, 1.125))
cases["translation"] = translation

composite = sitk.CompositeTransform(3)
composite.AddTransform(
    sitk.Euler3DTransform(CENTER, 0.05, 0.02, -0.04, (1.0, 0.0, -1.0))
)
composite.AddTransform(affine)
cases["composite_euler_affine"] = composite

rng = np.random.default_rng(20260924)
points = [
    (-12.3, 3.8, 10.1),
    (40.25, -60.5, 22.0),
    (0.0, 0.0, 0.0),
    (-75.0, 18.25, -40.5),
]
points += [tuple(rng.uniform(-90, 90, size=3)) for _ in range(8)]

rows = []
for index, (name, transform) in enumerate(cases.items()):
    text_path = os.path.join(OUT, f"{name}.tfm")
    sitk.WriteTransform(transform, text_path)
    if name != "composite_euler_affine":
        sitk.WriteTransform(transform, os.path.join(OUT, f"{name}.mat"))
    # Expected points come from the transform as ITK reads it back from the file: that is what the file means.
    # (An in-memory ScaleSkewVersor3DTransform keeps a stale offset after SetScale/SetSkew; the file does not.)
    from_file = sitk.ReadTransform(text_path)
    for p in points:
        q = from_file.TransformPoint(tuple(float(v) for v in p))
        rows.append([name, index, *p, *q])

oc.write_table(
    os.path.join(OUT, "points.tsv"),
    ["key", "case", "x", "y", "z", "tx", "ty", "tz"],
    rows,
)
oc.write_manifest(
    OUT,
    generator=__file__,
    kind="native-oracle",
    tools={"SimpleITK": sitk.Version_VersionString(), "numpy": np.__version__},
    commands=[
        "uv run --with SimpleITK==2.5.6 --with numpy python tools/transform/generate_itk_linear_oracle.py"
    ],
    notes="Expected points use the transform as re-read from each written .tfm (ITK ReadTransform). Points are LPS millimetres; tx,ty,tz = ITK TransformPoint (fixed -> moving pullback). Composite text lists transforms in ITK order (last applied first).",
)
print(len(cases), "cases,", len(points), "points each")
