"""Regenerate the small independent ITK fixtures (SimpleITK 2.5.6, numpy 2.5.3).

Run with a Python environment containing those packages. No neurotransform code
is used to write the transforms or evaluate the expected points/images.
"""
from pathlib import Path
import hashlib
import json

import numpy as np
import SimpleITK as sitk

out = Path(__file__).resolve().parents[1] / "inst/extdata/itk_oracle"
out.mkdir(parents=True, exist_ok=True)
size = (7, 8, 9)
z, y, x = np.indices(size[::-1], dtype=float)
vectors = np.stack((0.12 + 0.02*x + 0.01*y*z,
                    -0.18 + 0.015*y + 0.005*x*z,
                    0.25 - 0.01*z + 0.004*x*y), axis=-1)
field = sitk.GetImageFromArray(vectors, isVector=True)
field.SetOrigin((12., -8., 3.))
field.SetSpacing((1.3, 1.7, 2.1))
theta = 0.31
field.SetDirection((np.cos(theta), -np.sin(theta), 0.,
                    np.sin(theta), np.cos(theta), 0., 0., 0., 1.))
sitk.WriteImage(field, str(out / "warp.nii.gz"))
warp = sitk.DisplacementFieldTransform(sitk.Image(field))
affine = sitk.AffineTransform(3)
affine.SetMatrix((1.02, 0.03, -0.01, -0.02, 0.98, 0.02, 0.01, 0., 1.01))
affine.SetCenter(field.TransformContinuousIndexToPhysicalPoint((3., 3.5, 4.)))
affine.SetTranslation((0.15, -0.2, 0.1))
for suffix in ("tfm", "mat", "h5"):
    sitk.WriteTransform(affine, str(out / f"affine.{suffix}"))

# Interior fractional indices exercise interpolation as well as the oblique grid.
indices = [(1.2, 2.3, 3.4), (3.1, 4.2, 5.3), (4.4, 2.1, 2.8)]
points = [field.TransformContinuousIndexToPhysicalPoint(p) for p in indices]
flip = np.array([-1., -1., 1.])
source = sitk.GetImageFromArray(20. + 2*x + 3*y + 5*z)
source.CopyInformation(field)
sitk.WriteImage(source, str(out / "source.nii.gz"))
transforms = {"affine": affine, "warp": warp}
for name, components in (("affine_warp", [affine, warp]),
                         ("warp_affine", [warp, affine])):
    tx = sitk.CompositeTransform(components)
    sitk.WriteTransform(tx, str(out / f"{name}.h5"))
    reference = sitk.Resample(source, source, tx, sitk.sitkLinear, 0., sitk.sitkFloat64)
    sitk.WriteImage(reference, str(out / f"{name}_resampled.nii.gz"))
    transforms[name] = tx

evidence = {
    "SimpleITK": str(sitk.Version()), "numpy": np.__version__,
    "points_ras": (np.array(points) * flip).tolist(),
    "expected_ras": {name: (np.array([tx.TransformPoint(p) for p in points]) * flip).tolist()
                     for name, tx in transforms.items()},
    "sha256": {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
               for p in sorted(out.iterdir()) if p.suffix not in (".json", ".md")},
}
(out / "evidence.json").write_text(json.dumps(evidence, indent=2) + "\n")
