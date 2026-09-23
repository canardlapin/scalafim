#!/usr/bin/env python3
"""Independent reference mapping of MNI152NLin2009cAsym volumes onto fsLR 32k.

This is the WS5 oracle for docs/plans/fslr32k-exact-route-blockers.md. It shares
no code with ScalaFIM: nibabel reads inputs, SimpleITK (the ITK reference
implementation: trilinear displacement interpolation, half-voxel clamped border,
zero displacement outside) applies the ITK composite in LPS, and the lookup
contract is re-implemented from its declaration. nitransforms is NOT used as the
oracle: it interpolates displacement with cubic B-splines in float32 and differs
from ITK by up to ~0.015 mm on fsLR vertices (more near field edges).

- fsLR midthickness vertices are in MNI152NLin6Asym; they are moved into
  MNI152NLin2009cAsym by the per-vertex inverse of
  tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym (a 2009c->6Asym point map), solved
  by fixed-point iteration; the residual is reported per vertex;
- nearest voxel with ties rounded up (floor(i + 0.5)); per-axis support
  [-0.5, dim - 0.5);
- medial-wall vertices (desc-nomedialwall == 0) are MedialWall; nonfinite or
  out-of-grid lookups are NoSupport; everything else is Mapped.

Usage:
    python independent_fslr_mapping.py --assets DIR --out OUT.npz VOLUME.nii.gz [...]

Writes OUT.npz with, per hemisphere h in {L, R}: world_<h> (warped vertices, RAS
mm), residual_<h>, voxel_<h> (int, -1 outside), coverage_<h>
(0 Mapped, 1 MedialWall, 2 NoSupport) and value_<h>_<k> for each input volume k;
plus OUT.json with digests and counts.
"""
import argparse
import hashlib
import json
import pathlib

import nibabel as nib
import SimpleITK as sitk
import numpy as np

TRANSFORM = "tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5"


def sha256(path):
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for block in iter(lambda: handle.read(1 << 20), b""):
            digest.update(block)
    return digest.hexdigest()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--assets", required=True, type=pathlib.Path)
    parser.add_argument("--out", required=True, type=pathlib.Path)
    parser.add_argument("--iterations", type=int, default=12)
    parser.add_argument("volumes", nargs="+", type=pathlib.Path)
    args = parser.parse_args()

    itk = sitk.ReadTransform(str(args.assets / TRANSFORM))
    lps = np.array([-1.0, -1.0, 1.0])

    def point_map(points_ras):
        """2009c -> 6Asym point map in RAS mm, evaluated by ITK in LPS."""
        out = np.empty_like(points_ras)
        for i, p in enumerate(points_ras * lps):
            out[i] = itk.TransformPoint(tuple(float(v) for v in p))
        return out * lps

    images = [nib.load(p) for p in args.volumes]
    reference = images[0]
    for path, image in zip(args.volumes, images):
        if image.shape[:3] != reference.shape[:3] or not np.array_equal(
            image.affine, reference.affine
        ):
            raise SystemExit(f"{path} is not on the common grid")
    dims = np.array(reference.shape[:3])
    world_to_voxel = np.linalg.inv(reference.affine)
    data = [np.asarray(image.dataobj, dtype=np.float64) for image in images]

    arrays, summary = {}, {"inputs": {}, "hemispheres": {}}
    for path in list(args.volumes) + [args.assets / TRANSFORM]:
        summary["inputs"][path.name] = sha256(path)

    for hemi in "LR":
        surf = args.assets / f"tpl-fsLR_den-32k_hemi-{hemi}_midthickness.surf.gii"
        wall = (
            args.assets
            / f"tpl-fsLR_hemi-{hemi}_den-32k_desc-nomedialwall_dparc.label.gii"
        )
        summary["inputs"][surf.name] = sha256(surf)
        summary["inputs"][wall.name] = sha256(wall)
        vertices = nib.load(surf).darrays[0].data.astype(np.float64)
        cortex = nib.load(wall).darrays[0].data > 0.5

        solution = vertices.copy()
        for _ in range(args.iterations):
            solution = solution + (vertices - point_map(solution))
        residual = np.linalg.norm(vertices - point_map(solution), axis=1)

        continuous = solution @ world_to_voxel[:3, :3].T + world_to_voxel[:3, 3]
        rounded = np.floor(continuous + 0.5).astype(np.int64)
        inside = np.all((rounded >= 0) & (rounded < dims), axis=1) & np.all(
            np.isfinite(continuous), axis=1
        )
        linear = np.full(len(vertices), -1, dtype=np.int64)
        linear[inside] = np.ravel_multi_index(
            tuple(rounded[inside].T), tuple(dims), order="F"
        )
        ties = np.any(np.abs(continuous - np.floor(continuous) - 0.5) < 1e-6, axis=1)

        coverage = np.full(len(vertices), 2, dtype=np.int8)
        for k, volume in enumerate(data):
            values = np.full(len(vertices), np.nan)
            values[inside] = volume.ravel(order="F")[linear[inside]]
            finite = np.isfinite(values)
            if k == 0:
                coverage[:] = np.where(~cortex, 1, np.where(finite, 0, 2))
            elif np.any(np.where(~cortex, 1, np.where(finite, 0, 2)) != coverage):
                raise SystemExit(
                    "volumes disagree on finite support; map them separately"
                )
            values[coverage != 0] = np.nan
            arrays[f"value_{hemi}_{k}"] = values

        arrays[f"world_{hemi}"] = solution
        arrays[f"residual_{hemi}"] = residual
        arrays[f"voxel_{hemi}"] = linear
        arrays[f"coverage_{hemi}"] = coverage
        summary["hemispheres"][hemi] = {
            "vertices": int(len(vertices)),
            "mapped": int(np.sum(coverage == 0)),
            "medialWall": int(np.sum(coverage == 1)),
            "noSupport": int(np.sum(coverage == 2)),
            "tieVertices": int(np.sum(ties & cortex)),
            "inverseResidualMm": {
                "median": float(np.median(residual)),
                "max": float(residual.max()),
            },
        }

    summary["volumes"] = [p.name for p in args.volumes]
    summary["versions"] = {
        "nibabel": nib.__version__,
        "SimpleITK": sitk.Version_VersionString(),
        "numpy": np.__version__,
    }
    np.savez_compressed(args.out, **arrays)
    args.out.with_suffix(".json").write_text(json.dumps(summary, indent=2) + "\n")
    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
