#!/usr/bin/env python3
"""Build the TemplateFlow-only inputs of the fsLR 32k route qualification.

Writes two declaration specs (scalafim.fslr-qualification-input/1) for
`FslrQualification` and the derived volumes they name:

- OUT/gm-probseg/spec.json: the res-02 `label-GM` probseg itself, with the
  res-02 `desc-brain` mask as the declared analysis support. Both are TemplateFlow
  archive assets, declared in their own template frame.
- OUT/gm-nan-cut/spec.json: a derived volume, the GM probseg with NaN outside
  the brain mask, and a deliberately non-whole-brain support: every grid voxel
  whose centre lies at or above z = --zmin mm. The support is a slab rather
  than a cut brain mask because the route checks support before finiteness:
  NaN voxels must lie inside the support to yield NonFinite receipts, and the
  cut must reach cortex to yield OutsideSupport receipts.

The derived files are uncompressed NIfTI-1 so their bytes, and hence their
digests, do not depend on gzip timestamps.

Usage:
    python derive_templateflow_inputs.py --gm GM.nii.gz --brain-mask MASK.nii.gz \\
        --revision templateflow@<sha> [--zmin -30] --out OUT
"""
import argparse
import hashlib
import json
import pathlib

import nibabel as nib
import numpy as np

TEMPLATE = "MNI152NLin2009cAsym"
DOI = "10.1016/j.neuroimage.2010.07.033"


def sha256(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def templateflow(path, revision):
    return {
        "templateflow": {
            "template": TEMPLATE,
            "archivePath": f"tpl-{TEMPLATE}/{path.name}",
            "revision": revision,
            "sha256": sha256(path),
        }
    }


def save(data, affine, dtype, path):
    image = nib.Nifti1Image(data.astype(dtype), affine)
    image.set_sform(affine, code=4)
    image.set_qform(affine, code=4)
    nib.save(image, path)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--gm", required=True, type=pathlib.Path)
    parser.add_argument("--brain-mask", required=True, type=pathlib.Path)
    parser.add_argument("--revision", required=True)
    parser.add_argument("--zmin", type=float, default=-30.0)
    parser.add_argument("--out", required=True, type=pathlib.Path)
    args = parser.parse_args()

    gm_image, mask_image = nib.load(args.gm), nib.load(args.brain_mask)
    if gm_image.shape != mask_image.shape or not np.array_equal(
        gm_image.affine, mask_image.affine
    ):
        raise SystemExit("GM probseg and brain mask are not on one grid")
    affine = gm_image.affine
    gm = np.asarray(gm_image.dataobj, dtype=np.float64)
    brain = np.asarray(mask_image.dataobj, dtype=np.float64) > 0.5
    gm_asset = templateflow(args.gm, args.revision)
    mask_asset = templateflow(args.brain_mask, args.revision)
    frame = {"template": TEMPLATE, "release": args.revision}
    literature = {
        "literature": {
            "doi": DOI,
            "statement": f"TemplateFlow tpl-{TEMPLATE} asset, defined in its own template frame",
        }
    }
    script = {"data": {"name": pathlib.Path(__file__).name, "sha256": sha256(__file__)}}

    whole = args.out / "gm-probseg"
    whole.mkdir(parents=True, exist_ok=True)
    (whole / "spec.json").write_text(
        json.dumps(
            {
                "schema": "scalafim.fslr-qualification-input/1",
                "name": "gm-probseg",
                "frame": frame,
                "basis": literature,
                "volumes": [{"path": str(args.gm.resolve()), "asset": gm_asset}],
                "support": {
                    "path": str(args.brain_mask.resolve()),
                    "asset": mask_asset,
                    "threshold": 0.5,
                },
            },
            indent=2,
        )
        + "\n"
    )

    cut = args.out / "gm-nan-cut"
    cut.mkdir(parents=True, exist_ok=True)
    volume = cut / "gm-probseg_nan-outside-brain.nii"
    save(np.where(brain, gm, np.nan), affine, np.float64, volume)
    ijk = np.stack(np.indices(gm.shape), axis=-1).reshape(-1, 3)
    z = (ijk @ affine[:3, :3].T + affine[:3, 3])[:, 2].reshape(gm.shape)
    support = cut / f"support_z-ge-{args.zmin:g}mm.nii"
    save(z >= args.zmin, affine, np.uint8, support)
    (cut / "spec.json").write_text(
        json.dumps(
            {
                "schema": "scalafim.fslr-qualification-input/1",
                "name": "gm-nan-cut",
                "frame": frame,
                "basis": {
                    "derived": {
                        "recipe": "GM probseg with NaN outside the brain mask "
                        "(tools/fslr-qualification/derive_templateflow_inputs.py)",
                        "inputs": [gm_asset, mask_asset, script],
                    }
                },
                "volumes": [
                    {
                        "path": volume.name,
                        "asset": {
                            "data": {"name": volume.name, "sha256": sha256(volume)}
                        },
                    }
                ],
                "support": {
                    "path": support.name,
                    "asset": {
                        "data": {"name": support.name, "sha256": sha256(support)}
                    },
                    "basis": {
                        "derived": {
                            "recipe": f"grid voxels with centre z >= {args.zmin:g} mm "
                            "(tools/fslr-qualification/derive_templateflow_inputs.py)",
                            "inputs": [gm_asset, script],
                        }
                    },
                    "threshold": 0.5,
                },
            },
            indent=2,
        )
        + "\n"
    )
    print(
        json.dumps(
            {
                "brainVoxels": int(brain.sum()),
                "supportVoxels": int((z >= args.zmin).sum()),
            }
        )
    )


if __name__ == "__main__":
    main()
