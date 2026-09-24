#!/usr/bin/env python3
"""Reproduce the TemplateFlow MNI152NLin6Asym <-> MNI152NLin2009cAsym
transform-direction finding (docs/audits/templateflow-mni-transform-direction.md).

Usage:
    python transform_direction_evidence.py --assets DIR [--json OUT]

DIR must contain these TemplateFlow files (flat layout, archive basenames):
    tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5
    tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym_mode-image_xfm.h5
    tpl-fsLR_den-32k_hemi-{L,R}_midthickness.surf.gii
    tpl-fsLR_hemi-{L,R}_den-32k_desc-nomedialwall_dparc.label.gii
    tpl-MNI152NLin2009cAsym_res-01_label-GM_probseg.nii.gz

Requires the versions pinned in requirements.txt. Prints a JSON summary;
digests are included so a result is bound to the exact inputs.
"""
import argparse
import hashlib
import json
import pathlib

import h5py
import nibabel as nib
import nitransforms as nt
import numpy as np

FORWARD_NAMED = "tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5"
REVERSE_NAMED = "tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym_mode-image_xfm.h5"
GM = "tpl-MNI152NLin2009cAsym_res-01_label-GM_probseg.nii.gz"


def sha256(path):
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for block in iter(lambda: handle.read(1 << 20), b""):
            digest.update(block)
    return digest.hexdigest()


def stages(path):
    """Stage types and small parameters. ITK spells the datasets 'Tranform*' here."""
    out = []
    with h5py.File(path) as h5:
        group = h5["TransformGroup"]
        for key in sorted(group.keys(), key=int):
            node = group[key]
            kind = node["TransformType"][0]
            kind = kind.decode() if isinstance(kind, bytes) else str(kind)
            entry = {"index": int(key), "type": kind}
            for name in ("TranformFixedParameters", "TransformFixedParameters"):
                if name in node:
                    entry["fixed"] = np.round(node[name][:], 4).tolist()
            for name in ("TranformParameters", "TransformParameters"):
                if name in node:
                    params = node[name]
                    entry["parameterCount"] = int(params.shape[0])
                    if params.shape[0] <= 12:
                        entry["parameters"] = np.round(params[:], 4).tolist()
            out.append(entry)
    return out


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--assets", required=True, type=pathlib.Path)
    parser.add_argument("--json", type=pathlib.Path)
    args = parser.parse_args()
    d = args.assets

    vertices = np.vstack(
        [
            nib.load(d / f"tpl-fsLR_den-32k_hemi-{h}_midthickness.surf.gii")
            .darrays[0]
            .data.astype(float)
            for h in "LR"
        ]
    )
    cortex = np.concatenate(
        [
            nib.load(d / f"tpl-fsLR_hemi-{h}_den-32k_desc-nomedialwall_dparc.label.gii")
            .darrays[0]
            .data
            > 0.5
            for h in "LR"
        ]
    )
    gm_img = nib.load(d / GM)
    gm = gm_img.get_fdata()
    world_to_voxel = np.linalg.inv(gm_img.affine)

    def gm_score(points):
        ijk = np.rint(points @ world_to_voxel[:3, :3].T + world_to_voxel[:3, 3]).astype(
            int
        )
        inside = np.all((ijk >= 0) & (ijk < gm.shape), axis=1)
        values = np.full(len(points), np.nan)
        values[inside] = gm[tuple(ijk[inside].T)]
        values = values[cortex]
        return {
            "meanGM": round(float(np.nanmean(values)), 4),
            "fractionAbove0.5": round(float(np.nanmean(values > 0.5)), 4),
            "outsideGrid": int(np.sum(~inside[cortex])),
        }

    def invert(transform, targets, iterations=12):
        solution = targets.copy()
        for _ in range(iterations):
            solution = solution + (targets - transform.map(solution))
        residual = np.linalg.norm(targets - transform.map(solution), axis=1)
        return solution, residual

    summary = {
        "inputs": {
            p.name: sha256(p)
            for p in sorted(d.iterdir())
            if p.suffix in (".h5", ".gii", ".gz") and p.name.startswith("tpl-")
        },
        "versions": {
            "nibabel": nib.__version__,
            "nitransforms": nt.__version__,
            "h5py": h5py.__version__,
            "numpy": np.__version__,
        },
        "cortexVertices": int(cortex.sum()),
        "raw": gm_score(vertices),
        "files": {},
    }
    mapped = {}
    for name in (FORWARD_NAMED, REVERSE_NAMED):
        transform = nt.manip.load(str(d / name), fmt="itk")
        applied = transform.map(vertices)
        inverted, residual = invert(transform, vertices)
        mapped[name] = (transform, applied)
        shift = np.linalg.norm(applied - vertices, axis=1)
        summary["files"][name] = {
            "stages": stages(d / name),
            "pointMapDisplacementMm": {
                "median": round(float(np.median(shift)), 3),
                "p95": round(float(np.percentile(shift, 95)), 3),
                "max": round(float(shift.max()), 3),
            },
            "appliedAsPointMap": gm_score(applied),
            "inverted": gm_score(inverted),
            "inverseResidualMm": {
                "median": round(float(np.median(residual)), 5),
                "max": round(float(residual.max()), 5),
            },
        }
    reverse_transform = mapped[REVERSE_NAMED][0]
    forward_applied = mapped[FORWARD_NAMED][1]
    round_trip = np.linalg.norm(
        reverse_transform.map(forward_applied) - vertices, axis=1
    )
    summary["roundTripReverseOfForwardMm"] = {
        "median": round(float(np.median(round_trip)), 3),
        "p99": round(float(np.percentile(round_trip, 99)), 3),
        "max": round(float(round_trip.max()), 3),
    }
    text = json.dumps(summary, indent=2)
    print(text)
    if args.json:
        args.json.write_text(text + "\n")


if __name__ == "__main__":
    main()
