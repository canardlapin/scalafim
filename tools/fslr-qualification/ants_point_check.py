#!/usr/bin/env python3
"""Check the placed fsLR 32k vertices of a qualification run with ANTs.

The route places each fsLR midthickness vertex x (MNI152NLin6Asym) at the
per-vertex inverse y of the tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym point
map T, so T(y) = x up to the solver tolerance. This script applies T to the
placed positions with `antsApplyTransformsToPoints` and reports |T(y) - x|. It
also compares ANTs with SimpleITK on both opposite-named TemplateFlow files and
reports how far each file moves the vertices, so the direction semantics of the
ANTs command line are on record.

ANTs and SimpleITK share the ITK transform core. The check is independent of
ScalaFIM's placement and of the ANTs command-line direction semantics, not of
ITK itself.

Points are written as LPS CSV (x, y, z, t) and mapped with `-d 3 -p 1` (double
precision) and `-t FILE` as given.

Usage:
    python ants_point_check.py --placed-dir RUN --assets DIR \\
        --ants-bin .../antsApplyTransformsToPoints [--work DIR] [--out ants.json]

RUN is a `FslrQualification` output directory with world_L.npy and
world_R.npy; DIR holds the fsLR midthickness surfaces and both transforms.
"""
import argparse
import csv
import hashlib
import json
import pathlib
import subprocess
import tempfile

import nibabel as nib
import numpy as np
import SimpleITK as sitk

FORWARD = "tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5"
REVERSE = "tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym_mode-image_xfm.h5"
LPS = np.array([-1.0, -1.0, 1.0])


def sha256(path):
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for block in iter(lambda: handle.read(1 << 20), b""):
            digest.update(block)
    return digest.hexdigest()


def stats(values):
    return {
        "median": float(np.median(values)),
        "p99": float(np.percentile(values, 99)),
        "max": float(values.max()),
    }


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--placed-dir", required=True, type=pathlib.Path)
    parser.add_argument("--assets", required=True, type=pathlib.Path)
    parser.add_argument("--ants-bin", required=True, type=pathlib.Path)
    parser.add_argument("--work", type=pathlib.Path)
    parser.add_argument("--out", type=pathlib.Path)
    args = parser.parse_args()
    work = args.work or pathlib.Path(tempfile.mkdtemp(prefix="ants-point-check-"))
    work.mkdir(parents=True, exist_ok=True)
    forward, reverse = args.assets / FORWARD, args.assets / REVERSE

    def ants_map(points_ras, transform, tag):
        source, target = work / f"in-{tag}.csv", work / f"out-{tag}.csv"
        with open(source, "w", newline="") as handle:
            writer = csv.writer(handle)
            writer.writerow(["x", "y", "z", "t"])
            for p in points_ras * LPS:
                writer.writerow(
                    [repr(float(p[0])), repr(float(p[1])), repr(float(p[2])), "0"]
                )
        subprocess.run(
            [
                str(args.ants_bin),
                "-d",
                "3",
                "-p",
                "1",
                "-i",
                str(source),
                "-o",
                str(target),
                "-t",
                str(transform),
            ],
            check=True,
            capture_output=True,
        )
        rows = list(csv.reader(open(target)))[1:]
        return np.array([[float(r[0]), float(r[1]), float(r[2])] for r in rows]) * LPS

    def sitk_map(points_ras, transform):
        itk = sitk.ReadTransform(str(transform))
        return (
            np.array(
                [
                    itk.TransformPoint(tuple(float(v) for v in p))
                    for p in points_ras * LPS
                ]
            )
            * LPS
        )

    result = {
        "schema": "scalafim.fslr-ants-point-check/1",
        "antsBin": str(args.ants_bin),
        "command": "antsApplyTransformsToPoints -d 3 -p 1 -i IN.csv -o OUT.csv -t TRANSFORM (LPS x,y,z,t)",
        "placedDir": str(args.placed_dir),
        "inputs": {},
        "hemispheres": {},
    }
    for path in (forward, reverse):
        result["inputs"][path.name] = sha256(path)
    for h in "LR":
        surface = args.assets / f"tpl-fsLR_den-32k_hemi-{h}_midthickness.surf.gii"
        placed = args.placed_dir / f"world_{h}.npy"
        result["inputs"][surface.name] = sha256(surface)
        result["inputs"][f"{args.placed_dir.name}/{placed.name}"] = sha256(placed)
        vertices = nib.load(surface).darrays[0].data.astype(np.float64)
        y = np.load(placed)
        ok = np.all(np.isfinite(y), axis=1)
        error = np.linalg.norm(
            ants_map(y[ok], forward, f"placed-{h}") - vertices[ok], axis=1
        )
        ants_forward = ants_map(vertices, forward, f"fwd-{h}")
        ants_reverse = ants_map(vertices, reverse, f"rev-{h}")
        round_trip = ants_map(ants_forward, reverse, f"rt-{h}")
        result["hemispheres"][h] = {
            "vertices": int(len(vertices)),
            "placedVertices": int(ok.sum()),
            "antsForwardOfPlacedVsInputMm": stats(error),
            "antsVsSimpleItkMaxMm": {
                "forwardFile": float(
                    np.linalg.norm(
                        ants_forward - sitk_map(vertices, forward), axis=1
                    ).max()
                ),
                "reverseFile": float(
                    np.linalg.norm(
                        ants_reverse - sitk_map(vertices, reverse), axis=1
                    ).max()
                ),
            },
            "antsDisplacementMedianMm": {
                "forwardFile": float(
                    np.median(np.linalg.norm(ants_forward - vertices, axis=1))
                ),
                "reverseFile": float(
                    np.median(np.linalg.norm(ants_reverse - vertices, axis=1))
                ),
                "forwardVsReverseFile": float(
                    np.median(np.linalg.norm(ants_forward - ants_reverse, axis=1))
                ),
                "roundTripReverseOfForward": float(
                    np.median(np.linalg.norm(round_trip - vertices, axis=1))
                ),
            },
        }
    result["versions"] = {
        "SimpleITK": sitk.Version_VersionString(),
        "nibabel": nib.__version__,
        "numpy": np.__version__,
    }
    text = json.dumps(result, indent=2)
    if args.out:
        args.out.write_text(text + "\n")
    print(text)


if __name__ == "__main__":
    main()
