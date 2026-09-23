#!/usr/bin/env python3
"""Export a PLSNeuro result bundle's latent brain directions to NIfTI volumes.

Usage:
    python pls_bundle_to_nifti.py BUNDLE.pls-result BRAIN_MASK.nii.gz OUTDIR

BRAIN_MASK is TemplateFlow's tpl-MNI152NLin2009cAsym_res-02_desc-brain_mask on the
bundle's exact grid; it is used only to confirm the linear index order.

Writes OUTDIR/lv-<l>_coord-<k>_brain-direction.nii.gz (float64, NaN outside the
selection) on the bundle's own grid, one volume per latent variable l and feature
coordinate k (one coordinate for beta; one per FIR window), plus
OUTDIR/export.json with input/output digests. Feature rows come from
latent/features.bin, big-endian int32 (coordinate, absolute voxel) pairs.
The layout is self-checked rather than assumed:
- the selection holds big-endian int32 absolute voxel indices (the manifest says so);
- linear index order must be x-fastest (NIfTI/canonical order): under that order
  the selected voxels must lie inside the template brain mask (>= 0.95), and
  by >= 0.1 more than under z-fastest order;
- brain directions are f64 (rows = voxels, columns = latent variables); the
  byte order and row/column-major layout are chosen by requiring unit-norm
  columns (singular vectors).
"""
import hashlib
import json
import pathlib
import sys
import zipfile

import nibabel as nib
import numpy as np


def sha256(data):
    return hashlib.sha256(data).hexdigest()


def main():
    bundle, mask_path, out = (pathlib.Path(a) for a in sys.argv[1:4])
    out.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(bundle) as archive:
        manifest = json.loads(archive.read("manifest.json"))["result"]
        geometry = manifest["geometry"]
        selection_bytes = archive.read(geometry["selection"]["entry"])
        directions_entry = manifest["latent"]["brainDirections"]
        directions_bytes = archive.read(directions_entry["entry"])
        features_entry = manifest["latent"]["features"]
        features_bytes = archive.read(features_entry["entry"])

    if sha256(selection_bytes) != geometry["selection"]["sha256"]:
        raise SystemExit("selection digest mismatch")
    dims = tuple(geometry["dimensions"])
    affine = np.array(geometry["affine"], dtype=float).reshape(4, 4)
    selection = np.frombuffer(selection_bytes, dtype=">i4").astype(np.int64)
    if (
        selection.size != geometry["selection"]["length"]
        or selection.min() < 0
        or selection.max() >= np.prod(dims)
    ):
        raise SystemExit("selection out of grid range")

    mask_image = nib.load(mask_path)
    if mask_image.shape != dims or not np.allclose(mask_image.affine, affine):
        raise SystemExit("brain mask is not on the bundle grid")
    brain = np.asarray(mask_image.dataobj) > 0.5

    def inside_fraction(order):
        ijk = np.unravel_index(selection, dims, order=order)
        return float(brain[ijk].mean())

    inside_f, inside_c = inside_fraction("F"), inside_fraction("C")
    if not (inside_f >= 0.95 and inside_f - inside_c >= 0.1):
        raise SystemExit(f"cannot confirm x-fastest order (inside mask: F {inside_f:.3f}, C {inside_c:.3f})")

    if sha256(features_bytes) != features_entry["sha256"]:
        raise SystemExit("features digest mismatch")
    features = np.frombuffer(features_bytes, dtype=">i4").astype(np.int64).reshape(-1, 2)
    coordinates = features_entry["coordinates"]
    if features.shape[0] != features_entry["length"] or not np.isin(features[:, 1], selection).all():
        raise SystemExit("feature rows do not match the selection")
    if features[:, 0].min() < 0 or features[:, 0].max() >= len(coordinates):
        raise SystemExit("feature coordinate index out of range")

    rows, cols = directions_entry["rows"], directions_entry["columns"]
    if rows != features.shape[0]:
        raise SystemExit("brain-direction rows do not match features")
    candidates = {}
    for endian in (">", "<"):
        flat = np.frombuffer(directions_bytes, dtype=endian + "f8")
        for layout in ("row-major", "column-major"):
            matrix = (
                flat.reshape(rows, cols)
                if layout == "row-major"
                else flat.reshape(cols, rows).T
            )
            norms = np.linalg.norm(matrix, axis=0)
            if np.all(np.isfinite(norms)) and np.allclose(norms, 1.0, atol=1e-8):
                candidates[(endian, layout)] = matrix
    if len(candidates) != 1:
        raise SystemExit(
            f"brain-direction layout ambiguous or unknown: {sorted(candidates)}"
        )
    (endian, layout), matrix = next(iter(candidates.items()))

    outputs = {}
    for k in range(cols):
        for c in range(len(coordinates)):
            rows_c = features[:, 0] == c
            volume = np.full(int(np.prod(dims)), np.nan)
            volume[features[rows_c, 1]] = matrix[rows_c, k]
            image = nib.Nifti1Image(volume.reshape(dims, order="F"), affine)
            image.header.set_data_dtype(np.float64)
            image.set_sform(affine, code=4)  # 4 = MNI_152 (generic; exact frame recorded below)
            image.set_qform(affine, code=4)
            path = out / f"lv-{k + 1}_coord-{c + 1}_brain-direction.nii.gz"
            nib.save(image, path)
            outputs[path.name] = hashlib.sha256(path.read_bytes()).hexdigest()

    record = {
        "bundle": bundle.name,
        "bundleSha256": hashlib.sha256(bundle.read_bytes()).hexdigest(),
        "analysisSpace": geometry["analysisSpace"],
        "grid": geometry["grid"],
        "dimensions": list(dims),
        "affine": affine.ravel().tolist(),
        "selectionLength": int(selection.size),
        "featureCoordinates": coordinates,
        "linearOrder": "x-fastest (checked: inside brain mask F=%.4f vs C=%.4f)"
        % (inside_f, inside_c),
        "brainMaskSha256": hashlib.sha256(mask_path.read_bytes()).hexdigest(),
        "directionsLayout": {
            "byteOrder": "big" if endian == ">" else "little",
            "layout": layout,
            "check": "unit-norm columns",
        },
        "outputs": outputs,
    }
    (out / "export.json").write_text(json.dumps(record, indent=2) + "\n")
    print(json.dumps(record, indent=2))


if __name__ == "__main__":
    main()
