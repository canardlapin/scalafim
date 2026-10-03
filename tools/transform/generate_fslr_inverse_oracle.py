#!/usr/bin/env python3
"""All-vertex SimpleITK oracle for the fsLR 32k -> MNI152NLin2009cAsym inverse placement.

Independent of ScalaFIM and reframe4s: SimpleITK reads the original TemplateFlow
HDF5 composite and nibabel reads the GIFTI surfaces. For every fsLR 32k
midthickness vertex x (MNI152NLin6Asym, RAS mm) it solves T(y) = x, where
T = tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym is a 2009c -> 6Asym point map
evaluated by ITK in LPS. The method is the one recorded in the committed
fslr-inverse-oracle.json.gz (fixed-point y <- y + (x - T(y)) from y = x), run to
a residual tolerance instead of a fixed 12 iterations. Every vertex's residual,
iteration count and displacement-field support are recorded.

Before writing anything, the script reproduces the committed 2500-vertex-per-hemisphere
fixture. That fixture stopped after 12 iterations, so some of its solutions carry residuals
up to ~2.5e-5 mm; each committed vertex must agree within 4x its own recorded residual
plus 1e-9 mm (the fixed-point error bound for a contraction with constant <= 3/4).

Output (deterministic bytes): <out>/fslr-inverse-oracle-all.json and
<out>/fslr-inverse-oracle-all-{L,R}.f64le.gz, each a row-major float64
little-endian array of shape (32492, 6): solution x, y, z (RAS mm), residual
|T(y) - x| (mm), iterations, in-support (1.0 or 0.0).

Usage: generate_fslr_inverse_oracle.py --templateflow ~/.cache/templateflow --out DIR
"""

import argparse
import gzip
import hashlib
import io
import json
import pathlib
import sys

import nibabel as nib
import numpy as np
import SimpleITK as sitk

TRANSFORM = "tpl-MNI152NLin2009cAsym/tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5"
TRANSFORM_SHA256 = "2e3869a07b96aec406e0419ca2e434afc54882d37cc212b933b139d1b63a4dfe"
SURFACES = {
    "L": (
        "tpl-fsLR/tpl-fsLR_den-32k_hemi-L_midthickness.surf.gii",
        "036a8b6c84fa4b581b7ad7b36d99190b57ad6755c9d7ef7adc3e9ffc6448f1af",
    ),
    "R": (
        "tpl-fsLR/tpl-fsLR_den-32k_hemi-R_midthickness.surf.gii",
        "9d2cef05096c433b134870456abebe7ff201cdda60ce5741d7b794018eeccda7",
    ),
}
TOLERANCE_MM = 1e-10
MAX_ITERATIONS = 200
COMMITTED = (
    pathlib.Path(__file__).resolve().parents[2]
    / "modules/surface/jvm/src/test/resources/pointmap-real/fslr-inverse-oracle.json.gz"
)


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def locked(root, relative, digest):
    path = root / relative
    actual = sha256(path)
    if actual != digest:
        sys.exit(f"{path}: sha256 {actual}, locked {digest}")
    return path


def displacement_support(composite):
    """The displacement field transform inside the composite, for support checks."""
    for i in range(composite.GetNumberOfTransforms()):
        t = composite.GetNthTransform(i)
        if t.GetTransformEnum() == sitk.sitkDisplacementField:
            return sitk.DisplacementFieldTransform(t).GetDisplacementField()
    sys.exit("composite has no displacement field transform")


def solve(transform, field, ras):
    x = np.array([-ras[0], -ras[1], ras[2]])  # RAS -> LPS
    y = x.copy()
    residual = np.inf
    iterations = 0
    while iterations <= MAX_ITERATIONS:
        image = np.array(transform.TransformPoint(tuple(y)))
        residual = float(np.linalg.norm(image - x))
        if residual <= TOLERANCE_MM:
            break
        y = y + (x - image)
        iterations += 1
    index = field.TransformPhysicalPointToContinuousIndex(tuple(y))
    size = field.GetSize()
    inside = all(-0.5 <= c < n - 0.5 for c, n in zip(index, size))
    return np.array([-y[0], -y[1], y[2]]), residual, iterations, inside  # LPS -> RAS


def deterministic_gzip(raw):
    buffer = io.BytesIO()
    with gzip.GzipFile(fileobj=buffer, mode="wb", mtime=0, compresslevel=9) as f:
        f.write(raw)
    return buffer.getvalue()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--templateflow", type=pathlib.Path, required=True)
    parser.add_argument("--out", type=pathlib.Path, required=True)
    args = parser.parse_args()
    root = args.templateflow.expanduser()

    transform = sitk.ReadTransform(str(locked(root, TRANSFORM, TRANSFORM_SHA256)))
    field = displacement_support(sitk.CompositeTransform(transform))
    vertices = {
        h: nib.load(str(locked(root, rel, digest)))
        .agg_data("pointset")
        .astype(np.float64)
        for h, (rel, digest) in SURFACES.items()
    }

    # Reproduce the committed subsample before producing anything new.
    with gzip.open(COMMITTED, "rt") as f:
        committed = json.load(f)
    assert committed["transformSha256"] == TRANSFORM_SHA256
    worst = 0.0
    worst_ratio = 0.0
    for h, entry in committed["hemispheres"].items():
        assert entry["surfaceSha256"] == SURFACES[h][1]
        for v in entry["vertices"]:
            assert np.allclose(vertices[h][v["vertex"]], v["input"], atol=1e-4), f"{h} {v['vertex']} input differs"
            solution, _, _, _ = solve(transform, field, np.array(v["input"]))
            difference = float(np.linalg.norm(solution - np.array(v["solution"])))
            bound = 4.0 * v["residualMm"] + 1e-9
            if difference > bound:
                sys.exit(f"{h} vertex {v['vertex']}: differs from the committed oracle by {difference} mm "
                         f"(bound {bound} mm from its recorded residual)")
            worst = max(worst, difference)
            worst_ratio = max(worst_ratio, difference / bound)
    print(f"reproduces committed oracle: max difference {worst:.3g} mm, max difference/bound {worst_ratio:.3g}")

    args.out.mkdir(parents=True, exist_ok=True)
    manifest = {
        "schema": "scalafim.fslr-inverse-oracle-all/1",
        "description": "Every fsLR 32k midthickness vertex (MNI152NLin6Asym, RAS mm) solved for its "
        "MNI152NLin2009cAsym position by SimpleITK fixed-point iteration y <- y + (x - T(y)) from "
        "y = x, T = the TemplateFlow 2009c->6Asym composite evaluated in LPS, until |T(y) - x| <= "
        f"{TOLERANCE_MM} mm or {MAX_ITERATIONS} iterations.",
        "transform": {"path": TRANSFORM, "sha256": TRANSFORM_SHA256},
        "tool": {
            "SimpleITK": sitk.Version_VersionString(),
            "nibabel": nib.__version__,
            "numpy": np.__version__,
            "generator": "tools/transform/generate_fslr_inverse_oracle.py",
        },
        "toleranceMm": TOLERANCE_MM,
        "maxIterations": MAX_ITERATIONS,
        "columns": ["x", "y", "z", "residualMm", "iterations", "inSupport"],
        "reproducesCommittedSubsampleMm": worst,
        "reproducesCommittedSubsampleMaxBoundRatio": worst_ratio,
        "hemispheres": {},
    }
    for h, (rel, digest) in SURFACES.items():
        rows = np.empty((len(vertices[h]), 6), dtype="<f8")
        for i, ras in enumerate(vertices[h]):
            solution, residual, iterations, inside = solve(transform, field, ras)
            rows[i] = (*solution, residual, iterations, 1.0 if inside else 0.0)
        name = f"fslr-inverse-oracle-all-{h}.f64le.gz"
        data = deterministic_gzip(rows.tobytes())
        (args.out / name).write_bytes(data)
        unconverged = int(np.sum(rows[:, 3] > TOLERANCE_MM))
        manifest["hemispheres"][h] = {
            "surface": {"path": rel, "sha256": digest},
            "vertices": len(rows),
            "file": name,
            "sha256": hashlib.sha256(data).hexdigest(),
            "maxResidualMm": float(rows[:, 3].max()),
            "maxIterations": int(rows[:, 4].max()),
            "unconverged": unconverged,
            "outsideSupport": int(np.sum(rows[:, 5] == 0.0)),
        }
        print(h, json.dumps(manifest["hemispheres"][h]))
    (args.out / "fslr-inverse-oracle-all.json").write_text(
        json.dumps(manifest, indent=2) + "\n"
    )


if __name__ == "__main__":
    main()
