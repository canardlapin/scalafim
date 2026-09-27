#!/usr/bin/env python3
"""TemplateFlow MNI152NLin6Asym <-> MNI152NLin2009cAsym bridge oracle (STP P7, bead bd-01M37FQFRRF1TW2REJPWS8BRM8).

The two TemplateFlow composites are ~200 MB each and are never committed. This generator records, from the cached
files:

  * xfm.tsv: file name, SHA-256 and byte size of each composite, and the ITK layout of its components.
  * pull_points.tsv: ITK's own TransformPoint (SimpleITK ReadTransform) of
    tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym at MNI152NLin2009cAsym points: brain-extent landmarks of the
    2009c res-01 brain mask (centroid and the six extreme voxels) plus asymmetric interior lattice points. Columns are
    RAS millimetres (ITK's LPS converted once). TransformPoint maps a fixed (2009c) point to the moving (6Asym) point.
  * push_points.tsv: MNI152NLin6Asym points (6Asym res-01 brain-mask landmarks and asymmetric interior points) with the
    2009c point q solving TransformPoint(q) = p, found by fixed-point iteration on ITK's TransformPoint; `residual`
    is |TransformPoint(q) - p| in mm. This is the forward (6Asym -> 2009c) map ITK implies, the target of a numerical
    inverse.
  * reverse_points.tsv: TransformPoint of tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym at the same 2009c points.
  * direction.tsv: image evidence for which way each file pulls. Each file is used under both hypotheses (it pulls
    2009c -> 6Asym, i.e. resamples 6Asym data onto the 2009c grid; or it pulls 6Asym -> 2009c) with linear
    interpolation, and the warped TemplateFlow brain T1w / brain mask is compared with the other template's
    (Pearson r over the target grid, Dice of masks at 0.5). The identity transform is the baseline.

Evidence kind: native-oracle (ITK TransformPoint and resampling through SimpleITK).

Run (the files resolve through the TemplateFlow cache, TEMPLATEFLOW_HOME or the platform default):
    uv run --with SimpleITK==2.5.6 --with h5py==3.16.0 --with nibabel==5.3.2 --with numpy \
        python tools/transform/generate_templateflow_mni_bridge_oracle.py
"""

from __future__ import annotations

import os
import sys

import h5py
import nibabel as nib
import numpy as np
import SimpleITK as sitk

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

OUT = os.path.join(
    oc.REPO,
    "modules",
    "atlas",
    "jvm",
    "src",
    "test",
    "resources",
    "scalafim",
    "atlas",
    "oracle",
    "templateflow_mni_bridge",
)
os.makedirs(OUT, exist_ok=True)


def templateflow_roots():
    roots = []
    if os.environ.get("TEMPLATEFLOW_HOME"):
        roots.append(os.environ["TEMPLATEFLOW_HOME"])
    roots += [
        os.path.expanduser("~/.cache/templateflow"),
        os.path.expanduser("~/Library/Caches/templateflow"),
    ]
    return roots


def locate(template: str, name: str) -> str:
    for root in templateflow_roots():
        path = os.path.join(root, f"tpl-{template}", name)
        if os.path.exists(path) and os.path.getsize(path) > 0:
            return path
    raise SystemExit(f"missing TemplateFlow asset tpl-{template}/{name}")


FORWARD = locate(
    "MNI152NLin2009cAsym",
    "tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5",
)
REVERSE = locate(
    "MNI152NLin6Asym",
    "tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym_mode-image_xfm.h5",
)
IMAGES = {
    "2009c_T1w": locate(
        "MNI152NLin2009cAsym", "tpl-MNI152NLin2009cAsym_res-01_desc-brain_T1w.nii.gz"
    ),
    "2009c_mask": locate(
        "MNI152NLin2009cAsym", "tpl-MNI152NLin2009cAsym_res-01_desc-brain_mask.nii.gz"
    ),
    "6Asym_T1w": locate(
        "MNI152NLin6Asym", "tpl-MNI152NLin6Asym_res-01_desc-brain_T1w.nii.gz"
    ),
    "6Asym_mask": locate(
        "MNI152NLin6Asym", "tpl-MNI152NLin6Asym_res-01_desc-brain_mask.nii.gz"
    ),
}


def lps(p):
    return (-float(p[0]), -float(p[1]), float(p[2]))


ras = lps  # the flip is its own inverse

# ---- the composites, as stored -------------------------------------------------------------------------------
xfm_rows = []
for path in [FORWARD, REVERSE]:
    with h5py.File(path, "r") as h5:
        group = h5["TransformGroup"]
        layout = []
        for index in sorted(group.keys(), key=int):
            kind = group[index]["TransformType"][()]
            kind = kind[0] if hasattr(kind, "__len__") else kind
            kind = kind.decode() if isinstance(kind, bytes) else str(kind)
            layout.append(kind)
        fixed = np.asarray(group["2"]["TranformFixedParameters"]).ravel()
    xfm_rows.append(
        [
            os.path.basename(path),
            oc.sha256_file(path),
            str(os.path.getsize(path)),
            ",".join(layout),
            " ".join(repr(float(v)) for v in fixed),
        ]
    )
with open(os.path.join(OUT, "xfm.tsv"), "w") as handle:
    handle.write("file\tsha256\tbytes\tcomponents\tfield_fixed_parameters\n")
    for row in xfm_rows:
        handle.write("\t".join(row) + "\n")

forward = sitk.ReadTransform(FORWARD)
reverse = sitk.ReadTransform(REVERSE)


# ---- sample points -------------------------------------------------------------------------------------------
def mask_landmarks(path: str, prefix: str):
    image = nib.load(path)
    data = np.asarray(image.dataobj) > 0.5
    ijk = np.argwhere(data)
    affine = image.affine
    world = ijk @ affine[:3, :3].T + affine[:3, 3]
    out = [(f"{prefix}-centroid", tuple(world.mean(axis=0)))]
    for axis, name in enumerate(["x", "y", "z"]):
        for end, pick in [("min", np.min), ("max", np.max)]:
            extreme = pick(world[:, axis])
            on_face = world[world[:, axis] == extreme]
            # the face voxel nearest the face's centroid: a definite, reproducible surface landmark
            centre = on_face.mean(axis=0)
            chosen = on_face[np.argmin(np.linalg.norm(on_face - centre, axis=1))]
            out.append((f"{prefix}-{name}{end}", tuple(chosen)))
    return out, affine, image.shape


def interior_points(affine, shape, prefix: str, seed: int):
    points = oc.asymmetric_points(shape, count=16, seed=seed)
    return [
        (
            f"{prefix}-lattice{i:02d}",
            tuple(affine[:3, :3] @ np.asarray(p) + affine[:3, 3]),
        )
        for i, p in enumerate(points)
    ]


landmarks_2009c, affine_2009c, shape_2009c = mask_landmarks(
    IMAGES["2009c_mask"], "2009c"
)
points_2009c = landmarks_2009c + interior_points(
    affine_2009c, shape_2009c, "2009c", 7001
)
landmarks_6, affine_6, shape_6 = mask_landmarks(IMAGES["6Asym_mask"], "6Asym")
points_6 = landmarks_6 + interior_points(affine_6, shape_6, "6Asym", 7002)

# ---- pullback oracle: 2009c -> 6Asym ---------------------------------------------------------------------------
header = ["key", "x", "y", "z", "tx", "ty", "tz"]
oc.write_table(
    os.path.join(OUT, "pull_points.tsv"),
    header,
    [[k, *p, *ras(forward.TransformPoint(lps(p)))] for k, p in points_2009c],
)
oc.write_table(
    os.path.join(OUT, "reverse_points.tsv"),
    header,
    [[k, *p, *ras(reverse.TransformPoint(lps(p)))] for k, p in points_2009c],
)


# ---- forward (push) oracle: 6Asym -> 2009c, by inverting ITK's own TransformPoint ------------------------------
def invert(p):
    target = np.asarray(lps(p))
    q = target.copy()
    for _ in range(200):
        step = np.asarray(forward.TransformPoint(tuple(q))) - target
        q = q - step
        if np.linalg.norm(step) < 1e-12:
            break
    residual = float(
        np.linalg.norm(np.asarray(forward.TransformPoint(tuple(q))) - target)
    )
    return ras(q), residual


push_rows = []
for key, p in points_6:
    q, residual = invert(p)
    if residual > 1e-9:
        raise SystemExit(
            f"fixed-point inverse did not converge at {key}: residual {residual}"
        )
    push_rows.append([key, *p, *q, residual])
oc.write_table(os.path.join(OUT, "push_points.tsv"), header + ["residual"], push_rows)

# ---- image evidence of direction ---------------------------------------------------------------------------------
images = {k: sitk.ReadImage(v, sitk.sitkFloat64) for k, v in IMAGES.items()}
identity = sitk.Transform(3, sitk.sitkIdentity)


def correlation(a, b):
    x = sitk.GetArrayViewFromImage(a).ravel()
    y = sitk.GetArrayViewFromImage(b).ravel()
    return float(np.corrcoef(x, y)[0, 1])


def dice(a, b):
    x = sitk.GetArrayViewFromImage(a).ravel() > 0.5
    y = sitk.GetArrayViewFromImage(b).ravel() > 0.5
    return float(2 * (x & y).sum() / (x.sum() + y.sum()))


def warp(moving, reference, transform):
    return sitk.Resample(moving, reference, transform, sitk.sitkLinear, 0.0)


direction_rows = []
for name, transform in [
    ("identity", identity),
    (os.path.basename(FORWARD), forward),
    (os.path.basename(REVERSE), reverse),
]:
    for hypothesis, moving, fixed in [
        ("pulls-2009c-to-6Asym", "6Asym", "2009c"),
        ("pulls-6Asym-to-2009c", "2009c", "6Asym"),
    ]:
        r = correlation(
            warp(images[f"{moving}_T1w"], images[f"{fixed}_T1w"], transform),
            images[f"{fixed}_T1w"],
        )
        d = dice(
            warp(images[f"{moving}_mask"], images[f"{fixed}_mask"], transform),
            images[f"{fixed}_mask"],
        )
        direction_rows.append([name, hypothesis, r, d])
oc.write_table(
    os.path.join(OUT, "direction.tsv"),
    ["transform", "hypothesis", "t1w_pearson_r", "mask_dice"],
    direction_rows,
)

oc.write_manifest(
    OUT,
    generator=__file__,
    kind="native-oracle",
    tools={
        "SimpleITK": sitk.Version_VersionString(),
        "h5py": h5py.__version__,
        "nibabel": nib.__version__,
        "numpy": np.__version__,
    },
    commands=[
        "uv run --with SimpleITK==2.5.6 --with h5py==3.16.0 --with nibabel==5.3.2 --with numpy "
        "python tools/transform/generate_templateflow_mni_bridge_oracle.py"
    ],
    notes=(
        "Inputs are TemplateFlow cache files (not committed); their SHA-256 are in xfm.tsv and inputs below. "
        "Points are RAS mm; tx,ty,tz = ITK TransformPoint (fixed -> moving) or its fixed-point inverse (push). "
        "inputs: "
        + ", ".join(
            f"{os.path.basename(v)}={oc.sha256_file(v)}" for v in IMAGES.values()
        )
    ),
)
for row in direction_rows:
    print(row)
print(len(points_2009c), "pull points,", len(push_rows), "push points")
