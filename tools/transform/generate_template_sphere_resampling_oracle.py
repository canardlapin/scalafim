#!/usr/bin/env python3
"""Template sphere resampling oracle: Connectome Workbench `-metric-resample BARYCENTRIC` (STP P7.04).

Pins the TemplateFlow registration spheres ScalaFIM resamples between, and records Workbench's own resampling of
three fields between them, on a seeded subset of target vertices (the full outputs are 32k-164k rows per case and are
not committed).

  * spheres.tsv: every TemplateFlow sphere asset ScalaFIM admits (fsaverage 10k/41k spheres, the fsaverage 164k
    `desc-std` sphere, fsLR 32k/59k/164k on the fsLR sphere and deformed to fsaverage `space-fsaverage`, both
    hemispheres): relative path, SHA-256, vertex and face counts.
  * <case>.tsv: target vertex, then Workbench's output for each field. Fields are functions of the source vertex
    index and the source file's own (float32) coordinates, rounded to float32 as Workbench reads them:
      hash   = ((i * 2654435761) mod 2^32 >> 8) / 2^24     (seeded values in [0, 1))
      ramp   = x / 100
      saddle = y * z / 10000

Cases (current sphere -> new sphere, as Workbench names them):
  L/R fsaverage 164k (desc-std) -> fsLR 32k space-fsaverage; L fsLR 32k space-fsaverage -> fsaverage 164k;
  L fsLR 32k -> fsLR 164k on the fsLR sphere; L fsaverage 164k (desc-std) -> fsaverage 10k.

Evidence kind: native-oracle (Workbench itself).

Budgets, fixed before ScalaFIM's plans were compared (ScalaFIM casts the radial ray, Workbench takes the closest
point, and Workbench works in float32): ramp and saddle within 5e-5 at every sampled target; hash within 5e-5 at 99%
of them and 1e-3 at all.

Run (Workbench 2.2.1; the spheres resolve through the TemplateFlow cache):
    WB_COMMAND=/path/to/wb_command uv run --with nibabel==5.3.2 --with numpy \
        python tools/transform/generate_template_sphere_resampling_oracle.py
"""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile

import nibabel as nib
import numpy as np

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

OUT = os.path.join(
    oc.REPO,
    "modules",
    "surface",
    "jvm",
    "src",
    "test",
    "resources",
    "scalafim",
    "surface",
    "template_sphere_oracle",
)
os.makedirs(OUT, exist_ok=True)
WB = os.environ.get("WB_COMMAND", "wb_command")
SUBSET = 1000


def templateflow_roots():
    roots = []
    if os.environ.get("TEMPLATEFLOW_HOME"):
        roots.append(os.environ["TEMPLATEFLOW_HOME"])
    roots += [
        os.path.expanduser("~/.cache/templateflow"),
        os.path.expanduser("~/Library/Caches/templateflow"),
    ]
    return roots


def locate(relative: str) -> str:
    for root in templateflow_roots():
        path = os.path.join(root, relative)
        if os.path.exists(path) and os.path.getsize(path) > 0:
            return path
    raise SystemExit(f"missing TemplateFlow asset {relative}")


def sphere_paths():
    out = []
    for hemi in "LR":
        out.append(f"tpl-fsaverage/tpl-fsaverage_hemi-{hemi}_den-10k_sphere.surf.gii")
        out.append(f"tpl-fsaverage/tpl-fsaverage_hemi-{hemi}_den-41k_sphere.surf.gii")
        out.append(
            f"tpl-fsaverage/tpl-fsaverage_hemi-{hemi}_den-164k_desc-std_sphere.surf.gii"
        )
        for den in ["32k", "59k", "164k"]:
            out.append(f"tpl-fsLR/tpl-fsLR_hemi-{hemi}_den-{den}_sphere.surf.gii")
            out.append(
                f"tpl-fsLR/tpl-fsLR_space-fsaverage_hemi-{hemi}_den-{den}_sphere.surf.gii"
            )
    return out


def load(path):
    image = nib.load(path)
    coords = image.agg_data("NIFTI_INTENT_POINTSET").astype(np.float64)
    faces = image.agg_data("NIFTI_INTENT_TRIANGLE")
    return coords, faces


rows = []
for relative in sphere_paths():
    path = locate(relative)
    coords, faces = load(path)
    rows.append(
        [relative, oc.sha256_file(path), str(coords.shape[0]), str(faces.shape[0])]
    )
with open(os.path.join(OUT, "spheres.tsv"), "w") as handle:
    handle.write("path\tsha256\tvertices\tfaces\n")
    for row in rows:
        handle.write("\t".join(row) + "\n")


def fields(coords):
    n = coords.shape[0]
    index = np.arange(n, dtype=np.uint64)
    hashed = ((index * np.uint64(2654435761)) % np.uint64(2**32)) >> np.uint64(8)
    return {
        "hash": (hashed.astype(np.float64) / float(2**24)).astype(np.float32),
        "ramp": (coords[:, 0] / 100.0).astype(np.float32),
        "saddle": (coords[:, 1] * coords[:, 2] / 10000.0).astype(np.float32),
    }


def std(hemi):
    return f"tpl-fsaverage/tpl-fsaverage_hemi-{hemi}_den-164k_desc-std_sphere.surf.gii"


def fslr(hemi, den, on_fsaverage):
    space = "space-fsaverage_" if on_fsaverage else ""
    return f"tpl-fsLR/tpl-fsLR_{space}hemi-{hemi}_den-{den}_sphere.surf.gii"


CASES = [
    ("L_fsaverage164k_to_fsLR32k", std("L"), fslr("L", "32k", True), 11),
    ("R_fsaverage164k_to_fsLR32k", std("R"), fslr("R", "32k", True), 12),
    ("L_fsLR32k_to_fsaverage164k", fslr("L", "32k", True), std("L"), 13),
    ("L_fsLR32k_to_fsLR164k", fslr("L", "32k", False), fslr("L", "164k", False), 14),
    (
        "L_fsaverage164k_to_fsaverage10k",
        std("L"),
        "tpl-fsaverage/tpl-fsaverage_hemi-L_den-10k_sphere.surf.gii",
        15,
    ),
]

commands = []
with tempfile.TemporaryDirectory() as work:
    for name, current, new, seed in CASES:
        coords, _ = load(locate(current))
        values = fields(coords)
        names = list(values)
        metric = nib.gifti.GiftiImage(
            darrays=[
                nib.gifti.GiftiDataArray(
                    values[k], intent="NIFTI_INTENT_NONE", datatype="NIFTI_TYPE_FLOAT32"
                )
                for k in names
            ]
        )
        source = os.path.join(work, f"{name}.in.func.gii")
        target = os.path.join(work, f"{name}.out.func.gii")
        nib.save(metric, source)
        argv = [
            WB,
            "-metric-resample",
            source,
            locate(current),
            locate(new),
            "BARYCENTRIC",
            target,
        ]
        subprocess.run(argv, check=True, capture_output=True)
        commands.append(
            f"wb_command -metric-resample <fields> {current} {new} BARYCENTRIC <out>"
        )
        out = nib.load(target)
        resampled = [np.asarray(d.data, dtype=np.float64) for d in out.darrays]
        n = resampled[0].shape[0]
        rng = np.random.default_rng(seed)
        chosen = np.unique(
            np.concatenate([[0, n - 1], rng.choice(n, size=SUBSET, replace=False)])
        )
        oc.write_table(
            os.path.join(OUT, f"{name}.tsv"),
            ["vertex", *names],
            [[str(int(v)), *[r[v] for r in resampled]] for v in chosen],
        )
        print(name, n, "targets,", len(chosen), "recorded")

version = oc.tool_version([WB, "-version"]).splitlines()
oc.write_manifest(
    OUT,
    generator=__file__,
    kind="native-oracle",
    tools={
        "wb_command": " ".join(version[:3]),
        "wb_command_sha256": oc.sha256_file(WB) if os.path.exists(WB) else "unknown",
        "nibabel": nib.__version__,
        "numpy": np.__version__,
    },
    commands=[
        "WB_COMMAND=... uv run --with nibabel==5.3.2 --with numpy "
        "python tools/transform/generate_template_sphere_resampling_oracle.py",
        *commands,
    ],
    notes=(
        "Sphere assets are TemplateFlow cache files (not committed), pinned in spheres.tsv. Rows are a seeded subset "
        f"of {SUBSET} target vertices plus the first and last; fields: hash, ramp = x/100, saddle = y*z/10000 of the "
        "current sphere's file coordinates, float32."
    ),
)
