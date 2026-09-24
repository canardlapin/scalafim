#!/usr/bin/env python3
"""Surface resampling parity oracle (STP P7.04): neurotransform's surface_resampling_plan on two icospheres.

Writes the meshes (coarse reference, fine moving; different radii and a rotation) and, via Rscript, neurotransform
0.1.0's barycentric and nearest plans as 0-based triplets. Evidence kind: reference-implementation (neurotransform).

Run:
    uv run --with numpy python tools/transform/generate_surface_resampling_oracle.py
"""

from __future__ import annotations

import os
import subprocess
import sys

import numpy as np

sys.path.insert(0, os.path.dirname(__file__))
import oracle_common as oc  # noqa: E402

REPO = oc.REPO
OUT = os.path.join(
    REPO,
    "modules",
    "surface",
    "jvm",
    "src",
    "test",
    "resources",
    "scalafim",
    "surface",
    "resampling_oracle",
)
os.makedirs(OUT, exist_ok=True)


def icosphere(levels, radius, rotation=0.0):
    t = (1.0 + 5**0.5) / 2.0
    vertices = [
        (-1, t, 0),
        (1, t, 0),
        (-1, -t, 0),
        (1, -t, 0),
        (0, -1, t),
        (0, 1, t),
        (0, -1, -t),
        (0, 1, -t),
        (t, 0, -1),
        (t, 0, 1),
        (-t, 0, -1),
        (-t, 0, 1),
    ]
    vertices = [tuple(float(v) for v in p) for p in vertices]
    faces = [
        (0, 11, 5),
        (0, 5, 1),
        (0, 1, 7),
        (0, 7, 10),
        (0, 10, 11),
        (1, 5, 9),
        (5, 11, 4),
        (11, 10, 2),
        (10, 7, 6),
        (7, 1, 8),
        (3, 9, 4),
        (3, 4, 2),
        (3, 2, 6),
        (3, 6, 8),
        (3, 8, 9),
        (4, 9, 5),
        (2, 4, 11),
        (6, 2, 10),
        (8, 6, 7),
        (9, 8, 1),
    ]
    for _ in range(levels):
        mids = {}
        buf = list(vertices)

        def mid(a, b):
            key = (min(a, b), max(a, b))
            if key not in mids:
                p, q = buf[a], buf[b]
                buf.append(((p[0] + q[0]) / 2, (p[1] + q[1]) / 2, (p[2] + q[2]) / 2))
                mids[key] = len(buf) - 1
            return mids[key]

        new = []
        for a, b, c in faces:
            ab, bc, ca = mid(a, b), mid(b, c), mid(c, a)
            new += [(a, ab, ca), (b, bc, ab), (c, ca, bc), (ab, bc, ca)]
        faces, vertices = new, buf
    cz, sz, cx, sx = (
        np.cos(rotation),
        np.sin(rotation),
        np.cos(0.7 * rotation),
        np.sin(0.7 * rotation),
    )
    out = []
    for x, y, z in vertices:
        n = (x * x + y * y + z * z) ** 0.5
        x1, y1, z1 = x / n * radius, y / n * radius, z / n * radius
        x2, y2 = cz * x1 - sz * y1, sz * x1 + cz * y1
        out.append((x2, cx * y2 - sx * z1, sx * y2 + cx * z1))
    return np.array(out), np.array(faces)


for name, (v, f) in {
    "moving": icosphere(3, 87.0),
    "reference": icosphere(2, 101.0, 0.37),
}.items():
    np.savetxt(
        os.path.join(OUT, f"{name}_vertices.tsv"), v, delimiter="\t", fmt="%.17g"
    )
    np.savetxt(os.path.join(OUT, f"{name}_faces.tsv"), f, delimiter="\t", fmt="%d")

script = f"""
suppressMessages(library(neurotransform))
d <- "{OUT}"
rd <- function(n) as.matrix(read.table(file.path(d, n), sep = "\\t"))
mv <- rd("moving_vertices.tsv"); mf <- rd("moving_faces.tsv"); rv <- rd("reference_vertices.tsv"); rf <- rd("reference_faces.tsv")
mesh <- function(v, f) neurotransform:::surface_mesh(unname(v), unname(f))  # 0-based faces (min 0) stay 0-based
for (m in c("barycentric", "nearest")) {{
  p <- surface_resampling_plan(mesh(rv, rf), mesh(mv, mf), method = m)
  write.table(data.frame(row = p$rows - 1L, col = p$cols - 1L, val = sprintf("%.17g", p$vals)), file.path(d, paste0("plan_", m, ".tsv")), sep = "\\t", quote = FALSE, row.names = FALSE)
}}
cat(as.character(packageVersion("neurotransform")))
"""
version = subprocess.run(
    ["Rscript", "-e", script], capture_output=True, text=True, check=True
).stdout.strip()
oc.write_manifest(
    OUT,
    generator=__file__,
    kind="reference-implementation",
    tools={
        "neurotransform": version,
        "numpy": np.__version__,
        "R": subprocess.run(
            ["Rscript", "--version"], capture_output=True, text=True
        ).stderr.strip(),
    },
    commands=[
        "uv run --with numpy python tools/transform/generate_surface_resampling_oracle.py"
    ],
    notes="0-based triplets from neurotransform surface_resampling_plan(reference, moving, method) on icospheres (moving: level 3, r 87; reference: level 2, r 101, rotated).",
)
print("neurotransform", version)
