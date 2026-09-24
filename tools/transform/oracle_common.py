"""Shared helpers for ScalaFIM transform oracle generators.

Every generator writes one directory under
modules/transform/shared/src/test/resources/scalafim/transform/oracle/<set>/ containing its fixtures and a
manifest.json produced by `write_manifest`. Tests on the JVM and in Scala.js read the same files
(see OracleFixtures.scala), so nothing here emits Scala source.

Run Python generators through uv so their dependencies are pinned per invocation, e.g.
    uv run --with nibabel==5.3.2 --with numpy python tools/transform/generate_convention_oracle.py
"""

from __future__ import annotations

import datetime as _dt
import hashlib
import json
import os
import platform
import subprocess
import sys
from typing import Iterable, Sequence

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
ORACLE_ROOT = os.path.join(
    REPO,
    "modules",
    "transform",
    "shared",
    "src",
    "test",
    "resources",
    "scalafim",
    "transform",
    "oracle",
)


def oracle_dir(name: str) -> str:
    path = os.path.join(ORACLE_ROOT, name)
    os.makedirs(path, exist_ok=True)
    return path


def sha256_file(path: str) -> str:
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(1 << 16), b""):
            digest.update(chunk)
    return digest.hexdigest()


def write_manifest(
    directory: str,
    *,
    generator: str,
    tools: dict,
    commands: Sequence[str],
    notes: str = "",
    kind: str = "native-oracle",
) -> str:
    """Record what produced the fixtures and hash every file in `directory` (except the manifest itself).

    kind: "native-oracle" (output of the reference tool itself), "reference-implementation" (an independent
    implementation of the tool's documented convention, e.g. nibabel/fslpy), or "cross-implementation".
    """
    files = {}
    for root, _, names in os.walk(directory):
        for name in sorted(names):
            if name == "manifest.json":
                continue
            path = os.path.join(root, name)
            files[os.path.relpath(path, directory)] = sha256_file(path)
    manifest = {
        "generator": os.path.relpath(generator, REPO),
        "generated": _dt.datetime.now(_dt.timezone.utc).strftime("%Y-%m-%d"),
        "kind": kind,
        "tools": tools,
        "python": sys.version.split()[0],
        "platform": platform.platform(),
        "commands": list(commands),
        "notes": notes,
        "sha256": dict(sorted(files.items())),
    }
    path = os.path.join(directory, "manifest.json")
    with open(path, "w") as handle:
        json.dump(manifest, handle, indent=1)
        handle.write("\n")
    return path


def write_table(
    path: str, header: Sequence[str], rows: Iterable[Sequence[float]]
) -> None:
    """Tab-separated table with full-precision floats (repr round-trips exactly into Scala's Double parser)."""
    with open(path, "w") as handle:
        handle.write("\t".join(header) + "\n")
        for row in rows:
            handle.write(
                "\t".join(repr(float(v)) if not isinstance(v, str) else v for v in row)
                + "\n"
            )


def asymmetric_points(extent: Sequence[int], count: int = 12, seed: int = 20260924):
    """Continuous voxel positions that break symmetry: off-centre, distinct per axis, including sub-voxel offsets
    and points near (but inside) every face. Deterministic for a given seed."""
    import random

    rng = random.Random(seed)
    nx, ny, nz = extent
    points = [
        (0.25, 0.5, 0.75),
        (nx - 1.3, 0.4, nz / 3.0 + 0.1),
        (nx / 5.0 + 0.37, ny - 1.2, 0.6),
        (0.9, ny / 3.0 + 0.21, nz - 1.4),
    ]
    while len(points) < count:
        points.append(
            (
                rng.uniform(0.1, nx - 1.1),
                rng.uniform(0.1, ny - 1.1),
                rng.uniform(0.1, nz - 1.1),
            )
        )
    return points


def tool_version(command: Sequence[str]) -> str:
    try:
        return (
            subprocess.run(
                command, capture_output=True, text=True, timeout=60
            ).stdout.strip()
            or "unknown"
        )
    except (OSError, subprocess.TimeoutExpired):
        return "unavailable"
