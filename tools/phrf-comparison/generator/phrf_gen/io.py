"""Deterministic .npz + JSON manifest output (see SCHEMA.md)."""
from __future__ import annotations

import hashlib
import io
import json
import os
import zipfile

import numpy as np
import scipy

from .generate import generate_dataset
from .seeds import DENYLIST, HARNESS_ROOT, denylist_hit, sha256_hex, stream_seed
from . import __version__

SCHEMA_VERSION = "phrf-gen-npz-1"
_FIXED_DATE = (1980, 1, 1, 0, 0, 0)


def npy_bytes(a: np.ndarray) -> bytes:
    buf = io.BytesIO()
    np.lib.format.write_array(buf, np.ascontiguousarray(a), version=(1, 0), allow_pickle=False)
    return buf.getvalue()


def npz_bytes(arrays: dict) -> bytes:
    """Byte-deterministic zip: sorted members, stored (no compression), fixed timestamps."""
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w", zipfile.ZIP_STORED) as z:
        for name in sorted(arrays):
            zi = zipfile.ZipInfo(name + ".npy", date_time=_FIXED_DATE)
            zi.compress_type = zipfile.ZIP_STORED
            zi.external_attr = 0o644 << 16
            z.writestr(zi, npy_bytes(arrays[name]))
    return buf.getvalue()


def code_sha256() -> str:
    d = os.path.dirname(os.path.abspath(__file__))
    h = hashlib.sha256()
    for fn in sorted(os.listdir(d)):
        if fn.endswith(".py"):
            h.update(fn.encode())
            h.update(open(os.path.join(d, fn), "rb").read())
    return h.hexdigest()


def build(cell, root: int, root_kind: str, dataset: int):
    """Return (npz_bytes, manifest_dict)."""
    out = generate_dataset(cell, root, dataset)
    blob = npz_bytes(out["arrays"])
    seeds = out["meta"]["streams"]
    manifest = dict(
        schema=SCHEMA_VERSION,
        generator_version=__version__,
        generator_code_sha256=code_sha256(),
        numpy=np.__version__,
        scipy=scipy.__version__,
        root_kind=root_kind,
        root_hex=f"{root:016x}",
        root_denylisted=denylist_hit(root),
        denylist=sorted(DENYLIST),
        stream_denylist_check={p: {"seed": s, "hit": denylist_hit(s)} for p, s in seeds.items()},
        file=f"{cell.cell_id}__d{dataset:04d}.npz",
        npz_sha256=sha256_hex(blob),
        arrays={
            k: dict(dtype=str(v.dtype), shape=list(v.shape), npy_sha256=sha256_hex(npy_bytes(v)))
            for k, v in sorted(out["arrays"].items())
        },
        **out["meta"],
    )
    return blob, manifest


def write_dataset(cell, root, root_kind, dataset, outdir):
    blob, man = build(cell, root, root_kind, dataset)
    os.makedirs(outdir, exist_ok=True)
    with open(os.path.join(outdir, man["file"]), "wb") as f:
        f.write(blob)
    mp = os.path.join(outdir, man["file"][:-4] + ".manifest.json")
    with open(mp, "w") as f:
        json.dump(man, f, indent=2, sort_keys=True)
        f.write("\n")
    return man
