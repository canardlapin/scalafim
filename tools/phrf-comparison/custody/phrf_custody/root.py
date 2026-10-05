"""Pilot root derivation and the denylist check (design 4.1, F10, F8; commit-reveal secret).

root   = SHA-256(v0 manifest hash bytes || beacon randomness bytes || custodian secret)  (32 bytes)
root64 = first 8 bytes of root, big-endian, computed by the frozen ``seeds.root_from_sha256_hex``.
The denylist rule is seeds.denylist_hit; ``seeds.py`` is loaded from the checkout and its SHA-256
must equal the value frozen in v0 (F8).
"""
from __future__ import annotations

import hashlib
import importlib.util
from pathlib import Path

SEEDS_PY = Path(__file__).resolve().parents[2] / "generator" / "phrf_gen" / "seeds.py"
PILOT_CELLS = ("C-TX-.5", "C-TS-1", "C-TS-.5", "C-TG-.5", "T-TX-fast", "T-TX-jit", "T-TS-fast")
PILOT_DATASETS_PER_CELL = 20  # 7 x 20 = 140 datasets, 7 purposes each


class SeedsMismatch(Exception):
    pass


class DenylistHit(Exception):
    pass


def seeds_sha256(path: Path = SEEDS_PY) -> str:
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def load_seeds(expected_sha256: str, path: Path = SEEDS_PY):
    """Load seeds.py only if its bytes hash to the value frozen in v0."""
    if seeds_sha256(path) != expected_sha256.lower():
        raise SeedsMismatch("seeds.py does not match the SHA-256 frozen in v0")
    spec = importlib.util.spec_from_file_location("phrf_seeds_frozen", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def derive_root(v0_hash_hex: str, randomness_hex: str, secret: bytes, *, seeds) -> tuple[str, int]:
    v0, rnd = bytes.fromhex(v0_hash_hex), bytes.fromhex(randomness_hex)
    if len(v0) != 32 or len(rnd) != 32 or len(secret) != 32:
        raise ValueError("v0 hash, beacon randomness and secret must each be 32 bytes")
    root = hashlib.sha256(v0 + rnd + secret).hexdigest()
    return root, seeds.root_from_sha256_hex(root)


def denylist_check(root64: int, *, seeds, cells=PILOT_CELLS, n_datasets=PILOT_DATASETS_PER_CELL) -> dict:
    """Check root64 and every stream seed.  Raises DenylistHit (abort; amend v0, never re-derive)."""
    if seeds.denylist_hit(root64):
        raise DenylistHit("root64 is denylisted")
    n = 0
    for cell in cells:
        for ds in range(n_datasets):
            for p in seeds.PURPOSES:
                n += 1
                if seeds.denylist_hit(seeds.stream_seed(root64, cell, ds, p)):
                    raise DenylistHit(f"a stream seed is denylisted ({cell}, dataset {ds}, {p})")
    return {"seeds_checked": n, "denylist_hits": 0, "seeds_py_sha256": seeds_sha256(Path(seeds.__file__))}
