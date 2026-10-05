"""Seed derivation: SplitMix64 stream keys (root, fnv1a64(cell id), dataset index, purpose).

Definition (frozen for v0a; the protocol names the key tuple and SplitMix64 but not the
chaining, so this file is the normative chaining):

    s0 = root (uint64)
    s  = splitmix64(s ^ k)   for k in (fnv1a64(cell_id utf-8), dataset_index, purpose_index)
    stream seed = s

where splitmix64(x) is the SplitMix64 output function applied to state x (state += gamma,
then the two xor-shift-multiply rounds).  numpy's PCG64 is seeded from the stream seed via
SeedSequence.  Purposes, in index order: design, truth, noise, nullcal, audit, bootstrap, split.
"""
from __future__ import annotations

import hashlib

import numpy as np

M64 = (1 << 64) - 1
GAMMA = 0x9E3779B97F4A7C15
PURPOSES = ("design", "truth", "noise", "nullcal", "audit", "bootstrap", "split")

# Protocol section 8, "Denylisted seeds".
DENYLIST = frozenset(
    [7000930101, 7000930201, 7000930102, 7000930103, 101, 102, 103, 20260909, 20260910, 20260911, 11]
)

# Harness root: used by tests and smoke runs only.  Never a pilot or confirmatory root.
HARNESS_ROOT = 0x7A3C_91D5_0B44_E2F1  # arbitrary fixed constant; not derived from any beacon.


def splitmix64(x: int) -> int:
    z = (x + GAMMA) & M64
    z = ((z ^ (z >> 30)) * 0xBF58476D1CE4E5B9) & M64
    z = ((z ^ (z >> 27)) * 0x94D049BB133111EB) & M64
    return z ^ (z >> 31)


def fnv1a64(s: str) -> int:
    h = 0xCBF29CE484222325
    for b in s.encode("utf-8"):
        h = ((h ^ b) * 0x100000001B3) & M64
    return h


def root_from_sha256_hex(hex_digest: str) -> int:
    """Root = first 8 bytes (big-endian) of a SHA-256 digest, e.g. SHA-256(v0 hash || beacon A)."""
    return int.from_bytes(bytes.fromhex(hex_digest)[:8], "big")


def stream_seed(root: int, cell_id: str, dataset: int, purpose: str) -> int:
    if purpose not in PURPOSES:
        raise ValueError(f"unknown purpose {purpose!r}")
    s = root & M64
    for k in (fnv1a64(cell_id), int(dataset) & M64, PURPOSES.index(purpose)):
        s = splitmix64(s ^ k)
    return s


def denylist_hit(seed: int) -> bool:
    """True if the 64-bit seed, or its low 32 bits, is a denylisted value."""
    return seed in DENYLIST or (seed & 0xFFFFFFFF) in DENYLIST


def rng_for(root: int, cell_id: str, dataset: int, purpose: str) -> np.random.Generator:
    seed = stream_seed(root, cell_id, dataset, purpose)
    if denylist_hit(seed):
        raise RuntimeError(f"derived stream seed {seed} is denylisted")
    return np.random.Generator(np.random.PCG64(np.random.SeedSequence(seed)))


def sha256_hex(b: bytes) -> str:
    return hashlib.sha256(b).hexdigest()
