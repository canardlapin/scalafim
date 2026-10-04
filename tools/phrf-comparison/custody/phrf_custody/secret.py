"""Commit-reveal custodian secret (owner decision on bd-01M3TNWW).

root = SHA-256(v0_hash || beacon_A || secret).  v0 commits only SHA-256(secret).  The secret is
generated in-process, sealed to the owner key, and never written to disk in plaintext; the owner
reveals it at unseal and ``verify_reveal`` checks it against the v0 commitment.
"""
from __future__ import annotations

import hashlib
import hmac
import secrets
from pathlib import Path

from . import seal

SECRET_NAME = "custodian-secret"


class RevealError(Exception):
    pass


def generate() -> bytes:
    return secrets.token_bytes(32)


def commitment(secret: bytes) -> str:
    return hashlib.sha256(secret).hexdigest()


def seal_secret(secret: bytes, dst: Path, recipient, *, expect_fingerprint: str) -> dict:
    """Seal the secret into a FRESH directory.  An existing directory is refused up front: a crash
    after ``append`` and before ``close`` followed by a rerun would otherwise add a second, different
    secret to the same store and make the reveal impossible."""
    if Path(dst).exists():
        raise seal.SealError("secret store directory already exists; use a fresh, nonexistent directory")
    store = seal.SealedStore(dst, recipient, expect_fingerprint=expect_fingerprint)
    store.append(SECRET_NAME, secret)
    return store.close()


def verify_reveal(secret: bytes, v0_commitment: str) -> None:
    if not isinstance(secret, bytes) or len(secret) != 32:
        raise RevealError("secret must be 32 bytes")
    if not hmac.compare_digest(commitment(secret), v0_commitment.lower()):
        raise RevealError("revealed secret does not match the v0 commitment")


def reveal(sealed_secret: Path, priv, v0_commitment: str, *, allow_partial: bool = False) -> bytes:
    """Owner side: decrypt the sealed secret and verify it against the v0 commitment.
    ``allow_partial`` opens a store whose close never happened (crash between append and close)."""
    items = dict(seal.read_blobs(sealed_secret, priv, allow_partial=allow_partial))
    if set(items) != {SECRET_NAME}:
        raise RevealError("sealed secret store has unexpected contents")
    verify_reveal(items[SECRET_NAME], v0_commitment)
    return items[SECRET_NAME]
