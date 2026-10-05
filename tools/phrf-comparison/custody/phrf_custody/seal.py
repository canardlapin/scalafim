"""Sealed store: write-time encryption of raw outputs to an owner-held public key.

Normative format: docs/plans/phrf-pilot-sealed-format.md (phrf-sealed/1).  Every blob is
self-contained (fresh ephemeral X25519 key per blob), so writers are stateless, may run in
parallel, and may resume after a crash without any key material.  Only the public key is handled
here; the private key is used by ``unseal``/``read_blobs`` (owner, off this machine; tests with
throwaway keys).
"""
from __future__ import annotations

import hashlib
import json
import os
import re
import unicodedata
from pathlib import Path

from cryptography.exceptions import InvalidTag
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric.x25519 import X25519PrivateKey, X25519PublicKey
from cryptography.hazmat.primitives.ciphers.aead import AESGCM
from cryptography.hazmat.primitives.kdf.hkdf import HKDF

FORMAT = "phrf-sealed/1"
MAGIC = b"PHRFSB\x01\x00"
HEADER_LEN = 60  # magic 8 | eph_pub 32 | recipient_id 8 | nonce 12
INFO = b"phrf-sealed/1 blob"
KIND_DATA, KIND_CLOSE = 0, 1
CLOSE_NAME = "CLOSE"
BUCKET_MIN = 4096
GIB = 1 << 30
SEALED_EXCLUDE = ("SEALED",)
TMP_SUFFIX = (".tmp",)  # stale writer temp files: ignored by readers and the digest, removed at close
MAX_NAME = 1024


class SealError(Exception):
    pass


def _raw_pub(pub: X25519PublicKey) -> bytes:
    return pub.public_bytes(serialization.Encoding.Raw, serialization.PublicFormat.Raw)


def load_public_key(data: bytes) -> X25519PublicKey:
    """PEM (SubjectPublicKeyInfo) or 64 hex chars of the raw 32-byte key."""
    s = data.strip()
    if s.startswith(b"-----BEGIN"):
        k = serialization.load_pem_public_key(s)
        if not isinstance(k, X25519PublicKey):
            raise SealError("recipient key is not X25519")
        return k
    if re.fullmatch(rb"[0-9a-fA-F]{64}", s):
        return X25519PublicKey.from_public_bytes(bytes.fromhex(s.decode()))
    raise SealError("unrecognised public key format (want X25519 PEM or 64 hex chars)")


def fingerprint(pub: X25519PublicKey) -> str:
    """SHA-256 of the raw key, 16 groups of 4 hex chars (confirmed out-of-band with the owner)."""
    h = hashlib.sha256(_raw_pub(pub)).hexdigest()
    return " ".join(h[i : i + 4] for i in range(0, 64, 4))


def check_fingerprint(pub: X25519PublicKey, expected: str) -> None:
    if fingerprint(pub).replace(" ", "") != str(expected).replace(" ", "").lower():
        raise SealError("recipient key fingerprint does not match the owner-confirmed fingerprint")


def recipient_id(pub: X25519PublicKey) -> bytes:
    return hashlib.sha256(_raw_pub(pub)).digest()[:8]


def safe_name(name: str) -> str:
    """Logical blob names are relative, normalised, '/'-separated and free of traversal."""
    if not isinstance(name, str) or not name or len(name.encode("utf-8")) > MAX_NAME:
        raise SealError("bad blob name")
    if "\0" in name or "\\" in name or name.startswith("/"):
        raise SealError("bad blob name")
    if any(c in ("", ".", "..") for c in name.split("/")):
        raise SealError("bad blob name")
    return name


def bucket_size(n: int) -> int:
    """Padded envelope length: 4 KiB minimum, next power of two up to 1 GiB, then GiB multiples."""
    if n <= BUCKET_MIN:
        return BUCKET_MIN
    if n <= GIB:
        return 1 << (n - 1).bit_length()
    return -(-n // GIB) * GIB


def _envelope(kind: int, name: str, data: bytes) -> bytes:
    nb = safe_name(name).encode("utf-8")
    env = bytes([kind]) + len(nb).to_bytes(2, "big") + nb + len(data).to_bytes(8, "big") + data
    return env + b"\0" * (bucket_size(len(env)) - len(env))


def _parse_envelope(pt: bytes) -> tuple[int, str, bytes]:
    if len(pt) < BUCKET_MIN:
        raise SealError("bad envelope")
    kind = pt[0]
    nl = int.from_bytes(pt[1:3], "big")
    dl_off = 3 + nl
    dl = int.from_bytes(pt[dl_off : dl_off + 8], "big")
    end = dl_off + 8 + dl
    if kind not in (KIND_DATA, KIND_CLOSE) or end > len(pt) or len(pt) != bucket_size(end):
        raise SealError("bad envelope")
    if any(pt[end:]):
        raise SealError("nonzero padding")
    return kind, safe_name(pt[3:dl_off].decode("utf-8")), pt[dl_off + 8 : end]


def _exchange(priv: X25519PrivateKey, pub: X25519PublicKey) -> bytes:
    """X25519 with low-order or zero points reported as SealError (the library raises ValueError)."""
    try:
        return priv.exchange(pub)
    except ValueError as e:
        raise SealError("invalid X25519 key (low-order point or zero shared secret)") from e


def _aead(shared: bytes, rec_raw: bytes, eph_raw: bytes) -> AESGCM:
    return AESGCM(HKDF(hashes.SHA256(), 32, None, INFO + rec_raw + eph_raw).derive(shared))


def encrypt_blob(recipient: X25519PublicKey, kind: int, name: str, data: bytes, *,
                 eph_priv: X25519PrivateKey | None = None, nonce: bytes | None = None) -> bytes:
    """One blob.  ``eph_priv``/``nonce`` are injectable for golden vectors only."""
    eph = eph_priv or X25519PrivateKey.generate()
    nonce = os.urandom(12) if nonce is None else nonce
    if len(nonce) != 12:
        raise SealError("nonce must be 12 bytes")
    eph_raw, rec_raw = _raw_pub(eph.public_key()), _raw_pub(recipient)
    header = MAGIC + eph_raw + recipient_id(recipient) + nonce
    ct = _aead(_exchange(eph, recipient), rec_raw, eph_raw).encrypt(nonce, _envelope(kind, name, data), header)
    return header + ct


def decrypt_blob(priv: X25519PrivateKey, blob: bytes) -> tuple[int, str, bytes]:
    if len(blob) < HEADER_LEN + BUCKET_MIN + 16 or blob[:8] != MAGIC:
        raise SealError("not a phrf-sealed/1 blob")
    header, ct = blob[:HEADER_LEN], blob[HEADER_LEN:]
    eph_raw, rid, nonce = header[8:40], header[40:48], header[48:60]
    pub = priv.public_key()
    if rid != recipient_id(pub):
        raise SealError("sealed to a different recipient key")
    try:
        pt = _aead(_exchange(priv, X25519PublicKey.from_public_bytes(eph_raw)), _raw_pub(pub), eph_raw).decrypt(
            nonce, ct, header)
    except InvalidTag as e:
        raise SealError("authentication failed: wrong key or tampered data") from e
    return _parse_envelope(pt)


# ---------------------------------------------------------------- tree digests

def _files(root: Path, exclude=(), exclude_suffix=()):
    out = []
    for dp, dn, fn in os.walk(root, followlinks=False):
        for n in dn + fn:
            if (Path(dp) / n).is_symlink():
                raise SealError(f"symlink in tree: {Path(dp) / n}")
        for n in fn:
            p = Path(dp) / n
            if p.relative_to(root).as_posix() in exclude or n.endswith(tuple(exclude_suffix)):
                continue
            out.append((p.relative_to(root).as_posix(), p))
    return sorted(out)


def _sha256_file(p: Path) -> str:
    h = hashlib.sha256()
    with open(p, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def tree_listing(root: Path, exclude=(), exclude_suffix=()) -> str:
    return "".join(f"{_sha256_file(p)}  {rel}\n" for rel, p in _files(Path(root), exclude, exclude_suffix))


def tree_digest(root: Path, exclude=(), exclude_suffix=()) -> str:
    """SHA-256 of the sorted sha256sum-style listing (the `sealed.sha256` of design 4.3)."""
    return hashlib.sha256(tree_listing(root, exclude, exclude_suffix).encode()).hexdigest()


def verify_readback(recipient: X25519PublicKey, eph: X25519PrivateKey, blob: bytes, kind: int, name: str,
                    data: bytes) -> None:
    """Writer-side check: decrypt ``blob`` using the ephemeral PRIVATE key and the recipient PUBLIC key
    (the writer cannot hold the recipient private key) and compare with the plaintext inputs."""
    header, ct = blob[:HEADER_LEN], blob[HEADER_LEN:]
    eph_raw, rec_raw = header[8:40], _raw_pub(recipient)
    if eph_raw != _raw_pub(eph.public_key()):
        raise SealError("read-back: ephemeral key mismatch")
    try:
        pt = _aead(_exchange(eph, recipient), rec_raw, eph_raw).decrypt(header[48:60], ct, header)
    except InvalidTag as e:
        raise SealError("read-back: blob does not decrypt") from e
    if _parse_envelope(pt) != (kind, name, data):
        raise SealError("read-back: decrypted blob differs from the plaintext")


def sealed_digest(store: Path) -> str:
    """Tree digest of a sealed store: excludes the SEALED receipt and any ``*.tmp`` writer leftovers."""
    return tree_digest(store, SEALED_EXCLUDE, TMP_SUFFIX)


# ---------------------------------------------------------------- store

def _fsync_dir(d: Path) -> None:
    fd = os.open(d, os.O_RDONLY)
    try:
        os.fsync(fd)
    finally:
        os.close(fd)


def _write_durable(path: Path, data: bytes) -> None:
    tmp = path.with_name(path.name + ".tmp")
    with open(tmp, "wb") as f:
        f.write(data)
        f.flush()
        os.fsync(f.fileno())
    os.replace(tmp, path)
    _fsync_dir(path.parent)


class SealedStore:
    """Append-only encrypted blob directory.  ``append`` is the only way raw results are written."""

    def __init__(self, directory: Path, recipient: X25519PublicKey, *, expect_fingerprint: str):
        check_fingerprint(recipient, expect_fingerprint)
        self.dir, self.recipient = Path(directory), recipient
        (self.dir / "blobs").mkdir(parents=True, exist_ok=True)
        if (self.dir / "SEALED").exists():
            raise SealError("store is already closed")

    def _hashes(self) -> list[str]:
        return sorted(p.name[:-4] for p in (self.dir / "blobs").iterdir() if p.name.endswith(".enc") and p.is_file())

    def append(self, name: str, data: bytes) -> str:
        """Encrypt and publish one blob.  Before the blob is published the writer decrypts it with
        its own ephemeral private key and the recipient public key and compares kind, name and
        data with the inputs; after the durable write the file is re-read and compared with the
        bytes that were verified.  Any mismatch raises and nothing is published."""
        data = bytes(data)
        eph = X25519PrivateKey.generate()
        blob = encrypt_blob(self.recipient, KIND_DATA, name, data, eph_priv=eph)
        verify_readback(self.recipient, eph, blob, KIND_DATA, name, data)
        h = hashlib.sha256(blob).hexdigest()
        path = self.dir / "blobs" / f"{h}.enc"
        _write_durable(path, blob)
        if path.read_bytes() != blob:
            path.unlink()
            raise SealError("blob read-back from disk differs; nothing published")
        return h

    def close(self) -> dict:
        """Write the CLOSE record (count and digest of the data-blob hashes) and the SEALED receipt."""
        if (self.dir / "SEALED").exists():
            raise SealError("store is already closed")
        for stale in (self.dir / "blobs").glob("*.tmp"):  # leftovers of crashed writers
            stale.unlink()
        hs = self._hashes()
        info = {"format": FORMAT, "n_blobs": len(hs), "blobs_digest": hashlib.sha256("\n".join(hs).encode()).hexdigest()}
        blob = encrypt_blob(self.recipient, KIND_CLOSE, CLOSE_NAME, json.dumps(info, sort_keys=True).encode())
        _write_durable(self.dir / "blobs" / f"{hashlib.sha256(blob).hexdigest()}.enc", blob)
        receipt = {"format": FORMAT, "recipient_fp": fingerprint(self.recipient),
                   "sealed_tree_digest": sealed_digest(self.dir)}
        _write_durable(self.dir / "SEALED", (json.dumps(receipt, indent=1, sort_keys=True) + "\n").encode())
        return receipt


def seal_tree(src: Path, dst: Path, recipient: X25519PublicKey, *, expect_fingerprint: str,
              remove_plaintext: bool = True, rehearsal_report: bool = False) -> dict:
    """Seal a plaintext tree.  The fingerprint check happens here, before any byte is written.

    REHEARSAL ONLY: the real pilot never has a plaintext tree (the runner writes through the
    SealedStore).  The plaintext tree digest before and after is compared internally (must be equal,
    else abort and keep the plaintext) and is NOT returned, stored in SEALED, logged or printed,
    because a digest of the plaintext would let an observer test guesses against the sealed data;
    ``rehearsal_report=True`` adds them to the returned receipt for rehearsal output only.  Every blob
    is verified at write time by ``SealedStore.append`` (decrypted with its ephemeral private key and
    the recipient public key, compared with the plaintext, then re-read from disk); the plaintext is
    removed only after all blobs and the CLOSE record have been written and verified.
    """
    check_fingerprint(recipient, expect_fingerprint)
    src, dst = Path(src), Path(dst)
    if dst.exists():
        raise SealError(f"{dst} exists")
    before = tree_digest(src)
    store = SealedStore(dst, recipient, expect_fingerprint=expect_fingerprint)
    for rel, p in _files(src):
        store.append(rel, p.read_bytes())
    after = tree_digest(src)
    if after != before:
        raise SealError("source tree changed during sealing; plaintext kept, sealed tree is invalid")
    rec = store.close()
    if remove_plaintext:
        _remove_tree(src)
    if rehearsal_report:
        rec = {**rec, "REHEARSAL_ONLY_plaintext_tree_digest_before": before,
               "REHEARSAL_ONLY_plaintext_tree_digest_after": after}
    return rec


def _remove_tree(root: Path) -> None:
    for _, p in _files(root):
        n = p.stat().st_size
        with open(p, "r+b") as f:  # best effort; journaling or CoW filesystems may keep old blocks
            f.write(b"\0" * n)
            f.flush()
            os.fsync(f.fileno())
        p.unlink()
    for dp, _, _ in os.walk(root, topdown=False):
        os.rmdir(dp)


def read_blobs(sealed: Path, priv: X25519PrivateKey, *, allow_partial: bool = False,
               expect_tree_digest: str | None = None):
    """Owner side: yield (name, data) for every logical blob after verifying the whole store.

    Verifies, in order: the out-of-band tree digest anchor (if given), the directory layout,
    file names (= SHA-256 of contents; ``*.tmp`` leftovers are ignored), authentication of every
    blob, the duplicate rule (a logical name sealed more than once is accepted only when the
    plaintext is byte-identical; differing duplicates are refused), the CLOSE record (well-formed;
    count and digest over ALL data-blob file hashes, duplicates included) and the SEALED receipt.
    """
    sealed = Path(sealed)
    if expect_tree_digest is not None and sealed_digest(sealed) != expect_tree_digest.lower():
        raise SealError("sealed tree digest does not match the out-of-band anchor")
    blobs_dir = sealed / "blobs"
    extra = {p.name for p in sealed.iterdir()} - {"blobs", "SEALED"}
    if extra:
        raise SealError("unexpected entries in sealed directory")
    data_hashes, items, close = [], {}, []
    for p in sorted(blobs_dir.iterdir()):
        if p.name.endswith(TMP_SUFFIX):
            continue
        if not p.is_file():
            raise SealError("unexpected non-file entry in blobs/")
        raw = p.read_bytes()
        h = hashlib.sha256(raw).hexdigest()
        if p.name != f"{h}.enc":
            raise SealError("blob file name does not match its content")
        kind, name, data = decrypt_blob(priv, raw)
        if kind == KIND_CLOSE:
            try:
                info = json.loads(data)
            except ValueError as e:
                raise SealError("malformed CLOSE record") from e
            if not isinstance(info, dict) or not isinstance(info.get("n_blobs"), int) \
                    or not isinstance(info.get("blobs_digest"), str):
                raise SealError("malformed CLOSE record")
            close.append(info)
            continue
        data_hashes.append(h)
        if name in items:
            if items[name] != data:
                raise SealError("logical name sealed twice with different plaintext")
            continue
        items[name] = data
    if len(close) > 1:
        raise SealError("multiple CLOSE records")
    if not close and not allow_partial:
        raise SealError("no CLOSE record: partial store")
    if close:
        hs = sorted(data_hashes)
        c = close[0]
        if c["n_blobs"] != len(hs) or c["blobs_digest"] != hashlib.sha256("\n".join(hs).encode()).hexdigest():
            raise SealError("blob set does not match the CLOSE record")
    rp = sealed / "SEALED"
    if rp.exists() and json.loads(rp.read_text()).get("sealed_tree_digest") != sealed_digest(sealed):
        raise SealError("sealed tree digest does not match the receipt")
    yield from items.items()


def unseal(sealed: Path, priv: X25519PrivateKey, out: Path, *, allow_partial: bool = False,
           expect_tree_digest: str | None = None) -> dict:
    """Decrypt a store into ``out`` (must not exist).  Returns {name: sha256 of data}."""
    out = Path(out)
    items = list(read_blobs(sealed, priv, allow_partial=allow_partial, expect_tree_digest=expect_tree_digest))  # verify everything first
    names = [safe_name(n) for n, _ in items]
    folded = {}
    for n in names:  # names that collide on case-insensitive or Unicode-normalising filesystems
        k = unicodedata.normalize("NFC", n).casefold()
        if folded.setdefault(k, n) != n:
            raise SealError("logical names collide under case folding or Unicode normalisation")
    nameset = set(names)
    for n in names:  # a name may not also be a directory prefix of another name
        parts = n.split("/")
        if any("/".join(parts[:i]) in nameset for i in range(1, len(parts))):
            raise SealError("a logical name is also a directory prefix of another")
    out.mkdir(parents=True)
    root = out.resolve()
    manifest = {}
    for name, data in items:
        dest = (out / name).resolve()
        if root not in dest.parents:
            raise SealError("path traversal refused")
        try:
            dest.parent.mkdir(parents=True, exist_ok=True)
            dest.write_bytes(data)
        except OSError as e:
            raise SealError("could not write an unsealed file") from e
        manifest[name] = hashlib.sha256(data).hexdigest()
    return manifest
