import hashlib
import json
import os
import shutil
from pathlib import Path

import pytest
from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric.x25519 import X25519PrivateKey

from phrf_custody import seal

MARK = b"SECRET-MARKER-9f3a1c-pilot-error-values"


def keypair():  # throwaway, test-only
    k = X25519PrivateKey.generate()
    return k, k.public_key()


def make_tree(root: Path):
    (root / "ds").mkdir(parents=True)
    (root / "ds" / "cellA_dataset07.bin").write_bytes(MARK + os.urandom(2000))
    (root / "attempt-meta.json").write_bytes(b'{"cell": "C-TG-.5"}' + MARK)
    (root / "empty.bin").write_bytes(b"")


def scan(root: Path, needle: bytes):
    for dp, dn, fn in os.walk(root):
        for n in dn + fn:
            if needle in n.encode():
                return str(Path(dp) / n)
        for n in fn:
            if needle in (Path(dp) / n).read_bytes():
                return str(Path(dp) / n)
    return None


def seal_it(tmp_path, pub, **kw):
    src = tmp_path / "work" / "raw"; make_tree(src)
    expected = {p.relative_to(src).as_posix(): p.read_bytes() for p in src.rglob("*") if p.is_file()}
    rec = seal.seal_tree(src, tmp_path / "work" / "sealed", pub, expect_fingerprint=seal.fingerprint(pub), **kw)
    return src, expected, rec


def test_round_trip_digests_and_no_plaintext(tmp_path):
    priv, pub = keypair()
    src = tmp_path / "work" / "raw"; make_tree(src)
    expected = {p.relative_to(src).as_posix(): p.read_bytes() for p in src.rglob("*") if p.is_file()}
    before = seal.tree_digest(src)
    rec = seal.seal_tree(src, tmp_path / "work" / "sealed", pub, expect_fingerprint=seal.fingerprint(pub))
    # F6: no plaintext digest in the receipt or in SEALED (it would let observers test guesses)
    assert set(rec) == {"format", "recipient_fp", "sealed_tree_digest"}
    assert before not in json.dumps(rec) and before not in (tmp_path / "work" / "sealed" / "SEALED").read_text()
    sealed = tmp_path / "work" / "sealed"
    assert rec["sealed_tree_digest"] == seal.tree_digest(sealed, seal.SEALED_EXCLUDE)
    assert not src.exists()
    assert scan(tmp_path, MARK) is None
    assert scan(tmp_path, b"cellA") is None and scan(tmp_path, b"C-TG") is None
    out = tmp_path / "opened"
    seal.unseal(sealed, priv, out)
    got = {p.relative_to(out).as_posix(): p.read_bytes() for p in out.rglob("*") if p.is_file()}
    assert got == expected and seal.tree_digest(out) == before


def test_blob_sizes_are_bucketed(tmp_path):
    _, pub = keypair()
    store = seal.SealedStore(tmp_path / "s", pub, expect_fingerprint=seal.fingerprint(pub))
    for n, size in ((0, 4096), (100, 4096), (4082, 4096), (4083, 8192), (70000, 131072)):
        h = store.append(f"x/{n % 10}", os.urandom(n))  # 3-byte names: envelope = n + 14
        assert (tmp_path / "s" / "blobs" / f"{h}.enc").stat().st_size == seal.HEADER_LEN + size + 16
    assert seal.bucket_size((1 << 30) + 1) == 2 << 30 and seal.bucket_size(1 << 30) == 1 << 30


def test_wrong_key_fails(tmp_path):
    _, pub = keypair(); other, _ = keypair()
    _, _, _ = seal_it(tmp_path, pub)
    with pytest.raises(seal.SealError):
        seal.unseal(tmp_path / "work" / "sealed", other, tmp_path / "o")


def test_wrong_key_with_matching_recipient_id_fails_authentication(tmp_path):
    priv, pub = keypair(); other, _ = keypair()
    seal_it(tmp_path, pub)
    orig = seal.recipient_id
    seal.recipient_id = lambda p: orig(pub)
    try:
        with pytest.raises(seal.SealError, match="authentication"):
            seal.unseal(tmp_path / "work" / "sealed", other, tmp_path / "o")
    finally:
        seal.recipient_id = orig


def test_fingerprint_checked_inside_seal_tree_and_store(tmp_path):
    _, pub = keypair()
    src = tmp_path / "raw"; make_tree(src)
    with pytest.raises(seal.SealError, match="fingerprint"):
        seal.seal_tree(src, tmp_path / "sealed", pub, expect_fingerprint="0" * 64)
    assert src.exists() and not (tmp_path / "sealed").exists()
    with pytest.raises(seal.SealError, match="fingerprint"):
        seal.SealedStore(tmp_path / "s2", pub, expect_fingerprint="ab" * 32)
    with pytest.raises(TypeError):
        seal.seal_tree(src, tmp_path / "sealed", pub)  # the check is not optional


def _blobs(s):
    return sorted((s / "blobs").iterdir())


def _flip(p, off=None):
    b = bytearray(p.read_bytes()); b[len(b) // 2 if off is None else off] ^= 1; p.write_bytes(bytes(b))


@pytest.mark.parametrize("how", ["ciphertext", "header", "rename", "delete", "add", "stray", "receipt"])
def test_tamper_detected(tmp_path, how):
    priv, pub = keypair()
    seal_it(tmp_path, pub)
    s = tmp_path / "work" / "sealed"
    bl = _blobs(s)
    if how == "ciphertext":
        _flip(bl[0])
    elif how == "header":
        _flip(bl[0], 10)
    elif how == "rename":
        bl[0].rename(bl[0].with_name("0" * 64 + ".enc"))
    elif how == "delete":
        bl[0].unlink()
    elif how == "add":
        shutil.copy(bl[0], s / "blobs" / "dup.enc")
    elif how == "stray":
        (s / "notes.txt").write_text("x")
    elif how == "receipt":
        r = json.loads((s / "SEALED").read_text()); r["sealed_tree_digest"] = "0" * 64
        (s / "SEALED").write_text(json.dumps(r))
    with pytest.raises(seal.SealError):
        seal.unseal(s, priv, tmp_path / "o")


def test_swapped_blob_between_stores_does_not_authenticate_close(tmp_path):
    priv, pub = keypair()
    store = seal.SealedStore(tmp_path / "s", pub, expect_fingerprint=seal.fingerprint(pub))
    store.append("a", b"1"); store.append("b", b"2")
    store.close()
    # drop one data blob and replace it with a fresh valid blob under another name: CLOSE digest mismatch
    victim = next(p for p in _blobs(tmp_path / "s") if seal.decrypt_blob(priv, p.read_bytes())[1] == "b")
    victim.unlink()
    blob = seal.encrypt_blob(pub, seal.KIND_DATA, "c", b"3")
    (tmp_path / "s" / "blobs" / (hashlib.sha256(blob).hexdigest() + ".enc")).write_bytes(blob)
    with pytest.raises(seal.SealError):
        seal.unseal(tmp_path / "s", priv, tmp_path / "o")


def test_partial_store_needs_explicit_allow(tmp_path):
    priv, pub = keypair()
    store = seal.SealedStore(tmp_path / "s", pub, expect_fingerprint=seal.fingerprint(pub))
    store.append("a/b", b"1")
    with pytest.raises(seal.SealError, match="partial"):
        seal.unseal(tmp_path / "s", priv, tmp_path / "o")
    assert seal.unseal(tmp_path / "s", priv, tmp_path / "o2", allow_partial=True) == {"a/b": hashlib.sha256(b"1").hexdigest()}


def test_resume_appends_with_a_new_writer_and_closes_once(tmp_path):
    priv, pub = keypair(); fp = seal.fingerprint(pub)
    seal.SealedStore(tmp_path / "s", pub, expect_fingerprint=fp).append("a", b"1")
    w2 = seal.SealedStore(tmp_path / "s", pub, expect_fingerprint=fp)  # fresh process, no key state
    w2.append("b", b"2")
    w2.close()
    assert set(seal.unseal(tmp_path / "s", priv, tmp_path / "o")) == {"a", "b"}
    with pytest.raises(seal.SealError, match="closed"):
        w2.close()
    with pytest.raises(seal.SealError, match="closed"):
        seal.SealedStore(tmp_path / "s", pub, expect_fingerprint=fp)


@pytest.mark.parametrize("bad", ["../x", "/abs", "a/../../x", "a//b", "./a", "a\\b", "", "a\0b", "a/."])
def test_unsafe_names_refused_on_write(tmp_path, bad):
    _, pub = keypair()
    store = seal.SealedStore(tmp_path / "s", pub, expect_fingerprint=seal.fingerprint(pub))
    with pytest.raises(seal.SealError):
        store.append(bad, b"x")


def test_path_traversal_refused_on_unseal(tmp_path, monkeypatch):
    priv, pub = keypair()
    monkeypatch.setattr(seal, "safe_name", lambda n: n)  # a malicious writer bypasses the check
    store = seal.SealedStore(tmp_path / "s", pub, expect_fingerprint=seal.fingerprint(pub))
    store.append("../../escaped.txt", b"pwn")
    store.close()
    monkeypatch.undo()
    out = tmp_path / "deep" / "o"
    (tmp_path / "deep").mkdir()
    with pytest.raises(seal.SealError):
        seal.unseal(tmp_path / "s", priv, out)
    assert not (tmp_path / "escaped.txt").exists() and not (tmp_path.parent / "escaped.txt").exists()


def test_digest_sensitive_to_one_byte_and_stable(tmp_path):
    src = tmp_path / "raw"; make_tree(src)
    d = seal.tree_digest(src)
    assert seal.tree_digest(src) == d
    f = src / "ds" / "cellA_dataset07.bin"
    b = bytearray(f.read_bytes()); b[100] ^= 1; f.write_bytes(bytes(b))
    assert seal.tree_digest(src) != d


def test_source_change_during_seal_keeps_plaintext(tmp_path, monkeypatch):
    _, pub = keypair()
    src = tmp_path / "raw"; make_tree(src)
    calls = []
    orig = seal.tree_digest

    def td(r, exclude=()):
        calls.append(r)
        d = orig(r, exclude)
        if len(calls) == 1:
            (src / "late.bin").write_bytes(b"x")
        return d

    monkeypatch.setattr(seal, "tree_digest", td)
    with pytest.raises(seal.SealError):
        seal.seal_tree(src, tmp_path / "sealed", pub, expect_fingerprint=seal.fingerprint(pub))
    assert src.exists()


def test_keep_plaintext_option_and_key_formats(tmp_path):
    priv, pub = keypair()
    pem = pub.public_bytes(serialization.Encoding.PEM, serialization.PublicFormat.SubjectPublicKeyInfo)
    raw = pub.public_bytes(serialization.Encoding.Raw, serialization.PublicFormat.Raw).hex().encode()
    assert seal.fingerprint(seal.load_public_key(pem)) == seal.fingerprint(seal.load_public_key(raw))
    with pytest.raises(seal.SealError):
        seal.load_public_key(b"garbage")
    seal_it(tmp_path, pub, remove_plaintext=False)
    assert (tmp_path / "work" / "raw").exists()


def test_symlink_refused(tmp_path):
    _, pub = keypair()
    src = tmp_path / "raw"; make_tree(src)
    os.symlink("/etc/hosts", src / "link")
    with pytest.raises(seal.SealError):
        seal.seal_tree(src, tmp_path / "sealed", pub, expect_fingerprint=seal.fingerprint(pub))
