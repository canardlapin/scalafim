import copy
import hashlib
import json
from pathlib import Path

import pytest

from phrf_custody import drand, root

FIX = json.loads((Path(__file__).parent / "fixtures" / "drand_default_round_1000000.json").read_text())
HAVE_BLS = drand._bls_available()


def test_fixture_verifies_structurally_and_by_bls_when_available():
    b = drand.verify_beacon(FIX, require_bls=False)
    assert b.round == 1000000 and b.bls_verified == HAVE_BLS
    assert b.randomness == hashlib.sha256(bytes.fromhex(FIX["signature"])).hexdigest()


@pytest.mark.skipif(not HAVE_BLS, reason="py_ecc not installed")
def test_bls_is_the_default():
    assert drand.verify_beacon(FIX).bls_verified


def test_require_bls_refuses_without_verifier(monkeypatch):
    monkeypatch.setattr(drand, "_bls_available", lambda: False)
    assert drand.verify_beacon(FIX, require_bls=False).bls_verified is False
    with pytest.raises(drand.BeaconError):  # F6: default refuses without a verifier
        drand.verify_beacon(FIX)
    with pytest.raises(drand.BeaconError):
        drand.fetch_round(1000000, now=drand.round_time(1000000) + 99, http_get=lambda u, timeout=0: json.dumps(FIX).encode())


def _flip(hexs, i=5):
    b = bytearray(bytes.fromhex(hexs)); b[i] ^= 1
    return bytes(b).hex()


def test_corrupted_signature_rejected_structurally():
    d = copy.deepcopy(FIX); d["signature"] = _flip(d["signature"])
    with pytest.raises(drand.BeaconError):
        drand.verify_beacon(d, require_bls=False)


@pytest.mark.skipif(not HAVE_BLS, reason="py_ecc not installed")
def test_consistent_but_forged_signature_rejected_by_bls():
    d = copy.deepcopy(FIX)
    d["signature"] = _flip(d["signature"])
    d["randomness"] = hashlib.sha256(bytes.fromhex(d["signature"])).hexdigest()  # structurally consistent
    with pytest.raises(drand.BeaconError):
        drand.verify_beacon(d)
    d = copy.deepcopy(FIX); d["round"] += 1  # wrong round for this signature
    with pytest.raises(drand.BeaconError):
        drand.verify_beacon(d)


def test_future_round_never_fetched():
    def boom(url, timeout=0):
        raise AssertionError("network used for a future round")

    now = drand.round_time(1000000) + 1
    with pytest.raises(drand.BeaconError, match="future"):
        drand.fetch_round(1000001 + 10, now=now, http_get=boom)


def test_fetch_past_round_from_recorded_relays_and_disagreement():
    raw = json.dumps(FIX).encode()
    now = drand.round_time(1000000) + 10_000
    b, log = drand.fetch_round(1000000, now=now, http_get=lambda u, timeout=0: raw, require_bls=HAVE_BLS)
    assert b.randomness == FIX["randomness"] and len(log) == 3
    assert all(e["response_sha256"] == hashlib.sha256(raw).hexdigest() for e in log)

    other = copy.deepcopy(FIX)  # a second self-consistent but different beacon
    other["signature"] = _flip(other["signature"]); other["randomness"] = hashlib.sha256(bytes.fromhex(other["signature"])).hexdigest()
    answers = iter([raw, json.dumps(other).encode(), raw])
    with pytest.raises(drand.BeaconError):  # either BLS rejects the forgery or relays disagree
        drand.fetch_round(1000000, now=now, http_get=lambda u, timeout=0: next(answers), require_bls=False)

    with pytest.raises(drand.BeaconError, match="need 2"):
        drand.fetch_round(1000000, now=now, relays=drand.RELAYS[:1], http_get=lambda u, timeout=0: raw,
                           require_bls=False)


def test_non_https_relay_rejected():
    with pytest.raises(drand.BeaconError):
        drand.fetch_round(1, relays=("http://x", "http://y"), now=drand.round_time(5), http_get=lambda *a, **k: b"")


# ---- root derivation (commit-reveal secret) ----
V0 = hashlib.sha256(b"test v0 manifest").hexdigest()
SECRET = bytes(range(32))  # test-only
SEEDS_SHA = root.seeds_sha256()


def seeds():
    return root.load_seeds(SEEDS_SHA)


def test_root_vector_and_construction():
    r, r64 = root.derive_root(V0, FIX["randomness"], SECRET, seeds=seeds())
    assert r == hashlib.sha256(bytes.fromhex(V0) + bytes.fromhex(FIX["randomness"]) + SECRET).hexdigest()
    assert r64 == int.from_bytes(bytes.fromhex(r)[:8], "big")
    # frozen vector: any change to the construction (order, big-endian, hashing, secret) breaks this
    assert (r, r64) == ("d042419a0b19f47daed5ae0533c8a013be7b782383206dc1cde51f8d9cf980c5", 15006629038218933373)


def test_secret_changes_the_root_and_order_matters():
    s = seeds()
    r, _ = root.derive_root(V0, FIX["randomness"], SECRET, seeds=s)
    assert root.derive_root(V0, FIX["randomness"], bytes(32), seeds=s)[0] != r
    assert root.derive_root(FIX["randomness"], V0, SECRET, seeds=s)[0] != r


def test_parity_with_seeds_py():
    s = seeds()
    r, r64 = root.derive_root(V0, FIX["randomness"], SECRET, seeds=s)
    assert r64 == s.root_from_sha256_hex(r)
    assert root.denylist_check(r64, seeds=s)["seeds_checked"] == 7 * 20 * 7


def test_seeds_sha_mismatch_refused(tmp_path):
    with pytest.raises(root.SeedsMismatch):
        root.load_seeds("0" * 64)
    altered = tmp_path / "seeds.py"
    altered.write_bytes(root.SEEDS_PY.read_bytes() + b"\n# edit\n")
    with pytest.raises(root.SeedsMismatch):
        root.load_seeds(SEEDS_SHA, altered)


def test_pilot_cells_match_generator():
    import sys
    gen = Path(root.SEEDS_PY).parent.parent
    sys.path.insert(0, str(gen))
    try:
        from phrf_gen.cells import CELLS
    finally:
        sys.path.remove(str(gen))
    assert tuple(list(CELLS)[:7]) == root.PILOT_CELLS


@pytest.mark.parametrize("value", [101, 11, 7000930101, 20260911, (5 << 40) | 102, (1 << 63) | 20260909])
def test_denylist_root_hit_including_low32(value):
    with pytest.raises(root.DenylistHit):
        root.denylist_check(value, seeds=seeds())


def test_denylist_stream_hit_aborts():
    base = seeds()

    class Fake:
        PURPOSES = base.PURPOSES
        __file__ = base.__file__
        stream_seed = staticmethod(base.stream_seed)
        denylist_hit = staticmethod(lambda s: s == base.stream_seed(1, "C-TS-1", 3, "noise"))

    with pytest.raises(root.DenylistHit, match="C-TS-1"):
        root.denylist_check(1, seeds=Fake)


def test_bad_lengths_rejected():
    with pytest.raises(ValueError):
        root.derive_root("ab", FIX["randomness"], SECRET, seeds=seeds())
    with pytest.raises(ValueError):
        root.derive_root(V0, FIX["randomness"], b"short", seeds=seeds())
