import hashlib
import json
from pathlib import Path

import pytest
from cryptography.hazmat.primitives.asymmetric.x25519 import X25519PrivateKey

from phrf_custody import seal
import vectors

FIX = json.loads((Path(__file__).parent / "fixtures" / "sealed_format_vectors.json").read_text())


def test_fixture_is_current():
    assert vectors.build() == FIX


def test_key_derivation_matches_independent_hkdf():
    assert FIX["aead_key_hex"] == vectors.hkdf_sha256(
        bytes.fromhex(FIX["shared_secret_hex"]),
        b"phrf-sealed/1 blob" + bytes.fromhex(FIX["recipient_public_hex"]) + bytes.fromhex(FIX["ephemeral_public_hex"])).hex()


@pytest.mark.parametrize("case", FIX["cases"], ids=lambda c: c["id"])
def test_blob_bytes_and_decrypt(case):
    rk = X25519PrivateKey.from_private_bytes(bytes.fromhex(FIX["recipient_private_hex"]))
    ek = X25519PrivateKey.from_private_bytes(bytes.fromhex(FIX["ephemeral_private_hex"]))
    data = bytes(i % 251 for i in range(case["data_len"])) if case["data_hex"] is None else bytes.fromhex(case["data_hex"])
    blob = seal.encrypt_blob(rk.public_key(), case["kind"], case["name"], data, eph_priv=ek,
                             nonce=bytes.fromhex(FIX["nonce_hex"]))
    assert len(blob) == case["blob_len"] and hashlib.sha256(blob).hexdigest() == case["blob_sha256"]
    assert blob[:seal.HEADER_LEN].hex() == case["aad_hex"]
    assert seal.decrypt_blob(rk, blob) == (case["kind"], case["name"], data)
    if "blob_hex" in case:
        assert blob.hex() == case["blob_hex"]
