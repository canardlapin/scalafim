"""Published known-answer tests for the primitives, independent of the self-generated vectors."""

import hashlib
import hmac

from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.asymmetric.x25519 import (
    X25519PrivateKey,
    X25519PublicKey,
)
from cryptography.hazmat.primitives.kdf.hkdf import HKDF


def test_rfc7748_section_6_1_x25519():
    a = X25519PrivateKey.from_private_bytes(
        bytes.fromhex(
            "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
        )
    )
    b = X25519PrivateKey.from_private_bytes(
        bytes.fromhex(
            "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
        )
    )
    from cryptography.hazmat.primitives import serialization as s

    raw = lambda k: k.public_key().public_bytes(s.Encoding.Raw, s.PublicFormat.Raw)
    assert (
        raw(a).hex()
        == "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
    )
    assert (
        raw(b).hex()
        == "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"
    )
    shared = "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742"
    assert a.exchange(b.public_key()).hex() == shared
    assert b.exchange(a.public_key()).hex() == shared


def _hkdf(ikm, salt, info, length):  # RFC 5869 from hmac, independent of the library
    prk = hmac.new(salt or b"\0" * 32, ikm, hashlib.sha256).digest()
    okm, t, i = b"", b"", 1
    while len(okm) < length:
        t = hmac.new(prk, t + info + bytes([i]), hashlib.sha256).digest()
        okm += t
        i += 1
    return okm[:length]


CASES = [
    (  # RFC 5869 A.1
        bytes.fromhex("0b" * 22),
        bytes.fromhex("000102030405060708090a0b0c"),
        bytes.fromhex("f0f1f2f3f4f5f6f7f8f9"),
        42,
        "3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865",
    ),
    (  # RFC 5869 A.3: zero-length salt and info (the form phrf-sealed/1 uses for salt)
        bytes.fromhex("0b" * 22),
        b"",
        b"",
        42,
        "8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8",
    ),
]


def test_rfc5869_hkdf_sha256():
    for ikm, salt, info, n, okm in CASES:
        assert _hkdf(ikm, salt, info, n).hex() == okm
        lib = HKDF(hashes.SHA256(), n, salt or None, info).derive(ikm)
        assert lib.hex() == okm
