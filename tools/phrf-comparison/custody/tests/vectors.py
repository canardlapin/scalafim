"""Golden interop vectors for phrf-sealed/1 (docs/plans/phrf-pilot-sealed-format.md).

Run ``python tests/vectors.py`` to regenerate tests/fixtures/sealed_format_vectors.json.
"""
import hashlib
import hmac
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric.x25519 import X25519PrivateKey

from phrf_custody import seal

RECIPIENT_PRIV = bytes(range(1, 33))   # TEST ONLY
EPH_PRIV = bytes(range(101, 133))      # TEST ONLY
NONCE = bytes(range(201, 213))
CASES = [
    ("data-small", seal.KIND_DATA, "dataset/C-TS-1/0007", b"phrf golden vector plaintext\x00\x01\x02\xff"),
    ("close", seal.KIND_CLOSE, "CLOSE", json.dumps({"blobs_digest": "00" * 32, "format": "phrf-sealed/1", "n_blobs": 1}, sort_keys=True).encode()),
    ("pad-exact-4096", seal.KIND_DATA, "x", bytes(i % 251 for i in range(4084))),
    ("pad-next-8192", seal.KIND_DATA, "x", bytes(i % 251 for i in range(4085))),
    ("empty", seal.KIND_DATA, "e", b""),
]


def raw(k):
    return k.public_key().public_bytes(serialization.Encoding.Raw, serialization.PublicFormat.Raw)


def hkdf_sha256(ikm, info, length=32):  # independent of the cryptography HKDF implementation
    prk = hmac.new(b"\0" * 32, ikm, hashlib.sha256).digest()
    return hmac.new(prk, info + b"\x01", hashlib.sha256).digest()[:length]


def build():
    rk, ek = X25519PrivateKey.from_private_bytes(RECIPIENT_PRIV), X25519PrivateKey.from_private_bytes(EPH_PRIV)
    out = {"format": "phrf-sealed/1", "recipient_private_hex": RECIPIENT_PRIV.hex(), "recipient_public_hex": raw(rk).hex(),
           "ephemeral_private_hex": EPH_PRIV.hex(), "ephemeral_public_hex": raw(ek).hex(), "nonce_hex": NONCE.hex(),
           "shared_secret_hex": ek.exchange(rk.public_key()).hex(), "cases": []}
    key = hkdf_sha256(ek.exchange(rk.public_key()), seal.INFO + raw(rk) + raw(ek))
    out["aead_key_hex"] = key.hex()
    for cid, kind, name, data in CASES:
        blob = seal.encrypt_blob(rk.public_key(), kind, name, data, eph_priv=ek, nonce=NONCE)
        env = seal._envelope(kind, name, data)
        c = {"id": cid, "kind": kind, "name": name, "data_hex": data.hex() if len(data) < 200 else None,
             "data_len": len(data), "data_sha256": hashlib.sha256(data).hexdigest(),
             "envelope_len": len(env), "envelope_sha256": hashlib.sha256(env).hexdigest(),
             "aad_hex": blob[:seal.HEADER_LEN].hex(), "blob_len": len(blob), "blob_sha256": hashlib.sha256(blob).hexdigest(),
             "blob_file_name": hashlib.sha256(blob).hexdigest() + ".enc"}
        if cid == "data-small":
            c["envelope_hex"] = env.hex()
            c["blob_hex"] = blob.hex()
        out["cases"].append(c)
    return out


if __name__ == "__main__":
    p = Path(__file__).parent / "fixtures" / "sealed_format_vectors.json"
    p.write_text(json.dumps(build(), indent=1, sort_keys=True) + "\n")
    print("wrote", p)
