import os
from pathlib import Path

from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric.x25519 import X25519PrivateKey

from phrf_custody import cli, log


def test_seal_refuses_unconfirmed_fingerprint_and_logs(tmp_path, capsys):
    pub = X25519PrivateKey.generate().public_key()  # throwaway
    pk = tmp_path / "owner.pub"
    pk.write_bytes(pub.public_bytes(serialization.Encoding.PEM, serialization.PublicFormat.SubjectPublicKeyInfo))
    (tmp_path / "raw").mkdir(); (tmp_path / "raw" / "a").write_bytes(b"x")
    lg = tmp_path / "log.jsonl"
    rc = cli.main(["--log", str(lg), "seal", str(tmp_path / "raw"), str(tmp_path / "sealed"), str(pk),
                   "--expect-fingerprint", "0" * 64])
    assert rc == 2 and (tmp_path / "raw" / "a").exists() and not (tmp_path / "sealed").exists()
    assert log.verify(lg) == 1
    fp = capsys.readouterr()
    assert "REFUSED" in fp.err
    from phrf_custody import seal
    good = seal.fingerprint(pub)
    assert cli.main(["--log", str(lg), "seal", str(tmp_path / "raw"), str(tmp_path / "sealed"), str(pk),
                     "--expect-fingerprint", good]) == 0
    assert not (tmp_path / "raw").exists() and log.verify(lg) == 2


def test_unseal_cli_round_trip_and_partial_flag(tmp_path, capsys):
    from phrf_custody import seal
    priv = X25519PrivateKey.generate()  # throwaway
    pem = tmp_path / "k.pem"
    pem.write_bytes(priv.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8,
                                       serialization.NoEncryption()))
    store = seal.SealedStore(tmp_path / "s", priv.public_key(), expect_fingerprint=seal.fingerprint(priv.public_key()))
    store.append("a/b.bin", b"payload")
    lg = str(tmp_path / "l.jsonl")
    assert cli.main(["--log", lg, "unseal", str(tmp_path / "s"), str(pem), str(tmp_path / "o")]) == 2  # no CLOSE
    assert not (tmp_path / "o").exists()
    assert cli.main(["--log", str(tmp_path / "l.jsonl"), "unseal", str(tmp_path / "s"), str(pem), str(tmp_path / "o3"),
                     "--allow-partial"]) == 0
    assert (tmp_path / "o3" / "a" / "b.bin").read_bytes() == b"payload"
