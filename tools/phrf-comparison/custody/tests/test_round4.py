import hashlib
import io
import json
import os
import sys
import unicodedata
import urllib.error
from pathlib import Path

import pytest
from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric.x25519 import (
    X25519PrivateKey,
    X25519PublicKey,
)

from phrf_custody import cli, drand, log, secret, seal, session
import test_secret_session as T


def keypair():  # throwaway
    k = X25519PrivateKey.generate()
    return k, k.public_key()


# ---------------- F2: the secret store is created fresh ----------------


def test_seal_secret_refuses_an_existing_directory(tmp_path):
    priv, pub = keypair()
    fp = seal.fingerprint(pub)
    s1, s2 = bytes(32), bytes(range(32))
    crashed = seal.SealedStore(tmp_path / "sec", pub, expect_fingerprint=fp)
    crashed.append(secret.SECRET_NAME, s1)  # crash after append, before close
    with pytest.raises(seal.SealError, match="already exists"):
        secret.seal_secret(s2, tmp_path / "sec", pub, expect_fingerprint=fp)
    # nothing was added by the refused call
    assert len(list((tmp_path / "sec" / "blobs").iterdir())) == 1
    # the first secret is still revealable by the owner as a partial store
    with pytest.raises(seal.SealError, match="partial"):
        secret.reveal(tmp_path / "sec", priv, secret.commitment(s1))
    assert (
        secret.reveal(tmp_path / "sec", priv, secret.commitment(s1), allow_partial=True)
        == s1
    )
    # an already-closed store is refused too
    secret.seal_secret(s1, tmp_path / "ok", pub, expect_fingerprint=fp)
    with pytest.raises(seal.SealError, match="already exists"):
        secret.seal_secret(s2, tmp_path / "ok", pub, expect_fingerprint=fp)


def test_two_different_secrets_in_one_store_cannot_be_revealed(tmp_path):
    priv, pub = keypair()
    st = seal.SealedStore(tmp_path / "s", pub, expect_fingerprint=seal.fingerprint(pub))
    st.append(secret.SECRET_NAME, bytes(32))
    st.append(secret.SECRET_NAME, bytes(range(32)))
    st.close()
    with pytest.raises(seal.SealError):
        secret.reveal(tmp_path / "s", priv, secret.commitment(bytes(32)))


def test_session_refuses_an_existing_secret_dir_before_logging(tmp_path):
    pub = keypair()[1]
    (tmp_path / "secret.sealed").mkdir()
    r, f, sha = T.make_v0_repo(tmp_path, secret.commitment(bytes(32)))
    with pytest.raises(seal.SealError, match="already exists"):
        session.run_session(
            v0_path=f,
            repo=r,
            v0_commit=sha,
            v0_repo_path="v0.json",
            owner_pub=pub,
            expect_fingerprint=seal.fingerprint(pub),
            secret_dir=tmp_path / "secret.sealed",
            beacon_path=tmp_path / "b",
            log_path=tmp_path / "l.jsonl",
            argv=["true"],
            wait=False,
            exists=lambda r: False,
        )
    assert not (tmp_path / "l.jsonl").exists()


# ---------------- F3: the runner must acknowledge the root ----------------


def run_launch(code, ack_timeout=10):
    events = []
    rc = session.launch(
        [sys.executable, "-c", code],
        777,
        ack_timeout=ack_timeout,
        on_event=lambda a, c: events.append((a, c)),
    )
    return rc, events


def test_correct_ack_is_success():
    assert run_launch(T.ACK)[0] == 0


def test_usr_bin_true_is_a_failed_attempt():
    events = []
    rc = session.launch(
        ["/usr/bin/true"],
        777,
        ack_timeout=10,
        on_event=lambda a, c: events.append((a, c)),
    )
    assert rc == 1 and events == [(0, 1)]


def test_sleep_then_exit_child_is_a_failed_attempt():
    rc, _ = run_launch("import time; time.sleep(1)", ack_timeout=5)
    assert rc != 0
    events = []
    rc = session.launch(
        ["/bin/sleep", "1"],
        777,
        ack_timeout=5,
        on_event=lambda a, c: events.append((a, c)),
    )
    assert rc != 0 and events and events[0][1] != 0


def test_runner_that_never_acks_is_killed_at_the_timeout():
    import time

    t0 = time.monotonic()
    rc, _ = run_launch("import time; time.sleep(30)", ack_timeout=0.5)
    assert rc != 0 and time.monotonic() - t0 < 10


def test_wrong_ack_is_a_failed_attempt():
    rc, _ = run_launch(
        "import os\nos.write(int(os.environ['PHRF_ROOT_ACK_FD']), b'0'*64)\n"
    )
    assert rc != 0
    rc, _ = run_launch(  # acknowledging a different line (it did not read ours)
        "import os,hashlib\nos.write(int(os.environ['PHRF_ROOT_ACK_FD']), hashlib.sha256(b'1\\n').hexdigest().encode())\n"
    )
    assert rc != 0


def test_ack_exit_nonzero_still_fails_and_restart_succeeds(tmp_path):
    marker = tmp_path / "n"
    prog = (
        T.ACK
        + f"p={str(marker)!r}\nn=(open(p).read() if os.path.exists(p) else '')+'x'\nopen(p,'w').write(n)\n"
        + "sys.exit(0 if len(n)>=2 else 3)\n"
    )
    events = []
    rc = session.launch(
        [sys.executable, "-c", prog],
        5,
        max_restarts=2,
        ack_timeout=10,
        on_event=lambda a, c: events.append((a, c)),
    )
    assert rc == 0 and events == [(0, 3), (1, 0)]


# ---------------- F4: round A published check, log raw clock ----------------


def test_round_a_already_published_refuses_v0(tmp_path):
    with pytest.raises(session.SessionError, match="published"):
        T.run_session(tmp_path, exists=lambda r: True)
    assert "v0-late" in (tmp_path / "custody.log.jsonl").read_text()


def test_network_unavailable_is_logged_and_falls_back_to_the_clock(tmp_path):
    rc, *_ = T.run_session(tmp_path, exists=lambda r: None)
    text = (tmp_path / "custody.log.jsonl").read_text()
    assert rc == 0 and "round-check-unavailable" in text
    with pytest.raises(
        session.SessionError, match="at or after"
    ):  # the clock still protects
        T.run_session(
            tmp_path / "c" if (tmp_path / "c").mkdir() is None else tmp_path,
            exists=lambda r: None,
            clock=T.Clock(T.a_time() + 5),
        )


def test_round_exists_classification():
    ok = json.dumps({"round": 5}).encode()

    def get_ok(url, timeout=0):
        return ok

    def get_404(url, timeout=0):
        raise urllib.error.HTTPError(url, 404, "nf", {}, None)

    def get_down(url, timeout=0):
        raise urllib.error.URLError("down")

    assert drand.round_exists(5, http_get=get_ok) is True
    assert (
        drand.round_exists(6, http_get=get_ok) is False
    )  # wrong round answered: not available
    assert drand.round_exists(5, http_get=get_404) is False
    assert drand.round_exists(5, http_get=get_down) is None


def test_log_records_raw_clock_and_surfaces_a_backwards_step(tmp_path):
    p = tmp_path / "l"
    log.append(p, "a", {}, now=100)
    e = log.append(p, "b", {}, now=40)
    rows = [json.loads(x) for x in p.read_text().splitlines()]
    assert [r["event"] for r in rows] == ["a", "clock-backwards", "b"]
    assert (
        rows[1]["data"] == {"ts_raw": 40, "last_ts": 100}
        and rows[2]["ts"] == 100
        and rows[2]["ts_raw"] == 40
    )
    assert log.verify(p) == 3 and e["event"] == "b"


# ---------------- F5: hostile store shapes ----------------


def sealed_store(tmp_path, names):
    priv, pub = keypair()
    st = seal.SealedStore(tmp_path / "s", pub, expect_fingerprint=seal.fingerprint(pub))
    for n in names:
        st.append(n, b"x")
    st.close()
    return priv


def test_directory_inside_blobs_is_a_sealerror(tmp_path):
    priv = sealed_store(tmp_path, ["a"])
    (tmp_path / "s" / "blobs" / "somedir.enc").mkdir()
    with pytest.raises(seal.SealError):
        seal.unseal(tmp_path / "s", priv, tmp_path / "o")


def test_file_and_directory_prefix_conflict_is_a_sealerror(tmp_path):
    priv = sealed_store(tmp_path, ["a", "a/b"])
    with pytest.raises(seal.SealError, match="prefix"):
        seal.unseal(tmp_path / "s", priv, tmp_path / "o")
    assert not (tmp_path / "o").exists()


@pytest.mark.parametrize(
    "pair",
    [
        ("Report", "report"),
        ("café", unicodedata.normalize("NFD", "café")),
        ("ß/x", "SS/x"),
    ],
)
def test_names_colliding_under_casefold_or_nfc_are_refused(tmp_path, pair):
    priv = sealed_store(tmp_path, list(pair))
    with pytest.raises(seal.SealError, match="collide"):
        seal.unseal(tmp_path / "s", priv, tmp_path / "o")
    assert not (tmp_path / "o").exists()


def test_low_order_and_zero_keys_are_sealerrors(tmp_path):
    zero = X25519PublicKey.from_public_bytes(bytes(32))
    with pytest.raises(seal.SealError, match="invalid X25519"):
        seal.encrypt_blob(zero, seal.KIND_DATA, "a", b"x")
    priv, pub = keypair()
    blob = bytearray(seal.encrypt_blob(pub, seal.KIND_DATA, "a", b"x"))
    blob[8:40] = bytes(32)  # zero ephemeral key in the header
    with pytest.raises(seal.SealError, match="invalid X25519"):
        seal.decrypt_blob(priv, bytes(blob))


# ---------------- F6: no plaintext digests in the custody log or stdout ----------------


def test_cli_seal_is_labelled_rehearsal_and_prints_no_plaintext_digest(
    tmp_path, capsys
):
    priv, pub = keypair()
    pk = tmp_path / "owner.pub"
    pk.write_bytes(
        pub.public_bytes(
            serialization.Encoding.PEM, serialization.PublicFormat.SubjectPublicKeyInfo
        )
    )
    raw = tmp_path / "raw"
    raw.mkdir()
    (raw / "f").write_bytes(b"results")
    before = seal.tree_digest(raw)
    lg = tmp_path / "l.jsonl"
    rc = cli.main(
        [
            "--log",
            str(lg),
            "seal",
            str(raw),
            str(tmp_path / "sealed"),
            str(pk),
            "--expect-fingerprint",
            seal.fingerprint(pub),
        ]
    )
    cap = capsys.readouterr()
    assert rc == 0 and "REHEARSAL ONLY" in cap.err
    assert (
        before not in cap.out
        and before not in lg.read_text()
        and "seal-rehearsal" in lg.read_text()
    )
    assert before not in (tmp_path / "sealed" / "SEALED").read_text()


def test_rehearsal_report_flag_labels_plaintext_digests(tmp_path):
    _, pub = keypair()
    raw = tmp_path / "raw"
    raw.mkdir()
    (raw / "f").write_bytes(b"results")
    before = seal.tree_digest(raw)
    rec = seal.seal_tree(
        raw,
        tmp_path / "s",
        pub,
        expect_fingerprint=seal.fingerprint(pub),
        rehearsal_report=True,
    )
    assert rec["REHEARSAL_ONLY_plaintext_tree_digest_before"] == before
    assert rec["REHEARSAL_ONLY_plaintext_tree_digest_after"] == before
    assert before not in (tmp_path / "s" / "SEALED").read_text()


# ---------------- F7: pin the production derivation ----------------


def test_production_aead_derivation_is_pinned():
    shared, rec, eph = bytes(range(32)), bytes(range(32, 64)), bytes(range(64, 96))
    ct = seal._aead(shared, rec, eph).encrypt(bytes(12), b"pin-the-derivation", b"aad")
    assert (
        ct.hex()
        == "9b323a006d921f35c83b7bb1dc41b2975030a2f1c7ededadef1789c42b66b7c51a40"
    )
    # changing any input (key material, either public key) changes the result
    assert (
        seal._aead(shared, eph, rec).encrypt(bytes(12), b"pin-the-derivation", b"aad")
        != ct
    )
    assert (
        seal._aead(bytes(32), rec, eph).encrypt(
            bytes(12), b"pin-the-derivation", b"aad"
        )
        != ct
    )
