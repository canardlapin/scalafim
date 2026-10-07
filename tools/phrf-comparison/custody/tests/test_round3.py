import hashlib
import io
import json
import os
import subprocess
import sys
from pathlib import Path

import pytest
from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric.x25519 import X25519PrivateKey

from phrf_custody import checkout as C
from phrf_custody import cli, drand, log, secret, seal, session, v0 as v0mod
from phrf_custody import whitelist as W

import test_secret_session as T


def keypair():  # throwaway
    k = X25519PrivateKey.generate()
    return k, k.public_key()


def new_store(tmp_path, name="s"):
    priv, pub = keypair()
    return (
        priv,
        pub,
        seal.SealedStore(
            tmp_path / name, pub, expect_fingerprint=seal.fingerprint(pub)
        ),
    )


# ---------------- B1: writer-side read-back ----------------


def test_append_verifies_readback_and_publishes_nothing_on_corruption(
    tmp_path, monkeypatch
):
    _, pub, store = new_store(tmp_path)
    orig = seal.encrypt_blob

    def corrupt(*a, **k):
        b = bytearray(orig(*a, **k))
        b[-5] ^= 1
        return bytes(b)

    monkeypatch.setattr(seal, "encrypt_blob", corrupt)
    with pytest.raises(seal.SealError, match="read-back"):
        store.append("a", b"x")
    assert list((tmp_path / "s" / "blobs").iterdir()) == []


def test_seal_tree_keeps_plaintext_when_readback_fails(tmp_path, monkeypatch):
    _, pub = keypair()
    src = tmp_path / "raw"
    src.mkdir()
    (src / "f").write_bytes(b"data")
    orig = seal.encrypt_blob
    monkeypatch.setattr(
        seal,
        "encrypt_blob",
        lambda *a, **k: orig(*a, **k)[:-1] + b"\x00",
    )
    with pytest.raises(seal.SealError):
        seal.seal_tree(
            src, tmp_path / "sealed", pub, expect_fingerprint=seal.fingerprint(pub)
        )
    assert (src / "f").read_bytes() == b"data"


def test_readback_detects_wrong_plaintext(tmp_path):
    _, pub = keypair()
    eph = X25519PrivateKey.generate()
    blob = seal.encrypt_blob(pub, seal.KIND_DATA, "a", b"x", eph_priv=eph)
    seal.verify_readback(pub, eph, blob, seal.KIND_DATA, "a", b"x")
    with pytest.raises(seal.SealError):
        seal.verify_readback(pub, eph, blob, seal.KIND_DATA, "a", b"y")
    with pytest.raises(seal.SealError):
        seal.verify_readback(
            pub, X25519PrivateKey.generate(), blob, seal.KIND_DATA, "a", b"x"
        )


# ---------------- B2: duplicates on resume ----------------


def test_identical_duplicate_accepted_differing_refused(tmp_path):
    priv, pub, store = new_store(tmp_path)
    store.append("ds/1", b"same")
    store.append("ds/1", b"same")  # resumed run re-sealed the same dataset
    store.close()
    assert seal.unseal(tmp_path / "s", priv, tmp_path / "o") == {
        "ds/1": hashlib.sha256(b"same").hexdigest()
    }
    _, _, s2 = new_store(tmp_path, "s2")
    priv2 = None
    # differing duplicate: use a fresh key pair so we can read it back
    priv2, pub2 = keypair()
    s2 = seal.SealedStore(
        tmp_path / "s3", pub2, expect_fingerprint=seal.fingerprint(pub2)
    )
    s2.append("ds/1", b"one")
    s2.append("ds/1", b"two")
    s2.close()
    with pytest.raises(seal.SealError, match="different plaintext"):
        seal.unseal(tmp_path / "s3", priv2, tmp_path / "o3")


def test_close_counts_duplicate_files(tmp_path):
    priv, pub, store = new_store(tmp_path)
    store.append("a", b"1")
    store.append("a", b"1")
    store.close()
    # removing one of the duplicate blobs is detected by the CLOSE record
    victim = sorted((tmp_path / "s" / "blobs").iterdir())[0]
    kinds = []
    for p in sorted((tmp_path / "s" / "blobs").iterdir()):
        kinds.append((p, seal.decrypt_blob(priv, p.read_bytes())[0]))
    victim = next(p for p, k in kinds if k == seal.KIND_DATA)
    victim.unlink()
    with pytest.raises(seal.SealError):
        seal.unseal(tmp_path / "s", priv, tmp_path / "o")


# ---------------- stray .tmp, malformed CLOSE, anchor ----------------


def test_tmp_leftovers_ignored_then_removed_at_close(tmp_path):
    priv, pub, store = new_store(tmp_path)
    store.append("a", b"1")
    stale = tmp_path / "s" / "blobs" / ("f" * 64 + ".enc.tmp")
    stale.write_bytes(b"half written")
    d_with = seal.sealed_digest(tmp_path / "s")
    stale.unlink()
    assert seal.sealed_digest(tmp_path / "s") == d_with  # digest ignores *.tmp
    stale.write_bytes(b"half written")
    # reader ignores it on a partial store
    assert seal.unseal(tmp_path / "s", priv, tmp_path / "o", allow_partial=True) == {
        "a": hashlib.sha256(b"1").hexdigest()
    }
    store.close()
    assert not stale.exists()
    seal.unseal(tmp_path / "s", priv, tmp_path / "o2")


@pytest.mark.parametrize(
    "bad", [b"not json", b"[1]", b'{"n_blobs": "x", "blobs_digest": 1}', b"{}"]
)
def test_malformed_close_is_a_sealerror(tmp_path, bad):
    priv, pub = keypair()
    d = tmp_path / "s" / "blobs"
    d.mkdir(parents=True)
    blob = seal.encrypt_blob(pub, seal.KIND_CLOSE, "CLOSE", bad)
    (d / (hashlib.sha256(blob).hexdigest() + ".enc")).write_bytes(blob)
    with pytest.raises(seal.SealError, match="CLOSE"):
        seal.unseal(tmp_path / "s", priv, tmp_path / "o")


def test_expect_tree_digest_anchor(tmp_path):
    priv, pub, store = new_store(tmp_path)
    store.append("a", b"1")
    rec = store.close()
    good = rec["sealed_tree_digest"]
    seal.unseal(tmp_path / "s", priv, tmp_path / "o", expect_tree_digest=good)
    with pytest.raises(seal.SealError, match="anchor"):
        seal.unseal(tmp_path / "s", priv, tmp_path / "o2", expect_tree_digest="0" * 64)
    assert not (tmp_path / "o2").exists()


def test_unseal_cli_expect_tree_digest(tmp_path, capsys):
    priv, pub, store = new_store(tmp_path)
    store.append("a", b"1")
    rec = store.close()
    pem = tmp_path / "k.pem"
    pem.write_bytes(
        priv.private_bytes(
            serialization.Encoding.PEM,
            serialization.PrivateFormat.PKCS8,
            serialization.NoEncryption(),
        )
    )
    lg = str(tmp_path / "l.jsonl")
    base = ["--log", lg, "unseal", str(tmp_path / "s"), str(pem)]
    assert (
        cli.main(base + [str(tmp_path / "o1"), "--expect-tree-digest", "0" * 64]) == 2
    )
    assert (
        cli.main(
            base
            + [str(tmp_path / "o2"), "--expect-tree-digest", rec["sealed_tree_digest"]]
        )
        == 0
    )
    assert not Path(lg).exists()  # owner-side tools write no custody log


# ---------------- whitelist: icc <= ucl80 ----------------


def test_icc_must_not_exceed_ucl80():
    from test_whitelist_log import good, emitted

    c = good()
    c["icc"][W.ICC_CELL]["tau_error"] = {"icc": 0.5, "ucl80": 0.4}
    with pytest.raises(W.WhitelistError):
        W.validate(emitted(c))
    c["icc"][W.ICC_CELL]["tau_error"] = {"icc": 0.4, "ucl80": 0.4}
    W.validate(emitted(c))


# ---------------- v0 / session ----------------


def test_v0_commit_must_be_40_hex(tmp_path):
    c = secret.commitment(bytes(32))
    r, f, sha = T.make_v0_repo(tmp_path, c)
    with pytest.raises(v0mod.V0Error, match="40-hex"):
        v0mod.load_v0(f, repo=r, commit=sha[:12], repo_path="v0.json")
    with pytest.raises(SystemExit):
        cli.main(
            [
                "beacon",
                "--v0",
                str(f),
                "--repo",
                str(r),
                "--v0-commit",
                "HEAD",
                "--v0-repo-path",
                "v0.json",
                "--out",
                str(tmp_path / "b"),
            ]
        )


def test_late_v0_load_is_refused_and_logged(tmp_path):
    lg = tmp_path / "custody.log.jsonl"
    with pytest.raises(session.SessionError, match="round A"):
        T.run_session(tmp_path, clock=T.Clock(T.a_time() + 5))
    assert "v0-late" in lg.read_text()


def test_ordering_logged_when_enforced_and_skipped_in_recovery(tmp_path):
    rc, *_ = T.run_session(tmp_path)
    entry = [
        json.loads(x) for x in (tmp_path / "custody.log.jsonl").read_text().splitlines()
    ]
    v = next(e for e in entry if e["event"] == "v0")["data"]
    assert v["ordering_enforced"] is True and v["loaded_at"] < v["round_time"]
    t2 = tmp_path / "rec"
    t2.mkdir()
    T.run_session(t2, with_secret=bytes(range(32)), clock=T.Clock(T.a_time() + 5000))
    v = [
        json.loads(x)
        for x in (t2 / "custody.log.jsonl").read_text().splitlines()
        if '"event": "v0"' in x
    ][-1]["data"]  # the recovered session's entry (the seeded original comes first)
    assert v["ordering_enforced"] is True and v["method"] == "log+anchors"


def test_wait_loop_logs_each_reason_once_and_fails_on_permanent(tmp_path, monkeypatch):
    s = bytes(range(32))
    monkeypatch.setattr(secret, "generate", lambda: s)  # the session generates this secret
    r, f, sha = T.make_v0_repo(tmp_path, secret.commitment(s))
    saved = f.read_bytes()
    f.unlink()  # not there yet: transient
    clock = T.Clock(T.a_time() - 1000)
    calls = []

    def sleep(x):
        calls.append(x)
        if len(calls) == 4:
            f.write_bytes(saved)
        clock.sleep(1)

    priv, pub = keypair()
    lg = tmp_path / "w.jsonl"
    rc = session.run_session(
        v0_path=f,
        repo=r,
        v0_commit=sha,
        v0_repo_path="v0.json",
        owner_pub=pub,
        expect_fingerprint=seal.fingerprint(pub),
        secret_dir=tmp_path / "ss",
        beacon_path=tmp_path / "b.json",
        log_path=lg,
        argv=[sys.executable, "-c", T.ACK],
        require_bls=T.HAVE_BLS,
        now=clock.now,
        sleep=sleep,
        fetch=T.fake_fetch,
        exists=lambda r: False,
        poll_s=1,
        ack_timeout=10,
    )
    text = lg.read_text()
    assert rc == 0 and text.count("v0-wait") == 1 and len(calls) >= 4

    # a permanent error (wrong chain) fails at once and is logged
    t2 = tmp_path / "p"
    t2.mkdir()
    r2, f2, sha2 = T.make_v0_repo(t2, secret.commitment(s), chain="ab" * 32)
    lg2 = tmp_path / "p.jsonl"
    slept = []
    with pytest.raises(v0mod.V0Error, match="chain"):
        session.run_session(
            v0_path=f2,
            repo=r2,
            v0_commit=sha2,
            v0_repo_path="v0.json",
            owner_pub=pub,
            expect_fingerprint=seal.fingerprint(pub),
            secret_dir=t2 / "ss",
            beacon_path=t2 / "b.json",
            log_path=lg2,
            argv=["true"],
            secret=s,
            now=clock.now,
            sleep=slept.append,
            fetch=T.fake_fetch,
            exists=lambda r: False,
        )
    assert slept == [] and "v0-refused" in lg2.read_text()


def test_secret_stdin_length_and_format_checked_at_cli(tmp_path, monkeypatch):
    priv, pub = keypair()
    pk = tmp_path / "owner.pub"
    pk.write_bytes(
        pub.public_bytes(
            serialization.Encoding.PEM, serialization.PublicFormat.SubjectPublicKeyInfo
        )
    )
    for line in ("abcd\n", "zz\n", "\n"):
        monkeypatch.setattr(sys, "stdin", io.StringIO(line))
        rc = cli.main(
            [
                "--log",
                str(tmp_path / "l.jsonl"),
                "session",
                "--v0",
                str(tmp_path / "v0.json"),
                "--repo",
                str(tmp_path),
                "--v0-commit",
                "0" * 40,
                "--v0-repo-path",
                "v0.json",
                "--owner-pub",
                str(pk),
                "--expect-fingerprint",
                seal.fingerprint(pub),
                "--secret-dir",
                str(tmp_path / "sd"),
                "--beacon-out",
                str(tmp_path / "b"),
                "--secret-stdin",
                "--",
                "true",
            ]
        )
        assert rc == 2
    assert not (tmp_path / "sd").exists()


def test_broken_pipe_is_a_failed_attempt_not_a_crash(monkeypatch):
    events = []

    def boom(fd, data):
        raise BrokenPipeError

    monkeypatch.setattr(session.os, "write", boom)
    rc = session.launch(["true"], 123, on_event=lambda a, c: events.append((a, c)))
    assert rc == 1 and events == [(0, 1)]


def test_root_fd_not_inherited_by_grandchildren(tmp_path):
    out = tmp_path / "out"
    prog = (
        "import os,subprocess,sys,hashlib\n"
        "fd=int(os.environ['PHRF_ROOT_FD']); line=os.read(fd,64); v=line.decode().strip()\n"
        "a=int(os.environ['PHRF_ROOT_ACK_FD']); os.write(a,hashlib.sha256(line).hexdigest().encode()); os.close(a)\n"
        "g=subprocess.run([sys.executable,'-c',"
        '\'import os,sys\\ntry:\\n os.fstat(int(sys.argv[1]))\\n print("open")\\n'
        'except OSError:\\n print("closed")\',str(fd)],capture_output=True,text=True)\n'
        f"open({str(out)!r},'w').write(v+' '+g.stdout.strip())\n"
    )
    rc = session.launch([sys.executable, "-c", prog], 4242, ack_timeout=10)
    assert rc == 0 and out.read_text() == "4242 closed"


# ---------------- custody log ----------------


def test_log_tail_repair_and_monotonic_timestamps(tmp_path):
    p = tmp_path / "l.jsonl"
    log.append(p, "a", {}, now=10)
    log.append(
        p, "b", {}, now=5
    )  # clock went backwards: clamped, never recorded backwards
    assert [json.loads(x)["ts"] for x in p.read_text().splitlines()] == [10, 10, 10]
    kinds = [json.loads(x)["event"] for x in p.read_text().splitlines()]
    assert kinds == ["a", "clock-backwards", "b"]  # the step back is surfaced, not hidden
    assert json.loads(p.read_text().splitlines()[2])["ts_raw"] == 5
    # a crash mid-append leaves a partial final line
    with open(p, "a") as fh:
        fh.write('{"seq": 2, "ts": 11, "ev')
    with pytest.raises(log.LogCorrupt):
        log.append(p, "c", {}, now=12)
    with pytest.raises(ValueError):
        log.verify(p)
    e = log.repair(p, now=13)
    assert e["event"] == "tail-repaired" and e["data"]["removed_bytes"] > 0
    assert log.verify(p) == 4
    assert list(
        tmp_path.glob("l.jsonl.tail-*")
    )  # the partial tail is kept, not deleted
    log.append(p, "c", {}, now=14)
    assert log.verify(p) == 5
    with pytest.raises(log.LogCorrupt):
        log.repair(p)  # nothing to repair


def test_log_verify_rejects_backwards_timestamps(tmp_path):
    p = tmp_path / "l.jsonl"
    prev, lines = log.GENESIS, []
    for i, ts in enumerate((10, 5)):
        e = {
            "seq": i,
            "ts": ts,
            "actor": "x",
            "event": "e",
            "data": {},
            "prev": prev,
        }
        e["entry_sha256"] = log._h(e)
        prev = e["entry_sha256"]
        lines.append(json.dumps(e, sort_keys=True))
    p.write_text("\n".join(lines) + "\n")
    with pytest.raises(ValueError, match="backwards"):
        log.verify(p)


def test_cli_log_repair(tmp_path, capsys):
    p = tmp_path / "l.jsonl"
    log.append(p, "a", {}, now=1)
    with open(p, "a") as fh:
        fh.write("{partial")
    assert cli.main(["--log", str(p), "log-repair"]) == 0
    assert log.verify(p) == 2


# ---------------- checkout ----------------


def git(repo, *a, env=None):
    return subprocess.run(
        ["git", *a],
        cwd=repo,
        check=True,
        capture_output=True,
        text=True,
        env=env,
    ).stdout.strip()


@pytest.fixture
def repo(tmp_path):
    r = tmp_path / "repo"
    r.mkdir()
    git(r, "init", "-q")
    git(r, "config", "user.email", "t@example.org")
    git(r, "config", "user.name", "t")
    (r / "build.sbt").write_text('val galeRevision = "abc"\n')
    git(r, "add", "-A")
    git(r, "commit", "-qm", "init")
    return r


@pytest.fixture
def home(tmp_path):
    h = tmp_path / "home"
    h.mkdir()
    return h


@pytest.mark.parametrize(
    "flag",
    ["-Dsbt.global.base=/x", "-Dsbt.boot.directory=/x", "-Dsbt.ivy.home=/x"],
)
def test_sbt_relocation_flags_refused(repo, tmp_path, home, flag):
    sha = git(repo, "rev-parse", "HEAD")
    with pytest.raises(C.CheckoutError):
        C.prepare(
            repo,
            sha,
            tmp_path / "wt",
            tmp_path / "s.json",
            env={"SBT_OPTS": flag},
            home=home,
        )
    (home / ".sbtopts").write_text(flag + "\n")
    with pytest.raises(C.CheckoutError, match="sbtopts"):
        C.prepare(repo, sha, tmp_path / "wt", tmp_path / "s.json", env={}, home=home)


def test_stamp_records_repositories_and_staging_shas(repo, tmp_path, home):
    (home / ".sbt").mkdir()
    (home / ".sbt" / "repositories").write_text("[repositories]\n  local\n")
    dep = home / ".sbt" / "1.0" / "staging" / "abc123" / "gale"
    dep.mkdir(parents=True)
    git(dep, "init", "-q")
    git(dep, "config", "user.email", "t@e.org")
    git(dep, "config", "user.name", "t")
    (dep / "f").write_text("1")
    git(dep, "add", "f")
    git(dep, "commit", "-qm", "d")
    st = C.prepare(
        repo,
        git(repo, "rev-parse", "HEAD"),
        tmp_path / "wt",
        tmp_path / "s.json",
        env={},
        home=home,
    )
    assert (
        st["sbt_repositories_sha256"]
        == hashlib.sha256(b"[repositories]\n  local\n").hexdigest()
    )
    assert st["sbt_staging_clones"] == {
        "1.0/staging/abc123/gale": git(dep, "rev-parse", "HEAD")
    }


def test_global_git_templates_and_hooks_do_not_run(repo, tmp_path, home, monkeypatch):
    tpl = tmp_path / "tpl"
    (tpl / "hooks").mkdir(parents=True)
    marker = tmp_path / "hook-ran"
    hook = tpl / "hooks" / "post-checkout"
    hook.write_text(f"#!/bin/sh\ntouch {marker}\n")
    hook.chmod(0o755)
    monkeypatch.setenv("GIT_TEMPLATE_DIR", str(tpl))
    cfg = tmp_path / "gitconfig"
    cfg.write_text(f"[core]\n\thooksPath = {tpl / 'hooks'}\n")
    monkeypatch.setenv("GIT_CONFIG_GLOBAL", str(cfg))
    C.prepare(
        repo,
        git(repo, "rev-parse", "HEAD"),
        tmp_path / "wt",
        tmp_path / "s.json",
        env={},
        home=home,
    )
    assert not marker.exists()
    assert not (tmp_path / "wt" / ".git" / "hooks" / "post-checkout").exists()
