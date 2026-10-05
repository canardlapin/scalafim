import hashlib
import json
import os
import subprocess
import sys
from pathlib import Path

import pytest
from cryptography.hazmat.primitives.asymmetric.x25519 import X25519PrivateKey

from phrf_custody import drand, log, root, seal, secret, session, v0 as v0mod

FIX = json.loads(
    (
        Path(__file__).parent / "fixtures" / "drand_default_round_1000000.json"
    ).read_text()
)
HAVE_BLS = drand._bls_available()
OLD = "2021-01-01T00:00:00Z"  # before round_time(1000000) = 2021-07-04
needs_bls = pytest.mark.skipif(not HAVE_BLS, reason="py_ecc not installed")


def git(repo, *a, **env):
    return subprocess.run(
        ["git", *a],
        cwd=repo,
        check=True,
        capture_output=True,
        text=True,
        env={**os.environ, **env},
    ).stdout.strip()


def make_v0_repo(
    tmp_path,
    commitment,
    *,
    round_=1000000,
    date=OLD,
    seeds_sha=None,
    chain=drand.CHAIN_HASH,
):
    r = tmp_path / "v0repo"
    if (r / "v0.json").exists():  # a test that starts several sessions over one repo
        return r, r / "v0.json", git(r, "rev-parse", "HEAD")
    r.mkdir()
    git(r, "init", "-q")
    git(r, "config", "user.email", "t@e.org")
    git(r, "config", "user.name", "t")
    doc = {
        "beacon_round": round_,
        "drand_chain_hash": chain,
        "secret_commitment": commitment,
        "seeds_py_sha256": seeds_sha or root.seeds_sha256(),
    }
    f = r / "v0.json"
    f.write_text(json.dumps(doc, indent=1, sort_keys=True) + "\n")
    git(r, "add", "v0.json")
    git(r, "commit", "-qm", "v0", GIT_COMMITTER_DATE=date, GIT_AUTHOR_DATE=date)
    return r, f, git(r, "rev-parse", "HEAD")


class Clock:
    """Fake time: sleeping advances it, so waits terminate."""

    def __init__(self, t):
        self.t = t

    def now(self):
        return self.t

    def sleep(self, s):
        self.t += s


def a_time():
    return drand.round_time(1000000)


def fake_fetch(round_, **kw):
    assert round_ == 1000000
    b = drand.verify_beacon(FIX, require_bls=kw.get("require_bls", True))
    return b, [
        {"relay": "r1", "response_sha256": "00"},
        {"relay": "r2", "response_sha256": "00"},
    ]


# ---- commit-reveal ----


def test_commitment_reveal_and_mismatch(tmp_path):
    priv = X25519PrivateKey.generate()
    pub = priv.public_key()  # throwaway
    s = secret.generate()
    assert len(s) == 32 and s != secret.generate()
    c = secret.commitment(s)
    assert c == hashlib.sha256(s).hexdigest()
    secret.seal_secret(
        s, tmp_path / "sec", pub, expect_fingerprint=seal.fingerprint(pub)
    )
    assert secret.reveal(tmp_path / "sec", priv, c) == s
    with pytest.raises(secret.RevealError):
        secret.reveal(tmp_path / "sec", priv, hashlib.sha256(b"other").hexdigest())
    with pytest.raises(secret.RevealError):
        secret.verify_reveal(bytes(32), c)
    with pytest.raises(secret.RevealError):
        secret.verify_reveal(b"short", c)
    assert (
        scan_for(tmp_path, s) is None and scan_for(tmp_path, s.hex().encode()) is None
    )


def scan_for(root_dir, needle):
    for dp, _, fn in os.walk(root_dir):
        for n in fn:
            if needle in (Path(dp) / n).read_bytes():
                return Path(dp) / n
    return None


def test_prepare_root_uses_secret_and_refuses_wrong_secret(tmp_path):
    s = bytes(range(32))
    c = secret.commitment(s)
    r, f, sha = make_v0_repo(tmp_path, c)
    v = v0mod.load_v0(f, repo=r, commit=sha, repo_path="v0.json")
    res = session.prepare_root(v, FIX, s, require_bls=HAVE_BLS)
    seeds = root.load_seeds(v.seeds_py_sha256)
    root_hex, r64 = root.derive_root(v.v0_hash, FIX["randomness"], s, seeds=seeds)
    assert (
        res["root64"] == r64
        and res["root_sha256"] == hashlib.sha256(bytes.fromhex(root_hex)).hexdigest()
    )
    with pytest.raises(secret.RevealError):
        session.prepare_root(v, FIX, bytes(32), require_bls=HAVE_BLS)


@needs_bls
def test_prepare_root_reverifies_stored_signature_with_bls(tmp_path):
    s = bytes(range(32))
    r, f, sha = make_v0_repo(tmp_path, secret.commitment(s))
    v = v0mod.load_v0(f, repo=r, commit=sha, repo_path="v0.json")
    bad = dict(FIX)
    b = bytearray(bytes.fromhex(bad["signature"]))
    b[5] ^= 1
    bad["signature"] = bytes(b).hex()
    bad["randomness"] = hashlib.sha256(
        bytes(b)
    ).hexdigest()  # structurally consistent forgery
    with pytest.raises(drand.BeaconError):
        session.prepare_root(v, bad, s)
    other = dict(FIX)
    other["round"] = 1000001
    with pytest.raises((drand.BeaconError, session.SessionError)):
        session.prepare_root(v, other, s)


def test_prepare_root_requires_bls_by_default_without_verifier(tmp_path, monkeypatch):
    monkeypatch.setattr(drand, "_bls_available", lambda: False)
    s = bytes(range(32))
    r, f, sha = make_v0_repo(tmp_path, secret.commitment(s))
    v = v0mod.load_v0(f, repo=r, commit=sha, repo_path="v0.json")
    with pytest.raises(drand.BeaconError):
        session.prepare_root(v, FIX, s)


def test_seeds_hash_in_v0_is_enforced(tmp_path):
    s = bytes(range(32))
    r, f, sha = make_v0_repo(tmp_path, secret.commitment(s), seeds_sha="1" * 64)
    v = v0mod.load_v0(f, repo=r, commit=sha, repo_path="v0.json")
    with pytest.raises(root.SeedsMismatch):
        session.prepare_root(v, FIX, s, require_bls=HAVE_BLS)


# ---- F7: v0 file binding ----


def test_v0_loading_checks(tmp_path):
    c = secret.commitment(bytes(32))
    r, f, sha = make_v0_repo(tmp_path, c)
    v = v0mod.load_v0(f, repo=r, commit=sha, repo_path="v0.json")
    assert (
        v.round == 1000000
        and v.secret_commitment == c
        and v.v0_hash == hashlib.sha256(f.read_bytes()).hexdigest()
    )
    # supplied hash / round must equal the file's
    v0mod.load_v0(
        f,
        repo=r,
        commit=sha,
        repo_path="v0.json",
        expect_hash=v.v0_hash,
        expect_round=1000000,
    )
    with pytest.raises(v0mod.V0Error):
        v0mod.load_v0(f, repo=r, commit=sha, repo_path="v0.json", expect_hash="0" * 64)
    with pytest.raises(v0mod.V0Error):
        v0mod.load_v0(f, repo=r, commit=sha, repo_path="v0.json", expect_round=999999)
    f.write_text(f.read_text() + " ")  # working copy differs from the committed v0
    with pytest.raises(v0mod.V0Error, match="committed"):
        v0mod.load_v0(f, repo=r, commit=sha, repo_path="v0.json")


def test_round_must_be_later_than_commit_date(tmp_path):
    c = secret.commitment(bytes(32))
    r, f, sha = make_v0_repo(
        tmp_path, c, date="2022-01-01T00:00:00Z"
    )  # after round 1000000's time
    with pytest.raises(v0mod.V0Error, match="later"):
        v0mod.load_v0(f, repo=r, commit=sha, repo_path="v0.json")


def test_wrong_chain_and_bad_fields(tmp_path):
    c = secret.commitment(bytes(32))
    r, f, sha = make_v0_repo(tmp_path, c, chain="ab" * 32)
    with pytest.raises(v0mod.V0Error, match="chain"):
        v0mod.load_v0(f, repo=r, commit=sha, repo_path="v0.json")
    r2 = tmp_path / "r2"
    r2.mkdir()


# ---- full session: secret never on disk, root64 via pipe ----

# runner prelude: read the root once, close the fd, acknowledge with SHA-256 of the exact bytes read
ACK = (
    "import os,sys,hashlib\n"
    "r=int(os.environ['PHRF_ROOT_FD']); line=os.read(r,64); os.close(r)\n"
    "a=int(os.environ['PHRF_ROOT_ACK_FD'])\n"
    "os.write(a,hashlib.sha256(line).hexdigest().encode()); os.close(a)\n"
    "v=line.decode().strip()\n"
)
RUNNER = ACK + "open(sys.argv[1],'w').write(v)\n"


def run_session(
    tmp_path,
    *,
    with_secret=None,
    restarts=0,
    runner=None,
    require_bls=HAVE_BLS,
    clock=None,
    seed_commit_ts="auto",
    seed_v0_ts="auto",
    seed_v0_loaded_at=None,
    seed_v0_network_checked=True,
    anchors="auto",
    exists=None,
    require_online=False,
    ack_timeout=10,
):
    clock = clock or Clock(a_time() - 1000)
    priv = X25519PrivateKey.generate()
    pub = priv.public_key()  # throwaway
    fp = seal.fingerprint(pub)
    import phrf_custody.secret as S

    orig = S.generate
    sec = with_secret
    try:
        # v0 must commit to the secret the session will generate: pre-generate it and force it
        s = sec or secret.generate()
        S.generate = lambda: s
        r, f, sha = make_v0_repo(tmp_path, secret.commitment(s))
        lgp = tmp_path / "custody.log.jsonl"
        seeded = []
        if sec is not None:  # the original session's entries, as the owner would have anchored them
            if seed_commit_ts is not None:
                ts = a_time() - 5000 if seed_commit_ts == "auto" else seed_commit_ts
                seeded.append(
                    log.append(lgp, "secret-commit", {"secret_commitment": secret.commitment(s)}, now=ts)
                )
            if seed_v0_ts is not None:
                ts = a_time() - 3000 if seed_v0_ts == "auto" else seed_v0_ts
                v = v0mod.load_v0(f, repo=r, commit=sha, repo_path="v0.json")
                seeded.append(
                    log.append(
                        lgp,
                        "v0",
                        {
                            "v0_hash": v.v0_hash,
                            "round": v.round,
                            "loaded_at": ts if seed_v0_loaded_at is None else seed_v0_loaded_at,
                            "network_checked": seed_v0_network_checked,
                        },
                        now=ts,
                    )
                )
        if anchors == "auto":
            anchors = [log.anchor_of(e) for e in seeded]
        elif anchors == "secret-commit-only":
            anchors = [log.anchor_of(seeded[0])]
        elif anchors == "v0-only":
            anchors = [log.anchor_of(seeded[1])]
        out = tmp_path / "runner_out"
        argv = runner or [sys.executable, "-c", RUNNER, str(out)]
        rc = session.run_session(
            v0_path=f,
            repo=r,
            v0_commit=sha,
            v0_repo_path="v0.json",
            owner_pub=pub,
            expect_fingerprint=fp,
            secret_dir=tmp_path / "secret.sealed",
            beacon_path=tmp_path / "beacon.json",
            log_path=tmp_path / "custody.log.jsonl",
            argv=argv,
            secret=sec,
            require_bls=require_bls,
            log_anchors=anchors or (),
            require_online=require_online,
            max_restarts=restarts,
            now=clock.now,
            sleep=clock.sleep,
            fetch=fake_fetch,
            exists=exists or (lambda rnd: False),
            ack_timeout=ack_timeout,
        )
    finally:
        S.generate = orig
    return rc, s, priv, out, v0mod.load_v0(f, repo=r, commit=sha, repo_path="v0.json")


def test_session_end_to_end_secret_never_plaintext_on_disk(tmp_path, capsys):
    rc, s, priv, out, v = run_session(tmp_path)
    assert rc == 0
    # runner got root64 on the pipe; it matches an independent derivation from the revealed secret
    revealed = secret.reveal(tmp_path / "secret.sealed", priv, v.secret_commitment)
    assert revealed == s
    _, r64 = root.derive_root(
        v.v0_hash, FIX["randomness"], revealed, seeds=root.load_seeds(v.seeds_py_sha256)
    )
    assert out.read_text() == str(r64)
    # no plaintext secret or root64 anywhere on disk (files and log), and nothing in the log derived from them
    for needle in (s, s.hex().encode(), str(r64).encode(), hex(r64)[2:].encode()):
        hit = scan_for(tmp_path, needle)
        assert hit is None or (
            hit == out and needle == str(r64).encode()
        )  # only the test runner's own output file
    lg = (tmp_path / "custody.log.jsonl").read_text()
    assert str(r64) not in lg and s.hex() not in lg and "secret-commit" in lg
    assert v.secret_commitment in lg
    log.verify(tmp_path / "custody.log.jsonl")
    assert secret.commitment(s) in capsys.readouterr().out


def test_session_owner_assisted_recovery_reuses_secret(tmp_path):
    s = bytes(range(32))
    rc, _, _, out, v = run_session(tmp_path, with_secret=s)
    assert rc == 0 and out.read_text().isdigit()
    assert "secret-recovered" in (tmp_path / "custody.log.jsonl").read_text()


def test_session_refuses_when_v0_commits_other_secret(tmp_path):
    pub = X25519PrivateKey.generate().public_key()
    r, f, sha = make_v0_repo(tmp_path, secret.commitment(b"x" * 32))
    with pytest.raises(session.SessionError, match="different secret"):
        session.run_session(
            v0_path=f,
            repo=r,
            v0_commit=sha,
            v0_repo_path="v0.json",
            owner_pub=pub,
            expect_fingerprint=seal.fingerprint(pub),
            secret_dir=tmp_path / "ss",
            beacon_path=tmp_path / "b.json",
            log_path=tmp_path / "l.jsonl",
            argv=["true"],
            wait=False,
            now=lambda: 10**10,
            sleep=lambda x: None,
            fetch=fake_fetch,
            require_bls=HAVE_BLS,
        )


def test_runner_restart_gets_same_root_and_exit_logged(tmp_path):
    marker = tmp_path / "attempts"
    prog = (
        ACK
        + f"open({str(marker)!r},'a').write(v+'\\n')\n"
        f"sys.exit(0 if open({str(marker)!r}).read().count('\\n')>=2 else 3)\n"
    )
    rc, s, priv, out, v = run_session(
        tmp_path, restarts=2, runner=[sys.executable, "-c", prog]
    )
    lines = marker.read_text().split()
    assert rc == 0 and len(lines) == 2 and lines[0] == lines[1]
    assert (tmp_path / "custody.log.jsonl").read_text().count("runner-exit") == 2
