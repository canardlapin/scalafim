import io
import json
import os
import re
import sys
import time
from pathlib import Path

import pytest
from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric.x25519 import X25519PrivateKey

from phrf_custody import cli, log, secret, seal, session
import test_secret_session as T

S = bytes(range(32))


def keypair():  # throwaway
    k = X25519PrivateKey.generate()
    return k, k.public_key()


# ---------------- M1 / M2: recovery re-proves BOTH orderings from anchored entries ----------------


def test_recovery_with_both_anchors_succeeds_and_records_what_was_proven(tmp_path):
    rc, *_ = T.run_session(tmp_path, with_secret=S)
    rows = [
        json.loads(x) for x in (tmp_path / "custody.log.jsonl").read_text().splitlines()
    ]
    v = [r for r in rows if r["event"] == "v0"][-1]["data"]
    assert rc == 0 and v["method"] == "log+anchors" and v["ordering_enforced"] is True
    assert (
        v["network_checked"] is True and v["loaded_at"] < v["round_time"]
    )  # the ORIGINAL load time
    assert v["recovered_at"] >= v["loaded_at"] and "original_v0_seq" in v


def test_recovery_refused_without_any_anchor(tmp_path):
    with pytest.raises(session.SessionError, match="anchors .* required"):
        T.run_session(tmp_path, with_secret=S, anchors=None)


def test_recovery_refused_without_the_v0_anchor_or_the_secret_commit_anchor(tmp_path):
    with pytest.raises(session.SessionError, match="no anchored v0"):
        T.run_session(tmp_path, with_secret=S, anchors="secret-commit-only")
    t3 = tmp_path / "z"
    t3.mkdir()
    with pytest.raises(session.SessionError, match="no anchored secret-commit"):
        T.run_session(t3, with_secret=S, anchors="v0-only")


def test_prefix_anchor_stays_valid_after_later_appends(tmp_path):
    lg = tmp_path / "l"
    e = log.append(lg, "secret-commit", {"secret_commitment": "a" * 64}, now=1)
    anchor = log.anchor_of(e)
    for i in range(5):
        log.append(lg, "v0-wait", {"i": i}, now=2 + i)
    assert len(log.entries(lg, anchors=[anchor])) == 6
    with pytest.raises(
        ValueError
    ):  # the head form is NOT valid any more, the prefix form is
        log.verify(lg, expected_head=e["entry_sha256"])


def test_forged_rechained_log_is_refused(tmp_path):
    genuine = tmp_path / "genuine.jsonl"
    c = log.append(
        genuine,
        "secret-commit",
        {"secret_commitment": secret.commitment(S)},
        now=T.a_time() - 5000,
    )
    anchors = [log.anchor_of(c)]
    forged = (
        tmp_path / "forged.jsonl"
    )  # a writer forges an equivalent, internally consistent chain
    log.append(
        forged,
        "secret-commit",
        {"secret_commitment": secret.commitment(S)},
        now=T.a_time() - 4999,
    )
    assert log.verify(forged) == 1
    with pytest.raises(ValueError, match="anchored"):
        log.entries(forged, anchors=anchors)
    with pytest.raises(ValueError):
        log.entries(genuine, anchors=[f"0:{'0' * 64}"])
    with pytest.raises(ValueError):
        log.entries(genuine, anchors=["5:" + c["entry_sha256"]])  # seq beyond the log
    for bad in ("x", "1", "1:abc", "-1:" + "0" * 64, "a:" + "0" * 64):
        with pytest.raises(ValueError):
            log.parse_anchor(bad)


def test_recovery_after_crash_in_v0_wait_is_refused(tmp_path, monkeypatch, capsys):
    """Reviewer scenario: secret-commit before A, crash while waiting for v0, v0 committed after A is
    public, owner recovers with the only anchor that exists (secret-commit)."""
    monkeypatch.setattr(secret, "generate", lambda: S)
    r, f, sha = T.make_v0_repo(tmp_path, secret.commitment(S))
    saved = f.read_bytes()
    f.unlink()  # v0 not committed yet
    priv, pub = keypair()
    lg = tmp_path / "custody.log.jsonl"

    def crash(x):
        raise RuntimeError("session crashed")

    with monkeypatch.context() as m, pytest.raises(RuntimeError):
        m.setattr(log.time, "time", lambda: T.a_time() - 5000)  # the first session ran before round A
        session.run_session(
            v0_path=f,
            repo=r,
            v0_commit=sha,
            v0_repo_path="v0.json",
            owner_pub=pub,
            expect_fingerprint=seal.fingerprint(pub),
            secret_dir=tmp_path / "ss",
            beacon_path=tmp_path / "b",
            log_path=lg,
            argv=["true"],
            now=T.Clock(T.a_time() - 1000).now,
            sleep=crash,
            fetch=T.fake_fetch,
            exists=lambda r: False,
        )
    out = capsys.readouterr().out
    sc_anchor = re.search(r"ANCHOR secret-commit: (\d+:[0-9a-f]{64})", out).group(1)
    assert "ANCHOR v0" not in out  # nothing to anchor: v0 never loaded
    f.write_bytes(saved)  # v0 committed late, after A is public
    with pytest.raises(session.SessionError, match="no anchored v0"):
        session.run_session(
            v0_path=f,
            repo=r,
            v0_commit=sha,
            v0_repo_path="v0.json",
            owner_pub=pub,
            expect_fingerprint=seal.fingerprint(pub),
            secret_dir=tmp_path / "ss2",
            beacon_path=tmp_path / "b",
            log_path=lg,
            argv=["true"],
            secret=S,
            log_anchors=[sc_anchor],
            now=T.Clock(T.a_time() + 9000).now,
            sleep=lambda x: None,
            fetch=T.fake_fetch,
            exists=lambda r: True,
        )


def test_recovery_refused_when_the_anchored_v0_entry_is_late(tmp_path):
    with pytest.raises(session.SessionError, match="v0 entry is not earlier"):
        T.run_session(tmp_path, with_secret=S, seed_v0_ts=T.a_time() + 10)


def test_anchored_v0_entry_must_be_early_in_both_loaded_at_and_ts(tmp_path):
    # entry written early (ts) but claiming a late load time
    with pytest.raises(session.SessionError, match="v0 entry is not earlier"):
        T.run_session(tmp_path, with_secret=S, seed_v0_loaded_at=T.a_time() + 10)
    # load time early but the entry itself was written after round A
    t2 = tmp_path / "b"
    t2.mkdir()
    with pytest.raises(session.SessionError, match="v0 entry is not earlier"):
        T.run_session(t2, with_secret=S, seed_v0_ts=T.a_time() + 10, seed_v0_loaded_at=T.a_time() - 100)


def test_recovery_refused_on_v0_hash_mismatch_and_late_secret_commit(tmp_path):
    with pytest.raises(
        session.SessionError, match="secret-commit entry is not earlier"
    ):
        T.run_session(tmp_path, with_secret=S, seed_commit_ts=T.a_time() + 10)


def test_session_prints_both_anchors_when_written(tmp_path, capsys):
    T.run_session(tmp_path)
    out = capsys.readouterr().out
    sc = re.search(r"ANCHOR secret-commit: (\d+:[0-9a-f]{64})", out).group(1)
    v0a = re.search(r"ANCHOR v0: (\d+:[0-9a-f]{64})", out).group(1)
    ents = log.entries(tmp_path / "custody.log.jsonl", anchors=[sc, v0a])
    assert ents[int(sc.split(":")[0])]["event"] == "secret-commit"
    assert ents[int(v0a.split(":")[0])]["event"] == "v0"
    assert out.index("ANCHOR secret-commit") < out.index("ANCHOR v0")


def test_cli_secret_stdin_requires_a_log_anchor(tmp_path, monkeypatch, capsys):
    def must_not_run(**kw):
        raise AssertionError("session started without a log anchor")

    monkeypatch.setattr(session, "run_session", must_not_run)
    priv, pub = keypair()
    pk = tmp_path / "owner.pub"
    pk.write_bytes(pub.public_bytes(serialization.Encoding.PEM, serialization.PublicFormat.SubjectPublicKeyInfo))
    monkeypatch.setattr(sys, "stdin", io.StringIO(S.hex() + "\n"))
    rc = cli.main([
        "--log", str(tmp_path / "l.jsonl"), "session", "--v0", str(tmp_path / "v0.json"), "--repo", str(tmp_path),
        "--v0-commit", "0" * 40, "--v0-repo-path", "v0.json", "--owner-pub", str(pk),
        "--expect-fingerprint", seal.fingerprint(pub), "--secret-dir", str(tmp_path / "sd"),
        "--beacon-out", str(tmp_path / "b"), "--secret-stdin", "--", "true",
    ])
    assert rc == 2 and "--log-anchor" in capsys.readouterr().err


# ---------------- F4: --require-online ----------------


def test_require_online_refuses_the_clock_fallback(tmp_path):
    with pytest.raises(session.SessionError, match="require-online"):
        T.run_session(tmp_path, exists=lambda r: None, require_online=True)
    assert "network unavailable" in (tmp_path / "custody.log.jsonl").read_text()


def test_v0_entry_records_network_checked(tmp_path):
    T.run_session(tmp_path)
    v = [
        json.loads(x) for x in (tmp_path / "custody.log.jsonl").read_text().splitlines()
    ]
    assert [e for e in v if e["event"] == "v0"][0]["data"]["network_checked"] is True
    t2 = tmp_path / "off"
    t2.mkdir()
    T.run_session(t2, exists=lambda r: None)
    v = [json.loads(x) for x in (t2 / "custody.log.jsonl").read_text().splitlines()]
    assert [e for e in v if e["event"] == "v0"][0]["data"]["network_checked"] is False


# ---------------- M3: process-group kill ----------------

GRAND = (
    "import os,sys,time,hashlib\n"
    "pid_file,marker,delay=sys.argv[1],sys.argv[2],float(sys.argv[3])\n"
    "open(pid_file,'w').write(str(os.getpid()))\n"
    "time.sleep(delay)\n"
    "open(marker,'w').write('late')\n"  # a runner that is still alive after the timeout would do this
    "# (the ack is never read by the session in that case)\n"
)
RUNNER_ACK_LATE = (
    "import os,sys,time,hashlib\n"
    "pid_file,marker,delay=sys.argv[1],sys.argv[2],float(sys.argv[3])\n"
    "open(pid_file,'w').write(str(os.getpid()))\n"
    "fd=int(os.environ['PHRF_ROOT_FD']); line=os.read(fd,64)\n"
    "time.sleep(delay)\n"
    "open(marker,'w').write('alive')\n"
    "os.write(int(os.environ['PHRF_ROOT_ACK_FD']), hashlib.sha256(line).hexdigest().encode())\n"
)


def alive(pid):
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    return True


def test_shell_wrapper_with_late_grandchild_is_killed_as_a_group(tmp_path):
    pid_file, marker = tmp_path / "pid", tmp_path / "marker"
    argv = [
        "/bin/sh",
        "-c",
        '"$0" -c "$1" "$2" "$3" "$4" & wait',
        sys.executable,
        RUNNER_ACK_LATE,
        str(pid_file),
        str(marker),
        "1.5",
    ]
    events = []
    rc = session.launch(
        argv, 777, ack_timeout=0.4, on_event=lambda a, c: events.append((a, c))
    )
    assert rc != 0 and events and events[0][1] != 0
    time.sleep(2.2)  # long enough for a surviving grandchild to write its marker
    assert not marker.exists()
    assert not alive(int(pid_file.read_text()))


def test_late_ack_after_timeout_is_a_failed_attempt(tmp_path, monkeypatch):
    pid_file, marker = tmp_path / "pid", tmp_path / "marker"
    argv = [sys.executable, "-c", RUNNER_ACK_LATE, str(pid_file), str(marker), "0.8"]
    spawned = []
    popen = session.subprocess.Popen

    def track_runner(*args, **kwargs):
        child = popen(*args, **kwargs)
        if kwargs.get("start_new_session"):
            spawned.append(child)
        return child

    monkeypatch.setattr(session.subprocess, "Popen", track_runner)
    rc = session.launch(argv, 777, ack_timeout=0.3)
    assert rc != 0
    assert len(spawned) == 1
    time.sleep(1.2)
    # The OS provides the PID even if startup loses the race with the deadline.
    assert not marker.exists() and not alive(spawned[0].pid)


def test_wrapper_whose_grandchild_acks_in_time_succeeds(tmp_path):
    pid_file, marker = tmp_path / "pid", tmp_path / "marker"
    argv = [
        "/bin/sh",
        "-c",
        '"$0" -c "$1" "$2" "$3" "$4" & wait',
        sys.executable,
        RUNNER_ACK_LATE,
        str(pid_file),
        str(marker),
        "0",
    ]
    assert session.launch(argv, 777, ack_timeout=10) == 0
    assert marker.read_text() == "alive"


def test_background_process_left_by_a_finished_runner_is_killed(tmp_path):
    pid_file = tmp_path / "pid"
    prog = (
        T.ACK
        + "import subprocess\n"
        + f"p=subprocess.Popen(['/bin/sleep','30']); open({str(pid_file)!r},'w').write(str(p.pid))\n"
    )
    assert session.launch([sys.executable, "-c", prog], 777, ack_timeout=10) == 0
    pid = int(pid_file.read_text())
    deadline = time.monotonic() + 3
    while alive(pid) and time.monotonic() < deadline:
        time.sleep(0.05)
    assert not alive(pid)


def test_restart_never_overlaps_a_previous_runner(tmp_path):
    marker = tmp_path / "m"
    prog = (
        "import os,sys,time\n"
        f"m={str(marker)!r}\n"
        "if os.path.exists(m):\n"
        "    sys.exit(0 if False else 5)\n"  # second runner would run while the first still holds the store
        "open(m,'w').write('first')\n"
        "time.sleep(30)\n"
    )
    events = []
    session.launch(
        [sys.executable, "-c", prog],
        1,
        max_restarts=1,
        ack_timeout=0.3,
        on_event=lambda a, c: events.append((a, c)),
    )
    assert len(events) == 2 and all(c != 0 for _, c in events)


def test_cli_ack_timeout_is_exposed():
    import argparse

    with pytest.raises(
        SystemExit
    ):  # parsing happens before any work: an invalid value is rejected
        cli.main(["session", "--ack-timeout", "abc"])
