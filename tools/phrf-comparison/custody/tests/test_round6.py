import functools
import json
import os
import sys

import pytest

from phrf_custody import cli, log, session
import test_secret_session as T

S = bytes(range(32))


def run(code, **kw):
    events = []
    rc = session.launch([sys.executable, "-c", code], 777, ack_timeout=kw.pop("ack_timeout", 10),
                        on_event=lambda *a: events.append(a), **kw)
    return rc, events


# ---------------- L1 / L2 / nits: process-group handling ----------------


def test_live_member_after_kill_aborts_and_logs_before_propagating(tmp_path, monkeypatch):
    """A live member under another uid answers EPERM; EPERM must not be read as 'gone'."""
    monkeypatch.setattr(session.os, "killpg", lambda pgid, sig: (_ for _ in ()).throw(PermissionError()))
    monkeypatch.setattr(session, "_live_members", lambda pgid: [424242])
    monkeypatch.setattr(session, "_reap_group", functools.partial(session._reap_group, grace=0.2))
    events = []
    with pytest.raises(session.SessionError, match="process-group members still alive"):
        session.launch([sys.executable, "-c", "pass"], 777, ack_timeout=10, on_event=lambda *a: events.append(a))
    assert events and events[-1][1] == -1 and "still alive" in events[-1][2]


def test_eperm_with_no_live_members_is_just_a_failed_attempt(monkeypatch):
    monkeypatch.setattr(session.os, "killpg", lambda pgid, sig: (_ for _ in ()).throw(PermissionError()))
    rc, events = run("pass")  # exits at once without an ack; nothing is alive per ps
    assert rc != 0 and events == [(0, rc)]


def test_leader_is_reaped_before_liveness_is_polled(monkeypatch):
    """Linux: a zombie leader answers killpg(.., 0); the code must reap first and use ps, not signal 0."""
    seen = []
    real = session._live_members

    def spy(pgid):
        try:
            os.waitpid(pgid, os.WNOHANG)
            seen.append("leader not yet reaped")
        except ChildProcessError:
            seen.append("reaped")
        return real(pgid)

    monkeypatch.setattr(session, "_live_members", spy)
    calls = []
    real_killpg = os.killpg
    monkeypatch.setattr(session.os, "killpg", lambda pgid, sig: (calls.append(sig), real_killpg(pgid, sig))[1])
    rc, _ = run("import time; time.sleep(30)", ack_timeout=0.3)
    assert rc != 0 and seen and set(seen) == {"reaped"}
    assert 0 not in calls  # signal 0 is never used to decide that a group is gone


def test_no_second_signal_once_the_group_is_empty(monkeypatch):
    calls = []
    real_killpg = os.killpg
    monkeypatch.setattr(session.os, "killpg", lambda pgid, sig: (calls.append(sig), real_killpg(pgid, sig))[1])
    rc, _ = run(T.ACK)  # clean runner: acked, exited, nothing left: no signal at all
    assert rc == 0 and calls == []
    calls.clear()
    rc, _ = run("pass")  # no ack: exactly one kill, none after the group is reaped
    assert rc != 0 and len(calls) == 1


def test_zombie_only_rows_count_as_gone(monkeypatch):
    monkeypatch.setattr(session, "_ps_rows", lambda: [(1, 99, "Z"), (2, 99, "Z+"), (3, 98, "S")])
    assert session._live_members(99) == []
    monkeypatch.setattr(session, "_ps_rows", lambda: [(1, 99, "Z"), (2, 99, "S")])
    assert session._live_members(99) == [2]


def test_real_ps_parsing_sees_a_live_process_group():
    rows = session._ps_rows()
    me = [r for r in rows if r[0] == os.getpid()]
    assert me and session._live_members(os.getpgid(0)).count(os.getpid()) == 1


# ---------------- session / CLI ----------------


def test_log_anchor_refused_on_a_fresh_session(tmp_path):
    with pytest.raises(session.SessionError, match="only for owner-assisted recovery"):
        T.run_session(tmp_path, anchors=["0:" + "0" * 64])
    assert not (tmp_path / "secret.sealed").exists()  # refused before anything was created


def test_cli_log_anchor_refused_without_secret_stdin(tmp_path, capsys):
    rc = cli.main(["--log", str(tmp_path / "l.jsonl"), "session", "--v0", "x", "--repo", str(tmp_path),
                   "--v0-commit", "0" * 40, "--v0-repo-path", "v0.json", "--owner-pub", "x",
                   "--expect-fingerprint", "x", "--secret-dir", str(tmp_path / "sd"), "--beacon-out", "b",
                   "--log-anchor", "0:" + "0" * 64, "--", "true"])
    assert rc == 2 and "only for owner-assisted recovery" in capsys.readouterr().err


def test_refusal_paths_print_the_log_head(tmp_path, capsys):
    cand = tmp_path / "c.json"
    cand.write_text("{}")
    lg = tmp_path / "l.jsonl"
    rc = cli.main(["--log", str(lg), "release", str(cand), str(tmp_path / "out")])
    err = capsys.readouterr().err
    assert rc == 2 and "REFUSED" in err and f"log head: {log.head(lg)}" in err


def test_require_online_in_recovery_needs_the_original_network_check(tmp_path):
    with pytest.raises(session.SessionError, match="network_checked"):
        T.run_session(tmp_path, with_secret=S, require_online=True, seed_v0_network_checked=False)
    assert "network-checked" in (tmp_path / "custody.log.jsonl").read_text()
    t2 = tmp_path / "ok"
    t2.mkdir()
    rc, *_ = T.run_session(t2, with_secret=S, require_online=True, seed_v0_network_checked=True)
    assert rc == 0
    t3 = tmp_path / "off"  # without --require-online a clock-only original is still accepted
    t3.mkdir()
    rc, *_ = T.run_session(t3, with_secret=S, require_online=False, seed_v0_network_checked=False)
    assert rc == 0
    rows = [json.loads(x) for x in (t3 / "custody.log.jsonl").read_text().splitlines()]
    assert [r for r in rows if r["event"] == "v0"][-1]["data"]["network_checked"] is False
