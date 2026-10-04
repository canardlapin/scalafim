"""Custodian session: secret commitment, beacon, root derivation, and runner launch.

The secret lives only in this process.  It is sealed to the owner key (so the owner can reveal it
at unseal) and its SHA-256 goes into v0.  The derived root64 reaches the runner through an
inherited pipe (``PHRF_ROOT_FD``), never a file, argument or environment value, so it is not
visible in ``ps``.  Nothing derived from the secret except sha256(root) is logged.

Runner contract (format spec section 8): read the line from ``PHRF_ROOT_FD`` once, close it and
remove the variable BEFORE starting any subprocess or thread pool that forks, then write the
acknowledgement ``hex(SHA-256(<the exact bytes read>))`` (64 ASCII bytes) to ``PHRF_ROOT_ACK_FD``
and close that descriptor (write it BEFORE doing anything that can block).  The session counts an
attempt as failed (even with exit status 0) unless the acknowledgement arrives and matches: the
acknowledgement is proof that a process holding the ack descriptor received the root.  The runner
is started in its own process group; the group is killed on a missing or late ack and leftover
group members are killed after every attempt, and process-group members are confirmed gone before
any restart.  Descendants that escape the group (setsid, double-fork) are outside this check and
are forbidden by the runner contract.
"""

from __future__ import annotations

import hashlib
import json
import os
import select
import signal
import subprocess
import time
from pathlib import Path

from . import drand, log, root, secret as sec, v0 as v0mod


class SessionError(Exception):
    pass


def prepare_root(
    v0: v0mod.V0, beacon_record: dict, secret: bytes, *, require_bls: bool = True
) -> dict:
    """Re-verify the stored beacon (F7), derive the root, run the denylist check (F8)."""
    b = drand.verify_beacon(beacon_record, require_bls=require_bls)
    if b.round != v0.round:
        raise SessionError("beacon round differs from the round fixed in v0")
    sec.verify_reveal(secret, v0.secret_commitment)
    seeds = root.load_seeds(v0.seeds_py_sha256)
    root_hex, root64 = root.derive_root(v0.v0_hash, b.randomness, secret, seeds=seeds)
    res = root.denylist_check(root64, seeds=seeds)
    return {
        "root64": root64,
        "root_sha256": hashlib.sha256(bytes.fromhex(root_hex)).hexdigest(),
        "bls_verified": b.bls_verified,
        **res,
    }


def fetch_beacon_for_v0(
    v0: v0mod.V0, out: Path, *, require_bls: bool = True, fetch=drand.fetch_round
) -> dict:
    b, raw = fetch(v0.round, require_bls=require_bls)
    rec = {
        "round": b.round,
        "signature": b.signature,
        "previous_signature": b.previous_signature,
        "randomness": b.randomness,
        "chain": drand.CHAIN_HASH,
        "bls_verified": b.bls_verified,
        "raw": raw,
    }
    Path(out).write_text(json.dumps(rec, indent=1) + "\n")
    return rec


ACK_LEN = 64


def ack_for(root64: int) -> str:
    return hashlib.sha256(f"{root64}\n".encode()).hexdigest()


def _read_ack(fd: int, timeout: float) -> bytes:
    """Read up to ACK_LEN bytes from the ack pipe; stop at EOF (all holders closed it) or timeout."""
    got, deadline = b"", time.monotonic() + timeout
    while len(got) < ACK_LEN:
        left = deadline - time.monotonic()
        if left <= 0 or not select.select([fd], [], [], left)[0]:
            break
        chunk = os.read(fd, ACK_LEN - len(got))
        if not chunk:
            break
        got += chunk
    return got


def _ps_rows() -> list[tuple[int, int, str]]:
    """(pid, pgid, stat) of every process, from ``ps`` (portable across macOS and Linux)."""
    r = subprocess.run(["ps", "-A", "-o", "pid=,pgid=,stat="], capture_output=True, text=True)
    rows = []
    for line in r.stdout.splitlines():
        parts = line.split()
        if len(parts) >= 3 and parts[0].isdigit() and parts[1].isdigit():
            rows.append((int(parts[0]), int(parts[1]), parts[2]))
    return rows


def _live_members(pgid: int) -> list[int]:
    """Pids in process group ``pgid`` that are not zombies.  This, not ``killpg(.., 0)`` or an
    EPERM, is what decides that a group is gone: a zombie leader answers signal 0 on Linux, and
    EPERM can also mean a live member under another uid."""
    return [pid for pid, g, st in _ps_rows() if g == pgid and not st.startswith("Z")]


def _signal_group(pgid: int) -> None:
    try:
        os.killpg(pgid, signal.SIGKILL)
    except (ProcessLookupError, PermissionError):
        pass  # whether anything is still alive is decided by _live_members, not by this call


def _reap_group(p, pgid: int, *, kill_first: bool, grace: float = 5.0) -> int:
    """Kill (optionally) the runner's process group, reap the leader, and confirm that the
    process-group MEMBERS are gone; return the leader's exit status.

    Order matters: SIGKILL, then ``p.wait()`` at once (a zombie leader must not look alive), then
    check ``ps``.  A second signal is sent only while live members are actually visible, because a
    process-group id may be reused once its group is empty.  Limit: descendants that left the group
    (``setsid``, double-fork daemonisation) are outside this check; the runner contract forbids them
    (format spec section 8).  Raises SessionError if live members remain."""
    if kill_first:
        _signal_group(pgid)
    rc = p.wait()
    if _live_members(pgid):
        _signal_group(pgid)
        deadline = time.monotonic() + grace
        while time.monotonic() < deadline and _live_members(pgid):
            time.sleep(0.02)
        if _live_members(pgid):
            raise SessionError("runner process-group members still alive after SIGKILL; do not restart")
    return rc


def launch(argv, root64: int, *, max_restarts: int = 0, ack_timeout: float = 30.0,
           on_event=lambda *a: None) -> int:
    """Run the runner with root64 on a pipe and require its acknowledgement; relaunch (same root)
    up to max_restarts times.  The runner gets its own session and process group.  No
    acknowledgement, a wrong or late one, a broken pipe or a timeout is a failed attempt even when
    the exit status is 0.  On failure the whole process group is SIGKILLed and, after every attempt,
    leftover group members are killed, so a wrapper such as ``sh -c`` or an sbt launcher cannot leave
    the real runner alive holding the root.  Confirmation covers process-group members only (see
    ``_reap_group``); if members remain, ``on_event(attempt, rc, note)`` is called and SessionError
    propagates, so the failure is logged before the session aborts."""
    rc, expected = 1, ack_for(root64).encode()
    for attempt in range(max_restarts + 1):
        r, w = os.pipe()  # all four ends are non-inheritable by default
        ar, aw = os.pipe()
        env = dict(os.environ, PHRF_ROOT_FD=str(r), PHRF_ROOT_ACK_FD=str(aw))
        fds, p = [r, w, ar, aw], None
        try:
            p = subprocess.Popen(argv, pass_fds=(r, aw), env=env, start_new_session=True)
            os.close(r)
            os.close(aw)
            fds[0] = fds[3] = -1
            try:
                os.write(w, f"{root64}\n".encode())
            except BrokenPipeError:
                pass  # the missing acknowledgement below makes this a failed attempt
            os.close(w)
            fds[1] = -1
            acked = _read_ack(ar, ack_timeout) == expected
            rc = _reap_group(p, p.pid, kill_first=not acked)
            if not acked and rc == 0:
                rc = 1
        except SessionError as e:
            on_event(attempt, -1, str(e))
            raise
        except BaseException:
            if p is not None and p.returncode is None:  # interrupted: do not leave the runner behind
                try:
                    _reap_group(p, p.pid, kill_first=True)
                except SessionError:
                    pass
            raise
        finally:
            for fd in fds:
                if fd >= 0:
                    try:
                        os.close(fd)
                    except OSError:
                        pass
        on_event(attempt, rc)
        if rc == 0:
            break
    return rc


def _check_recovery(log_path: Path, commitment: str, v0_hash: str, a_time: int, anchors) -> dict:
    """Recovery must re-prove the ordering from the custody log, anchored by the owner.

    Required: (1) owner-held prefix anchors (``"<seq>:<hash>"``) that the verified log still
    contains; (2) an ANCHORED ``secret-commit`` entry with this commitment and ``ts < round_time(A)``;
    (3) an ANCHORED ``v0`` entry with this v0 hash and ``loaded_at`` and ``ts`` before
    ``round_time(A)``.  Without (3) a v0 committed after round A is public could be ground against the
    beacon; without anchors a log writer could forge both entries and re-chain the log."""
    if not anchors:
        raise SessionError(
            "recovery refused: owner-held log anchors for the secret-commit and v0 entries are required")
    try:
        ents = log.entries(log_path, anchors=anchors)
    except (OSError, ValueError) as e:
        raise SessionError("recovery needs a readable, verifiable custody log containing the anchored entries") from e
    pinned = {log.parse_anchor(a)[0] for a in anchors}
    sc = [e for e in ents if e["seq"] in pinned and e["event"] == "secret-commit"
          and e["data"].get("secret_commitment") == commitment]
    if not sc:
        raise SessionError("recovery refused: no anchored secret-commit entry with this commitment")
    sc = [e for e in sc if e["ts"] < a_time]
    if not sc:
        raise SessionError("recovery refused: the anchored secret-commit entry is not earlier than round A")
    v0s = [e for e in ents if e["seq"] in pinned and e["event"] == "v0" and e["data"].get("v0_hash") == v0_hash]
    if not v0s:
        raise SessionError("recovery refused: no anchored v0 entry with this v0 hash")
    v0s = [e for e in v0s if e["ts"] < a_time and isinstance(e["data"].get("loaded_at"), int)
           and e["data"]["loaded_at"] < a_time]
    if not v0s:
        raise SessionError("recovery refused: the anchored v0 entry is not earlier than round A")
    return {"secret_commit": sc[0], "v0": v0s[0]}


def _announce(entry: dict, what: str) -> None:
    """Print (and flush) an entry's prefix anchor so the owner can record it off-machine at once."""
    print(f"ANCHOR {what}: {log.anchor_of(entry)}  (record this off-machine now)", flush=True)


def run_session(*, v0_path: Path, repo: Path, v0_commit: str, v0_repo_path: str, owner_pub,
                expect_fingerprint: str, secret_dir: Path, beacon_path: Path, log_path: Path, argv,
                secret: bytes | None = None, log_anchors=(), require_bls: bool = True,
                require_online: bool = False, max_restarts: int = 0, wait: bool = True, now=time.time,
                sleep=time.sleep, fetch=drand.fetch_round, exists=drand.round_exists,
                poll_s: float = 30.0, ack_timeout: float = 30.0) -> int:
    """The whole custodian-side sequence.

    Fresh secret: generated, sealed into a fresh ``secret_dir``, ``secret-commit`` logged and its
    anchor printed.  v0 must then be found committed while the network does not yet serve round A
    (``exists``; with ``require_online`` an unreachable network is refused, otherwise it is logged and
    the local clock is used) and ``now() < round_time(A)``; the ``v0`` entry (anchor printed) records
    ``network_checked``.  Recovered secret (``secret`` given, owner-assisted): the ordering is
    re-proved from owner-anchored ``secret-commit`` and ``v0`` log entries (``_check_recovery``), so
    ``ordering_enforced`` is true only for what was actually proven."""
    recovery = secret is not None
    if log_anchors and not recovery:
        raise SessionError("--log-anchor is only for owner-assisted recovery (--secret-stdin)")
    if recovery:
        if len(secret) != 32:
            raise SessionError("recovered secret must be 32 bytes")
    else:
        secret = sec.generate()
        sec.seal_secret(secret, secret_dir, owner_pub, expect_fingerprint=expect_fingerprint)
        e = log.append(log_path, "secret-commit", {"secret_commitment": sec.commitment(secret)})
        _announce(e, "secret-commit")
    print(f"secret commitment (put this in v0): {sec.commitment(secret)}", flush=True)
    last_msg = None
    while True:  # wait for v0 to be committed with this commitment
        try:
            v0 = v0mod.load_v0(v0_path, repo=repo, commit=v0_commit, repo_path=v0_repo_path)
            break
        except v0mod.V0Error as ex:
            if not (wait and ex.transient):
                log.append(log_path, "v0-refused", {"error": str(ex)})
                raise
            if str(ex) != last_msg:  # log each distinct waiting reason once
                log.append(log_path, "v0-wait", {"reason": str(ex)})
                last_msg = str(ex)
            sleep(poll_s)
    if v0.secret_commitment != sec.commitment(secret):
        raise SessionError("v0 commits a different secret")
    loaded_at, a_time = now(), drand.round_time(v0.round)
    if recovery:
        proof = _check_recovery(log_path, sec.commitment(secret), v0.v0_hash, a_time, log_anchors)
        orig = proof["v0"]["data"]
        if require_online and not orig.get("network_checked"):
            log.append(log_path, "v0-refused", {"error": "original v0 entry was not network-checked and --require-online"})
            raise SessionError("recovery refused: the original v0 entry did not record network_checked and --require-online is set")
        log.append(log_path, "secret-recovered", {"secret_commitment": sec.commitment(secret)})
        info = {"method": "log+anchors", "secret_commit_seq": proof["secret_commit"]["seq"],
                "original_v0_seq": proof["v0"]["seq"], "recovered_at": int(loaded_at),
                "network_checked": bool(orig.get("network_checked")), "loaded_at": orig["loaded_at"]}
    else:
        seen = exists(v0.round)
        if seen is True:
            log.append(log_path, "v0-late", {"reason": "round A already published", "round_time": a_time})
            raise SessionError("v0 was loaded after round A was published")
        if seen is None:
            if require_online:
                log.append(log_path, "v0-refused", {"error": "network unavailable and --require-online"})
                raise SessionError("round A availability could not be checked and --require-online forbids the clock fallback")
            log.append(log_path, "round-check-unavailable", {"fallback": "local clock"})
        if loaded_at >= a_time:
            log.append(log_path, "v0-late", {"loaded_at": int(loaded_at), "round_time": a_time})
            raise SessionError("v0 was loaded at or after the time of round A")
        info = {"method": "clock+network", "network_checked": seen is False, "loaded_at": int(loaded_at)}
    e = log.append(log_path, "v0", {"v0_hash": v0.v0_hash, "round": v0.round, "commit": v0_commit,
                                     "round_time": a_time, "ordering_enforced": True, **info})
    _announce(e, "v0")
    target = a_time + drand.PERIOD
    while now() < target:
        sleep(min(poll_s, max(target - now(), 0.1)))
    rec = fetch_beacon_for_v0(v0, beacon_path, require_bls=require_bls, fetch=fetch)
    log.append(log_path, "beacon", {"round": rec["round"], "randomness": rec["randomness"],
                                    "bls_verified": rec["bls_verified"],
                                    "responses": [{k: r[k] for k in r if k != "response"} for r in rec["raw"]]})
    res = prepare_root(v0, rec, secret, require_bls=require_bls)
    root64 = res.pop("root64")
    log.append(log_path, "root", res)
    return launch(list(argv), root64, max_restarts=max_restarts, ack_timeout=ack_timeout,
                  on_event=lambda a, c, note=None: log.append(
                      log_path, "runner-exit", {"attempt": a, "rc": c, **({"note": note} if note else {})}))
