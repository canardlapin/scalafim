"""Append-only, hash-chained custody log (JSON lines): locked, fsynced, anchorable.

Anchors live OUTSIDE the log (owner's notes, the manifest, the Mote issue).  Two kinds:

* a **prefix anchor** ``"<seq>:<entry hash>"`` pins one specific entry; it stays valid however many
  entries are appended later, and a rewritten or re-chained log cannot reproduce it
  (``entries(path, anchors=[...])``).  The session prints one for the ``secret-commit`` entry and for
  the ``v0`` entry, and the CLI prints the current head hash after every action;
* the **head hash**, valid only while it is still the last entry (``verify(path, expected_head=...)``).
"""
from __future__ import annotations

import fcntl
import hashlib
import json
import os
import time
from pathlib import Path

GENESIS = "0" * 64


def _h(entry: dict) -> str:
    return hashlib.sha256(json.dumps(entry, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


class LogCorrupt(ValueError):
    pass


def _last_entry(path: Path):
    text = path.read_text()
    if not text:
        return None
    if not text.endswith("\n"):
        raise LogCorrupt("custody log tail is truncated; run repair() (explicit recovery)")
    try:
        return json.loads(text.splitlines()[-1])
    except ValueError as e:
        raise LogCorrupt("custody log tail is unreadable; run repair()") from e


def repair(path: Path, *, now=None, actor: str = "backlog-worker-20260930") -> dict:
    """Explicit recovery for a truncated final line (crash mid-append): move the partial tail aside
    to ``<log>.tail-<ts>`` (kept, never deleted), then append a ``tail-repaired`` entry that records
    the SHA-256 and length of the removed bytes.  Earlier entries must still verify."""
    path = Path(path)
    raw = path.read_bytes()
    cut = raw.rfind(b"\n") + 1
    tail = raw[cut:]
    if not tail:
        raise LogCorrupt("nothing to repair: the log ends with a complete line")
    ts = int(time.time() if now is None else now)
    Path(str(path) + f".tail-{ts}").write_bytes(tail)
    path.write_bytes(raw[:cut])
    verify(path)
    return append(path, "tail-repaired", {"removed_bytes": len(tail), "removed_sha256": hashlib.sha256(tail).hexdigest()},
                  actor=actor, now=now)


def append(path: Path, event: str, data: dict, *, actor: str = "backlog-worker-20260930", now=None) -> dict:
    """Append one entry (locked, fsynced).  ``ts`` never goes backwards; ``ts_raw`` is the clock reading.
    If the clock is behind the last entry, a ``clock-backwards`` event is written first, so a step
    back is visible in the log instead of being hidden by the clamp."""
    path = Path(path)
    raw = int(time.time() if now is None else now)
    lock = os.open(str(path) + ".lock", os.O_CREAT | os.O_RDWR, 0o600)
    try:
        fcntl.flock(lock, fcntl.LOCK_EX)
        prev, seq, last_ts = GENESIS, 0, 0
        if path.exists() and path.stat().st_size:
            last = _last_entry(path)
            prev, seq, last_ts = last["entry_sha256"], last["seq"] + 1, last["ts"]
        out = None
        todo = [("clock-backwards", {"ts_raw": raw, "last_ts": last_ts})] if raw < last_ts else []
        todo.append((event, data))
        for ev, dt in todo:
            e = {"seq": seq, "ts": max(raw, last_ts), "ts_raw": raw, "actor": actor, "event": ev, "data": dt, "prev": prev}
            e["entry_sha256"] = _h(e)
            fd = os.open(path, os.O_WRONLY | os.O_APPEND | os.O_CREAT, 0o600)
            try:
                os.write(fd, (json.dumps(e, sort_keys=True) + "\n").encode())
                os.fsync(fd)
            finally:
                os.close(fd)
            prev, seq, out = e["entry_sha256"], seq + 1, e
        dfd = os.open(path.parent, os.O_RDONLY)
        try:
            os.fsync(dfd)
        finally:
            os.close(dfd)
        return out
    finally:
        fcntl.flock(lock, fcntl.LOCK_UN)
        os.close(lock)


def anchor_of(entry: dict) -> str:
    return f"{entry['seq']}:{entry['entry_sha256']}"


def parse_anchor(a: str) -> tuple[int, str]:
    try:
        seq, h = a.split(":")
        if int(seq) < 0 or len(h) != 64 or int(h, 16) < 0:
            raise ValueError
        return int(seq), h.lower()
    except ValueError as e:
        raise ValueError("anchor must be '<seq>:<64 hex>'") from e


def entries(path: Path, *, expected_head: str | None = None, anchors=()) -> list[dict]:
    """The verified entries of the log: chain, timestamps, optionally the current head, and every
    prefix anchor (``"<seq>:<hash>"``) must name exactly the entry at that sequence number."""
    verify(path, expected_head=expected_head)
    ents = [json.loads(x) for x in Path(path).read_text().splitlines()]
    for a in anchors:
        seq, h = parse_anchor(a)
        if seq >= len(ents) or ents[seq]["entry_sha256"] != h:
            raise ValueError("the custody log does not contain the anchored entry")
    return ents


def head(path: Path) -> str:
    lines = Path(path).read_text().splitlines()
    return json.loads(lines[-1])["entry_sha256"] if lines else GENESIS


def verify(path: Path, *, expected_head: str | None = None, expected_count: int | None = None) -> int:
    """Return the number of entries; raise ValueError on a broken chain or a mismatching anchor."""
    prev, n, last_ts = GENESIS, 0, 0
    for line in Path(path).read_text().splitlines():
        try:
            e = json.loads(line)
            h = e.pop("entry_sha256")
        except (ValueError, KeyError) as ex:
            raise ValueError(f"custody log unreadable at entry {n}") from ex
        if e["prev"] != prev or e["seq"] != n or _h(e) != h:
            raise ValueError(f"custody log broken at entry {n}")
        if e["ts"] < last_ts:
            raise ValueError(f"custody log timestamps go backwards at entry {n}")
        prev, n, last_ts = h, n + 1, e["ts"]
    if expected_head is not None and prev != expected_head:
        raise ValueError("custody log head does not match the anchor (truncated or rewritten)")
    if expected_count is not None and n != expected_count:
        raise ValueError("custody log length does not match the anchor")
    return n
