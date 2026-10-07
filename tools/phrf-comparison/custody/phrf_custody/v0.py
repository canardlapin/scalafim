"""The v0 file: round A, secret commitment and seeds hash, bound to the v0 commit (F7)."""
from __future__ import annotations

import hashlib
import json
import re
import subprocess
from dataclasses import dataclass
from pathlib import Path

from . import drand

REQUIRED = ("beacon_round", "drand_chain_hash", "secret_commitment", "seeds_py_sha256")
HEX64 = re.compile(r"[0-9a-f]{64}")


class V0Error(Exception):
    """``transient`` marks conditions that can clear by themselves (file or commit not there yet,
    working copy mid-edit); everything else is a permanent refusal."""

    def __init__(self, msg: str, transient: bool = False):
        super().__init__(msg)
        self.transient = transient


SHA40 = re.compile(r"[0-9a-f]{40}")


@dataclass(frozen=True)
class V0:
    v0_hash: str
    round: int
    secret_commitment: str
    seeds_py_sha256: str
    commit_ts: int


def _git(repo, *args) -> bytes:
    r = subprocess.run(["git", "-C", str(repo), *args], capture_output=True)
    if r.returncode:
        raise V0Error(f"git {args[0]} failed", transient=True)
    return r.stdout


def load_v0(path: Path, *, repo: Path, commit: str, repo_path: str,
            expect_hash: str | None = None, expect_round: int | None = None) -> V0:
    """Parse the v0 file; check it is byte-identical to the one committed at ``commit``, that the
    caller's v0 hash / round (if given) equal the file's, and that round A's time is later than the
    committer date of that commit."""
    if not SHA40.fullmatch(commit):
        raise V0Error("v0 commit must be the full 40-hex id")
    try:
        raw = Path(path).read_bytes()
    except OSError as e:
        raise V0Error("v0 file not readable", transient=True) from e
    h = hashlib.sha256(raw).hexdigest()
    try:
        d = json.loads(raw)
        missing = [k for k in REQUIRED if k not in d]
    except ValueError as e:
        raise V0Error("v0 file is not JSON") from e
    if missing:
        raise V0Error("v0 file lacks required fields")
    if d["drand_chain_hash"] != drand.CHAIN_HASH:
        raise V0Error("v0 names a different drand chain")
    rnd = d["beacon_round"]
    if isinstance(rnd, bool) or not isinstance(rnd, int) or rnd < 1:
        raise V0Error("bad beacon_round")
    for k in ("secret_commitment", "seeds_py_sha256"):
        if not isinstance(d[k], str) or not HEX64.fullmatch(d[k]):
            raise V0Error(f"bad {k}")
    if expect_hash is not None and expect_hash.lower() != h:
        raise V0Error("supplied v0 hash differs from the v0 file")
    if expect_round is not None and expect_round != rnd:
        raise V0Error("supplied round differs from the v0 file")
    if _git(repo, "show", f"{commit}:{repo_path}") != raw:
        raise V0Error("v0 file differs from the committed v0", transient=True)
    ts = int(_git(repo, "show", "-s", "--format=%ct", commit).strip())
    if drand.round_time(rnd) <= ts:
        raise V0Error("round A is not later than the v0 commit date")
    return V0(h, rnd, d["secret_commitment"], d["seeds_py_sha256"], ts)
