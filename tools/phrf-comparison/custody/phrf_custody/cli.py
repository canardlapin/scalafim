"""Custodian command line.  Every custodian action appends to the custody log (--log) and prints the
log head hash afterwards (the session also prints the prefix anchors of its secret-commit and v0
entries when they are written).  The owner-side commands reveal and unseal write no custody log
entry and print no head; log-repair appends its own tail-repaired entry."""
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

from . import (
    checkout,
    drand,
    log,
    root,
    seal,
    secret as sec,
    session,
    v0 as v0mod,
    whitelist,
)

ERRORS = (
    drand.BeaconError,
    root.DenylistHit,
    root.SeedsMismatch,
    seal.SealError,
    whitelist.WhitelistError,
    checkout.CheckoutError,
    v0mod.V0Error,
    session.SessionError,
    sec.RevealError,
)


def _sha40(s: str) -> str:
    if not v0mod.SHA40.fullmatch(s):
        raise argparse.ArgumentTypeError("must be a full 40-hex commit id")
    return s


def _read_secret_stdin() -> bytes:
    """Owner-assisted recovery: 64 hex chars, piped from the OWNER's own terminal (see runbook)."""
    line = sys.stdin.readline().strip()
    try:
        s = bytes.fromhex(line)
    except ValueError as e:
        raise sec.RevealError("recovered secret is not hex") from e
    if len(s) != 32:
        raise sec.RevealError("recovered secret must be 32 bytes (64 hex characters)")
    return s


def _v0_args(p):
    p.add_argument("--v0", type=Path, required=True)
    p.add_argument("--repo", type=Path, required=True)
    p.add_argument("--v0-commit", required=True, type=_sha40)
    p.add_argument("--v0-repo-path", required=True)
    p.add_argument("--v0-hash")
    p.add_argument("--round", type=int)


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(prog="phrf_custody")
    ap.add_argument("--log", type=Path, default=Path("custody.log.jsonl"))
    sub = ap.add_subparsers(dest="cmd", required=True)
    p = sub.add_parser("beacon")
    _v0_args(p)
    p.add_argument("--out", type=Path, required=True)
    p.add_argument("--allow-unverified", action="store_true")
    p = sub.add_parser(
        "session",
        help="secret commitment, wait for v0 and round A, derive root, launch runner",
    )
    _v0_args(p)
    p.add_argument("--owner-pub", type=Path, required=True)
    p.add_argument("--expect-fingerprint", required=True)
    p.add_argument("--secret-dir", type=Path, required=True)
    p.add_argument("--beacon-out", type=Path, required=True)
    p.add_argument("--allow-unverified", action="store_true")
    p.add_argument("--max-restarts", type=int, default=0)
    p.add_argument(
        "--log-anchor",
        action="append",
        default=[],
        help="owner-held prefix anchor '<seq>:<hash>' printed by an earlier session (repeatable); "
        "MANDATORY with --secret-stdin: the secret-commit anchor and the v0 anchor",
    )
    p.add_argument(
        "--require-online",
        action="store_true",
        help="refuse the local-clock fallback when round A's availability cannot be checked",
    )
    p.add_argument(
        "--ack-timeout",
        type=float,
        default=30.0,
        help="seconds the runner has to acknowledge the root; use 600 for a cold JVM/sbt start",
    )
    p.add_argument(
        "--secret-stdin",
        action="store_true",
        help="owner-assisted recovery: 64 hex chars on stdin",
    )
    p.add_argument("runner", nargs=argparse.REMAINDER)
    p = sub.add_parser("fingerprint")
    p.add_argument("pubkey", type=Path)
    p = sub.add_parser(
        "seal", help="REHEARSAL ONLY: seal a plaintext directory (the pilot never has one)"
    )
    p.add_argument("src", type=Path)
    p.add_argument("dst", type=Path)
    p.add_argument("pubkey", type=Path)
    p.add_argument("--expect-fingerprint", required=True)
    p = sub.add_parser("release")
    p.add_argument("candidate", type=Path)
    p.add_argument("out", type=Path)
    p = sub.add_parser("checkout")
    p.add_argument("repo", type=Path)
    p.add_argument("sha")
    p.add_argument("dir", type=Path)
    p.add_argument("stamp", type=Path)
    p = sub.add_parser("build")
    p.add_argument("tree", type=Path)
    p.add_argument("argv", nargs=argparse.REMAINDER)
    p = sub.add_parser(
        "reveal",
        help="OWNER SIDE, off this machine: decrypt and verify the sealed secret",
    )
    p.add_argument("sealed_secret", type=Path)
    p.add_argument("private_key", type=Path)
    p.add_argument("commitment")
    p.add_argument(
        "--allow-partial",
        action="store_true",
        help="open a secret store whose close never happened (crash after append)",
    )
    p = sub.add_parser(
        "unseal", help="OWNER SIDE, off this machine: verify and decrypt a sealed store"
    )
    p.add_argument("sealed", type=Path)
    p.add_argument("private_key", type=Path)
    p.add_argument("out", type=Path)
    p.add_argument("--allow-partial", action="store_true")
    p.add_argument(
        "--expect-tree-digest",
        help="sealed-tree digest from the out-of-band anchor (manifest v1)",
    )
    sub.add_parser(
        "log-repair", help="explicit recovery for a truncated final custody-log line"
    )
    a = ap.parse_args(argv)
    try:
        if a.cmd == "beacon":
            v = v0mod.load_v0(
                a.v0,
                repo=a.repo,
                commit=a.v0_commit,
                repo_path=a.v0_repo_path,
                expect_hash=a.v0_hash,
                expect_round=a.round,
            )
            rec = session.fetch_beacon_for_v0(
                v, a.out, require_bls=not a.allow_unverified
            )
            log.append(
                a.log,
                "beacon",
                {
                    "round": rec["round"],
                    "randomness": rec["randomness"],
                    "bls_verified": rec["bls_verified"],
                    "responses": [
                        {k: r[k] for k in r if k != "response"} for r in rec["raw"]
                    ],
                },
            )
            print(
                f"round {rec['round']} randomness {rec['randomness']} bls_verified={rec['bls_verified']}"
            )
        elif a.cmd == "session":
            if a.log_anchor and not a.secret_stdin:
                raise session.SessionError(
                    "--log-anchor is only for owner-assisted recovery (--secret-stdin)"
                )
            if a.secret_stdin and not a.log_anchor:
                raise session.SessionError(
                    "--secret-stdin requires --log-anchor for the secret-commit and v0 entries"
                )
            pub = seal.load_public_key(a.owner_pub.read_bytes())
            seal.check_fingerprint(pub, a.expect_fingerprint)
            s = _read_secret_stdin() if a.secret_stdin else None
            runner = a.runner[1:] if a.runner[:1] == ["--"] else a.runner
            rc = session.run_session(
                v0_path=a.v0,
                repo=a.repo,
                v0_commit=a.v0_commit,
                v0_repo_path=a.v0_repo_path,
                owner_pub=pub,
                expect_fingerprint=a.expect_fingerprint,
                secret_dir=a.secret_dir,
                beacon_path=a.beacon_out,
                log_path=a.log,
                argv=runner,
                secret=s,
                log_anchors=a.log_anchor,
                require_bls=not a.allow_unverified,
                require_online=a.require_online,
                max_restarts=a.max_restarts,
                ack_timeout=a.ack_timeout,
            )
            print(f"log head: {log.head(a.log)}")
            return rc
        elif a.cmd == "fingerprint":
            print(seal.fingerprint(seal.load_public_key(a.pubkey.read_bytes())))
        elif a.cmd == "seal":
            pub = seal.load_public_key(a.pubkey.read_bytes())
            print("REHEARSAL ONLY: the real pilot writes through the sealed store", file=sys.stderr)
            rec = seal.seal_tree(
                a.src, a.dst, pub, expect_fingerprint=a.expect_fingerprint
            )
            log.append(a.log, "seal-rehearsal", rec)  # no plaintext digests: see seal_tree
            print(json.dumps(rec, indent=1))
        elif a.cmd == "release":
            h = whitelist.release(json.loads(a.candidate.read_text()), a.out)
            log.append(a.log, "release", {"whitelist_sha256": h})
            print(f"whitelist sha256 {h}")
        elif a.cmd == "checkout":
            st = checkout.prepare(a.repo, a.sha, a.dir, a.stamp)
            log.append(a.log, "checkout", st)
            print(json.dumps(st, indent=1))
        elif a.cmd == "build":
            argv2 = a.argv[1:] if a.argv[:1] == ["--"] else a.argv
            rc = checkout.run_build(a.tree, argv2)
            log.append(a.log, "build", {"argv": argv2, "rc": rc})
            return rc
        elif a.cmd == "reveal":
            from cryptography.hazmat.primitives import serialization

            k = serialization.load_pem_private_key(a.private_key.read_bytes(), None)
            s = sec.reveal(a.sealed_secret, k, a.commitment, allow_partial=a.allow_partial)
            print(f"revealed secret verified against commitment; secret hex: {s.hex()}")
            return 0
        elif a.cmd == "unseal":
            from cryptography.hazmat.primitives import serialization

            k = serialization.load_pem_private_key(a.private_key.read_bytes(), None)
            m = seal.unseal(
                a.sealed,
                k,
                a.out,
                allow_partial=a.allow_partial,
                expect_tree_digest=a.expect_tree_digest,
            )
            print(f"unsealed {len(m)} files to {a.out}")
            return 0
        elif a.cmd == "log-repair":
            e = log.repair(a.log)
            print(f"repaired; log head: {e['entry_sha256']}")
            return 0
        if a.cmd not in ("reveal", "unseal"):
            print(f"log head: {log.head(a.log)}")
    except ERRORS as e:
        try:
            if a.cmd not in (
                "reveal",
                "unseal",
                "log-repair",
            ):  # owner-side tools write no custody log
                log.append(a.log, "failure", {"cmd": a.cmd, "error": str(e)})
        finally:
            print(f"REFUSED: {e}", file=sys.stderr)
        if a.cmd not in ("reveal", "unseal", "log-repair"):  # the head is printed on refusal paths too
            try:
                print(f"log head: {log.head(a.log)}", file=sys.stderr)
            except (OSError, ValueError):
                pass
        return 2
    except log.LogCorrupt as e:
        print(f"REFUSED: {e}", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
