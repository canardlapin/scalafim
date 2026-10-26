#!/usr/bin/env python3
"""Create and verify an exact external custody mirror for an sbt-warm template.

The destination is deliberately a new directory.  A receipt is written beside
it as ``<destination>.receipt.json`` so it does not change the protected tree.
"""

import argparse
import hashlib
import json
import os
import shutil
import stat
import subprocess
import sys
from pathlib import Path


class CustodyError(RuntimeError):
    pass


def _outside(path: Path, root: Path) -> bool:
    try:
        path.resolve().relative_to(root.resolve())
        return False
    except ValueError:
        return True


def mounted_root(path: Path) -> Path:
    """Find the mount point containing an existing path without guessing volumes."""
    current = path.resolve()
    while not os.path.ismount(current):
        parent = current.parent
        if parent == current:
            raise CustodyError(f"cannot identify a mount point for {path}")
        current = parent
    return current


def volume_uuid(path: Path) -> str:
    """Return the mounted macOS VolumeUUID, or fail closed on unsupported hosts."""
    if sys.platform != "darwin":
        raise CustodyError("volume identity is only implemented for macOS")
    try:
        mount = mounted_root(path)
        result = subprocess.run(
            ["diskutil", "info", "-plist", str(mount)],
            capture_output=True,
            check=False,
        )
    except OSError as error:
        raise CustodyError(f"cannot identify mounted volume for {path}: {error}") from error
    if result.returncode != 0:
        raise CustodyError(f"cannot identify mounted volume for {path}")
    try:
        import plistlib
        record = plistlib.loads(result.stdout)
        identity = record.get("VolumeUUID")
    except (ValueError, TypeError):
        identity = None
    if not isinstance(identity, str) or not identity:
        raise CustodyError(f"mounted volume for {path} has no VolumeUUID")
    return identity


def tree_fingerprint(root: Path) -> dict:
    """Hash every entry without following links; reject special/unsafe entries."""
    if root.is_symlink() or not root.is_dir():
        raise CustodyError(f"not a real directory: {root}")
    digest = hashlib.sha256()
    counts = {"directories": 0, "files": 0, "symlinks": 0}

    def record(kind: str, relative: str, mode: int, payload: str = "") -> None:
        digest.update(f"{kind}\0{relative}\0{mode:o}\0{payload}\n".encode())

    def visit(directory: Path, prefix: Path) -> None:
        for entry in sorted(directory.iterdir(), key=lambda item: item.name):
            relative = (prefix / entry.name).as_posix()
            info = entry.lstat()
            mode = stat.S_IMODE(info.st_mode)
            if stat.S_ISDIR(info.st_mode):
                counts["directories"] += 1
                record("directory", relative, mode)
                visit(entry, prefix / entry.name)
            elif stat.S_ISREG(info.st_mode):
                content = hashlib.sha256()
                with entry.open("rb") as handle:
                    for chunk in iter(lambda: handle.read(1024 * 1024), b""):
                        content.update(chunk)
                counts["files"] += 1
                record("file", relative, mode, content.hexdigest())
            elif stat.S_ISLNK(info.st_mode):
                counts["symlinks"] += 1
                record("symlink", relative, mode, os.readlink(entry))
            else:
                raise CustodyError(f"unsupported filesystem entry: {entry}")

    root_info = root.lstat()
    counts["directories"] += 1
    record("directory", ".", stat.S_IMODE(root_info.st_mode))
    visit(root, Path())
    return {"tree_digest": digest.hexdigest(), "counts": counts}


def receipt_path(destination: Path) -> Path:
    return destination.with_name(destination.name + ".receipt.json")


def create_custody(source: Path, destination: Path, cache_root: Path, expected_volume_uuid: str) -> dict:
    source, destination, cache_root = source.resolve(), destination.resolve(), cache_root.resolve()
    if not _outside(destination, cache_root):
        raise CustodyError("custody destination must be outside the cache root")
    if destination.exists() or destination.is_symlink():
        raise CustodyError(f"custody destination must be new: {destination}")
    staging = destination.with_name(destination.name + ".partial")
    if staging.exists() or staging.is_symlink():
        raise CustodyError(f"custody staging destination must be absent: {staging}")
    if volume_uuid(destination.parent) != expected_volume_uuid:
        raise CustodyError("custody destination is not on the approved mounted volume")
    before = tree_fingerprint(source)
    try:
        shutil.copytree(source, staging, symlinks=True, copy_function=shutil.copy2)
        after = tree_fingerprint(source)
        mirror = tree_fingerprint(staging)
        if before != after or after != mirror:
            raise CustodyError("source changed during custody copy or mirror is not exact")
        staging.rename(destination)
    except BaseException:
        if staging.exists() or staging.is_symlink():
            shutil.rmtree(staging, ignore_errors=True)
        raise
    receipt = {
        "version": 1,
        "source": str(source),
        "key": source.name,
        "cache_root": str(cache_root),
        "destination": str(destination),
        "volume_uuid": expected_volume_uuid,
        **after,
    }
    receipt_path(destination).write_text(json.dumps(receipt, sort_keys=True, indent=2) + "\n")
    return receipt


def verify_custody(source: Path, destination: Path, cache_root: Path, expected_volume_uuid: str) -> tuple[bool, str]:
    """Verify a fresh exact source/mirror match and its immutable receipt."""
    try:
        source, destination, cache_root = source.resolve(), destination.resolve(), cache_root.resolve()
        if not _outside(destination, cache_root):
            raise CustodyError("destination is inside cache root")
        receipt = json.loads(receipt_path(destination).read_text())
        expected = {
            "version": 1, "source": str(source), "key": source.name,
            "cache_root": str(cache_root), "destination": str(destination),
            "volume_uuid": expected_volume_uuid,
        }
        if any(receipt.get(key) != value for key, value in expected.items()):
            raise CustodyError("receipt identity does not match this custody request")
        if volume_uuid(destination) != expected_volume_uuid:
            raise CustodyError("custody volume UUID no longer matches approval")
        source_tree, mirror_tree = tree_fingerprint(source), tree_fingerprint(destination)
        if source_tree != mirror_tree:
            raise CustodyError("source and custody mirror differ")
        if receipt.get("tree_digest") != source_tree["tree_digest"] or receipt.get("counts") != source_tree["counts"]:
            raise CustodyError("receipt fingerprint is stale or altered")
        return True, "verified"
    except (OSError, ValueError, CustodyError) as error:
        return False, str(error)


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", type=Path)
    parser.add_argument("destination", type=Path)
    parser.add_argument("--cache-root", type=Path, required=True)
    parser.add_argument("--volume-uuid", required=True)
    args = parser.parse_args(argv)
    try:
        receipt = create_custody(args.source, args.destination, args.cache_root, args.volume_uuid)
    except CustodyError as error:
        print(f"template-custody: {error}", file=sys.stderr)
        return 1
    print(json.dumps(receipt, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
