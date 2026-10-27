#!/usr/bin/env python3
"""Verify an external evidence archive and optionally restore it without overwrites."""
import argparse
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import shutil
import tarfile
import tempfile


def digest(stream):
    value = hashlib.sha256()
    for block in iter(lambda: stream.read(1024 * 1024), b""):
        value.update(block)
    return value.hexdigest()


def checked_entries(manifest):
    result = {}
    for row in manifest["entries"]:
        name = row["path"]
        path = PurePosixPath(name)
        if path.is_absolute() or ".." in path.parts or str(path) != name or name in result:
            raise ValueError(f"unsafe or duplicate manifest path: {name}")
        result[name] = row
    return result


def verify(manifest, archive):
    expected = checked_entries(manifest)
    with archive.open("rb") as stream:
        if digest(stream) != manifest["archive"]["sha256"]:
            raise ValueError("archive SHA-256 mismatch")
    if archive.stat().st_size != manifest["archive"]["bytes"]:
        raise ValueError("archive byte count mismatch")
    seen = set()
    with tarfile.open(archive, "r:") as bundle:
        for member in bundle:
            if not member.isfile() or member.name not in expected or member.name in seen:
                raise ValueError(f"unexpected, non-regular or repeated archive member: {member.name}")
            row = expected[member.name]
            if member.size != row["bytes"]:
                raise ValueError(f"member size mismatch: {member.name}")
            with bundle.extractfile(member) as stream:
                if digest(stream) != row["sha256"]:
                    raise ValueError(f"member SHA-256 mismatch: {member.name}")
            seen.add(member.name)
    if seen != set(expected):
        raise ValueError("archive membership does not match the manifest")
    return expected


def restore(manifest, archive, root):
    expected = verify(manifest, archive)
    root = root.resolve()
    root.mkdir(parents=True, exist_ok=True)
    # Validate every destination before writing any archive content.
    destinations = {}
    for name, row in expected.items():
        target = root / name
        if not target.resolve().is_relative_to(root) or any(p.is_symlink() for p in [target, *target.parents] if p != root):
            raise ValueError(f"unsafe restore destination: {name}")
        if target.exists():
            if not target.is_file():
                raise ValueError(f"existing destination is not a regular file: {name}")
            with target.open("rb") as stream:
                if digest(stream) != row["sha256"]:
                    raise ValueError(f"refusing to overwrite different bytes: {name}")
        else:
            destinations[name] = target
    with tarfile.open(archive, "r:") as bundle:
        for member in bundle:
            if member.name not in destinations:
                continue
            target = destinations[member.name]
            target.parent.mkdir(parents=True, exist_ok=True)
            fd, temporary = tempfile.mkstemp(prefix=".evidence-restore-", dir=target.parent)
            try:
                with os.fdopen(fd, "wb") as output, bundle.extractfile(member) as source:
                    shutil.copyfileobj(source, output)
                    output.flush()
                    os.fsync(output.fileno())
                with open(temporary, "rb") as stream:
                    if digest(stream) != expected[member.name]["sha256"]:
                        raise ValueError(f"restored SHA-256 mismatch: {member.name}")
                # A concurrent creator is never overwritten.
                os.link(temporary, target)
            finally:
                os.unlink(temporary)
    return len(destinations)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, required=True)
    parser.add_argument("--archive", type=Path, required=True)
    parser.add_argument("--restore-root", type=Path)
    args = parser.parse_args()
    manifest = json.loads(args.manifest.read_text())
    if args.restore_root:
        count = restore(manifest, args.archive, args.restore_root)
        print(f"verified archive; restored {count} files without overwriting existing content")
    else:
        print(f"verified archive and {len(verify(manifest, args.archive))} per-file hashes")


if __name__ == "__main__":
    main()
