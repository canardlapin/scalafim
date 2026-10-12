#!/usr/bin/env python3
"""Verify every source and converted payload before real fsLR qualification."""
import argparse
import hashlib
import json
from pathlib import Path


def digest(path):
    result = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            result.update(block)
    return result.hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("cache", type=Path)
    args = parser.parse_args()
    lock_path = Path(__file__).with_name("assets.lock.json")
    lock = json.loads(lock_path.read_text())
    root = args.cache.resolve()
    expected = [(a["archivePath"], a) for a in lock["assets"]]
    expected += [(str(Path(d["directory"]) / f["name"]), f)
                 for d in lock["derived"] for f in d["files"]]
    verified = []
    for relative, item in expected:
        path = (root / relative).resolve()
        if root not in path.parents or not path.is_file():
            raise ValueError(f"missing or out-of-cache payload: {relative}")
        if path.stat().st_size != item["bytes"] or digest(path) != item["sha256"]:
            raise ValueError(f"locked payload differs: {relative}")
        verified.append({"path": relative, "bytes": item["bytes"], "sha256": item["sha256"]})
    print(json.dumps({"schema": "scalafim.fslr-asset-verification/1", "verified": True,
                      "lock_sha256": digest(lock_path), "files": verified}, indent=2))


if __name__ == "__main__":
    main()
