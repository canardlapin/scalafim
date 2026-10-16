#!/usr/bin/env python3
"""Verify retained bytes without extracting archives or changing Mote state."""

import hashlib
import gzip
import json
from pathlib import Path
import tarfile
import tempfile


def digest(path):
    with path.open("rb") as source:
        return hashlib.file_digest(source, "sha256").hexdigest()


def main():
    root = Path(__file__).resolve().parent
    repository = root.parents[2]
    custody = json.loads((root / "custody.json").read_text())
    for bundle in custody["bundles"]:
        combined = hashlib.sha256()
        size = 0
        with tempfile.TemporaryFile() as restored:
            for part in bundle["parts"]:
                path = root / part["path"]
                assert path.stat().st_size == part["bytes"], path
                assert digest(path) == part["sha256"], path
                with path.open("rb") as source:
                    while block := source.read(1024 * 1024):
                        combined.update(block)
                        size += len(block)
                        restored.write(block)
            assert size == bundle["bytes"], bundle["name"]
            assert combined.hexdigest() == bundle["sha256"], bundle["name"]
            folder = root / "evidence" / bundle["name"]
            original = json.loads((folder / "artifact-manifest.json").read_text())
            assert original["bundle"]["sha256"] == bundle["sha256"]
            assert digest(folder / "bundle-manifest.json") == original["bundle"]["manifestSha256"]
            manifest_bytes = (folder / "bundle-manifest.json").read_bytes()
            expected = {entry["object"] for entry in json.loads(manifest_bytes)["files"]}
            restored.seek(0)
            objects = set()
            with tarfile.open(fileobj=restored, mode="r:gz") as archive:
                assert archive.extractfile("manifest.json").read() == manifest_bytes
                for member in archive:
                    if member.name.startswith("objects/"):
                        assert member.isfile(), member.name
                        with archive.extractfile(member) as source:
                            actual = hashlib.file_digest(source, "sha256").hexdigest()
                        assert member.name == "objects/" + actual, member.name
                        objects.add(member.name)
            assert objects == expected, bundle["name"]
        print(f"Verified {bundle['name']}: {size} bytes, {len(objects)} objects")
    for entry in custody["worktreeOverlayFiles"]:
        assert digest(root / entry["path"]) == entry["sha256"], entry["path"]
        if entry.get("compression") == "gzip":
            original = gzip.decompress((root / entry["path"]).read_bytes())
            assert hashlib.sha256(original).hexdigest() == entry["originalSha256"]
            assert len(original) == entry["originalBytes"]
    snapshot = json.loads((root / "mote-snapshot.json").read_text())
    for entry in snapshot["files"]:
        assert digest(repository / entry["path"]) == entry["sha256"], entry["path"]
    print(f"Verified {len(custody['worktreeOverlayFiles'])} overlay files and {len(snapshot['files'])} Mote files")
    print("Historical byte preservation verified; see the current development-machine handoff for tracker status.")


if __name__ == "__main__":
    main()
