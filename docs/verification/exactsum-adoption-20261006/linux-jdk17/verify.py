#!/usr/bin/env python3
"""Verify the retained native Linux gate without network or build tools."""
import hashlib
import json
import re
import tarfile
from pathlib import Path

folder = Path(__file__).resolve().parent
receipt = json.loads((folder / "receipt.json").read_text())
archive = folder / receipt["archive"]["path"]
assert hashlib.sha256(archive.read_bytes()).hexdigest() == receipt["archive"]["sha256"]
with tarfile.open(archive, "r:gz") as tar:
    run = json.load(tar.extractfile("native-host/run.json"))
    assert run["status"] == "completed" and run["conclusion"] == "success"
    assert run["headSha"] == receipt["head_sha"]
    assert tar.extractfile("native-host/source-commit.txt").read().decode().strip() == run["headSha"]
    assert tar.extractfile("native-host/gate-exit-code.txt").read().strip() == b"0"
    log = tar.extractfile("native-host/gate.log").read().decode()
    totals = re.findall(r"Passed: Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)", log)
    assert totals == [("155", "0", "0", "155"), ("25", "0", "0", "25")]
    for line in tar.extractfile("native-host/source-sha256.txt").read().decode().splitlines():
        digest, source = line.split(maxsplit=1)
        assert hashlib.sha256(tar.extractfile("source/" + source).read()).hexdigest() == digest, source
    build = tar.extractfile("source/build.sbt").read().decode()
    assert 'galeRevision = "54e73f8e8f1218c4cb115a850aa80e1a36c6978f"' in build
    host = tar.extractfile("native-host/host.txt").read().decode()
    assert "x86_64" in host and "Temurin-17.0.20.1+1" in host
assert receipt["total"] == 180 and receipt["source_overrides"] == []
print("PASS: 180 native Linux/JDK17 tests; source/archive digests; runtime fingerprint")
