#!/usr/bin/env python3
"""Verify the scientific failure and controlled observation provenance."""
import hashlib
import json
import tarfile
from pathlib import Path

folder = Path(__file__).resolve().parent
receipt = json.loads((folder / "receipt.json").read_text())
archive = folder / receipt["archive"]["path"]
assert hashlib.sha256(archive.read_bytes()).hexdigest() == receipt["archive"]["sha256"]
with tarfile.open(archive, "r:gz") as tar:
    assert hashlib.sha256(tar.extractfile("baseline-suite.scala").read()).hexdigest() == receipt["baseline_suite_sha256"]
    for source, digest in receipt["source_sha256"].items():
        assert hashlib.sha256(tar.extractfile("source/" + source).read()).hexdigest() == digest, source
    for path, code in receipt["exit_codes"].items():
        assert int(tar.extractfile(path + "-exit-code.txt").read()) == code
    def records(scope, lane):
        log = tar.extractfile(scope + "/" + lane + ".log").read().decode()
        return [json.loads(line) for line in log.splitlines() if line.startswith('{"kind":"compact-precision-')]
    for lane, count in receipt["matching_observation_records"].items():
        first = records("first-observation", lane)
        controlled = records("controlled-baseline-and-observation", lane)
        assert len(first) == count and first == controlled
    for scope in ["first-observation", "controlled-baseline-and-observation"]:
        run = json.load(tar.extractfile(scope + "/run.json"))
        assert run["conclusion"] == "success"
        assert "admitted 21/24" in tar.extractfile(scope + "/js.log").read().decode()
    assert "admitted 21/24" in tar.extractfile("controlled-baseline-and-observation/baseline-js.log").read().decode()
    assert "admitted 24/24" in tar.extractfile("controlled-baseline-and-observation/baseline-jvm.log").read().decode()
assert receipt["exit_codes"]["controlled-baseline-and-observation/baseline-js"] == 1
print("PASS: unchanged Linux baseline JVM24/JS21; scientific JS refusal; matching captured records; sealed sources")
