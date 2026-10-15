#!/usr/bin/env python3
"""Verify the retained published-provider integration packet."""

import hashlib
import json
from pathlib import Path
import re
import tarfile
import xml.etree.ElementTree as ET


folder = Path(__file__).resolve().parent
receipt = json.loads((folder / "receipt.json").read_text())
archive = folder / receipt["archive"]["path"]
assert hashlib.sha256(archive.read_bytes()).hexdigest() == receipt["archive"]["sha256"]
assert receipt["passing_test_executions"] == 2468 and receipt["declared_skips"] == 14
assert receipt["source_overrides"] == []
passed = skipped = 0
with tarfile.open(archive, "r:gz") as bundle:
    fingerprints = bundle.extractfile("source-fingerprints.json").read()
    assert hashlib.sha256(fingerprints).hexdigest() == receipt["source_fingerprints"]["sha256"]
    for name, gate in receipt["gates"].items():
        metadata = json.load(bundle.extractfile(name + ".json"))
        assert metadata["exit_code"] == 0 and metadata["source_overrides"] == []
        log_bytes = bundle.extractfile(name + ".log").read()
        assert hashlib.sha256(log_bytes).hexdigest() == gate["log_sha256"]
        log = log_bytes.decode()
        assert "[warn]" not in log
        for total, failed, errors, count, ignored in re.findall(
            r"Passed: Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)(?:, Skipped (\d+))?", log
        ):
            assert failed == errors == "0"
            passed += int(count)
            skipped += int(ignored or 0)
            assert int(count) + int(ignored or 0) == int(total)
    classes = json.load(bundle.extractfile("atlas-classpaths.json"))
    expected = {
        "image4s": receipt["pins"]["image4sRevision"],
        "locus4s": receipt["pins"]["locus4sRevision"],
        "reframe4s": receipt["pins"]["reframe4sRevision"],
        "ravel": "9c5669399ab8e2a11402e71973dd5f1e2f2c13f4",
    }
    for platform in ["jvm", "js"]:
        for name, revision in expected.items():
            providers = classes[platform]["unique_provider_roots"][name]
            assert len(providers) == 1 and providers[0]["head"] == revision
    native = ET.fromstring(bundle.extractfile("native-mni-oracle.xml").read())
    cases = native.findall(".//testcase")
    assert len(cases) == 8
    assert all(case.find(tag) is None for case in cases for tag in ["failure", "error", "skipped"])
    assert len(json.load(bundle.extractfile("native-skips.json"))) == 12
    assert len(json.load(bundle.extractfile("js-skips.json"))) == 2
    for name, code in receipt["shutdowns"].items():
        assert code == json.load(bundle.extractfile(name + ".json"))["exit_code"] == 0
    build = bundle.extractfile("source/build.sbt").read()
    assert hashlib.sha256(build).hexdigest() == receipt["build_sbt_sha256"]
assert passed == 2468 and skipped == 14
print("PASS: compiled both platforms; 2468 passed, 14 declared skips; native MNI 8/8; unique provider classpaths")
