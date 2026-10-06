#!/usr/bin/env python3
"""Verify the bounded precision rehearsal and retain the full-module failure."""
import hashlib
import json
import re
import tarfile
from pathlib import Path

folder = Path(__file__).resolve().parent
receipt = json.loads((folder / "receipt.json").read_text())
manifest = json.loads((folder / "source-manifest.json").read_text())
for name, digest in receipt["archives"].items():
    assert hashlib.sha256((folder / name).read_bytes()).hexdigest() == digest, name
assert hashlib.sha256((folder / "source-only.patch").read_bytes()).hexdigest() == receipt["source_only_patch_sha256"]
with tarfile.open(folder / "native-linux-focused.tar.gz", "r:gz") as tar:
    run = json.load(tar.extractfile("run.json"))
    assert run["conclusion"] == "success" and run["headSha"] == receipt["native_head"]
    for label, count in [("fit-jvm", 47), ("fit-js", 47), ("laws-jvm", 4), ("laws-js", 4)]:
        assert tar.extractfile(label + "-exit-code.txt").read().strip() == b"0"
        log = tar.extractfile(label + ".log").read().decode()
        assert f"Passed: Total {count}, Failed 0, Errors 0, Passed {count}" in log
    assert "admitted 23/24" in tar.extractfile("laws-js.log").read().decode()
    assert "admitted 24/24" in tar.extractfile("laws-jvm.log").read().decode()
    for line in tar.extractfile("source-sha256.txt").read().decode().splitlines():
        digest, source = line.split(maxsplit=1)
        if source in manifest["sources"]:
            assert manifest["sources"][source] == digest, source
        if source.endswith("CompactConditionRuntimeSuite.scala"):
            assert digest == receipt["cohort"]["source_sha256"]
    patch = tar.extractfile("source/tools/diagnostics/paired-residual-gale-source.patch").read()
    assert hashlib.sha256(patch).hexdigest() == "a18e2746e43e0414436d69c23534df74c3220c42d8ab5dc350fd032adc49736c"
with tarfile.open(folder / "local-gates-and-sources.tar.gz", "r:gz") as tar:
    prefix = "scalafim-compact-paired-adapter-packet-20261006/local-gates/"
    for label, passed, total in [("full-fit-jvm", 686, 686), ("full-fit-js", 629, 629),
                                  ("full-first-level-jvm", 83, 84), ("full-first-level-js", 83, 84)]:
        log = tar.extractfile(prefix + label + ".log").read().decode()
        if passed == total:
            assert f"Passed: Total {total}, Failed 0, Errors 0, Passed {passed}" in log
        else:
            assert f"Failed: Total {total}, Failed 1, Errors 0, Passed {passed}" in log
            request = re.search(r"kernel compiler requests ([\d.Ee+-]+) training matrix cells, maximum is (\d+)", log)
            assert request and float(request.group(1)) == 2831220 and int(request.group(2)) == 2000000
assert not receipt["cohort"]["accuracy_and_work_limits_changed"]
print("PASS: native102 tests, unchanged Linux cohort23/24; fit gates; full-module LWU failure retained")
