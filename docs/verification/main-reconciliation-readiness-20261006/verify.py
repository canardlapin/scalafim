#!/usr/bin/env python3
"""Verify CI diagnosis, retained gates and exact tested lifecycle repair."""
import hashlib
import json
from pathlib import Path
import re
import subprocess
import tarfile

ROOT = Path(__file__).resolve().parents[3]
EVIDENCE = Path(__file__).resolve().parent
RECEIPT = json.loads((EVIDENCE / "receipt.json").read_text())
WORKFLOW = ".github/workflows/first-level.yml"
workflow = (ROOT / WORKFLOW).read_bytes()
assert workflow == subprocess.check_output(["git", "show", f"{RECEIPT['base_commit']}:{WORKFLOW}"], cwd=ROOT)
assert hashlib.sha256(workflow).hexdigest() == RECEIPT["workflow_sha256"]
for path, digest in RECEIPT["source_sha256"].items():
    assert hashlib.sha256((ROOT / path).read_bytes()).hexdigest() == digest, path
archive = EVIDENCE / RECEIPT["archive"]["path"]
assert hashlib.sha256(archive.read_bytes()).hexdigest() == RECEIPT["archive"]["sha256"]
with tarfile.open(archive, "r:gz") as bundle:
    for path, digest in RECEIPT["source_sha256"].items():
        assert hashlib.sha256(bundle.extractfile("source/" + path).read()).hexdigest() == digest, path
    lifecycle = bundle.extractfile("lifecycle-final.log").read().decode()
    totals = re.findall(r"Passed: Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)", lifecycle)
    assert totals == [("10", "0", "0", "10")] * 15
    assert len(re.findall(r"\[sbt-warm\] fitJVM/testOnly scalafim.fmri.fit.profile.ParallelBlockExecutorSuite: exit 0", lifecycle)) == 15
    assert "set fitJVM / coverageEnabled := false: exit 1" in lifecycle
    assert json.load(bundle.extractfile("lifecycle-final.json"))["exit_code"] == 1
    assert json.load(bundle.extractfile("baseline-jvm.json"))["exit_code"] == 0
    matching = json.load(bundle.extractfile("baseline-js-node2421.json"))
    assert matching["exit_code"] == 0 and matching["node_version"] == "v24.21.0"
    assert json.load(bundle.extractfile("diagnostic-js-node2421.json"))["exit_code"] == 0
    assert json.load(bundle.extractfile("stop-after-diagnostics.json"))["exit_code"] == 0
subprocess.run(["git", "diff", "--check", "--", *RECEIPT["source_sha256"], str(EVIDENCE.relative_to(ROOT))], cwd=ROOT, check=True)
print("PASS:150 lifecycle tests; compact JVM/JS repro; withdrawn workflow change; exact source/archive hashes")
