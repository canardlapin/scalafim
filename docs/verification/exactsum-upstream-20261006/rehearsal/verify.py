#!/usr/bin/env python3
"""Verify retained rehearsal evidence without build or network calls."""
import hashlib, json, tarfile
from pathlib import Path
folder=Path(__file__).resolve().parent
receipt=json.loads((folder/"receipt.json").read_text())
archive=folder/receipt["archive"]["path"]
assert hashlib.sha256(archive.read_bytes()).hexdigest()==receipt["archive"]["sha256"]
with tarfile.open(archive,"r:gz") as tar:
 for path,sha in receipt["source_sha256"].items():
  assert hashlib.sha256(tar.extractfile("source/"+path).read()).hexdigest()==sha,path
 assert hashlib.sha256(tar.extractfile("upstream/gale/numeric/ExactSum.scala").read()).hexdigest()==receipt["gale_exact_sum_sha256"]
 assert hashlib.sha256(tar.extractfile("ar-exactsum-rehearsal.patch").read()).hexdigest()==receipt["portable_patch"]["sha256"]
 for gate in receipt["accepted_gates"]:
  assert gate["exit_code"]==0 and not gate["warnings"]
  assert hashlib.sha256(tar.extractfile(gate["gate"]+".log").read()).hexdigest()==gate["log_sha256"]
assert sum(gate["total_tests"] for gate in receipt["accepted_gates"])==receipt["accepted_test_executions"]==410
print("PASS:410 accepted JVM/JS tests; source, patch, upstream accumulator and log digests match")
