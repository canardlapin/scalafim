#!/usr/bin/env python3
"""Validate immutable native qualification artifacts and scientific scope."""
import hashlib,json,tarfile
from pathlib import Path
folder=Path(__file__).resolve().parent
receipt=json.loads((folder/"receipt.json").read_text());archive=folder/receipt["archive"]["path"]
assert hashlib.sha256(archive.read_bytes()).hexdigest()==receipt["archive"]["sha256"]
assert receipt["run_conclusion"]=="success" and receipt["source_overrides"]==[]
assert receipt["native_source_commit"]=="19f535758f3b1345d8667c055642ec464ab7a7c2"
total=0;manifest=None
with tarfile.open(archive,"r:gz") as tar:
 run=json.load(tar.extractfile("run.json"));assert run["conclusion"]=="success" and run["headSha"]==receipt["native_source_commit"]
 for platform,counts in [("JVM",[29,4,1,686]),("JS",[29,4,1,629])]:
  current=json.load(tar.extractfile(platform+"/final-inputs.json"))
  if manifest is None:manifest=current
  else:assert current==manifest
  assert current["gale_revision"]==receipt["gale_revision"]=="b56a9dd0b8ad479621a3594880f90c9add8c2824"
  assert current["source_sha256"]==receipt["source_sha256"] and current["source_overrides"]==[]
  assert current["lwu_scientific_inputs"]==receipt["lwu_scientific_inputs"]
  assert current["selector"]["count"]==1 and current["selector"]["timeout_minutes_inherited"]==60
  assert tar.extractfile(platform+"/source-commit.txt").read().decode().strip()==receipt["native_source_commit"]
  gate=json.load(tar.extractfile(platform+"/gate-receipt.json"));assert gate["qualified"]
  assert [sum(row["passed"] for row in stage["totals"]) for stage in gate["stages"]]==counts
  for stage in gate["stages"]:
   assert stage["exit_code"]==0 and not stage["validation_errors"]
   assert all(row["failed"]==row["errors"]==row["skipped"]==0 for row in stage["totals"])
   assert hashlib.sha256(tar.extractfile(platform+"/"+stage["stage"]+".log").read()).hexdigest()==stage["log_sha256"]
  host=tar.extractfile(platform+"/host.txt").read().decode();assert 'openjdk version "17.0.20.1"' in host and "v24.21.0" in host
  for point in receipt["platforms"][platform]["lwu"]:
   assert point["p95_latency_seconds"]<=.02 and point["p95_fwhm_seconds"]<=.05 and point["p95_relative_amplitude"]<=.001
   assert point["admission_caveat"] and point["admitted_percent"]<95
  total+=sum(counts)
assert total==receipt["passing_test_executions"]==1383
print("PASS:1383 native tests, exact source/defaultURI, unchanged LWU accuracy gates, admission caveat retained")
