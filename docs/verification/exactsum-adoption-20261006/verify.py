#!/usr/bin/env python3
"""Verify published-pin adoption evidence without running build tools."""
import hashlib,json,re,tarfile
from pathlib import Path
folder=Path(__file__).resolve().parent
receipt=json.loads((folder/"receipt.json").read_text())
archive=folder/receipt["archive"]["path"]
assert hashlib.sha256(archive.read_bytes()).hexdigest()==receipt["archive"]["sha256"]
assert receipt["gale_revision"]=="54e73f8e8f1218c4cb115a850aa80e1a36c6978f"
assert receipt["source_overrides"]==[]
with tarfile.open(archive,"r:gz") as tar:
 for path,sha in receipt["source_sha256"].items():
  assert hashlib.sha256(tar.extractfile("source/"+path).read()).hexdigest()==sha,path
 assert hashlib.sha256(tar.extractfile("source/build.sbt").read()).hexdigest()==receipt["build_sbt_sha256"]
 log=tar.extractfile("published-default.log").read().decode()
 totals=re.findall(r"Passed: Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)",log)
 assert totals==[("155","0","0","155"),("153","0","0","153"),("51","0","0","51"),("51","0","0","51")]
 assert "[warn]" not in log
 assert json.load(tar.extractfile("published-default.json"))["exit_code"]==0
 stage=json.load(tar.extractfile("gale-stage.json"))
 assert stage["head"]==receipt["gale_revision"] and stage["exactsum_sha256"]==receipt["gale_exactsum_sha256"]
 classes=json.load(tar.extractfile("classpath.json"))
 assert classes["fit_ravel_core_head"]=="9c5669399ab8e2a11402e71973dd5f1e2f2c13f4"
 assert len(classes["classpaths"]["fit-jvm"]["ravel_core_entries"])==1
 assert len(classes["classpaths"]["fit-js"]["ravel_core_entries"])==1
 assert json.load(tar.extractfile("shutdown.json"))["exit_code"]==0
assert receipt["passing_test_executions"]==410
print("PASS:410 tests; published Gale identity; source/archive digests; single fit Ravel core; clean shutdown")
