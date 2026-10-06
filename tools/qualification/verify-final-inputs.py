#!/usr/bin/env python3
"""Require the complete frozen final candidate before executing qualification."""
import hashlib,json,os,re,subprocess
from pathlib import Path
root=Path(__file__).resolve().parents[2]
manifest_path=root/"tools/qualification/final-inputs.json"
if not manifest_path.is_file():
    raise SystemExit("Final LWU source is not frozen: final-inputs.json is deliberately absent")
manifest=json.loads(manifest_path.read_text())
assert manifest["status"]=="frozen-final-candidate"
expected="b56a9dd0b8ad479621a3594880f90c9add8c2824"
revision=re.search(r'lazy val galeRevision = "([0-9a-f]{40})"', (root/"build.sbt").read_text()).group(1)
assert revision==manifest["gale_revision"]==expected
for name in ["JAVA_TOOL_OPTIONS","JDK_JAVA_OPTIONS"]:
    assert not os.environ.get(name),name
assert ".build=" not in os.environ.get("SBT_OPTS","")
for path,digest in manifest["source_sha256"].items():
    assert hashlib.sha256((root/path).read_bytes()).hexdigest()==digest,path
cohort=root/"modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/profile/CompactConditionRuntimeSuite.scala"
assert hashlib.sha256(cohort.read_bytes()).hexdigest()=="72f99bedd386a9c5721aeb214d08ee429aa3be1d0d92b96233e751f0db24aadb"
assert manifest["lwu_scientific_inputs"]=={
    "sample_size":100,"snr":[1.0,0.5],"seeds":[111,112],"oracle_nodes":[26,11,9],
    "latency_tolerance_seconds":0.02,"fwhm_tolerance_seconds":0.05,"relative_amplitude_tolerance":0.001}
output=Path(os.environ["SCALAFIM_QUALIFICATION_DIR"])
output.mkdir(parents=True,exist_ok=True)
(output/"final-inputs.json").write_text(json.dumps(manifest,indent=2)+"\n")
print("Final public Gale pin and frozen scientific source verified")
