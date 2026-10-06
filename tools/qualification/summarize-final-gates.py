#!/usr/bin/env python3
"""Require executed scientific stages and retain all failure outcomes."""
import hashlib,json,re,sys
from pathlib import Path
folder=Path(sys.argv[1]);platform=sys.argv[2];stages=[]
for name in ["budget-kernel","compact-cohort","lwu-accuracy","full-fit"]:
    content=(folder/(name+".log")).read_bytes()
    log=re.sub(r"\x1b\[[0-9;]*[A-Za-z]","",content.decode(errors="replace"))
    code=int((folder/(name+"-exit-code.txt")).read_text())
    totals=[{"total":int(a),"failed":int(b),"errors":int(c),"passed":int(d),"skipped":int(e or 0)}
        for a,b,c,d,e in re.findall(r"(?:Passed|Failed): Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)(?:, Skipped (\d+))?",log)]
    errors=[]
    if code!=0:errors.append("stage command failed")
    if not totals:errors.append("stage produced no test result")
    if any(row["failed"] or row["errors"] for row in totals):errors.append("tests failed")
    if name=="lwu-accuracy":
        if totals!=[{"total":1,"failed":0,"errors":0,"passed":1,"skipped":0}]:
            errors.append("LWU selector did not execute exactly one clean original test")
        for snr in ["1.00","0.50"]:
            if not re.search(r"\[milestone\] LWU SNR "+re.escape(snr)+r":",log):
                errors.append("missing frozen LWU SNR "+snr+" outcome")
    if name=="compact-cohort" and "[compact-condition] admitted" not in log:
        errors.append("missing unchanged compact cohort outcome")
    lwu_outcomes=[]
    if name=="lwu-accuracy":
        for snr,percent in re.findall(r"\[milestone\] LWU SNR ([0-9.]+): admitted ([0-9.]+)%",log):
            lwu_outcomes.append({"snr":float(snr),"admitted_percent":float(percent),
                "declared_caveat":"rho-weak admission below95percent" if float(percent)<95.0 else None})
    stages.append({"stage":name,"exit_code":code,"totals":totals,"validation_errors":errors,"lwu_outcomes":lwu_outcomes,
        "log_sha256":hashlib.sha256(content).hexdigest()})
summary={"platform":platform,"stages":stages,"qualified":all(not s["validation_errors"] for s in stages)}
(folder/"gate-receipt.json").write_text(json.dumps(summary,indent=2)+"\n")
print(json.dumps(summary))
sys.exit(0 if summary["qualified"] else 1)
