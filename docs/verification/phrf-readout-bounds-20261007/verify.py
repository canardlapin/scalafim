#!/usr/bin/env python3
"""Verify the frozen overlay/artifacts and the diagnostic evidence scope."""
import hashlib
import json
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]
manifest = json.loads((packet / "manifest.json").read_text())
checked = 0
for group, base in (("sources", root), ("artifacts", packet)):
    for relative, expected in manifest[group].items():
        actual = hashlib.sha256((base / relative).read_bytes()).hexdigest()
        assert actual == expected, f"hash mismatch: {relative}"
        checked += 1

audit = json.loads((packet / "horizon-audit.json").read_text())
assert audit["qualification"] == "not-admitted"
result = audit["result"]
assert len(result["records"]) == 16
assert len(result["preparationRefusals"]) == 4
assert len(result["scenarios"]) == 20
assert all(s["status"] == "Fail" and not s["ciPass"] for s in result["scenarios"])
assert result["geometry"][0]["onsets"] == result["geometry"][1]["onsets"]
for r in result["records"]:
    assert r["originalQrError2"] <= r["finiteCoefficientError2Upper"]
    assert r["signedQueryError"] <= r["signedQueryErrorUpper"]
    assert r["signedQueryArithmeticUpper"] >= 0
    assert r["queryRetainedTrialValues"] == 0
    assert r["residualGateEmits"] == (r["preparedResidual"] <= 1e-8)
    if r["mode"] == "ExactShape":
        assert r["preparedQrMaxError"] < 1e-8
        assert r["exactFactorAttempts"] == 1
    else:
        assert r["referenceInverseAttempts"] == 3
        assert r["exactFactorAttempts"] == 0

for name in ("b0-resource.json", "stress-resource.json"):
    d = json.loads((packet / name).read_text())
    assert d["qualification"] == "not-admitted"
    assert not d["completed"]
    assert d["attemptedVoxels"] < d["voxels"]
    assert sum(d["attemptedStatuses"].values()) == d["attemptedVoxels"]
    work = d["publicReadoutWork"]
    assert work["attempts"] == work["successes"] + work["failures"]
    assert work["residualGateNormalActions"] == 3 * work["residualGateRefusals"]
    assert work["residualGateResponseRowsEncoded"] == d["rows"] * work["residualGateRefusals"]
assert "kernel compiler requests" in json.loads((packet / "long-horizon-stress.json").read_text())["failure"]

probes = json.loads((packet / "gradient-probes.json").read_text())
assert set(probes) == {"JVM", "Scala.js"}
for p in probes.values():
    assert len(p["repetitions"]) == 5 and p["callsPerBatch"] == 64
    assert p["qualification"] == "not-admitted"
    for r in p["repetitions"]:
        assert abs(r["fullChecksum"] - r["envelopeChecksum"]) <= 1e-7

validation = json.loads((packet / "validation.json").read_text())
assert validation["jvmPassed"] and validation["jsPassed"]
assert not validation["epicAdmitted"]
assert validation["compilerWarnings"] == []
print(f"verified {checked} hashes and finite-array diagnostics; PHRF qualification remains open")
