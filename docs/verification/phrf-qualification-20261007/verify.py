#!/usr/bin/env python3
"""Verify the local evidence packet, never promote it to epic qualification."""
import hashlib
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
manifest = json.loads((HERE / "manifest.json").read_text())
for scope, root in (("sources", ROOT), ("artifacts", HERE)):
    for name, expected in manifest[scope].items():
        actual = hashlib.sha256((root / name).read_bytes()).hexdigest()
        assert actual == expected, f"{scope} hash changed: {name}"

audit = json.loads((HERE / "family-audit.json").read_text())
assert audit["qualification"] == "not-admitted"
records = audit["result"]["records"]
assert len(records) == 48
assert len(audit["result"]["geometry"]) == 12
assert len(audit["result"]["scenarios"]) == 48
assert all(s["status"] == "Fail" and not s["ciPass"] for s in audit["result"]["scenarios"])
for g in audit["result"]["geometry"]:
    assert g["finiteOriginalAugmentedSigmaLower"] > 0
exact = [r for r in records if r["mode"] == "ExactShape" and r["emitted"]]
corrected = [r for r in records if r["mode"] == "CorrectedReference" and r["emitted"]]
assert len(exact) == len(corrected) == 21
assert max(r["preparedQrMaxError"] for r in exact) < 1e-7
assert max(r["preparedQrMaxError"] for r in corrected) > 1.0
assert max(r["originalQrMaxError"] for r in exact) > 1e-3
assert max(g["relativeTailDesignError"] for g in audit["result"]["geometry"]) > 0.03

high = json.loads((HERE / "stress-high-noise.json").read_text())
low = json.loads((HERE / "stress-low-noise.json").read_text())
assert high["attemptedDelivered"] == low["attemptedDelivered"] == 4
assert high["statuses"] == {"Boundary": 4}
assert low["statuses"] == {"Accepted": 4}
assert low["emitted"] == 4 and low["maxPreparedBasisResidual"] < 1e-8

for name in ("b0-baseline.json", "b0-repaired.json", "stress-resource.json"):
    receipt = json.loads((HERE / name).read_text())
    assert receipt["qualification"] == "not-admitted"
    assert receipt["attemptedVoxels"] <= receipt["voxels"]
    assert sum(receipt["attemptedStatuses"].values()) == receipt["attemptedVoxels"]
    assert receipt["executionDeadlineSeconds"] == 120
    assert "certificate is unavailable" in receipt["originalFamilyAdmission"]
    if not receipt["completed"]:
        assert "cancelled" in receipt["executionError"]
        assert receipt["executionSecondsIncludingReadsAndSink"] >= 120
    assert receipt["sampledProcessHeapMaximumBytes"] > 0

validation = json.loads((HERE / "validation.json").read_text())
assert validation["jvmPassed"] and validation["jsPassed"]
assert validation["epicAdmitted"] is False
memory = json.loads((HERE / "memory-scenario.json").read_text())
stress = json.loads((HERE / "stress-resource.json").read_text())["trialPreparationReceipt"]
n, m = stress["trials"], stress["basisRank"]
k = stress["conditions"] + stress["nuisanceColumns"]
band = n * (stress["bandwidth"] + 1)
def reference(components):
    return 8 * (components * m + components * band + 2 * components * n * k +
                components * k * k + 2 * band + k * k)
shared = 8 * stress["retainedDoubles"] + 8 * reference(10)
assert shared == memory["sharedArrayBytes"]
assert shared + 8 * reference(4) == memory["sharedPlusEightFirstOrderReferencesBytes"]
assert shared + 8 * reference(10) == memory["sharedPlusEightFullReferencesBytes"]
assert memory["sharedPlusEightFirstOrderReferencesBytes"] > memory["budgetBytes"]
print(f"verified {sum(len(manifest[k]) for k in ('sources', 'artifacts'))} hashes; diagnostic gates remain not admitted")
