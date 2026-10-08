#!/usr/bin/env python3
"""Verify source/artifact integrity and the scope of the frozen experiments."""
import hashlib
import json
import math
from collections import defaultdict
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]


def read(name):
    return json.loads((packet / name).read_text())


manifest = read("manifest.json")
checked = 0
for group, base in (("sources", root), ("artifacts", packet)):
    for relative, expected in manifest[group].items():
        assert hashlib.sha256((base / relative).read_bytes()).hexdigest() == expected, relative
        checked += 1

for name, trials, count, records in (
    ("neighborhoods-n30.json", 30, 64, 1424),
    ("neighborhoods-n300.json", 300, 32, 656),
):
    audit = read(name)
    assert audit["qualification"] == "not-admitted"
    assert [r["geometry"]["horizon"] for r in audit["result"]] == [48, 96]
    for result in audit["result"]:
        assert result["geometry"]["trials"] == trials
        assert result["geometry"]["references"] == 8
        assert len(result["records"]) == records
        assert len(result["scenarios"]) == 2
        assert all(s["status"] == "Fail" and not s["ciPass"] for s in result["scenarios"])
        groups = defaultdict(list)
        for r in result["records"]:
            assert r["corrections"] == 1 and r["inverseApplications"] == 3
            assert r["exactFactors"] == 0 and r["retainedQueryTrials"] == 0
            for key in ("preparedRelativeError", "originalRelativeError", "exactOriginalRelativeError",
                        "relativeTailDesignError", "relativeBasisDesignError", "preparedResidual"):
                assert math.isfinite(r[key]) and r[key] >= 0
            if r["point"]["anchor"] is None:
                groups[(r["layout"], r["point"]["label"])].append(r)
        for coverage in result["coverage"]:
            selected = [v for (layout, _), v in groups.items() if layout == coverage["layout"]]
            assert len(selected) == count == coverage["attempted"]
            assert all(len(g) == 8 and {r["reference"] for r in g} == set(range(8)) for g in selected)
            assert all(sum(r["nearestTwo"] for r in g) == 2 for g in selected)
            comparisons = (
                ("anyReferenceAmplitudePasses", "preparedRelativeError", .001),
                ("anyReferenceOriginalAmplitudePasses", "originalRelativeError", .001),
                ("anyReferencePreparedQueryPasses", "preparedQueryError", .000001),
                ("anyReferenceOriginalQueryPasses", "originalQueryError", .000001),
            )
            for total, key, tolerance in comparisons:
                assert coverage[total] == sum(any(r[key] <= tolerance for r in g) for g in selected)
            assert coverage["nearestTwoAmplitudePasses"] == sum(
                any(r["nearestTwo"] and r["preparedRelativeError"] <= .001 for r in g) for g in selected)
            assert coverage["exactOriginalAmplitudePasses"] == sum(g[0]["exactOriginalRelativeError"] <= .001 for g in selected)
            for key, field in (("bestPreparedP95", "preparedRelativeError"), ("bestOriginalP95", "originalRelativeError")):
                p95 = sorted(min(r[field] for r in g) for g in selected)[math.ceil(.95 * count) - 1]
                assert coverage[key] == p95
            assert coverage["anyReferenceOriginalAmplitudePasses"] / count < .95

certificate = read("certificates.json")
assert certificate["qualification"] == "not-admitted"
points = [p for r in certificate["result"] for p in r["points"]]
boxes = [b for r in certificate["result"] for b in r["boxes"]]
assert len(points) == len(boxes) == 16
assert sum(p["refusal"] is None for p in points) == 12
assert sum(b["refusal"] is None for b in boxes) == 12
for p in points:
    assert p["retainedScalars"] == 45864 and p["normalProductTerms"] == 421200
    if p["refusal"] is None:
        assert 0 <= p["etaUpper"] < 1
        assert p["originalRelativeError"] <= p["relativeBound"] <= .001
        assert p["originalQueryError"] <= p["queryBound"]
    else:
        assert p["radius"] == .1 and p["etaUpper"] is None
        assert "must be below one" in p["refusal"]
for b in boxes:
    if b["refusal"] is None:
        assert 0 <= b["etaUpper"] < 1 and b["radius"] <= .0001
    else:
        assert b["radius"] == .001 and "must be below one" in b["refusal"]
useful_query = next(p for p in points if (p["horizon"], p["node"], p["radius"]) == (96, 0, .001))
assert useful_query["queryBound"] <= .000001

stress = read("stress-preparation.json")
assert stress["qualification"] == "not-admitted"
short, long = stress["result"]
assert short["admitted"] and short["sharedReferenceBytes"] == 204786040
assert not long["admitted"] and "24022669" in long["refusal"] and "16000000" in long["refusal"]

upstream = read("upstream.json")
assert upstream["revision"] in (root / "build.sbt").read_text()
assert upstream["state"] == "merged"
assert upstream["revision"] == upstream["resolvedBuildRevision"]
for relative, expected in upstream["resolvedSourceHashes"].items():
    assert hashlib.sha256((packet / "upstream-sources" / relative).read_bytes()).hexdigest() == expected
validation = read("validation.json")
assert validation["jvmPassed"] and validation["jsPassed"]
assert not validation["epicAdmitted"] and not validation["productionCertificateAdmitted"]
assert validation["compilerWarnings"] == []
print(f"verified {checked} hashes, bounded-work readouts and original-observation diagnostics; epic remains open")
