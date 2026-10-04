"""Independently recompute the six-response DEV reference-search panel metrics."""
import json
import math
from pathlib import Path
import sys

records = [json.loads(line) for line in Path(sys.argv[1]).read_text().splitlines()
           if line.startswith('{"kind":"oracle-adequacy"')]
assert len(records) == 1
record = records[0]
assert record["responses"] == 6 and len(record["comparisons"]) == 6
assert {(c["snr"], c["seed"], c["voxel"]) for c in record["comparisons"]} == {
    (snr, seed, voxel) for snr, seed in [(1.0, 101), (0.5, 102)] for voxel in range(3)
}
maxima = [0.0, 0.0, 0.0]
for comparison in record["comparisons"]:
    coarse, dense = comparison["coarse"], comparison["dense"]
    for result, starts, levels in [(coarse, 4, 14), (dense, 6, 16)]:
        assert not result["failures"] and len(result["starts"]) == starts
        assert all(math.isfinite(value) for value in result["coordinates"] + result["amplitudes"] + [result["energy"]])
        for start in result["starts"]:
            assert start["requestedLevels"] == start["completedLevels"] == levels
            assert start["termination"] == "DepthComplete" and start["sweeps"] <= 256
    differences = [
        abs(coarse["coordinates"][0] - dense["coordinates"][0]),
        2 * math.sqrt(2 * math.log(2)) * abs(math.exp(coarse["coordinates"][1]) - math.exp(dense["coordinates"][1])),
        math.sqrt(sum((a - b) ** 2 for a, b in zip(coarse["amplitudes"], dense["amplitudes"])) /
                  sum(b ** 2 for b in dense["amplitudes"])),
    ]
    for i, key in enumerate(["latency", "width", "relativeAmplitude"]):
        assert math.isclose(differences[i], comparison["difference"][key], rel_tol=1e-9, abs_tol=1e-14)
        maxima[i] = max(maxima[i], differences[i])
for value, key, limit in zip(maxima, ["maxLatency", "maxWidth", "maxRelativeAmplitude"], [.002, .005, 1e-4]):
    assert math.isclose(value, record[key], rel_tol=1e-9, abs_tol=1e-14)
    assert value <= limit
assert record["unresolved"] == 0 and record["adequate"]
print(json.dumps({"platform": record["platform"], "sourceId": record["sourceId"],
                  "responses": 6, "recomputedMaxima": maxima, "adequate": True}, indent=2))
