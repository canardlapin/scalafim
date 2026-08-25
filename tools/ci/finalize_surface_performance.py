#!/usr/bin/env python3
"""Finalize deterministic surface correctness and broad performance budgets."""

from __future__ import annotations

import argparse
import json
import math
import platform
import sys
from pathlib import Path
from typing import Any


EXPECTED = {
    "surface-compiler-allocation-v1": {
        "generated-fsaverage5-scale": (10_449, 20_480),
        "generated-cortical-scale": (163_842, 326_032),
    },
    "surface-morph-allocation-v1": {
        "generated-fsaverage5-scale": (10_449, 20_480),
        "generated-cortical-scale": (163_842, 326_032),
    },
    "surface-topology-js-v1": {
        "generated-fsaverage5-scale": (10_449, 20_480),
        "generated-cortical-scale": (163_842, 326_032),
    },
}

# Timing and process-memory ceilings are deliberately broad enough for hosted
# runners. Allocation identity and topology correspondence remain strict.
BUDGETS = {
    "compilerAllocatedBytesPerIteration": 1_048_576.0,
    "compilerIngestionAllocatedBytes": 3_221_225_472.0,
    "morphAllocationAmplification": 2.5,
    "morphElapsedNanosPerIteration": 2_000_000_000.0,
    "jsElapsedMilliseconds": 120_000.0,
    "jsObservedHeapDeltaBytes": 1_610_612_736.0,
    "jsObservedRssDeltaBytes": 1_610_612_736.0,
    "jsObservedExternalDeltaBytes": 1_610_612_736.0,
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", action="append", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    return parser.parse_args()


def load_records(paths: list[Path]) -> list[dict[str, Any]]:
    decoder = json.JSONDecoder()
    records: list[dict[str, Any]] = []
    for path in paths:
        for line in path.read_text(encoding="utf-8").splitlines():
            start = line.find('{"receipt":')
            if start < 0:
                continue
            try:
                value, _ = decoder.raw_decode(line[start:])
            except json.JSONDecodeError:
                continue
            if isinstance(value, dict) and value.get("receipt") in EXPECTED:
                records.append(value)
    return records


def main() -> int:
    args = parse_args()
    records = load_records(args.input)
    by_key: dict[tuple[str, str], dict[str, Any]] = {}
    correctness: list[dict[str, Any]] = []
    budgets: list[dict[str, Any]] = []

    def exact(name: str, observed: Any, expected: Any) -> None:
        correctness.append(
            {"name": name, "passed": observed == expected, "observed": observed, "expected": expected}
        )

    def finite(name: str, observed: Any) -> None:
        passed = isinstance(observed, (int, float)) and math.isfinite(float(observed))
        correctness.append({"name": name, "passed": passed, "observed": observed, "expected": "finite"})

    def ceiling(name: str, observed: float, maximum: float) -> None:
        budgets.append(
            {"name": name, "passed": float(observed) <= maximum, "observed": observed, "maximum": maximum}
        )

    for record in records:
        key = (str(record.get("receipt")), str(record.get("fixture")))
        exact(f"{key[0]}/{key[1]}/unique", key not in by_key, True)
        by_key[key] = record

    for receipt, fixtures in EXPECTED.items():
        for fixture, (vertices, faces) in fixtures.items():
            key = (receipt, fixture)
            exact(f"{receipt}/{fixture}/present", key in by_key, True)
            record = by_key.get(key)
            if record is None:
                continue
            exact(f"{receipt}/{fixture}/vertices", record.get("vertices"), vertices)
            exact(f"{receipt}/{fixture}/faces", record.get("faces"), faces)

            if receipt == "surface-compiler-allocation-v1":
                finite(f"{receipt}/{fixture}/checksum", record.get("checksum"))
                exact(f"{receipt}/{fixture}/packetReused", record.get("packetReused"), True)
                ceiling(
                    f"{receipt}/{fixture}/compileAllocatedBytesPerIteration",
                    float(record["compileAllocatedBytesPerIteration"]),
                    BUDGETS["compilerAllocatedBytesPerIteration"],
                )
                ceiling(
                    f"{receipt}/{fixture}/ingestionAllocatedBytes",
                    float(record["ingestionAllocatedBytes"]),
                    BUDGETS["compilerIngestionAllocatedBytes"],
                )
            elif receipt == "surface-morph-allocation-v1":
                finite(f"{receipt}/{fixture}/checksum", record.get("checksum"))
                exact(f"{receipt}/{fixture}/topologyReused", record.get("topologyReused"), True)
                ceiling(
                    f"{receipt}/{fixture}/allocationAmplification",
                    float(record["allocationAmplification"]),
                    BUDGETS["morphAllocationAmplification"],
                )
                ceiling(
                    f"{receipt}/{fixture}/elapsedNanosPerIteration",
                    float(record["elapsedNanos"]) / float(record["iterations"]),
                    BUDGETS["morphElapsedNanosPerIteration"],
                )
            else:
                exact(f"{receipt}/{fixture}/topologyVertices", record.get("topologyVertices"), vertices)
                exact(f"{receipt}/{fixture}/topologyFaces", record.get("topologyFaces"), faces)
                ceiling(
                    f"{receipt}/{fixture}/elapsedMilliseconds",
                    float(record["elapsedMilliseconds"]),
                    BUDGETS["jsElapsedMilliseconds"],
                )
                for field, budget in (
                    ("observedHeapDeltaBytes", "jsObservedHeapDeltaBytes"),
                    ("observedRssDeltaBytes", "jsObservedRssDeltaBytes"),
                    ("observedExternalDeltaBytes", "jsObservedExternalDeltaBytes"),
                ):
                    ceiling(f"{receipt}/{fixture}/{field}", float(record[field]), BUDGETS[budget])

    failed_correctness = [check for check in correctness if not check["passed"]]
    failed_budgets = [check for check in budgets if not check["passed"]]
    receipt = {
        "schema": "scalafim.surface-performance-court.v1",
        "status": "pass" if not failed_correctness and not failed_budgets else "fail",
        "host": {
            "platform": platform.platform(),
            "python": platform.python_version(),
        },
        "budgetPolicy": {
            "classification": "broad hosted-runner regression ceilings",
            "limits": BUDGETS,
        },
        "correctnessChecks": correctness,
        "budgetChecks": budgets,
        "records": records,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    if failed_correctness or failed_budgets:
        print(json.dumps({"correctnessFailures": failed_correctness, "budgetFailures": failed_budgets}, indent=2))
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
