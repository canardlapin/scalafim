#!/usr/bin/env python3
"""Validate and reduce host-local MVPA benchmark receipts.

The reduced CSV compares time only within one host run and keeps allocation
scopes distinct.  It refuses checksum or shape disagreements before emitting
the ledger.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import io
import json
import math
import os
import platform
from datetime import date
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[3]
SCHEMA = "scalafim-mvpa-performance-receipt/v1"
EXPECTED_SHAPES = {
    "observation": {"samples": 96, "features": 64},
    "relational": {"partitions": 8, "effects": 8, "features": 64},
    "predictive": {
        "runs": 8,
        "classes": 4,
        "features": 32,
        "supports": 16,
        "support_width": 8,
    },
}
EXPECTED_CHECKSUMS = {
    "observation-correlation": 4571.000223431536,
    "crossvalidated-identity-rdm": 6.874590519956278,
    "rsa-pearson-query": -0.11748100095173242,
    "rsa-intercepted-ols-query": 0.22064725063086724,
    "predictive-centroid-loro": 32.0,
    "predictive-fixed-support-frame": 512.0,
}
JMH_SCENARIOS = {
    "ObservationRdmConformanceBenchmark.correlationPublicCall": "observation-correlation",
    "RelationalRdmConformanceBenchmark.identityCrossvalidatedRdmPublicCall": "crossvalidated-identity-rdm",
    "RsaQueryConformanceBenchmark.pearsonQuery": "rsa-pearson-query",
    "RsaQueryConformanceBenchmark.interceptedOlsQuery": "rsa-intercepted-ols-query",
    "PredictiveCentroidConformanceBenchmark.leaveOneRunOutPublicCall": "predictive-centroid-loro",
    "PredictiveFrameConformanceBenchmark.fixedSupportFramePublicCall": "predictive-fixed-support-frame",
}


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def load_json(path: Path) -> Any:
    return json.loads(path.read_text(encoding="utf-8"))


def load_matlab(path: Path) -> dict[str, Any]:
    text = path.read_text(encoding="utf-8")
    begin = "SCALAFIM_JSON_BEGIN"
    end = "SCALAFIM_JSON_END"
    if text.count(begin) != 1 or text.count(end) != 1:
        raise ValueError(f"missing MATLAB receipt markers: {path}")
    payload = text.split(begin, 1)[1].split(end, 1)[0].strip()
    return json.loads(payload)


def percentile(values: list[float], probability: float) -> float:
    ordered = sorted(values)
    position = (len(ordered) - 1) * probability
    lower = math.floor(position)
    upper = math.ceil(position)
    if lower == upper:
        return ordered[lower]
    fraction = position - lower
    return ordered[lower] * (1.0 - fraction) + ordered[upper] * fraction


def unit_to_ns(value: float, unit: str) -> float:
    factors = {"ns/op": 1.0, "us/op": 1_000.0, "ms/op": 1_000_000.0, "s/op": 1_000_000_000.0}
    if unit not in factors:
        raise ValueError(f"unsupported JMH time unit: {unit}")
    return value * factors[unit]


def validate_shapes(rsatoolbox: dict[str, Any], pymvpa: dict[str, Any], matlab: dict[str, Any]) -> None:
    if rsatoolbox["shapes"] != EXPECTED_SHAPES:
        raise ValueError("Python-rsatoolbox benchmark shapes drifted")
    if pymvpa["shapes"] != EXPECTED_SHAPES:
        raise ValueError("PyMVPA benchmark shapes drifted")
    if matlab["shapes"]["relational"] != EXPECTED_SHAPES["relational"]:
        raise ValueError("MATLAB benchmark shapes drifted")


def validate_checksums(results: list[dict[str, Any]]) -> list[dict[str, Any]]:
    agreements = []
    for result in results:
        checksum = result.get("checksum")
        if checksum is None:
            continue
        expected = EXPECTED_CHECKSUMS[result["scenario"]]
        difference = abs(float(checksum) - expected)
        if difference > 1e-10:
            raise ValueError(
                f"checksum mismatch for {result['implementation']} {result['scenario']}: "
                f"{checksum} versus {expected}"
            )
        agreements.append(
            {
                "implementation": result["implementation"],
                "scenario": result["scenario"],
                "absolute_difference": difference,
                "tolerance": 1e-10,
            }
        )
    return agreements


def python_rows(receipt: dict[str, Any], implementation: str) -> list[dict[str, Any]]:
    output = []
    for result in receipt["results"]:
        output.append(
            {
                "scenario": result["scenario"],
                "implementation": implementation,
                "stage": result["stage"],
                "median_ns_per_call": float(result["median_ns_per_call"]),
                "p25_ns_per_call": float(result["p25_ns_per_call"]),
                "p75_ns_per_call": float(result["p75_ns_per_call"]),
                "samples_ns_per_call": [float(value) for value in result["samples_ns_per_call"]],
                "allocation_bytes": int(result["traced_python_peak_bytes_single_call"]),
                "allocation_scope": receipt["allocation_scope"],
                "checksum": float(result["checksum"]),
                "runtime": receipt["host"],
            }
        )
    return output


def matlab_rows(receipt: dict[str, Any]) -> list[dict[str, Any]]:
    output = []
    for result in receipt["results"]:
        output.append(
            {
                "scenario": result["scenario"],
                "implementation": "rsatoolbox-matlab-via-octave-11.1.0",
                "stage": result["stage"],
                "median_ns_per_call": float(result["median_ns_per_call"]),
                "p25_ns_per_call": float(result["p25_ns_per_call"]),
                "p75_ns_per_call": float(result["p75_ns_per_call"]),
                "samples_ns_per_call": [float(value) for value in result["samples_ns_per_call"]],
                "allocation_bytes": None,
                "allocation_scope": receipt["allocation_scope"],
                "checksum": float(result["checksum"]),
                "runtime": receipt["host"],
            }
        )
    return output


def jmh_rows(receipt: list[dict[str, Any]]) -> list[dict[str, Any]]:
    output = []
    seen = set()
    for result in receipt:
        benchmark = result["benchmark"]
        matches = [
            (suffix, scenario)
            for suffix, scenario in JMH_SCENARIOS.items()
            if benchmark.endswith(suffix)
        ]
        if len(matches) != 1:
            raise ValueError(f"unexpected JMH benchmark: {benchmark}")
        scenario = matches[0][1]
        if scenario in seen:
            raise ValueError(f"duplicate JMH scenario: {scenario}")
        seen.add(scenario)
        metric = result["primaryMetric"]
        samples = [
            unit_to_ns(float(value), metric["scoreUnit"])
            for fork in metric["rawData"]
            for value in fork
        ]
        allocation = result["secondaryMetrics"]["gc.alloc.rate.norm"]
        output.append(
            {
                "scenario": scenario,
                "implementation": "scalafim-jvm",
                "stage": "steady-state-public-call",
                "median_ns_per_call": percentile(samples, 0.5),
                "p25_ns_per_call": percentile(samples, 0.25),
                "p75_ns_per_call": percentile(samples, 0.75),
                "samples_ns_per_call": samples,
                "allocation_bytes": float(allocation["score"]),
                "allocation_scope": "JMH gc.alloc.rate.norm JVM bytes per operation",
                "checksum": EXPECTED_CHECKSUMS[scenario],
                "runtime": {
                    "jmh": result["jmhVersion"],
                    "jdk": result["jdkVersion"],
                    "vm": result["vmName"],
                    "vm_version": result["vmVersion"],
                    "forks": result["forks"],
                    "threads": result["threads"],
                    "warmup_iterations": result["warmupIterations"],
                    "measurement_iterations": result["measurementIterations"],
                },
            }
        )
    if seen != set(EXPECTED_CHECKSUMS):
        raise ValueError(f"missing JMH scenarios: {set(EXPECTED_CHECKSUMS) - seen}")
    return output


def shape_label(scenario: str) -> str:
    if scenario == "observation-correlation":
        return "96 samples x 64 features"
    if scenario in {
        "crossvalidated-identity-rdm",
        "rsa-pearson-query",
        "rsa-intercepted-ols-query",
    }:
        return "8 partitions x 8 effects x 64 features; 28 RDM pairs"
    if scenario == "predictive-centroid-loro":
        return "8 runs x 4 classes x 32 features; 32 samples"
    return "16 supports x 8 features; 8 runs x 4 classes"


def claim_boundary(result: dict[str, Any]) -> str:
    if result["implementation"].startswith("rsatoolbox-matlab"):
        return "Octave diagnostic only; not MATLAB performance"
    if result["implementation"] == "scalafim-jvm" and result["scenario"].startswith("predictive"):
        return "numeric schedule benchmark; run-axis receipt defect remains"
    return "one host and fixed shape; no cross-machine threshold"


CSV_FIELDS = (
    "scenario",
    "implementation",
    "stage",
    "shape",
    "median_ms_per_call",
    "p25_ms_per_call",
    "p75_ms_per_call",
    "relative_time_to_fastest",
    "allocation_bytes",
    "allocation_scope",
    "checksum",
    "claim_boundary",
)


def render_csv(results: list[dict[str, Any]]) -> str:
    fastest = {}
    for result in results:
        scenario = result["scenario"]
        fastest[scenario] = min(
            fastest.get(scenario, float("inf")),
            result["median_ns_per_call"],
        )
    stream = io.StringIO(newline="")
    writer = csv.DictWriter(stream, fieldnames=CSV_FIELDS, lineterminator="\n")
    writer.writeheader()
    for result in sorted(results, key=lambda value: (value["scenario"], value["implementation"])):
        writer.writerow(
            {
                "scenario": result["scenario"],
                "implementation": result["implementation"],
                "stage": result["stage"],
                "shape": shape_label(result["scenario"]),
                "median_ms_per_call": f"{result['median_ns_per_call'] / 1e6:.9f}",
                "p25_ms_per_call": f"{result['p25_ns_per_call'] / 1e6:.9f}",
                "p75_ms_per_call": f"{result['p75_ns_per_call'] / 1e6:.9f}",
                "relative_time_to_fastest": f"{result['median_ns_per_call'] / fastest[result['scenario']]:.6f}",
                "allocation_bytes": "" if result["allocation_bytes"] is None else f"{result['allocation_bytes']:.3f}",
                "allocation_scope": result["allocation_scope"],
                "checksum": f"{result['checksum']:.17g}",
                "claim_boundary": claim_boundary(result),
            }
        )
    return stream.getvalue()


def build_receipt(
    jmh_path: Path,
    rsatoolbox_path: Path,
    pymvpa_path: Path,
    matlab_path: Path,
) -> tuple[dict[str, Any], str]:
    jmh = load_json(jmh_path)
    rsatoolbox = load_json(rsatoolbox_path)
    pymvpa = load_json(pymvpa_path)
    matlab = load_matlab(matlab_path)
    validate_shapes(rsatoolbox, pymvpa, matlab)

    results = (
        python_rows(rsatoolbox, "rsatoolbox-python-0.3.2")
        + python_rows(pymvpa, "pymvpa-2.6.5.dev1")
        + matlab_rows(matlab)
        + jmh_rows(jmh)
    )
    agreements = validate_checksums(results)
    harness_files = [
        ROOT / "tools/reference/mvpa-conformance/benchmark_reference.py",
        ROOT / "tools/reference/mvpa-conformance/benchmark_matlab_reference.m",
        ROOT / "tools/reference/mvpa-conformance/summarize_performance.py",
        ROOT / "benchmarks/mvpa-jvm/src/main/scala/scalafim/fmri/mvpa/benchmark/MvpaConformanceBenchmark.scala",
    ]
    receipt = {
        "schema": SCHEMA,
        "date": date.today().isoformat(),
        "host": {
            "platform": platform.platform(),
            "machine": platform.machine(),
            "processor": platform.processor(),
            "logical_cpus": os.cpu_count(),
        },
        "shapes": EXPECTED_SHAPES,
        "protocol": {
            "setup": "all axes, data, designs, frames, and callable objects prepared outside timing",
            "timed_stage": "one steady-state public call",
            "time_statistic": "median of per-iteration/repeat mean nanoseconds per call",
            "jmh": "3 x 500 ms warmup; 7 x 500 ms measurements; one fork; one thread; gc profiler",
            "python": "explicit warmups; 9 repeats; perf_counter_ns; gc before repeats",
            "matlab": "Octave diagnostic with repeated tic/toc; per-call compatibility warnings suppressed",
        },
        "harness_sha256": {str(path.relative_to(ROOT)): sha256(path) for path in harness_files},
        "checksum_agreements": agreements,
        "results": results,
        "raw_input_sha256": {
            "jmh": sha256(jmh_path),
            "rsatoolbox_python": sha256(rsatoolbox_path),
            "pymvpa": sha256(pymvpa_path),
            "rsatoolbox_matlab_via_octave": sha256(matlab_path),
        },
        "limitations": [
            "host-local diagnostic evidence; not a portable pass/fail threshold",
            "Python tracemalloc excludes most NumPy and SciPy native allocations and is not comparable with JVM B/op",
            "Octave timing is not a claim about MATLAB performance",
            "predictive ScalaFIM timing has numeric parity but retains the known run-generalization receipt gap",
            "native spatial-neighborhood construction is outside this fixed-support performance court",
        ],
    }
    return receipt, render_csv(results)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--jmh", type=Path, required=True)
    parser.add_argument("--rsatoolbox", type=Path, required=True)
    parser.add_argument("--pymvpa", type=Path, required=True)
    parser.add_argument("--matlab", type=Path, required=True)
    parser.add_argument("--receipt-output", type=Path)
    parser.add_argument("--ledger-output", type=Path)
    parser.add_argument("--check-receipt", type=Path)
    parser.add_argument("--check-ledger", type=Path)
    args = parser.parse_args()

    receipt, ledger = build_receipt(args.jmh, args.rsatoolbox, args.pymvpa, args.matlab)
    rendered_receipt = json.dumps(receipt, allow_nan=False, indent=2, sort_keys=True) + "\n"
    if args.receipt_output is not None:
        args.receipt_output.write_text(rendered_receipt, encoding="utf-8")
    if args.ledger_output is not None:
        args.ledger_output.write_text(ledger, encoding="utf-8")
    if args.check_receipt is not None and args.check_receipt.read_text(encoding="utf-8") != rendered_receipt:
        raise SystemExit(f"performance receipt drift: {args.check_receipt}")
    if args.check_ledger is not None and args.check_ledger.read_text(encoding="utf-8") != ledger:
        raise SystemExit(f"performance ledger drift: {args.check_ledger}")
    if all(
        value is None
        for value in (args.receipt_output, args.ledger_output, args.check_receipt, args.check_ledger)
    ):
        print(rendered_receipt, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
