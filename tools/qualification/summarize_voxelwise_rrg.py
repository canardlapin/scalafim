#!/usr/bin/env python3
"""Summarize every attempted RRG dataset, without pooling correlated coordinates."""
import argparse
import collections
import gzip
import hashlib
import json
import math
import statistics
from pathlib import Path


LABELS = [f"coefficient_{r}_voxel_{v}" for r in range(2) for v in range(3)] + [
    f"{name}_voxel_{v}" for name in ("difference", "average") for v in range(3)
]
SCENARIOS = {"baseline": 400, "multiple_runs": 400, "strong_ar": 400,
             "censor_continuous": 400, "censor_reset": 100,
             "joint_lag_block1": 200, "joint_lag_block4": 200,
             "weak": 200, "boundary": 200}


def expected_keys(kind):
    if kind == "known":
        return {(f"baseline_known_rank{rank}", "FrozenWhitening", "main", d) for rank in (1, 2) for d in range(400)}
    expected = set()
    for scenario, count in SCENARIOS.items():
        n = 10 if kind == "pilot" else 1 if kind == "smoke" else count
        for d in range(n):
            for mode in ("FrozenWhitening", "RefitAutocorrelation"):
                expected.add((scenario, mode, "main", d))
                if kind != "pilot" and d < 100 and scenario in ("baseline", "censor_continuous"):
                    expected.add((scenario, mode, "nested", d))
                if kind != "pilot" and d < 20 and scenario == "baseline":
                    for seed in range(1, 5):
                        expected.add((scenario, mode, f"independent_{seed}", d))
    return expected


def wilson(k, n):
    if not n:
        return None
    z = 1.959963984540054
    p = k / n
    denominator = 1 + z * z / n
    center = (p + z * z / (2 * n)) / denominator
    half = z / denominator * math.sqrt(p * (1 - p) / n + z * z / (4 * n * n))
    return [0.0 if k == 0 else max(0.0, center - half),
            1.0 if k == n else min(1.0, center + half)]


def covered(row, j):
    return row["lower"][j] <= row["truth"][j] <= row["upper"][j]


def group_summary(rows):
    successes = [r for r in rows if r["status"] == "ok"]
    failures = [r for r in rows if r["status"] != "ok"]
    n = len(successes)
    metrics = []
    for j, label in enumerate(LABELS):
        if not n:
            metrics.append({"estimand": label, "coverage": None})
            continue
        values = [r["estimate"][j] for r in successes]
        errors = [r["estimate"][j] - r["truth"][j] for r in successes]
        variance = statistics.variance(values) if n > 1 else None
        bootstrap_variance = statistics.mean(r["variance"][j] for r in successes)
        k = sum(covered(r, j) for r in successes)
        ci = wilson(k, n)
        metrics.append({
            "estimand": label, "coverage": k / n, "coverage_wilson95": ci,
            "coverage_mcse": math.sqrt((k / n) * (1 - k / n) / n),
            "success_and_coverage_per_attempt": k / len(rows),
            "truth_below_interval": sum(r["truth"][j] < r["lower"][j] for r in successes) / n,
            "truth_above_interval": sum(r["truth"][j] > r["upper"][j] for r in successes) / n,
            "bias": statistics.mean(errors),
            "rmse": math.sqrt(statistics.mean(e * e for e in errors)),
            "sampling_variance": variance, "mean_bootstrap_variance": bootstrap_variance,
            "variance_ratio": bootstrap_variance / variance if variance else None,
            "mean_width": statistics.mean(r["upper"][j] - r["lower"][j] for r in successes),
            "unadjusted_undercoverage_signal": ci[1] < 0.95,
        })
    return {
        "attempts": len(rows), "successes": n, "failures": len(failures),
        "failure_records": [{k: r[k] for k in ("dataset", "error", "completed_replicates")} for r in failures],
        "fit_seconds_total": sum(r["seconds"] for r in rows), "metrics": metrics,
    }


def paired(rows_a, rows_b, sampling_variance):
    a = {r["dataset"]: r for r in rows_a if r["status"] == "ok"}
    b = {r["dataset"]: r for r in rows_b if r["status"] == "ok"}
    indices = sorted(a.keys() & b.keys())
    result = []
    for j, label in enumerate(LABELS):
        if not indices:
            continue
        differences = [int(covered(b[d], j)) - int(covered(a[d], j)) for d in indices]
        change = statistics.mean(differences)
        se = math.sqrt(statistics.variance(differences) / len(indices)) if len(indices) > 1 else None
        sd = math.sqrt(sampling_variance[j]) if sampling_variance[j] else None
        endpoints = [abs(b[d][edge][j] - a[d][edge][j]) for d in indices for edge in ("lower", "upper")]
        result.append({
            "estimand": label, "paired_successes": len(indices),
            "coverage_a": statistics.mean(covered(a[d], j) for d in indices),
            "coverage_b": statistics.mean(covered(b[d], j) for d in indices),
            "coverage_change_b_minus_a": change, "paired_change_mcse": se,
            "covered_only_a": sum(x == -1 for x in differences), "covered_only_b": sum(x == 1 for x in differences),
            "mean_width_change": statistics.mean((b[d]["upper"][j] - b[d]["lower"][j]) - (a[d]["upper"][j] - a[d]["lower"][j]) for d in indices),
            "mean_absolute_endpoint_change_in_sampling_sd": statistics.mean(endpoints) / sd if sd else None,
        })
    return result


def summarize(rows, kind="study"):
    seen = set()
    groups = collections.defaultdict(list)
    for row in rows:
        identity = tuple(row[k] for k in ("scenario", "mode", "budget", "dataset"))
        if identity in seen:
            raise ValueError(f"duplicate dataset identity: {identity}")
        seen.add(identity)
        expected_b = 1999 if row["budget"] == "nested" else 399
        offset = 100003 * int(row["budget"].split("_")[1]) if row["budget"].startswith("independent_") else 0
        if row["replicates"] != expected_b or row["seed"] != 2017 + 101 * row["dataset"] + offset:
            raise ValueError(f"wrong resampling budget or seed in {identity}")
        if row["status"] == "ok":
            for key in ("estimate", "variance", "lower", "upper", "truth"):
                if len(row[key]) != len(LABELS) or not all(math.isfinite(x) for x in row[key]):
                    raise ValueError(f"invalid {key} in {identity}")
            if any(x < 0 for x in row["variance"]) or any(a > b for a, b in zip(row["lower"], row["upper"])):
                raise ValueError(f"invalid uncertainty in {identity}")
        groups[identity[:3]].append(row)
    expected = expected_keys(kind)
    if seen != expected:
        raise ValueError(f"incomplete or unexpected {kind} schedule: missing={len(expected-seen)}, extra={len(seen-expected)}")
    summaries = {key: group_summary(values) for key, values in sorted(groups.items())}
    nested, modes, seed_variation, blocks = [], [], [], []
    for (scenario, mode, budget), values in sorted(groups.items()):
        if budget != "main":
            continue
        variances = [x.get("sampling_variance") for x in summaries[(scenario, mode, budget)]["metrics"]]
        high = groups.get((scenario, mode, "nested"))
        if high:
            nested.append({"scenario": scenario, "mode": mode, "metrics": paired(values, high, variances)})
        if mode == "FrozenWhitening":
            refit = groups.get((scenario, "RefitAutocorrelation", "main"))
            if refit:
                modes.append({"scenario": scenario, "a": mode, "b": "RefitAutocorrelation", "metrics": paired(values, refit, variances)})
        if scenario == "joint_lag_block1":
            block4 = groups.get(("joint_lag_block4", mode, "main"))
            if block4:
                a = {r["dataset"]: r for r in values if r["status"] == "ok"}
                for row in block4:
                    if row["status"] == "ok" and row["dataset"] in a and row["estimate"] != a[row["dataset"]]["estimate"]:
                        raise ValueError("block-length pair changed the original fitted estimates")
                blocks.append({"mode": mode, "a": "joint_lag_block1", "b": "joint_lag_block4",
                               "metrics": paired(values, block4, variances)})
        if scenario == "baseline":
            seeds = [r for key, rs in groups.items() if key[:2] == (scenario, mode) and (key[2] == "main" or key[2].startswith("independent_")) for r in rs if r["dataset"] < 20]
            by_dataset = collections.defaultdict(list)
            for r in seeds:
                if r["status"] == "ok":
                    by_dataset[r["dataset"]].append(r)
            complete = [rs for rs in by_dataset.values() if len(rs) == 5]
            metrics = []
            for j, label in enumerate(LABELS):
                sd = math.sqrt(variances[j]) if variances[j] else None
                metrics.append({"estimand": label, "complete_five_seed_datasets": len(complete),
                    "mean_endpoint_mc_sd_in_sampling_sd": statistics.mean(statistics.stdev(r[edge][j] for r in rs) for rs in complete for edge in ("lower", "upper")) / sd if complete and sd else None,
                    "coverage_decision_varies_fraction": statistics.mean(len({covered(r, j) for r in rs}) > 1 for rs in complete) if complete else None})
            seed_variation.append({"scenario": scenario, "mode": mode, "metrics": metrics})
    return {"schema": "voxelwise-rrg-coverage-v1", "schedule": kind, "complete": True,
            "coordinate_order": LABELS, "nominal_pointwise_level": 0.95,
            "interpretation": "Descriptive targeted study; coordinate Wilson intervals and investigation flags are not multiplicity-adjusted; unsuccessful fits are never replaced. No nominal T/F or general coverage certification.",
            "groups": [{"scenario": k[0], "mode": k[1], "budget": k[2], **v} for k, v in summaries.items()],
            "nested_replicate_comparisons": nested, "paired_mode_comparisons": modes,
            "paired_block_comparisons": blocks,
            "independent_seed_sensitivity": seed_variation}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--kind", choices=("study", "pilot", "smoke", "known"), default="study")
    args = parser.parse_args()
    raw = args.input.read_bytes()
    decoded = gzip.decompress(raw) if args.input.suffix == ".gz" else raw
    rows = [json.loads(line) for line in decoded.splitlines() if line.strip()]
    result = summarize(rows, args.kind)
    result["raw_input_sha256"] = hashlib.sha256(raw).hexdigest()
    result["raw_rows"] = len(rows)
    args.output.write_text(json.dumps(result, indent=2, allow_nan=False) + "\n")
    for group in result["groups"]:
        if group["budget"] == "main":
            coverage = [m["coverage"] for m in group["metrics"] if m["coverage"] is not None]
            print(group["scenario"], group["mode"], f"{group['successes']}/{group['attempts']}", f"coverage {min(coverage):.4f}..{max(coverage):.4f}" if coverage else "no successful intervals")


if __name__ == "__main__":
    main()
