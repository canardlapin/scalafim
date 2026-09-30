#!/usr/bin/env python3
"""Validate and summarize the frozen diagnostic schedule and calibration gates.

Requires SciPy for exact Clopper-Pearson beta quantiles. Inputs may be JSONL or
gzip JSONL. No attempt is dropped, retried, pooled across coordinates or replaced.
"""
import argparse
import collections
import gzip
import hashlib
import itertools
import json
import math
from pathlib import Path
import statistics

import scipy
from scipy.stats import beta

from summarize_voxelwise_rrg import LABELS, group_summary, paired


REGULAR = ("baseline", "strong_ar", "multiple_runs", "censor_continuous")
STRESS = ("weak", "boundary", "joint_lag")
ARMS = ("residual_frozen_full", "residual_refit_full", "oracle_known_rank1",
        "oracle_known_full", "oracle_refit_rank1", "oracle_refit_full",
        "plugin_refit_rank1", "plugin_refit_full")
CROSS = ("true_rho_plugin_sigma_rank1", "plugin_rho_true_sigma_rank1")
ALPHA = .05 / (48 * 3)
BAND = (.92, .98)


def clopper_pearson(k, n, alpha=ALPHA):
    if not 0 <= k <= n or n <= 0 or not 0 < alpha < 1:
        raise ValueError("invalid binomial count or alpha")
    return [0.0 if k == 0 else float(beta.ppf(alpha / 2, k, n - k + 1)),
            1.0 if k == n else float(beta.ppf(1 - alpha / 2, k + 1, n - k))]


def expected_keys(phase, count):
    if count <= 0:
        raise ValueError("count must be positive")
    if phase == "development":
        return {("baseline", a, d) for d in range(count)
                for a in ARMS + (CROSS if d < 100 else ())}
    scenarios, offset = (REGULAR, 100000) if phase == "fresh" else (STRESS, 200000)
    return {(s, "plugin_refit_rank1", offset + d) for s in scenarios for d in range(count)}


def key(row):
    return row["scenario"], row["arm"], row["dataset"]


def finite_tree(value):
    if value is None:
        return True
    if isinstance(value, list):
        return all(finite_tree(x) for x in value)
    return isinstance(value, (int, float)) and math.isfinite(value)


def shaped(value, dimensions):
    if not dimensions:
        return isinstance(value, (int, float)) and math.isfinite(value)
    return isinstance(value, list) and len(value) == dimensions[0] and all(shaped(v, dimensions[1:]) for v in value)


def validate_diagnostics(row):
    runs = 2 if row["scenario"] in ("multiple_runs", "censor_continuous") else 1
    if row.get("physical_run_ids") != list(range(runs)):
        raise ValueError("physical run identity mismatch")
    dimensions = {"original_rho_per_run_voxel": (runs, 3), "generating_rho": (runs, 3),
                  "fitted_sigma_per_run": (runs, 3, 3), "generating_sigma_per_run": (runs, 3, 3),
                  "true_sigma": (3, 3), "true_rho_per_run_voxel": (runs, 3),
                  "replicate_rho_mean": (runs, 3), "replicate_rho_sd": (runs, 3),
                  "stationarity_bound_hits": (runs, 3)}
    for name, shape in dimensions.items():
        if name not in row or (row[name] is not None and not shaped(row[name], shape)):
            raise ValueError(f"missing/nonfinite/wrong-axis diagnostic {name}")
    arm = row["arm"]
    residual = arm.startswith("residual_")
    known = arm.startswith("oracle_known")
    refit = not known and arm != "residual_frozen_full"
    if row.get("original_whitening") != ("known" if known else "estimated") or row.get("replicate_whitening") != ("refit" if refit else "frozen"):
        raise ValueError("arm whitening policy metadata mismatch")
    if row.get("generator") != ("production_residual" if residual else "joint_gaussian_ar1"):
        raise ValueError("arm generator metadata mismatch")
    if row["status"] != "ok":
        return
    required = ("original_rho_per_run_voxel", "true_sigma", "true_rho_per_run_voxel")
    if not residual:
        required += ("generating_rho", "generating_sigma_per_run", "replicate_rho_mean", "replicate_rho_sd", "stationarity_bound_hits")
        if arm in ("plugin_refit_rank1", "plugin_refit_full", "true_rho_plugin_sigma_rank1"):
            required += ("fitted_sigma_per_run",)
    if any(row[name] is None for name in required):
        raise ValueError("successful attempt lacks applicable noise diagnostics")
    if row["fitted_sigma_per_run"] is None and not row.get("covariance_diagnostic_error"):
        raise ValueError("missing optional covariance requires diagnostic error")
    if residual:
        if any(row[name] is not None for name in ("generating_rho", "generating_sigma_per_run", "replicate_rho_mean", "replicate_rho_sd", "stationarity_bound_hits")):
            raise ValueError("residual arm invents parametric generator/trace diagnostics")
        return
    expected_rho = row["true_rho_per_run_voxel"] if arm.startswith("oracle_") or arm == "true_rho_plugin_sigma_rank1" else row["original_rho_per_run_voxel"]
    expected_sigma = [row["true_sigma"] for _ in range(runs)] if arm.startswith("oracle_") or arm == "plugin_rho_true_sigma_rank1" else row["fitted_sigma_per_run"]
    if row["generating_rho"] != expected_rho or row["generating_sigma_per_run"] != expected_sigma:
        raise ValueError("actual generating parameters disagree with arm intervention")
    if any(v < 0 for v in flatten(row["replicate_rho_sd"])):
        raise ValueError("negative bootstrap rho SD")
    if any(v < 0 or v > row["replicates"] or int(v) != v for v in flatten(row["stationarity_bound_hits"])):
        raise ValueError("invalid stationarity-bound hit count")


def validate(rows, phase, count, allow_incomplete=False):
    expected = expected_keys(phase, count)
    found = [key(r) for r in rows]
    if len(found) != len(set(found)):
        raise ValueError("duplicate attempted dataset/arm")
    missing, extra = expected - set(found), set(found) - expected
    if extra or (missing and not allow_incomplete):
        raise ValueError(f"schedule mismatch: missing={len(missing)}, extra={len(extra)}")
    paired_inputs = {}
    for row in rows:
        d = row["dataset"]
        offset = 0 if phase == "development" else 100000 if phase == "fresh" else 200000
        if row["dataset_index"] != d - offset:
            raise ValueError("dataset/input identity mismatch")
        expected_seed = 2017 + 101 * d if phase == "development" else 660001 + 101 * d
        if row["bootstrap_seed"] != expected_seed or row["input_seed"] != 10007 + 7919 * d:
            raise ValueError("seed differs from frozen schedule")
        replicates = 999 if phase == "fresh" else 399
        rank = 2 if row["arm"].endswith("_full") else 1
        if row["replicates"] != replicates or row["rank"] != rank:
            raise ValueError("rank or replicate budget differs from schedule")
        digest = row["response_sha256"]
        if not isinstance(digest, str) or len(digest) != 64 or any(c not in "0123456789abcdef" for c in digest):
            raise ValueError("missing response identity")
        input_key = (row["scenario"], d)
        if paired_inputs.setdefault(input_key, digest) != digest:
            raise ValueError("paired arms have different responses")
        if row["status"] not in ("ok", "failed") or not shaped(row["seconds"], ()) or row["seconds"] < 0:
            raise ValueError("invalid outcome or elapsed time")
        validate_diagnostics(row)
        completed = row["completed_replicates"]
        if completed is not None and (not isinstance(completed, int) or not 0 <= completed <= replicates):
            raise ValueError("invalid completed replicate count")
        if row["status"] == "ok":
            if completed != replicates:
                raise ValueError("successful fit has missing replicates")
            for name in ("estimate", "variance", "lower", "upper", "truth"):
                a = row[name]
                if not isinstance(a, list) or len(a) != 12 or not all(isinstance(x, (int, float)) and math.isfinite(x) for x in a):
                    raise ValueError(f"invalid {name} vector")
            if any(x < 0 for x in row["variance"]) or any(a > b for a, b in zip(row["lower"], row["upper"])):
                raise ValueError("negative variance or inverted interval")
        elif not row.get("error"):
            raise ValueError("failed attempt requires retained error")
    return {"expected_attempts": len(expected), "observed_attempts": len(rows),
            "missing_attempts": len(missing), "complete": not missing,
            "scenario_dataset_count": len(paired_inputs),
            "distinct_input_seed_count": len({r["input_seed"] for r in rows})}


def flatten(value):
    if value is None:
        return []
    if isinstance(value, list):
        return [x for part in value for x in flatten(part)]
    return [value]


def vector_summary(rows, name):
    values = [flatten(r[name]) for r in rows if r[name] is not None]
    if not values:
        return None
    if len({len(v) for v in values}) != 1:
        raise ValueError(f"inconsistent diagnostic dimension {name}")
    return {"available_attempts": len(values), "mean": [statistics.mean(v) for v in zip(*values)],
            "sd": [statistics.stdev(v) if len(v) > 1 else None for v in zip(*values)]}


def gate(rows, count, complete):
    if not complete:
        return {"verdict": "incomplete_qualification", "reason": "planned checkpoint schedule is incomplete"}
    if count not in (500, 1000, 2000):
        return {"verdict": "not_a_prespecified_checkpoint"}
    failures = sum(r["status"] != "ok" for r in rows)
    if failures:
        return {"verdict": "computational_qualification_failure", "failed_attempts": failures,
                "reason": "zero failures required for admission; no statistical rejection inferred from failures"}
    intervals = []
    for scenario in REGULAR:
        group = [r for r in rows if r["scenario"] == scenario]
        if len(group) != count:
            raise ValueError("checkpoint settings must have identical complete dataset counts")
        for j, label in enumerate(LABELS):
            k = sum(r["lower"][j] <= r["truth"][j] <= r["upper"][j] for r in group)
            ci = clopper_pearson(k, count)
            intervals.append({"scenario": scenario, "coordinate": label, "covered": k,
                              "attempts": count, "coverage": k / count, "simultaneous_interval": ci,
                              "outside_band": ci[1] < BAND[0] or ci[0] > BAND[1],
                              "inside_band": ci[0] >= BAND[0] and ci[1] <= BAND[1]})
    rejected = any(x["outside_band"] for x in intervals)
    accepted = count == 2000 and all(x["inside_band"] for x in intervals)
    verdict = "prespecified_calibration_rejection" if rejected else "accepted_declared_approximate_band" if accepted else "inconclusive" if count == 2000 else "continue_to_next_checkpoint"
    return {"verdict": verdict, "checkpoint_per_scenario": count, "family_coordinates": 48,
            "planned_looks": 3, "family_error": .05, "per_interval_alpha": ALPHA,
            "band": list(BAND), "interval_method": "exact Clopper-Pearson via scipy.stats.beta",
            "coordinates": intervals,
            "scope": "Declared Gaussian AR1 fixedrank settings only; no exact nominal coverage, T/F or p-value admission."}


def summarize(rows, phase, count, allow_incomplete=False):
    schedule = validate(rows, phase, count, allow_incomplete)
    groups = collections.defaultdict(list)
    for row in rows:
        groups[(row["scenario"], row["arm"])].append(row)
    summaries = []
    for (scenario, arm), group in sorted(groups.items()):
        diagnostics = {name: vector_summary(group, name) for name in (
            "original_rho_per_run_voxel", "generating_rho", "fitted_sigma_per_run",
            "generating_sigma_per_run", "true_sigma", "true_rho_per_run_voxel",
            "replicate_rho_mean", "replicate_rho_sd", "stationarity_bound_hits")}
        means = [r for r in group if r["replicate_rho_mean"] is not None and r["original_rho_per_run_voxel"] is not None]
        deltas = [[a - b for a, b in zip(flatten(r["replicate_rho_mean"]), flatten(r["original_rho_per_run_voxel"]))] for r in means]
        diagnostics["mean_replicate_rho_minus_original"] = [statistics.mean(v) for v in zip(*deltas)] if deltas else None
        original = [r for r in group if r["original_rho_per_run_voxel"] is not None and r["true_rho_per_run_voxel"] is not None]
        errors = [[a - b for a, b in zip(flatten(r["original_rho_per_run_voxel"]), flatten(r["true_rho_per_run_voxel"]))] for r in original]
        diagnostics["mean_original_rho_minus_truth"] = [statistics.mean(v) for v in zip(*errors)] if errors else None
        summaries.append({"scenario": scenario, "arm": arm, **group_summary(group), "noise_diagnostics": diagnostics})
    comparisons = []
    if phase == "development":
        for a, b in itertools.combinations(ARMS + CROSS, 2):
            left, right = groups.get(("baseline", a), []), groups.get(("baseline", b), [])
            if not left or not right:
                continue
            successful = [r for r in left if r["status"] == "ok"]
            variance = [statistics.variance(r["estimate"][j] for r in successful) if len(successful) > 1 else 0 for j in range(12)]
            comparisons.append({"reference": a, "comparison": b, "metrics": paired(left, right, variance)})
    return {"schema": "voxelwise-rrg-calibration-summary-v1", "phase": phase,
            "schedule": schedule, "coordinate_order": LABELS,
            "groups": summaries, "paired_comparisons": comparisons,
            "qualification": gate(rows, count, schedule["complete"]) if phase == "fresh" else {"verdict": "descriptive_only"},
            "software": {"scipy": scipy.__version__},
            "interpretation": "Coordinates, arms and shared seeds are not independent sample units; coverage stays coordinate-specific. Failed attempts remain in success-and-coverage denominators."}


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("inputs", type=Path, nargs="+")
    p.add_argument("--output", type=Path, required=True)
    p.add_argument("--phase", choices=("development", "fresh", "stress"), required=True)
    p.add_argument("--count", type=int, required=True)
    p.add_argument("--allow-incomplete", action="store_true")
    args = p.parse_args()
    rows, hashes = [], {}
    for path in args.inputs:
        raw = path.read_bytes()
        hashes[str(path)] = hashlib.sha256(raw).hexdigest()
        payload = gzip.decompress(raw) if path.suffix == ".gz" else raw
        rows.extend(json.loads(line) for line in payload.splitlines() if line.strip())
    result = summarize(rows, args.phase, args.count, args.allow_incomplete)
    result["raw_input_sha256"] = hashes
    args.output.write_text(json.dumps(result, indent=2, allow_nan=False) + "\n")
    print(json.dumps({"schedule": result["schedule"], "qualification": result["qualification"]["verdict"]}))
    for group in result["groups"]:
        coverage = [m["coverage"] for m in group["metrics"] if m["coverage"] is not None]
        print(group["scenario"], group["arm"], f"{group['successes']}/{group['attempts']}",
              [min(coverage), max(coverage)] if coverage else None)


if __name__ == "__main__":
    main()
