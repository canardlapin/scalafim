#!/usr/bin/env python3
"""Summarize retained S10 harness evidence without issuing pilot authority."""
import argparse
import hashlib
import json
from pathlib import Path


def read(path):
    return json.loads(path.read_text())


def lines(path):
    return [json.loads(row) for row in path.read_text().splitlines() if row]


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("inputs", "real", "guard", "alpha", "output"):
        parser.add_argument(name, type=Path)
    args = parser.parse_args()
    inventory_path = args.inputs / "rehearsal-inputs.json"
    inventory = read(inventory_path)
    assert inventory["root_kind"] == "harness" and inventory["root_hex"] == "7a3c91d50b44e2f1"
    assert inventory["datasets_per_cell"] == 2 and inventory["no_deviation_datasets"] == 10
    assert len(inventory["records"]) == 22
    for row in inventory["records"]:
        stem = f"{row['cell']}__d{row['dataset']:04d}.npz"
        assert sha(args.inputs / stem) == row["npz_sha256"]
    real = read(args.real / "receipt.json")
    guard = read(args.guard / "receipt.json")
    events = lines(args.real / "engine-events.jsonl")
    alpha = lines(args.alpha)
    summary = read(Path(str(args.alpha) + ".summary.json"))
    assert real["input_inventory_sha256"] == sha(inventory_path)
    assert real["units"] == 60 and real["rerun_jobs"] > 0 and real["owner_verdict"].startswith("Valid(")
    assert guard["stop"] == "CpuCeilingReached" and guard["attempts_before_stop"] > 0
    assert guard["no_dispatch_on_unraised_resume"] and guard["resumed_units"] == 8
    assert not real["pilot_launch_authority"] and not guard["pilot_launch_authority"]
    expected = {(d, kind, method) for d in range(10) for kind in ("persistent", "no-deviation")
                for method in ("phrf-ml", "rlss")}
    assert len(alpha) == 40 and {(r["dataset"], r["truth_case"], r["method"]) for r in alpha} == expected
    assert all(not r["pilot_launch_authority"] for r in alpha)
    hashes = {r["dataset"]: r["npz_sha256"] for r in inventory["records"] if r["cell"] == "T-TX-fast"}
    assert all(r["input_sha256"] == hashes[r["dataset"]] for r in alpha)
    glm = [r for r in events if r["arm"] == "glmsingle"]
    assert len(glm) == 4 and all(r["succeeded"] and r["child_cpu_seconds"] > 0 for r in glm)
    refused = [r for r in alpha if r["refusal"] is not None]
    assert summary["complete"] == (not refused)
    report = {"schema": "phrf-s10-real-rehearsal-audit/v1", "harness_inventory_verified": True,
              "owner_reconstruction": real["owner_verdict"], "real_cpu_guard_pass": True,
              "glmsingle_successful_datasets": len(glm), "all_alpha_slots_retained": True,
              "alpha_complete": summary["complete"], "not_constant_endpoint": summary["not_constant_endpoint"],
              "alpha_refusals": refused, "real_engine_refusals": [r for r in events if "refusal" in r],
              "pilot_ready": False, "pilot_launch_authority": False,
              "remaining": ["Resolve retained trial ML refusals before claiming clean rehearsal readiness",
                            "Actual pilot coverage policy, owner custody, review and frozen manifest are separate"]}
    args.output.write_text(json.dumps(report, indent=2) + "\n")
    if refused or not summary["not_constant_endpoint"]:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
