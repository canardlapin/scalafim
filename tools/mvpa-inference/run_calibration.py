#!/usr/bin/env python3
"""Quarantined calibration coordinator. Statistical evaluations stay upstream.

This coordinator can plan assignments and summarize existing records. It does
not silently launch a build, simulator, pilot or confirmation campaign.
"""
from __future__ import annotations

import argparse
import csv
import json
import os
from pathlib import Path
import signal
import subprocess
import time

from calibration_protocol import (
    ProtocolError, actual_draws, canonical, dataset_count, file_locks,
    seed_record, summarize, validate_manifest,
)


def read_json(path: Path):
    return json.loads(path.read_text())


def write_once(path: Path, value) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("xb") as output:
        output.write(canonical(value))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", required=True, type=Path)
    parser.add_argument("--repository", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--phase", required=True, choices=("fixture", "simulator", "pilot", "confirmation"))
    parser.add_argument("--cell", required=True)
    sub = parser.add_subparsers(dest="command", required=True)
    plan = sub.add_parser("plan")
    plan.add_argument("--output", type=Path, required=True)
    plan.add_argument("--assignments", type=Path)
    summary = sub.add_parser("summarize")
    summary.add_argument("--records", type=Path, required=True)
    summary.add_argument("--output", type=Path, required=True)
    qa = sub.add_parser("qa-rank")
    qa.add_argument("--assignments", type=Path, required=True)
    qa.add_argument("--out-dir", type=Path, required=True)
    qa.add_argument("--rscript", default="/usr/local/bin/Rscript")
    args = parser.parse_args()
    started = time.monotonic()
    manifest = read_json(args.manifest)
    validate_manifest(manifest, args.repository, args.phase, "plan" if args.command == "plan" else "execute")
    matches = [cell for cell in manifest["cells"] if cell["id"] == args.cell]
    if len(matches) != 1:
        raise ProtocolError("the exact frozen cell is absent")
    cell = matches[0]
    count = dataset_count(args.phase, cell)
    if args.command == "plan":
        result = {
            "scope": "assignment/resource plan; no datasets generated or statistical procedure run",
            "cell": cell, "phase": args.phase, "datasets": count,
            "actual_reference_draws_per_dataset": actual_draws(args.phase, cell),
            "randomization_draws_total": count * (actual_draws(args.phase, cell) or 0),
            "bootstrap_draws": manifest["criteria"]["bootstrap_draws"],
            "source_locks": manifest["source_locks"],
            "resource_approval": manifest.get("resource_approval"),
            "empirical_runtime_estimate": None,
            "runtime_estimate_status": "requires source-bound simulator QA and measured pilot",
        }
        write_once(args.output, result)
        if args.assignments:
            args.assignments.parent.mkdir(parents=True, exist_ok=True)
            if args.assignments.suffix == ".tsv":
                with args.assignments.open("x", newline="") as output:
                    columns = ["phase","scenario_id","dataset_index","root_seed64","sha256_utf8","noise_seed64"] + ["r_state" + str(i) for i in range(1,7)]
                    writer = csv.DictWriter(output,columns,delimiter="\t")
                    writer.writeheader()
                    for i in range(count):
                        record = seed_record(args.phase,cell["id"],i)
                        flat = {key: record[key] for key in columns if key in record}
                        flat["noise_seed64"] = record["child_seeds64"]["noise"]
                        flat.update({"r_state" + str(j+1): value for j,value in enumerate(record["r_noise_state"])})
                        writer.writerow(flat)
            else:
                with args.assignments.open("xb") as output:
                    for i in range(count):
                        output.write(canonical(seed_record(args.phase, cell["id"], i)))
    elif args.command == "summarize":
        rows = [json.loads(line) for line in args.records.read_text().splitlines() if line.strip()]
        result = summarize(cell, args.phase, rows)
        result["elapsed_seconds"] = time.monotonic() - started
        result["resource_scope"] = "coordinator only; provider CPU/RSS require the separate process receipt"
        result["source_locks_observed"] = file_locks(args.repository, list(manifest["source_locks"]))
        write_once(args.output, result)
    else:
        if args.phase != "simulator" or cell["procedure"] != "rank":
            raise ProtocolError("this QA worker only generates rank simulator moments")
        if count != 10000 or cell["definition_status"] != "frozen":
            raise ProtocolError("QA requires the complete fixed population and frozen parameters")
        approval = manifest["resource_approval"]
        if args.cell not in approval.get("scope", []):
            raise ProtocolError("cell is outside the authorized QA resource scope")
        if approval.get("worker_processes") != 1 or not 1 <= approval.get("cpus", 0) <= 4 or not 0 < approval.get("max_rss_bytes", 0) <= 2 * 1024**3 or not 0 < approval.get("max_wall_seconds", 0) <= 600:
            raise ProtocolError("QA resource approval exceeds the authorized court")
        args.out_dir.mkdir(parents=True, exist_ok=True)
        command = [args.rscript, str(args.repository / "tools/mvpa-inference/generate_known_truth.R"),
                   "--simulator-qa", "--manifest", str(args.manifest.resolve()), "--cell", args.cell,
                   "--seed-records", str(args.assignments.resolve()), "--out-dir", str(args.out_dir.resolve())]
        env = dict(os.environ, LC_ALL="C", LANG="C", OMP_NUM_THREADS=str(approval["cpus"]),
                   OPENBLAS_NUM_THREADS=str(approval["cpus"]), VECLIB_MAXIMUM_THREADS=str(approval["cpus"]))
        maximum_rss = 0
        refusal = None
        with (args.out_dir / "worker.log").open("xb") as log:
            worker = subprocess.Popen(command, cwd=args.repository, env=env, stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
            worker_started = time.monotonic()
            try:
                while worker.poll() is None:
                    ps = subprocess.run(["ps", "-o", "rss=", "-p", str(worker.pid)], capture_output=True, text=True)
                    if ps.returncode == 0 and ps.stdout.strip():
                        maximum_rss = max(maximum_rss, int(ps.stdout.strip()) * 1024)
                    elapsed = time.monotonic() - worker_started
                    if maximum_rss > approval["max_rss_bytes"] or elapsed > approval["max_wall_seconds"]:
                        refusal = "rss-limit" if maximum_rss > approval["max_rss_bytes"] else "wall-limit"
                        break
                    time.sleep(.1)
            except OSError as error:
                refusal = "resource-monitor-unavailable: " + str(error)
            if refusal and worker.poll() is None:
                try:
                    os.killpg(worker.pid, signal.SIGTERM)
                    worker.wait(timeout=2)
                except subprocess.TimeoutExpired:
                    os.killpg(worker.pid, signal.SIGKILL)
            exit_code = worker.wait()
        receipt = {
            "cell": args.cell, "phase": "simulator", "scope": "generator moments only; no statistic or release",
            "expected_datasets": 10000, "worker_pid": worker.pid, "command": command,
            "resource_approval": approval, "observed_peak_rss_bytes": maximum_rss,
            "elapsed_seconds": time.monotonic() - worker_started, "worker_exit": exit_code,
            "resource_refusal": refusal, "source_locks": manifest["source_locks"],
            "status": "resource_refused" if refusal else ("simulator_qa_passed" if exit_code == 0 else "simulator_qa_failed"),
            "scientific_release": "unavailable",
        }
        write_once(args.out_dir / "process-receipt.json", receipt)
        if exit_code != 0 or refusal:
            raise ProtocolError("QA did not pass; preserve its complete resource/failure receipt")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (ProtocolError, OSError, ValueError) as error:
        raise SystemExit("calibration refused: " + str(error))
