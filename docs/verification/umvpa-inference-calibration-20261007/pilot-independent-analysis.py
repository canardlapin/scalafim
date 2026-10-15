#!/usr/bin/env python3
"""Inspect sealed pilot records; no simulator, fitting, or release adjudication."""
import csv
import hashlib
import json
from pathlib import Path
import statistics
import subprocess
import sys

packet = Path(__file__).resolve().parent
root = packet.parents[2]
sys.path.insert(0, str(root / "tools/mvpa-inference"))
from calibration_protocol import seed_record, summarize

sha = lambda path: hashlib.sha256(path.read_bytes()).hexdigest()
manifest = json.loads((packet / "pilot-manifest.json").read_text())
ready = json.loads((packet / "pilot-inputs-ready.json").read_text())
metrics = []
cells = []
all_times = []
for label, leading in [("R0", 0), ("R1", 1), ("R2", 2)]:
    folder = packet / ("run-pilot-" + label)
    receipt = json.loads((folder / "process-receipt.json").read_text())
    records = folder / "records.jsonl"
    assert sha(records) == receipt["records_sha256"]
    assert receipt["status"] == "completed" and receipt["worker_exit"] == 0
    assert receipt["resource_refusal"] is None
    assert receipt["resource_limits"]["heap"] == "2g"
    assert receipt["resource_limits"]["cpus"] == 4
    assert receipt["resource_limits"]["worker_processes"] == 1
    assert receipt["observed_peak_rss_bytes"] <= 3 * 1024**3
    assert receipt["elapsed_seconds"] <= 900
    for path, expected in manifest["source_locks"].items():
        committed = subprocess.check_output(["git", "show", receipt["source_commit"] + ":" + path], cwd=root)
        assert hashlib.sha256(committed).hexdigest() == expected
    batch = next(b for b in ready["case_batches"] if b["name"] == "pilot-" + label)
    assert sha(Path(batch["case_list"])) == receipt["input_sha256"]
    for case in batch["cases"]:
        assert sha(root / case["path"]) == case["sha256"]
    rows = [json.loads(line) for line in records.read_text().splitlines() if line.strip()]
    name = "rank." + label + ".n80.p6.q4.intercept.null"
    cell = next(c for c in manifest["cells"] if c["id"] == name)
    retained = summarize(cell, "pilot", rows)
    assert retained["unique_completed_datasets"] == 200
    assert retained["statuses"] == dict(evaluated=200, refused=0, failed=0, incomplete=0)
    assert retained["identical_retries"] == 0
    assert len(rows) == 200
    truth = [not any(cell["parameters"]["correlations"][k:]) for k in range(4)]
    for row in rows:
        assignment = seed_record("pilot", name, row["dataset_index"])
        assert row["resampling_seed64"] == assignment["child_seeds64"]["resampling"]
        assert row["completed_compact_fits"] == 796
        assert row["member_ids"] == ["rank-" + str(k + 1) for k in range(4)]
        assert row["declared_population_null"] == truth == row["actual_conditional_null"]
        assert len(row["p_values"]) == 4
        assert all(abs(p * 200 - round(p * 200)) < 1e-9 for p in row["p_values"])
        assert all(a <= b for a, b in zip(row["p_values"], row["p_values"][1:]))
        assert row["reject"] == [p <= .05 for p in row["p_values"]]
    counts = [sum(row["reject"][k] for row in rows) for k in range(4)]
    fwer = sum(any(reject and null for reject, null in zip(row["reject"], truth)) for row in rows)
    primary = fwer if leading == 0 else counts[leading]
    metrics.append(dict(cell=label, member="closed-sequence" if leading == 0 else "H" + str(leading + 1),
                        metric="closed-FWER" if leading == 0 else "closed-null-rejection", successes=primary,
                        datasets=200, confidence=.90, sidedness="two-sided", role="pilot-primary-description"))
    for k, count in enumerate(counts):
        metrics.append(dict(cell=label, member="H" + str(k + 1),
                            metric="closed-null-rejection" if truth[k] else "closed-detection", successes=count,
                            datasets=200, confidence=.90 if truth[k] else .95,
                            sidedness="two-sided" if truth[k] else "lower-one-sided", role="pilot-member-description"))
    times = [row["elapsed_seconds"] for row in rows]
    all_times.extend(times)
    cells.append(dict(cell=label, source_commit=receipt["source_commit"], records_sha256=sha(records),
                      retained_denominator=200, outcome_status_counts=retained["statuses"],
                      declared_population_null=truth, closed_member_rejection_counts=counts,
                      closed_true_null_family_rejections=fwer, primary_closed_rejections=primary,
                      actual_draws_per_dataset=199, completed_compact_fits=200 * 796,
                      kernel_wall_seconds_total=sum(times), kernel_wall_seconds_mean=statistics.mean(times),
                      kernel_wall_seconds_median=statistics.median(times),
                      kernel_wall_seconds_range=[min(times), max(times)],
                      supervised_process_wall_seconds=receipt["elapsed_seconds"],
                      observed_peak_rss_bytes=receipt["observed_peak_rss_bytes"],
                      cpu_time_seconds=None, cpu_time_status="not measured; wall timing must not be relabelled CPU time",
                      configured_cpus=4, configured_heap="2g"))

counts_file = packet / "pilot-descriptive-counts.tsv"
with counts_file.open("x", newline="") as output:
    writer = csv.DictWriter(output, list(metrics[0]), delimiter="\t")
    writer.writeheader()
    writer.writerows(metrics)
subprocess.run(["/usr/local/bin/Rscript", str(packet / "pilot-descriptive-cp.R"),
                str(counts_file), str(packet / "pilot-descriptive-cp.tsv"),
                str(packet / "pilot-analysis-R-session.txt")], check=True, cwd=root)
with (packet / "pilot-descriptive-cp.tsv").open() as source:
    intervals = list(csv.DictReader(source, delimiter="\t"))
defined = [c for c in manifest["cells"] if c["procedure"] == "rank" and c["definition_status"] == "frozen"]
datasets = sum(manifest["confirmation_counts"][c["rate_class"]] for c in defined)
projection = statistics.mean(all_times) * (1999 / 199) * datasets / 3600
summary = dict(scope="sealed JVM pilot only; no confirmation or released inference",
               source_commit=cells[0]["source_commit"], total_datasets=600,
               actual_draws_per_dataset=199, total_completed_compact_fits=477600,
               assigned_case_hashes_checked=600, committed_source_locks_checked=len(manifest["source_locks"]),
               all_failed_missing_refused_incomplete_counts=0,
               cells=cells, independent_R_descriptive_intervals=intervals,
               interval_method="R stats::qbeta Clopper-Pearson; dataset-level Bernoulli counts",
               raw_per_step_p_values_recorded=False, retrospective_raw_reconstruction=False,
               frozen_confirmation_counts_satisfied=False, confirmation_invoked=False,
               scientific_release="unavailable",
               projection=dict(defined_intercept_rank_cells=len(defined), confirmation_datasets=datasets,
                               serial_kernel_only_hours=projection,
                               scope="B-linear extrapolation of 600 n80/p6/q4 pilot wall timings; unmeasured n160 and reversed dimensions, bootstrap, I/O, generation, nativeJS and startup excluded; not resource admission or proven lower bound"),
               analysis_sources={"pilot-independent-analysis.py": sha(Path(__file__)),
                                 "pilot-descriptive-cp.R": sha(packet / "pilot-descriptive-cp.R")})
assert len({cell["source_commit"] for cell in cells}) == 1
with (packet / "pilot-summary.json").open("x") as output:
    json.dump(summary, output, sort_keys=True, indent=2)
    output.write("\n")
print(json.dumps(dict(total_datasets=600, cells=[dict(cell=c["cell"], primary=c["primary_closed_rejections"],
      all_rank_counts=c["closed_member_rejection_counts"]) for c in cells],
      projected_serial_kernel_hours=projection, scientific_release="unavailable"), indent=2))
