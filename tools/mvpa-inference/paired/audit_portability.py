#!/usr/bin/env python3
"""Audit before/after traces of the eight already-exposed rank resource fixtures."""
import argparse
import gzip
import hashlib
import json
from pathlib import Path


def read(path):
    data = path.read_bytes()
    plain = gzip.decompress(data) if path.suffix == ".gz" else data
    rows = [json.loads(line) for line in plain.splitlines()]
    assert len(rows) == 8 and len({r["scenario"] for r in rows}) == 8
    assert all(r["new_datasets"] == 0 and len(r["draws"]) == 199 for r in rows)
    return {r["scenario"]: r for r in rows}, hashlib.sha256(plain).hexdigest()


def gap(a, b):
    assert len(a) == len(b)
    return max((abs(x-y) for x, y in zip(a, b)), default=0.0)


def pair(a, b):
    for key in ("scenario", "input_sha256", "seed", "rows", "nuisance_columns", "nuisance"):
        assert a[key] == b[key], key
    for x, y in zip(a["draws"], b["draws"]):
        assert x["candidate"] == y["candidate"] and x["permutation"] == y["permutation"]
    cols = a["nuisance_columns"]
    return {"counts_jvm": a["counts"], "counts_js": b["counts"],
            "exact_counts": a["counts"] == b["counts"],
            "reflector_column_gaps": [gap(a["qr_reflectors"][j::cols], b["qr_reflectors"][j::cols]) for j in range(cols)],
            "basis_gap": gap(a["basis"], b["basis"]),
            "projected_x_gap": gap(a["projected_x"], b["projected_x"]),
            "projected_y_gap": gap(a["projected_y"], b["projected_y"]),
            "observed_gap": gap(a["observed"], b["observed"]),
            "null_statistic_gap": max(gap(x["statistics"], y["statistics"]) for x, y in zip(a["draws"], b["draws"]))}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("before_jvm", "before_js", "after_jvm", "after_js", "output"):
        parser.add_argument(name, type=Path)
    args = parser.parse_args()
    data = [read(getattr(args, name)) for name in ("before_jvm", "before_js", "after_jvm", "after_js")]
    assert all(set(d[0]) == set(data[0][0]) for d in data)
    rows = []
    for scenario in sorted(data[0][0]):
        a, b, c, d = [x[0][scenario] for x in data]
        assert len({x["input_sha256"] for x in (a, b, c, d)}) == 1
        assert a["seed"] == c["seed"] and a["nuisance"] == c["nuisance"]
        before, after = pair(a, b), pair(c, d)
        assert after["exact_counts"] and after["basis_gap"] <= 1e-12 and after["null_statistic_gap"] <= 1e-10
        rows.append({"scenario": scenario, "input_sha256": a["input_sha256"], "before": before, "after": after})
    output = {"schema": "scalafim-rank-portability-audit/v1", "input_trace_sha256": [d[1] for d in data],
              "current_exact_count_gate_pass": True, "new_datasets": 0,
              "historical_comparator_modified": False, "paired_pilot_authorized": False, "fixtures": rows}
    args.output.write_text(json.dumps(output, indent=2) + "\n")


if __name__ == "__main__":
    main()
