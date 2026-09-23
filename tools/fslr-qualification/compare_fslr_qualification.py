#!/usr/bin/env python3
"""Evaluate ScalaFIM's fsLR 32k qualification run against the frozen budgets.

Reads the output of `FslrQualification` (scalafim.fslr-qualification/1, one
directory per dataset, see its Scaladoc) and the independent SimpleITK oracle
of `independent_fslr_mapping.py` (OUT.npz + OUT.json), and evaluates each
budget frozen in docs/audits/group-volume-fslr-mapping.md section 4:

- values: identical (|d| <= 1e-9 * max(1, |v|)) over cortical vertices, with
  disagreements permitted only at tie vertices (a continuous index within 1e-6
  of a .5 voxel boundary, from ScalaFIM's or the oracle's position on the
  export.json grid), counted, <= 0.1 % of cortical vertices; coverage must be
  identical across volumes;
- coverage: MedialWall equals the desc-nomedialwall labels read here, vertex
  by vertex; NoSupport rate, and whether every NoSupport vertex of every volume
  has a receipt showing OutsideSupport, NonFinite or OutsideGrid;
- display identity: inflated and very-inflated carry the same object;
- picks: for every volume, the receipt voxel equals the oracle voxel and the
  receipt value equals the oracle value (or the oracle agrees on NoSupport);
- resources: warm JVM prepare+map per hemisphere and volume (median and max),
  and the live-heap probe and Scala.js FullOpt timing artifacts when given.

The scope budget is not script-evaluated, and the value oracle is SimpleITK,
not the Connectome Workbench implementation the budget names; both are
reported as such.

Usage:
    python compare_fslr_qualification.py --scalafim DIR/beta --oracle ORACLE/beta.npz \\
        --export PLS/beta/export.json --assets TEMPLATEFLOW/tpl-fsLR \\
        [--heap heap-probe.json] [--js js-timing.json] [--out summary.json]

Exit status is 0 only when every evaluated budget passes.
"""
import argparse
import hashlib
import json
import pathlib

import nibabel as nib
import numpy as np

COVERAGE = {0: "Mapped", 1: "MedialWall", 2: "NoSupport", 3: "BridgeUnavailable"}


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--scalafim", required=True, type=pathlib.Path)
    parser.add_argument("--oracle", required=True, type=pathlib.Path)
    parser.add_argument("--export", required=True, type=pathlib.Path)
    parser.add_argument("--assets", required=True, type=pathlib.Path)
    parser.add_argument("--heap", type=pathlib.Path)
    parser.add_argument("--js", type=pathlib.Path)
    parser.add_argument("--out", type=pathlib.Path)
    args = parser.parse_args()

    ours = args.scalafim
    results = json.loads((ours / "results.json").read_text())
    oracle = np.load(args.oracle)
    oracle_meta = json.loads(args.oracle.with_suffix(".json").read_text())
    export = json.loads(args.export.read_text())

    names = [v["file"] for v in results["volumes"]]
    if names != oracle_meta["volumes"]:
        raise SystemExit(f"volume order differs: {names} vs {oracle_meta['volumes']}")
    for v in results["volumes"]:
        if (
            oracle_meta["inputs"][v["file"]] != v["sha256"]
            or export["outputs"][v["file"]] != v["sha256"]
        ):
            raise SystemExit(
                f"{v['file']}: oracle, export and ScalaFIM disagree on the bytes"
            )

    grid = np.array(export["affine"], dtype=np.float64).reshape(4, 4)
    dims = np.array(export["dimensions"])
    world_to_voxel = np.linalg.inv(grid)

    def tie(points):
        c = points @ world_to_voxel[:3, :3].T + world_to_voxel[:3, 3]
        return np.any(np.abs(c - np.floor(c) - 0.5) < 1e-6, axis=1)

    artifacts = [ours / "results.json", args.oracle, args.export] + [
        p for p in (args.heap, args.js) if p
    ]
    summary = {
        "dataset": results["dataset"],
        "artifacts": {str(p): sha256(p) for p in artifacts},
        "hemispheres": {},
    }
    passes = {}
    for h in ("L", "R"):
        hemi = results["hemispheres"][h]
        n = hemi["vertices"]

        def load(name):
            return np.load(ours / name)

        world = load(f"world_{h}.npy")
        oracle_world = oracle[f"world_{h}"]
        oracle_cov = oracle[f"coverage_{h}"]
        labels = nib.load(
            args.assets / f"tpl-fsLR_hemi-{h}_den-32k_desc-nomedialwall_dparc.label.gii"
        )
        cortex = labels.darrays[0].data > 0.5
        cortical = int(cortex.sum())
        ties = tie(world) | tie(oracle_world)

        oracle_linear = oracle[f"voxel_{h}"]
        inside = oracle_linear >= 0
        oracle_ijk = np.full((n, 3), -1, dtype=np.int64)
        oracle_ijk[inside] = np.stack(
            np.unravel_index(oracle_linear[inside], tuple(dims), order="F"), axis=1
        )

        coverage0 = load(f"coverage_{h}_0.npy")
        value_bad = np.zeros(n, dtype=bool)
        coverage_bad = np.zeros(n, dtype=bool)
        voxel_bad = np.zeros(n, dtype=bool)
        coverage_stable = True
        receipts_explained = True
        worst = 0.0
        for k in range(len(names)):
            cov = load(f"coverage_{h}_{k}.npy")
            coverage_stable &= bool(np.array_equal(cov, coverage0))
            coverage_bad |= cov != oracle_cov
            voxel_bad |= np.any(load(f"voxel_{h}_{k}.npy") != oracle_ijk, axis=1)
            receipts_explained &= bool(
                np.all(np.isin(load(f"receipt_{h}_{k}.npy")[cov == 2], [1, 2, 3]))
            )
            ov = oracle[f"value_{h}_{k}"]
            sv = load(f"value_{h}_{k}.npy")
            both = (cov == 0) & (oracle_cov == 0)
            delta = np.abs(sv[both] - ov[both])
            bad = np.zeros(n, dtype=bool)
            bad[
                np.flatnonzero(both)[delta > 1e-9 * np.maximum(1.0, np.abs(ov[both]))]
            ] = True
            value_bad |= bad
            if delta.size:
                worst = max(
                    worst, float((delta / np.maximum(1.0, np.abs(ov[both]))).max())
                )
        disagree = (value_bad | coverage_bad | voxel_bad) & cortex

        medial_exact = bool(np.array_equal(coverage0 == 1, ~cortex))
        nosupport = coverage0 == 2
        receipt0 = load(f"receipt_{h}_0.npy")

        pick_rows = []
        for p in hemi["picks"]:
            v = p["vertex"]
            for entry in p["volumes"]:
                k = entry["volume"]
                sample = entry["samples"][0] if entry["samples"] else None
                voxel = (
                    list(sample["voxel"])
                    if sample and sample["voxel"] is not None
                    else [-1, -1, -1]
                )
                oracle_value = float(oracle[f"value_{h}_{k}"][v])
                if entry["coverage"] == "Mapped":
                    linked = (
                        sample["kind"] == "Included"
                        and oracle_cov[v] == 0
                        and entry["value"] == sample["sourceValue"]
                        and abs(sample["sourceValue"] - oracle_value)
                        <= 1e-9 * max(1.0, abs(oracle_value))
                    )
                else:
                    linked = (
                        sample is not None
                        and sample["kind"]
                        in ("NonFinite", "OutsideSupport", "OutsideGrid")
                        and entry["coverage"] == COVERAGE[int(oracle_cov[v])]
                    )
                pick_rows.append(
                    {
                        "vertex": v,
                        "volume": k,
                        "coverage": entry["coverage"],
                        "receiptVoxel": voxel,
                        "oracleVoxel": oracle_ijk[v].tolist(),
                        "voxelEqual": voxel == oracle_ijk[v].tolist(),
                        "linkedToOracle": bool(linked),
                    }
                )

        timings = hemi["volumes"]
        s = {
            "vertices": n,
            "cortical": cortical,
            "coverage": {COVERAGE[c]: int(np.sum(coverage0 == c)) for c in COVERAGE},
            "oracleCoverage": {
                COVERAGE[c]: int(np.sum(oracle_cov == c)) for c in (0, 1, 2)
            },
            "coverageStableAcrossVolumes": coverage_stable,
            "maxRelativeValueDelta": worst,
            "maxWorldDeltaMm": float(np.abs(world - oracle_world).max()),
            "tieVerticesOurs": int(np.sum(tie(world) & cortex)),
            "tieVerticesOracle": int(np.sum(tie(oracle_world) & cortex)),
            "disagreements": int(disagree.sum()),
            "disagreementsAtTies": int((disagree & ties).sum()),
            "disagreementsNotAtTies": int((disagree & ~ties).sum()),
            "voxelDisagreements": int((voxel_bad & cortex).sum()),
            "medialWall": {
                "ours": int(np.sum(coverage0 == 1)),
                "labels": int(n - cortical),
                "oracle": int(np.sum(oracle_cov == 1)),
                "vertexExactVsLabels": medial_exact,
            },
            "noSupport": {
                "count": int(nosupport.sum()),
                "ratePercentOfCortical": 100.0 * float(nosupport.sum()) / cortical,
                "receiptsExplainedAllVolumes": receipts_explained,
                "receiptKindsVolume0": {
                    name: int(np.sum(nosupport & (receipt0 == code)))
                    for code, name in (
                        (1, "OutsideGrid"),
                        (2, "OutsideSupport"),
                        (3, "NonFinite"),
                    )
                },
            },
            "display": hemi["display"],
            "picks": {
                "checked": len(pick_rows),
                "passed": sum(
                    r["voxelEqual"] and r["linkedToOracle"] for r in pick_rows
                ),
                "rows": pick_rows,
            },
            "jvm": {
                "prepareAndMapMedianMsMax": max(
                    t["prepareAndMapMedianMs"] for t in timings
                ),
                "prepareAndMapMaxMs": max(t["prepareAndMapMaxMs"] for t in timings),
                "mapMedianMsMax": max(t["mapMedianMs"] for t in timings),
                "prepareMedianMsMax": max(t["prepareMedianMs"] for t in timings),
                "admissionWarmMedianMs": hemi["admissionWarmMedianMs"],
                "admissionColdMs": hemi["admissionColdMs"],
                "allocatedMiBMax": max(t["allocatedMiB"] for t in timings),
                "retainedMiBMax": max(t["retainedMiB"] for t in timings),
                "poolPeakAboveBaselineMiBMax": max(
                    t["poolPeakAboveBaselineMiB"] for t in timings
                ),
            },
        }
        summary["hemispheres"][h] = s
        for name, ok in (
            (
                "values",
                s["disagreementsNotAtTies"] == 0
                and s["disagreementsAtTies"] <= 0.001 * cortical,
            ),
            ("coverageStableAcrossVolumes", coverage_stable),
            ("medialWall", medial_exact),
            ("noSupportReceipts", receipts_explained),
            ("noSupportRate", s["noSupport"]["ratePercentOfCortical"] <= 3.0),
            ("display", all(d["sameObject"] for d in hemi["display"])),
            ("picks", s["picks"]["passed"] == s["picks"]["checked"]),
            (
                "jvmTime",
                s["jvm"]["prepareAndMapMedianMsMax"] <= 500.0
                and s["jvm"]["prepareAndMapMaxMs"] <= 500.0,
            ),
        ):
            passes.setdefault(name, []).append(ok)

    budgets = {name: all(values) for name, values in passes.items()}
    if args.heap:
        heap = json.loads(args.heap.read_text())
        summary["heapProbe"] = heap
        budgets["heapLiveSampled"] = heap["sampledPhase"]["maxAboveBaselineMiB"] <= 64.0
    else:
        budgets["heapLiveSampled"] = "not evaluated (no --heap artifact)"
    if args.js:
        timing = json.loads(args.js.read_text())
        summary["jsTiming"] = timing
        budgets["jsFullOptSynthetic"] = (
            bool(timing["productionMode"]) and timing["prepareAndMapMaxMs"] <= 2000.0
        )
    else:
        budgets["jsFullOptSynthetic"] = "not evaluated (no --js artifact)"
    budgets["scope"] = "not script-evaluated"
    budgets[
        "valueOracleIsWorkbench"
    ] = "not met: compared against the SimpleITK oracle, not Connectome Workbench"
    summary["budgets"] = budgets

    text = json.dumps(summary, indent=2)
    if args.out:
        args.out.write_text(text + "\n")
    print(text)
    evaluated = [v for v in budgets.values() if isinstance(v, bool)]
    raise SystemExit(0 if all(evaluated) else 1)


if __name__ == "__main__":
    main()
