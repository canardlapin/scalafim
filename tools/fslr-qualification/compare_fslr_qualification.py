#!/usr/bin/env python3
"""Evaluate ScalaFIM's fsLR 32k qualification run against the frozen budgets.

Reads the output of `FslrQualification` (scalafim.fslr-qualification/1, one
directory per dataset, see its Scaladoc) and the independent SimpleITK oracle
of `independent_fslr_mapping.py` (OUT.npz + OUT.json), and evaluates each
budget frozen in docs/audits/group-volume-fslr-mapping.md section 4:

- values: identical (|d| <= 1e-9 * max(1, |v|)) over cortical vertices, with
  disagreements permitted only at tie vertices (a continuous index within 1e-6
  of a .5 voxel boundary), counted, <= 0.1 % of cortical vertices;
- coverage: MedialWall equals the admitted mask's medial count exactly;
  NoSupport rate and whether every NoSupport vertex has a receipt showing
  OutsideSupport, NonFinite or OutsideGrid;
- display identity: inflated and very-inflated carry the same object;
- picks: receipt voxel equals the oracle voxel and links to the source voxel;
- resources: warm JVM prepare+map per hemisphere and volume.

Usage:
    python compare_fslr_qualification.py --scalafim DIR/beta --oracle ORACLE/beta.npz [--out summary.json]

Exit status is 0 only when every evaluated budget passes.
"""
import argparse
import json
import pathlib

import numpy as np

COVERAGE = {0: "Mapped", 1: "MedialWall", 2: "NoSupport", 3: "BridgeUnavailable"}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--scalafim", required=True, type=pathlib.Path)
    parser.add_argument("--oracle", required=True, type=pathlib.Path)
    parser.add_argument("--out", type=pathlib.Path)
    args = parser.parse_args()

    ours = args.scalafim
    results = json.loads((ours / "results.json").read_text())
    oracle = np.load(args.oracle)
    oracle_meta = json.loads(args.oracle.with_suffix(".json").read_text())

    names = [v["file"] for v in results["volumes"]]
    if names != oracle_meta["volumes"]:
        raise SystemExit(f"volume order differs: {names} vs {oracle_meta['volumes']}")
    for v in results["volumes"]:
        if oracle_meta["inputs"][v["file"]] != v["sha256"]:
            raise SystemExit(f"{v['file']}: oracle and ScalaFIM read different bytes")

    # Grid of the exported volumes (res-2 MNI152NLin2009cAsym), from the export.
    grid = np.array(
        [[2.0, 0, 0, -96.5], [0, 2.0, 0, -132.5], [0, 0, 2.0, -78.5], [0, 0, 0, 1]]
    )
    dims = np.array([97, 115, 97])
    world_to_voxel = np.linalg.inv(grid)

    summary = {"dataset": results["dataset"], "hemispheres": {}, "budgets": {}}
    passes = {}
    for h in ("L", "R"):
        hemi = results["hemispheres"][h]
        n = hemi["vertices"]
        load = lambda name: np.load(ours / name)
        world = load(f"world_{h}.npy")
        voxel = load(f"voxel_{h}.npy")
        receipt = load(f"receipt_{h}.npy")
        oracle_cov = oracle[f"coverage_{h}"]
        cortex = oracle_cov != 1  # the oracle's medial wall is desc-nomedialwall == 0

        continuous = world @ world_to_voxel[:3, :3].T + world_to_voxel[:3, 3]
        ties = np.any(np.abs(continuous - np.floor(continuous) - 0.5) < 1e-6, axis=1)

        oracle_linear = oracle[f"voxel_{h}"]
        inside = oracle_linear >= 0
        oracle_ijk = np.full((n, 3), -1, dtype=np.int64)
        oracle_ijk[inside] = np.stack(
            np.unravel_index(oracle_linear[inside], tuple(dims), order="F"), axis=1
        )
        voxel_disagree = np.any(voxel != oracle_ijk, axis=1)

        coverage0 = load(f"coverage_{h}_0.npy")
        value_bad = np.zeros(n, dtype=bool)
        coverage_bad = np.zeros(n, dtype=bool)
        worst = 0.0
        coverage_stable = True
        for k in range(len(names)):
            cov = load(f"coverage_{h}_{k}.npy")
            coverage_stable &= bool(np.array_equal(cov, coverage0))
            coverage_bad |= cov != oracle_cov
            ov = oracle[f"value_{h}_{k}"]
            sv = load(f"value_{h}_{k}.npy")
            both = (cov == 0) & (oracle_cov == 0)
            delta = np.abs(sv[both] - ov[both])
            tolerance = 1e-9 * np.maximum(1.0, np.abs(ov[both]))
            bad = np.zeros(n, dtype=bool)
            bad[np.flatnonzero(both)[delta > tolerance]] = True
            value_bad |= bad
            if delta.size:
                worst = max(
                    worst, float((delta / np.maximum(1.0, np.abs(ov[both]))).max())
                )
        disagree = (value_bad | coverage_bad | voxel_disagree) & cortex
        unexplained = disagree & ~ties
        cortical = int(cortex.sum())

        medial_ours = int(np.sum(coverage0 == 1))
        medial_mask = int(n - cortical)
        nosupport = coverage0 == 2
        explained = np.isin(receipt, [1, 2, 3])
        world_delta = np.abs(world - oracle[f"world_{h}"]).max()

        picks = hemi["picks"]
        pick_rows = []
        for p in picks:
            v = p["vertex"]
            sample = p["samples"][0] if p["samples"] else None
            voxel_pick = (
                tuple(sample["voxel"])
                if sample and sample["voxel"] is not None
                else (-1, -1, -1)
            )
            linked = sample is not None and (
                (
                    sample["kind"] == "Included"
                    and sample["sourceValue"] == sample["volumeAtVoxel"]
                )
                or (
                    sample["kind"] == "NonFinite"
                    and sample["volumeAtVoxel"] == sample["sourceValue"]
                )
                or (sample["kind"] in ("OutsideSupport", "OutsideGrid"))
            )
            pick_rows.append(
                {
                    "vertex": v,
                    "coverage": p["coverage"],
                    "receiptVoxel": list(voxel_pick),
                    "oracleVoxel": oracle_ijk[v].tolist(),
                    "voxelEqual": list(voxel_pick) == oracle_ijk[v].tolist(),
                    "linked": bool(linked),
                }
            )

        timings = hemi["volumes"]
        summary["hemispheres"][h] = {
            "vertices": n,
            "cortical": cortical,
            "coverage": {COVERAGE[c]: int(np.sum(coverage0 == c)) for c in COVERAGE},
            "oracleCoverage": {
                COVERAGE[c]: int(np.sum(oracle_cov == c)) for c in (0, 1, 2)
            },
            "coverageStableAcrossVolumes": coverage_stable,
            "maxRelativeValueDelta": worst,
            "maxWorldDeltaMm": float(world_delta),
            "tieVertices": int(np.sum(ties & cortex)),
            "disagreements": int(disagree.sum()),
            "disagreementsAtTies": int((disagree & ties).sum()),
            "disagreementsNotAtTies": int(unexplained.sum()),
            "voxelDisagreements": int((voxel_disagree & cortex).sum()),
            "medialWall": {
                "ours": medial_ours,
                "mask": medial_mask,
                "oracle": int(np.sum(oracle_cov == 1)),
            },
            "noSupport": {
                "count": int(nosupport.sum()),
                "ratePercentOfCortical": 100.0 * float(nosupport.sum()) / cortical,
                "withReceipt": int((nosupport & explained).sum()),
                "receiptKinds": {
                    name: int(np.sum(nosupport & (receipt == code)))
                    for code, name in (
                        (1, "OutsideGrid"),
                        (2, "OutsideSupport"),
                        (3, "NonFinite"),
                    )
                },
            },
            "display": hemi["display"],
            "picks": pick_rows,
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
            },
        }
        s = summary["hemispheres"][h]
        passes.setdefault("values", []).append(
            s["disagreementsNotAtTies"] == 0
            and s["disagreementsAtTies"] <= 0.001 * cortical
        )
        passes.setdefault("medialWall", []).append(
            medial_ours == medial_mask == s["medialWall"]["oracle"]
        )
        passes.setdefault("noSupportReceipts", []).append(
            s["noSupport"]["withReceipt"] == s["noSupport"]["count"]
        )
        passes.setdefault("noSupportRate", []).append(
            s["noSupport"]["ratePercentOfCortical"] <= 3.0
        )
        passes.setdefault("display", []).append(
            all(d["sameObject"] for d in hemi["display"])
        )
        passes.setdefault("picks", []).append(
            all(r["voxelEqual"] and r["linked"] for r in pick_rows)
        )
        passes.setdefault("jvmTime", []).append(
            s["jvm"]["prepareAndMapMedianMsMax"] <= 500.0
        )

    summary["budgets"] = {name: all(values) for name, values in passes.items()}
    text = json.dumps(summary, indent=2)
    if args.out:
        args.out.write_text(text + "\n")
    print(text)
    raise SystemExit(0 if all(summary["budgets"].values()) else 1)


if __name__ == "__main__":
    main()
