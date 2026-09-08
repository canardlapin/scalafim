#!/usr/bin/env python3
"""Reduce the three external arms to one checked, convention-aware receipt."""

from __future__ import annotations

import argparse
import json
import math
from pathlib import Path
from typing import Any, Iterable

from fixture import build_fixture, canonical_json


SCHEMA = "scalafim-mvpa-external-conformance/v1"
TOLERANCE = 1.0e-10


def max_gap(left: Iterable[float], right: Iterable[float]) -> float:
    pairs = list(zip(left, right, strict=True))
    return max((abs(float(a) - float(b)) for a, b in pairs), default=0.0)


def load_json(path: Path) -> dict[str, Any]:
    return json.loads(path.read_text(encoding="utf-8"))


def load_matlab(path: Path) -> dict[str, Any]:
    text = path.read_text(encoding="utf-8")
    begin = "SCALAFIM_JSON_BEGIN\n"
    end = "\nSCALAFIM_JSON_END"
    if begin not in text or end not in text:
        raise SystemExit(f"missing MATLAB JSON markers in {path}")
    payload = text.split(begin, 1)[1].split(end, 1)[0]
    return json.loads(payload)


def require_gap(name: str, value: float) -> dict[str, Any]:
    if not math.isfinite(value) or value > TOLERANCE:
        raise SystemExit(f"{name} gap {value} exceeds {TOLERANCE}")
    return {"name": name, "max_abs_diff": value, "tolerance": TOLERANCE}


def build_receipt(
    rsa_python: dict[str, Any],
    pymvpa: dict[str, Any],
    rsa_matlab: dict[str, Any],
) -> dict[str, Any]:
    fixture = build_fixture()
    fixture_hashes = {
        rsa_python["fixture_sha256"],
        pymvpa["fixture_sha256"],
        rsa_matlab["fixture_sha256"],
        fixture["fixture_sha256"],
    }
    if len(fixture_hashes) != 1:
        raise SystemExit(f"fixture identity mismatch: {sorted(fixture_hashes)}")

    rsa_observation = rsa_python["scenarios"]["observation"]
    pymvpa_observation = pymvpa["scenarios"]["observation"]
    rsa_relation = rsa_python["scenarios"]["relational"]
    matlab_relation = rsa_matlab["scenarios"]["relational"]
    feature_count = len(fixture["observation"]["feature_ids"])
    pymvpa_per_feature = [
        value / feature_count for value in pymvpa_observation["sqeuclidean"]
    ]

    agreements = [
        require_gap(
            "observation-correlation-rsatoolbox-vs-pymvpa",
            max_gap(rsa_observation["correlation"], pymvpa_observation["correlation"]),
        ),
        require_gap(
            "observation-squared-euclidean-normalization",
            max_gap(
                rsa_observation["squared_euclidean_per_feature"],
                pymvpa_per_feature,
            ),
        ),
        require_gap(
            "crossnobis-rsatoolbox-python-vs-matlab",
            max_gap(
                rsa_relation["crossnobis_fixed_precision"]["whole"],
                matlab_relation["crossnobis_fixed_precision"],
            ),
        ),
        require_gap(
            "crossnobis-loo-vs-uniform-all-pairs",
            max_gap(
                rsa_relation["crossnobis_fixed_precision"]["whole"],
                rsa_relation["explicit_all_pairs_fixed_precision"]["whole"],
            ),
        ),
        require_gap(
            "rsa-ols-rsatoolbox-python-vs-matlab",
            max_gap(
                rsa_relation["rsa"]["ols_coefficients"],
                matlab_relation["rsa"]["ols_coefficients"],
            ),
        ),
    ]
    for metric in (
        "pearson_two_category",
        "spearman_two_category",
        "pearson_graded",
        "spearman_graded",
    ):
        agreements.append(
            require_gap(
                f"rsa-{metric}-rsatoolbox-python-vs-matlab",
                abs(
                    float(rsa_relation["rsa"][metric])
                    - float(matlab_relation["rsa"][metric])
                ),
            )
        )

    predictive = pymvpa["scenarios"]["predictive"]
    return {
        "schema": SCHEMA,
        "fixture_sha256": fixture["fixture_sha256"],
        "references": {
            "rsatoolbox_python": rsa_python["reference"],
            "rsatoolbox_matlab": rsa_matlab["reference"],
            "pymvpa": pymvpa["reference"],
        },
        "agreements": agreements,
        "scenarios": {
            "observation": {
                "pair_order": fixture["observation"]["pair_order"],
                "squared_euclidean_raw": pymvpa_observation["sqeuclidean"],
                "squared_euclidean_per_feature": rsa_observation[
                    "squared_euclidean_per_feature"
                ],
                "euclidean": pymvpa_observation["euclidean"],
                "correlation": rsa_observation["correlation"],
                "target_similarity_on_squared_euclidean": pymvpa_observation[
                    "target_similarity_on_sqeuclidean"
                ],
            },
            "relational": {
                "pair_order": fixture["relational"]["pair_order"],
                "crossnobis_fixed_precision": rsa_relation[
                    "crossnobis_fixed_precision"
                ],
                "crossvalidated_squared_euclidean": rsa_relation[
                    "crossvalidated_squared_euclidean"
                ],
                "rsa": rsa_relation["rsa"],
                "matlab_r_squared": matlab_relation["rsa"]["r_squared"],
            },
            "predictive": {
                "folds": predictive["folds"],
                "formula_matched_centroid": predictive[
                    "formula_matched_centroid"
                ],
                "builtin_lda": predictive["builtin_lda"],
                "fixed_support_searchlights": predictive[
                    "fixed_support_searchlights"
                ],
            },
        },
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--rsatoolbox-python", required=True, type=Path)
    parser.add_argument("--pymvpa", required=True, type=Path)
    parser.add_argument("--rsatoolbox-matlab", required=True, type=Path)
    parser.add_argument("--check", type=Path)
    args = parser.parse_args()

    receipt = build_receipt(
        load_json(args.rsatoolbox_python),
        load_json(args.pymvpa),
        load_matlab(args.rsatoolbox_matlab),
    )
    if args.check is not None:
        expected = load_json(args.check)
        if canonical_json(receipt) != canonical_json(expected):
            raise SystemExit(f"external conformance receipt drift: {args.check}")
        print(f"checked {args.check}")
    else:
        print(json.dumps(receipt, allow_nan=False, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
