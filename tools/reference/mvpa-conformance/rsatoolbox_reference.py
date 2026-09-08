#!/usr/bin/env python3
"""Independent Python-rsatoolbox arm of the MVPA conformance court."""

from __future__ import annotations

import argparse
import hashlib
import inspect
import json
import platform
from importlib import metadata
from pathlib import Path
from typing import Any

import numpy as np
import scipy
from rsatoolbox.data import Dataset
from rsatoolbox.rdm import RDMs, compare
from rsatoolbox.rdm.calc import calc_rdm, calc_rdm_crossnobis

from fixture import build_fixture, canonical_json


SCHEMA = "scalafim-mvpa-rsatoolbox-result/v1"


def sha256_file(path: str) -> str:
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def vector(value: Any) -> list[float]:
    return [float(item) for item in np.asarray(value).reshape(-1)]


def rdm(values: list[float], condition_ids: list[str], measure: str) -> RDMs:
    return RDMs(
        np.asarray(values, dtype=float),
        dissimilarity_measure=measure,
        pattern_descriptors={"condition": np.asarray(condition_ids)},
    )


def observation_scenarios(fixture: dict[str, Any]) -> dict[str, Any]:
    # BEGIN SCENARIO rsatoolbox-python-observation-rdm
    source = fixture["observation"]
    patterns = np.asarray(source["patterns"], dtype=float)
    dataset = Dataset(
        measurements=patterns,
        obs_descriptors={"sample_id": np.asarray(source["sample_ids"])},
        channel_descriptors={"feature_id": np.asarray(source["feature_ids"])},
    )
    squared_per_feature = calc_rdm(dataset, method="euclidean")
    correlation = calc_rdm(dataset, method="correlation")
    return {
        "pair_order": source["pair_order"],
        "squared_euclidean_per_feature": vector(squared_per_feature.get_vectors()),
        "correlation": vector(correlation.get_vectors()),
        "reported_methods": {
            "squared_euclidean_per_feature": squared_per_feature.dissimilarity_measure,
            "correlation": correlation.dissimilarity_measure,
        },
    }
    # END SCENARIO rsatoolbox-python-observation-rdm


def relational_scenarios(fixture: dict[str, Any]) -> dict[str, Any]:
    # BEGIN SCENARIO rsatoolbox-python-relational-crossnobis
    source = fixture["relational"]
    conditions = source["condition_ids"]
    runs = source["run_ids"]
    patterns_by_run = np.asarray(source["patterns_by_run"], dtype=float)
    precision = np.asarray(source["precision"], dtype=float)
    patterns = patterns_by_run.reshape(-1, patterns_by_run.shape[-1])
    condition_column = np.tile(np.asarray(conditions), len(runs))
    run_column = np.repeat(np.asarray(runs), len(conditions))

    supports = {
        "whole": np.arange(patterns.shape[1]),
        "anterior": np.asarray([0, 1, 2]),
        "posterior": np.asarray([3, 4, 5]),
    }
    fixed: dict[str, list[float]] = {}
    identity: dict[str, list[float]] = {}
    explicit_all_pairs: dict[str, list[float]] = {}
    triu = np.triu_indices(len(conditions), 1)

    for name, support in supports.items():
        dataset = Dataset(
            measurements=patterns[:, support],
            obs_descriptors={"condition": condition_column, "run": run_column},
            channel_descriptors={"feature_id": np.asarray(source["feature_ids"])[support]},
        )
        local_precision = precision[np.ix_(support, support)]
        fixed_rdm = calc_rdm_crossnobis(
            dataset,
            descriptor="condition",
            cv_descriptor="run",
            noise=local_precision,
        )
        identity_rdm = calc_rdm_crossnobis(
            dataset,
            descriptor="condition",
            cv_descriptor="run",
            noise=np.eye(len(support)),
        )
        fixed[name] = vector(fixed_rdm.get_vectors())
        identity[name] = vector(identity_rdm.get_vectors())

        accumulator = np.zeros(len(triu[0]), dtype=float)
        edges = 0
        for left in range(len(runs) - 1):
            for right in range(left + 1, len(runs)):
                first = patterns_by_run[left, :, :][:, support]
                second = patterns_by_run[right, :, :][:, support]
                gram = first @ local_precision @ second.T
                square = (
                    np.diag(gram)[:, None]
                    + np.diag(gram)[None, :]
                    - gram
                    - gram.T
                )
                accumulator += square[triu] / len(support)
                edges += 1
        explicit_all_pairs[name] = vector(accumulator / edges)
    # END SCENARIO rsatoolbox-python-relational-crossnobis

    # BEGIN SCENARIO rsatoolbox-python-relational-rsa
    observed = rdm(fixed["whole"], conditions, "crossnobis")
    models = source["models"]
    category_model = rdm(models["two_category"], conditions, "model")
    graded_model = rdm(models["graded"], conditions, "model")
    design = np.column_stack(
        [
            np.ones(len(fixed["whole"])),
            np.asarray(models["two_category"]),
            np.asarray(models["graded"]),
        ]
    )
    coefficients, *_ = np.linalg.lstsq(design, np.asarray(fixed["whole"]), rcond=None)
    residuals = np.asarray(fixed["whole"]) - design @ coefficients

    return {
        "pair_order": source["pair_order"],
        "crossnobis_fixed_precision": fixed,
        "crossvalidated_squared_euclidean": identity,
        "explicit_all_pairs_fixed_precision": explicit_all_pairs,
        "rsa": {
            "pearson_two_category": float(compare(observed, category_model, method="corr")[0, 0]),
            "spearman_two_category": float(compare(observed, category_model, method="spearman")[0, 0]),
            "pearson_graded": float(compare(observed, graded_model, method="corr")[0, 0]),
            "spearman_graded": float(compare(observed, graded_model, method="spearman")[0, 0]),
            "ols_terms": ["intercept", "two_category", "graded"],
            "ols_coefficients": vector(coefficients),
            "ols_residual_sum_squares": float(residuals @ residuals),
        },
    }
    # END SCENARIO rsatoolbox-python-relational-rsa


def build_result() -> dict[str, Any]:
    fixture = build_fixture()
    calc_source = inspect.getsourcefile(calc_rdm_crossnobis)
    compare_source = inspect.getsourcefile(compare)
    return {
        "schema": SCHEMA,
        "fixture_sha256": fixture["fixture_sha256"],
        "reference": {
            "name": "rsatoolbox-python",
            "version": metadata.version("rsatoolbox"),
            "python": platform.python_version(),
            "numpy": np.__version__,
            "scipy": scipy.__version__,
            "calc_source_sha256": sha256_file(calc_source),
            "compare_source_sha256": sha256_file(compare_source),
        },
        "scenarios": {
            "observation": observation_scenarios(fixture),
            "relational": relational_scenarios(fixture),
        },
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", type=Path)
    args = parser.parse_args()
    result = build_result()
    rendered = json.dumps(result, allow_nan=False, indent=2, sort_keys=True) + "\n"
    if args.check is not None:
        expected = json.loads(args.check.read_text(encoding="utf-8"))
        if canonical_json(result) != canonical_json(expected):
            raise SystemExit(f"reference drift: {args.check}")
        print(f"checked {args.check}")
    else:
        print(rendered, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
