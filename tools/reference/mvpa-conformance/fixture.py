#!/usr/bin/env python3
"""Canonical, dependency-free fixture for the MVPA conformance study.

The reference programs and the generated Scala court consume the same exact
numbers.  The fixture deliberately contains three different scientific
objects: observation patterns, partitioned condition relations, and a
supervised sample table.  Keeping them distinct prevents a convenient data
layout from silently changing the estimand.
"""

from __future__ import annotations

import hashlib
import json
import math
from typing import Any


SCHEMA = "scalafim-mvpa-conformance-fixture/v1"
SEED = 20260826


def canonical_json(value: Any) -> str:
    return json.dumps(value, allow_nan=False, separators=(",", ":"), sort_keys=True)


def digest(value: Any) -> str:
    return hashlib.sha256(canonical_json(value).encode("utf-8")).hexdigest()


def pair_order(keys: list[str]) -> list[list[str]]:
    return [[keys[i], keys[j]] for i in range(len(keys) - 1) for j in range(i + 1, len(keys))]


def _relational_patterns() -> list[list[list[float]]]:
    common = [0.20, -0.40, 0.50, 0.10, -0.20, 0.30]
    null_direction = [0.60, -0.30, 0.20, -0.50, 0.40, -0.10]
    null_scales = [1.0, -1.0, 0.8, -0.9]
    stable_c = [-0.65, 1.05, 0.55, -0.35, 0.75, 0.25]
    stable_d = [0.95, 0.35, -0.75, 0.80, -0.45, 0.60]

    runs: list[list[list[float]]] = []
    for run in range(4):
        offset = [0.035 * math.sin((run + 1) * (feature + 1)) for feature in range(6)]
        scale = null_scales[run]
        first = [
            common[feature] + offset[feature] + 0.5 * scale * null_direction[feature]
            for feature in range(6)
        ]
        second = [
            common[feature] + offset[feature] - 0.5 * scale * null_direction[feature]
            for feature in range(6)
        ]
        third = [
            stable_c[feature]
            + 0.045 * math.sin((run + 2) * (feature + 1) + 0.3)
            for feature in range(6)
        ]
        fourth = [
            stable_d[feature]
            + 0.040 * math.cos((run + 1) * (feature + 2) - 0.2)
            for feature in range(6)
        ]
        runs.append([first, second, third, fourth])
    return runs


def _predictive_patterns() -> list[list[float]]:
    bases = [
        [-2.00, -1.10, 0.30, 0.00],
        [0.10, 1.75, -1.20, 0.00],
        [1.85, -0.35, 1.10, 0.00],
    ]
    run_shifts = [
        [-0.25, 0.10, 0.00, 0.15],
        [0.20, -0.15, 0.10, -0.05],
        [0.05, 0.25, -0.15, 0.00],
        [-0.10, -0.20, 0.20, -0.10],
    ]
    rows: list[list[float]] = []
    for run in range(4):
        for category in range(3):
            rows.append(
                [
                    bases[category][feature]
                    + run_shifts[run][feature]
                    + (
                        0.030 * math.sin((run + 1) * (feature + 1))
                        if feature == 3
                        else 0.075 * math.sin((run + 1) * (category + 2) * (feature + 1))
                    )
                    for feature in range(4)
                ]
            )
    return rows


def build_fixture() -> dict[str, Any]:
    observation_samples = [f"observation-{index + 1}" for index in range(6)]
    observation_labels = ["a", "a", "b", "b", "c", "c"]
    observation = {
        "sample_ids": observation_samples,
        "feature_ids": [f"observation-feature-{index + 1}" for index in range(4)],
        "patterns": [
            [-2.00, -0.80, 0.20, 1.00],
            [-1.55, -1.05, 0.45, 0.70],
            [0.10, 1.20, -0.80, 0.50],
            [0.45, 0.75, -1.10, 0.15],
            [1.70, -0.20, 1.30, -0.90],
            [2.10, 0.15, 0.95, -1.25],
        ],
        "category_labels": observation_labels,
        "pair_order": pair_order(observation_samples),
        "target_rdm": [
            0.0 if observation_labels[i] == observation_labels[j] else 1.0
            for i in range(5)
            for j in range(i + 1, 6)
        ],
    }

    conditions = ["condition-a", "condition-b", "condition-c", "condition-d"]
    runs = [f"run-{index + 1}" for index in range(4)]
    relational = {
        "condition_ids": conditions,
        "run_ids": runs,
        "feature_ids": [f"relation-feature-{index + 1}" for index in range(6)],
        "patterns_by_run": _relational_patterns(),
        "precision": [
            [1.40, 0.12, 0.00, 0.00, 0.00, 0.00],
            [0.12, 1.20, -0.08, 0.00, 0.00, 0.00],
            [0.00, -0.08, 1.05, 0.10, 0.00, 0.00],
            [0.00, 0.00, 0.10, 1.30, -0.09, 0.00],
            [0.00, 0.00, 0.00, -0.09, 1.15, 0.11],
            [0.00, 0.00, 0.00, 0.00, 0.11, 1.25],
        ],
        "pair_order": pair_order(conditions),
        "models": {
            "two_category": [0.0, 1.0, 1.0, 1.0, 1.0, 0.0],
            "graded": [1.0, 2.0, 3.0, 1.0, 2.0, 1.0],
        },
    }

    classes = ["class-a", "class-b", "class-c"]
    predictive_runs = [f"run-{index + 1}" for index in range(4)]
    predictive = {
        "sample_ids": [
            f"{run}-{category}" for run in predictive_runs for category in classes
        ],
        "feature_ids": [f"predictive-feature-{index + 1}" for index in range(4)],
        "class_ids": classes,
        "labels": classes * 4,
        "run_ids": [run for run in predictive_runs for _ in classes],
        "patterns": _predictive_patterns(),
        "searchlights": [
            {"center": "predictive-feature-1", "ordinals": [0, 1]},
            {"center": "predictive-feature-2", "ordinals": [0, 1, 2]},
            {"center": "predictive-feature-3", "ordinals": [1, 2, 3]},
            {"center": "predictive-feature-4", "ordinals": [3]},
        ],
    }

    payload = {
        "schema": SCHEMA,
        "seed": SEED,
        "observation": observation,
        "relational": relational,
        "predictive": predictive,
    }
    return {**payload, "fixture_sha256": digest(payload)}


if __name__ == "__main__":
    print(json.dumps(build_fixture(), allow_nan=False, indent=2, sort_keys=True))
