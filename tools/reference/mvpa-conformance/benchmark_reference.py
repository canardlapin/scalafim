#!/usr/bin/env python3
"""Host-local steady-state benchmarks for matched MVPA conformance estimands.

Setup is deliberately outside the timed call.  The Python allocation field is
the peak observed by ``tracemalloc`` for one call and therefore excludes most
native NumPy/SciPy storage; it must not be compared directly with JMH B/op.
"""

from __future__ import annotations

import argparse
import gc
import hashlib
import inspect
import json
import math
import os
import platform
import resource
import statistics
import sys
import time
import tracemalloc
from importlib import metadata
from pathlib import Path
from typing import Any, Callable

import numpy as np
import scipy


SCHEMA = "scalafim-mvpa-python-performance/v1"
SHAPES = {
    "observation": {"samples": 96, "features": 64},
    "relational": {"partitions": 8, "effects": 8, "features": 64},
    "predictive": {
        "runs": 8,
        "classes": 4,
        "features": 32,
        "supports": 16,
        "support_width": 8,
    },
}


def sha256_file(path: str | None) -> str:
    if path is None:
        raise ValueError("source path is unavailable")
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def observation_patterns() -> np.ndarray:
    shape = SHAPES["observation"]
    rows = np.arange(1, shape["samples"] + 1, dtype=float)[:, None]
    columns = np.arange(1, shape["features"] + 1, dtype=float)[None, :]
    return np.sin(rows * 0.13 + columns * 0.07) + 0.25 * np.cos(rows * columns * 0.003)


def relational_patterns() -> np.ndarray:
    shape = SHAPES["relational"]
    result = np.empty(
        (shape["partitions"], shape["effects"], shape["features"]),
        dtype=float,
    )
    for partition in range(shape["partitions"]):
        for effect in range(shape["effects"]):
            for coordinate in range(shape["features"]):
                result[partition, effect, coordinate] = math.sin(
                    (effect + 1) * 0.31 + (coordinate + 1) * 0.017
                ) + 0.02 * partition * math.cos((coordinate + 1) * 0.11)
    return result


def predictive_patterns() -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    shape = SHAPES["predictive"]
    sample_count = shape["runs"] * shape["classes"]
    patterns = np.empty((sample_count, shape["features"]), dtype=float)
    targets: list[str] = []
    chunks: list[str] = []
    row = 0
    for run in range(shape["runs"]):
        for category in range(shape["classes"]):
            targets.append(f"class-{category}")
            chunks.append(f"run-{run}")
            for feature in range(shape["features"]):
                signal = 2.0 if feature % shape["classes"] == category else -0.4
                patterns[row, feature] = signal + 0.15 * math.sin(
                    (row + 1) * 0.37 + (feature + 1) * 0.19
                )
            row += 1
    return patterns, np.asarray(targets), np.asarray(chunks)


def supports() -> list[list[int]]:
    shape = SHAPES["predictive"]
    return [
        [
            (center * 2 + offset) % shape["features"]
            for offset in range(shape["support_width"])
        ]
        for center in range(shape["supports"])
    ]


def percentile(values: list[float], probability: float) -> float:
    ordered = sorted(values)
    position = (len(ordered) - 1) * probability
    lower = math.floor(position)
    upper = math.ceil(position)
    if lower == upper:
        return ordered[lower]
    fraction = position - lower
    return ordered[lower] * (1.0 - fraction) + ordered[upper] * fraction


def benchmark(
    scenario: str,
    operation: Callable[[], Any],
    checksum: Callable[[Any], float],
    calls: int,
    repeats: int,
    warmups: int,
) -> dict[str, Any]:
    for _ in range(warmups):
        operation()

    durations: list[float] = []
    last: Any = None
    for _ in range(repeats):
        gc.collect()
        started = time.perf_counter_ns()
        for _ in range(calls):
            last = operation()
        elapsed = time.perf_counter_ns() - started
        durations.append(elapsed / calls)

    tracemalloc.start()
    operation()
    _, traced_peak = tracemalloc.get_traced_memory()
    tracemalloc.stop()

    return {
        "scenario": scenario,
        "stage": "steady-state-public-call",
        "calls_per_repeat": calls,
        "repeats": repeats,
        "warmup_calls": warmups,
        "median_ns_per_call": statistics.median(durations),
        "p25_ns_per_call": percentile(durations, 0.25),
        "p75_ns_per_call": percentile(durations, 0.75),
        "min_ns_per_call": min(durations),
        "max_ns_per_call": max(durations),
        "samples_ns_per_call": durations,
        "traced_python_peak_bytes_single_call": traced_peak,
        "checksum": checksum(last),
    }


def process_peak_rss_bytes() -> int:
    peak = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    if sys.platform.startswith("linux"):
        return int(peak * 1024)
    return int(peak)


def rsatoolbox_benchmarks(repeats: int) -> tuple[dict[str, Any], list[dict[str, Any]]]:
    from rsatoolbox.data import Dataset
    from rsatoolbox.rdm import RDMs, compare
    from rsatoolbox.rdm.calc import calc_rdm, calc_rdm_crossnobis

    observation = observation_patterns()
    observation_dataset = Dataset(
        observation,
        obs_descriptors={"sample": np.arange(observation.shape[0])},
        channel_descriptors={"feature": np.arange(observation.shape[1])},
    )
    observation_op = lambda: calc_rdm(observation_dataset, method="correlation")

    relation = relational_patterns()
    relation_shape = SHAPES["relational"]
    relation_dataset = Dataset(
        relation.reshape(-1, relation_shape["features"]),
        obs_descriptors={
            "condition": np.tile(
                np.asarray([f"condition-{value}" for value in range(relation_shape["effects"])]),
                relation_shape["partitions"],
            ),
            "run": np.repeat(
                np.asarray([f"run-{value}" for value in range(relation_shape["partitions"])]),
                relation_shape["effects"],
            ),
        },
        channel_descriptors={"feature": np.arange(relation_shape["features"])},
    )
    identity = np.eye(relation_shape["features"])
    crossnobis_op = lambda: calc_rdm_crossnobis(
        relation_dataset,
        descriptor="condition",
        cv_descriptor="run",
        noise=identity,
    )
    observed = crossnobis_op()
    model_values = np.asarray(
        [
            float((left % 2) != (right % 2))
            for left in range(relation_shape["effects"] - 1)
            for right in range(left + 1, relation_shape["effects"])
        ]
    )
    model = RDMs(model_values, dissimilarity_measure="model")
    pearson_op = lambda: compare(observed, model, method="corr")
    design = np.column_stack([np.ones(len(model_values)), model_values])
    response = observed.get_vectors().reshape(-1)
    ols_op = lambda: np.linalg.lstsq(design, response, rcond=None)[0]

    results = [
        benchmark(
            "observation-correlation",
            observation_op,
            lambda value: float(np.sum(value.get_vectors())),
            calls=20,
            repeats=repeats,
            warmups=5,
        ),
        benchmark(
            "crossvalidated-identity-rdm",
            crossnobis_op,
            lambda value: float(np.sum(value.get_vectors())),
            calls=5,
            repeats=repeats,
            warmups=3,
        ),
        benchmark(
            "rsa-pearson-query",
            pearson_op,
            lambda value: float(np.asarray(value).reshape(-1)[0]),
            calls=100,
            repeats=repeats,
            warmups=10,
        ),
        benchmark(
            "rsa-intercepted-ols-query",
            ols_op,
            lambda value: float(np.sum(value)),
            calls=100,
            repeats=repeats,
            warmups=10,
        ),
    ]
    reference = {
        "name": "rsatoolbox-python",
        "version": metadata.version("rsatoolbox"),
        "calc_source_sha256": sha256_file(inspect.getsourcefile(calc_rdm_crossnobis)),
        "compare_source_sha256": sha256_file(inspect.getsourcefile(compare)),
    }
    return reference, results


def pymvpa_benchmarks(repeats: int) -> tuple[dict[str, Any], list[dict[str, Any]]]:
    import scipy.stats._distn_infrastructure as scipy_distributions

    original_init = scipy_distributions.rv_continuous.__init__

    def without_extradoc(self: Any, *args: Any, **kwargs: Any) -> None:
        kwargs.pop("extradoc", None)
        original_init(self, *args, **kwargs)

    scipy_distributions.rv_continuous.__init__ = without_extradoc

    import mvpa2
    from mvpa2.clfs.base import Classifier
    from mvpa2.datasets.base import Dataset
    from mvpa2.generators.partition import NFoldPartitioner
    from mvpa2.measures.base import CrossValidation
    from mvpa2.measures.rsa import PDist

    class FormulaMatchedCentroid(Classifier):
        __tags__ = ["linear"]

        def _train(self, dataset: Dataset) -> None:
            values = np.asarray(dataset.samples, dtype=float)
            self._mean = values.mean(axis=0)
            self._scale = values.std(axis=0, ddof=0)
            standardized = (values - self._mean) / self._scale
            targets = np.asarray(dataset.sa[self.get_space()].value)
            self._classes = np.unique(targets)
            self._centroids = np.vstack(
                [standardized[targets == target].mean(axis=0) for target in self._classes]
            )

        def _predict(self, data: Any) -> list[str]:
            standardized = (np.asarray(data, dtype=float) - self._mean) / self._scale
            differences = standardized[:, None, :] - self._centroids[None, :, :]
            scores = -np.sum(differences * differences, axis=2)
            self.ca.estimates = scores
            return list(self._classes[np.argmax(scores, axis=1)])

    observation = observation_patterns()
    observation_dataset = Dataset(observation)
    pdist = PDist(pairwise_metric="correlation")
    observation_op = lambda: pdist(observation_dataset)

    patterns, targets, chunks = predictive_patterns()
    predictive_dataset = Dataset(patterns, sa={"targets": targets, "chunks": chunks})
    centroid_cv = CrossValidation(
        FormulaMatchedCentroid(),
        NFoldPartitioner(),
        errorfx=None,
    )
    centroid_op = lambda: centroid_cv(predictive_dataset)
    local_datasets = [predictive_dataset[:, support] for support in supports()]
    local_cvs = [
        CrossValidation(FormulaMatchedCentroid(), NFoldPartitioner(), errorfx=None)
        for _ in local_datasets
    ]
    searchlight_op = lambda: [
        measure(dataset) for measure, dataset in zip(local_cvs, local_datasets)
    ]

    def prediction_checksum(value: Any) -> float:
        predicted = np.asarray(value.samples).reshape(-1)
        return float(np.sum(predicted == targets))

    def frame_checksum(values: list[Any]) -> float:
        return float(sum(prediction_checksum(value) for value in values))

    results = [
        benchmark(
            "observation-correlation",
            observation_op,
            lambda value: float(np.sum(value.samples)),
            calls=20,
            repeats=repeats,
            warmups=5,
        ),
        benchmark(
            "predictive-centroid-loro",
            centroid_op,
            prediction_checksum,
            calls=5,
            repeats=repeats,
            warmups=3,
        ),
        benchmark(
            "predictive-fixed-support-frame",
            searchlight_op,
            frame_checksum,
            calls=1,
            repeats=repeats,
            warmups=2,
        ),
    ]
    reference = {
        "name": "pymvpa",
        "version": mvpa2.__version__,
        "pdist_source_sha256": sha256_file(inspect.getsourcefile(PDist)),
        "cross_validation_source_sha256": sha256_file(inspect.getsourcefile(CrossValidation)),
    }
    return reference, results


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--tool", choices=("rsatoolbox", "pymvpa"), required=True)
    parser.add_argument("--repeats", type=int, default=9)
    args = parser.parse_args()
    if args.repeats < 3:
        raise SystemExit("--repeats must be at least 3")

    if args.tool == "rsatoolbox":
        reference, results = rsatoolbox_benchmarks(args.repeats)
    else:
        reference, results = pymvpa_benchmarks(args.repeats)

    receipt = {
        "schema": SCHEMA,
        "host": {
            "platform": platform.platform(),
            "machine": platform.machine(),
            "processor": platform.processor(),
            "python": platform.python_version(),
            "numpy": np.__version__,
            "scipy": scipy.__version__,
            "logical_cpus": os.cpu_count(),
            "process_peak_rss_bytes": process_peak_rss_bytes(),
        },
        "reference": reference,
        "shapes": SHAPES,
        "allocation_scope": "tracemalloc Python heap only; excludes most native arrays",
        "results": results,
    }
    print(json.dumps(receipt, allow_nan=False, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
