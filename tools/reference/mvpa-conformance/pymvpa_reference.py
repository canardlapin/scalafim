#!/usr/bin/env python3
"""Independent PyMVPA arm of the MVPA conformance court.

This runner intentionally uses PyMVPA's Dataset, PDist, NFoldPartitioner,
CrossValidation, LDA, and classifier lifecycle.  The formula-matched centroid
is a small PyMVPA classifier because PyMVPA has no built-in learner with
ScalaFIM's exact fold-local z-score plus class-centroid definition.
"""

from __future__ import annotations

import argparse
import hashlib
import inspect
import json
import platform
from pathlib import Path
from typing import Any

import numpy as np
import scipy
import scipy.stats._distn_infrastructure as scipy_distributions

from fixture import build_fixture, canonical_json


# PyMVPA 2.6.5 passes an extradoc keyword removed by modern SciPy.  Applying
# the compatibility shim before importing PyMVPA changes no distribution
# calculation; it only discards the obsolete documentation argument.
_original_rv_continuous_init = scipy_distributions.rv_continuous.__init__


def _rv_continuous_init_without_extradoc(self: Any, *args: Any, **kwargs: Any) -> None:
    kwargs.pop("extradoc", None)
    _original_rv_continuous_init(self, *args, **kwargs)


scipy_distributions.rv_continuous.__init__ = _rv_continuous_init_without_extradoc

import mvpa2  # noqa: E402
from mvpa2.clfs.base import Classifier  # noqa: E402
from mvpa2.clfs.gda import LDA  # noqa: E402
from mvpa2.datasets.base import Dataset  # noqa: E402
from mvpa2.generators.partition import NFoldPartitioner  # noqa: E402
from mvpa2.measures.base import CrossValidation  # noqa: E402
from mvpa2.measures.rsa import PDist, PDistTargetSimilarity  # noqa: E402


SCHEMA = "scalafim-mvpa-pymvpa-result/v1"


def sha256_file(path: str) -> str:
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def as_strings(values: Any) -> list[str]:
    return [str(value) for value in np.asarray(values).reshape(-1)]


def as_floats(values: Any) -> list[float]:
    return [float(value) for value in np.asarray(values).reshape(-1)]


class FormulaMatchedCentroid(Classifier):
    """Fold-local population z-score followed by nearest class centroid."""

    __tags__ = ["linear"]

    def _train(self, dataset: Dataset) -> None:
        samples = np.asarray(dataset.samples, dtype=float)
        self._mean = samples.mean(axis=0)
        self._scale = samples.std(axis=0, ddof=0)
        if np.any(self._scale == 0.0):
            raise ValueError("constant training feature")
        standardized = (samples - self._mean) / self._scale
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


def observation_scenarios(fixture: dict[str, Any]) -> dict[str, Any]:
    # BEGIN SCENARIO pymvpa-observation-rdm
    source = fixture["observation"]
    dataset = Dataset(
        np.asarray(source["patterns"], dtype=float),
        sa={"targets": np.asarray(source["category_labels"])},
    )
    metrics = {
        name: as_floats(PDist(pairwise_metric=name)(dataset).samples)
        for name in ("sqeuclidean", "euclidean", "correlation")
    }
    target = np.asarray(source["target_rdm"], dtype=float)
    target_similarity = {}
    for comparison in ("pearson", "spearman"):
        value = PDistTargetSimilarity(
            target,
            pairwise_metric="sqeuclidean",
            comparison_metric=comparison,
            corrcoef_only=True,
        )(dataset)
        target_similarity[comparison] = float(value.samples.reshape(-1)[0])
    return {
        "pair_order": source["pair_order"],
        **metrics,
        "target_similarity_on_sqeuclidean": target_similarity,
    }
    # END SCENARIO pymvpa-observation-rdm


def predictive_dataset(fixture: dict[str, Any], ordinals: list[int] | None = None) -> Dataset:
    source = fixture["predictive"]
    selected = np.arange(len(source["sample_ids"])) if ordinals is None else np.asarray(ordinals)
    return Dataset(
        np.asarray(source["patterns"], dtype=float)[selected],
        sa={
            "targets": np.asarray(source["labels"])[selected],
            "chunks": np.asarray(source["run_ids"])[selected],
            "sample_id": np.asarray(source["sample_ids"])[selected],
        },
        fa={"feature_id": np.asarray(source["feature_ids"])},
    )


def folds(dataset: Dataset) -> list[dict[str, Any]]:
    result = []
    for fold, partition in enumerate(NFoldPartitioner().generate(dataset)):
        roles = np.asarray(partition.sa["partitions"].value)
        sample_ids = np.asarray(partition.sa["sample_id"].value)
        result.append(
            {
                "fold": fold,
                "analysis": as_strings(sample_ids[roles == 1]),
                "assessment": as_strings(sample_ids[roles == 2]),
            }
        )
    return result


def run_classifier(dataset: Dataset, classifier: Classifier) -> dict[str, Any]:
    cross_validation = CrossValidation(
        classifier,
        NFoldPartitioner(),
        errorfx=None,
        enable_ca=["stats"],
    )
    predictions = cross_validation(dataset)
    predicted = as_strings(predictions.samples)
    truth = as_strings(predictions.sa["targets"].value)
    assessment_order = [
        sample_id
        for fold in folds(dataset)
        for sample_id in fold["assessment"]
    ]
    return {
        "sample_ids": assessment_order,
        "predicted": predicted,
        "truth": truth,
        "accuracy": float(np.mean(np.asarray(predicted) == np.asarray(truth))),
        "cvfolds": [int(value) for value in predictions.sa["cvfolds"].value],
    }


def predictive_scenarios(fixture: dict[str, Any]) -> dict[str, Any]:
    # BEGIN SCENARIO pymvpa-centroid-searchlights
    dataset = predictive_dataset(fixture)
    centroid = run_classifier(dataset, FormulaMatchedCentroid())
    lda = run_classifier(dataset, LDA())

    searchlights = []
    for searchlight in fixture["predictive"]["searchlights"]:
        local = dataset[:, searchlight["ordinals"]]
        estimate = run_classifier(local, FormulaMatchedCentroid())
        searchlights.append(
            {
                "center": searchlight["center"],
                "ordinals": searchlight["ordinals"],
                "accuracy": estimate["accuracy"],
                "predicted": estimate["predicted"],
            }
        )

    return {
        "folds": folds(dataset),
        "formula_matched_centroid": centroid,
        "builtin_lda": lda,
        "fixed_support_searchlights": searchlights,
    }
    # END SCENARIO pymvpa-centroid-searchlights


def build_result() -> dict[str, Any]:
    fixture = build_fixture()
    return {
        "schema": SCHEMA,
        "fixture_sha256": fixture["fixture_sha256"],
        "reference": {
            "name": "pymvpa",
            "version": mvpa2.__version__,
            "python": platform.python_version(),
            "numpy": np.__version__,
            "scipy": scipy.__version__,
            "pdist_source_sha256": sha256_file(inspect.getsourcefile(PDist)),
            "cross_validation_source_sha256": sha256_file(inspect.getsourcefile(CrossValidation)),
            "lda_source_sha256": sha256_file(inspect.getsourcefile(LDA)),
        },
        "scenarios": {
            "observation": observation_scenarios(fixture),
            "predictive": predictive_scenarios(fixture),
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
