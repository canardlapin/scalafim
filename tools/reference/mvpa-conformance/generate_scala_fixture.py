#!/usr/bin/env python3
"""Generate the portable JVM/Scala.js fixture from the checked receipt."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any, Callable

from fixture import build_fixture


HERE = Path(__file__).resolve().parent
DEFAULT_RECEIPT = HERE / "reference-results-v1.json"


def string(value: str) -> str:
    return json.dumps(value, ensure_ascii=False)


def number(value: float) -> str:
    rendered = repr(float(value))
    if rendered == "-0.0":
        return "-0.0"
    return rendered


def vector(values: list[Any], render: Callable[[Any], str]) -> str:
    return "Vector(" + ", ".join(render(value) for value in values) + ")"


def matrix(values: list[list[float]]) -> str:
    return vector(values, lambda row: vector(row, number))


def tensor(values: list[list[list[float]]]) -> str:
    return vector(values, matrix)


def string_pairs(values: list[list[str]]) -> str:
    return vector(values, lambda pair: f"{string(pair[0])} -> {string(pair[1])}")


def double_map(values: dict[str, list[float]]) -> str:
    entries = [f"{string(key)} -> {vector(value, number)}" for key, value in values.items()]
    return "Map(" + ", ".join(entries) + ")"


def render(receipt: dict[str, Any]) -> str:
    fixture = build_fixture()
    if receipt["fixture_sha256"] != fixture["fixture_sha256"]:
        raise SystemExit("reference receipt does not bind the current fixture")
    observation = fixture["observation"]
    relational = fixture["relational"]
    predictive = fixture["predictive"]
    expected_observation = receipt["scenarios"]["observation"]
    expected_relational = receipt["scenarios"]["relational"]
    expected_predictive = receipt["scenarios"]["predictive"]

    folds = vector(
        expected_predictive["folds"],
        lambda fold: (
            "ExpectedFold("
            + vector(fold["analysis"], string)
            + ", "
            + vector(fold["assessment"], string)
            + ")"
        ),
    )
    searchlights = vector(
        predictive["searchlights"],
        lambda item: (
            "SearchlightSupport("
            + string(item["center"])
            + ", "
            + vector(item["ordinals"], str)
            + ")"
        ),
    )
    expected_searchlights = vector(
        expected_predictive["fixed_support_searchlights"],
        lambda item: (
            "ExpectedSearchlight("
            + string(item["center"])
            + ", "
            + number(item["accuracy"])
            + ", "
            + vector(item["predicted"], string)
            + ")"
        ),
    )

    return f'''package scalafim.fmri.mvpa.scenarios

private[scenarios] object MvpaExternalReferenceFixture:
  val fixtureSha256 = {string(receipt["fixture_sha256"])}

  val observationSampleIds = {vector(observation["sample_ids"], string)}
  val observationFeatureIds = {vector(observation["feature_ids"], string)}
  val observationPatterns = {matrix(observation["patterns"])}
  val observationPairOrder = {string_pairs(observation["pair_order"])}
  val squaredEuclideanRaw = {vector(expected_observation["squared_euclidean_raw"], number)}
  val squaredEuclideanPerFeature = {vector(expected_observation["squared_euclidean_per_feature"], number)}
  val euclidean = {vector(expected_observation["euclidean"], number)}
  val correlation = {vector(expected_observation["correlation"], number)}
  val targetSimilarityPearson = {number(expected_observation["target_similarity_on_squared_euclidean"]["pearson"])}
  val targetSimilaritySpearman = {number(expected_observation["target_similarity_on_squared_euclidean"]["spearman"])}

  val conditionIds = {vector(relational["condition_ids"], string)}
  val relationRunIds = {vector(relational["run_ids"], string)}
  val relationFeatureIds = {vector(relational["feature_ids"], string)}
  val relationPatternsByRun = {tensor(relational["patterns_by_run"])}
  val precision = {matrix(relational["precision"])}
  val relationPairOrder = {string_pairs(relational["pair_order"])}
  val twoCategoryModel = {vector(relational["models"]["two_category"], number)}
  val gradedModel = {vector(relational["models"]["graded"], number)}
  val crossnobisFixedPrecision = {double_map(expected_relational["crossnobis_fixed_precision"])}
  val crossvalidatedSquaredEuclidean = {double_map(expected_relational["crossvalidated_squared_euclidean"])}
  val rsaPearsonTwoCategory = {number(expected_relational["rsa"]["pearson_two_category"])}
  val rsaSpearmanTwoCategory = {number(expected_relational["rsa"]["spearman_two_category"])}
  val rsaPearsonGraded = {number(expected_relational["rsa"]["pearson_graded"])}
  val rsaSpearmanGraded = {number(expected_relational["rsa"]["spearman_graded"])}
  val rsaOlsCoefficients = {vector(expected_relational["rsa"]["ols_coefficients"], number)}
  val rsaOlsResidualSumSquares = {number(expected_relational["rsa"]["ols_residual_sum_squares"])}

  val predictiveSampleIds = {vector(predictive["sample_ids"], string)}
  val predictiveFeatureIds = {vector(predictive["feature_ids"], string)}
  val classIds = {vector(predictive["class_ids"], string)}
  val predictiveLabels = {vector(predictive["labels"], string)}
  val predictiveRunIds = {vector(predictive["run_ids"], string)}
  val predictivePatterns = {matrix(predictive["patterns"])}

  final case class ExpectedFold(analysis: Vector[String], assessment: Vector[String])
  val expectedFolds = {folds}

  final case class SearchlightSupport(center: String, ordinals: Vector[Int])
  val searchlights = {searchlights}

  final case class ExpectedSearchlight(
      center: String,
      accuracy: Double,
      predicted: Vector[String]
  )
  val expectedSearchlights = {expected_searchlights}
  val expectedCentroidPredictions = {vector(expected_predictive["formula_matched_centroid"]["predicted"], string)}
  val expectedCentroidAccuracy = {number(expected_predictive["formula_matched_centroid"]["accuracy"])}
  val expectedLdaPredictions = {vector(expected_predictive["builtin_lda"]["predicted"], string)}
  val expectedLdaAccuracy = {number(expected_predictive["builtin_lda"]["accuracy"])}
'''


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--receipt", type=Path, default=DEFAULT_RECEIPT)
    parser.add_argument("--check", type=Path)
    args = parser.parse_args()
    output = render(json.loads(args.receipt.read_text(encoding="utf-8")))
    if args.check is not None:
        if args.check.read_text(encoding="utf-8") != output:
            raise SystemExit(f"generated Scala fixture drift: {args.check}")
        print(f"checked {args.check}")
    else:
        print(output, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
