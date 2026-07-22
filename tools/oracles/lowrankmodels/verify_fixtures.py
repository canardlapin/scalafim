#!/usr/bin/env python3
"""Verify generated LowRankModels.jl fixtures without importing Julia packages."""

from __future__ import annotations

import json
import math
import sys
from pathlib import Path


SCHEMA_VERSION = "scalafim.lowrankmodels.oracle.v1"
UPSTREAM_REPOSITORY = "https://github.com/madeleineudell/LowRankModels.jl.git"
UPSTREAM_COMMIT = "a18f0df45f1a6ce37634bf4e347062b6090397eb"
EXPECTED_CASES = {
    "quadratic-masked-rank2": "quadratic",
    "logistic-masked-rank2": "logistic",
    "poisson-masked-rank2": "poisson",
    "categorical-masked-rank2": "categorical",
}
ABS_TOL = 1e-12
REL_TOL = 1e-11


def close(actual: float, expected: float, label: str) -> None:
    if not math.isclose(actual, expected, abs_tol=ABS_TOL, rel_tol=REL_TOL):
        raise AssertionError(f"{label}: expected {expected:.17g}, got {actual:.17g}")


def rectangular(matrix: list[list[object]], label: str) -> tuple[int, int]:
    if not matrix or not matrix[0]:
        raise AssertionError(f"{label} must be non-empty")
    width = len(matrix[0])
    if any(len(row) != width for row in matrix):
        raise AssertionError(f"{label} must be rectangular")
    return len(matrix), width


def dot(left: list[float], right: list[float]) -> float:
    return sum(a * b for a, b in zip(left, right, strict=True))


def sigmoid(value: float) -> float:
    if value >= 0:
        inverse = math.exp(-value)
        return 1.0 / (1.0 + inverse)
    exponential = math.exp(value)
    return exponential / (1.0 + exponential)


def softplus(value: float) -> float:
    return max(value, 0.0) + math.log1p(math.exp(-abs(value)))


def softmax(values: list[float]) -> list[float]:
    shift = max(values)
    weights = [math.exp(value - shift) for value in values]
    total = sum(weights)
    return [value / total for value in weights]


def penalty_value(matrix: list[list[float]], penalty: dict[str, object]) -> float:
    weight = float(penalty["scalaWeight"])
    kind = penalty["kind"]
    if kind == "none":
        return 0.0
    if kind == "l1":
        return weight * sum(abs(value) for row in matrix for value in row)
    if kind == "squared-frobenius":
        return 0.5 * weight * sum(value * value for row in matrix for value in row)
    raise AssertionError(f"unsupported penalty {kind!r}")


def verify_penalty_mapping(penalty: dict[str, object], label: str) -> None:
    weight = float(penalty["scalaWeight"])
    expected = weight / 2 if penalty["kind"] == "squared-frobenius" else weight
    close(float(penalty["juliaScale"]), expected, f"{label}.juliaScale")


def verify_scalar(case: dict[str, object]) -> None:
    case_id = str(case["id"])
    loss = str(case["loss"])
    data = case["data"]
    observed = case["observed"]
    row_codes = case["rowCodes"]
    decoder = case["decoder"]
    natural = case["naturalParameters"]
    gradients = case["naturalGradients"]
    decoded = case["decoded"]
    rows, columns = rectangular(data, f"{case_id}.data")
    if rectangular(observed, f"{case_id}.observed") != (rows, columns):
        raise AssertionError(f"{case_id}: observation shape mismatch")
    rank = int(case["rank"])
    if rectangular(row_codes, f"{case_id}.rowCodes") != (rows, rank):
        raise AssertionError(f"{case_id}: row-code shape mismatch")
    if rectangular(decoder, f"{case_id}.decoder") != (columns, rank):
        raise AssertionError(f"{case_id}: decoder shape mismatch")
    for label, matrix in (
        ("naturalParameters", natural),
        ("naturalGradients", gradients),
        ("decoded", decoded),
    ):
        if rectangular(matrix, f"{case_id}.{label}") != (rows, columns):
            raise AssertionError(f"{case_id}: {label} shape mismatch")

    observed_loss = 0.0
    response_constant = 0.0
    for row in range(rows):
        for column in range(columns):
            theta = dot(row_codes[row], decoder[column])
            close(float(natural[row][column]), theta, f"{case_id}.natural[{row},{column}]")
            value = float(data[row][column])
            if loss == "quadratic":
                prediction = theta
                gradient = theta - value
                contribution = 0.5 * gradient * gradient
            elif loss == "logistic":
                prediction = sigmoid(theta)
                gradient = prediction - value
                contribution = softplus(theta) - value * theta
            elif loss == "poisson":
                prediction = math.exp(theta)
                gradient = prediction - value
                constant = 0.0 if value == 0 else value * (math.log(value) - 1.0)
                contribution = prediction - value * theta + constant
            else:
                raise AssertionError(f"{case_id}: unexpected scalar loss {loss}")
            close(float(decoded[row][column]), prediction, f"{case_id}.decoded[{row},{column}]")
            if observed[row][column]:
                if gradients[row][column] is None:
                    raise AssertionError(f"{case_id}: observed gradient is null at {row},{column}")
                close(float(gradients[row][column]), gradient, f"{case_id}.gradient[{row},{column}]")
                observed_loss += contribution
                if loss == "poisson":
                    response_constant += constant
            elif gradients[row][column] is not None:
                raise AssertionError(f"{case_id}: unobserved gradient must be null at {row},{column}")

    objective = case["objective"]
    close(float(objective["observedEntryLoss"]), observed_loss, f"{case_id}.observedEntryLoss")
    close(float(objective["responseConstant"]), response_constant, f"{case_id}.responseConstant")
    verify_objective(case)


def verify_categorical(case: dict[str, object]) -> None:
    case_id = str(case["id"])
    data = case["data"]
    observed = case["observed"]
    row_codes = case["rowCodes"]
    decoder = case["decoder"]
    natural = case["naturalParameters"]
    gradients = case["naturalGradients"]
    decoded = case["decoded"]
    rows, columns = rectangular(data, f"{case_id}.data")
    if columns != 1 or rectangular(observed, f"{case_id}.observed") != (rows, 1):
        raise AssertionError(f"{case_id}: categorical fixture must have one feature")
    rank = int(case["rank"])
    levels = int(case["levels"])
    if rectangular(row_codes, f"{case_id}.rowCodes") != (rows, rank):
        raise AssertionError(f"{case_id}: row-code shape mismatch")
    if rectangular(decoder, f"{case_id}.decoder") != (levels, rank):
        raise AssertionError(f"{case_id}: expanded decoder shape mismatch")

    observed_loss = 0.0
    for row in range(rows):
        expected_natural = [dot(row_codes[row], decoder[level]) for level in range(levels)]
        actual_natural = natural[row][0]
        if len(actual_natural) != levels:
            raise AssertionError(f"{case_id}: natural width mismatch at row {row}")
        for level in range(levels):
            close(float(actual_natural[level]), expected_natural[level], f"{case_id}.natural[{row},{level}]")
        probabilities = softmax(expected_natural)
        actual_decoded = decoded[row][0]
        for level in range(levels):
            close(float(actual_decoded[level]), probabilities[level], f"{case_id}.decoded[{row},{level}]")
        level_index = int(data[row][0])
        if not 0 <= level_index < levels:
            raise AssertionError(f"{case_id}: invalid level {level_index}")
        if observed[row][0]:
            actual_gradient = gradients[row][0]
            for level in range(levels):
                expected_gradient = probabilities[level] - (1.0 if level == level_index else 0.0)
                close(float(actual_gradient[level]), expected_gradient, f"{case_id}.gradient[{row},{level}]")
            maximum = max(expected_natural)
            observed_loss += maximum + math.log(
                sum(math.exp(value - maximum) for value in expected_natural)
            ) - expected_natural[level_index]
        elif any(value is not None for value in gradients[row][0]):
            raise AssertionError(f"{case_id}: unobserved categorical gradient must be null at row {row}")

    objective = case["objective"]
    close(float(objective["observedEntryLoss"]), observed_loss, f"{case_id}.observedEntryLoss")
    close(float(objective["responseConstant"]), 0.0, f"{case_id}.responseConstant")
    verify_objective(case)


def verify_objective(case: dict[str, object]) -> None:
    case_id = str(case["id"])
    row_penalty = case["rowPenalty"]
    decoder_penalty = case["decoderPenalty"]
    verify_penalty_mapping(row_penalty, f"{case_id}.rowPenalty")
    verify_penalty_mapping(decoder_penalty, f"{case_id}.decoderPenalty")
    expected_row = penalty_value(case["rowCodes"], row_penalty)
    expected_decoder = penalty_value(case["decoder"], decoder_penalty)
    objective = case["objective"]
    close(float(objective["rowPenalty"]), expected_row, f"{case_id}.rowPenalty.value")
    close(float(objective["decoderPenalty"]), expected_decoder, f"{case_id}.decoderPenalty.value")
    expected_total = float(objective["observedEntryLoss"]) + expected_row + expected_decoder
    close(float(objective["total"]), expected_total, f"{case_id}.total")


def verify(path: Path) -> None:
    fixture = json.loads(path.read_text(encoding="utf-8"))
    if fixture.get("schemaVersion") != SCHEMA_VERSION:
        raise AssertionError("unexpected fixture schema version")
    provenance = fixture.get("provenance", {})
    if provenance.get("repository") != UPSTREAM_REPOSITORY:
        raise AssertionError("unexpected upstream repository")
    if provenance.get("commit") != UPSTREAM_COMMIT:
        raise AssertionError("unexpected upstream commit")
    if provenance.get("packageVersion") != "1.1.1":
        raise AssertionError("unexpected LowRankModels version")
    julia_parts = str(provenance.get("juliaVersion", "")).split(".")
    if len(julia_parts) < 2 or int(julia_parts[0]) != 1 or not 6 <= int(julia_parts[1]) <= 10:
        raise AssertionError("fixture was not generated with Julia 1.6 through 1.10")

    cases = fixture.get("cases", [])
    actual_cases = {str(case["id"]): str(case["loss"]) for case in cases}
    if actual_cases != EXPECTED_CASES:
        raise AssertionError(f"unexpected fixture cases: {actual_cases}")
    for case in cases:
        if case["loss"] == "categorical":
            verify_categorical(case)
        else:
            verify_scalar(case)

    print(f"verified {len(cases)} LowRankModels.jl cases from {path}")


def main() -> None:
    default = (
        Path(__file__).resolve().parents[3]
        / "modules/multivar/shared/src/test/resources/lowrankmodels/objective-v1.json"
    )
    path = Path(sys.argv[1]).resolve() if len(sys.argv) > 1 else default
    verify(path)


if __name__ == "__main__":
    main()
