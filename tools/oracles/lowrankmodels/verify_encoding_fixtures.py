#!/usr/bin/env python3
"""Independently verify frozen-decoder LowRankModels.jl encoding fixtures."""

from __future__ import annotations

import json
import math
import sys
from pathlib import Path

from verify_fixtures import (
    ABS_TOL,
    REL_TOL,
    UPSTREAM_COMMIT,
    UPSTREAM_REPOSITORY,
    close,
    dot,
    sigmoid,
    softplus,
)


SCHEMA_VERSION = "scalafim.lowrankmodels.encoding-oracle.v1"
EXPECTED_CASES = {
    "quadratic-dense-ridge": ("quadratic", "ridge"),
    "quadratic-sparse-l1": ("quadratic", "l1"),
    "logistic-sparse-ridge": ("logistic", "ridge"),
}


def proximal(penalty: str, weight: float, values: list[float], step: float) -> list[float]:
    if penalty == "ridge":
        return [value / (1.0 + step * weight) for value in values]
    if penalty == "l1":
        threshold = step * weight
        return [math.copysign(max(abs(value) - threshold, 0.0), value) for value in values]
    raise AssertionError(f"unsupported penalty {penalty}")


def verify_case(case: dict[str, object]) -> None:
    case_id = str(case["id"])
    loss = str(case["loss"])
    penalty = case["penalty"]
    penalty_kind = str(penalty["kind"])
    if EXPECTED_CASES.get(case_id) != (loss, penalty_kind):
        raise AssertionError(f"unexpected encoding case {case_id}")
    data = [float(value) for value in case["data"]]
    observed = [bool(value) for value in case["observed"]]
    decoder = case["decoder"]
    code = [float(value) for value in case["solution"]]
    rank = int(case["rank"])
    if len(code) != rank or len(case["initial"]) != rank:
        raise AssertionError(f"{case_id}: code dimension mismatch")
    if len(data) != len(observed) or len(data) != len(decoder):
        raise AssertionError(f"{case_id}: feature dimension mismatch")
    if any(len(row) != rank for row in decoder):
        raise AssertionError(f"{case_id}: decoder rank mismatch")

    natural = [dot(code, row) for row in decoder]
    gradient = [0.0] * rank
    observed_loss = 0.0
    decoded = []
    for feature, theta in enumerate(natural):
        close(float(case["naturalParameters"][feature]), theta, f"{case_id}.natural[{feature}]")
        if loss == "quadratic":
            prediction = theta
            natural_gradient = theta - data[feature]
            contribution = 0.5 * natural_gradient * natural_gradient
        elif loss == "logistic":
            prediction = sigmoid(theta)
            natural_gradient = prediction - data[feature]
            contribution = softplus(theta) - data[feature] * theta
        else:
            raise AssertionError(f"{case_id}: unsupported loss {loss}")
        decoded.append(prediction)
        close(float(case["decoded"][feature]), prediction, f"{case_id}.decoded[{feature}]")
        if observed[feature]:
            observed_loss += contribution
            for latent in range(rank):
                gradient[latent] += decoder[feature][latent] * natural_gradient

    weight = float(penalty["scalaWeight"])
    expected_julia_scale = weight / 2 if penalty_kind == "ridge" else weight
    close(float(penalty["juliaScale"]), expected_julia_scale, f"{case_id}.juliaScale")
    if penalty_kind == "ridge":
        row_penalty = 0.5 * weight * sum(value * value for value in code)
    else:
        row_penalty = weight * sum(abs(value) for value in code)
    objective = case["objective"]
    close(float(objective["observedEntryLoss"]), observed_loss, f"{case_id}.observedEntryLoss")
    close(float(objective["rowPenalty"]), row_penalty, f"{case_id}.rowPenalty")
    close(float(objective["total"]), observed_loss + row_penalty, f"{case_id}.total")

    step = float(case["residualStep"])
    candidate = proximal(
        penalty_kind,
        weight,
        [code[index] - step * gradient[index] for index in range(rank)],
        step,
    )
    residual = math.sqrt(sum((code[index] - candidate[index]) ** 2 for index in range(rank))) / step
    close(float(case["proxGradientResidual"]), residual, f"{case_id}.proxGradientResidual")
    if residual > 1e-9:
        raise AssertionError(f"{case_id}: stationarity residual is too large: {residual}")

    trajectory = [float(value) for value in case["objectiveTrajectory"]]
    if len(trajectory) != int(case["iterations"]) + 1:
        raise AssertionError(f"{case_id}: trajectory length does not match iterations")
    for index in range(1, len(trajectory)):
        if trajectory[index] > trajectory[index - 1] + ABS_TOL + REL_TOL * abs(trajectory[index - 1]):
            raise AssertionError(f"{case_id}: objective increased at iteration {index}")
    close(trajectory[-1], float(objective["total"]), f"{case_id}.trajectory.final")


def verify(path: Path) -> None:
    fixture = json.loads(path.read_text(encoding="utf-8"))
    if fixture.get("schemaVersion") != SCHEMA_VERSION:
        raise AssertionError("unexpected encoding fixture schema")
    provenance = fixture.get("provenance", {})
    if provenance.get("repository") != UPSTREAM_REPOSITORY:
        raise AssertionError("unexpected upstream repository")
    if provenance.get("commit") != UPSTREAM_COMMIT:
        raise AssertionError("unexpected upstream commit")
    if provenance.get("packageVersion") != "1.1.1" or provenance.get("juliaVersion") != "1.6.7":
        raise AssertionError("unexpected encoding fixture runtime provenance")
    cases = fixture.get("cases", [])
    if {case["id"] for case in cases} != set(EXPECTED_CASES):
        raise AssertionError("encoding fixture case inventory mismatch")
    for case in cases:
        verify_case(case)
    print(f"verified {len(cases)} LowRankModels.jl encoding cases from {path}")


def main() -> None:
    root = Path(__file__).resolve().parents[3]
    path = (
        Path(sys.argv[1]).resolve()
        if len(sys.argv) > 1
        else root / "modules/multivar/shared/src/test/resources/lowrankmodels/encoding-v1.json"
    )
    verify(path)


if __name__ == "__main__":
    main()
