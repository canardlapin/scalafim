#!/usr/bin/env python3
"""Independently verify deterministic fitted LowRankModels.jl fixtures."""

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
)


SCHEMA_VERSION = "scalafim.lowrankmodels.fitted-oracle.v1"
EXPECTED_STARTS = {"positive", "negative-sign", "skew"}
EXPECTED_CHECKPOINTS = [0, 1, 2, 5, 10, 25, 50, 100, 200]


def squared_norm(values: list[float]) -> float:
    return sum(value * value for value in values)


def fitted_quantities(
    data: list[list[float]],
    ridge: float,
    row_codes: list[float],
    decoder: list[float],
) -> tuple[list[list[float]], float, float]:
    reconstruction = [
        [row_code * coefficient for coefficient in decoder]
        for row_code in row_codes
    ]
    residual = [
        [reconstruction[row][column] - data[row][column] for column in range(len(decoder))]
        for row in range(len(row_codes))
    ]
    objective = 0.5 * sum(squared_norm(row) for row in residual)
    objective += 0.5 * ridge * (squared_norm(row_codes) + squared_norm(decoder))
    gradient_u = [
        sum(residual[row][column] * decoder[column] for column in range(len(decoder)))
        + ridge * row_codes[row]
        for row in range(len(row_codes))
    ]
    gradient_v = [
        sum(residual[row][column] * row_codes[row] for row in range(len(row_codes)))
        + ridge * decoder[column]
        for column in range(len(decoder))
    ]
    stationarity = max(math.sqrt(squared_norm(gradient_u)), math.sqrt(squared_norm(gradient_v)))
    return reconstruction, objective, stationarity


def verify(path: Path) -> None:
    fixture = json.loads(path.read_text(encoding="utf-8"))
    if fixture.get("schemaVersion") != SCHEMA_VERSION:
        raise AssertionError("unexpected fitted fixture schema")
    provenance = fixture.get("provenance", {})
    if provenance.get("repository") != UPSTREAM_REPOSITORY:
        raise AssertionError("unexpected upstream repository")
    if provenance.get("commit") != UPSTREAM_COMMIT:
        raise AssertionError("unexpected upstream commit")
    if provenance.get("packageVersion") != "1.1.1" or provenance.get("juliaVersion") != "1.6.7":
        raise AssertionError("unexpected fitted fixture runtime provenance")

    program = fixture["program"]
    if program.get("loss") != "quadratic" or int(program.get("rank")) != 1:
        raise AssertionError("unexpected fitted fixture program")
    data = [[float(value) for value in row] for row in program["data"]]
    ridge = float(program["rowPenalty"]["scalaWeight"])
    close(float(program["decoderPenalty"]["scalaWeight"]), ridge, "decoder ridge")
    close(float(program["rowPenalty"]["juliaScale"]), ridge / 2.0, "row Julia scale")
    close(float(program["decoderPenalty"]["juliaScale"]), ridge / 2.0, "decoder Julia scale")
    algorithm = fixture["algorithm"]
    if algorithm.get("implementation") != "LowRankModels.fit!(GLRM, ProxGradParams)":
        raise AssertionError("fitted fixture did not run the admitted upstream implementation")
    if int(algorithm.get("maximumIterations")) != EXPECTED_CHECKPOINTS[-1]:
        raise AssertionError("unexpected fitted iteration budget")

    starts = fixture.get("starts", [])
    if {start["id"] for start in starts} != EXPECTED_STARTS:
        raise AssertionError("fitted fixture start inventory mismatch")
    reference_reconstruction: list[list[float]] | None = None
    for start in starts:
        start_id = str(start["id"])
        row_codes = [float(value) for value in start["fittedFactors"]["rowCodes"]]
        decoder = [float(value) for value in start["fittedFactors"]["decoder"]]
        reconstruction, objective, stationarity = fitted_quantities(data, ridge, row_codes, decoder)
        for row in range(len(data)):
            for column in range(len(data[row])):
                close(
                    float(start["reconstruction"][row][column]),
                    reconstruction[row][column],
                    f"{start_id}.reconstruction[{row},{column}]",
                )
                close(
                    float(start["decoded"][row][column]),
                    reconstruction[row][column],
                    f"{start_id}.decoded[{row},{column}]",
                )
        close(float(start["fullObjective"]), objective, f"{start_id}.objective")
        close(float(start["libraryObjective"]), objective, f"{start_id}.libraryObjective")
        close(float(start["stationarity"]), stationarity, f"{start_id}.stationarity")
        if stationarity > 1e-6:
            raise AssertionError(f"{start_id}: fitted stationarity is too weak: {stationarity}")

        checkpoints = start["objectiveCheckpoints"]
        if [int(point["iteration"]) for point in checkpoints] != EXPECTED_CHECKPOINTS:
            raise AssertionError(f"{start_id}: checkpoint inventory mismatch")
        checkpoint_values = [float(point["fullObjective"]) for point in checkpoints]
        for index in range(1, len(checkpoint_values)):
            before = checkpoint_values[index - 1]
            after = checkpoint_values[index]
            if after > before + ABS_TOL + REL_TOL * abs(before):
                raise AssertionError(f"{start_id}: full objective increased at checkpoint {index}")
        close(checkpoint_values[-1], objective, f"{start_id}.checkpoint.final")

        history = [float(value) for value in start["upstreamReportedHistory"]]
        if len(history) != EXPECTED_CHECKPOINTS[-1] + 1:
            raise AssertionError(f"{start_id}: upstream history length mismatch")
        reported_final = float(start["upstreamReportedFinal"])
        close(history[-1], reported_final, f"{start_id}.reported.final")
        discrepancy = reported_final - objective
        close(
            float(start["upstreamHistoryObjectiveDiscrepancy"]),
            discrepancy,
            f"{start_id}.reported.discrepancy",
        )
        if abs(discrepancy) < 0.5:
            raise AssertionError(f"{start_id}: expected upstream history limitation disappeared")

        if reference_reconstruction is None:
            reference_reconstruction = reconstruction
        else:
            for row in range(len(data)):
                for column in range(len(data[row])):
                    close(
                        reconstruction[row][column],
                        reference_reconstruction[row][column],
                        f"{start_id}.cross-start reconstruction",
                    )

    expected = [[0.92 * value for value in row] for row in data]
    assert reference_reconstruction is not None
    for row in range(len(data)):
        for column in range(len(data[row])):
            if not math.isclose(
                reference_reconstruction[row][column],
                expected[row][column],
                abs_tol=1e-8,
                rel_tol=1e-8,
            ):
                raise AssertionError("fitted reconstruction does not match the analytic rank-one optimum")
    close(float(starts[0]["fullObjective"]), 1.92, "analytic optimum")
    if len(fixture.get("limitations", [])) < 3:
        raise AssertionError("fitted fixture must retain explicit limitations")
    print(f"verified {len(starts)} deterministic LowRankModels.jl fitted starts from {path}")


def main() -> None:
    root = Path(__file__).resolve().parents[3]
    path = (
        Path(sys.argv[1]).resolve()
        if len(sys.argv) > 1
        else root / "modules/multivar/shared/src/test/resources/lowrankmodels/fitted-v1.json"
    )
    verify(path)


if __name__ == "__main__":
    main()
