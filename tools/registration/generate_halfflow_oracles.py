#!/usr/bin/env python3
"""Generate independent analytic fixtures for the HalfFlow-LM contracts.

The generator intentionally uses only the Python standard library and does not
call ScalaFIM, Gale, ITK, ANTs, NumPy, or SciPy. Affine expectations are
closed-form. Smooth stationary-velocity expectations use a high-resolution RK4
integration of both the trajectory and its variational equation.
"""

from __future__ import annotations

import argparse
import json
import math
from pathlib import Path
from typing import Any, Callable, Iterable


ROOT = Path(__file__).resolve().parents[2]
DEFAULT_OUTPUT = (
    ROOT
    / "modules"
    / "registration"
    / "fixtures"
    / "v1"
    / "half-flow-synthetic.json"
)
SCHEMA = "scalafim-half-flow-synthetic-v1"
RK4_STEPS = 4096
HALF_TIME = 0.5
ROUND_DIGITS = 14

Vector3 = tuple[float, float, float]
Matrix3 = tuple[
    tuple[float, float, float],
    tuple[float, float, float],
    tuple[float, float, float],
]
Matrix4 = tuple[
    tuple[float, float, float, float],
    tuple[float, float, float, float],
    tuple[float, float, float, float],
    tuple[float, float, float, float],
]


def clean_float(value: float) -> float:
    if not math.isfinite(value):
        raise ValueError(f"oracle produced non-finite value: {value}")
    rounded = float(format(value, f".{ROUND_DIGITS}g"))
    return 0.0 if rounded == 0.0 else rounded


def clean_tree(value: Any) -> Any:
    if isinstance(value, float):
        return clean_float(value)
    if isinstance(value, tuple):
        return [clean_tree(item) for item in value]
    if isinstance(value, list):
        return [clean_tree(item) for item in value]
    if isinstance(value, dict):
        return {key: clean_tree(item) for key, item in value.items()}
    return value


def norm(vector: Iterable[float]) -> float:
    return math.sqrt(sum(value * value for value in vector))


def subtract(left: Vector3, right: Vector3) -> Vector3:
    return tuple(left[index] - right[index] for index in range(3))  # type: ignore[return-value]


def determinant3(matrix: Matrix3) -> float:
    a, b, c = matrix[0]
    d, e, f = matrix[1]
    g, h, i = matrix[2]
    return a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)


def inverse3(matrix: Matrix3) -> Matrix3:
    a, b, c = matrix[0]
    d, e, f = matrix[1]
    g, h, i = matrix[2]
    determinant = determinant3(matrix)
    if abs(determinant) <= 1e-14:
        raise ValueError("fixture affine must be nonsingular")
    scale = 1.0 / determinant
    return (
        ((e * i - f * h) * scale, (c * h - b * i) * scale, (b * f - c * e) * scale),
        ((f * g - d * i) * scale, (a * i - c * g) * scale, (c * d - a * f) * scale),
        ((d * h - e * g) * scale, (b * g - a * h) * scale, (a * e - b * d) * scale),
    )


def matrix3_vector(matrix: Matrix3, vector: Vector3) -> Vector3:
    return tuple(
        sum(matrix[row][column] * vector[column] for column in range(3))
        for row in range(3)
    )  # type: ignore[return-value]


def matrix3_multiply(left: Matrix3, right: Matrix3) -> Matrix3:
    return tuple(
        tuple(
            sum(left[row][inner] * right[inner][column] for inner in range(3))
            for column in range(3)
        )
        for row in range(3)
    )  # type: ignore[return-value]


def affine(linear: Matrix3, offset: Vector3) -> Matrix4:
    return (
        (linear[0][0], linear[0][1], linear[0][2], offset[0]),
        (linear[1][0], linear[1][1], linear[1][2], offset[1]),
        (linear[2][0], linear[2][1], linear[2][2], offset[2]),
        (0.0, 0.0, 0.0, 1.0),
    )


def affine_inverse(matrix: Matrix4) -> Matrix4:
    linear: Matrix3 = tuple(tuple(matrix[row][column] for column in range(3)) for row in range(3))  # type: ignore[assignment]
    offset: Vector3 = tuple(matrix[row][3] for row in range(3))  # type: ignore[assignment]
    inverse_linear = inverse3(linear)
    inverse_offset = matrix3_vector(inverse_linear, tuple(-value for value in offset))  # type: ignore[arg-type]
    return affine(inverse_linear, inverse_offset)


def affine_apply(matrix: Matrix4, point: Vector3) -> Vector3:
    return tuple(
        sum(matrix[row][column] * point[column] for column in range(3)) + matrix[row][3]
        for row in range(3)
    )  # type: ignore[return-value]


def affine_multiply(left: Matrix4, right: Matrix4) -> Matrix4:
    return tuple(
        tuple(
            sum(left[row][inner] * right[inner][column] for inner in range(4))
            for column in range(4)
        )
        for row in range(4)
    )  # type: ignore[return-value]


def max_identity_error(matrix: Matrix4) -> float:
    return max(
        abs(matrix[row][column] - (1.0 if row == column else 0.0))
        for row in range(4)
        for column in range(4)
    )


IDENTITY3: Matrix3 = (
    (1.0, 0.0, 0.0),
    (0.0, 1.0, 0.0),
    (0.0, 0.0, 1.0),
)

IDENTITY4 = affine(IDENTITY3, (0.0, 0.0, 0.0))

SAMPLE_POINTS: tuple[Vector3, ...] = (
    (-12.5, -8.0, -4.25),
    (-7.0, 3.5, 9.0),
    (-1.25, -2.75, 6.5),
    (0.0, 0.0, 0.0),
    (2.5, 4.0, -3.0),
    (6.75, -5.5, 2.25),
    (10.0, 7.5, 5.0),
    (14.25, -1.5, 11.0),
)


def velocity(point: Vector3) -> Vector3:
    x, y, z = point
    return (
        0.04 * math.sin(0.09 * x)
        + 0.35 * math.sin(0.13 * y)
        + 0.08 * math.cos(0.17 * z),
        -0.28 * math.sin(0.11 * x)
        - 0.03 * math.cos(0.08 * y)
        + 0.06 * math.cos(0.15 * z),
        0.22 * math.sin(0.10 * x + 0.07 * y) + 0.025 * math.sin(0.12 * z),
    )


def velocity_jacobian(point: Vector3) -> Matrix3:
    x, y, z = point
    phase = 0.10 * x + 0.07 * y
    return (
        (
            0.04 * 0.09 * math.cos(0.09 * x),
            0.35 * 0.13 * math.cos(0.13 * y),
            -0.08 * 0.17 * math.sin(0.17 * z),
        ),
        (
            -0.28 * 0.11 * math.cos(0.11 * x),
            0.03 * 0.08 * math.sin(0.08 * y),
            -0.06 * 0.15 * math.sin(0.15 * z),
        ),
        (
            0.22 * 0.10 * math.cos(phase),
            0.22 * 0.07 * math.cos(phase),
            0.025 * 0.12 * math.cos(0.12 * z),
        ),
    )


def augmented_rhs(state: tuple[float, ...]) -> tuple[float, ...]:
    point: Vector3 = (state[0], state[1], state[2])
    tangent: Matrix3 = tuple(
        tuple(state[3 + row * 3 + column] for column in range(3))
        for row in range(3)
    )  # type: ignore[assignment]
    point_rate = velocity(point)
    tangent_rate = matrix3_multiply(velocity_jacobian(point), tangent)
    return point_rate + tuple(value for row in tangent_rate for value in row)


def combine(
    state: tuple[float, ...],
    scale: float,
    delta: tuple[float, ...],
) -> tuple[float, ...]:
    return tuple(state[index] + scale * delta[index] for index in range(len(state)))


def rk4(
    initial: tuple[float, ...],
    time: float,
    steps: int,
    rhs: Callable[[tuple[float, ...]], tuple[float, ...]],
) -> tuple[float, ...]:
    state = initial
    step = time / steps
    for _ in range(steps):
        k1 = rhs(state)
        k2 = rhs(combine(state, 0.5 * step, k1))
        k3 = rhs(combine(state, 0.5 * step, k2))
        k4 = rhs(combine(state, step, k3))
        state = tuple(
            state[index]
            + (step / 6.0)
            * (k1[index] + 2.0 * k2[index] + 2.0 * k3[index] + k4[index])
            for index in range(len(state))
        )
    return state


def flow(
    point: Vector3,
    time: float,
    steps: int = RK4_STEPS,
) -> tuple[Vector3, Matrix3]:
    initial = point + tuple(value for row in IDENTITY3 for value in row)
    integrated = rk4(initial, time, steps, augmented_rhs)
    output: Vector3 = (integrated[0], integrated[1], integrated[2])
    tangent: Matrix3 = tuple(
        tuple(integrated[3 + row * 3 + column] for column in range(3))
        for row in range(3)
    )  # type: ignore[assignment]
    return output, tangent


def affine_case(case_id: str, forward: Matrix4) -> dict[str, Any]:
    backward = affine_inverse(forward)
    samples = []
    for point in SAMPLE_POINTS:
        mapped = affine_apply(forward, point)
        recovered = affine_apply(backward, mapped)
        samples.append(
            {
                "input": point,
                "forward": mapped,
                "backwardOfForward": recovered,
                "roundTripError": norm(subtract(recovered, point)),
            }
        )
    linear: Matrix3 = tuple(tuple(forward[row][column] for column in range(3)) for row in range(3))  # type: ignore[assignment]
    return {
        "id": case_id,
        "kind": "affine-pull",
        "forward": forward,
        "backward": backward,
        "forwardJacobianDeterminant": determinant3(linear),
        "samples": samples,
    }


def midpoint_case() -> dict[str, Any]:
    fixed = affine(
        (
            (1.01, -0.03, 0.02),
            (0.04, 0.98, -0.01),
            (0.00, 0.02, 1.03),
        ),
        (1.5, -0.75, 0.5),
    )
    moving = affine(
        (
            (0.97, 0.05, -0.01),
            (-0.02, 1.02, 0.03),
            (0.01, -0.04, 0.99),
        ),
        (-2.0, 1.25, -0.25),
    )
    fixed_to_moving = affine_multiply(moving, affine_inverse(fixed))
    moving_to_fixed = affine_inverse(fixed_to_moving)
    samples = []
    for point in SAMPLE_POINTS:
        mapped = affine_apply(fixed_to_moving, point)
        recovered = affine_apply(moving_to_fixed, mapped)
        samples.append(
            {
                "fixed": point,
                "moving": mapped,
                "fixedRecovered": recovered,
                "roundTripError": norm(subtract(recovered, point)),
            }
        )
    return {
        "id": "affine-midpoint-composition",
        "kind": "midpoint-pull-composition",
        "fixedWorkToSource": fixed,
        "movingWorkToSource": moving,
        "fixedToMoving": fixed_to_moving,
        "movingToFixed": moving_to_fixed,
        "swapExpected": moving_to_fixed,
        "samples": samples,
    }


def constant_flow_case() -> dict[str, Any]:
    field_velocity: Vector3 = (1.5, -2.0, 0.75)
    half_delta: Vector3 = tuple(HALF_TIME * value for value in field_velocity)  # type: ignore[assignment]
    samples = []
    for point in SAMPLE_POINTS:
        plus: Vector3 = tuple(point[index] + half_delta[index] for index in range(3))  # type: ignore[assignment]
        minus: Vector3 = tuple(point[index] - half_delta[index] for index in range(3))  # type: ignore[assignment]
        minus_of_plus: Vector3 = tuple(plus[index] - half_delta[index] for index in range(3))  # type: ignore[assignment]
        plus_of_minus: Vector3 = tuple(minus[index] + half_delta[index] for index in range(3))  # type: ignore[assignment]
        samples.append(
            {
                "input": point,
                "plusHalf": plus,
                "minusHalf": minus,
                "minusOfPlus": minus_of_plus,
                "plusOfMinus": plus_of_minus,
                "plusHalfJacobianDeterminant": 1.0,
                "minusHalfJacobianDeterminant": 1.0,
            }
        )
    return {
        "id": "constant-velocity-half-flow",
        "kind": "stationary-velocity-flow",
        "time": HALF_TIME,
        "velocity": field_velocity,
        "samples": samples,
    }


def smooth_flow_case() -> dict[str, Any]:
    samples = []
    for point in SAMPLE_POINTS:
        plus, plus_tangent = flow(point, HALF_TIME)
        minus, minus_tangent = flow(point, -HALF_TIME)
        plus_then_minus, _ = flow(plus, -HALF_TIME)
        minus_then_plus, _ = flow(minus, HALF_TIME)
        samples.append(
            {
                "input": point,
                "velocity": velocity(point),
                "plusHalf": plus,
                "minusHalf": minus,
                "plusHalfJacobianDeterminant": determinant3(plus_tangent),
                "minusHalfJacobianDeterminant": determinant3(minus_tangent),
                "minusOfPlus": plus_then_minus,
                "plusOfMinus": minus_then_plus,
                "minusOfPlusError": norm(subtract(plus_then_minus, point)),
                "plusOfMinusError": norm(subtract(minus_then_plus, point)),
            }
        )
    return {
        "id": "smooth-sinusoidal-svf-half-flow",
        "kind": "stationary-velocity-flow",
        "time": HALF_TIME,
        "integration": {
            "method": "classical-rk4-trajectory-plus-variational-equation",
            "steps": RK4_STEPS,
        },
        "velocity": {
            "units": "millimetres-per-unit-time",
            "components": [
                "0.04*sin(0.09*x) + 0.35*sin(0.13*y) + 0.08*cos(0.17*z)",
                "-0.28*sin(0.11*x) - 0.03*cos(0.08*y) + 0.06*cos(0.15*z)",
                "0.22*sin(0.10*x + 0.07*y) + 0.025*sin(0.12*z)",
            ],
        },
        "samples": samples,
    }


def trust_cases() -> dict[str, Any]:
    eta_accept = 0.10
    low_gain = 0.25
    high_gain = 0.75
    initial_damping = 0.01
    raw_cases = (
        ("high-gain-accept", 12.0, 10.0, 2.5),
        ("middle-gain-accept", 12.0, 11.0, 2.0),
        ("low-gain-accept-and-tighten", 12.0, 11.7, 2.0),
        ("below-threshold-reject", 12.0, 11.9, 2.0),
        ("objective-increase-reject", 12.0, 12.2, 1.0),
        ("nonpositive-prediction-reject", 12.0, 11.0, 0.0),
    )
    cases = []
    for case_id, old_value, candidate_value, predicted_drop in raw_cases:
        actual_drop = old_value - candidate_value
        ratio = actual_drop / predicted_drop if predicted_drop > 0.0 else None
        accepted = (
            actual_drop > 0.0
            and predicted_drop > 0.0
            and ratio is not None
            and ratio >= eta_accept
        )
        if ratio is None or ratio < low_gain:
            next_damping = initial_damping * 4.0
        elif ratio > high_gain:
            next_damping = initial_damping * 0.5
        else:
            next_damping = initial_damping
        cases.append(
            {
                "id": case_id,
                "oldValue": old_value,
                "candidateValue": candidate_value,
                "actualDrop": actual_drop,
                "predictedDrop": predicted_drop,
                "gainRatio": ratio,
                "accepted": accepted,
                "nextDamping": next_damping,
            }
        )
    return {
        "etaAccept": eta_accept,
        "lowGain": low_gain,
        "highGain": high_gain,
        "initialDamping": initial_damping,
        "cases": cases,
        "acceptedStepSequence": {
            "acceptedByAttempt": [False, True, False, True, True],
            "acceptedCountAfterAttempt": [0, 1, 1, 2, 3],
            "attemptCountAfterAttempt": [1, 2, 3, 4, 5],
        },
    }


def build_fixture() -> dict[str, Any]:
    theta = 0.31
    cosine = math.cos(theta)
    sine = math.sin(theta)
    oblique_linear: Matrix3 = (
        (1.10 * cosine, -0.92 * sine + 0.08, 0.05),
        (1.10 * sine, 0.92 * cosine, -0.04),
        (0.02, 0.06, 1.17),
    )
    return clean_tree(
        {
            "schema": SCHEMA,
            "generator": {
                "path": "tools/registration/generate_halfflow_oracles.py",
                "dependencies": "python-standard-library-only",
                "floatRoundingSignificantDigits": ROUND_DIGITS,
            },
            "coordinateContract": {
                "points": "3D physical coordinates in millimetres",
                "matrixConvention": "row-major 4x4 matrix applied to a homogeneous column point",
                "mapConvention": "pull map from target/work coordinates to source coordinates",
                "composition": "A >>> B means B(A(x)); matrix is B*A",
                "halfFlow": "plus=exp(+0.5*v), minus=exp(-0.5*v)",
            },
            "recommendedOracleTolerances": {
                "affineAbsolute": 1e-12,
                "rk4PointAbsoluteMm": 2e-11,
                "rk4JacobianDeterminantAbsolute": 2e-11,
                "rk4InverseCompositionAbsoluteMm": 5e-11,
            },
            "affineCases": [
                affine_case("identity", IDENTITY4),
                affine_case("translation", affine(IDENTITY3, (2.25, -3.5, 1.125))),
                affine_case("oblique-affine", affine(oblique_linear, (4.5, -2.25, 7.0))),
            ],
            "midpointCase": midpoint_case(),
            "constantFlowCase": constant_flow_case(),
            "smoothFlowCase": smooth_flow_case(),
            "trustModel": trust_cases(),
        }
    )


def validate_fixture(fixture: dict[str, Any]) -> None:
    if fixture["schema"] != SCHEMA:
        raise ValueError("unexpected fixture schema")
    for case in fixture["affineCases"]:
        forward = tuple(tuple(row) for row in case["forward"])
        backward = tuple(tuple(row) for row in case["backward"])
        if max_identity_error(affine_multiply(backward, forward)) > 2e-12:
            raise ValueError(f"affine inverse failed for {case['id']}")
        if max(sample["roundTripError"] for sample in case["samples"]) > 2e-12:
            raise ValueError(f"affine round trip failed for {case['id']}")

    midpoint = fixture["midpointCase"]
    if max(sample["roundTripError"] for sample in midpoint["samples"]) > 2e-12:
        raise ValueError("midpoint affine round trip failed")

    constant_flow = fixture["constantFlowCase"]
    for sample in constant_flow["samples"]:
        if norm(subtract(tuple(sample["minusOfPlus"]), tuple(sample["input"]))) > 2e-12:
            raise ValueError("constant half-flow minus-of-plus failed")
        if norm(subtract(tuple(sample["plusOfMinus"]), tuple(sample["input"]))) > 2e-12:
            raise ValueError("constant half-flow plus-of-minus failed")

    flow_case = fixture["smoothFlowCase"]
    for sample in flow_case["samples"]:
        if sample["plusHalfJacobianDeterminant"] <= 0.0:
            raise ValueError("plus half-flow folded")
        if sample["minusHalfJacobianDeterminant"] <= 0.0:
            raise ValueError("minus half-flow folded")
        if sample["minusOfPlusError"] > 5e-11 or sample["plusOfMinusError"] > 5e-11:
            raise ValueError("RK4 half-flow inverse composition exceeded oracle budget")

    trust = fixture["trustModel"]
    accepted = trust["acceptedStepSequence"]["acceptedByAttempt"]
    expected_counts = []
    count = 0
    for decision in accepted:
        count += 1 if decision else 0
        expected_counts.append(count)
    if expected_counts != trust["acceptedStepSequence"]["acceptedCountAfterAttempt"]:
        raise ValueError("accepted-step accounting fixture is inconsistent")


def validate_generator_math() -> None:
    derivative_step = 1e-6
    for point in SAMPLE_POINTS:
        analytic = velocity_jacobian(point)
        for column in range(3):
            plus_point = list(point)
            minus_point = list(point)
            plus_point[column] += derivative_step
            minus_point[column] -= derivative_step
            plus_value = velocity(tuple(plus_point))  # type: ignore[arg-type]
            minus_value = velocity(tuple(minus_point))  # type: ignore[arg-type]
            for row in range(3):
                finite_difference = (
                    plus_value[row] - minus_value[row]
                ) / (2.0 * derivative_step)
                if abs(finite_difference - analytic[row][column]) > 2e-10:
                    raise ValueError("analytic velocity Jacobian failed finite-difference check")

    for point in SAMPLE_POINTS[:2]:
        fine_point, fine_tangent = flow(point, HALF_TIME)
        coarse_point, coarse_tangent = flow(point, HALF_TIME, RK4_STEPS // 2)
        if norm(subtract(fine_point, coarse_point)) > 2e-11:
            raise ValueError("RK4 point integration did not converge at the fixture budget")
        tangent_error = max(
            abs(fine_tangent[row][column] - coarse_tangent[row][column])
            for row in range(3)
            for column in range(3)
        )
        if tangent_error > 2e-11:
            raise ValueError("RK4 variational integration did not converge at the fixture budget")

    flow_difference_step = 1e-4
    point = SAMPLE_POINTS[2]
    _, analytic_tangent = flow(point, HALF_TIME)
    for column in range(3):
        plus_point = list(point)
        minus_point = list(point)
        plus_point[column] += flow_difference_step
        minus_point[column] -= flow_difference_step
        plus_flow, _ = flow(tuple(plus_point), HALF_TIME)  # type: ignore[arg-type]
        minus_flow, _ = flow(tuple(minus_point), HALF_TIME)  # type: ignore[arg-type]
        for row in range(3):
            finite_difference = (
                plus_flow[row] - minus_flow[row]
            ) / (2.0 * flow_difference_step)
            if abs(finite_difference - analytic_tangent[row][column]) > 2e-9:
                raise ValueError("variational flow Jacobian failed finite-difference check")


def render_fixture(fixture: dict[str, Any]) -> str:
    return json.dumps(fixture, indent=2, sort_keys=True, allow_nan=False) + "\n"


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument(
        "--check",
        action="store_true",
        help="fail if the checked-in fixture differs from a fresh generation",
    )
    args = parser.parse_args()

    fixture = build_fixture()
    validate_generator_math()
    validate_fixture(fixture)
    rendered = render_fixture(fixture)
    output = args.output if args.output.is_absolute() else ROOT / args.output

    if args.check:
        if not output.exists():
            raise SystemExit(f"missing fixture: {output.relative_to(ROOT)}")
        actual = output.read_text(encoding="utf-8")
        if actual != rendered:
            raise SystemExit(
                f"fixture is stale: run {Path(__file__).relative_to(ROOT)}"
            )
        print(f"fixture current: {output.relative_to(ROOT)}")
    else:
        output.parent.mkdir(parents=True, exist_ok=True)
        output.write_text(rendered, encoding="utf-8")
        print(f"wrote {output.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
