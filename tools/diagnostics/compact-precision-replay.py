#!/usr/bin/env python3
"""Replay chunked compact-condition precision snapshots, without changing fit policy.

Requires mpmath==1.3.0. JSON numbers are first decoded to binary64, then converted
exactly to MP. All stages retain the captured basis and compact factor/response.
"""
import argparse
import hashlib
import json
import math
from pathlib import Path
import mpmath as mp


def records(path):
    result = []
    for line in path.read_text().splitlines():
        start = line.find('{"kind":"compact-precision-')
        if start >= 0:
            result.append(json.loads(line[start:].strip()))
    return result


def load(path):
    rows = records(path)
    headers = [row for row in rows if row["kind"] == "compact-precision-header"]
    if len(headers) != 1:
        raise ValueError(f"requires one platform/cohort header; got {len(headers)}")
    chunks = {}
    seen = {}
    for row in rows:
        if row["kind"] == "compact-precision-array":
            key = (row["scope"], row["field"])
            values = chunks.setdefault(key, [None] * row["total"])
            occupied = seen.setdefault(key, set())
            if len(values) != row["total"]:
                raise ValueError(f"array shape changed: {key}")
            for offset, value in enumerate(row["values"], row["offset"]):
                if offset in occupied or offset >= len(values) or value is None or not math.isfinite(value):
                    raise ValueError(f"invalid array element {key}[{offset}]")
                values[offset] = mp.mpf(value)
                occupied.add(offset)
    for key, values in chunks.items():
        if any(value is None for value in values):
            raise ValueError(f"incomplete array {key}")
    points = {row["scope"]: row for row in rows if row["kind"] == "compact-precision-point"}
    voxels = [row for row in rows if row["kind"] == "compact-precision-voxel"]
    if len(voxels) != 24 or sorted(row["voxel"] for row in voxels) != list(range(24)):
        raise ValueError("incomplete fixed 24-voxel cohort")
    for row in voxels:
        if row["jets"] > 8 or row["exact"] > 2 or row["steps"] > 6 or row["nodes"] > 90:
            raise ValueError("algorithm counters exceed unchanged caps")
    replay = {row["voxel"]: row for row in rows if row["kind"] == "compact-precision-replay"}
    for row in voxels:
        if row["status"] != "Accepted":
            if row["voxel"] not in replay or not replay[row["voxel"]]["sameResult"]:
                raise ValueError("missing unchanged-result observation replay")
    return headers[0], rows, chunks, points, voxels


def matrix(values, rows, columns):
    if len(values) != rows * columns:
        raise ValueError("matrix shape mismatch")
    return mp.matrix([[values[row * columns + column] for column in range(columns)] for row in range(rows)])


def design_from_coefficients(coefficients, factor, rank, conditions, basis_rank):
    return mp.matrix([[mp.fsum(
        factor[row * conditions * basis_rank + basis * conditions + condition] * coefficients[basis]
        for basis in range(basis_rank)) for condition in range(conditions)] for row in range(rank)])


def project(kernel, phi, basis_rank, fine_count):
    return [mp.fsum(phi[basis * fine_count + lag] * kernel[lag] for lag in range(fine_count))
            for basis in range(basis_rank)]


def fit(design, response, beta=None):
    gram = design.T * design
    rhs = design.T * response
    fitted = mp.lu_solve(gram, rhs) if beta is None else mp.matrix(beta)
    residual = response - design * fitted
    return (residual.T * residual)[0], fitted, residual


def gaussian_kernel(lags, coordinates):
    tau, log_sd = map(mp.mpf, coordinates)
    inv2 = mp.exp(-2 * log_sd)
    return [mp.mpf(0) if lag < 0 else mp.exp(-mp.mpf('0.5') * (lag - tau) ** 2 * inv2) for lag in lags]


def analytic_jet(lags, phi, factor, coordinates, rank, conditions, basis_rank, fine_count):
    tau, log_sd = map(mp.mpf, coordinates)
    inv2 = mp.exp(-2 * log_sd)
    kernels = [[] for _ in range(6)]
    for lag in lags:
        h = mp.mpf(0) if lag < 0 else mp.exp(-mp.mpf('0.5') * (lag - tau) ** 2 * inv2)
        u = lag - tau
        a = u * inv2
        a2 = u * a
        terms = (h, h * a, h * a2, h * (a * a - inv2),
                 h * (a2 * a - 2 * a), h * (a2 * a2 - 2 * a2))
        for index, value in enumerate(terms):
            kernels[index].append(value)
    return [design_from_coefficients(project(kernel, phi, basis_rank, fine_count), factor,
                                    rank, conditions, basis_rank) for kernel in kernels]


def profile_derivatives(jets, response):
    design = jets[0]
    energy, beta, residual = fit(design, response)
    gram = design.T * design
    first = jets[1:3]
    second = ((jets[3], jets[4]), (jets[4], jets[5]))
    correction_rhs = [derivative.T * residual - design.T * derivative * beta for derivative in first]
    gradient = mp.matrix([-2 * (residual.T * derivative * beta)[0] for derivative in first])
    hessian = mp.matrix(2, 2)
    for p in range(2):
        for q in range(2):
            hessian[p, q] = (-2 * (residual.T * second[p][q] * beta)[0]
                              + 2 * (beta.T * first[p].T * first[q] * beta)[0]
                              - 2 * (correction_rhs[p].T * mp.lu_solve(gram, correction_rhs[q]))[0])
    raw = -mp.lu_solve(hessian, gradient)
    return {"gradient": [mp.nstr(value, 50) for value in gradient],
            "hessian": [mp.nstr(hessian[p, q], 50) for p in range(2) for q in range(2)],
            "rawCorrection": [mp.nstr(value, 50) for value in raw],
            "rawCorrectionInfinityNorm": mp.nstr(max(map(abs, raw)), 50)}


def analyze(path):
    header, rows, arrays, points, voxels = load(path)
    rank, conditions, basis_rank, fine_count = (header[key] for key in ("rank", "conditions", "basisRank", "fineCount"))
    lags = arrays[("global", "lags")]
    phi = arrays[("global", "phi")]
    factor = arrays[("global", "rHat")]
    results = []
    for voxel in voxels:
        if voxel["status"] == "Accepted":
            continue
        index = voxel["voxel"]
        response = mp.matrix(arrays[(f"voxel-{index}", "response")])
        stages = {}
        derivatives = {}
        for label in ("terminal", "candidate"):
            scope = f"voxel-{index}-{label}"
            point = points[scope]
            if not point["ok"]:
                raise ValueError("cannot classify a refused full-jet pair as a finite precision problem")
            source_design = matrix(arrays[(scope, "design")], rank, conditions)
            coefficients = arrays[(scope, "coefficients")]
            kernel = arrays[(scope, "kernel")]
            designs = {
                "storedBinaryDesign": source_design,
                "storedBinaryCoefficients": design_from_coefficients(coefficients, factor, rank, conditions, basis_rank),
                "storedBinaryKernel": design_from_coefficients(project(kernel, phi, basis_rank, fine_count), factor,
                                                                 rank, conditions, basis_rank),
                "analyticGaussianKernel": design_from_coefficients(project(gaussian_kernel(lags, point["coordinates"]), phi,
                                                                           basis_rank, fine_count), factor,
                                                                    rank, conditions, basis_rank),
            }
            stages[label] = {name: fit(design, response)[0] for name, design in designs.items()}
            stages[label]["storedBinaryDesignReportedAmplitudes"] = fit(source_design, response,
                                                                       [mp.mpf(value) for value in point["amplitudes"]])[0]
            derivatives[label] = profile_derivatives(analytic_jet(lags, phi, factor, point["coordinates"],
                                                                 rank, conditions, basis_rank, fine_count), response)
        differences = {name: mp.nstr(stages["candidate"][name] - stages["terminal"][name], 60)
                       for name in stages["terminal"]}
        pair = next(row for row in rows if row["kind"] == "compact-precision-pair" and row["voxel"] == index)
        results.append({"voxel": index, "status": voxel["status"], "budgetExit": voxel["budgetExit"],
                        "reportedDifference": pair["reportedDifference"],
                        "reportedUlpDifference": pair["reportedUlpDifference"],
                        "reportedPredictedDrop": pair["predictedDrop"], "pairedResidualDifferencesMP": differences,
                        "analyticGaussianDerivativesMP": derivatives})
    admitted = sum(row["status"] == "Accepted" for row in voxels)
    return {"source": str(path), "source_sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
            "mpmath": mp.__version__, "decimal_digits": mp.mp.dps, "header": header,
            "admitted": admitted, "required_minimum": 22, "cohort_size": 24,
            "scope": "Fixed captured binary64 phi/rHat/response and points; high precision stages isolate evaluation effects. Diagnostic only, no policy qualification.",
            "results": results}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("log", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--digits", type=int, default=100)
    args = parser.parse_args()
    if args.digits < 80:
        parser.error("at least80 decimaldigits required")
    mp.mp.dps = args.digits
    result = analyze(args.log)
    args.output.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n")
    print(json.dumps({"admitted": result["admitted"], "results": result["results"]}, indent=2))


if __name__ == "__main__":
    main()
