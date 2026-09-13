#!/usr/bin/env python3
"""Generate the frozen native-searchlight parity fixture.

The volume oracle exercises PyMVPA's own ``Sphere``, ``IndexQueryEngine``, and
``sphere_searchlight`` implementations.  A separate direct-distance oracle is
used only to cross-check those native results.  Surface geodesics are frozen
with an independent Dijkstra implementation because PyMVPA's voxel sphere is
not a surface-distance oracle.
"""

from __future__ import annotations

import argparse
import hashlib
import heapq
import json
import math
import subprocess
import sys
from pathlib import Path

import h5py
import numpy as np


EXPECTED_PYMVPA_COMMIT = "f699189b5b7e7a1bcaaf6f0a19aa077d8879b422"
EXPECTED_PYMVPA_VERSION = "2.6.5.dev1"
SCHEMA = "scalafim.mvpa.searchlight-pymvpa.v1"


def repository_root() -> Path:
    return Path(__file__).resolve().parents[2]


def linear_ordinal(dims: tuple[int, int, int], coordinate: tuple[int, int, int]) -> int:
    x, y, z = coordinate
    return (x * dims[1] + y) * dims[2] + z


def voxel_coordinates(dims: tuple[int, int, int]) -> list[tuple[int, int, int]]:
    return [
        (x, y, z)
        for x in range(dims[0])
        for y in range(dims[1])
        for z in range(dims[2])
    ]


def apply_affine(
    affine: np.ndarray, coordinate: tuple[int, int, int]
) -> tuple[int, int, int]:
    homogeneous = np.asarray((*coordinate, 1), dtype=np.int64)
    result = affine @ homogeneous
    if not np.array_equal(result, result.astype(np.int64)):
        raise AssertionError("PyMVPA fixture requires integer-valued world coordinates")
    return tuple(int(value) for value in result[:3])


def pattern_value(sample: int, feature: int, target: int) -> float:
    base = (
        (sample + 1) * 13
        + (feature + 1) * 7
        + ((sample + 1) * (feature + 3)) % 11
    ) / 20.0
    interaction = target * ((feature % 7) - 3) * 0.35
    return base + interaction


def pattern_rows(samples: int, features: int, targets: list[int]) -> list[list[float]]:
    return [
        [pattern_value(sample, feature, targets[sample]) for feature in range(features)]
        for sample in range(samples)
    ]


def mean_contrast(patterns: np.ndarray, targets: np.ndarray) -> float:
    return float(np.mean(patterns[targets == 1]) - np.mean(patterns[targets == 0]))


def native_pymvpa_volume(
    support_ordinals: list[int],
    center_ordinals: list[int],
    all_voxels: list[tuple[int, int, int]],
    all_world: list[tuple[int, int, int]],
    radius: float,
    patterns: np.ndarray,
    targets: np.ndarray,
) -> tuple[dict[int, list[int]], dict[int, list[int]], list[float], str, dict[str, str]]:
    # PyMVPA imports h5py.highlevel, which was public in its supported h5py
    # generation and is now folded into the top-level module.  This alias is an
    # import-only compatibility shim; it does not replace searchlight logic.
    sys.modules.setdefault("h5py.highlevel", h5py)

    import mvpa2
    from mvpa2.datasets.base import Dataset
    from mvpa2.measures.searchlight import sphere_searchlight
    from mvpa2.misc.neighborhood import IndexQueryEngine, Sphere

    if mvpa2.__version__ != EXPECTED_PYMVPA_VERSION:
        raise RuntimeError(
            f"expected PyMVPA {EXPECTED_PYMVPA_VERSION}, found {mvpa2.__version__}"
        )

    support_world = np.asarray([all_world[index] for index in support_ordinals], dtype=np.int64)
    support_voxels = np.asarray([all_voxels[index] for index in support_ordinals], dtype=np.int64)
    compact_patterns = patterns[:, support_ordinals]
    dataset = Dataset(
        compact_patterns,
        sa={"targets": targets},
        fa={
            "world_coordinates": support_world,
            "voxel_indices": support_voxels,
            "voxel_ordinal": np.asarray(support_ordinals, dtype=np.int64),
        },
    )
    compact_centers = [support_ordinals.index(center) for center in center_ordinals]

    physical_engine = IndexQueryEngine(world_coordinates=Sphere(radius))
    physical_engine.train(dataset)
    neighborhoods = {
        center: [support_ordinals[index] for index in physical_engine.query_byid(compact)]
        for center, compact in zip(center_ordinals, compact_centers)
    }

    index_engine = IndexQueryEngine(voxel_indices=Sphere(radius))
    index_engine.train(dataset)
    index_neighborhoods = {
        center: [support_ordinals[index] for index in index_engine.query_byid(compact)]
        for center, compact in zip(center_ordinals, compact_centers)
    }

    def local_measure(roi):
        value = mean_contrast(
            np.asarray(roi.samples, dtype=np.float64),
            np.asarray(roi.sa.targets, dtype=np.int64),
        )
        return Dataset([[value]])

    searchlight = sphere_searchlight(
        local_measure,
        radius=radius,
        center_ids=compact_centers,
        space="world_coordinates",
    )
    result = searchlight(dataset)
    observed_centers = [
        support_ordinals[int(index)] for index in np.asarray(result.fa.center_ids)
    ]
    if observed_centers != center_ordinals:
        raise AssertionError(
            f"PyMVPA changed center order: {observed_centers} != {center_ordinals}"
        )
    values = [float(value) for value in np.asarray(result.samples)[0]]

    duplicate_dataset = Dataset(
        np.asarray([[1.0, 2.0], [3.0, 4.0]]),
        fa={"world_coordinates": np.asarray([[0, 0, 0], [0, 0, 0]])},
    )
    try:
        IndexQueryEngine(world_coordinates=Sphere(radius)).train(duplicate_dataset)
    except ValueError as error:
        duplicate_error = {"type": type(error).__name__, "message": str(error)}
    else:
        raise AssertionError("PyMVPA accepted colliding physical coordinates")

    return (
        neighborhoods,
        index_neighborhoods,
        values,
        mvpa2.__version__,
        duplicate_error,
    )


def direct_volume_neighborhoods(
    support_ordinals: list[int],
    center_ordinals: list[int],
    world: list[tuple[int, int, int]],
    radius_squared: float,
) -> dict[int, list[int]]:
    result: dict[int, list[int]] = {}
    for center in center_ordinals:
        center_world = world[center]
        result[center] = [
            candidate
            for candidate in support_ordinals
            if sum(
                (world[candidate][axis] - center_world[axis]) ** 2
                for axis in range(3)
            )
            <= radius_squared
        ]
    return result


def mesh_adjacency(
    vertices: list[tuple[float, float, float]], faces: list[tuple[int, int, int]]
) -> list[dict[int, float]]:
    adjacency: list[dict[int, float]] = [dict() for _ in vertices]
    for face in faces:
        for left, right in ((face[0], face[1]), (face[1], face[2]), (face[2], face[0])):
            weight = math.dist(vertices[left], vertices[right])
            previous = adjacency[left].get(right)
            if previous is None or weight < previous:
                adjacency[left][right] = weight
                adjacency[right][left] = weight
    return adjacency


def dijkstra(adjacency: list[dict[int, float]], source: int) -> list[float]:
    distances = [math.inf] * len(adjacency)
    distances[source] = 0.0
    queue = [(0.0, source)]
    while queue:
        distance, vertex = heapq.heappop(queue)
        if distance != distances[vertex]:
            continue
        for neighbor, weight in adjacency[vertex].items():
            candidate = distance + weight
            if candidate < distances[neighbor]:
                distances[neighbor] = candidate
                heapq.heappush(queue, (candidate, neighbor))
    return distances


def surface_fixture(targets: list[int]) -> dict[str, object]:
    vertices = [
        (0.0, 0.0, 0.0),
        (1.0, 0.0, 0.0),
        (2.0, 0.0, 0.0),
        (0.1, 0.0, 0.1),
        (0.0, 1.0, 0.0),
        (1.0, 1.0, 0.0),
        (2.0, 1.0, 0.0),
        (0.1, 1.0, 0.1),
    ]
    faces = [
        (0, 1, 4),
        (1, 5, 4),
        (1, 2, 5),
        (2, 6, 5),
        (2, 3, 6),
        (3, 7, 6),
    ]
    centers = [0, 2, 3, 7]
    radius = 1.0
    adjacency = mesh_adjacency(vertices, faces)
    distances = {center: dijkstra(adjacency, center) for center in centers}
    neighborhoods = {
        center: [
            vertex
            for vertex, distance in enumerate(distances[center])
            if distance <= radius
        ]
        for center in centers
    }
    chord_neighborhoods = {
        center: [
            vertex
            for vertex in range(len(vertices))
            if math.dist(vertices[center], vertices[vertex]) <= radius
        ]
        for center in centers
    }
    patterns = np.asarray(pattern_rows(len(targets), len(vertices), targets), dtype=np.float64)
    local_values = [
        mean_contrast(patterns[:, neighborhoods[center]], np.asarray(targets))
        for center in centers
    ]
    if neighborhoods[0] == chord_neighborhoods[0] or 3 not in chord_neighborhoods[0]:
        raise AssertionError("folded-surface counterfactual no longer distinguishes chord distance")
    return {
        "metric": "shortest path over unique triangle edges weighted by 3D Euclidean edge length",
        "radius": radius,
        "boundary": "closed (distance <= radius)",
        "vertices": vertices,
        "faces": faces,
        "center_ordinals": centers,
        "patterns": patterns.tolist(),
        "expected": {
            "geodesic_neighborhoods": [
                {"center": center, "members": neighborhoods[center]} for center in centers
            ],
            "local_mean_contrast": local_values,
        },
        "euclidean_chord_counterfactual": {
            "not_an_admissible_surface_searchlight": True,
            "neighborhoods": [
                {"center": center, "members": chord_neighborhoods[center]}
                for center in centers
            ],
        },
    }


def git_revision(source: Path) -> str:
    completed = subprocess.run(
        ["git", "-C", str(source), "rev-parse", "HEAD"],
        check=True,
        capture_output=True,
        text=True,
    )
    return completed.stdout.strip()


def build_fixture(pymvpa_source: Path) -> dict[str, object]:
    revision = git_revision(pymvpa_source)
    if revision != EXPECTED_PYMVPA_COMMIT:
        raise RuntimeError(
            f"expected PyMVPA commit {EXPECTED_PYMVPA_COMMIT}, found {revision}"
        )

    dims = (4, 3, 2)
    affine = np.asarray(
        [
            [2, 1, 0, 10],
            [0, 3, 1, -7],
            [0, 0, 4, 5],
            [0, 0, 0, 1],
        ],
        dtype=np.int64,
    )
    voxels = voxel_coordinates(dims)
    if [linear_ordinal(dims, coordinate) for coordinate in voxels] != list(range(24)):
        raise AssertionError("fixture voxel order is not ScalaFIM's x-major domain order")
    world = [apply_affine(affine, coordinate) for coordinate in voxels]
    support = [0, 1, 2, 3, 4, 5, 7, 8, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20, 22, 23]
    centers = [0, 5, 8, 13, 18, 23]
    radius_squared = 10
    radius = math.sqrt(radius_squared)
    targets = [0, 0, 0, 1, 1, 1]
    patterns = np.asarray(pattern_rows(len(targets), math.prod(dims), targets), dtype=np.float64)

    (
        native,
        index_counterfactual,
        native_values,
        pymvpa_version,
        duplicate_coordinate_error,
    ) = native_pymvpa_volume(
        support,
        centers,
        voxels,
        world,
        radius,
        patterns,
        np.asarray(targets, dtype=np.int64),
    )
    direct = direct_volume_neighborhoods(support, centers, world, radius_squared)
    if native != direct:
        raise AssertionError(f"PyMVPA/direct physical neighborhood mismatch: {native} != {direct}")
    direct_values = [
        mean_contrast(patterns[:, direct[center]], np.asarray(targets))
        for center in centers
    ]
    if not np.allclose(native_values, direct_values, rtol=0.0, atol=1e-14):
        raise AssertionError(
            f"PyMVPA/direct local-analysis mismatch: {native_values} != {direct_values}"
        )
    if native == index_counterfactual:
        raise AssertionError("voxel-index counterfactual unexpectedly matches physical neighborhoods")

    fixture = {
        "schema": SCHEMA,
        "source": {
            "implementation": "PyMVPA native Sphere + IndexQueryEngine + sphere_searchlight",
            "repository": "https://github.com/PyMVPA/PyMVPA",
            "repository_revision": revision,
            "installed_version": pymvpa_version,
            "install_mode": "python setup.py --no-libsvm install",
            "generator": "tools/mvpa/generate_pymvpa_searchlight_reference.py",
            "oracle_independence": "PyMVPA native output is checked against direct affine distance; surface uses standalone Dijkstra and never calls ScalaFIM",
            "zero_based_indices": True,
        },
        "environment": {
            "python": ".".join(str(part) for part in sys.version_info[:3]),
            "numpy": np.__version__,
            "h5py": h5py.__version__,
            "compatibility_note": "h5py.highlevel is aliased to h5py for import compatibility only",
        },
        "tolerances": {
            "absolute": 1e-12,
            "relative": 1e-12,
            "rationale": "finite means over exactly represented fixture values; disagreement should be roundoff-scale",
        },
        "response": {
            "targets": targets,
            "class_zero": 0,
            "class_one": 1,
        },
        "local_analysis": {
            "estimand": "mean of every class-one sample-feature cell minus mean of every class-zero sample-feature cell within each neighborhood",
            "minimum_features": 1,
        },
        "volume": {
            "dims": dims,
            "ordinal_order": "(x * dims[1] + y) * dims[2] + z",
            "affine_row_major": affine.reshape(-1).tolist(),
            "coordinate_frame": "integer-valued physical millimeters after the complete affine",
            "radius_mm": radius,
            "radius_squared_mm": radius_squared,
            "boundary": "closed (squared distance <= squared radius)",
            "support_ordinals": support,
            "center_ordinals": centers,
            "voxel_coordinates": voxels,
            "world_coordinates": world,
            "patterns": patterns.tolist(),
            "expected": {
                "physical_neighborhoods": [
                    {"center": center, "members": native[center]} for center in centers
                ],
                "local_mean_contrast": native_values,
            },
            "voxel_index_counterfactual": {
                "not_the_claimed_metric": True,
                "radius_in_index_units": radius,
                "neighborhoods": [
                    {"center": center, "members": index_counterfactual[center]}
                    for center in centers
                ],
            },
        },
        "surface": surface_fixture(targets),
        "failure_policy": {
            "duplicate_physical_coordinates": {
                "behavior": "PyMVPA IndexQueryEngine rejects colliding feature coordinates",
                "observed": duplicate_coordinate_error,
            },
            "invalid_radius": "ScalaFIM typed construction failure",
            "local_analysis_failure": "ScalaFIM records one RoiOutcome.Failure and continues remaining centers",
        },
    }
    return fixture


def canonical_bytes(fixture: dict[str, object]) -> bytes:
    return (json.dumps(fixture, indent=2, sort_keys=True, allow_nan=False) + "\n").encode()


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("output", nargs="?", type=Path)
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--pymvpa-source", type=Path, required=True)
    args = parser.parse_args()
    if args.check == (args.output is not None):
        parser.error("provide exactly one of OUTPUT or --check")

    payload = canonical_bytes(build_fixture(args.pymvpa_source))
    canonical = repository_root() / "docs/scenarios/fixtures/mvpa.searchlight-pymvpa.v1.json"
    if args.check:
        observed = canonical.read_bytes()
        if observed != payload:
            raise SystemExit("generated fixture differs from the checked-in receipt")
        print(
            "PyMVPA searchlight fixture is current: "
            + hashlib.sha256(payload).hexdigest()
        )
    else:
        assert args.output is not None
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_bytes(payload)


if __name__ == "__main__":
    main()
