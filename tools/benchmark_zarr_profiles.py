#!/usr/bin/env python3
"""Measure candidate canonical-BOLD Zarr layouts and chunk-local codecs.

This is an evidence tool, not a runtime dependency.  It uses the production
array shape for request-amplification estimates and deterministic synthetic
int16 BOLD chunks for codec throughput/ratio measurements.
"""

from __future__ import annotations

import argparse
import json
import math
import platform
import statistics
import time
from dataclasses import asdict, dataclass
from typing import Iterable

import numcodecs
import numpy as np


ARRAY_SHAPE = (1200, 72, 96, 96)  # [t,z,y,x]
DTYPE_BYTES = 2


@dataclass(frozen=True)
class Profile:
    name: str
    chunk: tuple[int, int, int, int]
    shard: tuple[int, int, int, int]


@dataclass(frozen=True)
class Request:
    name: str
    origin: tuple[int, int, int, int]
    extent: tuple[int, int, int, int]
    weight: float


PROFILES = (
    Profile("canonical-balanced-0.1", (16, 24, 32, 32), (64, 72, 96, 96)),
    Profile("volume-oriented", (4, 24, 32, 32), (32, 72, 96, 96)),
    Profile("temporal-oriented", (64, 8, 16, 16), (256, 32, 64, 64)),
)

REQUESTS = (
    Request("single-volume", (400, 0, 0, 0), (1, 72, 96, 96), 0.30),
    Request("movie-32", (400, 0, 0, 0), (32, 72, 96, 96), 0.25),
    Request("roi-window", (400, 24, 32, 32), (32, 24, 32, 32), 0.35),
    Request("voxel-timeseries-proxy", (0, 36, 48, 48), (1200, 1, 1, 1), 0.10),
)


def product(values: Iterable[int]) -> int:
    return math.prod(values)


def touched(origin: tuple[int, ...], extent: tuple[int, ...], block: tuple[int, ...]) -> int:
    total = 1
    for start, length, size in zip(origin, extent, block):
        first = start // size
        last = (start + length - 1) // size
        total *= last - first + 1
    return total


def layout_measurements(profile: Profile) -> dict[str, object]:
    chunks_per_shard = tuple(s // c for s, c in zip(profile.shard, profile.chunk))
    index_bytes = product(chunks_per_shard) * 16 + 4
    chunk_bytes = product(profile.chunk) * DTYPE_BYTES
    rows = []
    weighted_amplification = 0.0
    weighted_requests = 0.0
    for request in REQUESTS:
        chunks = touched(request.origin, request.extent, profile.chunk)
        shards = touched(request.origin, request.extent, profile.shard)
        logical_bytes = product(request.extent) * DTYPE_BYTES
        planned_bytes = chunks * chunk_bytes + shards * index_bytes
        amplification = planned_bytes / logical_bytes
        requests = chunks + shards
        weighted_amplification += request.weight * amplification
        weighted_requests += request.weight * requests
        rows.append(
            {
                "request": request.name,
                "weight": request.weight,
                "logical_bytes": logical_bytes,
                "inner_chunks": chunks,
                "shards": shards,
                "planned_uncompressed_bytes": planned_bytes,
                "read_amplification": amplification,
            }
        )
    return {
        "profile": asdict(profile),
        "chunk_uncompressed_bytes": chunk_bytes,
        "shard_uncompressed_bytes": product(profile.shard) * DTYPE_BYTES,
        "index_bytes_per_shard": index_bytes,
        "weighted_read_amplification": weighted_amplification,
        "weighted_requests": weighted_requests,
        "requests": rows,
    }


def synthetic_chunks(shape: tuple[int, int, int, int], count: int) -> list[np.ndarray]:
    rng = np.random.default_rng(20260720)
    t, z, y, x = shape
    chunks = []
    for chunk_index in range(count):
        spatial = rng.normal(1000.0, 220.0, size=(z, y, x)).astype(np.float32)
        drift = np.linspace(-18.0, 18.0, t, dtype=np.float32)[:, None, None, None]
        noise = rng.normal(0.0, 20.0, size=shape).astype(np.float32)
        pulse = np.float32(8.0 * math.sin(chunk_index * 0.7))
        values = spatial[None, :, :, :] + drift + noise + pulse
        chunks.append(np.rint(values).clip(-32768, 32767).astype("<i2"))
    return chunks


def timed(operation, repeats: int) -> tuple[object, float]:
    durations = []
    result = None
    for _ in range(repeats):
        started = time.perf_counter()
        result = operation()
        durations.append(time.perf_counter() - started)
    return result, statistics.median(durations)


def codec_measurements(profile: Profile, chunk_count: int, repeats: int) -> list[dict[str, object]]:
    chunks = synthetic_chunks(profile.chunk, chunk_count)
    raw_bytes = sum(chunk.nbytes for chunk in chunks)
    codecs = {
        "gzip-1": numcodecs.GZip(level=1),
        "blosc-zstd-3-shuffle": numcodecs.Blosc(
            cname="zstd", clevel=3, shuffle=numcodecs.Blosc.SHUFFLE
        ),
        "blosc-zstd-3-bitshuffle": numcodecs.Blosc(
            cname="zstd", clevel=3, shuffle=numcodecs.Blosc.BITSHUFFLE
        ),
    }
    rows = []
    for name, codec in codecs.items():
        encoded, encode_seconds = timed(
            lambda: [codec.encode(chunk) for chunk in chunks], repeats
        )
        encoded_bytes = sum(len(value) for value in encoded)
        _, decode_seconds = timed(
            lambda: [codec.decode(value) for value in encoded], repeats
        )
        rows.append(
            {
                "profile": profile.name,
                "codec": name,
                "raw_bytes": raw_bytes,
                "encoded_bytes": encoded_bytes,
                "compression_ratio": raw_bytes / encoded_bytes,
                "encode_mib_per_second": raw_bytes / (1024 * 1024) / encode_seconds,
                "decode_mib_per_second": raw_bytes / (1024 * 1024) / decode_seconds,
            }
        )
    return rows


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--chunks", type=int, default=8)
    parser.add_argument("--repeats", type=int, default=3)
    args = parser.parse_args()
    if args.chunks <= 0 or args.repeats <= 0:
        parser.error("--chunks and --repeats must be positive")

    result = {
        "schema": "scalafim-zarr-profile-benchmark-1",
        "environment": {
            "python": platform.python_version(),
            "platform": platform.platform(),
            "numpy": np.__version__,
            "numcodecs": numcodecs.__version__,
            "dtype": "int16",
            "array_shape_tzyx": ARRAY_SHAPE,
            "chunk_samples_per_profile": args.chunks,
            "timing_repeats": args.repeats,
        },
        "workload": [asdict(request) for request in REQUESTS],
        "layouts": [layout_measurements(profile) for profile in PROFILES],
        "codecs": [
            row
            for profile in PROFILES
            for row in codec_measurements(profile, args.chunks, args.repeats)
        ],
    }
    print(json.dumps(result, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
