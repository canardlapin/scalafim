#!/usr/bin/env python3
"""Capture or verify the pre-kernel HalfFlow-LM structural baseline receipt."""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
import platform
import re
import shutil
import subprocess
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[2]
HODGEFLOW_ROOT = Path("/Users/bbuchsbaum/code/hodgeflow")
DOCKER_CONTEXT = "desktop-linux"
DOCKER_SOCKET = Path("/Users/bbuchsbaum/.docker/run/docker.sock")
ANTS_DOCKER_IMAGE = "antsx/ants:latest"
ANTS_DOCKER_PLATFORM = "linux/amd64"
DEFAULT_OUTPUT = (
    ROOT
    / "docs"
    / "benchmarks"
    / "receipts"
    / "half-flow-lm-p0-baseline-2026-07-21.json"
)
SCHEMA = "scalafim-half-flow-p0-baseline-v1"

HASHED_PATHS = (
    "build.sbt",
    "project/build.properties",
    "modules/image/shared/src/main/scala/scalafim/image/SpatialCoordinates.scala",
    "modules/image/shared/src/main/scala/scalafim/image/Morphism.scala",
    "modules/image/shared/src/main/scala/scalafim/image/MorphismFields.scala",
    "modules/image/shared/src/main/scala/scalafim/image/DenseFieldInterpolationPlan.scala",
    "modules/image/shared/src/main/scala/scalafim/image/DenseFieldInverse.scala",
    "modules/image/shared/src/main/scala/scalafim/image/ResamplingPlan.scala",
    "modules/image/shared/src/main/scala/scalafim/image/Transforms.scala",
    "tools/registration/generate_halfflow_oracles.py",
    "tools/registration/capture_halfflow_p0_baseline.py",
    "modules/registration/fixtures/v1/half-flow-synthetic.json",
)


def command(*args: str, timeout_seconds: float = 5.0) -> tuple[bool, str]:
    try:
        completed = subprocess.run(
            args,
            cwd=ROOT,
            check=False,
            capture_output=True,
            text=True,
            timeout=timeout_seconds,
        )
    except FileNotFoundError:
        return False, "not-found"
    except subprocess.TimeoutExpired:
        return False, f"timeout-after-{timeout_seconds:g}-seconds"
    output = (completed.stdout or completed.stderr).strip()
    return completed.returncode == 0, output


def first_line(output: str) -> str | None:
    return output.splitlines()[0] if output else None


def extract(pattern: str, text: str, label: str) -> str:
    match = re.search(pattern, text, flags=re.MULTILINE)
    if match is None:
        raise ValueError(f"could not find {label}")
    return match.group(1)


def sha256(relative: str) -> str:
    return hashlib.sha256((ROOT / relative).read_bytes()).hexdigest()


def sha256_path(path: Path) -> str | None:
    return hashlib.sha256(path.read_bytes()).hexdigest() if path.is_file() else None


def docker_environment() -> dict[str, Any]:
    docker_path = shutil.which("docker")
    if docker_path is None:
        return {
            "available": False,
            "context": None,
            "expectedContext": DOCKER_CONTEXT,
            "socket": str(DOCKER_SOCKET),
            "socketExists": DOCKER_SOCKET.exists(),
            "daemonResponsive": False,
            "daemonProbe": "docker-cli-not-found",
        }

    version_ok, docker_version = command("docker", "--version")
    context_ok, context = command("docker", "context", "show")
    inspect_ok, inspect = command("docker", "context", "inspect", DOCKER_CONTEXT)
    info_ok, info = command(
        "docker",
        "--context",
        DOCKER_CONTEXT,
        "info",
        "--format",
        "{{json .}}",
        timeout_seconds=5.0,
    )

    context_host = None
    if inspect_ok:
        parsed = json.loads(inspect)
        context_host = parsed[0]["Endpoints"]["docker"]["Host"]

    image_id = None
    repo_digests: list[str] = []
    if info_ok:
        id_ok, image_id_output = command(
            "docker",
            "--context",
            DOCKER_CONTEXT,
            "image",
            "inspect",
            ANTS_DOCKER_IMAGE,
            "--format",
            "{{.Id}}",
        )
        digest_ok, digest_output = command(
            "docker",
            "--context",
            DOCKER_CONTEXT,
            "image",
            "inspect",
            ANTS_DOCKER_IMAGE,
            "--format",
            "{{json .RepoDigests}}",
        )
        image_id = image_id_output if id_ok else None
        if digest_ok:
            parsed_digests = json.loads(digest_output)
            repo_digests = parsed_digests if isinstance(parsed_digests, list) else []

    return {
        "available": True,
        "cli": docker_path,
        "version": first_line(docker_version) if version_ok else None,
        "context": first_line(context) if context_ok else None,
        "expectedContext": DOCKER_CONTEXT,
        "contextHost": context_host,
        "socket": str(DOCKER_SOCKET),
        "socketExists": DOCKER_SOCKET.exists(),
        "daemonResponsive": info_ok,
        "daemonProbe": "ok" if info_ok else first_line(info),
        "antsImage": {
            "retrievalReference": ANTS_DOCKER_IMAGE,
            "platform": ANTS_DOCKER_PLATFORM,
            "imageId": image_id,
            "repoDigests": repo_digests,
            "versionProbeExecuted": False,
            "versionProbeCommand": (
                "docker run --rm --platform linux/amd64 "
                "antsx/ants:latest antsRegistration --version"
            ),
        },
    }


def hodgeflow_harness() -> dict[str, Any]:
    script = HODGEFLOW_ROOT / "inst/benchmarks/benchmark_sub18_synquick_fixedmask.R"
    common = HODGEFLOW_ROOT / "inst/benchmarks/benchmark_common.R"
    head_ok, head = command("git", "-C", str(HODGEFLOW_ROOT), "rev-parse", "HEAD")
    return {
        "repository": str(HODGEFLOW_ROOT),
        "head": first_line(head) if head_ok else None,
        "entrypoint": str(script),
        "entrypointSha256": sha256_path(script),
        "commonHarnessSha256": sha256_path(common),
        "workMount": "/work",
    }


def environment() -> dict[str, Any]:
    java_ok, java = command("java", "-version")
    node_ok, node = command("node", "--version")
    ants_path = shutil.which("antsRegistration")
    ants_ok = ants_path is not None
    ants_version_ok = False
    ants_version = ""
    if ants_ok:
        ants_version_ok, ants_version = command("antsRegistration", "--version")
    return {
        "platform": platform.platform(),
        "python": platform.python_version(),
        "java": first_line(java) if java_ok else None,
        "node": first_line(node) if node_ok else None,
        "antsRegistration": {
            "available": ants_ok,
            "path": ants_path,
            "version": first_line(ants_version) if ants_version_ok else None,
        },
    }


def build_receipt(
    captured_on: str,
    repository_head: str | None = None,
) -> dict[str, Any]:
    build = (ROOT / "build.sbt").read_text(encoding="utf-8")
    properties = (ROOT / "project" / "build.properties").read_text(encoding="utf-8")
    git_ok, git_head = command("git", "rev-parse", "HEAD")
    if not git_ok:
        raise ValueError("could not resolve ScalaFIM git HEAD")
    docker = docker_environment()
    ants_image = docker.get("antsImage", {})
    repo_digests = ants_image.get("repoDigests", [])
    materialized_image_identity = (
        repo_digests[0]
        if repo_digests
        else ants_image.get("imageId")
    )

    return {
        "schema": SCHEMA,
        "capturedOn": captured_on,
        "repository": {
            "head": repository_head or git_head,
            "workingTreePolicy": (
                "shared checkout may be dirty; hashes below, not global status, "
                "define this baseline"
            ),
        },
        "toolchain": {
            "scala": extract(r'ThisBuild / scalaVersion\s*:=\s*"([^"]+)"', build, "Scala version"),
            "sbt": extract(r"sbt\.version=([^\s]+)", properties, "sbt version"),
            "galeRevision": extract(r'lazy val galeRevision\s*=\s*"([0-9a-f]+)"', build, "Gale revision"),
        },
        "environment": environment(),
        "docker": docker,
        "externalHarness": hodgeflow_harness(),
        "antsComparator": {
            "profileId": "hodgeflow-synquick-fixedmask-docker-v1",
            "profile": "synquick_fixedmask",
            "imageRetrievalReference": ANTS_DOCKER_IMAGE,
            "materializedImageIdentity": materialized_image_identity,
            "materializedIdentityRule": "repo digest preferred, image ID fallback",
            "digestRequiredBeforeBenchmark": True,
            "randomSeed": 1,
            "threads": 1,
            "execution": "serial-only",
            "hostArchitecture": "Apple Silicon",
            "containerPlatform": ANTS_DOCKER_PLATFORM,
            "emulated": True,
            "controlledNonlinearSameAffineProfile": "separate and not yet materialized",
        },
        "sourceSha256": {relative: sha256(relative) for relative in HASHED_PATHS},
        "structuralBaseline": {
            "registrationKernelMeasurementsPresent": False,
            "reason": (
                "P0 precedes the registration module and allocation-controlled field kernels; "
                "timing a semantically different boxed API would not be a comparable leaf-kernel baseline"
            ),
            "image": {
                "denseVectorStorage": "structure-of-arrays NArray[Double]",
                "sourceCoordinateMaterialization": "GridSpec.worldPoints plus Vector[WorldPoint]",
                "denseInterpolationPlan": "Vector[DenseFieldStencil] with per-stencil arrays",
                "inversePath": "iterative point path; reference-only for HalfFlow optimization",
                "primitiveCoordinateBridgeVisibility": "private[image]",
            },
            "gale": {
                "role": "tiny-algebra and matrix-free operator reference contracts",
                "fieldTensorBackendPresent": False,
                "registrationSpecificStencilAllowed": False,
            },
        },
        "nextComparableMeasurements": {
            "jvm": {
                "harness": "JMH with GC profiler",
                "shapes": [[64, 64, 64], [128, 128, 128], [256, 256, 256]],
                "metrics": ["ns/op", "bytes/op", "gc.count", "checksum"],
            },
            "scalaJs": {
                "harness": "fullLinkJS Node benchmark",
                "shapes": [[32, 32, 32], [64, 64, 64], [128, 128, 128]],
                "metrics": ["median-ms", "owned-full-volume-constructions", "checksum"],
            },
            "rule": (
                "P1 records reference and destination-writing candidates in the same harness "
                "before selecting or deleting either path"
            ),
        },
    }


def render(receipt: dict[str, Any]) -> str:
    return json.dumps(receipt, indent=2, sort_keys=True, allow_nan=False) + "\n"


def validate_historical_receipt(receipt: dict[str, Any]) -> None:
    if receipt.get("schema") != SCHEMA:
        raise SystemExit(f"unexpected receipt schema: {receipt.get('schema')!r}")
    captured_on = receipt.get("capturedOn")
    if not isinstance(captured_on, str):
        raise SystemExit("receipt lacks capturedOn")
    try:
        dt.date.fromisoformat(captured_on)
    except ValueError as error:
        raise SystemExit(f"invalid capturedOn date: {captured_on!r}") from error

    repository_head = receipt.get("repository", {}).get("head")
    if not isinstance(repository_head, str) or re.fullmatch(r"[0-9a-f]{40}", repository_head) is None:
        raise SystemExit("receipt lacks a valid repository head")

    source_hashes = receipt.get("sourceSha256")
    if not isinstance(source_hashes, dict) or set(source_hashes) != set(HASHED_PATHS):
        raise SystemExit("receipt sourceSha256 paths do not match the frozen P0 source set")
    for relative, digest in source_hashes.items():
        if not isinstance(digest, str) or re.fullmatch(r"[0-9a-f]{64}", digest) is None:
            raise SystemExit(f"invalid frozen source hash for {relative}")

    for required in (
        "antsComparator",
        "docker",
        "environment",
        "externalHarness",
        "nextComparableMeasurements",
        "structuralBaseline",
        "toolchain",
    ):
        if required not in receipt:
            raise SystemExit(f"receipt lacks required section: {required}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument(
        "--check",
        action="store_true",
        help="validate the historical receipt and its frozen hash inventory",
    )
    parser.add_argument(
        "--live-sources",
        action="store_true",
        help="with --check, also compare the receipt with the current sources and environment",
    )
    args = parser.parse_args()
    output = args.output if args.output.is_absolute() else ROOT / args.output

    if args.live_sources and not args.check:
        parser.error("--live-sources requires --check")

    if args.check:
        if not output.exists():
            raise SystemExit(f"missing receipt: {output.relative_to(ROOT)}")
        existing = json.loads(output.read_text(encoding="utf-8"))
        validate_historical_receipt(existing)
        if not args.live_sources:
            print(f"historical receipt valid: {output.relative_to(ROOT)}")
            return
        captured_on = existing.get("capturedOn")
        repository_head = existing.get("repository", {}).get("head")
        expected = render(build_receipt(captured_on, repository_head))
        actual = output.read_text(encoding="utf-8")
        if actual != expected:
            raise SystemExit("P0 baseline receipt is stale for the current sources or environment")
        print(f"live-source receipt current: {output.relative_to(ROOT)}")
    else:
        captured_on = dt.date.today().isoformat()
        output.parent.mkdir(parents=True, exist_ok=True)
        output.write_text(render(build_receipt(captured_on)), encoding="utf-8")
        print(f"wrote {output.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
