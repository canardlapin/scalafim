#!/usr/bin/env python3
"""Capture or validate the response/archive Phase 0 baseline receipt."""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
import platform
import re
import struct
import subprocess
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[2]
SCHEMA = "scalafim-response-archive-phase0-receipt-v1"
CORPUS_SCHEMA = "scalafim-response-archive-migration-baseline-v1"
START_HEAD = "7c590372dd3cea733f5835b9ca1bb6f62e92292f"
PLAN_COMMIT = "7afc0ee19167eeb8f2acc4380e7733e3e1f0951e"
BRIDGE_COMMIT = "b9d96e2a23e24a57beeb36b978b14935c81071f5"
ANCESTOR_COMMITS = {
    "scala374Experiment": "4d2ddddf5b7a7183a16235bb15eb50abcbfe75ed",
    "frame4sExtraction": "b6a3af2af4dad57413e80ebce0ddf33633d704a9",
}
DEFAULT_OUTPUT = (
    ROOT
    / "docs"
    / "benchmarks"
    / "receipts"
    / "response-archive-phase0-baseline-2026-07-26.json"
)

HASHED_PATHS = (
    "build.sbt",
    "project/build.properties",
    "docs/plans/response-representation-archive-architecture.md",
    "docs/plans/response-representation-archive-phase-0.md",
    "tools/response_archive/capture_phase0_baseline.py",
    "modules/image/shared/src/main/scala/scalafim/image/DMat.scala",
    "modules/image/shared/src/test/scala/scalafim/image/CatsEffectSharedBoundarySuite.scala",
    "modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/design/SamplingFrame.scala",
    "modules/archive/shared/src/main/scala/scalafim/archive/lna/LnaModel.scala",
    "modules/archive/shared/src/main/scala/scalafim/archive/lna/LnaPipeline.scala",
    "modules/latent/shared/src/main/scala/scalafim/latent/LatentArchiveDescriptors.scala",
    "modules/latent/shared/src/test/scala/scalafim/latent/DecodePlanTypingSpikeSuite.scala",
    "modules/latent/shared/src/test/scala/scalafim/latent/ResponseArchiveMigrationBaselineSuite.scala",
    "modules/dataset-zarr/jvm/src/main/scala/scalafim/dataset/zarr/FmriDatasetZarr.scala",
    "modules/dataset-zarr/jvm/src/main/scala/scalafim/dataset/zarr/NiftiCanonicalImporter.scala",
    "modules/dataset-zarr/jvm/src/main/scala/scalafim/dataset/zarr/ZarrResponseBlockSource.scala",
    "modules/dataset-zarr/shared/src/main/scala/scalafim/dataset/zarr/CanonicalBoldSampling.scala",
    "modules/dataset/jvm/src/main/scala/scalafim/dataset/io/FmriDatasetLna.scala",
    "modules/dataset/jvm/src/main/scala/scalafim/dataset/io/LnaDataset.scala",
    "modules/dataset/shared/src/main/scala/scalafim/dataset/DatasetError.scala",
    "modules/dataset/shared/src/main/scala/scalafim/dataset/LatentArchiveDatasetBackend.scala",
    "modules/dataset/shared/src/main/scala/scalafim/dataset/LatentResponseDatasetBackend.scala",
    "modules/latent/shared/src/main/scala/scalafim/latent/BoldZipLatentArchiveCodec.scala",
    "modules/latent/shared/src/main/scala/scalafim/latent/ExplicitLatentArchiveCodec.scala",
    "modules/latent/shared/src/main/scala/scalafim/latent/LatentArchiveCodec.scala",
    "modules/latent/shared/src/main/scala/scalafim/latent/LatentArchivePayloads.scala",
    "modules/latent/shared/src/main/scala/scalafim/latent/LatentEncoder.scala",
    "modules/latent/shared/src/main/scala/scalafim/latent/RadialBasis.scala",
    "modules/latent/shared/src/main/scala/scalafim/latent/SharedBasisEncoder.scala",
    "modules/latent/shared/src/main/scala/scalafim/latent/SharedBasisLatentArchiveCodec.scala",
    "modules/latent/shared/src/main/scala/scalafim/latent/TransportLatentArchiveCodec.scala",
)

TWO_DOMAIN_FILES = (
    "modules/dataset-zarr/jvm/src/main/scala/scalafim/dataset/zarr/FmriDatasetZarr.scala",
    "modules/dataset-zarr/jvm/src/main/scala/scalafim/dataset/zarr/NiftiCanonicalImporter.scala",
    "modules/dataset-zarr/jvm/src/main/scala/scalafim/dataset/zarr/ZarrResponseBlockSource.scala",
    "modules/dataset-zarr/shared/src/main/scala/scalafim/dataset/zarr/CanonicalBoldSampling.scala",
    "modules/dataset/jvm/src/main/scala/scalafim/dataset/io/FmriDatasetLna.scala",
    "modules/dataset/jvm/src/main/scala/scalafim/dataset/io/LnaDataset.scala",
    "modules/dataset/shared/src/main/scala/scalafim/dataset/DatasetError.scala",
    "modules/dataset/shared/src/main/scala/scalafim/dataset/LatentArchiveDatasetBackend.scala",
    "modules/latent/shared/src/main/scala/scalafim/latent/BoldZipLatentArchiveCodec.scala",
    "modules/latent/shared/src/main/scala/scalafim/latent/ExplicitLatentArchiveCodec.scala",
    "modules/latent/shared/src/main/scala/scalafim/latent/SharedBasisLatentArchiveCodec.scala",
    "modules/latent/shared/src/main/scala/scalafim/latent/TransportLatentArchiveCodec.scala",
)

FORBIDDEN_EDGE_FILES = {
    "latentToArchive": (
        "modules/latent/shared/src/main/scala/scalafim/latent/BoldZipLatentArchiveCodec.scala",
        "modules/latent/shared/src/main/scala/scalafim/latent/ExplicitLatentArchiveCodec.scala",
        "modules/latent/shared/src/main/scala/scalafim/latent/LatentArchiveCodec.scala",
        "modules/latent/shared/src/main/scala/scalafim/latent/LatentArchiveDescriptors.scala",
        "modules/latent/shared/src/main/scala/scalafim/latent/LatentArchivePayloads.scala",
        "modules/latent/shared/src/main/scala/scalafim/latent/LatentEncoder.scala",
        "modules/latent/shared/src/main/scala/scalafim/latent/RadialBasis.scala",
        "modules/latent/shared/src/main/scala/scalafim/latent/SharedBasisEncoder.scala",
        "modules/latent/shared/src/main/scala/scalafim/latent/SharedBasisLatentArchiveCodec.scala",
        "modules/latent/shared/src/main/scala/scalafim/latent/TransportLatentArchiveCodec.scala",
    ),
    "datasetToArchive": (
        "modules/dataset/jvm/src/main/scala/scalafim/dataset/io/FmriDatasetLna.scala",
        "modules/dataset/jvm/src/main/scala/scalafim/dataset/io/LnaDataset.scala",
        "modules/dataset/shared/src/main/scala/scalafim/dataset/DatasetError.scala",
        "modules/dataset/shared/src/main/scala/scalafim/dataset/LatentArchiveDatasetBackend.scala",
    ),
    "datasetToLatent": (
        "modules/dataset/jvm/src/main/scala/scalafim/dataset/io/FmriDatasetLna.scala",
        "modules/dataset/jvm/src/main/scala/scalafim/dataset/io/LnaDataset.scala",
        "modules/dataset/shared/src/main/scala/scalafim/dataset/DatasetError.scala",
        "modules/dataset/shared/src/main/scala/scalafim/dataset/LatentArchiveDatasetBackend.scala",
        "modules/dataset/shared/src/main/scala/scalafim/dataset/LatentResponseDatasetBackend.scala",
    ),
}

CORPUS_CASES = (
    ("dense", "raw-bits"),
    ("lna-quant", "raw-bits"),
    ("lna-delta", "raw-bits"),
    ("lna-delta-quant", "raw-bits"),
    ("lna-basis-embed", "raw-bits"),
    ("latent-explicit", "raw-bits"),
    ("lna-temporal-dct", "absolute-relative"),
    ("latent-temporal-dct", "absolute-relative"),
    ("latent-temporal-haar", "absolute-relative"),
    ("latent-shared-basis", "absolute-relative"),
    ("latent-hrbf-shared-basis", "absolute-relative"),
    ("latent-transport", "raw-bits"),
    ("latent-boldzip", "raw-bits"),
)

JVM_CORPUS_COMMAND = (
    "sbt",
    "archivedResponseInteropJVM/Test/runMain "
    "scalafim.latent.ResponseArchiveMigrationBaselineMain",
)
JS_CORPUS_COMMAND = (
    "sbt",
    "set archivedResponseInteropJS / Test / "
    "scalaJSUseTestModuleInitializer := false",
    "set archivedResponseInteropJS / Test / "
    "scalaJSUseMainModuleInitializer := true",
    "set archivedResponseInteropJS / Test / mainClass := "
    'Some("scalafim.latent.ResponseArchiveMigrationBaselineMain")',
    "archivedResponseInteropJS / Test / run",
)


def command(
    *args: str,
    timeout_seconds: float = 600.0,
    require_success: bool = False,
) -> tuple[bool, str]:
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
        if require_success:
            raise SystemExit(f"command not found: {args[0]}")
        return False, "not-found"
    except subprocess.TimeoutExpired as error:
        if require_success:
            raise SystemExit(f"command timed out: {' '.join(args)}") from error
        return False, f"timeout-after-{timeout_seconds:g}-seconds"

    output = "\n".join(part for part in (completed.stdout, completed.stderr) if part).strip()
    if require_success and completed.returncode != 0:
        raise SystemExit(
            f"command failed ({completed.returncode}): {' '.join(args)}\n{output}"
        )
    return completed.returncode == 0, output


def extract(pattern: str, text: str, label: str) -> str:
    match = re.search(pattern, text, flags=re.MULTILINE)
    if match is None:
        raise ValueError(f"could not find {label}")
    return match.group(1)


def sha256_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def sha256(relative: str) -> str:
    return sha256_bytes((ROOT / relative).read_bytes())


def canonical_json(value: Any) -> str:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), allow_nan=False)


def git_output(*args: str) -> str:
    ok, output = command("git", *args)
    if not ok:
        raise ValueError(f"git command failed: {' '.join(args)}")
    return output.splitlines()[0]


def is_ancestor(commit: str, descendant: str) -> bool:
    ok, _ = command("git", "merge-base", "--is-ancestor", commit, descendant)
    return ok


def extract_corpus(output: str) -> dict[str, Any]:
    match = re.search(
        r"RRA_PHASE0_BASELINE_BEGIN\s*(\{.*\})\s*RRA_PHASE0_BASELINE_END",
        output,
        flags=re.DOTALL,
    )
    if match is None:
        raise SystemExit("corpus command did not emit the Phase 0 markers")
    payload = json.loads(match.group(1))
    if payload.get("schema") != CORPUS_SCHEMA:
        raise SystemExit(f"unexpected corpus schema: {payload.get('schema')!r}")
    cases = payload.get("cases")
    if not isinstance(cases, list):
        raise SystemExit("corpus lacks cases")
    observed = tuple(
        (case.get("id"), case.get("comparator", {}).get("kind")) for case in cases
    )
    if observed != CORPUS_CASES:
        raise SystemExit(f"unexpected corpus cases: {observed!r}")
    return payload


def double_from_hex(value: str) -> float:
    return struct.unpack(">d", struct.pack(">Q", int(value, 16)))[0]


def validate_cross_platform(
    jvm: dict[str, Any],
    scala_js: dict[str, Any],
) -> None:
    jvm_cases = {case["id"]: case for case in jvm["cases"]}
    js_cases = {case["id"]: case for case in scala_js["cases"]}
    if jvm_cases.keys() != js_cases.keys():
        raise SystemExit("JVM and Scala.js corpus case sets differ")

    for case_id, comparator_kind in CORPUS_CASES:
        expected = jvm_cases[case_id]
        observed = js_cases[case_id]
        for field in ("rows", "columns"):
            if expected[field] != observed[field]:
                raise SystemExit(f"{case_id}: JVM/Scala.js {field} differs")
        if comparator_kind == "raw-bits":
            if expected["rawBits"] != observed["rawBits"]:
                raise SystemExit(f"{case_id}: raw-bit JVM/Scala.js drift")
        else:
            comparator = expected["comparator"]
            absolute = float(comparator["absolute"])
            relative = float(comparator["relative"])
            for index, (expected_hex, observed_hex) in enumerate(
                zip(expected["rawBits"], observed["rawBits"], strict=True)
            ):
                expected_value = double_from_hex(expected_hex)
                observed_value = double_from_hex(observed_hex)
                tolerance = absolute + relative * max(
                    abs(expected_value), abs(observed_value)
                )
                if abs(observed_value - expected_value) > tolerance:
                    raise SystemExit(
                        f"{case_id}[{index}]: JVM/Scala.js drift exceeds {tolerance}"
                    )


def corpus_summary(payload: dict[str, Any]) -> dict[str, Any]:
    canonical = canonical_json(payload)
    return {
        "canonicalSha256": sha256_bytes(canonical.encode("utf-8")),
        "caseSha256": {
            case["id"]: sha256_bytes(canonical_json(case).encode("utf-8"))
            for case in payload["cases"]
        },
    }


def capture_corpus() -> dict[str, Any]:
    _, jvm_output = command(*JVM_CORPUS_COMMAND, require_success=True)
    _, js_output = command(*JS_CORPUS_COMMAND, require_success=True)
    jvm = extract_corpus(jvm_output)
    scala_js = extract_corpus(js_output)
    validate_cross_platform(jvm, scala_js)
    return {
        "status": "passed",
        "jvm": corpus_summary(jvm),
        "scalaJs": corpus_summary(scala_js),
        "commands": {
            "jvm": list(JVM_CORPUS_COMMAND),
            "scalaJs": list(JS_CORPUS_COMMAND),
        },
    }


def linked_js_bytes() -> int | None:
    output = (
        ROOT
        / "modules"
        / "image"
        / "js"
        / "target"
        / "scala-3.4.2"
        / "scalafim-image-test-fastopt"
        / "main.js"
    )
    return output.stat().st_size if output.is_file() else None


def environment() -> dict[str, Any]:
    java_ok, java = command("java", "-version", timeout_seconds=5.0)
    node_ok, node = command("node", "--version", timeout_seconds=5.0)
    return {
        "platform": platform.platform(),
        "python": platform.python_version(),
        "java": java.splitlines()[0] if java_ok and java else None,
        "node": node.splitlines()[0] if node_ok and node else None,
    }


def build_receipt(
    captured_on: str,
    corpus: dict[str, Any] | None = None,
) -> dict[str, Any]:
    build = (ROOT / "build.sbt").read_text(encoding="utf-8")
    properties = (ROOT / "project" / "build.properties").read_text(encoding="utf-8")
    head = git_output("rev-parse", "HEAD")
    branch = git_output("branch", "--show-current")
    missing = [relative for relative in HASHED_PATHS if not (ROOT / relative).is_file()]
    if missing:
        raise ValueError(f"missing frozen source paths: {missing!r}")

    return {
        "schema": SCHEMA,
        "capturedOn": captured_on,
        "repository": {
            "canonical": "canardlapin/scalafim",
            "branchAtCapture": branch,
            "startHead": START_HEAD,
            "workingHeadAtCapture": head,
            "workingTreePolicy": (
                "startHead plus sourceSha256 define the baseline; unrelated dirty "
                "working-tree state is excluded"
            ),
            "governingPlanCommit": PLAN_COMMIT,
            "datasetBridgeCommit": BRIDGE_COMMIT,
            "ancestorCommits": {
                name: {
                    "commit": commit,
                    "isAncestorOfStartHead": is_ancestor(commit, START_HEAD),
                    "unmergedPrerequisite": False,
                }
                for name, commit in ANCESTOR_COMMITS.items()
            },
        },
        "toolchain": {
            "scala": extract(
                r'ThisBuild / scalaVersion\s*:=\s*"([^"]+)"',
                build,
                "Scala version",
            ),
            "sbt": extract(r"sbt\.version=([^\s]+)", properties, "sbt version"),
            "catsEffect": extract(
                r'"org\.typelevel"\s*%%%\s*"cats-effect"\s*%\s*"([^"]+)"',
                build,
                "Cats Effect version",
            ),
        },
        "environment": environment(),
        "inventories": {
            "twoPrincipalDomainFiles": list(TWO_DOMAIN_FILES),
            "forbiddenEdges": {
                edge: list(files) for edge, files in FORBIDDEN_EDGE_FILES.items()
            },
            "representationFamilies": [
                "Explicit",
                "TemporalDct",
                "TemporalHaar",
                "SharedBasis",
                "Transport",
                "BoldZip",
            ],
            "manifestVariants": {
                "lna": "LNA R v2.0",
                "sharedBasisRegistry": "scalafim-lna-basis-registry-0",
                "zarrProfile": "neuroarchive-zarr-0.1",
                "zarrPublicationReceipt": 1,
            },
        },
        "decisions": {
            "resolved": ["D1", "D2", "D3", "D4", "D6", "D7", "D8", "D9", "D10"],
            "deferred": ["D5"],
            "responseBlock": "owned row-major Double buffer; no tensor framework",
            "datasetEffects": "pure FmriDataset; effects in OpenedDataset and ResponseSource",
        },
        "corpus": {
            "schema": CORPUS_SCHEMA,
            "definitionPath": (
                "modules/latent/shared/src/test/scala/scalafim/latent/"
                "ResponseArchiveMigrationBaselineSuite.scala"
            ),
            "definitionSha256": sha256(
                "modules/latent/shared/src/test/scala/scalafim/latent/"
                "ResponseArchiveMigrationBaselineSuite.scala"
            ),
            "cases": [
                {"id": case_id, "comparator": comparator}
                for case_id, comparator in CORPUS_CASES
            ],
            "execution": corpus or {"status": "not-captured"},
        },
        "effectsSpike": {
            "path": (
                "modules/image/shared/src/test/scala/scalafim/image/"
                "CatsEffectSharedBoundarySuite.scala"
            ),
            "laws": ["release-on-success", "release-on-failure", "release-on-cancel"],
            "scalaJsTestFastLinkBytes": linked_js_bytes(),
            "sizeInterpretation": (
                "total image test fast-link output, not marginal production cost; "
                "Phase 1 repeats for the response artifact"
            ),
        },
        "sourceSha256": {relative: sha256(relative) for relative in HASHED_PATHS},
    }


def render(receipt: dict[str, Any]) -> str:
    return json.dumps(receipt, indent=2, sort_keys=True, allow_nan=False) + "\n"


def validate_historical_receipt(receipt: dict[str, Any]) -> None:
    if receipt.get("schema") != SCHEMA:
        raise SystemExit(f"unexpected receipt schema: {receipt.get('schema')!r}")
    try:
        dt.date.fromisoformat(receipt["capturedOn"])
    except (KeyError, TypeError, ValueError) as error:
        raise SystemExit("receipt lacks a valid capturedOn date") from error
    repository = receipt.get("repository", {})
    if repository.get("startHead") != START_HEAD:
        raise SystemExit("receipt startHead differs from the frozen migration baseline")
    hashes = receipt.get("sourceSha256")
    if not isinstance(hashes, dict) or set(hashes) != set(HASHED_PATHS):
        raise SystemExit("receipt sourceSha256 paths do not match the frozen source set")
    for relative, digest in hashes.items():
        if re.fullmatch(r"[0-9a-f]{64}", str(digest)) is None:
            raise SystemExit(f"invalid source hash for {relative}")
    inventories = receipt.get("inventories", {})
    if tuple(inventories.get("twoPrincipalDomainFiles", ())) != TWO_DOMAIN_FILES:
        raise SystemExit("receipt two-domain inventory differs from the frozen inventory")
    corpus = receipt.get("corpus", {})
    observed_cases = tuple(
        (case.get("id"), case.get("comparator"))
        for case in corpus.get("cases", ())
    )
    if observed_cases != CORPUS_CASES:
        raise SystemExit("receipt corpus inventory differs from the frozen inventory")


def validate_live_sources(receipt: dict[str, Any]) -> None:
    live = {relative: sha256(relative) for relative in HASHED_PATHS}
    if receipt["sourceSha256"] != live:
        changed = sorted(
            relative
            for relative in HASHED_PATHS
            if receipt["sourceSha256"].get(relative) != live.get(relative)
        )
        raise SystemExit(f"Phase 0 receipt is stale for: {', '.join(changed)}")


def verify_corpus(receipt: dict[str, Any]) -> None:
    recorded = receipt.get("corpus", {}).get("execution", {})
    if recorded.get("status") != "passed":
        raise SystemExit("historical receipt does not contain a passed corpus execution")
    observed = capture_corpus()
    for platform_name in ("jvm", "scalaJs"):
        if observed[platform_name] != recorded.get(platform_name):
            raise SystemExit(f"{platform_name} corpus output differs from the receipt")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument(
        "--run-corpus",
        action="store_true",
        help="execute JVM and Scala.js corpus generators while writing the receipt",
    )
    parser.add_argument(
        "--stdout",
        action="store_true",
        help="render a new receipt to stdout instead of writing --output",
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="validate the historical receipt without rewriting it",
    )
    parser.add_argument(
        "--live-sources",
        action="store_true",
        help="with --check, compare frozen hashes with the current source files",
    )
    parser.add_argument(
        "--verify-corpus",
        action="store_true",
        help="with --check, rerun and compare the JVM and Scala.js corpus",
    )
    args = parser.parse_args()
    output = args.output if args.output.is_absolute() else ROOT / args.output

    if (args.live_sources or args.verify_corpus) and not args.check:
        parser.error("--live-sources and --verify-corpus require --check")
    if args.check and args.run_corpus:
        parser.error("--run-corpus writes a receipt and cannot be combined with --check")

    if args.check:
        if not output.is_file():
            raise SystemExit(f"missing receipt: {output.relative_to(ROOT)}")
        receipt = json.loads(output.read_text(encoding="utf-8"))
        validate_historical_receipt(receipt)
        if args.live_sources:
            validate_live_sources(receipt)
        if args.verify_corpus:
            verify_corpus(receipt)
        print(f"Phase 0 receipt valid: {output.relative_to(ROOT)}")
        return

    corpus = capture_corpus() if args.run_corpus else None
    receipt = build_receipt(dt.date.today().isoformat(), corpus)
    if args.stdout:
        print(render(receipt), end="")
        return
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(render(receipt), encoding="utf-8")
    print(f"wrote {output.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
