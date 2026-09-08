#!/usr/bin/env python3
"""Verify or regenerate the versioned MVPA oracle freshness receipt.

The default check is deliberately standard-library-only and never invokes R.
Regeneration is a separate explicit operation that executes each manifest's
claimed base-R source, replaces its committed Scala output, and refreshes the
R version plus generator/output SHA-256 receipts.
"""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import math
import os
from pathlib import Path, PurePosixPath
import re
import subprocess
import sys
from typing import NoReturn


REPO_ROOT = Path(__file__).resolve().parents[2]
MANIFEST_PATH = REPO_ROOT / "tools" / "r-parity" / "mvpa-oracle-manifest-v1.json"
SCHEMA_PATH = REPO_ROOT / "tools" / "r-parity" / "mvpa-oracle-manifest-v1.schema.json"
SCHEMA_VERSION = "scalafim-mvpa-oracle-manifest/v1"
SCHEMA_RELATIVE_PATH = "tools/r-parity/mvpa-oracle-manifest-v1.schema.json"
HEX_DIGEST = re.compile(r"^[0-9a-f]{64}$")
R_VERSION = re.compile(r"^[0-9]+\.[0-9]+\.[0-9]+$")
FAMILY_ID = re.compile(r"^[a-z][a-z0-9-]*$")

REQUIRED_FAMILIES = {
  "crossvalidated-rdm": (
    "tools/r-parity/generate_crossvalidated_rdm_fixtures.R",
    "modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/CrossvalidatedRdmReferenceFixtures.scala",
    "scalafim.fmri.mvpa.CrossvalidatedRdmParitySuite",
  ),
  "cross-domain-classification": (
    "tools/r-parity/generate_cross_domain_classification_fixtures.R",
    "modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/CrossDomainClassificationReferenceFixtures.scala",
    "scalafim.fmri.mvpa.predictive.CrossDomainClassificationSuite",
  ),
  "canonical-effect": (
    "tools/r-parity/generate_canonical_effect_fixtures.R",
    "modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/CanonicalEffectReferenceFixtures.scala",
    "scalafim.fmri.mvpa.CanonicalEffectAcceptanceSuite",
  ),
  "manova": (
    "tools/r-parity/generate_manova_fixtures.R",
    "modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/ManovaReferenceFixtures.scala",
    "scalafim.fmri.mvpa.ManovaMvpaSuite",
  ),
  "nonnegative-canonical": (
    "tools/r-parity/generate_constrained_canonical_fixtures.R",
    "modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/ConstrainedCanonicalReferenceFixtures.scala",
    "scalafim.fmri.mvpa.ConstrainedCanonicalMvpaSuite",
  ),
  "signed-cross-run-rayleigh": (
    "tools/r-parity/generate_signed_cross_run_rayleigh_fixtures.R",
    "modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/SignedCrossRunRayleighReferenceFixtures.scala",
    "scalafim.fmri.mvpa.SignedCrossRunRayleighMvpaSuite",
  ),
}


class ManifestError(ValueError):
  """A deterministic, user-facing manifest validation failure."""


def fail(message: str) -> NoReturn:
  raise ManifestError(message)


def load_json(path: Path) -> dict[str, object]:
  try:
    payload = json.loads(path.read_text(encoding="utf-8"))
  except (OSError, json.JSONDecodeError) as error:
    fail(f"cannot read JSON object {path}: {error}")
  if not isinstance(payload, dict):
    fail(f"expected a JSON object in {path}")
  return payload


def sha256(path: Path) -> str:
  digest = hashlib.sha256()
  with path.open("rb") as source:
    for block in iter(lambda: source.read(1024 * 1024), b""):
      digest.update(block)
  return digest.hexdigest()


def object_field(container: dict[str, object], name: str, context: str) -> dict[str, object]:
  value = container.get(name)
  if not isinstance(value, dict):
    fail(f"{context}.{name} must be an object")
  return value


def array_field(container: dict[str, object], name: str, context: str) -> list[object]:
  value = container.get(name)
  if not isinstance(value, list) or not value:
    fail(f"{context}.{name} must be a non-empty array")
  return value


def string_field(container: dict[str, object], name: str, context: str) -> str:
  value = container.get(name)
  if not isinstance(value, str) or not value:
    fail(f"{context}.{name} must be a non-empty string")
  return value


def exact_keys(container: dict[str, object], expected: set[str], context: str) -> None:
  actual = set(container)
  if actual != expected:
    fail(
      f"{context} fields differ from the v1 schema; "
      f"missing={sorted(expected - actual)}, extra={sorted(actual - expected)}"
    )


def safe_repository_path(root: Path, relative: str, context: str) -> Path:
  logical = PurePosixPath(relative)
  if logical.is_absolute() or ".." in logical.parts or "." in logical.parts:
    fail(f"{context} must be a normalized repository-relative path: {relative}")
  resolved = (root / Path(*logical.parts)).resolve()
  try:
    resolved.relative_to(root.resolve())
  except ValueError:
    fail(f"{context} escapes the repository: {relative}")
  return resolved


def validate_schema(root: Path, manifest: dict[str, object]) -> None:
  exact_keys(manifest, {"schema_version", "schema_path", "software", "families"}, "manifest")
  if manifest.get("schema_version") != SCHEMA_VERSION:
    fail(f"schema_version must be {SCHEMA_VERSION}")
  if manifest.get("schema_path") != SCHEMA_RELATIVE_PATH:
    fail(f"schema_path must be {SCHEMA_RELATIVE_PATH}")
  schema = load_json(root / SCHEMA_RELATIVE_PATH)
  properties = object_field(schema, "properties", "schema")
  version_contract = object_field(properties, "schema_version", "schema.properties")
  if version_contract.get("const") != SCHEMA_VERSION:
    fail("schema and manifest disagree on schema_version")


def configured_software(root: Path) -> dict[str, str]:
  build = (root / "build.sbt").read_text(encoding="utf-8")
  properties = (root / "project" / "build.properties").read_text(encoding="utf-8")
  scala_match = re.search(r'ThisBuild\s*/\s*scalaVersion\s*:=\s*"([^"]+)"', build)
  gale_match = re.search(r'lazy val galeRevision\s*=\s*"([0-9a-f]{40})"', build)
  sbt_match = re.search(r"^sbt\.version=(\S+)$", properties, re.MULTILINE)
  if scala_match is None or gale_match is None or sbt_match is None:
    fail("cannot resolve Scala, sbt, and Gale versions from the build")
  return {
    "scala": scala_match.group(1),
    "sbt": sbt_match.group(1),
    "gale": gale_match.group(1),
  }


def validate_software(root: Path, manifest: dict[str, object]) -> None:
  software = object_field(manifest, "software", "manifest")
  expected_keys = {"r", "scala", "sbt", "gale"}
  exact_keys(software, expected_keys, "manifest.software")
  r = object_field(software, "r", "manifest.software")
  exact_keys(r, {"version", "packages"}, "manifest.software.r")
  r_version = string_field(r, "version", "manifest.software.r")
  if R_VERSION.fullmatch(r_version) is None or r.get("packages") != "base only":
    fail("manifest.software.r must record a semantic R version and base-only packages")
  configured = configured_software(root)
  for name in ("scala", "sbt"):
    section = object_field(software, name, "manifest.software")
    exact_keys(section, {"version"}, f"manifest.software.{name}")
    actual = string_field(section, "version", f"manifest.software.{name}")
    if actual != configured[name]:
      fail(f"manifest {name} version is {actual}, build declares {configured[name]}")
  gale = object_field(software, "gale", "manifest.software")
  exact_keys(gale, {"revision"}, "manifest.software.gale")
  revision = string_field(gale, "revision", "manifest.software.gale")
  if revision != configured["gale"]:
    fail(f"manifest Gale revision is {revision}, build declares {configured['gale']}")


def validate_estimand(family: dict[str, object], context: str) -> None:
  estimand = object_field(family, "estimand", context)
  exact_keys(
    estimand,
    {"name", "quantity", "normalization", "fold_reduction", "metadata"},
    f"{context}.estimand",
  )
  for name in ("name", "quantity", "normalization", "fold_reduction"):
    string_field(estimand, name, f"{context}.estimand")
  metadata = object_field(estimand, "metadata", f"{context}.estimand")
  if not metadata:
    fail(f"{context}.estimand.metadata must not be empty")
  for name, value in metadata.items():
    if not isinstance(name, str) or not name or not isinstance(value, (str, int, float, bool)):
      fail(f"{context}.estimand.metadata must contain named scalar values")
    if isinstance(value, float) and not math.isfinite(value):
      fail(f"{context}.estimand.metadata contains a non-finite number at {name}")


def validate_axes(family: dict[str, object], context: str) -> None:
  axes = array_field(family, "axes", context)
  names: set[str] = set()
  for position, value in enumerate(axes):
    axis_context = f"{context}.axes[{position}]"
    if not isinstance(value, dict):
      fail(f"{axis_context} must be an object")
    exact_keys(value, {"name", "role", "ordered_keys"}, axis_context)
    name = string_field(value, "name", axis_context)
    string_field(value, "role", axis_context)
    keys = array_field(value, "ordered_keys", axis_context)
    if name in names:
      fail(f"{context} repeats axis name {name}")
    names.add(name)
    if any(not isinstance(key, str) or not key for key in keys) or len(set(keys)) != len(keys):
      fail(f"{axis_context}.ordered_keys must be unique non-empty strings")


def validate_family(
    root: Path,
    family: dict[str, object],
    position: int,
    verify_digests: bool,
) -> str:
  context = f"manifest.families[{position}]"
  exact_keys(
    family,
    {"id", "estimand", "axes", "source_provenance", "generator", "output", "court"},
    context,
  )
  family_id = string_field(family, "id", context)
  if FAMILY_ID.fullmatch(family_id) is None:
    fail(f"{context}.id is not a clean estimand identifier: {family_id}")
  validate_estimand(family, context)
  validate_axes(family, context)

  provenance = object_field(family, "source_provenance", context)
  exact_keys(
    provenance,
    {"kind", "description", "authoritative_equations", "invocation"},
    f"{context}.source_provenance",
  )
  if provenance.get("kind") != "base-r":
    fail(f"{context}.source_provenance.kind must be base-r")
  string_field(provenance, "description", f"{context}.source_provenance")
  string_field(provenance, "authoritative_equations", f"{context}.source_provenance")

  generator = object_field(family, "generator", context)
  output = object_field(family, "output", context)
  exact_keys(generator, {"path", "sha256"}, f"{context}.generator")
  exact_keys(output, {"path", "sha256"}, f"{context}.output")
  generator_path = string_field(generator, "path", f"{context}.generator")
  output_path = string_field(output, "path", f"{context}.output")

  invocation = array_field(provenance, "invocation", f"{context}.source_provenance")
  claimed_invocation = ["Rscript", "--vanilla", generator_path]
  if invocation != claimed_invocation:
    fail(f"{context}.source_provenance.invocation must be {claimed_invocation}")

  expected = REQUIRED_FAMILIES.get(family_id)
  if expected is None:
    fail(f"unexpected retained MVPA oracle family {family_id}")
  expected_generator, expected_output, expected_court = expected
  if (generator_path, output_path) != (expected_generator, expected_output):
    fail(f"{family_id} generator/output mapping differs from the retained oracle contract")

  court = object_field(family, "court", context)
  exact_keys(court, {"shared_scala_suite", "platforms"}, f"{context}.court")
  suite = string_field(court, "shared_scala_suite", f"{context}.court")
  platforms = array_field(court, "platforms", f"{context}.court")
  if suite != expected_court or set(platforms) != {"jvm", "scala-js"} or len(platforms) != 2:
    fail(f"{family_id} must declare its exact shared JVM and Scala.js court")
  court_path = root / "modules" / "mvpa" / "shared" / "src" / "test" / "scala"
  court_path = court_path.joinpath(*suite.split(".")).with_suffix(".scala")
  if not court_path.is_file():
    fail(f"missing shared JVM/Scala.js court for {family_id}: {court_path.relative_to(root)}")

  for artifact_name, artifact, relative in (
      ("generator", generator, generator_path),
      ("output", output, output_path),
  ):
    path = safe_repository_path(root, relative, f"{context}.{artifact_name}.path")
    if not path.is_file():
      fail(f"missing {family_id} {artifact_name}: {relative}")
    digest = string_field(artifact, "sha256", f"{context}.{artifact_name}")
    if HEX_DIGEST.fullmatch(digest) is None:
      fail(f"{context}.{artifact_name}.sha256 must be lowercase SHA-256")
    if verify_digests:
      actual = sha256(path)
      if digest != actual:
        fail(f"stale {family_id} {artifact_name}: recorded {digest}, actual {actual}")

  output_text = (root / output_path).read_text(encoding="utf-8")
  lowered_output = output_text.lower()
  if "r version" in lowered_output or "getrversion" in lowered_output:
    fail(f"{family_id} output embeds an R runtime version instead of remaining version-neutral")
  return family_id


def validate_manifest(
    root: Path,
    manifest: dict[str, object],
    *,
    verify_digests: bool = True,
) -> None:
  validate_schema(root, manifest)
  validate_software(root, manifest)
  families = array_field(manifest, "families", "manifest")
  observed: list[str] = []
  for position, family in enumerate(families):
    if not isinstance(family, dict):
      fail(f"manifest.families[{position}] must be an object")
    observed.append(validate_family(root, family, position, verify_digests))
  if len(observed) != len(set(observed)):
    fail("manifest contains duplicate oracle family identifiers")
  if observed != list(REQUIRED_FAMILIES):
    fail("manifest oracle families must use the deterministic retained-family order")
  if set(observed) != set(REQUIRED_FAMILIES):
    missing = sorted(set(REQUIRED_FAMILIES) - set(observed))
    extra = sorted(set(observed) - set(REQUIRED_FAMILIES))
    fail(f"retained MVPA oracle audit mismatch; missing={missing}, extra={extra}")


def update_receipts(root: Path, manifest: dict[str, object], r_version: str) -> None:
  software = object_field(manifest, "software", "manifest")
  r = object_field(software, "r", "manifest.software")
  r["version"] = r_version
  families = array_field(manifest, "families", "manifest")
  for position, family in enumerate(families):
    if not isinstance(family, dict):
      fail(f"manifest.families[{position}] must be an object")
    for artifact_name in ("generator", "output"):
      artifact = object_field(family, artifact_name, f"manifest.families[{position}]")
      relative = string_field(artifact, "path", artifact_name)
      artifact["sha256"] = sha256(safe_repository_path(root, relative, artifact_name))


def detected_r_version(root: Path) -> str:
  completed = subprocess.run(
    ["Rscript", "--vanilla", "-e", "cat(as.character(getRversion()))"],
    cwd=root,
    env={**os.environ, "LC_ALL": "C", "LANG": "C"},
    capture_output=True,
    check=False,
    text=True,
  )
  if completed.returncode != 0:
    fail(f"cannot obtain the R version: {completed.stderr.strip()}")
  version = completed.stdout.strip()
  if R_VERSION.fullmatch(version) is None:
    fail(f"R reported an invalid version: {version}")
  return version


def regenerate(root: Path, manifest_path: Path, manifest: dict[str, object]) -> None:
  validate_manifest(root, manifest, verify_digests=False)
  families = array_field(manifest, "families", "manifest")
  environment = {**os.environ, "LC_ALL": "C", "LANG": "C", "TZ": "UTC"}
  for position, family in enumerate(families):
    assert isinstance(family, dict)
    provenance = object_field(family, "source_provenance", f"manifest.families[{position}]")
    invocation = array_field(provenance, "invocation", "source_provenance")
    command = [str(part) for part in invocation]
    completed = subprocess.run(
      command,
      cwd=root,
      env=environment,
      capture_output=True,
      check=False,
    )
    family_id = string_field(family, "id", f"manifest.families[{position}]")
    if completed.returncode != 0:
      detail = completed.stderr.decode("utf-8", errors="replace").strip()
      fail(f"{family_id} claimed R source failed: {detail}")
    if not completed.stdout:
      fail(f"{family_id} claimed R source emitted an empty fixture")
    output = object_field(family, "output", f"manifest.families[{position}]")
    output_path = safe_repository_path(
      root,
      string_field(output, "path", f"manifest.families[{position}].output"),
      f"manifest.families[{position}].output.path",
    )
    output_path.write_bytes(completed.stdout)

  update_receipts(root, manifest, detected_r_version(root))
  manifest_path.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
  validate_manifest(root, manifest)


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument(
    "--manifest",
    type=Path,
    default=MANIFEST_PATH,
    help="manifest to check (defaults to the committed v1 manifest)",
  )
  parser.add_argument(
    "--regenerate",
    action="store_true",
    help="invoke every claimed R source and refresh outputs plus digests",
  )
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  manifest_path = args.manifest.resolve()
  root = REPO_ROOT if manifest_path == MANIFEST_PATH else manifest_path.parents[2]
  try:
    manifest = load_json(manifest_path)
    if args.regenerate:
      regenerate(root, manifest_path, copy.deepcopy(manifest))
      print(f"regenerated and verified {len(REQUIRED_FAMILIES)} MVPA oracle families")
    else:
      validate_manifest(root, manifest)
      print(f"verified {len(REQUIRED_FAMILIES)} fresh MVPA oracle families without R")
  except ManifestError as error:
    print(f"MVPA oracle freshness failed: {error}", file=sys.stderr)
    return 1
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
