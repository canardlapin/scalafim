#!/usr/bin/env python3
"""Hostile freshness tests for the committed MVPA oracle receipt."""

from __future__ import annotations

import copy
from pathlib import Path
import shutil
import sys
import tempfile
import unittest


sys.path.insert(0, str(Path(__file__).resolve().parent))
import mvpa_oracle_manifest


class MvpaOracleManifestSuite(unittest.TestCase):
  def setUp(self) -> None:
    self.temporary = tempfile.TemporaryDirectory()
    self.root = Path(self.temporary.name).resolve()
    self.payload = copy.deepcopy(
      mvpa_oracle_manifest.load_json(mvpa_oracle_manifest.MANIFEST_PATH)
    )
    paths = {
      "build.sbt",
      "project/build.properties",
      mvpa_oracle_manifest.SCHEMA_RELATIVE_PATH,
    }
    for generator, output, court in mvpa_oracle_manifest.REQUIRED_FAMILIES.values():
      paths.add(generator)
      paths.add(output)
      court_path = Path("modules/mvpa/shared/src/test/scala").joinpath(*court.split("."))
      paths.add(str(court_path.with_suffix(".scala")))
    for relative in paths:
      destination = self.root / relative
      destination.parent.mkdir(parents=True, exist_ok=True)
      shutil.copy2(mvpa_oracle_manifest.REPO_ROOT / relative, destination)

  def tearDown(self) -> None:
    self.temporary.cleanup()

  def family(self, family_id: str) -> dict[str, object]:
    families = self.payload["families"]
    assert isinstance(families, list)
    for family in families:
      if isinstance(family, dict) and family.get("id") == family_id:
        return family
    self.fail(f"missing fixture family {family_id}")

  def validate(self) -> None:
    mvpa_oracle_manifest.validate_manifest(self.root, self.payload)

  def test_committed_manifest_is_fresh_without_r(self) -> None:
    self.validate()

  def test_output_tampering_is_rejected(self) -> None:
    family = self.family("crossvalidated-rdm")
    output = family["output"]
    assert isinstance(output, dict)
    path = self.root / str(output["path"])
    path.write_text(path.read_text(encoding="utf-8") + "// stale\n", encoding="utf-8")

    with self.assertRaisesRegex(mvpa_oracle_manifest.ManifestError, "stale crossvalidated-rdm output"):
      self.validate()

  def test_generator_tampering_is_rejected(self) -> None:
    family = self.family("canonical-effect")
    generator = family["generator"]
    assert isinstance(generator, dict)
    path = self.root / str(generator["path"])
    path.write_text(path.read_text(encoding="utf-8") + "# stale\n", encoding="utf-8")

    with self.assertRaisesRegex(mvpa_oracle_manifest.ManifestError, "stale canonical-effect generator"):
      self.validate()

  def test_missing_axes_are_rejected(self) -> None:
    family = self.family("manova")
    family["axes"] = []

    with self.assertRaisesRegex(mvpa_oracle_manifest.ManifestError, "axes must be a non-empty array"):
      self.validate()

  def test_missing_estimand_metadata_is_rejected(self) -> None:
    family = self.family("manova")
    estimand = family["estimand"]
    assert isinstance(estimand, dict)
    estimand["metadata"] = {}

    with self.assertRaisesRegex(mvpa_oracle_manifest.ManifestError, "metadata must not be empty"):
      self.validate()

  def test_unversioned_extra_fields_are_rejected(self) -> None:
    family = self.family("canonical-effect")
    family["legacy_alias"] = "canonical"

    with self.assertRaisesRegex(mvpa_oracle_manifest.ManifestError, r"extra=\['legacy_alias'\]"):
      self.validate()

  def test_claimed_invocation_must_execute_the_recorded_r_source(self) -> None:
    family = self.family("signed-cross-run-rayleigh")
    provenance = family["source_provenance"]
    assert isinstance(provenance, dict)
    provenance["invocation"] = ["Rscript", "different-generator.R"]

    with self.assertRaisesRegex(mvpa_oracle_manifest.ManifestError, "invocation must be"):
      self.validate()

  def test_runtime_versions_cannot_leak_into_fixture_outputs(self) -> None:
    family = self.family("cross-domain-classification")
    output = family["output"]
    assert isinstance(output, dict)
    path = self.root / str(output["path"])
    path.write_text("// R version 4.5.1\nobject Fixture\n", encoding="utf-8")
    output["sha256"] = mvpa_oracle_manifest.sha256(path)

    with self.assertRaisesRegex(mvpa_oracle_manifest.ManifestError, "version-neutral"):
      self.validate()


if __name__ == "__main__":
  unittest.main()
