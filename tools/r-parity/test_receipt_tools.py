#!/usr/bin/env python3
"""Regression tests for independent JSON and generated-Scala freshness checks."""

from __future__ import annotations

import json
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parent))
import receipt_tools


class ReceiptToolsSuite(unittest.TestCase):
  def setUp(self) -> None:
    self.temporary = tempfile.TemporaryDirectory()
    self.root = Path(self.temporary.name).resolve()
    self.lock = self.root / "tools" / "r-parity" / "reference-lock.json"
    self.fixture = self.root / "fixture.json"
    self.scala = self.root / "Fixture.scala"
    self.generator = self.root / "generator.R"
    self.lock.parent.mkdir(parents=True)
    self.lock.write_text(json.dumps({
      "schema_version": receipt_tools.LOCK_SCHEMA,
      "r": {"locale": "C", "packages": {}, "version": "4.5.1"},
      "python": {"packages": {}, "version": "3.11"},
    }))
    self.generator.write_text("# deterministic generator\n")
    self.scala.write_text("object Fixture\n")
    source = {
      "producer": "generator.R",
      "r_version": "4.5.1",
    }
    self.fixture.write_text(json.dumps({
      "schema_version": "test-receipt/v1",
      "inputs": {"x": [1.0, 2.0]},
      "outputs": {"y": [3.0]},
      "source": source,
      "receipt": {"source": source},
    }))

  def tearDown(self) -> None:
    self.temporary.cleanup()

  def finalize(self, *, check: bool) -> int:
    with (
      patch.object(receipt_tools, "REPO_ROOT", self.root),
      patch.object(receipt_tools, "LOCK_PATH", self.lock),
      patch.dict(os.environ, {"LC_ALL": "C", "LANG": "C"}),
    ):
      return receipt_tools.finalize_receipt(
        "fixture.json",
        "test-receipt/v1",
        "Fixture.scala",
        "fit_inference",
        check=check,
      )

  def test_json_and_scala_staleness_are_detected_independently(self) -> None:
    self.assertEqual(self.finalize(check=False), 0)
    current_json = self.fixture.read_text()
    current_scala = self.scala.read_text()
    self.assertEqual(
      json.loads(current_json)["receipt"]["comparison_policy"],
      receipt_tools.COMPARISON_POLICY,
    )
    self.assertEqual(self.finalize(check=True), 0)

    payload = json.loads(current_json)
    payload["receipt"]["comparison_policy"]["alignment"] = "best_lag"
    self.fixture.write_text(json.dumps(payload))
    with self.assertRaisesRegex(SystemExit, "receipt.comparison_policy"):
      self.finalize(check=True)

    payload = json.loads(current_json)
    payload["outputs"]["y"] = [4.0]
    self.fixture.write_text(json.dumps(payload))
    with self.assertRaisesRegex(SystemExit, "receipt.hashes.outputs_sha256"):
      self.finalize(check=True)

    self.fixture.write_text(current_json)
    self.scala.write_text(current_scala + "// stale\n")
    with self.assertRaisesRegex(SystemExit, "receipt.hashes.generated_scala_sha256"):
      self.finalize(check=True)


if __name__ == "__main__":
  unittest.main()
