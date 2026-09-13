from __future__ import annotations

import hashlib
import json
from pathlib import Path
import sys
import tempfile
import unittest


sys.path.insert(0, str(Path(__file__).resolve().parent))
from validate_manifest import validate_locked_environment  # noqa: E402


class ReferenceLockSuite(unittest.TestCase):
  def setUp(self) -> None:
    self.temporary = tempfile.TemporaryDirectory()
    self.root = Path(self.temporary.name)
    (self.root / "tools" / "r-parity").mkdir(parents=True)
    self.generator = self.root / "tools" / "generator.R"
    self.generator.write_text("# independent generator\n")

  def tearDown(self) -> None:
    self.temporary.cleanup()

  def write_lock(self, name: str, revision: str = "locked-revision") -> Path:
    path = self.root / "tools" / "r-parity" / name
    path.write_text(json.dumps({
      "schema_version": "scalafim-parity-environment-lock/v1",
      "r": {
        "locale": "C",
        "version": "4.5.1",
        "packages": {
          "fmrihrf": {
            "revision": revision,
            "version": "0.4.0",
          }
        },
      },
    }))
    return path

  def payload(self, lock: Path, revision: str = "locked-revision") -> dict[str, object]:
    source = {
      "producer": "tools/generator.R",
      "r_version": "4.5.1",
      "fmrihrf_revision": revision,
      "fmrihrf_version": "0.4.0",
    }
    return {
      "schema_version": "scalafim-r-design-fixture/v1",
      "source": source,
      "receipt": {
        "environment": {
          "generator_sha256": hashlib.sha256(self.generator.read_bytes()).hexdigest(),
          "locale": "C",
          "lock_path": lock.relative_to(self.root).as_posix(),
          "lock_sha256": hashlib.sha256(lock.read_bytes()).hexdigest(),
          "runtime": "R 4.5.1",
        }
      },
    }

  def validate(self, payload: dict[str, object]) -> list[str]:
    errors: list[str] = []
    validate_locked_environment(Path("fixture.json"), payload, errors, self.root)
    return errors

  def test_default_reference_lock_passes(self) -> None:
    lock = self.write_lock("reference-lock.json")
    self.assertEqual(self.validate(self.payload(lock)), [])

  def test_declared_mixed_tr_reference_lock_passes(self) -> None:
    lock = self.write_lock("mixed-tr-reference-lock.json")
    self.assertEqual(self.validate(self.payload(lock)), [])

  def test_wrong_lock_sha_fails(self) -> None:
    lock = self.write_lock("reference-lock.json")
    payload = self.payload(lock)
    payload["receipt"]["environment"]["lock_sha256"] = "0" * 64  # type: ignore[index]
    self.assertTrue(any("lock_sha256 is stale" in error for error in self.validate(payload)))

  def test_wrong_package_revision_fails(self) -> None:
    lock = self.write_lock("reference-lock.json")
    errors = self.validate(self.payload(lock, revision="wrong-revision"))
    self.assertTrue(any("fmrihrf_revision disagrees" in error for error in errors))

  def test_missing_lock_fails(self) -> None:
    lock = self.root / "tools" / "r-parity" / "reference-lock.json"
    payload = self.payload(self.write_lock("reference-lock.json"))
    lock.unlink()
    self.assertTrue(any("reference lock does not exist" in error for error in self.validate(payload)))

  def test_path_escape_fails(self) -> None:
    outside = self.root / "outside.json"
    outside.write_text("{}")
    lock = self.write_lock("reference-lock.json")
    payload = self.payload(lock)
    payload["receipt"]["environment"]["lock_path"] = "../outside.json"  # type: ignore[index]
    self.assertTrue(any("escapes the repository" in error for error in self.validate(payload)))


if __name__ == "__main__":
  unittest.main()
