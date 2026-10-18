"""Exercise the dependency bootstrap without network or an sbt installation."""

import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
MULTIVAR = "a" * 40
GALE = "b" * 40
SOURCE_PROVIDERS = ("alder", "graph4s", "image4s", "mvpa4s", "reframe4s")


class PinnedDependenciesTest(unittest.TestCase):
    def run_bootstrap(self, local_gale, provider_gale, source_gale=None, mismatched=""):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "tools").mkdir()
            shutil.copyfile(ROOT / "tools/prepare-pinned-dependencies.sh", root / "tools/prepare-pinned-dependencies.sh")
            (root / "build.sbt").write_text(
                f'lazy val multivarRevision = "{MULTIVAR}"\n'
                f'lazy val galeRevision = "{local_gale}"\n'
                + "".join(f'lazy val {name}Revision = "{"d" * 40}"\n' for name in SOURCE_PROVIDERS)
            )
            executable = root / "git"
            executable.write_text('''#!/usr/bin/env python3
import os
from pathlib import Path
import sys
if sys.argv[1] == "clone":
    repo = Path(sys.argv[-1])
    (repo / "tools").mkdir(parents=True)
    revision = os.environ["PROVIDER_GALE"]
    (repo / "build.sbt").write_text('lazy val galeRevision = "' + revision + '"\\n' if revision else '')
    source = os.environ["SOURCE_GALE"] if repo.name != os.environ["MISMATCHED"] else "c" * 40
    # Alder spells its pin over two lines; the bootstrap must read both forms.
    pin = 'lazy val galeRevision =\\n  "' + source + '"\\n' if repo.name == "alder" else 'lazy val galeRevision = "' + source + '"\\n'
    (repo / "source-build.sbt").write_text(pin)
    for provider in ("gale", "resample4s"):
        script = repo / "tools" / ("publish-" + provider + "-local.sh")
        script.write_text('#!/bin/sh\\necho ' + provider + ' >> "$PROVIDER_CALL_LOG"\\n')
        script.chmod(0o755)
elif sys.argv[3:4] == ["show"] and sys.argv[4] == "d" * 40 + ":build.sbt":
    sys.stdout.write((Path(sys.argv[2]) / "source-build.sbt").read_text())
elif sys.argv[3:] == ["rev-parse", "HEAD"]:
    print(os.environ["PROVIDER_MULTIVAR"])
elif sys.argv[3:6] != ["checkout", "--quiet", "--detach"]:
    raise SystemExit("unexpected git invocation: " + repr(sys.argv))
''')
            executable.chmod(0o755)
            calls = root / "calls"
            result = subprocess.run(
                ["sh", str(root / "tools/prepare-pinned-dependencies.sh")],
                env=dict(os.environ, PATH=str(root) + os.pathsep + os.environ["PATH"],
                         TMPDIR=str(root), PROVIDER_GALE=provider_gale,
                         SOURCE_GALE=provider_gale if source_gale is None else source_gale,
                         MISMATCHED=mismatched,
                         PROVIDER_MULTIVAR=MULTIVAR, PROVIDER_CALL_LOG=str(calls)),
                capture_output=True, text=True,
            )
            return result, calls.read_text().splitlines() if calls.exists() else []

    def test_aligned_revisions_publish_both_providers(self):
        result, calls = self.run_bootstrap(GALE, GALE)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(calls, ["gale", "resample4s"])

    def test_mismatch_refuses_before_publishing(self):
        result, calls = self.run_bootstrap(GALE, "c" * 40)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Gale revision mismatch", result.stderr)
        self.assertEqual(calls, [])

    def test_source_provider_mismatch_refuses_before_publishing(self):
        for provider in SOURCE_PROVIDERS:
            with self.subTest(provider=provider):
                result, calls = self.run_bootstrap(GALE, GALE, mismatched=provider)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn(f"Gale revision mismatch: ScalaFIM={GALE} {provider}=" + "c" * 40, result.stderr)
                self.assertEqual(calls, [])

    def test_missing_source_provider_gale_refuses(self):
        result, calls = self.run_bootstrap(GALE, GALE, source_gale="")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(calls, [])

    def test_missing_provider_pin_refuses(self):
        result, calls = self.run_bootstrap(GALE, "")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(calls, [])

    def test_equal_but_truncated_pins_refuse(self):
        result, calls = self.run_bootstrap("b" * 12, "b" * 12)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(calls, [])


if __name__ == "__main__":
    unittest.main()
