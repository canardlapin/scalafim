"""Exercise default-graph admission without network or an sbt installation."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
# Independent fixture graph from the admitted providers, not parsed from the
# implementation's edge loop. Distinct revisions detect swapped expected pins.
DEPENDENCIES = {
    "multivar": ("gale",),
    "alder": ("gale", "resample4s"),
    "graph4s": ("gale",),
    "image4s": ("gale", "ravel", "locus4s", "intaglio", "spatial4s"),
    "reframe4s": ("gale", "ravel", "image4s", "spatial4s"),
    "mvpa4s": ("gale", "resample4s", "locus4s", "multivar", "alder", "neuroatlas4s", "neurosurf4s"),
    "mesh4s": ("graph4s", "locus4s", "spatial4s"),
    "neuroim4s": ("graph4s", "gale", "image4s", "reframe4s", "intaglio", "spatial4s"),
    "neuroatlas4s": ("neuroim4s", "neurosurf4s", "locus4s"),
    "neurosurf4s": ("mesh4s", "neuroim4s", "image4s", "reframe4s", "locus4s", "graph4s", "zarr4s", "intaglio", "spatial4s"),
}
PINS = {name: hashlib.sha256(name.encode()).hexdigest()[:40]
        for name in set(DEPENDENCIES) | {dep for deps in DEPENDENCIES.values() for dep in deps}}

FAKE_GIT = r'''#!/usr/bin/env python3
import json
import os
from pathlib import Path
import sys
fixture = json.loads(Path(os.environ["PROVIDER_FIXTURE"]).read_text())
if sys.argv[1] == "clone":
    repo = Path(sys.argv[-1])
    repo.mkdir()
    (repo / "tools").mkdir()
    for provider in ("gale", "resample4s"):
        script = repo / "tools" / ("publish-" + provider + "-local.sh")
        script.write_text('#!/bin/sh\necho ' + provider + ' >> "$PROVIDER_CALL_LOG"\n')
        script.chmod(0o755)
elif sys.argv[1] == "-C":
    repo = Path(sys.argv[2])
    command = sys.argv[3:]
    if command[:1] == ["show"]:
        assert command == ["show", fixture["pins"][repo.name] + ":build.sbt"], command
        for index, (dependency, sha) in enumerate(fixture["graph"][repo.name].items()):
            if sha is None:
                continue
            # Both spellings occur in real providers; exercise them together.
            print('lazy val ' + dependency + 'Revision =' + ('\n  ' if index % 2 else ' ') + '"' + sha + '"')
    elif command == ["rev-parse", "HEAD"]:
        print("f" * 40 if fixture["wrong_head"] else fixture["pins"][repo.name])
    elif command[:3] != ["checkout", "--quiet", "--detach"]:
        raise SystemExit("unexpected Git command: " + repr(command))
else:
    raise SystemExit("unexpected Git arguments: " + repr(sys.argv))
'''


class PinnedDependenciesTest(unittest.TestCase):
    def run_bootstrap(self, mutation=None, root_mutation=None, check_only=False, wrong_head=False):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "tools").mkdir()
            shutil.copyfile(ROOT / "tools/prepare-pinned-dependencies.sh", root / "tools/prepare-pinned-dependencies.sh")
            pins = dict(PINS)
            graph = {consumer: {dep: PINS[dep] for dep in deps}
                     for consumer, deps in DEPENDENCIES.items()}
            if mutation:
                consumer, dependency, value = mutation
                graph[consumer][dependency] = value
            if root_mutation:
                dependency, value = root_mutation
                pins[dependency] = value
            # Root has no independent spatial4s revision: image4s anchors it.
            (root / "build.sbt").write_text("".join(
                f'lazy val {name}Revision = "{sha}"\n'
                for name, sha in pins.items() if name != "spatial4s"))
            fixture = root / "fixture.json"
            fixture.write_text(json.dumps({"pins": pins, "graph": graph, "wrong_head": wrong_head}))
            executable = root / "git"
            executable.write_text(FAKE_GIT)
            executable.chmod(0o755)
            calls = root / "calls"
            command = ["sh", str(root / "tools/prepare-pinned-dependencies.sh")]
            if check_only:
                command.append("--check-only")
            result = subprocess.run(command, capture_output=True, text=True, env=dict(
                os.environ, PATH=str(root) + os.pathsep + os.environ["PATH"],
                TMPDIR=str(root), PROVIDER_FIXTURE=str(fixture), PROVIDER_CALL_LOG=str(calls)))
            return result, calls.read_text().splitlines() if calls.exists() else []

    def test_distinct_aligned_revisions_publish_both_providers(self):
        result, calls = self.run_bootstrap()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(calls, ["gale", "resample4s"])

    def test_check_only_verifies_graph_without_publishing(self):
        result, calls = self.run_bootstrap(check_only=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("40 shared edges agree", result.stdout)
        self.assertEqual(calls, [])

    def test_each_missing_or_mismatched_edge_refuses_before_either_publication(self):
        for consumer, dependencies in DEPENDENCIES.items():
            for dependency in dependencies:
                for invalid in (None, "f" * 40):
                    with self.subTest(consumer=consumer, dependency=dependency, invalid=invalid):
                        result, calls = self.run_bootstrap(mutation=(consumer, dependency, invalid))
                        self.assertNotEqual(result.returncode, 0)
                        self.assertEqual(calls, [])
                        if consumer == "image4s" and dependency == "spatial4s":
                            # A valid changed anchor is refused by the other
                            # coordinate consumers, rather than self-compared.
                            self.assertIn("spatial4s", result.stderr)
                        else:
                            self.assertIn(f"{consumer}=", result.stderr)
                            self.assertIn("revision mismatch", result.stderr)

    def test_invalid_root_pins_refuse_before_publishing(self):
        for dependency in ("multivar", "gale", "neurosurf4s", "intaglio"):
            for invalid in ("", "b" * 12, "g" * 40):
                with self.subTest(dependency=dependency, invalid=invalid):
                    result, calls = self.run_bootstrap(root_mutation=(dependency, invalid))
                    self.assertNotEqual(result.returncode, 0)
                    self.assertEqual(calls, [])

    def test_wrong_multivar_head_refuses_before_publishing(self):
        result, calls = self.run_bootstrap(wrong_head=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(calls, [])


if __name__ == "__main__":
    unittest.main()
