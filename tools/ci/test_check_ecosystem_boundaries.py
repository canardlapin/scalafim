from pathlib import Path
import sys
import tempfile
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parent))
from check_ecosystem_boundaries import (  # noqa: E402
    EXPECTED_GATES, inspect_classpaths, inspect_source,
)


class EcosystemBoundarySuite(unittest.TestCase):
    def test_duplicate_authorities_and_aliases_fail_but_literals_do_not(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "modules/example/shared/src/main/scala/Example.scala"
            source.parent.mkdir(parents=True)
            source.write_text('''package scalafim.example
// class TriangleMesh
/* outer /* class AxisRef */ comment */
val description = "class DomainRegistry"
val example = """type WorldSpace = String"""
opaque type `AxisRef` = String
final case class NeuroVolume(values: Vector[Double])
type TriangleMesh = mesh4s.Mesh
''')
            files, errors = inspect_source(root)
            self.assertEqual(len(files), 1)
            self.assertEqual(len(errors), 3)
            self.assertTrue(any("AxisRef" in error for error in errors))
            self.assertTrue(any("NeuroVolume" in error for error in errors))
            self.assertTrue(any("TriangleMesh" in error for error in errors))

    def test_retired_source_fails_but_ignored_build_output_is_not_source(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for name in ("modules/locus-data/shared/src/main/scala/Old.scala",
                         "modules/live/shared/src/main/scala/Live.scala",
                         "modules/live/target/generated/src/main/scala/Generated.scala"):
                source = root / name
                source.parent.mkdir(parents=True, exist_ok=True)
                source.write_text("package scalafim.example\nobject Example\n")
            files, errors = inspect_source(root)
            self.assertEqual(len(files), 2)
            self.assertEqual(len(errors), 1)
            self.assertIn("retired implementation module", errors[0])

    def gates(self):
        return {gate: ["/cache/scala3-library_3-3.7.4.jar"] for gate in EXPECTED_GATES}

    def check(self, gates):
        return inspect_classpaths(Path("/root/scalafim"), gates,
                                  ["https://github.com/canardlapin/locus4s.git#" + "a" * 40])

    def test_optional_surface_and_atlas_closures_allow_only_their_core(self):
        gates = self.gates()
        gates["mvpa-surface-JVM"] += [
            "/staging/neurosurf4s/modules/neurosurf4s-core/jvm/target/classes",
            "/staging/neuroim4s/modules/neuroim4s-spaces/jvm/target/classes",
        ]
        gates["mvpa-atlas-JS"] += ["/ivy/neuroatlas4s-core_sjs1_3-0.1.jar"]
        self.assertEqual(self.check(gates), [])
        gates["mvpa-atlas-JS"] += ["/ivy/neuroatlas4s-volume_sjs1_3-0.1.jar"]
        gates["mvpa-surface-JVM"] += [
            "/staging/neuroim4s/modules/neuroim4s-core/jvm/target/classes"]
        errors = self.check(gates)
        self.assertEqual(len(errors), 2)
        self.assertTrue(any("neuroatlas4s-volume" in error for error in errors))
        self.assertTrue(any("neuroim4s-core" in error for error in errors))

    def test_transitive_root_test_and_wrong_checkout_edges_fail(self):
        gates = self.gates()
        gates["volume-core-JVM"] += [
            "/root/scalafim/modules/fit/jvm/target/classes",
            "/work/scalafim-other/modules/dataset/jvm/target/classes",
            "/staging/provider/target/test-classes",
            "/staging/mvpa4s/modules/mvpa4s-testkit/jvm/target/classes",
        ]
        errors = self.check(gates)
        self.assertEqual(len(errors), 5)
        self.assertEqual(sum("backedge" in error for error in errors), 2)
        self.assertEqual(sum("Test dependency" in error for error in errors), 2)

    def test_missing_or_empty_platform_closure_is_not_qualification(self):
        gates = self.gates()
        del gates["spaces-JS"]
        gates["atlas-core-JVM"] = []
        errors = self.check(gates)
        self.assertEqual(len(errors), 2)
        self.assertTrue(any("spaces-JS" in error for error in errors))
        self.assertTrue(any("empty Compile" in error for error in errors))

    def test_two_loaded_revisions_of_one_provider_fail(self):
        builds = ["https://github.com/canardlapin/image4s.git#" + sha * 40
                  for sha in ("a", "b")]
        errors = inspect_classpaths(Path("/root/scalafim"), self.gates(), builds)
        self.assertEqual(len(errors), 1)
        self.assertIn("multiple loaded revisions of image4s", errors[0])


if __name__ == "__main__":
    unittest.main()
