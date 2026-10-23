"""Keep the six cross-provider workflow witnesses registered and scheduled."""

import hashlib
import json
from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parent))
from check_test_inventory import alias_targets, release_batches

ROOT = Path(__file__).resolve().parents[2]
# Scientific contracts at 5c7936ef6: relocation fields are excluded, while IDs,
# evidence claims, platforms, allowed statuses and every caveat remain bound.
CONTRACTS = {
    "transform.fmriprep-chain.v1": ("FmriprepChainScenarioSuite", "3a4ebbe431f9015d6ac4592ea4845e1996da9896ec9f21391a1a19e94399693c"),
    "transform.fmriprep-demo1-interior.v1": ("Demo1NativeScenarioSuite", "897a27a5d2fc22ef8f70f62fd36a46a7126a2f6fe47bf0cba7e2b281159ff785"),
    "transform.fsl-chain.v1": ("FslChainScenarioSuite", "569817050cf0e5e64f71559feaa53607390cf09d4425ce57878810b8b23462a4"),
    "transform.fsl-real-demo1-interior.v1": ("FslRealChainScenarioSuite", "bacefdeeeae944ff4be5c6de63dc4889a911c699d41cb8e126296b1ee5ef5734"),
    "surface.real-orig-to-fsaverage-to-fslr32k.v1": ("SurfaceRealChainScenarioSuite", "690635b3c0afedf8789f2d1e6bce24d5d0e4f86e2ef05b9f4f07f56bdc0f0b1f"),
    "surface.volume-to-template-mesh-chain.v1": ("SurfaceChainScenarioSuite", "7fa840359ef35a569d08f5412a648877bcd91387017b2a38c6cd91ce194483bf"),
}


class ScenarioRegistrationSuite(unittest.TestCase):
  def test_original_contracts_remain_registered_in_the_integration_court(self) -> None:
    manifest = json.loads((ROOT / "docs/scenarios/manifest.json").read_text())
    scenarios = {scenario["id"]: scenario for scenario in manifest["scenarios"]}
    for identifier, (name, digest) in CONTRACTS.items():
      with self.subTest(identifier=identifier):
        scenario = scenarios[identifier]
        self.assertEqual(scenario["suite_class"], f"scalafim.spike.scenarios.{name}")
        path = ROOT / scenario["suite_path"]
        self.assertEqual(path.relative_to(ROOT).as_posix(), f"modules/provider-spike/shared/src/test/scala/scalafim/spike/scenarios/{name}.scala")
        self.assertIn(identifier, path.read_text())
        self.assertEqual(scenario["acceptance_commands"], ["sbt providerSpikeJVM/test", "sbt providerSpikeJS/test"])
        contract = {key: scenario[key] for key in ("id", "tier", "public_seam", "platforms", "allowed_statuses", "caveats")}
        contract["reference"] = {key: value for key, value in scenario["reference"].items() if key != "source_paths"}
        self.assertEqual(hashlib.sha256(json.dumps(contract, sort_keys=True, separators=(",", ":")).encode()).hexdigest(), digest)

  def test_both_platform_courts_are_in_the_full_release_batches(self) -> None:
    build = (ROOT / "build.sbt").read_text()
    targets = alias_targets(build, "scalafimTestAll")
    batches = release_batches((ROOT / "tools/ci/full-repository-tests.sh").read_text())
    scheduled = [target for batch in batches.values() for target in batch]
    for platform in ("JVM", "JS"):
      target = f"providerSpike{platform}/test"
      self.assertIn(target, targets)
      self.assertEqual(scheduled.count(target), 1)

  def test_real_surface_reference_sources_are_retained(self) -> None:
    manifest = json.loads((ROOT / "docs/scenarios/manifest.json").read_text())
    scenario = next(entry for entry in manifest["scenarios"] if entry["id"] == "surface.real-orig-to-fsaverage-to-fslr32k.v1")
    for path in scenario["reference"]["source_paths"]:
      self.assertTrue((ROOT / path).is_file(), path)


if __name__ == "__main__":
  unittest.main()
