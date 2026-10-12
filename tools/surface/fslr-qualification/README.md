# Locked real fsLR qualification

Use TemplateFlow4s commit `67097a9ecab7ea4ed8a81eb3532861096a3b2282` to acquire
the source assets and reproduce the admitted point map. From that checkout:

```sh
export TEMPLATEFLOW_HOME=/path/to/templateflow-cache
sbt 'templateflowCli/run fetch --lock /path/to/scalafim/tools/surface/fslr-qualification/assets.lock.json --json'
sbt 'templateflowTransforms/runMain templateflow.transforms.ConvertMain --lock /path/to/scalafim/tools/surface/fslr-qualification/assets.lock.json --archive-path tpl-MNI152NLin2009cAsym/tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5'
```

From ScalaFIM, verify every payload before running the real gates:

```sh
python3 tools/surface/fslr-qualification/verify_assets.py "$TEMPLATEFLOW_HOME"
SCALAFIM_REQUIRE_REAL_ASSETS=1 SCALAFIM_TIMING_BUDGETS=1 sbt \
  surfaceTestFixturesJVM/test \
  'atlasJVM/testOnly *Fslr32kRouteBudgetSuite *StandardSurfaceRouteFilesSuite'
SCALAFIM_REQUIRE_REAL_ASSETS=1 SCALAFIM_JS_REAL_TIMING=1 sbt \
  'surfaceJS/testOnly *RealPointwiseJsSuite' surfaceTestFixturesJS/test \
  'atlasJS/testOnly *StandardSurfaceRoutesSuite'
```

For repeated runs, `python3 tools/build/sbt-warm` accepts the same tasks. Shut
down a resident server before changing its environment flags. Acquisition
and conversion are successful only when the verifier accepts all 12 hashes;
CLI usage output alone is insufficient.

P4 needs each consumer's `scalafim.fslr-qualification-input/1` spec and original
source volumes. Run the public-route comparison for each spec:

```sh
sbt 'atlasJVM/Test/runMain scalafim.atlas.reference.FslrBridgeDisagreement pointwise /path/to/spec.json /path/to/new-report.json'
```

The [2026-10-12 packet](../../../docs/verification/fslr-real-qualification-20261012/README.md)
records the locked GM check and the still-missing historical 15-map consumer
replay. GM does not substitute for those consumer maps. Frozen budgets and
the publisher-methods anatomical declaration remain those in the
[pointwise-inverse contract](../../../docs/verification/fslr-pointwise-inverse-20261002.md).
