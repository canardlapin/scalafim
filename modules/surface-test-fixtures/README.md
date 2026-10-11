# Surface test fixtures

Unpublished support for the JVM fsLR qualification courts and atlas consumer tests. The module owns locked TemplateFlow asset readers, the independent SimpleITK inverse oracle and its original bytes, synthetic point-map inputs, the portable synthetic SimpleITK oracle, and the export/allocation probe helpers. It depends on `surface` Compile and `scenario-testkit`; `atlasJVM` Test consumes its Compile configuration. No production module depends on this module.

The asset-dependent surface qualification suites and the synthetic point-map reader suite live here to keep the project graph acyclic. Ordinary surface unit tests remain in `surface`. The Scala.js surface court reads the same oracle files from this module's JVM resource directory. The fixture module has no JS IO implementation.

```sh
python3 tools/build/sbt-warm surfaceJVM/test surfaceTestFixturesJVM/test atlasJVM/test
python3 tools/build/sbt-warm surfaceJS/test surfaceTestFixturesJS/test atlasJS/test
```

Set `SCALAFIM_REQUIRE_REAL_ASSETS=1` for a required real-asset court; absence then fails. Otherwise unavailable assets remain explicit skips. `SCALAFIM_JS_REAL_TIMING=1` enables the existing real JS timing court. No numerical, anatomical or resource threshold changed during extraction.

Run the export helper through its new owning module:

```sh
python3 tools/build/sbt-warm 'surfaceTestFixturesJVM/runMain scalafim.surface.reference.FslrQualification <spec.json> <out>'
```

The whole-repository compile/test aliases and bounded CI inventory include both fixture platforms.
