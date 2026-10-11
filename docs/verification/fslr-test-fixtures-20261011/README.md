# Shared fsLR fixtures — 2026-10-11

The atlas JVM tests now consume `surfaceTestFixturesJVM` Compile instead of `surfaceJVM` Test. The new unpublished cross-project owns the reusable JVM asset locks, inverse oracle, export/allocation helpers, point-map resources and portable synthetic oracle. Its dependent qualification courts moved with the fixtures, so the project graph remains acyclic: fixtures depend on surface Compile; atlas Test depends on fixtures Compile. Surface never depends on the fixture project.

The generic JVM `RealAssetGate` moved unchanged into the existing unpublished `scenario-testkit`, which now has a JVM Compile MUnit dependency. This lets ordinary surface file-reader tests retain their asset gate without depending on the domain fixture project. No production atlas classpath contains either test-support project.

`moves.json` records the original SHA-256 of every relocated file. `verify_moves.py` verifies unchanged resource bytes, unchanged portable oracle bytes, unchanged assertions/budgets, and only the declared gate-import/run-command/path edits. `move-verification.json` passes. The original scientific evidence and P1–P6 thresholds were not edited. The Scala.js real-placement court still reads exactly the same all-vertex oracle bytes, at their new location.

The root aggregates, whole-repository compile/test aliases and bounded CI runner include both fixture platforms. The CI inventory check passes with 99 supported targets in 16 batches. The oracle generator now points to the moved committed reference file.

Validation (`gates-2.log`), zero failures and no source compiler warnings:

| Target | Passed | Skipped |
| --- | ---: | ---: |
| surfaceJVM | 290 | 6 |
| surfaceTestFixturesJVM | 16 | 5 |
| atlasJVM | 121 | 9 |
| surfaceJS | 254 | 2 |
| surfaceTestFixturesJS | 7 | 0 |
| atlasJS | 84 | 0 |

Total: 772 passed, 22 explicit skips. The skipped cases include unavailable real assets and opt-in real timing/native measurements. This extraction supplies **no new real-asset qualification**. `SCALAFIM_REQUIRE_REAL_ASSETS=1` retains its fail-on-absence policy, and `SCALAFIM_JS_REAL_TIMING=1` retains the existing JS opt-in policy.

`dependency-scopes.log` records the actual resolved atlas Compile/Test internal classpaths and both fixture projects' publication skip settings. The initial failed build is retained: it exposed the synthetic oracle dependency of the moved file-reader court; moving the portable oracle and its two courts completed the extraction without a test-to-test edge or duplicated fixtures.

Run the owning courts and helpers with the commands in `modules/surface-test-fixtures/README.md`. The broader fsLR numerical/anatomical qualification Mote remains open.
