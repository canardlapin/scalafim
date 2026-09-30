# Owning CPU nearest ties and historical consumer pin

Issue: `bd-01M37FQGV8ZPT30X37TA4MM8R7`. Actor: `backlog-worker-20260930`.
Branch: `surface/cpu-tie-regressions-20260930`. Base: `79b8b7d5e7e9b52890b395bdd37674e1f068937c`.

## Result and scope

Closes review note N2 for the two owning CPU suites: three absolute nearest-neighbour tests per kernel, one per axis. The previous cross-engine comparison could let both kernels agree on the same wrong tie policy. These tests name the expected voxel independently; neither the other engine nor the production rounding implementation supplies the expectation.

The historical pin incompatibility is reproduced on both JVM and Scala.js. The ordinary main-derived provider graph compiles and passes the affected module suites. This is library evidence for the tested main-derived source, not a rebuilt downstream consumer or a revised PLSNeuro provider manifest.

Only two shared test files and this receipt change. No production, build, GIFTI, affine-inverse or GPU source is edited. The broader issue remains open for its remaining work.

## Independent fixture

A non-cubic identity grid has dimensions 3 by 4 by 5. Each axis checks nine coordinates: just below, exactly at and just above -0.5; 0.5; and dimension - 0.5, using epsilon 1e-8. Explicit expected indices establish half-up rounding and asymmetric support boundaries: -0.5 selects zero, +0.5 selects one, and dimension - 0.5 is outside support. Thus all 27 cases per kernel run on JVM and Scala.js.

The spatial owner checks the named column in canonical Z-fast layout `(x * 4 + y) * 5 + z`, unit weight with explicit floating-point tolerance, and zero/one coverage. The eager owner samples a synthetic volume encoded independently as `x + 10*y + 100*z`; it checks literal voxel values, accepted counts, outside NaNs and the exact requested/outside/accepted tally 9/3/6. White and pial points coincide, and the white path avoids introducing another interpolation policy.

These tests establish the synthetic identity-grid rounding contract. They do not qualify equality of voxel choices across different inverse-affine implementations at exact oblique-world half ties (N1), or extreme-world-coordinate handling (N3).

## Gates and mutation evidence

Full module batches passed: surface/spatial JVM 169 + 227 = 396; Scala.js 135 + 202 = 337. Surface JVM suites ran sequentially because of the documented pre-existing TemplateSphereFilesSuite scheduling timeout. Assertions and the original 30-second deadline remain unchanged; no default-parallel-scheduling reliability claim is made.

After replacing the spatial unit-weight vector comparison with the repository-required explicit `assertEqualsDouble` tolerance, both final owning suites passed again: 18 surface and 10 spatial tests on each platform. Coordinate fixtures and production bytes did not change. CompileAll passed after production restoration, with no warning/error markers. Tests run in bounded batches, not a monolithic TestAll.

For mutation checks, both CPU kernels were changed simultaneously from `math.round` to `ceil(value - 0.5)`, retaining Long until bounds checks. The eager owner fails all three new axis tests (15 old tests pass); the spatial owner fails all three new axis tests (seven old tests, including cross-engine parity, pass). Each failure is an absolute expectation at -0.5. The consumer SurfaceProjectionNetworkSuite is not involved in either kill.

The first `all .../testOnly ...` invocation executed only the surface input task; the spatial input task was selected separately with both mutations reapplied. No claim that the first invocation ran both suites is made. Both source files were restored byte-for-byte in finally blocks, with SHA256 checks. CompileAll and final owning gates run on restored sources. Mutant failures are retained as expected failures, not reported as passing gates.

Toolchain in these logs: sbt 1.11.7, Scala 3.7.4, Homebrew Java 25.0.1; Scala.js tests use NodeJSEnv. Launch arguments include `-J-Xmx3g` and `-J-XX:ActiveProcessorCount=4`. Fourteen loaded provider checkouts are tracked-clean in the retained source freeze; their distinct revisions are recorded, without claiming a uniform provider pin.

## Historical pin

Exact source/build snapshot: ScalaFIM `7c3ff0a0edc92be0e2fe35c431d5b9e458c92ab4`, whose root build pins Gale `83cac90a678d1b8a31c590e0c1b8fc8bf3427161`. The original graph also loads a transitive Gale `d55fe2f97196a76ab7879e1a12f1e92403aeba06`; both loaded Gale checkouts are tracked-clean. No source/build override was supplied.

`all spatialJVM/compile spatialJS/compile` exits 1. Both fail at `GaleSpatialSupport.scala:37:16`: `builder.writeLinear(index, values(index))`, because `writeLinear` is not a member of the selected `gale.linalg.DMatBuilder`.

For space efficiency, the immutable archive includes exactly `build.sbt`, `project`, `modules` and `.jvmopts` from the historical commit, omitting documentation assets. This is a source/build snapshot, not a complete Git checkout. Archive SHA256: `6c6c92072ba60f3c6c65ad23433c5a9155417f487bcddb56b8d1fdfd6a20a453`. Snapshot and source receipt are retained under `/private/tmp/scalafim-consumer-pin-source-20260930`. Historical Intaglio unused-key warnings remain in the failed log; that run is not warning-clean.

The tested current base pins root Gale `18d24dbb5056122032b0278f8bad557a9bb1cf23` and loads a transitive Gale `099832ff15c8a4a8fcf3398c7b779fb4bbc12434`. Its ordinary spatial JVM/JS compilation and tests pass. The historical revision has not been repaired or silently repinned. PLSNeuro requires an explicit prepared provider directory, so this check cannot qualify a downstream provider selection, rebuild or pin update.

## Retained receipts

Raw logs and `.meta.json` sidecars are under `/private/tmp/scalafim-execution-20260929/logs`. The metadata records actual command, working directory, duration, child return code and exit status. Full raw warning/error/test markers were inspected.

| Raw log | Exit | Outcome | SHA256 |
|---|---:|---|---|
| `cpu-ties-jvm.log` | 0 | [info] Passed: Total 169, Failed 0, Errors 0, Passed 169; [info] Passed: Total 227, Failed 0, Errors 0, Passed 227 | `ed18bb336e7cef1015d46047230e948ab8f5e1ded056ec8638d70c10c8561bb7` |
| `cpu-ties-js.log` | 0 | [info] Passed: Total 135, Failed 0, Errors 0, Passed 135; [info] Passed: Total 202, Failed 0, Errors 0, Passed 202 | `3d37936ad1274eca5013d28e26ce266a7d00987d3c013c43ab076d09bb43c107` |
| `cpu-ties-half-down-mutant.log` | 1 | [error] Failed: Total 18, Failed 3, Errors 0, Passed 15 | `937ddebdca3c2cdd8e744a6d724e9b33afcd99a9d9c1d37f9aec65064414c281` |
| `cpu-ties-half-down-spatial.log` | 1 | [error] Failed: Total 10, Failed 3, Errors 0, Passed 7 | `23ffeb9e08cf0381e78cb0abfe49d591dcae984d649161d413eef9b8f87ff5fa` |
| `consumer-pin-7c3ff0a-compile.log` | 1 | spatial JVM and JS: DMatBuilder.writeLinear missing | `64b845375d8d1a24521d84c73cdf972967cc3dce4c6bdd7216488441c82fe95f` |
| `cpu-ties-compileall.log` | 0 | warning-clean CompileAll | `01a86cf53ebaa3292c0a318bf2f29b2fb21ff2aa1e6835fed536ca29d1df44d8` |
| `cpu-ties-final-owning-jvm.log` | 0 | [info] Passed: Total 18, Failed 0, Errors 0, Passed 18; [info] Passed: Total 10, Failed 0, Errors 0, Passed 10 | `a0d75d5a501b0b845fe8fcbe618a669485a07b06a0d985f8640f32a91eeea2f5` |
| `cpu-ties-final-owning-js.log` | 0 | [info] Passed: Total 18, Failed 0, Errors 0, Passed 18; [info] Passed: Total 10, Failed 0, Errors 0, Passed 10 | `d276804df6b18e12afc7b1ff14f6aebb86b71c46224de1ffc425406a865cce63` |

Final tested test-source SHA256:

- `modules/spatial/shared/src/test/scala/scalafim/spatial/VolumeToSurfaceParitySuite.scala`: `f3b27c4701259d12b6dc5d8cade32bd714098cc0008c769d6aa146de813c713a`
- `modules/surface/shared/src/test/scala/scalafim/surface/SurfaceSamplingSuite.scala`: `753ab1c8d2bb43b08fa283b121d6b78d4254c69b0f21a043a49a657f882dfb93`

Manifest: `/private/tmp/scalafim-cpu-ties-20260930/qualification-manifest.json`, SHA256 `b1cb7aca3d523d3b2c1f16a11902ed8525226736d17b19cc12ebdb476ff97aeb`. It binds exact base, source/build hashes, loaded provider heads and tracked state, the historical archive, full/final gates, expected mutation failures and byte-exact restoration receipts. Branch commit identity is supplied by the Mote candidate and Fray handoff after committing this receipt.

## Remaining boundary

No scientific fsLR/group-space admission, broad engine consolidation, affine tie reconciliation, extreme-world overflow repair, GIFTI frame metadata closure or downstream consumer qualification is claimed. Those remain separate parts of the open parent work.
