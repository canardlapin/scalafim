# Module simplification, 2026-10-19

Baseline: `eco/u2-pins` at `747282570b1132486968845f2baefdce3658b960`.
The hygiene change is `933fc6c23`; the commit containing this receipt carries
the two module merges. The local working branch is `simplify/phase-a-20261019`.

## Changes

The build declares 49 modules, down from 51: `hrf-laws` is absorbed by
`first-level-laws`, and `fit-estimates` by `fmri-workflow`. Packages and
scientific implementations stay the same. [moves.json](moves.json) records
original and final hashes for all 13 moved Scala files. Two files needed only
unused-import/pattern cleanup under the destination's existing strict compiler
policy; no compiler gate was relaxed. CI batches and current documentation use
the surviving projects. `estimates-io` stays test-only for workflow readback.

The hygiene change removes unused HRF/design dependencies, unused Multivar
project references, wrong or redundant project edges, the unreferenced
separable filters and R-name facade, stale removed-type/file scans, duplicate
all-build aliases, the duplicate frame-erasure scan, and the Mill experiment.
MVPA spatial predictive tests explicitly depend on the predictive provider;
MVPA artifact codecs explicitly depend on `ujson`. The image boundary still
enforces frame erasure and provider-state ownership.

## Verification

All eight affected modules passed their complete test targets on JVM and
Scala.js: `hrf`, `design`, `image`, `mvpaSpatial`, `mvpaArtifacts`,
`fmriWorkflow`, `firstLevelLaws`, and `phrfComparison`. The 16 targets report
3,957 passes and six existing opt-in skips. `scalafimCompileAll` passed across
the repository on both platforms. This is affected-module test evidence and
repository compilation; the full repository test suite and hosted CI were not run.

[gates.json](gates.json) records each result, dependency classpath checks, log
hashes, and all attempts. Full raw logs and command metadata are preserved under
the common Git directory at `verification/module-simplification-20261019/`.
The worktree sbt server was stopped after the final gate. There was an unexplained
wall-clock gap during the law JS run; timing observations are retained with the
logs, and these durations must not be used as performance evidence.

## Adverse attempts retained

- `01-jvm.log`: removing the MVPA dataset test edge exposed a missing predictive
  provider/Alder test dependency. The direct test dependency replaced it.
- `02-jvm-resume.log`: removing estimate IO exposed the artifact codecs' `ujson`
  dependency. It is now explicit.
- `04-merges-jvm-and-core-js.log` and `05-merges-jvm-and-core-js.log`: the stricter
  law destination rejected two unused imports and one unused pattern variable.
  These were corrected without changing the policy or assertions.
- `06-merges-jvm-and-core-js.log`: completed law/workflow JVM and HRF/design JS
  gates, then image JS reached sustained 98% garbage collection at the 6 GiB
  heap bound. Only the verified task-owned sbt PID was stopped. The unchanged
  image JS gate passed in a fresh server (`07-image-js-fresh.log`). Subsequent
  batches restart this worktree's server between runs.

## Recovered topology prerequisite

The exact unpublished mesh4s commit is
`54b7278880e6db38c543a4a565355afd864e576a`. It was recovered from the previous
agent's `scratchpad/u2/mesh4s` clone, after both the ordinary sibling repository
and GitHub lacked that object. GitHub's exact-SHA fetch returned `not our ref`.

It is now reachable in `~/code/scala/mesh4s` as
`recovery/eco-topology-packed-reads`. Its parent `3e8eb70` is the advertised
remote main; it also descends from the sibling's original local main `491bea6`.
A complete, verified Git bundle lives at the ScalaFIM common Git directory's
`mesh4s-topology-54b7278-recovery.bundle`. An isolated worktree is available at
`/private/tmp/scalafim-mesh4s-54b7278`.

The original uncommitted topology wiring remains in `scalafim-u2pins` and is
also preserved under the common Git directory at
`verification/module-simplification-20261019/pending-u3-wiring.patch`
(SHA-256 `758424db3c791120a0a968f3f55e46824f4b05e935e29a6c96d2edfb1c2d89fb`).
The temporary ignored census suite was deleted. Provider publication and the
surface topology cutover remain separate work.

## Remaining work

Neither simplification phase is complete. Phase A still has deprecated API,
`PrimitiveBuffers`, reader duplication, and public-definition cleanup. Phase B
still has the model merge with its coverage gate, the graphics split and
consumer coordination, provider-contract rename, and example consolidation.
The next topology step is the persisted scene/MVPA/atlas identity goldens,
followed by the mesh4s-backed composition and strict admission. Its full-suite
and independent review requirements remain in force.
