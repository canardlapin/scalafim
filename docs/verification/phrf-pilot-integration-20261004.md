# PHRF branch recovery and canonical-main integration

Mote: `bd-01M41SAG4MA1DEZ2V6GQCAD89Z`.

## Candidate scope

The integration prerequisites and origin reconciliation were already present on
canonical main at `3f261adde7fb716cb997e290516fa49379121c81`. This candidate
starts from that revision, plus the CI-resolution receipt `4e3de63c`.

The retained PHRF branch is at `a18e32fb83fcb1051a06a26ae614582fa2f89689`.
Its repository has missing historical objects, so a complete merge/archive is
unavailable. Recovery uses the 313 explicitly scoped tracked files whose working
bytes match their HEAD Git blob IDs. The complete source hashes, modes, sizes,
and failed-archive boundary are recorded in
`phrf-pilot-integration-20261004/source-manifest.json`.

The recovered module is unpublished. Its portable preparation, scoring, and
runner contracts compile on JVM and JS; process custody and native GLMsingle
integration remain JVM-only. The build aggregates and bounded CI batches now
include the module. Existing `phrf-cmp-*` receipts describe historical author
qualification and are retained as source evidence, not new test results.

Recovery initially changed three retained implementation/test files: `RidgeLss.scala`
rejects group IDs outside the trial count before arithmetic/allocation, its
suite covers `Int.MaxValue` and another huge sparse ID, and the Python late-ack
test captures the actual spawned PID instead of racing a child-written file.
One trailing space is removed from the retained S8 Markdown receipt.

Two current-main CI defects are repaired without changing numerical tolerances:
the twelve first-level law files reported by the PR #19 formatting gate are
formatted with the repository formatter; the JVM parallel-fit semantic matrix
has a ten-minute MUnit deadline. It retains all 2,048 voxels, chunk sizes, worker
counts, and explicit lifecycle deadlines. Hosted Java 17 coverage recorded
212.856 and 196.048 seconds for its two matrices.

## Qualification results

Completed before the review candidate:

- JVM and JS compilation of the recovered PHRF module and test sources.
- `RidgeLssSuite` and `TrialHeldOutPredictionSuite`: 15 passed on each platform.
- First-level law formatting check, test-inventory check, scenario-manifest
  validation, and first-level documentation check.
- GLMsingle converter: 13 synthetic-fixture tests passed, including real native
  recomputation and deterministic archive checks.
- Python custody in the retained lock environment: 196 passed, no skips.
  The earlier 193-pass run skipped three BLS cases; the locked rerun supersedes it.

The bounded recovery candidate `bfa66bd5` received independent integration
approval subject to qualification. The final module gates below completed
successfully. Logs and actual process exit metadata are retained under
`/private/tmp/scalafim-mote-queue-evidence`.

One initial warm-server reload exhausted its three-GB heap while retaining two
large builds. That owned process was stopped before tests ran. Subsequent gates
use a fresh five-GB server; the cancelled reload is environment evidence, not
a numerical test failure.

## Java 17 numerical repair

The initial complete JVM comparison gate recorded 473 tests: 463 passed, nine
failed, one opt-in heavy test skipped. Seven failures were environment/version
selection or CPU-versus-wall assumptions. Explicit custody and generator
interpreter selection, a generator lock matching the committed manifests, and
an independently timed CPU burner repair those boundaries without weakening
assertions. The remaining two tests refused one exact-data voxel.

Temurin 17.0.20.1 reproduces those refusals. An independent original-time,
augmented-QR residual-plus-penalty calculation proves the current Newton step
lowers `J = E + sigma2 D` by `1.224e-13`; subtractive native energy reports an
increase. The independent determinant agrees within `2e-14`. The captured
experiment and log are `PhrfTrialTheorySuite-qr-oracle-experiment.scala` and
`phrf-qr-terminal-oracle-java17.log` in the evidence directory.

The repair evaluates native ML energy from prepared whitened sparse design
rows and recovered trial/nuisance coefficients using that evaluation's accepted
factor. Native readout evaluates its actual solved coefficients through the
same residual-plus-centred-penalty formula. Analytic derivatives, determinant
checks, sigma2, decode budgets and Accepted-only policy are retained.

Each active ML worker owns `2T + N + C` additional doubles. Its unused prepared
prototype allocates no such workspace. Setup declares this storage, provenance
identifies the energy policy, and ML receipts count response copies, residual
rows, sparse basis and nuisance visits, and coefficient-recovery products.
Full-jet evaluation's extra response solve appears in the existing solve ledger.
The encoded-only public objective keeps its existing ownership/storage contract.

The first repair passed all 15 unchanged PHRF theory tests on Java 17 and all 15
on Scala.js (`phrf-stable-energy-theory-java17.log`). Subsequent qualification
adds independent current/half/full-step QR ordering, native readout energy,
terminal Newton stationarity, caller-mutation isolation, worker isolation and
work/storage regressions. These focused gates passed on both platforms:
11 native-backend tests, 16 PHRF theory tests, and three compact-condition tests
per platform (`phrf-stable-energy-qualified-regressions.log`, exit 0). Native
and independent criteria differ by at most `2.8e-19` on the captured three-point
fixture, comfortably below its `1.224e-13` step decrease. No pilot timing claim
is made with the added work unmeasured.

The local compact-condition cohort met its 22-of-24 admission requirement but
took 89 seconds, exceeding the default 30-second MUnit timeout. Its ten-minute
suite bound retains every numerical assertion, the 24-voxel cohort, and all
per-voxel work caps. PR #19's Linux run admitted 21 of 24; that hosted result is
not superseded by a local macOS run.

## Full-gate follow-up

The first full Java 17 fit gate recorded 587 tests: 581 passed and six failed.
Five failed only the default 30-second MUnit deadline: the backend dense/finite-
difference oracle (82 s), status-allocation setup/check (53 s), original-time
readout oracle (69 s), conditional dense oracle (42 s), and constrained
determinant oracle (76 s). Those five suite bounds are now ten minutes. Their
numeric, allocation, solve and work limits are unchanged. The captured full log
has no GC or out-of-memory warning.

The remaining failure expected the complete ML work ledger to remain unchanged
through native readout. The declared energy policy now records one traversal:
one energy evaluation, T residual rows, the prepared sparse basis length, and
T*F nuisance visits. The updated expectation permits exactly those increments;
factor, solve, response-copy and coefficient-recovery increments remain zero.
Existing measurement/no-second-solve assertions remain, and both memoized
rereads must leave the complete ML ledger unchanged. Independent review
confirmed this test-only accounting adjustment. Full gates passed on the
resulting candidate before landing.

Independent read-only review approved the production repair at `723bc27d`
and its test-only follow-up at `4290c12659f5ea28a0c2367730c61aab6cb59c43`,
subject to full gates. The latter review verified all six changed files and
confirmed the exact charged traversal plus no-second-solve and memoized-reread
checks. Focused results are author-run; the reviewer inspected code and terminal
evidence rather than independently rerunning those tests.

The final rerun tested code revision
`4290c12659f5ea28a0c2367730c61aab6cb59c43` on macOS arm64 with Temurin
17.0.20.1. All commands exited zero:

| Target | Result |
| --- | --- |
| `firstLevelLawsJVM/Test/scalafmtCheck` | Passed |
| `fitJVM/test` | 587 passed |
| `fitJS/test` | 532 passed |
| `phrfComparisonJVM/test` | 473 passed, one opt-in heavy test skipped |
| `phrfComparisonJS/test` | 242 passed |
| `firstLevelLawsJVM/test` | 83 passed |
| `firstLevelLawsJS/test` | 83 passed |
| `scalafimCompileAll` | JVM and JS compilation passed |

The single skipped test is `RhoBiasHeavySuite`'s frozen bias criterion over
50 harness datasets. It remains outside this bounded landing. The raw log is
`phrf-integration-final-module-gates-java17-v2.log` (493,492 bytes, SHA-256
`5129c6f15e7cb0638101af374d6899dd88f4e57ac073db9d20e6b34fd63d0290`).
Its sidecar records exit zero and 4,475.59 seconds. The complete log has no
compiler warnings or errors; it contains one sbt multiple-main-class notice.
Only this receipt changed after the tested code revision. The owned warm
server was stopped after every gate completed, and the two owned synthetic
GLMsingle output files were removed.

The retained condition-milestone law reports
weak LWU admission below its 95% target; its reporting policy is unchanged.
Passing the law suite does not establish that unmet scientific target.

During the five-GB warm-server run, read-only diagnostics observed a full
optimized JIT code cache and 11 compilation stops/restarts. Compilation was
enabled at the snapshot. The heap contained about 2.8 GiB and short GC counter
samples were stable. The gate was not interrupted; these measurements do not
establish the cause of its longer fit-JVM duration or a performance result.
The thread, heap/GC and code-cache snapshots are retained in the evidence
directory as `full-gate-live-*.txt`.

## Explicit landing boundary

This work lands the retained S0–S9/S11 implementation and prerequisites. S10
(runner entry point, final `ScoreFeed` wiring, owner reader, freeze and custody
rehearsal) follows branch landing in the coordinator's accepted plan. No sealed
pilot outputs were inspected, no pilot was run, and no scientific pilot or
release qualification is claimed. No push is authorized.
