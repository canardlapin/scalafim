# Main reconciliation: exact CI diagnosis — 2026-10-06

## Historical failures retrieved

Canonical `tools/github/gh-repo` authentication now verifies `canardlapin`, with
repository admin permissions. Authenticated logs identify these failures:

| Run | Full compile | Full and focused tests | Coverage |
| --- | --- | --- | --- |
| [PR 20, run 37379226968](https://github.com/canardlapin/scalafim/actions/runs/37379226968) | Passed | `CompactConditionRuntimeSuite` JS synthetic cohort admitted 21/24 | Passed |
| [PR 21, run 37383711609](https://github.com/canardlapin/scalafim/actions/runs/37383711609) | Passed | Same JS admission failure | `ParallelBlockExecutorSuite` line 129 thread-death assertion failed |

The compact law requires at least 90% admission, which means 22 of this unchanged
24-voxel cohort. Its accuracy and work limits were not changed. PR 21's coverage
job failed during `fitJVM/test`, before the fit coverage report; this was not a
coverage floor failure. Earlier unauthenticated 401/403 records remain retained
as historical observations and are superseded by the authenticated retrieval.

## Native lifecycle test repair

The four lifecycle checks now join distinct captured owned threads within one
five-second deadline, then still require every captured thread to be dead.
`ThreadPoolExecutor` can report pool termination before its exiting worker has
returned from `Thread.run`, so the previous immediate `isAlive` probes raced that
final return. The worker-failure fixture also waits until its second worker is
inside the interruption-catching region before triggering the first failure.
Production executor code is unchanged.

The repaired suite passed 10 ordinary repetitions and five coverage-instrumented
repetitions: **150 test executions, zero failed tests or test commands**. The
runner's last optional settings reset exhausted its 3 GB server during settings
reapplication and returned exit 1 after all 15 test commands succeeded. That
attempt, its GC warnings and disconnection are retained. Subsequent task-owned
server shutdowns returned exit 0. No coverage floor was changed or evaluated by
this subset.

## Current compact-law reproduction

The compact law, decoder, native lifecycle law and production executor were
byte-identical between both historical heads and inspected handoff `bc981ddf`.
`CompactCondition.scala` has later summary/readout refactoring, whose numerical
projection and decoding expressions are preserved. Detailed hashes are in
[historical-source-comparison.json](historical-source-comparison.json).

The isolated current-source reproduction used committed provider pins:

| Scope | Runtime | Tests | Admitted |
| --- | --- | ---: | ---: |
| Compact law, JVM | macOS ARM64, Temurin 21.0.12.1+1 | 3 passed | 24/24 |
| Compact law, Scala.js | macOS ARM64, verified Node 24.21.0 | 3 passed | 22/24 |
| Native lifecycle, before repair | macOS ARM64, Temurin 21 | 10 passed | — |

CI used Ubuntu 24.04 x64, Temurin 17.0.20-1 and Node 24.21.0. The matching Node
binary was downloaded into TMP from the official Node distribution and checked
against its published SHA256; global Node remains 24.1.0.

The local JS pass establishes the declared local scope. It does not establish a
fix for the historical Linux/x64 result of 21/24. TMP-only post-fit diagnostics
recorded refusals at voxels 4 and 15, both `CandidateAttemptCap`: their full Newton
corrections exceeded the unchanged `1e-9` stationarity limit; predicted gains were
below half an ULP, while evaluated candidate energies increased by two and one
ULPs. Candidate full-jet gradients were near zero. These observations do not
distinguish loss subtraction error from a true uphill move of the frozen double
design; that high-precision calculation was not performed. The decoder policy
is unchanged. The archived `UnresolvableDecrease` policy was not adopted: it
relaxes stationarity and depends on additive energy offsets.

## Withdrawn initial workflow proposal

The initial proposal added provider preparation to focused and coverage jobs.
Dependency inspection and authenticated logs disproved its necessity for the
declared first-level court: those paths do not reach Multivar's unpublished
provider artifacts. The extra work was withdrawn. The workflow now exactly
matches `bc981ddf`; its existing full-repository preparation remains intact.
The original proposal and receipt are retained as
[withdrawn-bootstrap-proposal.patch](withdrawn-bootstrap-proposal.patch) and
[initial-withdrawn-proposal.json](initial-withdrawn-proposal.json).

## Evidence and remaining gate

[receipt.json](receipt.json) records hashes and precise scope. The compressed
archive retains authenticated raw logs, cleaned views, source comparisons,
Node download metadata, local gate logs and receipts, runners, the tested native
fixture, and TMP-only diagnostic source. Verify without sbt or network access:

```sh
python3 -S docs/verification/main-reconciliation-readiness-20261006/verify.py
```

This agent performed no push, PR or main merge. Publication Mote
`bd-01M3ZGFS1WNA76Q1HJXN5BTE60` remains pending publication and exact-candidate
GitHub checks; local passes cannot substitute for that Linux CI gate.
