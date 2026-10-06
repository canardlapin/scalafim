# Gale ExactSum → ScalaFIM AR rehearsal — 2026-10-06

The source-only consumer commit is `be7e6caa740b776ee71c811f306b51e9a84933ac`, based on
ScalaFIM `bc981ddf`. All gates use explicit local Gale candidate
`54e73f8e8f1218c4cb115a850aa80e1a36c6978f` through
`-Dscalafim.gale.build=/private/tmp/scalafim-exactsum-gale-20261006`.
Root source, pins and Motes were not modified.

The consumer imports `gale.numeric.ExactSum`, deletes the private generic sum
implementation and its generic suite, and maps typed capacity refusals at every
add/merge site into `ArError.NoiseSumCapacityExceeded`. Pair-count overflow still
takes precedence; existing nonfinite input and rounded-total validation remain.
The new domain regression self-merges a two-row summary sixty times, reaches the
upstream `2^60` finite-input bound, then verifies typed refusal and preservation
of source sums, counts, live exact values and finite-input counts.

| Accepted gate | Passing tests |
| --- | ---: |
| `arJVM/test` | 155 |
| `arJS/test` | 153 |
| Selected affected fit suites, JVM | 51 |
| Same fit suites, Scala.js | 51 |
| Total | 410 |

Selected fit suites are `PooledGlsPreparationSuite`,
`BoundedRobustPreparationSuite`, `GlsSuite` and `TrialReadoutSuite`. They cover
bit-exact chunked noise pooling, robust AR, native estimated/fixed GLS fixtures,
censor/reset behavior and readout regressions. No additional API adapter was
needed. All completed accepted gates have zero failures, errors and warnings.
The initial AR JVM gate before stronger live-value preservation assertions is
retained as development evidence, excluded from the accepted total.

The runtime was Temurin JDK 21.0.12.1+1, Scala 3.7.4, sbt 1.11.7 and Node 24.1.0,
with requested 3 GB sbt heap and 2 active processors. JS linking was serialized with
the independent provider audit. The isolated server cleanup command exited 0.
Thin clients restarted a departed server between invocations; these receipts
make no claim about resident-server reuse or performance.

The portable source patch is `../ar-exactsum-rehearsal.patch`, also retained in
`logs-and-sources.tar.gz` with raw logs, command receipts, runner, source hashes,
all three final source files and the exact upstream accumulator snapshot.
`receipt.json` records digests and limits. `verify.py` verifies the retained
artifacts and frozen source, without running sbt.

This is a rehearsal, ready for review. The current default root Gale pin lacks
the new API; upstream publication and a corresponding pin update must precede
adoption. Local JDK 21 tests do not establish GitHub JDK 17 compatibility. The
selected fit checks do not replace the full fit court. No push or PR occurred.
