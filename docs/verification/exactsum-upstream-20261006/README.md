# Gale ExactSum upstream preparation

Prepared local commit `54e73f8e8f1218c4cb115a850aa80e1a36c6978f` on public main base
`d12a83fc6c31e685dff710e2c5a0ddbfb043320a`. The preparation receipt describes
the local qualification before publication. With owner approval, the exact
commit is published in [Gale draft PR #15](https://github.com/canardlapin/gale/pull/15);
`publication.json` records the verified account, head and draft state.

Full core tests passed: 763 JVM and 751 Scala.js, including 13 ExactSum tests on each.
The new files are admitted to Gale's existing scoped scalafmt configuration. The
final scalafmtCheckAll passed after a documentation-only review clarification.

`gale-exactsum.patch` is a format-patch suitable for git am on that base.
`gale-exactsum.bundle` carries the branch commit with the public base as a prerequisite.
`git bundle verify` succeeded; its output is retained. `receipt.json` hashes source
files, artifacts and all gate receipts. `logs-and-sources.tar.gz` retains full logs,
runner and final source snapshots.

The public API returns typed capacity admission, with one cached success result.
Capacity is 2^60 nonzero finite inputs, counted through copies and merges; refusal
precedes mutation. Zero and nonfinite inputs preserve the IEEE channel without
consuming finite capacity. Finite-input totals round once, nearest-even; NaN payload
bits and ordering are unspecified.

Development gates used JDK21/Scala3.7.4/sbt1.11.7/Node with -Xmx3g, four processors,
a nonresident sbt process and separate global/boot/Ivy/Coursier caches in private tmp.
JVM and JS test processes were serialized; JS links were coordinated with the other
agent. The expected JDK incubator-vector notices are retained separately from
compiler diagnostics. No compiler warnings occurred in the passing numerical gates.

The initial post-review format check failed only because the clarified Scaladoc
needed reflow. The subsequent formatter and final check passed. That documentation
change did not alter numerical/API bodies after the full core tests.

No ScalaFIM production source or Mote state was changed by this preparation. Upstream
merge and final ScalaFIM immutable pin adoption remain pending.

The source-only ScalaFIM AR migration is retained as `ar-exactsum-rehearsal.patch`.
Its [rehearsal receipt](rehearsal/README.md) records 410 passing AR/affected fit
test executions on JVM and JS against this exact local Gale candidate.
