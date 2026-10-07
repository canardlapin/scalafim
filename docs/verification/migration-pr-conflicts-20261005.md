# Migration PR audit, 2026-10-05

Migration refs preserve development for recovery on another machine. They were not published as a queue of branches to merge wholesale. The qualified source is consolidated in PR #20; PR #21 adds the complete portable tracker and handoff. All 166 archival ref tips were independently verified on GitHub.

This assessment uses remote main `568ce108` and handoff `283af506`. Git merge simulations ran in an isolated object store; no source checkout or original branch was changed. Conflict counts are distinct paths, not conflict hunks. Hosted CI status is a separate gate.

| PR | Against remote main | Against consolidated handoff | Disposition |
|---|---:|---:|---|
| [#6](https://github.com/canardlapin/scalafim/pull/6) | 100 | 117 | Historical GIFTI lineage; remaining C6 requires a current-contract port. |
| [#19](https://github.com/canardlapin/scalafim/pull/19) | 0 | 1 | Superseded reconciliation tip; no current-main conflicts, but hosted CI failed. |
| [#20](https://github.com/canardlapin/scalafim/pull/20) | 0 | 0 | Reviewed code publication; conflict-free, required CI pending. |
| [#21](https://github.com/canardlapin/scalafim/pull/21) | 0 | 0 | Consolidated code plus portable Mote history; conflict-free, required CI pending. |
| [#22](https://github.com/canardlapin/scalafim/pull/22) | 100 | 122 | Historical compatible-layer transaction; retained within other archival lines. |
| [#23](https://github.com/canardlapin/scalafim/pull/23) | 100 | 122 | Historical affine/atlas recovery; only the bounded qualified slice is consolidated. |
| [#24](https://github.com/canardlapin/scalafim/pull/24) | 5 | 5 | Historical template sphere-chain integration; current qualification deferred. |
| [#25](https://github.com/canardlapin/scalafim/pull/25) | 5 | 5 | Source represented in main or its history; audit residual behavior before porting. |
| [#26](https://github.com/canardlapin/scalafim/pull/26) | 2 | 0 | Exact tip already an ancestor of the consolidated handoff; no separate merge needed. |
| [#27](https://github.com/canardlapin/scalafim/pull/27) | 31 | 32 | Historical atlas/domain realization; current identity/consumer port deferred. |
| [#28](https://github.com/canardlapin/scalafim/pull/28) | 4 | 37 | Exact dirty-worktree snapshot; unfinished changes and evidence, not a merge candidate. |
| [#29](https://github.com/canardlapin/scalafim/pull/29) | 36 | 38 | Historical fsLR/group line contained in archived main-catchup; admission limits remain. |

PR #6 and #22–#29 were converted to drafts with exact-head disposition and conflict paths in their descriptions. Readback verified each draft flag, description and unchanged head. The branches remain intact. PR #26 should be retired after the consolidated publication lands; it adds no new commits to that candidate. PR #25 source-history presence does not imply all historical behavior should be restored.

The older branch tips overlap each other. Taking every branch version of a conflicted file can undo newer contracts or combine unqualified work. Remaining feature work should start from consolidated main, extract the intended changes, retain Mote acceptance limits, and run the affected JVM/Scala.js and consumer gates.

See [the machine handoff](../development-machine-handoff.md), [consolidation dispositions](consolidation-20261005.md), [current Mote assessments](open-mote-audit-20261005.md), and [complete conflict-path data](migration-pr-conflicts-20261005.json).
