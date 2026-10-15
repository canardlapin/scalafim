# Publication ownership handoff

The user explicitly assigned publication and the main merge to the other session. This session will perform no further push, PR update or merge. Hosted CI is left running; only this session's read-only watcher was stopped. The original checkout's concurrent working changes were preserved.

The original requested S1/S2/S11 batch MUST land: `e1cfa089` (`work/phrf-cmp-closeout-20261006`). PR #21's current published head `d8dd45c8f4a01d39c26d57e633798d52ac42ca12` contains it. All 46 batch paths, including 26 immutable Mote operations, were verified byte-identical at that head. Motes S1/S2/S11 are closed by those operations; the exact receipt is docs/verification/phrf-cmp-closeout-20261006/README.md.

Before merging any revised publication head:

    git fetch origin main migration/20261005/handoff
    python3 .mote/local/verify-phrf-closeout-20261006.py origin/migration/20261005/handoff

Then run the required exact-head protected checks and merge PR #21 with the matching head. Merge commits preserve ancestry. If choosing a squash/rebase route, explicitly preserve the checked batch files and immutable operations and retain the commit-to-content mapping.

Published repairs: `95236dc1` adopts merged Gale `d03eb99b`, the preserved compact adapter and an explicit Gale-block/partial-SVD LWU compiler. Local compiler controls29JVM/29JS, full first-level84/84 both, fit686JVM/629JS and CompileAll warning-clean. The unchanged frozen LWU grid, lags, all derivative columns, limits and tolerances remain; weak-LWU admission remains documented. Source/command/count/artifact receipts: docs/verification/main-landing-20261006.

`d8dd45c8` extends CI workflow wall-clock deadlines to finish all16 bounded batches (historical full run failed after82minutes at batch10/16). Test inventory95targets, per-test/scientific limits and coverage floors are unchanged.

Current exact-head Linux run: https://github.com/canardlapin/scalafim/actions/runs/37535757067
PR: https://github.com/canardlapin/scalafim/pull/21
At handoff all3requiredjobs were running; full compilation had passed and full test batches had started. No main merge was performed. Existing main remained568ce108. Do not reuse CI for another source head.

The isolated local branch land/main-20261006 has the published candidate and a subsequent local handoff/release record. Publication claims are released in that local record. Reconcile any alternative kernel implementation against the published head; preserve the original PHRF batch regardless of the chosen compiler repair.
