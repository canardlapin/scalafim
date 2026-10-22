# U6 local modularization acceptance

Executed gates and independent final receipt review pass at qualified source `92154d81c39b6a384be195dde0e933ed0dff29ca`. The SHA-bound verdict is recorded in `final-review.md`. Source is published on `eco/u5-surface-20261021`; main is unmerged. Hosted CI is separate evidence.

| Gate | Executed result |
| --- | --- |
| Final-source compile-all | JVM and Scala.js Pass, compile55 |
| Bounded full regression | 37 batches, 11,868 passed, 18 declared skips, 5 ignored |
| Independent public consumers | 71 passed, 0 skipped across neuroim4s, neuroatlas4s, mvpa4s and neurosurf4s |
| Connected workflow matrix | All seven scenario families Pass |
| Standalone volume app | Actual NIfTI4D/ITK file IO, transform, Java2D/pick and refusal witness Pass |

The full regression is a **three-segment campaign**: 17 batches at `31a42a690`, 8 at `616860a2e`, and 12 at `92154d81c`. Two failed runs are retained. Full34 stopped before imaging tests because a provider alias replaced the Root alias. Its repaired six imaging targets then passed. Resume51 stopped at a real motion BIDS selection failure: regex `.*` defaults were being passed to provider glob selectors. Nine defaults now use `*`; the sessionless/default/glob/nonmatch regression and owning JVM/JS module gates passed. The failed partial batch is excluded entirely and rerun. No scientific tolerance or strict admission was weakened.

The collector checks 140 positive project summaries and 144 success markers, exact batch order, both platform-specific functional/prediction/RSA courts, 5 JVM + 4 JS new scenario results, raw metadata/invocations and cross-bound input manifests. Three test targets have no own tests (two pure MVPA example consumers and surface spatial IO); they supply compilation evidence only. Their exact pinned source inventories are recorded. The surface examples have three real smoke tests and are required.

The independent consumers record 27 Compile classpath sections, 15 clean source owners at exact revisions, and 120 qualified private artifact files (including metadata/checksums, not 120 runtime artifacts). Builds use isolated external caches and exact source pins, without sibling overrides or a machine-wide Ivy store. Source/build review additionally excludes provider test internals and checks minimal-core optional dependency boundaries. Root compile ownership checks cover 561 production files and 24 actual Compile closures.

`qualification.json` and `campaign-receipt-57.json` are the primary summaries. `seven-families.json` binds the workflow courts. Raw logs and actual exit metadata are gzip-compressed alongside source/input archives and executed drivers. `declared-nonexecutions.json` lists every skip/ignored case. Earlier failures and resource-custody receipts remain available; old checkpoints are labeled historical.

`size-and-boundaries.md` reports the exact before/after measurement: ScalaFIM 49 -> 34 source-bearing module directories and 214,313 -> 132,082 physical production Scala lines. Per-library ownership areas and dependency declaration syntax counts are qualified; this is not a claim of net ecosystem shrink or resolved-edge reduction.

Historical native/browser receipts retain their exact earlier sources, runtimes, failures and limits in `historical-display-review.md`. They are not final `73a244f` artifact visual executions. Synthetic software-raster/Java2D workflows do not establish native/WebGL, real-cortex, performance or broader scientific acceptance. Existing atlas-route residuals/missing 15-map replay, platform-specific legacy affine IDs, below-target LWU observations and disabled scientific flags remain explicit in `carried-forward-limits.json`.

`tracker-closure.json` records 36/36 tasks, 9/9 milestones and the epic closed. Four other ecosystem-tagged follow-ups remain separately tracked; closure does not claim those or a main merge. `final-review.md` records the independent approval of evidence candidate `92d6f27f9`; subsequent changes only record this verdict and tracker closure.
