# PHRF decoder local landing — 2026-10-01

## Selection and source identity

Owner explicitly requested local landing. Integration source `e6eb79d0912fcf8a4e43cc0aecf3e92f0acf9697` consists of selective source/evidence commit `447a19e4d82ace9a9c121d94c862b5f8a0dc2b2c` and test-fixture migration `e6eb79d0`, based on current committed main `c05a0811e96295c105633bbad0325790e22377c6`. The full original repair branch contains other execution/provider ancestry and is not merged wholesale.

Four module files are byte identical to reviewed source `8840dbfcebb53ccf60396b8244b75235eb589bd4`: `ShapeDecoder.scala`, `WorkReceipt.scala`, `ShapeDecoderSuite.scala`, and `ShapeDecoderTangentSuite.scala`. Against committed main this includes earlier decoder correctness prerequisites: prior-aware scanning, honest budget/terminal-curvature handling, stationary rounded-equality checks, final full-jet use, and exit diagnostics. It is broader than the final tangent-only diff. The preexisting dirty shared decoder and suite exactly match the already reviewed `04a68798` baseline; the dirty work receipt exactly matches the selected reviewed version. No main-only committed changes to these files are displaced.

Two first-level test files carry only reviewed quota/counter hunks, described below. No other module source is selected. Owned LWU diagnosis, repair-design and tangent verification archives retain their original bytes and receipts, including all unsuccessful attempts. The preparation admission wrapper, trial/executor/provider ancestry, dirty HRF/build/atlas/other-fit edits and unrelated new files are outside this landing.

`selection-audit.json` pins the initial/final integration source and all 137 source/archive paths. `integration-source-manifest.json` pins 1,981 tracked module/build files at e6eb79d0. All four decoder-seam hashes match the reviewed source. Original tangent report/index/review remain bound to their original source; they are not relabeled as new-main qualification.

## Legacy fixture migration

The selected decoder with old main fixture quotas (2 Newton steps, 2 jets, 6 exact evaluations) passed full fit tests but failed the two first-level admission assertions on both platforms: compact cohort 0/24 and Gram cohort 0/20. A separate actual JS run of the unchanged preexisting shared decoder seam reproduced the same two failures. Observation-only println additions retain every original assertion/quota: all 44 baseline statuses are BudgetExceeded; all 24 runtime terminal curvatures are unavailable. The failures precede this tangent repair and are preserved in separate receipts.

Only the previously reviewed fixture allowance of 6 Newton steps, 8 jets and 2 exact evaluations is carried into `CompactConditionRuntimeSuite` and `ConditionProfileFitSuite`, with matching actual-counter bounds. This changes test execution allowance; it does not establish compatibility or admission under the old caps. Accuracy/admission thresholds, cohorts, seeds, production defaults and numerical tolerances stay unchanged. Certification-wrapper and provider changes from the broader reviewed branch are not imported. No test assertion is removed; the Gram route gains an exact-evaluation bound.

## Integrated verification

| Check | Result | Receipt |
| --- | --- | --- |
| Whole-project compile, JVM and JS | Pass; no warnings in full output | `tangent-landing-jvm-v1.log` |
| Full fit JVM | 350/350, including all eight tangent controls | JVM v1 receipt |
| Full fit JS | 339/339, including all eight tangent controls | `tangent-landing-js-v1.log` |
| Original-cap selected consumers | 4/6 each platform; two admission failures | Both v1 receipts, exit 1 |
| Old shared baseline JS, two suites | 3/5, same two failures; 44 BudgetExceeded observations | `tangent-landing-oldcaps-baseline-js-v1.log`, exit 1 |
| Migrated selected consumers JVM | 6/6 | `tangent-landing-jvm-fixtures-v2.log`, exit 0 |
| Migrated selected consumers JS | 6/6 | `tangent-landing-js-fixtures-v2.log`, exit 0 |
| Workflow JVM / JS | 23/23 and 19/19 | Both v2 receipts |

Compile/full-fit checks ran at 447a19e4; production source and fit tests are unchanged at e6eb79d0. The affected first-level test changes were subsequently checked on both platforms at e6eb79d0. There is no reason to rerun unchanged fit or library compilation after these two test-only edits. The two v1 batches each exit 1 despite their successful compile/fit stages; only v2 batches are called successful. Shared sbt serialization and existing completed own APFS COW caches were used; no foreign cache/process cleanup occurred. Exact commands, working directories, actual exits and full output are archived.

Both integrated compact fixture cohorts admit 23/24 under the migrated quota. Maximum latency error is about 0.0003706374 seconds and maximum relative amplitude error about 0.0001063465, within the unchanged assertions. JVM mean jets are 4.4167, exact evaluations 0; JS mean jets 4.5833, exact evaluations 0.08333. These small existing consumer fixtures do not qualify PHRF-21.

## Historical evidence and remaining limits

Re-analysis of the archived 400 historical records reproduces their existing noncryptographic fingerprint, 57 literal-pair and 343 Accepted recorded-jet algebra checks. This is archive validation, not an execution replay of e6eb79d0. Historical counts JVM 91/81 and JS 89/82 belong to the separately reviewed 8840dbfc execution context and its 6/8/2 milestone policy. The current main milestone source is older (Gaussian 2/2/6 and LWU 3/3/6), and its full accuracy/throughput suite was not executed or promoted here. No fresh cohort, 100k/campaign, paired energy comparator or boundary inference change occurred. All parent PHRF-21 admission/inference/resource gates retain their prior disposition.

The public Stalled case preserves previous enum ordinals but external exhaustive matches need handling. This local integration is not a release compatibility claim. Whole-repository compilation passed; aggregate whole-repository tests were not run. Tests qualify the isolated committed integration source, not the user's complete unrelated dirty working tree.

## Local landing protocol and review

Use standard `git merge --ff-only --no-autostash`, explicitly setting `merge.autoStash=false`. First check current main identity and every owned target hash/absence against the captured snapshot. Durable backups of only the three preexisting owned dirty baseline files and a recovery journal are retained. Temporarily restore only those three owned files to committed main before the fast-forward: all their reviewed changes are already included in the integration commit. Do not reset, stash, manually replace refs/index, overwrite unrelated edits or merge the full original branch.

On a refused fast-forward without HEAD advancing, restore only owned backups and verify the old index and target snapshots; an unexpected partial state retains the recovery journal/backups for explicit repair. This is standard Git integration with narrow recovery, not a claim of collectively atomic filesystem replacement. Verify every landed target against the new commit and all unrelated index entries and working states afterwards. Separate NUL-delimited tracked-modified/deleted, untracked-all and staged-no-renames Git listings capture nested untracked files, both rename sides, absent paths, symlink targets and file modes. An unexpected outside state change leaves preservation unverified and retains the journal/backups; its cause is not attributed without investigation. `land_git_ff.py` records the exact protocol; its execution receipt, final main identity and preservation check are reported in the Mote completion/handoff after successful landing. No remote operation is authorized or performed.

The first protocol review rejected incomplete outside-state capture and unsupported concurrency attribution; its exact report/index/helper snapshots and verdict remain separately named. Both flaws were corrected before any user-checkout landing. A real synthetic Git fast-forward with an unrelated staged rename preserves all eight expected outside states, including nested untracked files, deletion, rename, symlink and mode changes. A deliberately changed nested file is detected as unexpected without attribution. This control tests the landing mechanism, not numerical qualification.

The independent verdict in `review.txt` is bound to this report, the source and `payload-sha256.json`. The payload index excludes itself and the verdict to avoid circular hashing. Child `bd-01M3VSY6BYK65WZVWT0D422VRW` closes after local landing and preservation verification; parent `bd-01M25Q0JY8GA7CJRKZF2934PJ0` remains open.
