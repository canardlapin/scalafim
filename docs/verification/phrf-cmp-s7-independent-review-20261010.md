# Independent S7 re-review (Codex gpt-5.3-codex, read-only), 2026-10-10

Reviewer: OpenAI Codex CLI 0.161.0 via `codex exec -s read-only`, a different model family from the author. Scope: `8e85ef61..f2e92b42`. Required by the 2026-10-03 owner decision. Verbatim output follows.

**Request changes.** The specific H1, M2 and M4 repairs are supported by the current source, but CPU accounting still loses work on exception paths, and the new owner verifier can return `Valid` without the required evidence.

Reviewed `8e85ef61..f2e92b42`, including full touched files and surrounding runner/store/bridge code. No files changed. I ran synthetic, in-memory probes against existing compiled classes; I did **not** rebuild or rerun the file-writing suites. Historical receipts are not treated as acceptance.

All requested finding definitions were located: M1/M3/M6 in the [S7 findings table](/Users/bbuchsbaum/code/scala/sf-phrf-s7/docs/verification/phrf-cmp-s7-20261001.md:102), and M5 in the [S6 review](/Users/bbuchsbaum/code/scala/sf-phrf-s7/docs/verification/phrf-cmp-s6-20261001.md:185).

| Item | Verdict | Evidence and reasoning |
|---|---|---|
| **H1: changed successful bytes poison resumed store** | **PASS** | [PilotRunner.scala:334](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:334) selects run-specific payload, ledger and timing names. A crash after `LedgerSealed` followed by different successful bytes preserves both records without a differing duplicate. This repair predates the diff. |
| **M1: aggregate-name collision** | **PASS** | [PilotAggregation.scala:114](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotAggregation.scala:114) claims one aggregation per store/run ID and seals run-specific diagnostics and records. Different-D invocations do not collide. |
| **M2: partial selection retains holes** | **PASS** | [Policy.scala:162](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Policy.scala:162) takes the minimum contiguous prefix, keeps exactly `0 until D`, and drops higher completed indices in descending order. |
| **M3: missing/unparsable cost resets guard** | **PASS**, narrowly | [PilotRunner.scala:192](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:192) refuses missing cost beside existing work and malformed cost unconditionally. A **valid but stale** cost file remains a separate failure below. |
| **M4: caller returns with blocked worker alive** | **PASS** | [PilotRunner.scala:455](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:455) keeps joining after interruptions, sets the crash flag, and returns `Interrupted` only after the futures finish. The supplied latch scenario is covered. |
| **M5 / S6 parent-death watchdog** | **PASS** for the specified process-group case | [GlmSingleBridge.scala:826](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/run/GlmSingleBridge.scala:826) checks the parent before fork and during polling, then kills its own group. The test kills an actual intermediate parent and checks launcher, payload and grandchild. I did not independently remeasure the timing bound. |
| **M6: blob counts expose retries/refusals** | **PASS** for completed commits | [PilotRunner.scala:334](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:334) seals payload + ledger + timing for every terminal commit. Retries stay buffered. Crash orphans and padded-size differences remain outside this narrower count guarantee. |
| **Child-CPU error-path accounting** | **FAIL end to end** | The S6 `AttemptSink` repair preserves measured CPU through output-reading exceptions: [GlmSingleBridge.scala:755](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/run/GlmSingleBridge.scala:755). But an exception in the subsequent arm adapter can still replace that measured result with zero CPU. See failure 3. |
| **Helper environment scrub** | **PASS** | [ChildProcess.scala:54](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/ChildProcess.scala:54) scrubs after caller overrides; [GlmSingleBridge.scala:299](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/run/GlmSingleBridge.scala:299) scrubs helper launches; the GLMsingle launcher clears its environment. Both `StampBuilder` spawn sites also scrub. |
| **New M2 dispatch-marker repair** | **PASS** for runs recorded by this implementation | [Progress.scala:81](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/Progress.scala:81) writes marker and hash before the arm starts. [Policy.scala:19](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Policy.scala:19) includes dispatched incomplete jobs regardless of index; the scheduler executes them above soft stop. |
| **Final-run recomputation and sealed digest** | **PASS** mechanically | [PilotRunner.scala:485](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:485) recomputes kept earlier jobs in memory through the same CPU meter, using `Rerun` names. [PilotAggregation.scala:138](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotAggregation.scala:138) seals outcome/timing digests and consumed units. The runner does not call the owner verifier or decrypt previous outputs. |
| **Cumulative, fixed 60-core-hour guard** | **FAIL** | Exception paths bypass persistence; the public guard also accepts ceilings above 60. See failures 2 and 4. |
| **Owner verifier** | **FAIL** | Earliest scheduled selection and zero/multiple-aggregate refusal are implemented correctly. Missing contribution evidence and inconsistent records can nevertheless produce `Valid`; malformed payloads can escape as exceptions. |
| **Fresh JVM + JS validation** | **CANNOT-DETERMINE** | No fresh build or full suite execution in this read-only review. |

The concrete failures are:

1. **New: missing scorer contributions can verify `Valid`.**  
   [OwnerVerification.scala:194](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:194) converts a missing reserved entry into `UnitContribution(..., None)`. It delegates to the assembler without requiring contribution evidence. A successful matching rebuild reaches `Valid` at line 92.

   **Executed reproducer:** fifteen correctly hashed `Done` payloads containing **zero entries**, matching ledgers/unit list, a known schema, and a deterministic assembler returning a fixed synthetic corpus. Result:

   ```text
   MISSING_CONTRIBUTIONS=Valid(probe,15,15,Vector())
   ```

   The assembler can therefore make absent evidence acceptable. The binding rule requires this store to be `Unverifiable`. A regression should construct this store and assert that `Valid` is impossible, even if the assembler supplies defaults.

2. **Existing: a commit exception loses already-spent CPU across resume.**  
   [PilotRunner.scala:365](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:365) adds child CPU before committing. A commit exception is rethrown at lines 383–385, bypassing the checkpoint at line 386. Neither `schedule` nor `run` performs a final cost checkpoint on this exit.

   **Test sketch:** one worker; advance `FakeClock` by 10 seconds; return `Done(7)`; throw from the first `DataSealed` hook. Assert durable cost includes the 17 seconds, or resume refuses uncertain accounting. Currently the initial valid `cost.json` remains at zero. A new invocation accepts it and starts a new meter baseline, losing those 17 seconds. Repeated crashes can defeat cumulative enforcement without deleting or corrupting cost state.

   Existing crash tests check store readability and logical outputs, **not durable CPU after the crash**.

3. **Existing: an adapter exception discards measured GLMsingle CPU.**  
   [PilotArms.scala:238](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotArms.scala:238) obtains `guardCpuSeconds`, but invokes `feed.trial`/`feed.timing` before returning `ArmResult.Done(guard)` or `Failed(..., guard)`. If a callback throws, [PilotRunner.scala:361](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:361) substitutes `ArmResult.Failed("executor_exception:...")`, whose CPU defaults to zero.

   **Test sketch:** extend the existing fake-engine test with a measured 37-second GLMsingle attempt and a `ScoreFeed.trial` that throws. Use `maxRetries = 0` and a stationary fake JVM clock. The scheduler should account for 37 seconds per attempted unit; currently it accounts for zero. With retries, every repeated child execution is similarly lost.

4. **Existing policy conflict: 60 is a default, not an enforced maximum.**  
   [Policy.scala:47](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Policy.scala:47) validates only `0 < soft <= hard`. My executed probe:

   ```scala
   CpuGuard(45.0, 61.0).check(60.0 * 3600)
   // SoftStop
   ```

   Resumed/recomputed jobs can consequently run beyond the binding ceiling. The implementation does not raise it automatically, but still exposes and documents that route: [runner design:462](/Users/bbuchsbaum/code/scala/sf-phrf-s7/docs/plans/phrf-pilot-runner-design.md:462) and [custody runbook:78](/Users/bbuchsbaum/code/scala/sf-phrf-s7/docs/plans/phrf-pilot-custody-runbook.md:78) explicitly permit raises. Those statements conflict with the supplied owner decision. The existing “resumable with a raised ceiling” test preserves the older policy.

5. **New: the verifier accepts inconsistent plan/payload evidence.**  
   [OwnerVerification.scala:157](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:157) derives expected units from the aggregate’s D without checking D against `plan.datasets` or `plan.minDatasets`. Its payload decode at line 198 also discards the payload status, without comparing it or entry names with the ledger.

   Executed results included:

   ```text
   D_EXCEEDS_PLAN=Valid(probe,15,15,Vector())
   D_BELOW_PLAN_MIN=Valid(probe,15,15,Vector())
   LEDGER_PAYLOAD_STATUS_MISMATCH=Valid(probe,15,15,Vector())
   ```

   The first two used D=15 against plans requesting 14 datasets, and 20 datasets with minimum 16, respectively. The third changed a ledger to `Failed` while its payload remained `Done`.

   Regression sketches: run an otherwise-valid contribution fixture against those incompatible plans; separately change only the ledger status or entry inventory. Each should refuse.

6. **New verifier integration failure: malformed payload length throws instead of returning a verdict.**  
   [PilotRunner.scala:94](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:94) narrows an unvalidated `Long` length to `Int` and allocates an array; `decode` catches only `IOException`. The new verifier calls it without containing other decode failures.

   **Executed reproducer:** encode one entry with length `-1L`, update the ledger and consumed hashes consistently, then verify:

   ```text
   MALFORMED_PAYLOAD_THREW=NegativeArraySizeException
   ```

   This should be a typed malformed-record refusal. The decoder predates the diff; exposing it through the new verifier introduces this failure into owner verification.

The regression tests are substantive, but several claims exceed what they establish:

- **H1:** [probe H1:749](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/test/scala/scalafim/phrfcmp/exec/PilotRunnerSuite.scala:749) genuinely changes successful bytes and crashes between ledger and timing. It would fail with deterministic payload names. Its title’s “first attempt is scored” claim is stronger than its assertions: it inspects records and hashes, without invoking the owner verifier.
- **M2:** [probes M2a/M2b:768](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/test/scala/scalafim/phrfcmp/exec/PilotRunnerSuite.scala:768) exercise the actual equal-to-top and above-top failures. The old completed-only `mustFinish` skips those jobs, so these would fail before this repair.
- **M4:** [probe M4:805](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/test/scala/scalafim/phrfcmp/exec/PilotRunnerSuite.scala:805) uses a latch-blocked worker that ignores interrupts and verifies caller liveness before release. That genuinely distinguishes joining from abandoning workers.
- **M1/M3/M6:** R5’s differing-D aggregate, R3’s deleted/malformed cost, and the mixed-status fixed-blob-count test target their original failures. R3 does not cover failure 2.
- **Watchdog / S6 CPU / scrub:** [GlmSingleBridgeSuite.scala:850](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/test/scala/scalafim/phrfcmp/run/GlmSingleBridgeSuite.scala:850), line 914 and line 931 respectively exercise real parent death, a post-child exception, and injected environment variables with a surviving control variable. These are meaningful regression designs, although the CPU test stops short of failure 3.
- **Owner verification:** [OwnerVerificationSuite.scala:145](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/test/scala/scalafim/phrfcmp/exec/OwnerVerificationSuite.scala:145) genuinely distinguishes a discarded retry contribution (`99.0`) from the terminal contribution. It does not test contribution discard on abort. Nor does the suite test an earlier failed scheduled commit against a later successful scheduled commit. By inspection, replacing `minBy` with `maxBy` would survive its data: differing repeated values occur in `Rerun` records, while repeated `Scheduled` values match.

Finally, production contribution encoding remains absent: [PilotArms.scala:35](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotArms.scala:35) explicitly retains the nontransactional `ScoreFeed`. That is an acknowledged unfinished integration, not a newly introduced regression. The new verifier defects above prevent accepting even the synthetic verification seam as satisfying the owner’s missing-evidence rule.