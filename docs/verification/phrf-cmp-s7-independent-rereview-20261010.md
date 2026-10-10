# Independent S7 follow-up review (Codex, read-only), 2026-10-10

Reviewer: OpenAI Codex CLI 0.161.0 via `codex exec -s read-only`. Scope: fixes in `27339af6..b172836e`. Verbatim output follows.

**REQUEST CHANGES.** The original reproducers are addressed, but accounting can still become uncertain without blocking resume, and the ceiling restriction has a working deserialization bypass.

I read the prior review fully, the complete touched files, and surrounding runner, ledger, aggregation, store and bridge code. No files changed. I executed synthetic in-memory probes against the existing JVM classes; I did not rebuild or run the file-writing JVM/JS suites.

“Fails before” below means a behavioral failure against `27339af6`, allowing compatibility declarations for newly introduced types—not counting compilation errors as regression evidence.

| Prior failure / extra ask | Verdict | Evidence and regression assessment |
|---|---|---|
| **1. Missing contributions can verify `Valid`** | **FIXED** | [OwnerVerification.scala:231](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:231) checks every kept unit’s first contribution before invoking the assembler. My probe returned `Unverifiable(MissingContribution(...))`. I found no remaining path to `Valid` with an absent reserved entry. **Fails before:** the defaulting assembler in [OwnerVerificationSuite.scala:213](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/test/scala/scalafim/phrfcmp/exec/OwnerVerificationSuite.scala:213) previously produced `Valid`. |
| **2. Commit exception loses metered CPU** | **PARTIAL** | The original exception now checkpoints at [PilotRunner.scala:405](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:405), with another checkpoint after failed/interrupted joins at line 491. **Fails before:** [PilotRunnerSuite.scala:850](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/test/scala/scalafim/phrfcmp/exec/PilotRunnerSuite.scala:850) distinguishes durable **17 seconds from zero**. Checkpoint failure and uncertain resume remain open; see below. |
| **3. Adapter exception discards GLMsingle CPU** | **PARTIAL** | [PilotArms.scala:243](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotArms.scala:243) preserves CPU for `NonFatal` exceptions. `InterruptedException` still escapes without the measured CPU. **Fails before:** [PilotArmsSuite.scala:200](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/test/scala/scalafim/phrfcmp/exec/PilotArmsSuite.scala:200) previously throws at its direct adapter call; its scheduler assertions meaningfully cover **74 and 222 seconds** for zero and two retries. |
| **4. Ceiling above 60 without owner authorization** | **PARTIAL** | Ordinary construction is restricted by [Policy.scala:61](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Policy.scala:61). JVM deserialization bypasses those checks, and raises are not durably bound to runs. **Fails before:** [SchedulePolicySuite.scala:172](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/test/scala/scalafim/phrfcmp/exec/SchedulePolicySuite.scala:172) rejects `CpuGuard(45,61)`, which previously succeeded. It does not exercise either remaining defect. |
| **5. Inconsistent plan/ledger/payload evidence verifies** | **PARTIAL** | The original D violations now refuse at [OwnerVerification.scala:178](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:178); status and entry inventories are compared at line 165. My probes confirmed all these refusals. **Fails before:** both [review 5a](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/test/scala/scalafim/phrfcmp/exec/OwnerVerificationSuite.scala:223) and [review 5b](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/test/scala/scalafim/phrfcmp/exec/OwnerVerificationSuite.scala:241) previously reach `Valid`. Other plan/ledger inconsistencies remain accepted. |
| **6. Malformed lengths throw instead of refusing** | **PARTIAL** | The **unit-payload defect is fixed**: [PilotRunner.scala:97](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:97) bounds counts, and line 106 bounds lengths before narrowing/allocation. **Fails before:** [OwnerVerificationSuite.scala:263](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/test/scala/scalafim/phrfcmp/exec/OwnerVerificationSuite.scala:263) previously throws for `-1`; `1L << 40` previously narrowed to zero. Other numeric-count paths remain unchecked, detailed below. |
| **Abort-discard test** | **FIXED** | [OwnerVerificationSuite.scala:283](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/test/scala/scalafim/phrfcmp/exec/OwnerVerificationSuite.scala:283) plants `99.0`, crosses the hard ceiling, checks that nothing for that attempt was sealed, then verifies the resumed contribution. **Would pass on `27339af6`**: this closes a coverage gap in existing behavior. It covers abortion before commit begins. |
| **Earliest scheduled commit / `minBy` versus `maxBy` test** | **FIXED** | [OwnerVerificationSuite.scala:314](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/test/scala/scalafim/phrfcmp/exec/OwnerVerificationSuite.scala:314) creates an invocation-1 **Failed** scheduled commit and a differing invocation-2 **Done** scheduled commit. `maxBy` would select the consumed later commit and lose the asserted invalidation. **Would pass on `27339af6`**, whose selection already used `minBy`. |
| **H1 probe title** | **FIXED** | [PilotRunnerSuite.scala:752](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/test/scala/scalafim/phrfcmp/exec/PilotRunnerSuite.scala:752) now claims that both scheduled commits survive, matching its assertions. **The test already passed on `27339af6`;** this is a wording correction. |

The remaining substantive defects are:

1. **A failed checkpoint still permits undercounted resume.**  
   [PilotRunner.scala:319](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:319) suppresses every `NonFatal` checkpoint exception. If the worker checkpoint and final join checkpoint both fail—for example, persistent I/O failure or another interrupt during the write—the previous valid `cost.json` survives. [loadCost at line 204](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:204) accepts that stale file. There is no durable indication that accounting is uncertain.

   Ordinary `ExecutionException` and caller interruption during `f.get()` are now covered when persistence succeeds. Exceptions during the subsequent `awaitTermination` at [line 499](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:499) still escape without a schedule-wide final checkpoint. Abrupt process death likewise leaves no accounting-uncertainty marker. The new 17-second regression exercises none of these cases.

2. **Worker-side interruption still loses measured child CPU.**  
   My compiled-class probe produced:

   ```text
   FEED_IllegalStateException=Failed(glmsingle_adapter_IllegalStateException,37.0)
   FEED_InterruptedException_THREW=InterruptedException; engine guard=37.0
   ```

   `NonFatal` excludes `InterruptedException` in both the adapter and [PilotRunner.scala:383](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:383). Consequently, `meter.addChild` at line 385 is never reached. The final checkpoint cannot recover those 37 seconds and can persist a perfectly parseable undercount.

   **Retries themselves accumulate correctly** when an `ArmResult` reaches the runner: child CPU is added before every retry decision, including the terminal and discarded attempts. The remaining loss is on the exceptional handoff.

3. **The ceiling can still exceed 60 with no `OwnerCeilingRaise`.**  
   Normal `apply`, JVM constructor and generated product construction run `CpuGuard`’s requirements; the generated `copy` is private. I found no repository-defined JSON codec. However, the class is JVM `Serializable` and has no deserialization validation.

   I serialized a valid guard in memory, changed its encoded hard limit from 60 to 61, and deserialized it:

   ```text
   SERIALIZED_GUARD=CpuGuard(45.0,61.0,None)
   RAISE=None
   CHECK_60=SoftStop
   ```

   This bypasses [Policy.scala:64](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Policy.scala:64). The runner performs no independent admission check on the received guard.

   Separately, authorization is only sent to `log` at [PilotRunner.scala:249](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:249). That callback defaults to a no-op at line 161. The message omits the reason and run ID; neither cost state nor sealed metadata records the raise. An authorization value can also be reused across unrelated runs. **Durable recording and run binding are absent.**

4. **The verifier still accepts impossible ledger histories.**  
   With an otherwise valid 15-unit fixture, compiled-class probes returned:

   ```text
   ATTEMPTS_999=Valid(probe,15,15,Vector())
   INVOCATION_99_ONE_UNIT=Valid(probe,15,15,Vector())
   OUTSIDE_PLAN_LEDGER=Valid(probe,15,15,Vector())
   ```

   These respectively used `maxRetries=2`, changed one record’s ordinal while retaining the same run ID as the other records, and added a correctly named/hashed record for an unplanned cell.

   [LedgerRecord.parse at Model.scala:143](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Model.scala:143) checks positivity but not the plan’s attempt limit. [OwnerVerification.scala:154](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:154) receives no plan when validating ledgers, and line 201 trusts each record’s ordinal independently. The consumed-unit inventory is checked correctly; these broader history checks are missing.

5. **The broader count audit is incomplete.**  
   Unit-payload probes for negative counts, `Int.MaxValue` counts, negative lengths, `Int.MaxValue + 1`, `1L << 40`, `Long.MaxValue`, and truncated data all returned `Left`. Zero-length entries remain valid. Ledger integer fields also reject negative/out-of-range values appropriately.

   Two other paths remain problematic:

   - [OwnerVerification.scala:142](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:142) omits the upper bound before converting aggregate D to `Int`. I observed `datasets: 1e100` parse as **2147483647**. A normal small plan subsequently refuses it; an allowed `PilotPlan(datasets = Int.MaxValue)` passes that check and reaches the enormous expected-unit allocation at line 184 before inventory comparison.
   - [CostState.parse at PilotRunner.scala:61](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:61) accepts `invocations=Int.MaxValue`; [line 244](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:244) then overflows the increment and writes a negative ordinal. The new length regression does not cover either count path.

**One distinct new defect introduced by the fixes:** `OwnerCeilingRaise` validates only in `.of`, not in its constructor. The new case class at [Policy.scala:43](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Policy.scala:43) exposes generated product construction that bypasses the factory. My probe constructed `OwnerCeilingRaise(Infinity,"","")` through `fromProduct`; `CpuGuard.raised` accepted it and returned `SoftStop` even at one million core-hours. Thus the new authorization type does not preserve its own finite-ceiling, approver or reason invariants. The other defects above are remaining gaps in earlier behavior or incomplete fixes.

**Overall verdict: REQUEST CHANGES.**