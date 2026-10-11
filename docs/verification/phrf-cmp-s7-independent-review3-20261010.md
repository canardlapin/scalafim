# Independent S7 third-round review (Codex, read-only), 2026-10-10

Reviewer: OpenAI Codex CLI 0.161.0 via `codex exec -s read-only`. Scope: fixes in `78d61661..e75d2b49`. Verbatim output follows.

The fixes are incomplete. I read both prior reviews, the full touched files, and surrounding runner/store/bridge code. No files were modified. I reran synthetic probes against the existing compiled JVM classes using an in-memory filesystem, and executed all 16 shared policy tests successfully. I did not rebuild or run fresh JVM/JS build gates.

The numbering below follows the second review’s five remaining findings plus its sixth, newly introduced construction bypass.

| Finding | Verdict | Evidence and rerun result |
|---|---|---|
| **1. Failed checkpoint permits undercounted resume** | **PARTIAL** | The ordinary checkpoint failure now leaves `accounting.open`; resume returned `AccountingUncertain`. The original commit-exception probe persisted **17 seconds**. However, an exceptional child-CPU handoff can still close accounting with zero CPU; recovery also has a replay window. See below. [PilotRunner.scala:378](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:378), [checkpoint installation:450](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:450). |
| **2. Worker interruption loses measured child CPU** | **FIXED** | The exact probe now returns `Failed(glmsingle_interrupted,37.0)` and restores the interrupt flag. Through the scheduler: `Left(Interrupted)`, durable cost **37**, marker absent. Ordinary exception/retry probes still account for **74/222 seconds**. [PilotArms.scala:244](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotArms.scala:244), [PilotRunner.scala:530](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:530). |
| **3. Ceiling deserialization bypass; missing durable/run binding** | **PARTIAL** | Serialization bypass is closed. Successful invocations record the full authorization in cost state and sealed metadata, and later reuse within the same output refuses. But the same authorization succeeds in two unrelated outputs at the same ordinal. [admission:308](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:308), [recording:349](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:349), [sealing:402](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:402). |
| **4. Impossible ledger histories verify** | **PARTIAL** | All three prior probes now refuse: `ATTEMPTS_999`, `INVOCATION_99_ONE_UNIT`, and `OUTSIDE_PLAN_LEDGER`. A terminal `Failed` record before retry exhaustion still verifies, and the new attempt-limit expression overflows for an accepted plan. [OwnerVerification.scala:166](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:166), [invocation checks:182](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:182). |
| **5. Incomplete count audit** | **PARTIAL** | D=`1e100` now gives `MalformedRecord`; D=`Int.MaxValue` with the corresponding plan gives `UnitListMismatch` without enormous allocation. Cost ordinal `Int.MaxValue` refuses. But accepted ordinal `Int.MaxValue-1` advances to an unreadable durable state. [D validation:145](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:145), [inventory count:203](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:203), [cost parsing:107](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:107). |
| **6. `OwnerCeilingRaise.fromProduct` bypasses invariants** | **FIXED** | No generated `apply`, `copy`, or product mirror remains. Mirror/direct-construction compilation probes fail; invalid constructor invocation throws `IllegalArgumentException`. Guard, raise, and recovery serialization each throw `NotSerializableException`. [Policy.scala:48](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Policy.scala:48), [recovery:72](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Policy.scala:72), [guard:98](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Policy.scala:98). |

The concrete remaining and newly found defects are:

1. **High: a post-child fatal exception still permits undercounted resume.**  
   With the same measured 37-second GLMsingle attempt, I made `ScoreFeed.trial` throw a synthetic `OutOfMemoryError`. The adapter catches interruption and `NonFatal`, so this escapes before `meter.addChild`. The invocation-wide exception handler nevertheless checkpoints and removes the marker.

   ```text
   POST_CHILD_OOME_THREW=OutOfMemoryError
   POST_CHILD_OOME_COST={"cpu_seconds_total":0,"invocations":1}
   MARKER=false
   POST_CHILD_OOME_RESUME=Right(0.0)
   ```

   The exceptional handoff gap predates this batch; the new marker protocol still incorrectly certifies its accounting as closed. Evidence: [PilotArms.scala:245](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotArms.scala:245), [PilotRunner.scala:538](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:538), [exception close:384](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:384). The measured CPU must survive this handoff, or the invocation must remain accounting-uncertain.

2. **High: ceiling authorization remains reusable across unrelated runs.**  
   `OwnerCeilingRaise` contains an ordinal, but no output/run identity. `admitGuard` compares only that ordinal. One unchanged authorization for invocation 1 produced:

   ```text
   RAISE_OUTPUT_A=Right(1)
   SAME_RAISE_OUTPUT_B=Right(1)
   SAME_OUTPUT_REUSE=Some(CeilingNotAuthorized(...not 2))
   ```

   Recording each newly chosen run ID afterward does not bind the owner’s original authorization to that run. This is the unresolved run-binding part of finding 3, not a constructor bypass. Evidence: [Policy.scala:48](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Policy.scala:48), [PilotRunner.scala:312](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:312).

3. **New, medium: accounting recovery is not single-use across its own crash window.**  
   Recovery adds its charge and appends a record. `open` then writes cost state **before** replacing the old marker. Death between those writes leaves the charge recorded but the old run ID still open. The next recovery never checks whether that run has already been charged.

   I reconstructed exactly that intermediate state: cost 100, recovery record for `dead`, marker `dead`. Reusing the same 100-second recovery succeeded with **cost 200 and two recovery records**.

   Evidence: [reconciliation:299](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:299), [write ordering:366](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:366). Recovery needs replay-safe application keyed by the uncertain run. This defect overcounts CPU and can prematurely exhaust the ceiling.

4. **Medium: another impossible terminal history still verifies `Valid`.**  
   With `maxRetries=2`, I changed a unit’s payload and ledger consistently to `Failed`, leaving `attempts=1` and updating all hashes. Result:

   ```text
   FAILED_BEFORE_RETRIES_EXHAUSTED=Valid(probe,15,15,Vector())
   ```

   The runner necessarily retries that failure; terminal `Failed` requires three attempts. The verifier checks only the upper limit. Evidence: [RetryPolicy:31](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Policy.scala:31), [OwnerVerification.scala:168](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:168). This is a newly demonstrated gap in finding 4.

5. **Lower-severity numeric defects remain, including two introduced by this batch.**

   | Case | Executed result and consequence |
   |---|---|
   | **Ordinal boundary** | Cost ordinal `2147483646` runs successfully and writes `2147483647`; the next invocation returns `CostStateLost`, with no marker. The writer emits a state its parser rejects, contrary to the documented “always below `Int.MaxValue`” invariant. [increment:346](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:346). |
   | **New recovery-sum overflow** | Finite prior CPU `1e308` plus a valid finite recovery charge `1e308` writes `"Infinity"`, removes the marker, then permanently refuses with `CostStateLost`. `OwnerAccountingRecovery` preserves its individual-field invariants, but applying it does not preserve the cost-state invariant. [addition:302](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:302). |
   | **New retry-limit overflow** | An accepted plan with `maxRetries=Int.MaxValue` makes `maxRetries + 1` negative. An otherwise valid one-attempt `Done` fixture is rejected as exceeding the retry cap. [OwnerVerification.scala:168](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:168). |

   These are demonstrated boundary-validation defects, not ordinary pilot-sized failures.

The `accounting.open` lifecycle otherwise behaves as follows:

| Exit/death point | Resume behavior |
|---|---|
| Before marker publication, including after the initial cost write | No arm has started; ordinary resume is safe. A failed open can consume an ordinal. |
| After marker publication, before or during work | Surviving marker refuses resume. Mid-work snapshots reproduced this. |
| After an intermediate checkpoint | Marker remains because later work may be uncounted; resume refuses. |
| Normal return, typed stop, ordinary exception, caller/worker interruption | Final checkpoint precedes removal. The exception and repeated-interrupt probes preserved CPU and joined workers. |
| Exception during post-join `awaitTermination` | Now passes through the invocation-wide close; the earlier missing-final-checkpoint path is covered by source. |
| Failed final checkpoint | Marker remains; stale-cost resume refuses. Matching recovery worked; mismatched recovery refused. |
| Death after final checkpoint but before marker removal | Conservatively refuses despite complete cost. A documented zero-charge recovery succeeded in the probe. |
| During/after marker removal | Completed cost precedes removal; a surviving marker causes conservative refusal, an absent marker permits resume. |
| Fatal child-CPU handoff or recovery transition | Defective as demonstrated above. |

Thus ordinary process death does not silently reset CPU, but **resume can still proceed undercounted after the fatal handoff**, and the numeric cases can create states unrecoverable through the documented recovery API.

Power-loss durability also retains the previously documented limitation: [Fs.scala:46](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/Fs.scala:46) suppresses directory-fsync `IOException`. The process-death analysis is not proof of durable marker publication when directory forcing fails. This is an existing limitation, not a newly demonstrated regression.

For authorization bypasses, I found no remaining normal constructor, mirror, or Java-serialization route around the validated ceiling. `CostState.parse` does accept malformed historical authorization records—for example ordinal 0, unsafe run ID, empty approver/reason, and a million-hour limit—but those records do **not** become guard authorizations. My forged-history probe still stopped at 60 hours. Stronger historical-record validation is hardening; it is not another ceiling bypass.

The **240-core-hour maximum is an unrequested owner-policy addition**, enforced at [Policy.scala:54](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Policy.scala:54) and documented in the [runbook:78](/Users/bbuchsbaum/code/scala/sf-phrf-s7/docs/plans/phrf-pilot-custody-runbook.md:78). Code and updated documentation agree; I do not classify the cap itself as a defect.

No regression was found in the previously passing protections:

- **H1:** changed successful bytes after a ledger-stage crash retained two readable scheduled commits with different hashes.
- **M1:** D=15 and D=16 aggregates coexist under distinct names.
- **M2 and dispatch markers:** both equal-to-top and above-top resume probes finished dispatched work, returned D=3, and dispatched no new D=3 jobs.
- **M3:** missing/malformed cost state still refuses.
- **M4:** repeated caller interrupts kept the caller blocked until its worker finished; CPU and interrupt status survived.
- **M6:** successful and mixed failure/refusal runs both produced eight blobs in the two-unit probe.
- Missing contributions, plan bounds, payload/status/inventory mismatches, malformed lengths, aggregate ambiguity, earliest failed scheduled selection, and abort contribution discard retained their repaired behavior.
- Watchdog and spawn-site scrubbing remain unchanged in source. The environment-map scrub probe passed; I did not independently rerun the real parent-death timing test.

**Overall verdict: REQUEST CHANGES.**