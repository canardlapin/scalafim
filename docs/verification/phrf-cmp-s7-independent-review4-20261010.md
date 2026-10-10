# Independent S7 fourth-round review (Codex, read-only), 2026-10-10

Reviewer: OpenAI Codex CLI 0.161.0 via `codex exec -s read-only`. Scope: fixes in `9633419c..839053c9`. Verbatim output follows.

**REQUEST CHANGES.** H1 and H2 are only partially fixed. The other third-review defects are fixed, but I found three additional defects.

I read all three prior reviews, the full touched files, and surrounding runner, store, bridge, and verifier code. I reran probes against the existing compiled JVM classes using an in-memory filesystem; all 16 shared policy tests passed. I did not rebuild, run fresh JVM/JS gates, or repeat the real parent-death timing test. No repository files were modified; the worktree remains clean.

The classification below distinguishes demonstrated **DEFECTs** from **HARDENING**.

| Third-review finding | Verdict | Classification and evidence |
|---|---|---|
| **H1: fatal-throwable accounting close** | **PARTIAL** | **DEFECT remains.** A direct post-child `OutOfMemoryError` now leaves the marker and makes resume refuse. Combining that error with caller interruption still removes the marker with CPU unaccounted. Details below. [PilotRunner.scala:428](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:428) |
| **H2: raise lacks output identity** | **PARTIAL** | **DEFECT remains.** Different paths now refuse, and later invocations refuse. A distinct output created at the same path has the same identity and accepts the same authorization. [PilotRunner.scala:284](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:284), [admission:352](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:352) |
| **M1: recovery replay double-charges** | **FIXED** | **DEFECT resolved for charging.** Reconstructed crash state—cost 100, invocation 2, recovery for `dead`, marker `dead`—resumed at invocation 3 with cost **100 and one recovery record**. The recorded run ID prevents another charge. A separate sealed-evidence defect remains below. [PilotRunner.scala:333](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:333) |
| **M2: terminal `Failed` before retry exhaustion** | **FIXED** | **DEFECT resolved.** With `maxRetries=2`, consistently changing payload, ledger, and hashes to `Failed/attempts=1` now returns `Refused(ImpossibleLedger(...))`; `attempts=3` verifies. [OwnerVerification.scala:174](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:174) |
| **L1: ordinal boundary** | **FIXED** | **DEFECT resolved in cost accounting.** Starting at `2147483645` writes readable `2147483646`; the next invocation returns `InvocationsExhausted`, preserving cost and creating no accounting marker. [limit:91](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:91), [admission:393](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:393) |
| **L2: recovery-sum overflow** | **FIXED** | **DEFECT resolved.** `1e308 + 1e308` returns `AccountingRecoveryInvalid`; cost remains unchanged and the old marker remains. [PilotRunner.scala:339](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:339) |
| **L3: `maxRetries + 1` overflow** | **FIXED** | **DEFECT resolved.** Plans reject `Int.MaxValue`, including through `copy`; the maximum is 100. A one-attempt `Done` fixture verifies with that maximum. Verifier arithmetic uses `Long`. [StampBuilder.scala:11](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/StampBuilder.scala:11), [OwnerVerification.scala:168](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:168) |
| **Optional historical-record validation** | **FIXED** | **HARDENING implemented.** Individually forged ordinal 0, unsafe run ID, million-hour ceiling, empty approver/reason, and malformed stamp hash now produce `CostStateLost`. Recovery fields receive analogous validation. These historical records were not guard authorizations. [PilotRunner.scala:127](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:127) |

The two remaining high-severity defects are:

1. **H1 — DEFECT: caller interruption masks fatal accounting uncertainty.**

   The accounting wrapper decides whether accounting is certain from the *escaping* exception. However, `runWave` collects worker failures, then gives caller interruption precedence over them: [PilotRunner.scala:699](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:699) and [PilotRunner.scala:708](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:708). `accounted` converts that interruption into a result and closes the marker.

   I used the real GLMsingle arm adapter with an engine reporting **37 measured CPU seconds**. Its score feed blocked on a latch, then threw synthetic `OutOfMemoryError`. While blocked, I interrupted the calling runner thread, then released the feed:

   ```text
   result                 = Left(Interrupted)
   durable CPU            = 0
   accounting.open exists = false
   ordinary resume        = Right(0.0)
   ```

   Without caller interruption, the same fatal error correctly leaves the marker and resume returns `AccountingUncertain`. Thus the direct probe is repaired, but uncertainty must survive exception selection and cleanup independently of the final exception type. The child handoff still occurs before the scheduler meters its result: [PilotArms.scala:239](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotArms.scala:239).

2. **H2 — DEFECT: the stamp identifies configuration and path, not a distinct output instance.**

   **Precomputable: yes.** An existing output can be hashed directly. Before the first invocation, `PilotRunner.stampSha256` exposes the hash of exactly `effectiveStamp.json + "\n"`; there is no circular dependency on the newly drawn run ID. [PilotRunner.scala:293](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:293)

   **Stable across accepted resumes: yes.** My probe confirmed that the pre-run hash matched the written file, a new runner computed the same hash, and resume left the stamp unchanged. Invocation count, CPU, run ID, and ceiling are absent. Changed pinned inputs can cause `StampMismatch`; ordinary invocation changes do not.

   **Unique to an output: no.** The added identity fields are the recipient fingerprint and a hash of the normalized absolute path string. There is no per-output identifier.

   Concrete single-filesystem probe:

   - Run `/output` with a 70-hour authorization for invocation 1.
   - Move the entire output to `/archive`, preserving it.
   - Create a fresh `/output/sealed` with the same key and stamp inputs.
   - Reuse the unchanged authorization.

   ```text
   original invocation   = Right(1)
   fresh output hash same = true
   reused authorization  = Right(1)
   archived output exists = true
   ```

   This requires no SHA-256 collision or forged history. Both outputs have identical stamp bytes. It violates the explicit “cannot be reused for another output” rule in the [runbook:78](/Users/bbuchsbaum/code/scala/sf-phrf-s7/docs/plans/phrf-pilot-custody-runbook.md:78). The binding needs a distinct output identity that is established before authorization and retained on resume.

Three additional defects were demonstrated:

3. **NEW, medium — DEFECT: replay recovery can permanently omit its sealed accounting record.**

   `open` writes the updated cost before replacing the old marker: [PilotRunner.scala:414](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:414). After a crash there, replay correctly preserves the existing recovery record at invocation 2 while advancing to invocation 3. But `sealMeta` only seals recovery records belonging to the *current* invocation: [PilotRunner.scala:459](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:459).

   Executed result for that reconstructed crash state:

   ```text
   resume                         = Right((invocation=3, CPU=100))
   cost recovery records          = one, invocation=2
   accounting.open exists         = false
   decrypted accounting-recovery names = Vector()
   ```

   The total is correct, but the sealed accounting evidence is missing, contrary to the requirement that cost contains the same records as sealed metadata: [format:268](/Users/bbuchsbaum/code/scala/sf-phrf-s7/docs/plans/phrf-pilot-sealed-format.md:268), [runbook:79](/Users/bbuchsbaum/code/scala/sf-phrf-s7/docs/plans/phrf-pilot-custody-runbook.md:79). Completing replay must also complete the pending evidence write.

4. **NEW, low — DEFECT: the verifier accepts an impossible maximum invocation.**

   Ledger parsing accepts `Int.MaxValue`; history validation checks only the run-ID/ordinal mapping. [Model.scala:122](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Model.scala:122), [OwnerVerification.scala:185](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:185)

   Changing every ledger ordinal in an otherwise valid 15-unit fixture to `2147483647` returned:

   ```text
   Valid(vprobe,15,15,Vector())
   ```

   The runner cannot emit that ordinal; its maximum is `2147483646`. This is a newly demonstrated verifier gap, separate from the repaired cost-state boundary.

5. **NEW, low — DEFECT: journal recovery rejects a marker the runner can write.**

   `PilotPlan` accepts 10,001 datasets. `SealedNames.dataset(10000)` emits `d10000`, because `%04d` specifies minimum width. Recovery accepts exactly four digits. [StampBuilder.scala:15](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/StampBuilder.scala:15), [Arm.scala:113](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Arm.scala:113), [Progress.scala:62](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/Progress.scala:62)

   Calling `markDispatched(Job(C0,10000))`, then `recover(Set(C0))`, returned:

   ```text
   Left(LedgerCorrupt(
     unrecognised or inconsistent journal entry progress/C0/d10000.dispatched))
   ```

   This is a pre-existing boundary defect newly found here: valid writer output fails journal verification and blocks resume. It does not affect ordinary pilot-sized plans.

The previously confirmed defect protections otherwise retained their behavior:

| Protection | Fourth-round result |
|---|---|
| **Original H1: changed successful payload after crash** | Ledger-stage crash/resume retained two readable scheduled commits with different hashes. |
| **Original M1: aggregate name collision** | D=15 and D=16 aggregates coexist under distinct names. |
| **Original M2: prefix/dispatch recovery** | Equal-to-top and above-top interrupted jobs were finished; both probes returned D=3 and dispatched no new index-3 jobs. |
| **Original M3: lost cost state** | Missing and malformed cost with existing work still return `CostStateLost`. |
| **Original M4: worker joining** | Three caller interrupts did not return control before the blocked worker finished. Final CPU was 37 and the interrupt flag survived. |
| **Original M5 / watchdog** | No source regression in parent detection or process-group killing. Real parent-death timing was not rerun. |
| **Original M6: fixed blob count** | Successful and mixed failure/refusal two-unit runs both produced eight blobs. |
| **Environment scrub** | Root/ack variables were removed while a control variable remained. Reviewed spawn sites still scrub after overrides or use the restricted environment. |
| **Ordinary interrupt CPU** | Real adapter with synthetic 37-second attempt returned `Interrupted`, persisted 37, and closed accounting. Ordinary exception/retry probes retained 74/222 seconds. |
| **Construction/serialization bypasses** | Shared compile-negative tests passed; invalid reflective raise construction threw `IllegalArgumentException`; guard, raise, and recovery serialization each threw `NotSerializableException`. |

Verifier probes also retained the repaired behavior: missing contributions remain `Unverifiable`; excessive attempts, inconsistent invocation mappings, unplanned units, payload/status/inventory mismatches, invalid D, malformed lengths, and ambiguous aggregates refuse. Huge D refuses without huge allocation. Earliest failed scheduled commits still invalidate an aggregate despite a later successful commit, and aborted contributions remain discarded.

For `accounting.open`, mid-work and post-checkpoint probes retained the marker. An ordinary commit exception persisted **17 seconds** before removal. A failed final checkpoint retained the marker, and resume against restored stale cost returned `AccountingUncertain`. These paths remain sound; the compound fatal/interruption case above is the demonstrated exception.

The existing suppression of directory-fsync `IOException` remains a **HARDENING** concern in this review, with no reproduced power-loss violation claimed: [Fs.scala:46](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/Fs.scala:46).

**Overall verdict: REQUEST CHANGES.**