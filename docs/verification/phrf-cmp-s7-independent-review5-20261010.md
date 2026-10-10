# Independent S7 fifth-round review (Codex, read-only), 2026-10-10

Reviewer: OpenAI Codex CLI 0.161.0 via `codex exec -s read-only`. Scope: fixes in `63d3dccf..447de9d5`. Verbatim output follows.

**REQUEST CHANGES.** The exact fourth-round fatal-plus-interrupt reproducer is fixed, but output binding remains replaceable, and I found another fatal-error accounting failure.

I read all four reviews, the full touched files in `63d3dccf..447de9d5`, and surrounding implementation. No files were modified.

| Fourth-review finding | Verdict | Classification, evidence and probe result |
|---|---|---|
| **H1: sticky uncertainty under fatal + interrupt** | **FIXED** for the reported combination | **DEFECT resolved for that reproducer.** Worker failures set the sticky flag before exception selection at [PilotRunner.scala:732](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:732); accounting closure consults it at [line 459](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:459). The 37-second adapter probe with caller interruption retained `accounting.open`; ordinary resume refused. A separate cleanup-masking defect remains below. |
| **H2: output-id binding** | **PARTIAL** | **DEFECT remains.** A fresh output at the same path receives a different identity and rejects the old raise. However, replacing its `output-id` makes it accept that authorization. Resume validates only the file’s syntax: [PilotRunner.scala:377](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:377). |
| **M: replayed recovery sealing** | **FIXED** | **DEFECT resolved.** [PilotRunner.scala:481](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:481) seals all retained authorization records. Replaying the cost-100/invocation-2 recovery preserved one charge and produced `meta/accounting-recovery/dead`, retaining the record’s original invocation. |
| **L1: verifier ordinal bound** | **FIXED** | **DEFECT resolved.** [OwnerVerification.scala:168](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/OwnerVerification.scala:168) rejects ordinals above `2147483646`. The fixture with every ledger changed to `2147483647` now returns `ImpossibleLedger`. |
| **L2: dataset-name bound** | **FIXED** | **DEFECT resolved.** [StampBuilder.scala:17](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/StampBuilder.scala:17) caps plans at 10,000 datasets. The 10,001-dataset plan rejects; 10,000 accepts, and index 9999’s journal marker recovers successfully. |

**H2 — DEFECT: substituting `output-id` transfers an existing raise to a distinct output.**

`outputIdentity` hashes the files currently present at [PilotRunner.scala:306](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:306). Neither `effectiveStamp` nor sealed `meta/stamp` retains the original output ID: [line 311](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:311).

Executed reproducer:

1. Run `/out` once; obtain its identity and a 70-hour authorization for invocation 2.
2. Use that authorization successfully.
3. Move the entire output to `/archive`.
4. Run a fresh `/out` once with the same key and stamp inputs.
5. Confirm that the unchanged authorization rejects.
6. Replace only the fresh `output-id` with the archived output’s bytes, then reuse the authorization.

```text
original raise                 = Right(2)
fresh output                   = Right(1)
same stamp bytes               = true
new output-id differs          = true
reuse before substitution      = Left(CeilingNotAuthorized(...))
reuse after substitution       = Right(2)
archived output still present  = true
both sealed stores readable    = true
```

This requires deliberate replacement of one plaintext file. Both cost histories and sealed trees remain intact. It demonstrates a violation of the specific “cannot be reused for another output” rule at [runbook:78](/Users/bbuchsbaum/code/scala/sf-phrf-s7/docs/plans/phrf-pilot-custody-runbook.md:78), so I classify it as **DEFECT** under the requested criterion. The broader threat model excludes a hostile custodian; this finding concerns the claimed output-binding guarantee.

The identity lifecycle checks were:

- **Established before admission/work:** yes. Initialization writes the ID, then the stamp, before guard admission and accounting opening: [PilotRunner.scala:395](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:395). Normal resumes retain it.
- **Exactly once:** only after the stamp exists. Simulating death between ID creation and stamp creation caused the next run to replace the existing ID. **HARDENING:** no authorization or metered work had yet been established.
- **Atomic:** the individual write uses a forced temporary file and atomic rename. **Durability is conditional:** directory-fsync `IOException` is swallowed at [Fs.scala:46](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/Fs.scala:46). **HARDENING:** no power-loss violation was reproduced.
- **Missing or ordinarily malformed on resume:** both refused before changing cost. Deleting the ID alone does not reopen authorization.
- **Changed to another syntactically valid ID:** accepted on ordinary resume. Restoring a previously authorized ID enables the substitution above; the invocation-number check still applies.
- **Malformed Unicode:** 32 Arabic-Indic zero characters plus newline were accepted because [PilotRunner.scala:820](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:820) uses `isDigit`. **HARDENING:** this violates the documented hexadecimal format but adds no separate demonstrated accounting or authorization failure.
- **Sharing across outputs:** copying the ID alone to a different path still encounters the path-bound stamp. A distinct output recreated at the same path can share the full identity through the substitution demonstrated above.

**NEW, high — DEFECT: `jobFinished` can hide a fatal error before uncertainty becomes sticky.**

A fatal exception escaping `arms.run` is not recorded at its origin: [PilotRunner.scala:615](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:615). Before `runWave` observes it, this executes:

```scala
try cell.arms.foreach(...)
finally arms.jobFinished(job)
```

An ordinary exception from the finalizer replaces the fatal exception: [PilotRunner.scala:668](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:668). The sticky flag consequently sees only an accountable exception.

I used the real `PilotArmRunner`, an engine reporting **37 measured CPU seconds**, and a score feed throwing synthetic `OutOfMemoryError`. A delegating `ArmRunner` supplied `jobFinished` that threw `IllegalStateException("cleanup failed")`. No caller interruption was necessary.

```text
escaping exception     = IllegalStateException: cleanup failed
cost.json               = {"cpu_seconds_total":0,"invocations":1}
accounting.open exists  = false
ordinary resume         = Right(0.0)
```

The public cleanup callback has no no-throw contract: [Arm.scala:98](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/shared/src/main/scala/scalafim/phrfcmp/exec/Arm.scala:98). This loses known measured CPU and silently permits continuation. The stock cache-removal callback was not observed to throw; the failure is demonstrated through the supported runner interface. Fatal uncertainty must be recorded before cleanup can replace the exception.

The previously repaired **DEFECT** protections otherwise retained their behavior:

| Protection | Fifth-round evidence |
|---|---|
| Original H1, changed payload after crash | Both differing scheduled commits survived and remained readable. |
| M1, aggregate collisions | D=15 and D=16 aggregates coexisted. |
| M2, prefix recovery and dispatch markers | Equal-to-top and above-top interrupted jobs finished; D=3 was retained without dispatching new index-3 jobs. |
| M3, lost cost | Missing and malformed cost beside existing work refused. |
| M4, worker joining | Repeated caller interrupts waited for the worker; CPU and interrupt status survived. |
| M5, watchdog | Reran the actual parent-death process-group probe. Watchdog, payload and grandchild disappeared within **0.075 seconds** of parent termination. |
| M6, blob counts | Successful and mixed failure/refusal two-unit runs both produced eight blobs. |
| Environment scrub | Root/ack variables disappeared while a control variable remained; spawn-site checks passed. |
| `accounting.open` lifecycle | Mid-run and failed-checkpoint cases retained the marker. Ordinary commit failure persisted 17 seconds before closure. |
| Ordinary adapter interruption/exceptions | Interrupted execution persisted 37 seconds; ordinary exception/retry cases retained 74/222 seconds. |
| Earlier accounting bounds | Recovery replay remained single-charge; overflow recovery refused; ordinal and retry limits held. |
| Verifier refusals | Missing contributions remained `Unverifiable`; impossible attempts, mappings, units, status/inventory mismatches, malformed lengths, invalid D and ambiguous aggregates refused. Earliest failed commits and discarded aborted contributions retained their behavior. |
| Construction/serialization bypass closure | Shared policy assertions passed; invalid reflective construction rejected; guard, raise and recovery serialization each threw `NotSerializableException`. |

Validation comprised **117 passing test bodies** against the existing compiled implementation, using in-memory filesystem adaptations for filesystem tests, plus the independent probes above. I did not rebuild or run fresh JVM/Scala.js sbt gates. The full `ChildProcess.run` environment probe encountered the sandbox’s `ProcessHandle.descendants` restriction; environment-map and source-site checks completed.

**Overall verdict: REQUEST CHANGES — the output-ID substitution and fatal-error cleanup masking are demonstrated DEFECTs.**