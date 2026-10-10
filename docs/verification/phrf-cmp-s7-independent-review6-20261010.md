# Independent S7 targeted review (Codex, read-only), 2026-10-10

Reviewer: OpenAI Codex CLI 0.161.0 via `codex exec -s read-only`. Scope: fifth-review fixes in `6cadeb61..e906cb2c`. Verbatim output follows.

**D1 is PARTIAL; D2 is FIXED. One DEFECT remains.**

I read review 5 first and all eight touched files in full. I ran independent probes against the cached compiled implementation plus **26 targeted regression bodies: 26 passed**. Filesystem fixtures and probe compilation stayed in memory; no files were modified. I did not rerun sbt or Scala.js gates.

| Item | Verdict | Classification |
|---|---|---|
| D1: exact six-step ID-only substitution | **FIXED** | Reported DEFECT resolved |
| D1: consistent substitution of ID and plaintext stamp | **NOT FIXED** | **DEFECT remains** |
| Existing-ID preservation and ASCII validation | **FIXED** | HARDENING completed |
| D2: fatal error masked by cleanup | **FIXED** | Reported DEFECT resolved |
| Targeted prior protections | Retained | No additional DEFECT reproduced |

**D1 — the sealed stamp is not compared before admission.** The exact reproducer now gives:

```text
original raise                 Right(2)
fresh output                   Right(1)
reuse before substitution      Left(CeilingNotAuthorized(...))
reuse after ID substitution    Left(StampMismatch(output_id))
cost unchanged                 true
additional arm calls           0
accounting.open                absent
both sealed stores readable    true
```

Both plaintext and sealed stamps contain `output_id`. The mismatch also wins when `cost.json` is malformed and `accounting.open` exists, confirming refusal before accounting or guard admission.

However, after that refusal, I changed **only `stamp.json`’s `output_id` field** to agree with the substituted ID, leaving the fresh cost history and sealed blobs intact:

```text
reuse after consistent edit    Right((2,12.0))
CPU total                      6 → 12 seconds
additional arm calls           2
old authorization recorded     yes
archived store readable        true
fresh store unseal             differing duplicate plaintext
```

[The comparison at PilotRunner.scala:399](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:399) checks plaintext against an effective stamp derived from the current ID file. It does not authenticate either against the existing sealed stamp. The conflicting sealed copy is detected only afterward, during unseal.

This remains a **DEFECT** against the [runbook’s prohibition on reusing authorization for another output](/Users/bbuchsbaum/code/scala/sf-phrf-s7/docs/plans/phrf-pilot-custody-runbook.md:78). Later detection does not prevent the unauthorized invocation or CPU expenditure. Admission needs an independently authenticated output binding.

The hardening checks passed: an ID surviving the ID-before-stamp crash window was preserved with **zero supplier calls**. Missing IDs, incorrect lengths, uppercase, Arabic-Indic digits, nonhex characters, missing newline, CRLF and extra newline all refused without cost changes.

**D2 — the requested cleanup probe passes.** With the real `PilotArmRunner`, 37 measured seconds, a feed throwing `OutOfMemoryError`, and cleanup throwing `IllegalStateException`:

```text
escaping throwable             original OutOfMemoryError
suppressed throwable           IllegalStateException
cost.json                      CPU 0, invocation 1
accounting.open                 retained
ordinary resume                Left(AccountingUncertain(...))
```

[The job-body catch and cleanup handler](/Users/bbuchsbaum/code/scala/sf-phrf-s7/modules/phrf-comparison/jvm/src/main/scala/scalafim/phrfcmp/exec/PilotRunner.scala:688) record uncertainty before cleanup and preserve the original throwable. Additional fatal injections at timing callbacks, live CPU checks, commit hooks, checkpoint-clock reads, cleanup and completion marking all retained the marker and blocked ordinary resume.

Ordinary feed exceptions persisted **74/222 seconds** without/with retries. Ordinary cleanup failure persisted **37 seconds** and closed accounting. Feed interruption, including interruption plus throwing cleanup, returned `Interrupted`, persisted **37 seconds**, and closed accounting.

Targeted regressions also retained stamp/key/path mismatch handling, failed-checkpoint and mid-run markers, recovery sealing, 17-second commit-failure accounting, repeated-interrupt joining, and fatal-plus-caller-interrupt uncertainty.

**REQUEST CHANGES.**