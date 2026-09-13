# Compiled design ownership repair

Implementation issue: `bd-01M2DEFNKNX6NY113MQA6YJRTM`.
Parent epic: `bd-01KZ3ZCDD2RMYQ3WVY606FNKY1`.
Base commit: `e071e83b3a23bc6f304b1e72625d6281d9cfc9ba`, with existing
uncommitted HRF, fixed-effects, provider-adapter, and other work retained.
This report qualifies the ownership repair, not a clean release candidate.

## Contract and implementation

`DesignSchema.validated` snapshots its input before computing rank evidence and
the fingerprint. The schema and runwise slices retain immutable Gale `DMat`
values. `matrix` remains a detached `Mat` compatibility export; changing its
array cannot alter the compiled design. Repeated readers should use
`matrixValues`, which exposes the existing immutable numerical value.

`DesignBlock` retains that same numerical authority and derives all structural
columns, including combined baseline ordinals, from its schema. Model
construction rejects a source event/baseline matrix that differs from its
compiled schema. Label-only compatibility changes remain supported.

Ordinary and chunked execution select rows directly from the retained Gale
matrix. LSS selects both trial and fixed columns from this matrix as well,
instead of returning to mutable event/baseline arrays. There is no new
fingerprint or QR computation in the per-voxel or per-chunk execution path.
Allocation/timing qualification remains a separate release task.

### API migration

`DesignSchema` and `RunwiseDesignSlice` no longer expose unchecked case-class
copy construction. Rebuild numerical/schema changes through
`DesignSchema.validated`; use `withRenderedLabels` for presentation changes and
`runwiseSlice` for a checked run projection. Two existing LSS permutation
fixtures were corrected to transport their structural columns and rebuild the
schema with the permuted matrix. Previously they retained the old schema.

## Verification

Before the repair, all three new `DesignSchemaOwnershipSuite` tests failed on
the JVM: input/export aliasing, relabel/export aliasing, and runwise-slice
aliasing. The retained value was changed to 42 by each mutation probe.

After the repair, both ownership suites pass on JVM and Scala.js (six tests
per platform). They cover input and compatibility-export mutation, combined
and relabeled schemas, runwise slices, compiled hypotheses, ordinary/chunked
OLS, chunked LSS, and typed model rejection of stale source schemas.

The full `designJVM/test`, `designJS/test`, `modelJVM/test`, and `modelJS/test`
gates pass: 241 design tests and 28 model tests per platform. The full fit
gates retain the three numerical failures listed below: JVM 322/325 pass and
Scala.js 310/313 pass. They are not green release evidence. The LSS permutation
scenario passes after its fixture repair.

Commands used, in bounded sbt processes:

```sh
sbt designJVM/test modelJVM/test fitJVM/test
sbt designJS/test modelJS/test fitJS/test
```

The local invocations also set `sbt.global.base`, `sbt.boot.directory`, and
`sbt.ivy.home` under `/private/tmp/scalafim-fruit-sbt` and disabled the sbt
supershell. Normal runs use the build's pinned providers without source
overrides. Temporary logs are `/tmp/scalafim-identity-red.log`,
`/tmp/scalafim-identity-jvm.log`, `/tmp/scalafim-identity-fit-jvm-final.log`,
and `/tmp/scalafim-identity-js.log`.

### Independent baseline isolation

The three failing scenarios were rerun with temporary sbt source overrides
using the original HEAD versions of exactly these production files:

- `DesignSchema.scala`
- `DesignBlock.scala`
- `MatrixAdapters.scala`
- `FitPlanExecutor.scala`

All other worktree sources, including the existing SPMG correction, were kept
the same. The override run failed the same three scenarios, with all 33 failing
observations identical to the repaired run. Its log is
`/tmp/scalafim-identity-baseline.log`. Ordinary sbt settings were restored for
the final verification; no workspace source was reverted for the comparison.

| Scenario | Representative unchanged failure | Follow-up |
| --- | --- | --- |
| S07 structural 2x2 | Simple effect 0.4705485415038636 versus planted 0.6 | `bd-01M2DFDCYFN7HFA2W1188RD8HA` |
| DMS | Sample statistic 462.2342450591221 versus historical R 980.8491718335549 | `bd-01M2DFDN0TRN1JVS7RA1BBZ4C6` |
| S15 realistic nuisance | Task-a estimate 7.58460388165137 versus historical R 0.7497435200650893 | `bd-01M2DFDXM8JR8HMKJAJE60C6JW` |

See `docs/audits/spmg-correction.md`: the adopted SPMG change is a breaking
scientific correction and does not recertify historical receipts. These tasks
preserve historical evidence, require independent corrected references, and
forbid weakening tolerances or reverting the corrected basis to obtain a pass.

## Remaining work

P4.4 (`bd-01KZ3ZPJ89W7W86084JQRDV7JD`) contains thirteen bounded tasks. Each
has exact files, steps, acceptance commands, and scope exclusions in Mote.

| Task | Issue | Prerequisites |
| --- | --- | --- |
| a. Bounded test inventory | `bd-01M2DERP7G58K5YCXKK3PG8QCZ` | None |
| b. Moved scenario registrations | `bd-01M2DERTPJGR463AS4D5TF45H8` | None |
| c. Declared reference-lock validation | `bd-01M2DERZ859NP6T3ZC7S90BE81` | None |
| d. External evidence validation contract | `bd-01M2DES3V5BZAJY7XW9ZRWQ1X6` | None |
| e. Source-bound local gate receipts | `bd-01M2DES8BB3FHPYMT86RRPQC40` | a, d |
| f. CI and regeneration evidence collection | `bd-01M2DESFGR65HYYHXR62V2RY67` | d |
| g. Branch-policy evidence collection | `bd-01M2DESP42AKSVD77PEZ1Z0PJQ` | d |
| h. Acceptance-to-evidence mapping | `bd-01M2DESX94BQMCZ5Z863J51XNX` | b, c |
| i. Candidate performance evidence | `bd-01M2DET4MAE97QS80B6D2R0FR4` | e, ownership repair |
| k. S07 noisy/planted oracle | `bd-01M2DFDCYFN7HFA2W1188RD8HA` | b |
| l. Corrected DMS reference | `bd-01M2DFDN0TRN1JVS7RA1BBZ4C6` | b, c |
| m. Corrected nuisance reference | `bd-01M2DFDXM8JR8HMKJAJE60C6JW` | l |
| j. Final clean-candidate qualification | `bd-01M2DETCQFVACPV4FHP4GQ8HM0` | Every preceding task and ownership repair |

The parent epic, Phase 4, and P4.4 stay open. The final task must bind all
results to one admitted clean candidate and may close those milestones only
when their original acceptance criteria pass.
