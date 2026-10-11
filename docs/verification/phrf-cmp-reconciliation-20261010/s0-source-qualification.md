# PHRF-CMP S0 reconciliation: source qualification spike

Mote: `bd-01M3VCSEYDYQSG52JT7F0ZG1B4`. Date: 2026-10-10. Worktree branch
`work/phrf-cmp-reconcile`, base origin/main `8e85ef61`.

This record credits the delivered S0 receipt
(`docs/verification/phrf-cmp-s0-20261001.md`, recovered in `bfa66bd5`, sha256
prefix `d176fe1686b1c79d` per the recovery manifest) and maps its exact
acceptance. **The old spike was not rerun, and its observations are not used
as confirmation of anything.** The runner design records S0 as "DONE at
`256cd1b0`" (`docs/plans/phrf-pilot-runner-design.md`, section 6).

## Acceptance mapping (mote body, design fc698d6e section 6 S0)

| Acceptance | Evidence (author-run, 2026-10-01, retained) |
| --- | --- |
| At bound source `8d5b0e51` | The receipt names `8d5b0e51` plus tool commits. `8d5b0e51` is retained in this repository on `origin/migration/20261005/branches/candidate/fir-reviewed-20261003`. Its content landed on main through `3ceba77c` ("Land reviewed PHRF prerequisites from integration 8d5b0e51"). |
| Run the existing ProfileHrf ML executor suites, JVM and JS | Receipt section 1: `ProfileHrfFitSuite`, `ProfileHrfTrialOutputsSuite`, `TrialBandedMlBackendSuite` and `TrialBandedWorkReceiptSuite`, **69/69 on JVM and 69/69 on JS**. This includes "ML executor returns coherent terminal J evidence ...". The cited logs under `/private/tmp/scalafim-execution-20260929/logs/` no longer exist; the counts rest on the receipt and the mote's 2026-10-01 progress note. |
| 40-voxel smoke fit of a condition (Gaussian) fit | Receipt section 2: compact route `fromTrialEvents(alpha = 0)` (`fromFixed` refuses AR), 40 delivered, 33 Accepted / 7 Boundary. Raw log `tools/phrf-comparison/spike/results/s0-smoke-log.txt` (sha256 `a96c30f638c7956d0601cdadac48789f24537d3a4720ca99187e320dee9e2077`, on main). |
| 40-voxel trial `TrialRandomEffectsML` fit | Receipt section 2: route `trial-banded-ml`, 40 delivered (32 Accepted). `ExactShape` public outputs emit 32/40, plus 8 typed `DecodeRefused(Boundary)`. The ExactShape-only guard is shown (`TrialOutputMlIntent` refusal). |
| On one harness-seed generator dataset each | `C-TX-.5__d0000` (`5d44787a...638fe`, reproduced byte-identically today in `v0a-truth-generator-tx-gate.md`) and `T-TX-fast__d0000`; manifests `root_kind=harness`. |
| ONE global pooled AR(1) plan (owner O1) | `WhiteningPlan.global(ArmaCoefficients.ar(rho), ...)` from a common FIR pre-fit, via `CanonicalTemporalWhitening.Shared`. rho_hat 0.284 (condition) and 0.375 (trial). |
| Record timings for the five pilot timing quantities as a preview | Receipt section 4, quantities (a)-(e). (c) is a finding, not a timing: alpha caching is not possible through the public API. |
| No production changes; spike code only under `tools/phrf-comparison/spike/` or a test-scope path | `tools/phrf-comparison/spike/export_flat.py` (`b89c7f53...15ad`) and the log are on main. The spike driver `modules/fit/jvm/src/test/scala/scalafim/fmri/fit/phrfspike/PhrfSpikeSuite.scala` (test scope) was **not** in the recovery scope. It is retained off-main at `d1ec37b3` on `origin/migration/20261005/preserved/consolidation-20261005/b63bda5a2e25` (blob `3e3827e7`, sha256 `424d09f59f276cc67fa51c68ad741be87ed3785a9cc7eb149254b1789d33b53f`). |

Every S0 criterion is evidenced. The receipt's blockers B1 (ML executed) and
B2 (global AR via O1) are closed. B3 (LOROCV held-out prediction) was outside
S0.

## Current-source regression (supplementary; not a rerun of the spike)

These are today's runs of the same four executor suites on main. They show
the suites still pass at current source. They do not re-qualify the
historical bound source.

| Gate | Result |
| --- | --- |
| `fitJVM/testOnly` (four suites) | **71 passed**, 0 failed (`logs/s0-fit-and-phrfcomparison-js.log.gz`) |
| `fitJS/testOnly` (four suites) | **71 passed**, 0 failed |
| `phrfComparisonJS/test` (same batch) | **243 passed**, 0 failed |

The count rose from 69 to 71 because `TrialBandedMlBackendSuite` gained tests
after `8d5b0e51` (`723bc27d`, `4290c126`: stable native ML energy and
work-ledger assertions).

## Historical-only observations (not current claims)

The receipt's 2026-10-02 annotations remain binding. The smoke statuses
(17.5% and 20% non-Accepted), correlations (0.993, 0.901) and preview timings
were computed under a half-TR-shifted `SamplingFrame` and a 0.2 s lowering
grid. They are **superseded and must be re-measured in S10**. Since
`723bc27d` the native ML energy policy also adds per-evaluation work, so the
S0 timings do not describe current source. The FIR pre-fit rho bias it found
(0.375 against 0.3 in trial cells) was resolved by S2's per-kind design with a
corrected estimate (closed 2026-10-06 with the 50-dataset bias gate).

## Scope limits

No pilot timing, no pilot admission, no LOROCV (B3), no PHRF-can and no JS
execution of the smoke are claimed. The spike driver is not on main.

## Verdict

The delivered S0 receipt satisfies the S0 acceptance. **Close**, crediting
the receipt, with the off-main location of the spike driver and the
superseded preview numbers recorded above.
