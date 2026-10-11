# Corrected GLS confirmation — 2026-10-08

**The complete scientific qualification failed.** Five corrected cells passed
every frozen criterion. The censored, high-nuisance, voxelwise AR(2) cell failed
recovery RMSE, joint-F rejection and interval coverage. All six known-phi controls
passed. The AR implementation audit is closed; this result limits inference
claims for the implemented estimator.

The [protocol and result ledger](../../plans/corrected-gls-statistical-qualification.md)
describes the public fit/contrast path, planted coefficients, stationary Gaussian
noise, case grid, independent sampling units and scientific margins.
[protocol-declaration.json](protocol-declaration.json) binds the pre-outcome
declaration to SHA256 `5d046b21b8398697f6a07fbefb365dfa45489dc7446881c82282019b98f58d5e`.
The confirmation root is 1149777122, domain 3, with 2,048 independent datasets per
cell. Response columns and paired engines do not multiply the independent count.

## Complete retained execution

Each platform retained 12,288 independent datasets across six cells and 36,864
fits across known-phi, raw and corrected engines. Both simulator controls passed.
There were zero fit refusals and every corrected measurement required `Applied`
outcomes. No attempted dataset was omitted. Scientific gate rejection produced
exit code 1 on both targets, with seven tests passing and one failing per target.

[confirmation/run.json](confirmation/run.json) binds the exact source hashes and
source base `e0fc72b5c4ad72d9a75167708eedef6e7d192d02`; all measured sources stayed
unchanged during the run. JVM fitting/QA took 248 seconds, Scala.js 484 seconds.
Target wall time including startup was about 338 seconds and 485 seconds,
respectively. The full target commands and timestamps are in that receipt.

[confirmation/summary.json](confirmation/summary.json) contains independent
base-R exact Clopper-Pearson and dataset-bootstrap analysis plus paired-platform
checks. [summary-js.json](confirmation/summary-js.json) independently analyzes
Scala.js records and returns identical gate verdicts. Maximum scaled disagreement
was 9.972e-15 against a 1e-9 bound, with identical dataset keys and all rejection/
coverage decisions. The paired platforms are reproducibility checks on the same
data, not an additional independent confirmation cohort.

| Failed endpoint | Observed | Frozen gate |
| --- | --- | --- |
| AR recovery RMSE | 0.1062507 | At most 0.10 |
| Null joint-F rejection | 159/2048 = 7.7637%; 90% exact interval [6.8119%,8.8063%] | Entire interval inside [2%,8%] |
| 95% interval coverage | 1897/2048 = 92.6270%; 90% exact interval [91.6066%,93.5555%] | Entire interval inside [92%,98%] |

The point rates alone do not establish equivalence. This cell passed its t
rejection, AR mean bias, residual whiteness, standardized effect bias and
variance-ratio criteria. Its reported/empirical variance ratio was 0.9098573,
90% bootstrap interval [0.8660834,0.9618387]. Raw results are descriptive paired
comparisons; their `scientific_pass` fields do not confer admission.

The six-cell grid cannot separate the causal contributions of censoring,
voxelwise pooling, nuisance burden and AR order. Follow-up mote
`bd-01M4E9Q1YRN88KS4BCK5N912YA` tracks factor isolation and remedy qualification.
No inference formula or production behavior was changed based on this outcome.
This evidence does not qualify group inference, effective-df corrections,
variance/effect independence, robust/iterative corrected fitting or ARMA fitting.

## Evidence and replay

- [confirmation/artifacts.json](confirmation/artifacts.json) hashes retained
  logs, records, summaries, plot and the frozen source archive.
- `confirmation/records-{jvm,js}.jsonl.gz` retain all replicate outputs and
  executable per-cell results. `replicate-records.json.gz` retains the R parser's
  normalized records. Timing fields are descriptive and excluded from numeric
  parity comparisons.
- `confirmation/{jvm,js}.log.gz` retain the positive simulator controls, complete
  fit counts and adverse assertions. `frozen-source.tar.gz` retains every
  source file bound by `run.json` before post-outcome test registration changed.
- [confirmation/qualification.png](confirmation/qualification.png) plots exact
  coverage/t-rejection intervals, bootstrap variance ratios and AR mean bias.
  [visual-review.json](confirmation/visual-review.json) records visual inspection.
- `pilot/` retains the 16-dataset runtime/engineering pilot, including paired
  records and visual corrections. `pilot-v1-harness-failure.log.gz` retains an
  ACF measurement harness failure; `pilot-v2-profile-mismatch.log.gz` retains
  the unintended JS screen caused by missing environment access. Both were
  repaired before confirmation. Neither pilot nor screen supplies scientific
  admission.

After confirmation, the complete qualification suite was made an explicit
campaign via the study profile or law calibration profile. Ordinary tests run
simulator QA; the explicit campaign retains all six original cells and the
negative scientific gate. This is a post-outcome engineering registration
change, not a revised scientific criterion. The frozen source archive records
the earlier default registration, and the registration replay verifies the
same pilot results through the final harness. Routine CI results and campaign
admission are reported separately in
[registration-disposition.json](registration-disposition.json).

[summary.json](summary.json) separates the complete negative scientific verdict
from the final engineering checks. The full affected module passed 86 routine
tests on JVM and 86 on Scala.js. Scala.js reports one ignored placeholder for
the inactive qualification suite: a targeted probe reproduced 0 tests/1 ignored
on JS and 0 tests on JVM, retained in `routine-checks/inactive-campaign.log.gz`.
All six scientific cells were executed in the explicit confirmation campaign.
Both `Test/scalafmtCheck` targets passed. The complete commands, times and log
hashes are in [routine-checks/checks.json](routine-checks/checks.json). The
post-registration 16-dataset pilot replay passed both targets with identical
decisions and maximum scaled numeric difference 5.784e-15 from the original
pilot; [registration-replay/check.json](registration-replay/check.json) records
that reproducibility check. These positive engineering checks do not revise
the negative qualification result.

Use `tools/scenarios/corrected-gls/run_study.py` for both platforms and
`analyze.R` for exact-interval/bootstrap analysis as shown in the linked plan.
Keep existing output directories immutable. Reusing the frozen seed is a
reproducibility check, not fresh validation. The original confirmation streams
are spent and its overall `scientific_pass=false` remains binding.
