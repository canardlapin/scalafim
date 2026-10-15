# Corrected GLS: bounded known-truth qualification

Mote `bd-01M4E3E9KYX39Q2H6GSZMB3H5G`, authorized 2026-10-08. The broader
first-level calibration campaign `bd-01M21VA8DYMNJS9PZ8076SBWWJ` retains its own
frozen runs, seeds, effective-df/group criteria and admission rules. This slice
exercises the new one-pass OLS correction and does not resume that campaign.

The authoritative pre-outcome declaration is
[`protocol.json`](../../tools/scenarios/corrected-gls/protocol.json), hash-bound by
[`protocol-declaration.json`](../verification/corrected-gls-qualification-20261008/protocol-declaration.json).
No simulation outcomes were read before declaring the case grid, seed domains,
replicate counts and scientific margins. A source-inspection amendment corrected
the covariance convention before execution: noise is continuous within a run;
whitening restarts at censor gaps, so known-phi whitening is a known-parameter
comparator whose censored inference is measured, not presumed exact.

## Frozen experiment

Six cells use runs of 96 and 144 samples, TR 1 s, four independent response
columns, two alternating SPMG1-convolved task epochs, run intercepts, and either
zero or twelve run-local DCT nuisance regressors. AR truth is rho 0.5 or
phi=(0.45,-0.10); Gaussian innovation variance is one and each independent run
has 256 burn-in draws. Censor masks are deterministic and independent of noise.
Censored rows remain on the fitting axis, matching the current API contract.

| Cell | AR order | Predictors | Censoring | Estimated noise pooling |
| --- | --- | --- | --- | --- |
| ar1-low-global | 1 | 4 | none | global |
| ar1-high-global | 1 | 16 | none | global |
| ar1-high-censored-run | 1 | 16 | isolated/adjacent frames | run |
| ar2-low-global | 2 | 4 | none | global |
| ar2-high-global | 2 | 16 | none | global |
| ar2-high-censored-voxelwise | 2 | 16 | isolated/adjacent frames | voxelwise |

Each dataset goes through public `FitPlanExecutor.fitDense` as known-phi, raw
estimated AR, and `ArBiasCorrection.Ols` estimated AR. Public structural t and
joint two-row F contrasts retain coefficient identity and public uncertainty.
Voxel 0 supplies the two-task null; voxel 1 supplies a task-a coefficient of 0.75
for 95% interval coverage. Replicate datasets, not response columns or contrast
rows, are the independent sampling units. Fit refusals are retained and fail the
complete-cell gate; they are never dropped from denominators.

Seed domains are disjoint: QA=0, pilot=1, screen=2, confirmation=3, bootstrap=4.
The immutable default root is derived from SHA256 of the declaration label.
The portable generator is Park-Miller plus Box-Muller; no platform global RNG is
used. A configured law seed identifies a separately reported confirmation root.

## Execution and acceptance

Pilot uses 16 datasets per cell to measure fit costs; screen uses 64 for bounded
engineering regression. Neither can confer scientific admission. Confirmation
uses 2,048 independent datasets per cell. The qualification campaign requires
`SCALAFIM_GLS_STUDY_PROFILE=pilot|screen|confirmation`; the existing
`SCALAFIM_LAW_PROFILE=calibration` also selects confirmation. Ordinary tests run
simulator QA without registering the qualification campaign. This registration
change followed the adverse result and is recorded in the receipt; no scientific
bound, dataset, seed, or gate was changed.

Before fits, QA checks Gaussian mean/variance, stationary AR autocovariance and
independent run boundaries against analytic theory, plus noiseless coefficient
recovery through the public OLS executor. Bounds are in the protocol. The full
known-phi confirmation is an inference control; failures remain in the receipt.

Corrected and known-phi confirmation require:

- every dataset retained, finite outputs, and `Applied` correction outcomes for
  every corrected run/voxel;
- corrected AR mean-bias 90% intervals inside +/-0.04 and RMSE at most 0.10;
- mean absolute residual ACF over lags 1..4 at most 0.10, with corrected excess
  over the known-phi comparator at most 0.025;
- 90% exact Clopper-Pearson intervals for nominal 5% t/F rejection inside
  [0.02,0.08], and 95% interval coverage inside [0.92,0.98];
- 90% dataset-bootstrap variance-ratio intervals inside [0.80,1.20], with 1,999
  resamples; and standardized effect-bias intervals inside [-0.10,0.10].

The exact confirmation rejection-count range is 53..143; coverage is 1905..1995.
These are the 90% interval equivalence/TOST criteria, not post-outcome tolerance
adjustments. Raw results are paired descriptive comparisons, never required to
be worse. Effective df by variance moments and inverse moments, and effect-square
versus reported-variance correlation, are descriptive endpoints; this slice does
not install an effective-df formula or qualify variance/effect independence.

R thresholds are generated before outcomes with base-R t/F and beta quantiles.
The binomial quantile/CDF conventions follow the [R statistics documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Binomial.html).
`analyze.R` recomputes statistics from all retained replicate records, applies
exact interval and bootstrap equivalence decisions, and checks paired-platform
numeric outputs at 1e-9 scaled error with identical reject/coverage decisions.

Each executable cell returns one `ScenarioResult`; CI requires clean `Pass`.
Scientific admission additionally requires the complete confirmation analysis,
including bootstrap equivalence and simulator controls. A screen pass is only a
regression-screen pass. Reports retain adverse outcomes, source hashes, wall time,
and plots with a recorded visual disposition. No bound or seed changes follow
observed outcomes.

## Observed result: complete campaign, qualification failed

The [confirmation receipt](../verification/corrected-gls-qualification-20261008/README.md)
retains 2,048 datasets per cell, 12,288 datasets and 36,864 paired-engine fits per
platform. All simulator controls and known-phi controls passed. No fit refusal or
non-`Applied` corrected outcome was dropped. Both platforms produced identical
rejection and coverage decisions; maximum scaled numeric disagreement was
9.98e-15. Base-R exact-interval and dataset-bootstrap analysis independently
returned the same gates for both platforms.

| Corrected cell | Null t count | Null F count | Covered | AR RMSE | Verdict |
| --- | ---: | ---: | ---: | ---: | --- |
| ar1-low-global | 105 | 94 | 1924 | 0.0289 | Pass |
| ar1-high-global | 111 | 112 | 1920 | 0.0365 | Pass |
| ar1-high-censored-run | 118 | 129 | 1910 | 0.0524 | Pass |
| ar2-low-global | 108 | 120 | 1920 | 0.0326 | Pass |
| ar2-high-global | 89 | 109 | 1936 | 0.0360 | Pass |
| ar2-high-censored-voxelwise | 129 | 159 | 1897 | 0.1063 | Fail |

Counts have denominator 2,048; the linked machine receipt retains full precision. Passing individual cells
supports only those declared designs and noise settings, not neighboring regimes
or general inference admission.

The final cell failed three frozen criteria. AR recovery RMSE was 0.1062507,
above 0.10. The null-F rejection rate was 7.7637%, with a 90% exact interval
[6.8119%,8.8063%] extending beyond the 8% upper margin. Coverage was 92.6270%,
with a 90% exact interval [91.6066%,93.5555%] extending below the 92% lower margin.
Its t rejection, AR mean bias, whiteness, effect bias and variance-ratio bootstrap
criteria passed. Its variance ratio was 0.90986, interval [0.86608,0.96184].

The grid confounds voxelwise pooling, censoring, nuisance load and AR order in
this cell. It cannot identify which factor caused the failure. The next study
must isolate those factors and declare fresh diagnostic/confirmation seeds
before testing a remedy. The complete campaign remains `scientific_pass=false`;
the original confirmation streams are spent.

To reproduce an explicit campaign from the repository root:

```sh
python3 tools/scenarios/corrected-gls/run_study.py \
  --profile confirmation --output /private/tmp/scalafim-corrected-gls-replay
LC_ALL=C Rscript tools/scenarios/corrected-gls/analyze.R \
  /private/tmp/scalafim-corrected-gls-replay/records-jvm.jsonl \
  /private/tmp/scalafim-corrected-gls-replay/analysis \
  /private/tmp/scalafim-corrected-gls-replay/records-js.jsonl
```

The runner retains both targets even when a gate fails and exits nonzero for
this frozen confirmation. Replaying the same seeds checks reproducibility; it
does not create independent validation evidence.
