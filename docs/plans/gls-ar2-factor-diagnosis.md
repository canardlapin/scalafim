# Corrected GLS: AR(2) factor diagnosis and mitigation confirmation

Follow-up `bd-01M4E9Q1YRN88KS4BCK5N912YA` was authorized after the
[original six-cell campaign](corrected-gls-statistical-qualification.md).
Its short, censored, high-nuisance voxelwise AR(2) result remains failed and
immutable. This follow-up measures the confounded factors and tests declared
existing-configuration mitigations. No replacement degrees-of-freedom formula
or production estimator change is proposed before diagnosis.

The [protocol](../../tools/scenarios/gls-factor-diagnosis/protocol.json) and
[declaration receipt](../verification/gls-ar2-factor-diagnosis-20261008/protocol-declaration.json)
were frozen before diagnostic or mitigation outcomes. Root 973860617 and all
declared data streams were checked for overlap with the previous campaign's
declared streams; none overlap. QA, pilot, diagnostic, confirmation and bootstrap
use distinct domains. The portable generator and all original scientific
margins are retained.

## Paired diagnosis

The complete factorial grid has 24 cells: AR(1)/AR(2), zero/twelve nuisance
columns, complete/censored runs, and global/run/voxelwise pooling. Runs remain
96/144 frames with four independent response columns and the same planted task,
intercept and nuisance coefficients. Public known-phi and one-pass OLS-corrected
GLS fits supply public structural t/F contrasts and interval coverage.

There are 512 independent innovation blocks. Each block is reused across all
24 cells; different AR orders filter the same innovations. Each cell therefore
has 512 independent replicate datasets, while the whole grid is a paired
experiment with 512 blocks. Cell-engine fits and response columns do not
multiply the independent count. The pilot uses eight separate blocks to verify
runtime and recording. Neither profile confers scientific admission.

Besides the existing endpoints, every fit retains public variance scale,
normalized task-a covariance geometry and residual df. The engineering gate
checks that their product reproduces the public contrast variance and that
reported df matches the declared design. The variance-ratio identity separates
changes in reported variance from changes in empirical sampling variance:

```
corrected calibration ratio = known-phi calibration ratio
  * (mean corrected reported variance / mean known-phi reported variance)
  / (corrected estimator variance / known-phi estimator variance)
```

Dataset bootstrap resamples the same replicate indexes across paired cells and
engines, with 1,999 resamples and 90% intervals. AR order, nuisance load,
censoring, voxelwise-versus-global and run-versus-global effects are reported
both averaged over their matched cells and in the AR(2), high-nuisance, censored
context. Mechanistic attribution requires measured effects and decomposition;
a failing interval alone does not identify a cause.

Covariance estimation accuracy is a recognized requirement for prewhitening;
Woolrich and colleagues studied estimation and constrained spatial smoothing
in their [primary FMRIB report](https://www.fmrib.ox.ac.uk/datasets/techrep/tr01mw1/tr01mw1/index.html).
That motivates testing pooling, but does not establish that pooling is valid
for heterogeneous noise or that it explains this result. Those claims require
the present measurements or another explicitly declared study.

## Fresh mitigation confirmation

All three cells below were declared before diagnostic outcomes, and all will
run regardless of what diagnosis finds. Each has 2,048 fresh datasets, the
original high nuisance load and censor rule, and known-phi controls.

| Mitigation | Run lengths | Pooling | Noise truth |
| --- | --- | --- | --- |
| ar2-high-censored-global | 96/144 | global | Same AR(2) across four columns |
| ar2-high-censored-run | 96/144 | run | Same AR(2) across four columns |
| ar2-high-censored-voxelwise-long | 192/288 | voxelwise | Same AR(2) across four columns |

Short pooling cells share 2,048 fresh innovation blocks deliberately. The long
cell uses a separate 2,048-block stream. Longer runs keep six DCT terms per run
and extend the same task/censor rule; this mitigation changes relative nuisance
burden and acquisition duration together. It does not isolate a pure duration
effect. Longer-run t/F thresholds are fixture-generated in base R before fits.

The order factor compares the two declared AR truth scenarios: rho 0.5 and
phi=(0.45,-0.10). It changes coefficient values and autocorrelation spectrum as
well as order. It cannot identify a pure penalty from fitting an additional
coefficient to otherwise identical noise. Pooling, nuisance and censor effects
are isolated within each fixed truth scenario; any order attribution retains
this limitation.

Acceptance retains original AR bias/RMSE, whiteness, exact 90% binomial
equivalence, 90% bootstrap variance-ratio and standardized effect-bias limits.
Every fit and refusal stays in the receipt. Both platforms and independent R
analyses are required. A passing mitigation supports only its declared model
and noise homogeneity, not general voxelwise inference or the original failed
case. Failed mitigations remain failed, and spent confirmation streams are not
reused as fresh validation.

Run from the repository root:

```sh
python3 tools/scenarios/gls-factor-diagnosis/run_study.py \
  --profile diagnostic --output /private/tmp/scalafim-gls-factor-diagnostic
python3 tools/scenarios/gls-factor-diagnosis/run_study.py \
  --profile confirmation --output /private/tmp/scalafim-gls-factor-confirmation
```

The runner binds a fresh idle sbt server's environment, retains both targets
even if one rejects a gate, binds main consumer/test/tool sources and performs
separate R analyses. Confirmation returns nonzero if any declared scientific
cell or retained-data analysis fails. Routine tests keep campaigns explicit;
no statistical failure becomes a passing scientific verdict through a profile
selection change.

## Recorded outcome

The [complete report](../verification/gls-ar2-factor-diagnosis-20261008/README.md)
retains the full paired diagnostic and both-platform confirmations. Voxelwise
estimation with high nuisance load has recovery/calibration pressure under both
declared truth models, including complete runs. The target paired comparison
supports improved recovery, variance calibration and F rejection from pooling;
its coverage difference remains uncertain. The known-filter paired censor
comparison also identifies a contribution from the restart approximation.

Fresh global and run pooling confirmations passed every original margin for
the homogeneous-noise setup. Doubled-run voxelwise confirmation improved RMSE
and coverage but failed joint-F equivalence: 148/2048, 90% interval
[0.063069,0.082384]. All known-filter controls passed, no result was dropped and
both platforms agreed. The complete mitigation campaign remains failed, and
the original short voxelwise adverse result remains binding.

The original six-cell pilot replay preserved all 288 fit outputs and decisions
per platform within 6e-15, with eight tests passing on each target. Both factor
pilots/diagnostics passed 27 engineering/control tests per target. Five GLS
Scala files were formatted on JVM and JS with unchanged source bytes; unrelated
merged PHRF files were left at their committed content. This work does not
claim a new full-module routine-suite gate for the separately merged checkpoint.

Follow-up `bd-01M4EMEBCBMBHA7BF4BQY6AFPB` tracks a justified estimated-covariance
uncertainty treatment and a continuity-aware censor/noise policy. No numerical
indexing defect was identified and no production estimator, df or standard-error
formula was changed as a guessed remedy.
