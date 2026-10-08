# GLS factor diagnosis and mitigation confirmation — 2026-10-08

**Global and run pooling met the declared confirmation margins in the tested
homogeneous-noise setup. Voxelwise inference remains limited:** the original
short-run failure is unchanged, and doubled-run voxelwise estimation failed the
joint-F equivalence gate. The complete three-mitigation campaign has a negative
overall verdict. No production estimator or uncertainty formula was changed.

The [frozen plan](../../plans/gls-ar2-factor-diagnosis.md) and
[protocol declaration](protocol-declaration.json) bind protocol SHA256
`cb6b6ad72cdbbff293662bc48b9f7df66a779f12e99cf15b5246e4e9e92cb1e3`, root
973860617, cases, streams, budgets and original margins before outcomes.
The original [six-cell adverse receipt](../corrected-gls-qualification-20261008/README.md)
remains binding. Diagnostic and confirmation data streams do not overlap its
declared streams.

## What the paired diagnostic localized

The full 24-cell factorial retained 512 independent innovation blocks, reused
across two AR truth models, two nuisance loads, two censor policies and three
pooling strategies. Each cell has 512 independent replicate datasets. The
grid has 512 paired blocks, not 12,288 independent experiments. Both platforms
retained all 24,576 fits, with zero refusals; all 27 engineering/simulator tests
passed on each target. Main outputs agree within 8.54e-15, additional public
variance fields within 1.96e-15, and every rejection/coverage decision matches.

[diagnostic/factors.json](diagnostic/factors.json) retains paired dataset
bootstrap estimates and 90% intervals. [diagnostic/summary-jvm.json](diagnostic/summary-jvm.json)
and [summary-js.json](diagnostic/summary-js.json) independently analyze the full
records. Diagnosis confers no scientific admission; intervals are descriptive
localization evidence without a multiple-comparison qualification claim.

| AR(2), high nuisance | AR RMSE | Reported / empirical variance | Null F rejection |
| --- | ---: | ---: | ---: |
| Complete, voxelwise | 0.1044 | 0.9027 | 7.03% |
| Censored, global | 0.0399 | 0.9374 | 5.27% |
| Censored, run | 0.0555 | 0.9237 | 5.08% |
| Censored, voxelwise | 0.1090 | 0.8791 | 7.42% |

Recovery RMSE also exceeded 0.10 for high-nuisance voxelwise AR(1) in both
complete and censored layouts. The pressure is not confined to AR(2) or created
solely by censoring. The AR-order factor compares different coefficient truths
and spectra, so it does not isolate a pure fitted-order penalty.

In the censored AR(2), high-nuisance context, voxelwise minus global pooling
gave:

- calibration ratio difference -0.0583, interval [-0.0864,-0.0304];
- RMSE difference +0.0691, interval [0.0675,0.0707];
- F rejection difference +0.0215, interval [0.00586,0.0371];
- coverage difference -0.00781, interval [-0.01758,0.00195], which includes zero.

The latter distinction matters: these data support recovery/calibration/F
effects, while this particular paired coverage contrast remains uncertain.

## Variance decomposition and code inspection

Public variance scale times public normalized task-a covariance reproduced
the public contrast variance on every measured fit. Voxelwise covariance
selection and assembly did not show a numerical indexing error. The fitting
code executes OLS inference after whitening by the estimated filter; its
reported uncertainty is conditional on that filter. It does not include a
separate covariance-estimation uncertainty treatment.

For the target voxelwise cell, mean reported variance was 0.94933 of the
known-filter comparator, interval [0.93455,0.96500]. Empirical estimator variance
was 1.02832 of the comparator, interval [0.99896,1.05743]; that increase remains
uncertain in this censored context. Without censoring, the sampling-variance
ratio was 1.04444 [1.01708,1.07228] and the reported-variance ratio was 0.94806
[0.93319,0.96446]. Both effects contribute to undercalibration in that complete
layout. Public scale and covariance geometry means were also below their
known-filter counterparts. These measurements support a finite-sample
estimated-filter inference limitation rather than an indexing repair.

Censoring changed **known-filter** calibration by -0.04223, paired interval
[-0.06190,-0.02321]. This effect occurs without AR estimation. The generator's
noise remains continuous within runs while the current censor policy restarts
whitening at gaps and retains censored response rows on the fitting axis.
That known-parameter comparator is an explicitly approximate covariance model;
the paired control shows that restart behavior contributes separately.

## Fresh confirmation verdicts

All three mitigations were declared before diagnostic outcomes and all were
executed on both platforms. Each case has 2,048 fresh datasets. Global/run cases
share one 2,048-block stream deliberately; the long case uses a separate stream.
There are 4,096 independent confirmation innovation blocks, 6,144 cell-datasets
and 12,288 fits per platform. No fit was dropped. All known-filter controls
passed. Both platforms rejected the same single scientific gate; main numeric
agreement is within 2.04e-14 with identical decisions.

| Corrected configuration | AR RMSE | Null F count | 95% intervals covered | Verdict |
| --- | ---: | ---: | ---: | --- |
| Global, runs 96/144 | 0.03841 | 108 | 1930 | Pass |
| Run pooling, runs 96/144 | 0.05356 | 122 | 1926 | Pass |
| Voxelwise, runs 192/288 | 0.06998 | 148 | 1929 | Fail: joint F |

Counts have denominator 2,048. The joint-F acceptance range remains 53..143;
coverage remains 1905..1995. Longer-run voxelwise F rejection was 7.2266%,
90% exact interval [6.3069%,8.2384%], extending above the 8% margin. Its other
frozen criteria passed. The complete grid remains `scientific_pass=false`.

Independent [confirmation/summary-jvm.json](confirmation/summary-jvm.json) and
[summary-js.json](confirmation/summary-js.json) include all exact intervals,
bootstrap variance ratios, control outcomes and adverse gates. Both target
processes exit 1 for the retained scientific failure. This result was not
converted into a passing engineering or scientific claim.

## Available mitigation and remaining work

For the specific homogeneous AR(2) truth, design, censor policy and four
independent response columns tested here, existing global or run pooling met
all original scientific margins. The public configuration uses the ordinary
GLS executor and one-pass OLS residual-bias correction:

```scala
ArOptions(
  structure = ArStructure.Ar(2),
  global = true,
  censoredTimepoints = censoredFrames,
  biasCorrection = ArBiasCorrection.Ols
)
```

Set `global = false` and retain `voxelwise = false` for the validated run-pooling
configuration. Pooling across heterogeneous AR noise is not validated by this
study. Passing these margins does not establish exact nominal calibration,
group inference, variance/effect independence or general corrected-GLS
admission. Longer duration alone did not qualify voxelwise inference.

Follow-up `bd-01M4EMEBCBMBHA7BF4BQY6AFPB` tracks a scientifically justified
estimated-covariance uncertainty treatment and a continuity-aware censor/noise
policy, with homogeneous/heterogeneous truths and fresh declared validation
streams. No guessed df or standard-error multiplier was installed. The original
and present adverse confirmation streams are spent.

## Receipts and visual review

`diagnostic/` and `confirmation/` retain exact source archives, source/command
receipts, both complete compressed record streams, positive controls and adverse
logs. Source base was `4276ee59`; measured consumer, test and analysis sources
remained unchanged within each run. The pre-outcome declaration was made at
`81c7e1f6`; an unrelated PHRF checkpoint was merged before execution. No PHRF
statistical campaign was rerun or admitted as part of this follow-up.

The initial module-wide formatter also reflowed unrelated PHRF files. Those
changes were compared against committed tokens/comments and restored before
diagnosis. Subsequent source formatting was scoped to GLS files; no unrelated
formatting is included in this work.

The [diagnostic figure](diagnostic/variance-decomposition.png) and
[confirmation figure](confirmation/variance-decomposition.png) use compact
labels, visible intervals and explicit paired counts. Rendering versions with
crowded full labels are retained as `variance-decomposition-v1.png`. The four
panels alone cannot establish admission: the failed F endpoint is retained in
the complete gate table above. Visual review records this limit separately
from statistical adjudication.

[summary.json](summary.json) binds the final outcome and test receipts. The
[original-pilot replay](original-pilot-replay/check.json) retains all 288 fit
outputs per platform, identical decisions and maximum scaled difference
5.784e-15 from the original pilot; eight original tests passed per platform.
The [scoped formatting receipt](checks/format.json) verifies the five GLS Scala
files with unchanged bytes on both targets. The full routine module suite was
not rerun after the separately merged PHRF checkpoint; validation here covers
the affected simulator, measurement and campaign paths on both platforms.
