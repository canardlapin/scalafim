# Voxelwise FGLS inference: diagnosis, typed candidate, held-out confirmation — 2026-10-10

Mote `bd-01M4EMEBCBMBHA7BF4BQY6AFPB`. Original margins and the 2026-10-08 adverse
receipts are unchanged and remain binding. The
[protocol](../../../tools/scenarios/fgls-voxelwise/protocol.json) (sha256
`10e57f6a…b28c`, root 1310161010, seed overlap with earlier roots 0) was committed
in `7f994394` before any Scala development or confirmation stream existed.

**Held-out confirmation passed on JVM and Scala.js.** The frozen candidate,
`VoxelwiseArSpec(2, Estimated(AcrossRuns, 25), ContinuousMissing, KenwardRoger)`
(one AR(2) filter per voxel shared across that voxel's runs, censored frames as
missing observations of continuous noise, Kenward-Roger inference), met every
original criterion in all four admission cells on fresh streams (domain 12,
2,048 datasets per cell, 36,864 fits per platform, 0 refusals). Both platforms
reached identical decisions; maximum scaled numeric difference 4.5e-12 (variance
fields), 1.3e-14 for estimates. Source unchanged during execution (base `3d79675f`).

| Admission cell (candidate) | Null t | Null F | Covered | AR RMSE | Var. ratio 90% | Std. effect bias | Whiteness |
| --- | ---: | ---: | ---: | ---: | --- | ---: | ---: |
| AR(2) censored, 96/144 (original failing DGP) | 119 | 124 | 1941 | 0.0771 | [0.912, 1.009] | 0.004 | 0.089 |
| AR(2) censored, 192/288 | 104 | 90 | 1954 | 0.0497 | [0.975, 1.080] | 0.062 | 0.056 |
| AR(2) complete, 96/144 | 103 | 106 | 1909 | 0.0726 | [0.932, 1.037] | 0.023 | 0.085 |
| voxel-heterogeneous AR(2), censored, 96/144 | 127 | 132 | 1941 | 0.0766 | [0.885, 0.986] | 0.002 | 0.089 |

Counts of 2,048; acceptance remains 53..143 rejections and 1905..1995 covered
(exact 90% intervals inside [.02,.08] and [.92,.98]). Margins are close in places:
complete-short coverage 1909 (limit 1905), and long-cell effect bias 0.062 + 0.036
half-width = 0.099 against 0.10 (the exact known-filter fit has the same 0.062 on
those datasets, so it is a property of the stream, not the estimator).

Baselines on the same confirmation datasets: the **existing voxelwise path failed
again** in the short censored cell (t 153, F 181 of 2,048; RMSE 0.107) and in the
complete cell (coverage 1890, RMSE 0.102); the **same pooled estimator with
conditional (plug-in) inference failed** null F in the voxel-heterogeneous cell
(148) and coverage in the complete cell (1900). The KR treatment is therefore
load-bearing, not cosmetic.

**Scope boundary (declared, reported, not required):** when the AR truth differs
between runs, across-run pooling is misspecified and the candidate fails AR RMSE
(0.135) and whiteness (0.1075); rejection counts stayed inside margins (130/135).
The admission is limited to AR noise homogeneous across runs within a voxel
(heterogeneous across voxels is covered), AR(2), this design family and the
censor rule tested. It is not a claim for per-run voxelwise estimation, which
cannot meet the RMSE margin at these run lengths (finding 3 below), nor for the
production `FitPlan` path, which is unchanged.


## What was built

- `scalafim.fmri.ar.StationaryArFactor`: stationary AR(p) autocovariance and the
  exact initial factor `L^-1` (`L L' = Gamma_p`).
- `scalafim.fmri.fit.VoxelwiseArFgls` with three explicit typed policies:
  - `CensorContinuity.RestartAfterCensor` (the current approximation) or
    `ContinuousMissing`: noise is continuous within a run, each censored row is
    absorbed by an indicator column (exactly GLS on retained rows under their
    marginal covariance; residual df fall by one per censored row), whitening is
    continuous with the exact stationary initial rows.
  - `VoxelArScope.PerRun` or `AcrossRuns` (one AR filter per voxel, shared by runs).
  - `CovarianceUncertainty.Conditional` or `KenwardRoger` (KR 1997 for
    `V = sigma^2 (W(phi)'W(phi))^-1`, keeping the second-derivative term because
    the model is non-linear in phi; Wald tests carry KR scale and denominator df).
- The production `FitPlan` GLS path, its defaults and its estimator are untouched.

Verification of the mathematics (both platforms, `VoxelwiseArFglsSuite`, 10 tests):
exact whitening gives `W Sigma W' = I` through censor gaps (1e-10), indicator GLS
equals marginal GLS (1e-10), KR with known phi reproduces the exact `F(q, n-rank)`
test (df, scale 1, covariance), and KR matches an independent generic R
implementation on four deterministic AR(2) cases (2e-6 relative). That generic R
KR equals `pbkrtest::vcovAdj` to 2.1e-15 on a linear mixed model
([exploration/generic-kr-vs-pbkrtest.txt](exploration/generic-kr-vs-pbkrtest.txt)).

## Root causes (development stream, JVM, 1024 datasets/cell, paired)

Ratios are against the exact known-filter fit (`known-continuous`) on the same
datasets, with 90% paired bootstrap intervals. "Reported" is mean reported null
variance; "estimator" is the empirical variance of the null estimate.

| Short censored AR(2), high nuisance | Reported | Estimator | t / F rejections (of 1024) | AR RMSE |
| --- | ---: | ---: | ---: | ---: |
| known filter, current restart | 0.963 [0.962,0.964] | 0.990 [0.970,1.012] | 65 / 47 | 0 |
| existing voxelwise (restart, per run) | 0.915 [0.904,0.925] | 1.033 [1.005,1.064] | 90 / 80 | 0.107 |
| per run, continuous, conditional | 0.950 [0.938,0.961] | 1.043 [1.023,1.063] | 74 / 79 | 0.108 |
| per run, continuous, KR (df ~37) | 0.979 [0.966,0.991] | 1.043 | 62 / 68 | 0.108 |
| across runs, continuous, conditional | 0.970 [0.958,0.981] | 1.009 [1.002,1.016] | 70 / 73 | 0.076 |
| across runs, continuous, KR (df ~37) | 0.950 [0.938,0.961] | 1.009 | 65 / 67 | 0.076 |
| across runs, restart, conditional | 0.934 [0.923,0.945] | 0.999 | 75 / 68 | 0.075 |

1. **Censor restart (known-filter) bias: -3.7% reported variance**, deterministic
   given the data (interval width 0.002). For AR(2) the restart leaves each segment's
   first row unscaled (variance gamma_0 = 1.213) and the next rows correlated with
   the preceding segment (correlation up to 0.50 in the exact covariance check).
   The complete-run control shows the identical whitener is exact without censoring
   (1.0002). Doubled runs shrink it to -1.4% (0.986).
2. **Estimated-filter variance underreporting and inflation.** Per-run voxelwise
   estimation adds ~-5% reported and +4.3% estimator variance (calibration ~0.91),
   in complete runs too (0.955 / 1.032), so it is not caused by censoring.
   Both AR coefficients are biased toward zero after the OLS residual correction
   (mean -0.010, -0.013), which lowers the low-frequency spectral density and hence
   the reported variance of slow task regressors.
3. **Per-run AR precision floor.** The Yule-Walker asymptotic variance for
   phi=(0.45,-0.10) is (1-phi_2^2)/n = 0.99/n per coefficient. With 93 and 139
   retained rows per run, the run-weighted RMSE floor is 0.092 (0.096 after the
   ~9 regressors per run), against the 0.10 margin; the realized 0.102–0.108 fails
   it in every short per-run cell. No inference correction can change this; only
   more data per filter (longer runs: 0.070, or pooling across runs: 0.072–0.076).
4. **KR is the measurable part of the inference fix**, mainly through its df
   (~37 for a slow task contrast: the variance of the contrast variance is dominated
   by phi uncertainty, d log Var/d phi_1 ~ 2/(1-phi_1-phi_2) ~ 3.1). Its covariance
   adjustment raises per-run reported variance (0.950→0.979) but *lowers* it when
   pooled (0.970→0.950): the second-order curvature term outweighs the
   Kackar-Harville inflation once phi is precise, and KR cannot correct the
   residual small-sample bias of the AR estimator itself.

The run-heterogeneous scope cell shows across-run pooling is misspecified when runs
differ (RMSE 0.135, whiteness fails): any pooled claim is restricted to noise that
is homogeneous across runs within a voxel. Heterogeneity across voxels is handled
(voxel-heterogeneous cell: candidate passed all criteria on development).

## Candidate selection

Declared rule over the four admission cells: run-continuous-conditional 31,
run-continuous-kr 38, pooled-continuous-conditional 39, **pooled-continuous-kr 42**
of 44. Frozen at `1d03dfce` (mote decision with source hash) before the
confirmation stream was generated.

## Receipts

`pilot-jvm/`, `development/` and `confirmation/` keep run receipts (source hashes,
commands, exit codes), complete compressed records, logs and the independent R
analyses (`analysis-*/summary.json`, `table.csv`). `exploration/` holds the
pre-declaration R prototype (R RNG, 600 replicates, not a declared stream).

Note: in the confirmation analysis tables the existing-voxelwise "whiteness"
failure is an artifact: its excess-over-known comparator (existing-known-restart)
was not executed in confirmation (declared engine subset), so the excess is NaN.
It is excluded from the baseline failures listed above.

## Routine gates

`scalafimCompileAll` exit 0 with no compiler warnings. Tests, all passing:
`arJVM/test` 158, `fitJVM/test` 748, `firstLevelLawsJVM/test` 121;
`arJS/test` 156, `fitJS/test` 690, `firstLevelLawsJS/test` 121 (2 ignored).
`firstLevelLawsJVM/Test/scalafmtCheck` reports 26 pre-existing unformatted files
from the merged PHRF checkpoint; the new study files are formatted and those
26 files were not touched.
