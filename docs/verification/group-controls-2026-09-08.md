# Controlled group calibration: variance targets, corrections and noisy SEs

8 September 2026 · ScalaFIM native group qualification · **No new production default**

The controlled study separates two problems. HC3 and bootstrap multiplier changes
improve several Gaussian settings but leave clear skewed-error failures. Separately,
noisy first-level variance estimates cause substantial inflation in the existing
native PM/mKH route even in an ideal Gaussian hierarchy. Independent R agreement
supports implementation correctness for the checked cases; it does not remove
these calibration failures.

A fresh feasible-weighting follow-up is encouraging for HC3 within its Gaussian
scope, but precision weighting often loses power when SEs are noisy. The evidence
does not justify automatically preferring inverse-variance weights or replacing
the production default. General admission remains open.

## Evidence at a glance

| Stage | Settings | Independent studies per setting | Total | Role |
| --- | ---: | ---: | ---: | --- |
| Fixed pilot | 41 | 2,000 | 82,000 | Screen variance, leverage, error-shape and correction choices |
| Fresh confirmation | 8 | 10,000 | 80,000 | Deterministic worst-case/control selection from the pilot |
| Estimated first-level variance | 24 | 10,000 | 240,000 | Native PM/mKH, truth-informed controls and HC3 |
| Fresh feasible weighting | 24 | 10,000 | 240,000 | Inverse-vhat HC3 without knowledge of heterogeneity |
| **Total** | **97** | | **642,000** | |

Methods share outcomes within each setting. Power calculations, shifted-center
checks, bootstrap draws, explicit R fixtures and audit replays are not additional
independent studies. The earlier 376,000-study nuisance receipt is unchanged and
is not included in this total.

The [overview figure](../../tools/group-calibration-controls/calibration-overview.png)
shows the confirmed error-shape failure and the native PM/mKH response to noisy
SEs. The [complete feasible-weighting figure](../../tools/group-calibration-controls/feasible-weighting.png)
shows null rejection and power for every fresh follow-up setting. Bars are
pointwise 95% binomial intervals, not inference intervals for brain responses.

## Findings and decisions

### Corrections help Gaussian cases but do not resolve skewness

Fresh confirmation for n=20, groups 5 versus 15, a smooth covariate, reversed
variances and zero additive variance:

| Method | Gaussian false positives | Skewed false positives |
| --- | ---: | ---: |
| HC2 / Satterthwaite | 5.84% | 11.39% |
| HC3 / Satterthwaite | 3.94% | 8.90% |
| Restricted wild HC2, Rademacher | 6.24% | 13.20% |
| Restricted wild HC2, Mammen | 3.54% | 7.20% |
| Restricted wild HC3, Rademacher | 6.25% | 13.27% |
| Restricted wild HC3, Mammen | 3.53% | 7.30% |

The target is 5%. At n=80 in the reversed-variance skewed case, HC3 rejects 8.88%
and all four bootstrap variants reject 10.04–10.11%. Increasing the sample count
or changing multipliers does not establish valid inference for the tested skewed
population. This is a finding about these exact specifications, not a rejection
of every possible bootstrap procedure.

There are also conservative failures of usefulness: at n=8 with reversed
variances and additive variance 0.2, HC3 rejects 2.48%, while the two Rademacher
bootstrap variants reject 8.71% and 8.99%. The pilot's dominant-variance cases
are retained, including severe conservatism. Low rejection alone is not success.

### Native PM/mKH inflation follows uncertainty in precision

The next stage generates first-level effects and their estimated variances from
an actual Gaussian hierarchy. The candidate receives estimated variances only;
the separate oracle methods receive truth. At n=80, groups 20 versus 60, reversed
variances, zero heterogeneity:

| First-level variance information | Native PM/mKH | Plug-in WLS t | WLS HC3 / Satterthwaite | True-variance WLS t |
| --- | ---: | ---: | ---: | ---: |
| Known variances | 4.16% | 4.86% | 4.49% | 4.86% |
| Estimated, df=40 | 4.78% | 5.61% | 4.54% | 5.10% |
| Estimated, df=8 | **8.85%** | **9.84%** | 4.69% | 4.93% |

The 8.85% PM/mKH estimate has pointwise 95% interval **8.30–9.42%**, and a
one-sided family-adjusted 99.9% lower bound of **7.66%**. Its inverted-test
coverage at a true nonzero coefficient is 91.15%, against a 95% target. The
n=80 spread-variance, df=8 setting similarly gives 8.22% for PM/mKH and 10.16%
for plug-in WLS t. These excesses are not explained by ordinary Monte Carlo noise.

First-level df and group residual df are different: the latter are n−3 here.
The df settings use distinct fresh streams, so rows are controlled comparisons
of distributions, not paired realizations across df.

In settings with positive heterogeneity, the original plug-in WLS/HC3 controls
receive vhat plus **true** tau squared. They isolate first-level variance
uncertainty and are not proposed feasible estimators. PM/mKH always estimates its
own heterogeneity from the native public API. The zero-heterogeneity table above
does not need a nonzero true-tau input, but that does not erase the privileged
information in the rest of this stage.

### A feasible HC3 candidate works conservatively in this Gaussian grid

A separately declared follow-up uses only first-level vhat to form weights. It
adds neither the true tau squared nor a fitted tau squared. Outcomes and SE
estimates are fresh, on the same 24-cell Gaussian design grid.

| Method | n=20 null range | n=80 null range |
| --- | ---: | ---: |
| Inverse-vhat HC3 / Satterthwaite | 1.98–3.72% | 4.19–4.78% |
| Equal-subject HC3 / Satterthwaite | 3.12–4.44% | 4.29–4.95% |

Both target the same coefficient under the simulated correct conditional mean
model. That does not make their targets equivalent under arbitrary model
misspecification or participant-specific mean effects.

Power gives a reason to retain the equal-subject comparison. At n=80, reversed
variances, tau squared 0.2, df=8, the inverse-vhat method has 55.78% power and
4.25% false positives; equal-subject HC3 has 71.54% power and 4.58% false positives.
The alternative is a group coefficient of +0.7 in the simulation's outcome
units. Power uses each method's implemented 5% threshold; these are not
size-matched power comparisons. The complete figure retains every case, including
known-variance cases where weighting improves power.

This fresh stage was motivated by the preceding results. Its amendment was
recorded before its outcomes, and its streams are independent, but it remains
an exploratory extension rather than a general confirmation campaign. It omits
small n=8, spikes, dominant variances, non-Gaussian errors and dependent SEs. Those
omissions preclude a general admission claim.

## A simple analytical explanation for noisy-weight inflation

This is an explanatory calculation, not a PM/mKH correction. Consider an
intercept-only Gaussian model with common true variance v and independent
vhat = v Q / nu, where Q is chi-squared with nu degrees of freedom. With weights
w=1/vhat and nu>4:

- E[w] = nu / {v(nu−2)}.
- E[w²] = nu² / {v²(nu−2)(nu−4)}.
- The weighted mean's asymptotic variance is (v/n)(nu−2)/(nu−4).
- The usual residual-scaled WLS variance estimate tends to v/n.

Thus the asymptotic variance-underestimation factor is 1.5 at nu=8 and 1.0556
at nu=40. A nominal two-sided 5% normal-reference test then has limiting rejection
about **10.95%** and **5.64%**, respectively. More participants do not remove this
problem if the first-level variance information stays fixed. The issue occurs
even though vhat is unbiased and independent of the effect estimate.

The moments follow by integrating the [chi-square density](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Chisquare.html).
The retained unit test independently evaluates both moments by numerical
quadrature. These limiting calculations do not predict exact finite-sample
PM/mKH rejection and do not supply a universal factor to patch its SEs.

## Design, methods and randomness

The scalar target is the group coefficient in E[Y|X]=1.7+beta_group*g+0.8*z,
with z omitted for no-nuisance cases. The intercept and nuisance effect are
nonzero; the null group effect is zero. The alternative and true-center
inversion check use +0.7. Independent participants are the sampling units.

Group membership uses the first n/2 subjects for balanced designs, otherwise
max(2,n/4). The smooth covariate is cos((i+0.5)2pi/n); the leverage stress adds
5 to its first entry. Variances are constant 0.52, equally spaced 0.04–1 in
forward or reverse order, or 10 for the first participant and 0.04 for the rest.
The pilot/confirmation errors are Gaussian, variance-one t3, or centered and
standardized lognormal with log SD=1. In these stages, tau squared 0.2 augments
the marginal variance; it is not a separate non-Gaussian hierarchical component.

The estimated and feasible stages instead use independent Gaussian sampling
errors with variance v_i, Gaussian subject effects with variance tau squared,
and independent vhat_i=v_i*chi-square(df)/df. The known-variance case uses v_i
exactly. This ideal first-level model does not validate estimated serial
correlation, censoring, run aggregation or first-level effect/SE dependence in
real fMRI data.

HC2 and HC3 use OLS influence, residual adjustments (1−h)^−1/2 and (1−h)^−1,
and an identity working covariance for Satterthwaite df. Feasible weighted HC3
applies the same construction after whitening by sqrt(vhat). The diagnostic
true-target CR2 uses the actual diagonal marginal covariance and corresponding
residual adjustment; it has privileged information and is not an available user
method. Oracle WLS z knows absolute variances; oracle WLS t knows their shape.
Their reference guarantees are Gaussian, not automatically valid for skew/t3.

Every wild-bootstrap variant fits the null-restricted nuisance model, adjusts
its residuals using restricted leverage, and uses matching full-model HC2 or
HC3 studentization for the observed and bootstrap statistics. Rademacher and
Mammen multipliers use explicit independent-per-study uniform actions. Mammen
has mean zero, variance one and third moment one. P-values use
(1+exceedances)/(B+1), a two-sided absolute statistic and absolute tie tolerance
1e-12. Pilot draws are 499; confirmation draws are 999.

The initial protocol is `protocol.md` and `protocol.json`; selection is retained
in `confirmation-selection.json`. SeedSequence combines a role-specific root
with the cell index; NumPy PCG64 supplies the streams. Root pairs are
2026091601/02 (pilot outcomes/actions), 2026091701/02 (confirmation),
2026091801/02 (estimated outcomes/SEs) and 2026091901/02 (fresh feasible follow-up).
Methods share actions within a study. Confirmation selection is deterministic:
for four corrected candidates, retain maximum pilot null rates in normal/skew
families and minimum nominal power among normal cells, plus four mandatory
controls/previous failures, then deduplicate to eight cells.

`summary.json` preserves all cells, nonfinite counts, rejection, nominal power,
center-inversion coverage and intervals. Definite inflation uses prespecified
one-sided 99.9% Clopper–Pearson lower bounds, Bonferroni adjusted over all feasible
methods and cells in a stage. The same conservative reporting rule is extended
to all methods in each of the two variance-estimation stages. Plot intervals
are pointwise 95%; they are not those family bounds. Lack of a detected excess
is not proof of exact calibration or validity outside this grid.

## Numerical and reproducibility evidence

- Eight meaningful tests pass: explicit R method comparisons, all four bootstrap
  variants on explicit actions, multiplier moments, equivalent units/covariate
  recoding/subject ordering, Gaussian oracle identities, weighted invariances
  and inverse-chi-square quadrature.
- Eight explicit bootstrap/covariance fixtures agree with independent R
  lm/clubSandwich calculations. Bootstrap p-values agree exactly; maximum
  statistic error is below 9e-15 and df error below 1.8e-13.
- Seventy-two native PM/mKH fixtures agree with metafor PM/`test="adhoc"`:
  maximum p error below 7.8e-10 and tau squared error below 6.4e-10. They cover
  all 24 estimated-variance cells with explicit X, effects and variances.
- Seventy-two additional feasible-weighting fixtures agree with R's whitened
  lm/CR3/Satterthwaite: maximum p error below 6.8e-15 and df error below 8.9e-14.
- The full estimated-stage audit regenerates all 24 input hashes, reproduces
  all null/power/center-inversion counts and verifies finite p-values throughout.
  Native shifted refits preserve the +0.7 effect shift, SE and heterogeneity to
  the recorded tolerances. All stages retain zero observed nonfinite counts.

The R methods document their [working covariance targets](https://jepusto.github.io/clubSandwich/reference/vcovCR.lm.html)
and [small-sample reference approximations](https://jepusto.github.io/clubSandwich/reference/coef_test.html).
Finite-sample failures of wild-bootstrap heteroskedasticity tests are also a
known possibility in the [theoretical literature](https://arxiv.org/abs/2005.04089).
Those sources motivate checking the specific implemented method; our empirical
rates above come from the retained experiment, not from those publications.

The receipt records exact native source snapshots and hashes, compiler/runtime
inputs, R and Python versions, logs, input/output hashes and explicit fixtures.
`NativePmCalibration.scala` was compiled with all current native group sources
and executes the ordinary provider API. Full-study bulk binaries/NPZ files are
retained in the original scratch run, not duplicated in the compressed receipt;
the provided scripts regenerate them. The final native smoke re-executes the
72 explicit PM fixtures. No new production source or inference API is introduced
by this study; it adds research tools, reports and regressions. Consequently this
is not a fresh JVM/JS library-suite or consumer-adoption claim. Earlier portable
qualification remains in the earlier reports.

## Visual review and remaining work

Actual rendered figures were opened and inspected. The first overview failed
because panel titles collided with subtitles; that specimen is retained and
superseded. The final version separates them and uses marker/line differences
as well as color. The feasible figure was revised to avoid an overly broad
headline and to include power uncertainty. Reviewed figures retain all plotted
cells, 5% references, honest axes and the Gaussian/diagnostic scope. Exact artifact
hashes, render dimensions, critiques and dispositions are in `visual-review.json`.
These are standalone scientific exports, not native application screenshots or
an independent expert UI sign-off. This is not the product's visual milestone.

Next work should preserve first-level uncertainty provenance and qualify a
Gaussian bootstrap that regenerates both estimates and variance estimates,
using defensible first-level df and explicit nuisance estimation. Keep native
PM/mKH and equal-subject HC3 as baselines; retain the skewed and low-information
failures, test df misspecification, and require fresh null/coverage/power evidence.
That is an exploratory next direction, not a silently adopted inference method.

Native general-admission issue `bd-01M20TKX7VYFT3BHCM7QAMA6NG` and uncertainty issue
`bd-01M210WJ4BWVCXEMTARHDR2AC7` remain open. This bounded study is tracked by
`bd-01M219Z3QVF32QFAAJNVSC3SSW`. PLS Neuro integration remains separately tracked
by `bd-01M20V5F314V68XNY9SHDY4RR0` in its own store. General F, repeated-subject,
spatial and real serial-noise uncertainty qualification remain outstanding.

The joint effect-and-variance bootstrap investigation is lodged as
`bd-01M21BNZR9ZBRAYY9JD5WCQ8KX`; it is open research, not an admitted method.

## Receipt identity

Compressed evidence: `group-controls-2026-09-08.json.gz` (2848103 bytes).
SHA-256: `3dbe86486236827217be003897f5ef3599ff2dc096a7996b05823c3e2da1b4dc`.

Native HEAD at qualification: `72a35a46df661e6e8b0e8dd6bd7dbb59002ec9e4`;
uncommitted source snapshots, not HEAD alone, identify the tested group code.
