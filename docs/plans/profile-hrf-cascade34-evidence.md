# Cascade34 kernel evidence (PHRF-04)

Date: 2026-09-12. Scope: the continuous scalar kernel and parametric family
specified in [the revised work plan](profile-hrf-work-items-v2.md#phrf-04-implement-the-continuous-cascade34-kernel-and-its-summaries).

## Scientific contract

For nonnegative lag, `h(t) = g3(t; kappaP) - rho g4(t; kappaU)`, where
`gn(t; k) = k^n t^(n-1) exp(-k t) / (n-1)!`. Both components have unit
integral; `kappaP > kappaU > 0` in inverse seconds and `rho >= 0` is the
component area ratio. Negative lags return zero. Positive-component area is
the intrinsic normalization, and the full signed area `1 - rho` is never a
divisor. In particular, `rho = 1` remains a regular kernel and chart point.

The chart is `(logKappaP, logitRateRatio, rho)`, with
`kappaU = exp(logKappaP) * logistic(logitRateRatio)`. The implementation
provides the value, three first derivatives and six mixed/pure second
derivatives in the existing component-major jet layout. Evaluation and jet
loops make one pass over the caller's lags and write caller-owned arrays;
there are no per-lag allocations or dense design construction. This is an
implementation property, not a measured throughput or allocation benchmark.

`Cascade34.summaries` / `Cascade34Family.attainedSummaries` solve for the
continuous peak, half-height crossings and trough. They report attained
latency, peak height, positive-lobe FWHM, trough latency/height and the attained
trough-to-peak ratio. The evaluation horizon does not clip these summaries or
the scalar kernel. Unrepresentable summary brackets or peak heights fail
explicitly rather than entering an unbounded root search.

At `rho = 0`, the kernel is independent of `logitRateRatio`; its first
derivative and pure second derivative in that coordinate vanish. The family
explicitly reports that coordinate as structurally unidentified. Mixed
derivatives with respect to `rho` and the tail coordinate generally remain
nonzero. This report is separate from
experiment-specific information and conditional standard errors.

## Independent numerical evidence

`tools/validation/cascade34_reference.py` generates the checked-in
`Cascade34Reference.scala` with **mpmath 1.3.0 at 80 decimal digits**. It has no
Scala dependencies and does not call the production kernel, primitive or
derivative routines. Generation was repeated and compared byte-for-byte.

| Contract | Independent evidence | Test tolerance |
| --- | --- | --- |
| Value and all nine derivatives | 32 point/lag cases, numerical differentiation of the literal Erlang polynomial at 80 digits; includes negative/zero lag, slow/fast rates, `rho = 0` and `rho = 1` | `3e-12 * abs(reference) + 1e-14 * abs(kernel) + 1e-300` |
| Exact box integrals | 12 high-precision quadrature fixtures, including a `1e-5 s` onset interval and a `240..240.1 s` far-tail interval | `3e-12` relative, `1e-300` floor |
| Signed area | Independent midpoint quadrature to 200 s at step 0.002 s, including zero signed area and a negative signed area | `2e-11` absolute |
| Continuous peak and FWHM | Six high-precision roots of the time derivative and half-height equations in seconds | `2e-12` relative |
| Continuous trough | Same oracle, including a trough at 111.11 s and nearly equal rates with a trough of order `1e-87` | `2e-12` relative for time; `2e-10` for height and ratio |
| Chart chain rule | Reusable family laws: central differences of values and analytic first derivatives, three interior points across the chart | step `1e-4`, error `1e-6` |

Additional contracts check causality, unbounded support, all jet buffer slots,
finite zero tails at extreme finite lags, physical-parameter validation,
chart/horizon validation, constant normalization jets, rate/time dilation,
named and typed dispatch, descriptor provenance, and clearing the primitive
after lagging the kernel. Both `BoxHeight` and `BoxMass` are checked at coarse
and fine integration precision. Far-tail integrals use differences of Erlang
survival probabilities to avoid subtracting CDFs that have rounded to one.

Final composition review reproduced a failure when Cascade34 was bound into
a nested multi-column basis: the exact integral over `240..240.1 s` became
zero instead of the oracle's `-1.5614331456542072e-19`. Stacked integration
now composes each component's definite integral, preserving the stable tail
calculation. The reproduction is retained as a regression test.

## Validation

| Gate | Result |
| --- | --- |
| `hrfJVM/test` | 247 passed, including the stacked-integral regression |
| `hrfLawsJVM/test` | 82 passed |
| `hrfJS/test` | 247 passed, including the stacked-integral regression |
| `hrfLawsJS/test` | 82 passed |
| `scalafimCompileAll` | Passed, warning-clean |
| `designJVM/test` | 238 passed |
| `firstLevelLawsJVM/testOnly scalafim.fmri.laws.profile.ConditionProfileFitSuite scalafim.fmri.laws.profile.CompactConditionRuntimeSuite` | 5 passed |
| `designJS/test` | 238 passed |
| `firstLevelLawsJS/testOnly scalafim.fmri.laws.profile.ConditionProfileFitSuite scalafim.fmri.laws.profile.CompactConditionRuntimeSuite` | 5 passed |

The full HRF suites were rerun after the stacked-integral correction. Tests use
the repository's pinned providers, without local overrides.

## Remaining plan boundaries

The default chart is a modeling domain, not a calibrated scientific prior.
Its finite evaluation horizon makes no claim that the tail is zero or that
every shape meets a truncation tolerance. PHRF-14 still owns family/domain
calibration and the previously unmet low-SNR LWU admission target.

The mathematical seven-state realization is declared by the family; the
finite-state execution backend remains optional PHRF-30. PHRF-06 remains the
required upstream Gale factor/solve/logdet work before TrialBanded can proceed.
This kernel evidence does not close trial-backend, calibration or release gates.
