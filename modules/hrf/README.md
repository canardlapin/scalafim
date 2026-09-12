# scalafim-fmri-hrf

Cross-compiled JVM/Scala.js HRF module for `scalafim`.

Package root:

```scala
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.HrfCombinators.*
```

This module contains hemodynamic response functions, basis generators,
decorators, regressors, sampling frames, and small shared matrix/vector helpers.
It has no dependency on the design or image modules.

`Hrfs.cascade34(Cascade34Params(0.5, 0.2, 0.3))` constructs the continuous
kernel `g3(t; kappaP) - rho * g4(t; kappaU)`. Rates are in inverse seconds,
with `kappaP > kappaU > 0`; `rho >= 0` is the component **area** ratio. Each
Erlang component integrates to one. The full signed area is `1 - rho`, which
is never used as a divisor. `Pulse.BoxHeight` and `Pulse.BoxMass` use its
analytic integral, including a survival-probability calculation in the tail.

For shape fitting, `family.Cascade34Family` supplies analytic first and mixed
second derivatives in `(logKappaP, logitRateRatio, rho)` and declares
`NormalizationRule.PositiveComponentArea`. `attainedSummaries` returns the
continuous peak time and height, positive-lobe FWHM, and trough time, height
and ratio. These summaries use the unbounded kernel, independently of the
evaluation horizon. At `rho = 0`, `unidentifiedCoordinates` reports
`logitRateRatio`; the positive component remains identified by its rate.
This is structural information, not an experiment-specific uncertainty test.
The default chart is a modeling domain and has not undergone PHRF-14
scientific calibration. See the [kernel evidence](../../docs/plans/profile-hrf-cascade34-evidence.md).

Run it directly with:

```sh
sbt hrfJVM/test
sbt hrfJS/test
```
