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

Run it directly with:

```sh
sbt hrfJVM/test
sbt hrfJS/test
```

## Inspect sampled event support

`scalafim.fmri.hrf.regressor.SampledEventSupport.assess` evaluates each original
event separately using the native regressor kernels. The receipt retains the
supplied strictly increasing sample grid, event indices, amplitude, numerical
settings, and each basis column's nonzero sample count, first/last sample indices,
and maximum absolute response. Use the actual acquired or retained samples;
nominal run length is insufficient, especially for delayed FIR bins.

```scala
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.regressor.SampledEventSupport

val support = for
  reg <- Regressor.validated(Seq(-1.0, 3.0, 4.5),
    hrf = Hrfs.fir(nBasis = 2, span = 4.s)).left.map(_.message)
  receipt <- SampledEventSupport.assess(reg,
    Vector(0.s, 1.s, 2.s, 3.s, 4.s), precision = 0.25.s).left.map(_.toString)
yield receipt
// Basis sample counts: (1,2), (2,0), (0,0).
```

Counts use `abs(response) > absoluteTolerance`, in response-amplitude units.
The default zero tolerance means numeric nonzero; FFT roundoff may require an
explicit positive tolerance. Zero-amplitude events stay in the receipt. Two
opposing supported events remain supported even if their aggregate cancels.
Per-event HRF assignments retain their original identities and native evaluation
path. Non-finite sampled values fail with event/sample/basis indices.

Envelope flags compare onset and onset + duration + configured span with the
first and last supplied samples. These flags do not establish physiological
completeness: span is a computational horizon and the grid can contain gaps.
This API does not establish design estimability, choose exclusions, modify source
events, or admit inference. Callers must carry source-row identities into their
own reports and apply any event-selection policy before fitting and before
selection-dependent centering or modulation transforms.
