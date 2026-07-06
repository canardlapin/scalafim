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
