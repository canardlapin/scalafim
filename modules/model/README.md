# scalafim-fmri-model

Cross-compiled JVM/Scala.js model module for `scalafim`.

Package root:

```scala
import scalafim.fmri.model.*
```

This module is the first rewrite landing zone for `fmrireg`. It combines a
typed dataset with event and baseline design models, then describes fitting as a
typed `FitPlan` plus algebraic configuration. Numerical execution lives in
separate fitting modules such as `scalafim-fmri-fit`; this module is intentionally
not an R-list or S3-method port.

Run it directly with:

```sh
sbt modelJVM/test
sbt modelJS/test
```
