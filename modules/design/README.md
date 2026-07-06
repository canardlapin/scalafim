# scalafim-fmri-design

Cross-compiled JVM/Scala.js fMRI design module for `scalafim`.

Package root:

```scala
import scalafim.fmri.design.*
```

This module contains event models, formula parsing, condition bases, baseline
models, contrast definitions, design-matrix metadata, and design export helpers.
It depends on `scalafim-fmri-hrf`.

Run it directly with:

```sh
sbt designJVM/test
sbt designJS/test
```
