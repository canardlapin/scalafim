package scalafim.fmri

package object hrf:

  // High-level Scala-first API surface.
  // Importing `scalafim.fmri.hrf.*` brings these names into scope.

  export scalafim.fmri.hrf.regressor.{Regressor, RegressorSet, NeuralInput}
  export scalafim.fmri.hrf.design.{SamplingFrame, Design}
  export scalafim.fmri.hrf.linalg.{Mat, Vec}

  // The `Regressor` operations live as extension methods in the `regressor`
  // package, so exporting the type alone left `reg.evaluate(...)` unavailable
  // to anyone importing `scalafim.fmri.hrf.*`.
  export scalafim.fmri.hrf.regressor.{evaluate, shift, nbasis, neuralInput}
