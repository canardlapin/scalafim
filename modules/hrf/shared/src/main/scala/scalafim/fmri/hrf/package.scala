package scalafim.fmri

package object hrf:

  // High-level Scala-first API surface.
  // Importing `scalafim.fmri.hrf.*` brings these names into scope.

  export scalafim.fmri.hrf.regressor.{Regressor, RegressorSet, NeuralInput}
  export scalafim.fmri.hrf.design.{SamplingFrame, Design}
  export scalafim.fmri.hrf.linalg.{Mat, Vec}
