package scalafim.fmri.fit

import gale.linalg.Matrix
import scalafim.fmri.ar.RunCorrection
import scalafim.fmri.fit.fixtures.FmriArCorrectedGlsFixture as R
import scalafim.fmri.model.{ArBiasCorrection, ArOptions, ArStructure}

class ArConsumerReviewProbeSuite extends munit.FunSuite:
  test("corrected voxelwise GLS preserves fully censored run outcomes") {
    val design = DesignMatrix.unsafe(Matrix.tabulate(R.design.length, R.design.head.length)((row, col) => R.design(row)(col)))
    val response = ResponseBlock.unsafe(Matrix.tabulate(R.response.length, R.response.head.length)((row, col) => R.response(row)(col)))
    val splits = Vector(
      RunPartition(0, (0 until R.runLengths.head).toVector, (0 until R.runLengths.head).toVector),
      RunPartition(1, (R.runLengths.head until R.runLengths.sum).toVector, (R.runLengths.head until R.runLengths.sum).toVector)
    )
    val options = ArOptions(ArStructure.Ar(1), voxelwise = true,
      censoredTimepoints = (R.runLengths.head until R.runLengths.sum).toVector,
      biasCorrection = ArBiasCorrection.Ols)
    val actual = Gls.fit(design, response, splits, options)
    assert(actual.isRight, actual.toString)
    assert(actual.toOption.get.diagnostics.runs.last.voxelwiseCorrections.forall(_.isInstanceOf[RunCorrection.NotAttempted]))
  }

  test("corrected run-pooled AR(2) GLS preserves fully censored run outcomes") {
    val design = DesignMatrix.unsafe(Matrix.tabulate(R.design.length, R.design.head.length)((row, col) => R.design(row)(col)))
    val response = ResponseBlock.unsafe(Matrix.tabulate(R.response.length, R.response.head.length)((row, col) => R.response(row)(col)))
    val splits = Vector(
      RunPartition(0, (0 until R.runLengths.head).toVector, (0 until R.runLengths.head).toVector),
      RunPartition(1, (R.runLengths.head until R.runLengths.sum).toVector, (R.runLengths.head until R.runLengths.sum).toVector)
    )
    val options = ArOptions(ArStructure.Ar(2),
      censoredTimepoints = (R.runLengths.head until R.runLengths.sum).toVector,
      biasCorrection = ArBiasCorrection.Ols)
    val actual = Gls.fit(design, response, splits, options)
    assert(actual.isRight, actual.toString)
    assert(actual.toOption.get.diagnostics.runs.last.correction.exists(_.isInstanceOf[RunCorrection.NotAttempted]))
  }
