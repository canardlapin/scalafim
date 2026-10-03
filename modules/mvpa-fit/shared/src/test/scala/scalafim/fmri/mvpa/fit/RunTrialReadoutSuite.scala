package scalafim.fmri.mvpa.fit

import gale.linalg.DMat
import munit.FunSuite
import scalafim.dataset.RunId
import scalafim.fmri.fit.{LeastSquaresSeparate, LssTrialDesign, ResponseBlock, TrialEstimability}
import scalafim.fmri.mvpa.{FeatureIndex, PatternCopyBudget}

class RunTrialReadoutSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A =
    value.fold(error => fail(error.toString), identity)

  private val design = right(
    LeastSquaresSeparate
      .unsafePrepare(
        LssTrialDesign.unsafe(
          DMat.dense(4, 2, Vector(1, 0, 1, 0, 0, 1, 0, 1)),
          Vector("a", "b")
        )
      )
      .trialReadout
  )

  private def run(
      response: DMat = DMat.dense(4, 2, Vector(2, 20, 2.2, 18, -2, -20, -2.2, -18))
  ): RunTrialReadout =
    right(RunTrialReadout.make(RunId("run-1"), ResponseBlock.unsafe(response), design))

  test("readout composes a design-only temporal operator and materializes only under caller budget") {
    val value = run()
    val dense = right(value.explicitPatterns(PatternCopyBudget(4L)))

    assertEquals(value.fitScope, ReadoutFitScope.DesignOnly)
    assertEquals(value.readout.axis.names, Vector("a", "b"))
    assertEquals(value.readout.axis.estimability, Vector(TrialEstimability.Estimable, TrialEstimability.Estimable))
    assertEquals((dense.value.rows, dense.value.cols), (2, 2))
    assertEqualsDouble(dense.value(0, 0), 2.1, 1e-12, "first coefficient")
    assertEqualsDouble(dense.value(1, 1), -19.0, 1e-12, "second coefficient")
    assert(value.explicitPatterns(PatternCopyBudget(3L)).isLeft)
  }

  test("timepoint and feature-axis mismatches return their native error cases") {
    val short = ResponseBlock.unsafe(DMat.dense(3, 2, Vector(1, 2, 3, 4, 5, 6)))
    assertEquals(
      RunTrialReadout.make(RunId("short"), short, design).left.toOption,
      Some(OneShotMvpaError.TimepointMismatch(RunId("short"), readoutTimepoints = 4, responseTimepoints = 3))
    )
    assertEquals(
      RunTrialReadout
        .make(RunId("bad"), ResponseBlock.unsafe(DMat.zeros(4, 2)), design, Vector(FeatureIndex(0)))
        .left
        .toOption,
      Some(OneShotMvpaError.FeatureAxisLengthMismatch(RunId("bad"), axisLength = 1, responseFeatures = 2))
    )
  }
