package scalafim.fmri.hrf

/** Review reproducer: passing assertions establish the defect, not a qualification pass. */
class ReviewDiscretizationProbeSuite extends munit.FunSuite:
  test("a valid tiny step enters kernel evaluation instead of refusing an unrepresentable grid"):
    val sentinel = new IllegalStateException("review-stop-before-large-loop")
    var evaluations = 0
    val kernel = Hrf.multi("review-grid", 1, span = Seconds(1.0)) { _ =>
      evaluations += 1
      throw sentinel
    }
    val step = PositiveSeconds(1e-20).toOption.get
    val thrown = intercept[IllegalStateException]:
      ResponseBasis.of(kernel).responseFunctional(
        ResponseFunctional.WindowIntegral(Seconds(0.0), Seconds(1.0)),
        FunctionalDiscretization.Trapezoid(step))
    assert(thrown eq sentinel)
    assertEquals(evaluations, 1)
    val intervals = math.ceil(1.0 / step.value).toInt
    assertEquals(intervals, Int.MaxValue)
    assert(1.0 / intervals.toDouble > step.value)
    assertEquals(intervals + 1, Int.MinValue)
