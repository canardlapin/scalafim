package scalafim.fmri.hrf

import scalafim.fmri.hrf.design.{BlockSelection, SamplingFrame, TimeReference}
import scalafim.fmri.hrf.regressor.{Regressor, evaluate}

class HrfTypedRefactorSuite extends munit.FunSuite:

  test("built-in HRFs expose typed descriptors and basis arity") {
    val spmg3 = Hrfs.SPMG3

    assertEquals(spmg3.basis.value, 3, clue = "")
    assertEquals(spmg3.descriptor.nbasis, spmg3.nbasis, clue = "")
    assertEquals(spmg3.descriptor.components.length, 3, clue = "")

    spmg3.descriptor.family match
      case HrfFamily.Known(HrfKind.Spmg3) => ()
      case other => fail(s"expected SPMG3 descriptor, got $other")

    spmg3.descriptor.derivative match
      case DerivativePolicy.Spmg(params, columns) =>
        assertEquals(columns.value, 3, clue = "")
        assertEqualsDouble(params.p1, 5.0, 0.0)
      case other => fail(s"expected analytic SPMG derivative policy, got $other")

    assertEquals(Hrfs.fourier(nBasis = 4).descriptor.penalty, PenaltyPolicy.FourierFrequency, clue = "")
    assertEquals(Hrfs.bspline(nBasis = 2).descriptor.nbasis, 4, clue = "B-spline keeps R minimum-basis behavior")
  }

  test("weighted and empirical HRFs carry validated sampled profiles") {
    val weighted = Hrfs.weighted(Vector(1.0, 2.0, 3.0), width = Some(2.0.s), method = Hrfs.WeightedMethod.Linear)

    weighted.descriptor.params match
      case HrfParams.Weighted(profile, method, normalize) =>
        assertEquals(method, Hrfs.WeightedMethod.Linear, clue = "")
        assert(!normalize, clue = "")
        assertEquals(profile.times.toVector.map(_.value), Vector(0.0, 1.0, 2.0), clue = "")
        assertEquals(profile.weights, Vector(1.0, 2.0, 3.0), clue = "")
      case other => fail(s"expected weighted profile params, got $other")

    assert(WeightedProfile.fromExplicit(Vector(1.0, 2.0), Vector(0.0.s, 0.0.s)).isLeft)

    val empirical = Hrfs.empirical(Seq(2.0.s, 0.0.s), Seq(20.0, 10.0))
    empirical.descriptor.params match
      case HrfParams.Empirical(curve) =>
        assertEquals(curve.times.toVector.map(_.value), Vector(0.0, 2.0), clue = "")
        assertEquals(curve.values, Vector(10.0, 20.0), clue = "")
      case other => fail(s"expected empirical curve params, got $other")
  }

  test("typed sampling-frame block selections match legacy block-id API") {
    val frame = SamplingFrame(blockLens = Seq(2, 3), tr = Seq(1.0, 2.0), startTime = Seq(0.5, 1.0))
    val selection = BlockSelection.fromInts(Seq(1), frame.nBlocks).fold(err => fail(err.message), identity)

    val typed = frame.sampleTimes(selection, TimeReference.Global).map(_.value)
    val legacy = frame.samples(blocks = Seq(1), global = true).map(_.value)
    assertEquals(typed, legacy, clue = "")

    val onsets = frame.globalOnsets(Seq(Seconds(0.25)), selection).map(_.value)
    assertEquals(onsets, Vector(2.25), clue = "")
    assert(BlockSelection.fromInts(Seq(2), frame.nBlocks).isLeft)
    assert(SamplingFrame.validated(blockLens = Seq(2), tr = Seq(1.0), precision = 2.0).isLeft)
  }

  test("per-event regressor evaluation dispatch is exhaustive across methods") {
    val reg = Regressor.perEvent(
      onsets = Seq(0.0, 2.0),
      hrfs = Seq(Hrfs.gamma(), Hrfs.gaussian()),
      duration = Seq(0.0),
      amplitude = Seq(1.0),
      span = Some(10.0)
    )
    val grid = Seq(0.0, 1.0, 2.0, 3.0, 4.0)
    val loop = reg.evaluate(grid, precision = 0.5, method = Regressor.EvalMethod.Loop)
    val conv = reg.evaluate(grid, precision = 0.5, method = Regressor.EvalMethod.Conv)
    val fft = reg.evaluate(grid, precision = 0.5, method = Regressor.EvalMethod.FFT)

    assertEquals(conv.data.toVector, loop.data.toVector, clue = "")
    assertEquals(fft.data.toVector, loop.data.toVector, clue = "")
  }
