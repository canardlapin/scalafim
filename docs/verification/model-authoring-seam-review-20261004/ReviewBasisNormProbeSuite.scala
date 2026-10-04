package scalafim.fmri.design

import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

/** Review reproducers use public kernel construction and impulse convolution.
  * Passing assertions establish defects, not a qualification pass. */
class ReviewBasisNormProbeSuite extends munit.FunSuite:
  private def convolve(kernel: Hrf) =
    val term = EventTerm(Vector(Event.factor(Vector("task"), "condition")), Vector(Seconds(0.0)), termTag = Some("task"))
    term.convolve(kernel, SamplingFrame(blockLens = Seq(4), tr = Seq(1.0)))

  test("finite nonzero collinear columns underflow into an all-zero rank receipt"):
    val kernel = Hrf.multi("review-underflow", 2, span = Seconds(3.0))(_ => Array(1e-200, 3e-200))
    val raw = convolve(kernel)
    assert(raw.data.data.exists(_ != 0.0))
    val (changed, receipts) = ConvolvedBasisOrthogonalization(raw).toOption.get
    assertEquals(receipts.head.groups.head.referenceRank, 0)
    assertEquals(receipts.head.groups.head.transform, Vector(1.0, 0.0, 0.0, 1.0))
    assertEquals(changed.data.data.toVector, raw.data.data.toVector)

  test("reference cutoff discards an earlier tiny column and returns nonorthogonal full-rank output"):
    val kernel = Hrf.multi("review-scale", 3, span = Seconds(3.0)) { lag =>
      if lag.value < 1.0 then Array(1e-18, 0.0, 1.0)
      else if lag.value < 2.0 then Array(0.0, 1.0, 1.0)
      else Array(0.0, 0.0, 1.0)
    }
    val raw = convolve(kernel)
    val (changed, receipts) = ConvolvedBasisOrthogonalization(raw).fold(error => fail(error.message), identity)
    assertEquals(receipts.head.groups.head.referenceRank, 3)
    val left = (0 until changed.data.rows).map(r => changed.data(r, 0) / 1e-18)
    val right = (0 until changed.data.rows).map(r => changed.data(r, 2))
    val cosine = left.zip(right).map(_ * _).sum / math.sqrt(left.map(x => x * x).sum * right.map(x => x * x).sum)
    assertEqualsDouble(cosine, 1.0 / math.sqrt(2.0), 1e-12)
