package scalafim.fmri.fit.profile

import scalafim.fmri.design.hrf.{ExpandedTrialDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, ShapePoint}

/** Held-out prediction helper (JVM and JS): linear in the amplitudes, equal to direct convolution, typed refusals. */
class TrialHeldOutPredictionSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  private lazy val basis: HrfKernelBasis =
    HrfKernelBasis
      .compile(KernelBasisSpec(GaussianFamily.Default, PositiveSeconds.unsafe(Seconds(0.2)), Vector(26, 21), tolerance = 1e-4, maxRank = 40))
      .fold(e => fail(e.message), identity)

  private val rows = 90
  // multiples of 0.2 s so the 0.2 s lowering grid carries no onset quantisation
  private val onsets = Vector(3.0, 12.4, 21.6, 30.2, 41.0, 52.8)
  private val conds = Vector(0, 1, 0, 1, 0, 1)

  /** One run whose scan k sits at `k * TR` (start 0), not the library default `(k + 1/2) TR`. */
  private lazy val frame = SamplingFrame(blockLens = Vector(rows), tr = Vector(1.0), startTime = Vector(0.0))

  private lazy val design: ExpandedTrialDesign =
    val membership = TrialMembership.make(conds, 2).fold(e => fail(e.message), identity)
    ExpandedTrialDesign
      .lower(onsets.map(Seconds(_)), Vector.fill(onsets.length)(0), Vector.fill(onsets.length)(Seconds(0.0)), membership, frame, basis, Seconds(0.2))
      .fold(e => fail(e.message), identity)

  private val theta = Vector(5.2, math.log(1.6))
  private val amps = Array(1.5, -0.7, 2.2, 0.4, -1.1, 0.9)

  private def predict(a: Array[Double], coords: Vector[Double] = theta): Array[Double] =
    val out = new Array[Double](rows)
    TrialHeldOutPrediction.signalInto(design, coords, a, out).fold(e => fail(e.message), identity)
    out

  test("the frame places scan k at k * TR (the library default would put it at (k + 1/2) TR)"):
    assertEquals(frame.samples().map(_.value), Vector.tabulate(rows)(_.toDouble))
    val shifted = SamplingFrame(blockLens = Vector(rows), tr = Vector(1.0))
    assertEquals(shifted.samples().map(_.value).head, 0.5)

  test("prediction equals direct evaluation of the family kernel at the shape (unit-peak Gaussian)"):
    val sd = math.exp(theta(1))
    val got = predict(amps)
    var worst = 0.0
    var peak = 0.0
    var t = 0
    while t < rows do
      var direct = 0.0
      var i = 0
      while i < onsets.length do
        val lag = t.toDouble - onsets(i)
        if lag >= 0.0 then direct += amps(i) * math.exp(-0.5 * (lag - theta(0)) * (lag - theta(0)) / (sd * sd))
        i += 1
      worst = math.max(worst, math.abs(got(t) - direct))
      peak = math.max(peak, math.abs(direct))
      t += 1
    println(f"HELDOUT-PRED max |prediction - direct| = $worst%.3e (peak $peak%.3f)")
    // the 0.2 s lowering and the rank-40 basis bound the error (S3 observed 1.6e-5 of the peak)
    assert(worst < 1e-3 * peak, s"worst $worst of peak $peak")

  test("prediction is linear in the amplitudes"):
    val other = Array(-0.3, 0.8, 1.1, -2.0, 0.2, 0.5)
    val sum = predict(amps.zip(other).map(_ + _))
    val a = predict(amps)
    val b = predict(other)
    var worst = 0.0
    var t = 0
    while t < rows do
      worst = math.max(worst, math.abs(sum(t) - a(t) - b(t)))
      t += 1
    assert(worst < 1e-12, s"worst $worst")

  test("a trial with amplitude zero contributes nothing; a lone trial equals its own regressor"):
    val lone = Array(0.0, 0.0, 1.0, 0.0, 0.0, 0.0)
    val got = predict(lone)
    // before the onset (lag < 0) the regressor is exactly zero
    assert((0 until 21).forall(t => got(t) == 0.0))
    assert(got.exists(_ != 0.0))
    assertEquals(predict(Array.fill(onsets.length)(0.0)).count(_ != 0.0), 0)

  test("typed refusals: amplitude count, non-finite amplitude, output length, shape outside the chart"):
    val out = new Array[Double](rows)
    assertEquals(
      TrialHeldOutPrediction.signalInto(design, theta, Array(1.0), out),
      Left(TrialHeldOutError.AmplitudeCount(onsets.length, 1))
    )
    assertEquals(
      TrialHeldOutPrediction.signalInto(design, theta, amps.updated(2, Double.NaN), out),
      Left(TrialHeldOutError.NonFiniteAmplitude(2))
    )
    assertEquals(
      TrialHeldOutPrediction.signalInto(design, theta, amps, new Array[Double](rows - 1)),
      Left(TrialHeldOutError.OutputLength(rows, rows - 1))
    )
    assert(TrialHeldOutPrediction.signalInto(design, Vector(50.0, 0.0), amps, out).left.exists(_.isInstanceOf[TrialHeldOutError.InvalidShape]))
    assert(TrialHeldOutPrediction.basisCoefficients(design, Vector(5.0)).isLeft)
    assert(ShapePoint.unsafe(theta).coordinates == theta)
