package scalafim.phrfcmp.run

import scalafim.fmri.fit.profile.{TrialBandedPreparation, TrialHeldOutPrediction}
import scalafim.fmri.hrf.{HrfFunctions, Lag}
import scalafim.fmri.hrf.family.{ParametricHrfFamily, ShapePoint}
import scalafim.phrfcmp.ingest.FitInputs
import scalafim.phrfcmp.prep.CommonPrep

/**
  * The PHRF "canonical shape": the point of the family chart whose unit-peak kernel is closest, in squared error on a
  * 0.1 s grid over `[0, 24 s]`, to the peak-normalised SPM canonical response (`Spmg1`) that the rLSS, LSS and LSA
  * regressors use. It is the shape at which the PHRF-can ablation is fixed and at which the PHRF-to-rLSS penalty scale
  * ([[PhrfPenaltyScale]]) is evaluated.
  *
  * Found by a deterministic nested grid search (41 x 41 points over the chart, then four zoom rounds of the same size
  * around the best point), with no randomness; the result is clamped to the chart.
  */
final case class PhrfCanonicalShape(coordinates: Vector[Double], objective: Double)

object PhrfCanonicalShape:

  private val Step = 0.1
  private val Span = 24.0

  private def lags: Array[Double] = Array.tabulate((Span / Step).toInt + 1)(_ * Step)

  private lazy val spm: Array[Double] =
    val raw = lags.map(l => HrfFunctions.spmg1(Lag(l)))
    val peak = raw.map(math.abs).max
    raw.map(_ / peak)

  /** Squared distance of the family's unit-peak kernel at `coordinates` to the normalised canonical response. */
  def objective(family: ParametricHrfFamily, coordinates: Vector[Double]): Double =
    val l = lags
    val k = new Array[Double](l.length)
    family.evalInto(l, ShapePoint.unsafe(coordinates), k)
    var s = 0.0
    var i = 0
    while i < l.length do
      val d = k(i) - spm(i)
      s += d * d
      i += 1
    s

  /** Only two-coordinate charts are supported (the Gaussian pilot family); anything else is a typed refusal. */
  def derive(family: ParametricHrfFamily): Either[String, PhrfCanonicalShape] =
    if family.dimension != 2 then Left("canonical shape search supports two-coordinate charts only")
    else
      val chart = family.chart
      val lo = Array.tabulate(2)(chart.lower(_))
      val hi = Array.tabulate(2)(chart.upper(_))
      var center = Array.tabulate(2)(d => 0.5 * (lo(d) + hi(d)))
      var half = Array.tabulate(2)(d => 0.5 * (hi(d) - lo(d)))
      var best = Double.PositiveInfinity
      val n = 41
      var round = 0
      while round < 5 do
        val a0 = math.max(lo(0), center(0) - half(0)); val a1 = math.min(hi(0), center(0) + half(0))
        val b0 = math.max(lo(1), center(1) - half(1)); val b1 = math.min(hi(1), center(1) + half(1))
        var bestA = center(0)
        var bestB = center(1)
        var i = 0
        while i < n do
          val a = a0 + (a1 - a0) * i / (n - 1)
          var j = 0
          while j < n do
            val b = b0 + (b1 - b0) * j / (n - 1)
            val o = objective(family, Vector(a, b))
            if o < best then
              best = o
              bestA = a
              bestB = b
            j += 1
          i += 1
        center = Array(bestA, bestB)
        half = Array(2.0 * (a1 - a0) / (n - 1), 2.0 * (b1 - b0) / (n - 1))
        round += 1
      if best.isFinite then Right(PhrfCanonicalShape(Vector(center(0), center(1)), best)) else Left("non-finite canonical objective")

/**
  * What the penalty-scale derivation found for one dataset (a receipt; the scale itself is `scale`).
  *
  * @param phrfNorm2 sum over trials of the whitened squared norm of the PHRF trial regressor at the canonical shape
  * @param rlssNorm2 the same for the rLSS (canonical `Spmg1`, peak 1) regressors
  */
final case class PhrfPenaltyReceipt(scale: PenaltyScale, shape: PhrfCanonicalShape, rlssNorm2: Double, phrfNorm2: Double)

/**
  * The real PHRF-to-rLSS penalty scale (design 2.2; S4 deferred it to here). S4 derived `p = kappa^2 (1 - 1/n_c) lambda`
  * with `kappa^2` the ratio of whitened squared regressor norms, rLSS over PHRF at the canonical shape, and
  * `(1 - 1/n_c)` the within-condition centring factor; this object evaluates it on a real dataset:
  *
  *  - `phrfNorm2(i)` is read from the PHRF `TrialBandedPreparation`'s own packed Gram blocks at the canonical shape's
  *    kernel-basis coefficients ([[PenaltyScale.phrfNorm2]]), whitened with the shared plan (so it is exactly the quantity
  *    PHRF's `lambda` is added to);
  *  - `rlssNorm2(i)` is the squared norm of the whitened canonical regressor column of the shared native inputs.
  *
  * The pilot's rLSS arm must be built from [[PhrfTrialRunner.rlssSettings]], which calls this; `PenaltyScale.Identity` is
  * a test placeholder.
  */
object PhrfPenaltyScale:

  def derive(inputs: FitInputs, prep: CommonPrep, config: PhrfTrialConfig): Either[PhrfTrialRefusal, PhrfPenaltyReceipt] =
    for
      _ <- PhrfTrialRunner.checked(inputs, prep)
      basis <- PhrfTrialRunner.basisOf(config)
      shape <- PhrfCanonicalShape.derive(basis.family).left.map(m => PhrfTrialRefusal.Setup("canonical_shape", m.take(40).filter(_.isLetterOrDigit)))
      problem <- PhrfTrialAssembly.problem(inputs, prep, prep.segments.map(_.runIndex), prep.sigma2All.sigma2, config)
      expanded <- PhrfTrialAssembly.expandedOf(problem, basis)
      tb <- TrialBandedPreparation
        .prepare(expanded, Some(problem.spec.plan), None, 1.0)
        .left.map(e => PhrfTrialRefusal.Dataset("penalty_scale", PhrfTrialAssembly.fmt(e)))
      coef <- TrialHeldOutPrediction.basisCoefficients(expanded, shape.coordinates).left.map(e => PhrfTrialRefusal.Setup("penalty_scale", PhrfTrialAssembly.fmt(e)))
      phrfNorm2 <- PenaltyScale.phrfNorm2(tb, coef).left.map(PhrfTrialRefusal.Df(_))
      native <- TrialNativeInputs.from(inputs, prep).left.map(e => PhrfTrialRefusal.Inconsistent(e.code))
      rlssNorm2 = Array.tabulate(native.trials) { i =>
        var s = 0.0
        var t = 0
        while t < native.timepoints do
          val x = native.trialDesign(t, i)
          s += x * x
          t += 1
        s
      }
      scale <- PenaltyScale.derive(rlssNorm2, phrfNorm2, native.trialCond).left.map(PhrfTrialRefusal.Df(_))
    yield PhrfPenaltyReceipt(scale, shape, rlssNorm2.sum, phrfNorm2.sum)
