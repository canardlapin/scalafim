package scalafim.fmri.fit

import gale.linalg.{DMat, DMatBuilder, LinAlgError, QR, QROptions, QRPivoting}

/**
  * How the unpenalised "group" columns of [[RidgeLeastSquaresSeparate]] are formed.
  *
  * For trial `i` in group `g(i)` the model is `y = sum_g m_g X_g + delta_i x_i + F gamma + e`, with `X_g` the sum of the
  * single-trial regressors of group `g` (trial `i` included). Only `delta_i` is penalised.
  */
enum RidgeLssGrouping:
  /** One group per condition: `groups(i)` in `0 until G`, every id used. Trials shrink toward their condition. */
  case ByGroup(groups: Vector[Int])

  /** One group holding every trial. With ridge 0 this is exactly least squares separate (single "rest" column). */
  case Pooled

/**
  * Condition-centred ridge least squares separate (rLSS).
  *
  * With `Z = [X_1 .. X_G, F]` and `x~_i` the residual of `x_i` on `Z`:
  * `delta_i = x~_i' y / (q_i + r)` with `q_i = x~_i' x~_i`, and the amplitude of trial `i` is
  * `m_g(i) + delta_i`, where `theta = Z^+ (y - x_i delta_i)` gives `m_g(i) = (Z^+ y)_g(i) - (Z^+ x_i)_g(i) delta_i`.
  * `Z` and `x~` are fixed across trials, so one pivoted QR of `Z` serves every trial, voxel and ridge value.
  * All linear algebra is Gale's pivoted QR; the per-element combination is allocation-free `while` loops.
  */
object RidgeLeastSquaresSeparate:

  def prepare(
      trials: LssTrialDesign,
      fixed: LssFixedDesign,
      grouping: RidgeLssGrouping,
      options: LssOptions = LssOptions()
  ): Either[FitError, RidgeLssPrepared] =
    if fixed.timepoints != trials.timepoints then Left(FitError.RowMismatch(trials.timepoints, fixed.timepoints))
    else if containsNonFinite(trials.value) then Left(FitError.NonFiniteInput("rLSS trial design"))
    else if containsNonFinite(fixed.value) then Left(FitError.NonFiniteInput("rLSS fixed design"))
    else
      resolveGroups(grouping, trials.trials).flatMap { (groupOf, groupCount) =>
        val rows = trials.timepoints
        val n = trials.trials
        val f = fixed.predictors
        val z = DMatBuilder.zeros(rows, groupCount + f)
        var t = 0
        while t < rows do
          var i = 0
          while i < n do
            val g = groupOf(i)
            z(t, g) = z(t, g) + trials.value(t, i)
            i += 1
          var c = 0
          while c < f do
            z(t, groupCount + c) = fixed.value(t, c)
            c += 1
          t += 1
        val zMat = z.result()
        if rows < zMat.cols then Left(FitError.SingularDesign(LinAlgError.UnsupportedOperation("underdetermined least squares")))
        else
          val qr = zMat.qr(QROptions(QRPivoting.Column, Some(options.rankTol)))
          val rank = qr.diagnostics.rank.getOrElse(qr.r.rows)
          if rank < zMat.cols then Left(FitError.SingularDesign(LinAlgError.RankDeficient(rank, zMat.cols)))
          else
            for
              loadings <- qr.solveLeastSquares(trials.value).left.map(FitError.SingularDesign.apply)
              residualized <- qr.residualize(trials.value).left.map(FitError.SingularDesign.apply)
            yield
              val q = new Array[Double](n)
              val own = new Array[Double](n)
              var i = 0
              while i < n do
                var s = 0.0
                var r = 0
                while r < rows do
                  val v = residualized(r, i)
                  s += v * v
                  r += 1
                q(i) = s
                own(i) = loadings(groupOf(i), i)
                i += 1
              new RidgeLssPrepared(trials.trialNames, groupCount, f, rank, groupOf, residualized, own, qr, q, options)
      }

  /** Prepare and fit at one ridge. */
  def fit(
      trials: LssTrialDesign,
      response: ResponseBlock,
      fixed: LssFixedDesign,
      grouping: RidgeLssGrouping,
      ridge: Double,
      options: LssOptions = LssOptions()
  ): Either[FitError, RidgeLssFit] =
    prepare(trials, fixed, grouping, options).flatMap(_.project(response)).flatMap(_.solve(ridge))

  private def resolveGroups(grouping: RidgeLssGrouping, trials: Int): Either[FitError, (Array[Int], Int)] =
    grouping match
      case RidgeLssGrouping.Pooled => Right((new Array[Int](trials), 1))
      case RidgeLssGrouping.ByGroup(groups) =>
        if groups.length != trials then
          Left(FitError.UnsupportedLssDesign(s"rLSS grouping has ${groups.length} entries for $trials trials"))
        else if groups.exists(_ < 0) then Left(FitError.UnsupportedLssDesign("rLSS group ids must be non-negative"))
        else if groups.exists(_ >= trials) then Left(FitError.UnsupportedLssDesign("rLSS group ids must be dense 0 until G"))
        else
          val count = groups.max + 1
          val used = new Array[Boolean](count)
          groups.foreach(g => used(g) = true)
          if used.exists(!_) then Left(FitError.UnsupportedLssDesign("rLSS group ids must be dense 0 until G"))
          else Right((groups.toArray, count))

  private def containsNonFinite(m: DMat): Boolean =
    var r = 0
    while r < m.rows do
      var c = 0
      while c < m.cols do
        if !m(r, c).isFinite then return true
        c += 1
      r += 1
    false

/**
  * A prepared rLSS design: the residualised trial regressors, `q_i`, and the factor of `Z`. Shape-free and data-free, so
  * `q` and [[amplitudeEdf]] are available before any response exists.
  */
final class RidgeLssPrepared private[fit] (
    val trialNames: Vector[String],
    val groupCount: Int,
    val fixedColumns: Int,
    val rank: Int,
    private[fit] val groupOf: Array[Int],
    private[fit] val residualized: DMat,
    private[fit] val ownLoading: Array[Double],
    private[fit] val zQr: QR,
    private val qValues: Array[Double],
    val options: LssOptions
):
  def timepoints: Int = residualized.rows
  def trials: Int = trialNames.length

  /** `q_i = x~_i' x~_i`: the information the data carry about trial `i` after the groups and `F` are removed. */
  def q: Vector[Double] = qValues.toVector

  /**
    * `(1/N) sum_i q_i / (q_i + penalty)`: the mean information ratio of the penalised deviation block. This is a q-based
    * DIAGNOSTIC, not an effective degrees of freedom; see [[amplitudeEdf]].
    */
  def informationRatio(penalty: Double): Double = RidgeLssPrepared.informationRatio(qValues, penalty)

  /** `phi_i = (Z^+ x_i)_{g(i)}`: the loading of trial `i` on its own group column; `sum over a group of phi_i = 1`. */
  def selfLoading: Vector[Double] = ownLoading.toVector

  /**
    * Effective degrees of freedom of the rLSS amplitude estimator at ridge `r`: `tr(S X) = sum_i d ahat_i / d a_i`, the sum
    * of the self-influences of the trial amplitudes (condition levels included, nuisance projected out). Closed form
    * `sum_i [ phi_i + (1 - phi_i) q_i / (q_i + r) ]`: equals `trials` at `r = 0` and tends to `groupCount` as `r` grows.
    */
  def amplitudeEdf(ridge: Double): Double =
    var s = 0.0
    var i = 0
    while i < qValues.length do
      s += ownLoading(i) + (1.0 - ownLoading(i)) * qValues(i) / (qValues(i) + ridge)
      i += 1
    s

  /** Group of trial `i` (a copy). */
  def groups: Vector[Int] = groupOf.toVector

  def project(response: ResponseBlock): Either[FitError, RidgeLssProjection] =
    if response.timepoints != timepoints then Left(FitError.RowMismatch(timepoints, response.timepoints))
    else
      for theta <- zQr.solveLeastSquares(response.value).left.map(FitError.SingularDesign.apply)
      yield new RidgeLssProjection(this, residualized.t * response.value, theta)

object RidgeLssPrepared:

  /** `(1/N) sum_i q_i / (q_i + penalty)`; strictly decreasing in `penalty` when every `q_i > 0` (diagnostic, not edf). */
  def informationRatio(q: Array[Double], penalty: Double): Double =
    var s = 0.0
    var i = 0
    while i < q.length do
      s += q(i) / (q(i) + penalty)
      i += 1
    s / q.length

/** The response-dependent part of rLSS, independent of the ridge: `x~' Y` and `Z^+ Y`. */
final class RidgeLssProjection private[fit] (prepared: RidgeLssPrepared, cross: DMat, theta: DMat):
  def voxels: Int = cross.cols

  /** Solve every trial and voxel at one ridge `r >= 0`; `Left` when some trial has `q_i + r` at or below `eps`. */
  def solve(ridge: Double): Either[FitError, RidgeLssFit] =
    if !(ridge >= 0.0 && ridge.isFinite) then
      Left(FitError.UnsupportedLssDesign(s"rLSS ridge must be finite and non-negative, got $ridge"))
    else
      val n = prepared.trials
      val v = cross.cols
      val q = prepared.q
      val weak = Vector.newBuilder[String]
      var i = 0
      while i < n do
        if q(i) + ridge <= prepared.options.eps then weak += prepared.trialNames(i)
        i += 1
      val bad = weak.result()
      if bad.nonEmpty then Left(FitError.NonEstimableLssTrials(bad))
      else
        val amp = DMatBuilder.zeros(n, v)
        val dev = DMatBuilder.zeros(n, v)
        i = 0
        while i < n do
          val denom = q(i) + ridge
          val g = prepared.groupOf(i)
          val keep = 1.0 - prepared.ownLoading(i)
          var c = 0
          while c < v do
            val d = cross(i, c) / denom
            dev(i, c) = d
            amp(i, c) = theta(g, c) + keep * d
            c += 1
          i += 1
        Right(RidgeLssFit(ridge, CoefficientBlock(amp.result()), CoefficientBlock(dev.result()), prepared.trialNames))

/** rLSS result: trial amplitudes `m_g(i) + delta_i` and the penalised deviations `delta_i`, both N x V. */
final case class RidgeLssFit(
    ridge: Double,
    amplitudes: CoefficientBlock,
    deviations: CoefficientBlock,
    trialNames: Vector[String]
):
  require(amplitudes.predictors == trialNames.length && deviations.predictors == trialNames.length, "rows must match trials")
