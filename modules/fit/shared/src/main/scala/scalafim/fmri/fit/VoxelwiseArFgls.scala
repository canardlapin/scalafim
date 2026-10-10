package scalafim.fmri.fit

import gale.linalg.{CholeskyOptions, DMat, Matrix, QROptions, QRPivoting}
import scalafim.fmri.ar.{
  AcvfBias,
  ArFitOptions,
  ArOrder,
  CorrectionBudget,
  NoiseEstimationLayout,
  NoiseFit,
  NoisePooling,
  RunCorrection,
  StationaryArFactor,
  TimeSegment,
  TimeSegments
}

/** How censored rows enter AR whitening.
  *
  *   - `RestartAfterCensor` is the current GLS approximation: censored rows stay in the regression and whitening
  *     restarts on the following row, with the truncated initial rows of [[scalafim.fmri.ar.WhiteningTransform]].
  *     For noise that is continuous through the censored frame, the restarted rows remain correlated with the
  *     preceding segment, so even known-coefficient inference is approximate.
  *   - `ContinuousMissing` treats the noise as one stationary AR process per run and every censored row as a missing
  *     observation. Each censored row is absorbed by an indicator column; GLS with those indicators equals GLS on the
  *     retained rows under their exact marginal covariance. Whitening is continuous within each run and uses the
  *     exact stationary initial factor, so the residual degrees of freedom fall by one per censored row.
  */
enum CensorContinuity:
  case RestartAfterCensor
  case ContinuousMissing

/** Rows that share one estimated AR filter within a voxel. */
enum VoxelArScope:
  case PerRun
  case AcrossRuns

/** Source of the AR coefficients used to whiten each voxel. */
enum VoxelArCoefficients:
  /** Known coefficients, one vector per run: a control whose only estimated covariance parameter is the scale. */
  case Known(phiByRun: Vector[Vector[Double]])

  /** Per-voxel estimates from the one-pass OLS residual-bias-corrected Yule-Walker estimator. */
  case Estimated(scope: VoxelArScope, correctionCeiling: Int)

/** How inference treats the estimated noise covariance.
  *
  *   - `Conditional` reports the plug-in covariance `sigma^2 (Z'Z)^-1` of the whitened fit with the nominal residual
  *     degrees of freedom: it conditions on the estimated filter.
  *   - `KenwardRoger` applies Kenward and Roger (1997, Biometrics 53:983-997) to the covariance model
  *     `V(theta) = sigma^2 (W(phi)'W(phi))^-1` with `theta` the scale and every estimated AR coefficient. The
  *     coefficient covariance is inflated by the small-sample estimated-covariance terms, and each Wald test carries a
  *     scale and denominator degrees of freedom from moment matching. The parameter covariance is the inverse expected
  *     REML information evaluated at the estimate; the Yule-Walker estimator shares that first-order covariance. With
  *     known coefficients it reproduces the exact `F(q, n - rank)` test.
  */
enum CovarianceUncertainty:
  case Conditional
  case KenwardRoger

final case class VoxelwiseArSpec(
    order: Int,
    coefficients: VoxelArCoefficients,
    censor: CensorContinuity,
    uncertainty: CovarianceUncertainty
):
  require(order >= 1, "voxelwise AR order must be at least 1")
  coefficients match
    case VoxelArCoefficients.Known(phiByRun) =>
      require(
        phiByRun.nonEmpty && phiByRun.forall(phi => phi.length == order && phi.forall(_.isFinite)),
        "known AR coefficients must match the order for every run"
      )
    case VoxelArCoefficients.Estimated(_, ceiling) =>
      require(ceiling >= 1, "AR residual-bias correction ceiling must be positive")

object VoxelwiseArSpec:
  def make(
      order: Int,
      coefficients: VoxelArCoefficients,
      censor: CensorContinuity,
      uncertainty: CovarianceUncertainty
  ): Either[FitError, VoxelwiseArSpec] =
    try Right(VoxelwiseArSpec(order, coefficients, censor, uncertainty))
    catch case error: IllegalArgumentException => Left(FitError.UnsupportedAutocorrelation(error.getMessage))

/** One Wald test. `statistic` is compared with `F(numeratorDf, denominatorDf)`; under `Conditional` the scale is one
  * and the denominator df is the residual df.
  */
final case class ArFglsTest(
    statistic: Double,
    numeratorDf: Int,
    denominatorDf: Double,
    scale: Double,
    unscaledWald: Double
)

/** Kenward-Roger ingredients for one voxel, over the augmented design (original predictors then censor indicators).
  */
final case class KenwardRogerTerms private[fit] (
    parameterCovariance: DMat,
    firstDerivativeTerms: Vector[DMat],
    adjustedCovariance: DMat
)

/** One voxel's fit. `coefficients`, `covariance` and `adjustedCovariance` cover the original predictors only;
  * `whitenedResiduals` are on the full row axis.
  */
final case class VoxelArFglsFit private[fit] (
    coefficients: Vector[Double],
    phiByRun: Vector[Vector[Double]],
    residualVariance: Double,
    residualDf: Int,
    covariance: DMat,
    adjustedCovariance: DMat,
    corrections: Vector[RunCorrection],
    whitenedResiduals: Vector[Double],
    private[fit] val fullCoefficients: Vector[Double],
    private[fit] val fullCovariance: DMat,
    kenwardRoger: Option[KenwardRogerTerms]
):
  def predictors: Int = coefficients.length

  /** Wald test of `contrast * beta = hypothesis`; `contrast` is `q x predictors` with full row rank. */
  def test(contrast: DMat, hypothesis: Vector[Double]): Either[FitError, ArFglsTest] =
    VoxelwiseArFgls.test(this, contrast, hypothesis)

final case class VoxelwiseArFit private[fit] (spec: VoxelwiseArSpec, voxels: Vector[VoxelArFglsFit])

/** Voxelwise AR(p) FGLS with explicit censor-continuity and estimated-covariance inference policies. */
object VoxelwiseArFgls:

  def fit(
      design: DMat,
      response: DMat,
      runs: Vector[TimeSegment],
      censoredRows: Vector[Int],
      spec: VoxelwiseArSpec
  ): Either[FitError, VoxelwiseArFit] =
    val rows = design.rows
    val censored = censoredRows.distinct.sorted
    if response.rows != rows then Left(FitError.RowMismatch(rows, response.rows))
    else if response.cols == 0 then Left(FitError.EmptyResponse)
    else if censored.exists(row => row < 0 || row >= rows) then
      Left(FitError.InvalidFitAxis("censored rows", s"must lie within 0 until $rows"))
    else
      for
        _ <- TimeSegments.validateCoverage(runs, rows).left.map(Gls.arToFitError)
        augmented = spec.censor match
          case CensorContinuity.RestartAfterCensor => design
          case CensorContinuity.ContinuousMissing  => withIndicators(design, censored)
        segments = spec.censor match
          case CensorContinuity.RestartAfterCensor => TimeSegments.withCensorResets(runs, censored.toSet)
          case CensorContinuity.ContinuousMissing  => runs
        layout <- NoiseEstimationLayout.excludingRows(segments, rows, censored.toSet).left.map(Gls.arToFitError)
        initial <- Ols.fit(DesignMatrix.unsafe(augmented), ResponseBlock.unsafe(response))
        _ <-
          if initial.diagnostics.fullRank then Right(())
          else Left(FitError.UnsupportedAutocorrelation("voxelwise AR FGLS requires a full-rank augmented design"))
        residuals = Gls.residualMatrix(augmented, response, initial.coefficients.value)
        coefficientSets <- coefficientsByVoxel(augmented, residuals, layout, runs.length, spec)
        voxels <- traverse(coefficientSets.indices.toVector) { voxel =>
          val (phi, corrections) = coefficientSets(voxel)
          fitVoxel(augmented, design.cols, column(response, voxel), segments, phi, corrections, spec)
        }
      yield VoxelwiseArFit(spec, voxels)

  private def coefficientsByVoxel(
      design: DMat,
      residuals: DMat,
      layout: NoiseEstimationLayout,
      runCount: Int,
      spec: VoxelwiseArSpec
  ): Either[FitError, Vector[(Vector[Vector[Double]], Vector[RunCorrection])]] =
    spec.coefficients match
      case VoxelArCoefficients.Known(phiByRun) =>
        if phiByRun.length != runCount then
          Left(FitError.UnsupportedAutocorrelation(s"known AR coefficients cover ${phiByRun.length} of $runCount runs"))
        else Right(Vector.fill(residuals.cols)((phiByRun, Vector.empty)))
      case VoxelArCoefficients.Estimated(scope, ceiling) =>
        val options = ArFitOptions(
          order = ArOrder.Fixed(spec.order),
          pooling = scope match
            case VoxelArScope.PerRun     => NoisePooling.Run
            case VoxelArScope.AcrossRuns => NoisePooling.Global
          ,
          exactFirstAr1 = true
        )
        AcvfBias
          .prepare(design, layout, CorrectionBudget.Adaptive(ceiling), spec.order)
          .left
          .map(Gls.arToFitError)
          .flatMap { prepared =>
            traverse((0 until residuals.cols).toVector) { voxel =>
              NoiseFit
                .estimate(column(residuals, voxel), layout, options, design, prepared)
                .left
                .map(Gls.arToFitError)
                .map { noise =>
                  val phi = (0 until runCount).toVector.map { run =>
                    val segment = noise.plan.segments.find(_.runIndex == run).getOrElse(noise.plan.segments.head)
                    noise.plan.coefficientsFor(segment).phi.padTo(spec.order, 0.0)
                  }
                  (phi, noise.corrections)
                }
            }
          }

  private[fit] def fitVoxel(
      design: DMat,
      originalPredictors: Int,
      response: DMat,
      segments: Vector[TimeSegment],
      phi: Vector[Vector[Double]],
      corrections: Vector[RunCorrection],
      spec: VoxelwiseArSpec
  ): Either[FitError, VoxelArFglsFit] =
    for
      model <- ArWhiteningModel.build(segments, phi, spec.order, spec.censor)
      z = model.apply(design)
      zy = model.apply(response)
      whitened <- Ols.fit(DesignMatrix.unsafe(z), ResponseBlock.unsafe(zy))
      sigma2 = whitened.residualVariance(0)
      _ <-
        if sigma2 > 0.0 && sigma2.isFinite then Right(())
        else Left(FitError.UnsupportedAutocorrelation(s"non-positive whitened residual variance $sigma2"))
      full = scaled(whitened.normalizedCovariance, sigma2)
      beta = Vector.tabulate(design.cols)(i => whitened.coefficients(i, 0))
      kr <- spec.uncertainty match
        case CovarianceUncertainty.Conditional => Right(None)
        case CovarianceUncertainty.KenwardRoger =>
          KenwardRoger.terms(model, design, z, full, sigma2, spec).map(Some(_))
    yield
      val adjusted = kr.fold(full)(_.adjustedCovariance)
      VoxelArFglsFit(
        coefficients = beta.take(originalPredictors),
        phiByRun = phi,
        residualVariance = sigma2,
        residualDf = whitened.residualDegreesOfFreedom.value,
        covariance = block(full, originalPredictors),
        adjustedCovariance = block(adjusted, originalPredictors),
        corrections = corrections,
        whitenedResiduals = Vector.tabulate(zy.rows) { row =>
          var fitted = 0.0
          var col = 0
          while col < z.cols do
            fitted += z(row, col) * beta(col)
            col += 1
          zy(row, 0) - fitted
        },
        fullCoefficients = beta,
        fullCovariance = full,
        kenwardRoger = kr
      )

  private[fit] def test(fit: VoxelArFglsFit, contrast: DMat, hypothesis: Vector[Double]): Either[FitError, ArFglsTest] =
    val q = contrast.rows
    val p = fit.fullCoefficients.length
    if q == 0 || contrast.cols != fit.predictors || hypothesis.length != q then
      Left(FitError.InvalidFitAxis("contrast", s"expected q x ${fit.predictors} with q hypothesis values"))
    else
      val l = Matrix.tabulate(q, p)((row, col) => if col < contrast.cols then contrast(row, col) else 0.0)
      val difference = Matrix.tabulate(q, 1)((row, _) =>
        var value = -hypothesis(row)
        var col = 0
        while col < p do
          value += l(row, col) * fit.fullCoefficients(col)
          col += 1
        value
      )
      fit.kenwardRoger match
        case None =>
          wald(l, fit.fullCovariance, difference).map { value =>
            ArFglsTest(value / q, q, fit.residualDf.toDouble, 1.0, value / q)
          }
        case Some(terms) =>
          for
            value <- wald(l, terms.adjustedCovariance, difference)
            theta <- symmetricInverse(l * fit.fullCovariance * l.t).map(inner => l.t * inner * l)
            moments <- KenwardRoger.moments(theta, fit.fullCovariance, terms, q)
          yield
            val (scale, denominator) = moments
            ArFglsTest(scale * value / q, q, denominator, scale, value / q)

  private def wald(l: DMat, covariance: DMat, difference: DMat): Either[FitError, Double] =
    symmetricInverse(l * covariance * l.t).map(inner => (difference.t * inner * difference)(0, 0))

  private[fit] def symmetricInverse(matrix: DMat): Either[FitError, DMat] =
    matrix
      .cholesky(CholeskyOptions())
      .flatMap(_.solve(Matrix.eye(matrix.rows)))
      .left
      .map(error => FitError.UnsupportedAutocorrelation(s"covariance is not positive definite: $error"))

  private[fit] def withIndicators(design: DMat, censored: Vector[Int]): DMat =
    Matrix.tabulate(design.rows, design.cols + censored.length) { (row, col) =>
      if col < design.cols then design(row, col) else if censored(col - design.cols) == row then 1.0 else 0.0
    }

  private def column(matrix: DMat, col: Int): DMat =
    Matrix.tabulate(matrix.rows, 1)((row, _) => matrix(row, col))

  private def scaled(matrix: DMat, factor: Double): DMat =
    Matrix.tabulate(matrix.rows, matrix.cols)((row, col) => matrix(row, col) * factor)

  private def block(matrix: DMat, size: Int): DMat =
    Matrix.tabulate(size, size)((row, col) => matrix(row, col))

  private def traverse[A, B](values: Vector[A])(f: A => Either[FitError, B]): Either[FitError, Vector[B]] =
    val out = Vector.newBuilder[B]
    var index = 0
    while index < values.length do
      f(values(index)) match
        case Left(error)  => return Left(error)
        case Right(value) => out += value
      index += 1
    Right(out.result())

/** A lower-triangular, run-block-diagonal AR whitening operator `W(phi)` held as sparse rows.
  *
  * Rows at least `p` past their segment start apply the AR recursion. The first rows of each segment use either the
  * exact stationary factor (`ContinuousMissing`) or the truncated restart of the current GLS whitener
  * (`RestartAfterCensor`). Coefficient derivatives are analytic on recursion rows; the small initial block is
  * differentiated by central differences.
  */
private[fit] final class ArWhiteningModel private (
    val rows: Int,
    val order: Int,
    val segments: Vector[TimeSegment],
    val phi: Vector[Vector[Double]],
    val censor: CensorContinuity,
    private val columns: Array[Array[Int]],
    private val values: Array[Array[Double]]
):
  def runCount: Int = phi.length

  def apply(input: DMat): DMat =
    val out = Matrix.newBuilder(rows, input.cols)
    var row = 0
    while row < rows do
      val cols = columns(row)
      val vals = values(row)
      var col = 0
      while col < input.cols do
        var sum = 0.0
        var k = 0
        while k < cols.length do
          sum += vals(k) * input(cols(k), col)
          k += 1
        out(row, col) = sum
        col += 1
      row += 1
    out.result()

  /** Dense inverse block `W_r^-1` for the rows of `run`, indexed from the run's first row. */
  def inverseBlock(start: Int, length: Int): Array[Double] =
    val out = new Array[Double](length * length)
    var col = 0
    while col < length do
      var row = col
      val segment = segments.find(s => s.contains(start + col)).get
      while row < length && start + row < segment.endExclusive do
        val cols = columns(start + row)
        val vals = values(start + row)
        var sum = if row == col then 1.0 else 0.0
        var diagonal = 1.0
        var k = 0
        while k < cols.length do
          val source = cols(k) - start
          if source == row then diagonal = vals(k)
          else if source >= col then sum -= vals(k) * out(source * length + col)
          k += 1
        out(row * length + col) = sum / diagonal
        row += 1
      col += 1
    out

private[fit] object ArWhiteningModel:
  val FirstDifferenceStep: Double = 1e-5
  val SecondDifferenceStep: Double = 1e-4

  def build(
      segments: Vector[TimeSegment],
      phi: Vector[Vector[Double]],
      order: Int,
      censor: CensorContinuity
  ): Either[FitError, ArWhiteningModel] =
    val rows = segments.last.endExclusive
    val columns = new Array[Array[Int]](rows)
    val values = new Array[Array[Double]](rows)
    val blocks = phi.map(initialBlock(_, censor))
    blocks.collectFirst { case Left(error) => error } match
      case Some(error) => Left(error)
      case None =>
        val initial = blocks.map(_.toOption.get)
        segments.foreach { segment =>
          val coefficients = phi(segment.runIndex)
          var row = segment.start
          while row < segment.endExclusive do
            val local = row - segment.start
            if local < order then
              val b = initial(segment.runIndex)
              columns(row) = Array.tabulate(local + 1)(j => segment.start + j)
              values(row) = Array.tabulate(local + 1)(j => b(local)(j))
            else
              columns(row) = Array.tabulate(order + 1)(k => row - k)
              values(row) = Array.tabulate(order + 1)(k => if k == 0 then 1.0 else -coefficients(k - 1))
            row += 1
        }
        Right(new ArWhiteningModel(rows, order, segments, phi, censor, columns, values))

  /** The `p x p` lower-triangular rule applied to the first rows of a segment. */
  def initialBlock(phi: Vector[Double], censor: CensorContinuity): Either[FitError, Array[Array[Double]]] =
    val p = phi.length
    censor match
      case CensorContinuity.ContinuousMissing =>
        StationaryArFactor(phi).left.map(Gls.arToFitError).map { factor =>
          Array.tabulate(p, p)((i, j) => if j <= i then factor.initial(i, j) else 0.0)
        }
      case CensorContinuity.RestartAfterCensor =>
        if p == 1 && math.abs(phi.head) >= 1.0 then
          Left(FitError.UnsupportedAutocorrelation(s"AR(1) coefficient ${phi.head} is not stationary"))
        else
          Right(Array.tabulate(p, p) { (i, j) =>
            if i == j then if p == 1 then math.sqrt(1.0 - phi.head * phi.head) else 1.0
            else if j < i then -phi(i - j - 1)
            else 0.0
          })

/** Kenward-Roger small-sample inference for `V(theta) = sigma^2 (W(phi)'W(phi))^-1`.
  *
  * With `D_i = dV^-1/dtheta_i = W' S_i W / sigma^2`, `S_i = A_i + A_i'` and `A_i = (dW/dphi_i) W^-1` (`S = -I/sigma^2`
  * for the scale), and whitened design `Z = W X`:
  * {{{
  * P_i = Z' S_i Z / sigma^2,  Q_ij = Z' S_i S_j Z / sigma^2,
  * R_ij = Q_ij + Q_ji - X' d2V^-1/dtheta_i dtheta_j X,
  * I_ij = tr(M S_i M S_j) / 2,  M = I - Z (Z'Z)^-1 Z'
  * }}}
  * `I` is the expected REML information, so its inverse is the parameter covariance, and
  * `Phi_A = Phi + 2 Phi [sum W_ij (Q_ij - P_i Phi P_j - R_ij / 4)] Phi`.
  */
private[fit] object KenwardRoger:

  def terms(
      model: ArWhiteningModel,
      design: DMat,
      z: DMat,
      phiCovariance: DMat,
      sigma2: Double,
      spec: VoxelwiseArSpec
  ): Either[FitError, KenwardRogerTerms] =
    val parameters = estimatedParameters(model, spec)
    val p = design.cols
    val n = model.rows
    val basis = z.qr(QROptions(pivoting = QRPivoting.Column))
    val rank = basis.diagnostics.rank.getOrElse(0)
    if rank != p then Left(FitError.UnsupportedAutocorrelation("Kenward-Roger requires a full-rank whitened design"))
    else
      val u = basis.q.slice(0, n, 0, p)
      derivatives(model, parameters).flatMap { derived =>
        val k = 1 + parameters.length
        // T_i = S_i Z and G_i = S_i U for every coefficient parameter.
        val sz = derived.map(d => blockProduct(d.s, model, z))
        val su = derived.map(d => blockProduct(d.s, model, u))
        val zdot = derived.map(d => sparseApply(d.first, design, n))
        val information = Matrix.newBuilder(k, k)
        information(0, 0) = (n - p).toDouble / (2.0 * sigma2 * sigma2)
        var i = 0
        while i < parameters.length do
          val traceS = derived(i).trace
          val traceUSU = frobeniusInner(u, su(i))
          information(0, i + 1) = -(traceS - traceUSU) / (2.0 * sigma2)
          information(i + 1, 0) = information(0, i + 1)
          var j = 0
          while j <= i do
            val trSS = blockInner(derived(i), derived(j))
            val trHSS = frobeniusInner(su(i), su(j))
            val trHSHS = frobeniusInner((u.t * su(i)), (u.t * su(j)).t)
            information(i + 1, j + 1) = 0.5 * (trSS - 2.0 * trHSS + trHSHS)
            information(j + 1, i + 1) = information(i + 1, j + 1)
            j += 1
          i += 1
        VoxelwiseArFgls.symmetricInverse(information.result()).flatMap { w =>
          val zz = z.t * z
          val pScale = scale(zz, -1.0 / (sigma2 * sigma2))
          val pTerms = pScale +: sz.map(t => scale(z.t * t, 1.0 / sigma2))
          def q(a: Int, b: Int): DMat =
            if a == 0 && b == 0 then scale(zz, 1.0 / (sigma2 * sigma2 * sigma2))
            else if a == 0 then scale(pTerms(b), -1.0 / sigma2)
            else if b == 0 then scale(pTerms(a), -1.0 / sigma2)
            else scale(sz(a - 1).t * sz(b - 1), 1.0 / sigma2)
          def r(a: Int, b: Int): Either[FitError, DMat] =
            if a == 0 && b == 0 then Right(Matrix.zeros(p, p))
            else if a == 0 then Right(scale(pTerms(b), -1.0 / sigma2))
            else if b == 0 then Right(scale(pTerms(a), -1.0 / sigma2))
            else
              secondDerivative(model, parameters(a - 1), parameters(b - 1)).map { second =>
                val wddx = sparseApply(second, design, model.rows)
                val curvature = scale(
                  wddx.t * z + zdot(a - 1).t * zdot(b - 1) + zdot(b - 1).t * zdot(a - 1) + z.t * wddx,
                  1.0 / sigma2
                )
                q(a, b) + q(b, a) - curvature
              }
          var correction = Matrix.zeros(p, p)
          var a = 0
          var failure: Option[FitError] = None
          while a < k && failure.isEmpty do
            var b = 0
            while b < k && failure.isEmpty do
              r(a, b) match
                case Left(error) => failure = Some(error)
                case Right(rab) =>
                  val term = q(a, b) - pTerms(a) * phiCovariance * pTerms(b) - scale(rab, 0.25)
                  correction = correction + scale(term, w(a, b))
              b += 1
            a += 1
          failure.toLeft {
            val adjusted = phiCovariance + scale(phiCovariance * correction * phiCovariance, 2.0)
            KenwardRogerTerms(w, pTerms, symmetrize(adjusted))
          }
        }
      }

  /** Kenward-Roger scale and denominator df for a `q`-row Wald test (pbkrtest `.KR_adjust`). */
  def moments(theta: DMat, phi: DMat, terms: KenwardRogerTerms, q: Int): Either[FitError, (Double, Double)] =
    val k = terms.firstDerivativeTerms.length
    val u = terms.firstDerivativeTerms.map(p => theta * phi * p * phi)
    val traces = u.map(trace)
    var a1 = 0.0
    var a2 = 0.0
    var i = 0
    while i < k do
      var j = 0
      while j < k do
        val w = terms.parameterCovariance(i, j)
        a1 += w * traces(i) * traces(j)
        a2 += w * frobeniusInner(u(i), u(j).t)
        j += 1
      i += 1
    val qd = q.toDouble
    val b = (a1 + 6.0 * a2) / (2.0 * qd)
    val g = ((qd + 1.0) * a1 - (qd + 4.0) * a2) / ((qd + 2.0) * a2)
    val denominator = 3.0 * qd + 2.0 * (1.0 - g)
    val c1 = g / denominator
    val c2 = (qd - g) / denominator
    val c3 = (qd + 2.0 - g) / denominator
    val expectation = 1.0 / (1.0 - a2 / qd)
    val variance = (2.0 / qd) * (1.0 + c1 * b) / ((1.0 - c2 * b) * (1.0 - c2 * b) * (1.0 - c3 * b))
    val rho = variance / (2.0 * expectation * expectation)
    val df = 4.0 + (qd + 2.0) / (qd * rho - 1.0)
    val scaleValue = df / (expectation * (df - 2.0))
    if !(a2 > 0.0) || !df.isFinite || df <= 2.0 || !scaleValue.isFinite || scaleValue <= 0.0 then
      Left(FitError.UnsupportedAutocorrelation(s"Kenward-Roger moments are degenerate: A2=$a2 df=$df scale=$scaleValue"))
    else Right(scaleValue -> df)

  /** One estimated coefficient: `phi(run)(lag)`, shared by `runs`. */
  private final case class Parameter(runs: Vector[Int], lag: Int)

  private def estimatedParameters(model: ArWhiteningModel, spec: VoxelwiseArSpec): Vector[Parameter] =
    spec.coefficients match
      case VoxelArCoefficients.Known(_) => Vector.empty
      case VoxelArCoefficients.Estimated(VoxelArScope.PerRun, _) =>
        for
          run <- (0 until model.runCount).toVector
          lag <- 0 until model.order
        yield Parameter(Vector(run), lag)
      case VoxelArCoefficients.Estimated(VoxelArScope.AcrossRuns, _) =>
        (0 until model.order).toVector.map(lag => Parameter((0 until model.runCount).toVector, lag))

  /** Sparse derivative rows (row, col, value) of `W` and the dense run blocks of `S = A + A'`. */
  private final case class Derived(first: Vector[(Int, Int, Double)], s: Vector[(Int, Int, Array[Double])]):
    def trace: Double =
      s.map { (_, length, values) =>
        var sum = 0.0
        var i = 0
        while i < length do
          sum += values(i * length + i)
          i += 1
        sum
      }.sum

  private def runBounds(model: ArWhiteningModel, run: Int): (Int, Int) =
    val owned = model.segments.filter(_.runIndex == run)
    owned.head.start -> (owned.last.endExclusive - owned.head.start)

  private def derivatives(model: ArWhiteningModel, parameters: Vector[Parameter]): Either[FitError, Vector[Derived]] =
    val inverses = (0 until model.runCount).toVector.map { run =>
      val (start, length) = runBounds(model, run)
      model.inverseBlock(start, length)
    }
    val out = Vector.newBuilder[Derived]
    var index = 0
    while index < parameters.length do
      firstDerivative(model, parameters(index)) match
        case Left(error) => return Left(error)
        case Right(first) =>
          val blocks = parameters(index).runs.map { run =>
            val (start, length) = runBounds(model, run)
            val inverse = inverses(run)
            val a = new Array[Double](length * length)
            first.foreach { (row, source, value) =>
              if row >= start && row < start + length then
                val r = row - start
                val s = source - start
                var col = 0
                while col <= s do
                  a(r * length + col) += value * inverse(s * length + col)
                  col += 1
            }
            val sym = new Array[Double](length * length)
            var i = 0
            while i < length do
              var j = 0
              while j < length do
                sym(i * length + j) = a(i * length + j) + a(j * length + i)
                j += 1
              i += 1
            (start, length, sym)
          }
          out += Derived(first, blocks)
      index += 1
    Right(out.result())

  private def firstDerivative(model: ArWhiteningModel, parameter: Parameter): Either[FitError, Vector[(Int, Int, Double)]] =
    val out = Vector.newBuilder[(Int, Int, Double)]
    val h = ArWhiteningModel.FirstDifferenceStep
    var failure: Option[FitError] = None
    parameter.runs.foreach { run =>
      val phi = model.phi(run)
      val blocks =
        for
          plus <- ArWhiteningModel.initialBlock(phi.updated(parameter.lag, phi(parameter.lag) + h), model.censor)
          minus <- ArWhiteningModel.initialBlock(phi.updated(parameter.lag, phi(parameter.lag) - h), model.censor)
        yield (plus, minus)
      blocks match
        case Left(error) => failure = Some(error)
        case Right((plus, minus)) =>
          model.segments.filter(_.runIndex == run).foreach { segment =>
            var row = segment.start
            while row < segment.endExclusive do
              val local = row - segment.start
              if local < model.order then
                var j = 0
                while j <= local do
                  val value = (plus(local)(j) - minus(local)(j)) / (2.0 * h)
                  if value != 0.0 then out += ((row, segment.start + j, value))
                  j += 1
              else out += ((row, row - parameter.lag - 1, -1.0))
              row += 1
          }
    }
    failure.toLeft(out.result())

  private def secondDerivative(
      model: ArWhiteningModel,
      left: Parameter,
      right: Parameter
  ): Either[FitError, Vector[(Int, Int, Double)]] =
    val out = Vector.newBuilder[(Int, Int, Double)]
    val h = ArWhiteningModel.SecondDifferenceStep
    val shared = left.runs.intersect(right.runs)
    var failure: Option[FitError] = None
    shared.foreach { run =>
      val phi = model.phi(run)
      def shifted(a: Double, b: Double) =
        ArWhiteningModel.initialBlock(
          phi.updated(left.lag, phi(left.lag) + a).updated(right.lag, phi(right.lag) + b),
          model.censor
        )
      val blocks: Either[FitError, (Int, Int) => Double] =
        if left.lag == right.lag then
          for
            plus <- ArWhiteningModel.initialBlock(phi.updated(left.lag, phi(left.lag) + h), model.censor)
            centre <- ArWhiteningModel.initialBlock(phi, model.censor)
            minus <- ArWhiteningModel.initialBlock(phi.updated(left.lag, phi(left.lag) - h), model.censor)
          yield (i: Int, j: Int) => (plus(i)(j) - 2.0 * centre(i)(j) + minus(i)(j)) / (h * h)
        else
          for
            pp <- shifted(h, h)
            pm <- shifted(h, -h)
            mp <- shifted(-h, h)
            mm <- shifted(-h, -h)
          yield (i: Int, j: Int) => (pp(i)(j) - pm(i)(j) - mp(i)(j) + mm(i)(j)) / (4.0 * h * h)
      blocks match
        case Left(error) => failure = Some(error)
        case Right(entry) =>
          model.segments.filter(_.runIndex == run).foreach { segment =>
            var local = 0
            while local < math.min(model.order, segment.length) do
              var j = 0
              while j <= local do
                val value = entry(local, j)
                if value != 0.0 then out += ((segment.start + local, segment.start + j, value))
                j += 1
              local += 1
          }
    }
    failure.toLeft(out.result())

  private def sparseApply(entries: Vector[(Int, Int, Double)], input: DMat, rows: Int): DMat =
    val out = Array.ofDim[Double](rows * input.cols)
    entries.foreach { (row, source, value) =>
      var col = 0
      while col < input.cols do
        out(row * input.cols + col) += value * input(source, col)
        col += 1
    }
    Matrix.tabulate(rows, input.cols)((row, col) => out(row * input.cols + col))

  private def blockProduct(blocks: Vector[(Int, Int, Array[Double])], model: ArWhiteningModel, input: DMat): DMat =
    val out = Array.ofDim[Double](model.rows * input.cols)
    blocks.foreach { (start, length, values) =>
      var i = 0
      while i < length do
        var j = 0
        while j < length do
          val sij = values(i * length + j)
          if sij != 0.0 then
            var col = 0
            while col < input.cols do
              out((start + i) * input.cols + col) += sij * input(start + j, col)
              col += 1
          j += 1
        i += 1
    }
    Matrix.tabulate(model.rows, input.cols)((row, col) => out(row * input.cols + col))

  private def blockInner(left: Derived, right: Derived): Double =
    var sum = 0.0
    left.s.foreach { (start, length, a) =>
      right.s.find(_._1 == start).foreach { (_, _, b) =>
        var i = 0
        while i < length * length do
          sum += a(i) * b(i)
          i += 1
      }
    }
    sum

  private def frobeniusInner(left: DMat, right: DMat): Double =
    var sum = 0.0
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        sum += left(row, col) * right(row, col)
        col += 1
      row += 1
    sum

  private def trace(matrix: DMat): Double =
    var sum = 0.0
    var i = 0
    while i < matrix.rows do
      sum += matrix(i, i)
      i += 1
    sum

  private def scale(matrix: DMat, factor: Double): DMat =
    Matrix.tabulate(matrix.rows, matrix.cols)((row, col) => matrix(row, col) * factor)

  private def symmetrize(matrix: DMat): DMat =
    Matrix.tabulate(matrix.rows, matrix.cols)((row, col) => 0.5 * (matrix(row, col) + matrix(col, row)))
