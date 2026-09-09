package scalafim.fmri.group

import gale.linalg.{CholeskyOptions, DMat, DVec, Matrix, QROptions, QRPivoting, Vec}

/** Group fitting on a shared, orthonormal design basis. Scientific
  * reparameterization lives here; all factorizations and solves belong to Gale.
  * Weighted passes operate one sample at a time without full weight matrices.
  */
object GroupGlm:
  final case class OlsPieces(coefficients: DMat, standardErrors: DMat, inverse: DMat, residualVariance: DVec, residualDf: Int)
  final case class WlsPieces(
      coefficients: DMat, standardErrors: DMat, covariance: DMat, terms: Int,
      q: DVec, c: DVec, failures: Vector[GroupSampleFailure] = Vector.empty
  )
  private[group] final case class MetaPieces(fit: WlsPieces, tau2: DVec, fixedQ: DVec)
  private[group] final case class Prepared(basis: DMat, toOriginal: DMat)

  /** Center on an existing constant column, normalize units, then use pivoted
    * QR to identify rank. The returned transformation preserves the original
    * coefficient and contrast meanings, including the intercept convention.
    */
  private[group] def prepare(x: DMat): Either[GroupError, Prepared] =
    val n = x.rows
    val p = x.cols
    if n == 0 || p == 0 then Left(GroupError.EmptyDesign)
    else if n <= p then Left(GroupError.InsufficientSubjects(n, p))
    else if !finite(x) then Left(GroupError.NonFiniteData("group design"))
    else
      val constant = (0 until p).find(j => x(0, j) != 0.0 && (1 until n).forall(i => x(i, j) == x(0, j)))
      val scales = new Array[Double](p)
      val centers = new Array[Double](p)
      val norms = new Array[Double](p)
      val standardized = Matrix.newBuilder(n, p)
      var j = 0
      while j < p do
        var i = 0
        var scale = 0.0
        while i < n do
          scale = math.max(scale, math.abs(x(i, j)))
          i += 1
        scales(j) = if scale == 0.0 then 1.0 else scale
        var center = 0.0
        if constant.exists(_ != j) then
          i = 0
          while i < n do
            center += (x(i, j) / scales(j)) / n
            i += 1
        centers(j) = center
        var ss = 0.0
        i = 0
        while i < n do
          val v = x(i, j) / scales(j) - center
          ss += v * v
          i += 1
        norms(j) = if ss == 0.0 then 1.0 else math.sqrt(ss)
        i = 0
        while i < n do
          standardized(i, j) = (x(i, j) / scales(j) - center) / norms(j)
          i += 1
        j += 1
      val qr = standardized.result().qr(QROptions(pivoting = QRPivoting.Column))
      for
        _ <- qr.normalizedCovariance.left.map(GroupError.SingularDesign.apply)
        q <- qr.applyQ(Matrix.tabulate(n, p)((i, j) => if i == j then 1.0 else 0.0)).left.map(GroupError.SingularDesign.apply)
        transform <- qr.solveLeastSquares(q).left.map(GroupError.SingularDesign.apply)
        original <-
          val out = Matrix.newBuilder(p, p)
          j = 0
          while j < p do
            var k = 0
            while k < p do
              var v = (transform(j, k) / norms(j)) / scales(j)
              if constant.contains(j) then
                var a = 0
                while a < p do
                  if a != j then v -= (centers(a) * transform(a, k) / norms(a)) / x(0, j)
                  a += 1
              out(j, k) = v
              k += 1
            j += 1
          val result = out.result()
          if finite(result) then Right(result) else Left(GroupError.NumericalFailure("coefficient transformation exceeds finite precision"))
      yield Prepared(q, original)

  def ols(design: DMat, effects: DMat): Either[GroupError, OlsPieces] =
    for
      _ <- validateResponse(design, effects)
      prepared <- prepare(design)
      pieces <- olsPrepared(prepared, effects)
    yield pieces

  private[group] def olsPrepared(prepared: Prepared, effects: DMat): Either[GroupError, OlsPieces] =
    val q = prepared.basis
    val t = prepared.toOriginal
    val fittedBasis = q.t * effects
    val coefficients = t * fittedBasis
    val inverse = t * t.t
    val df = q.rows - q.cols
    val variance = Vec.newBuilder(effects.cols)
    val se = Matrix.newBuilder(q.cols, effects.cols)
    var s = 0
    while s < effects.cols do
      var sse = 0.0
      var i = 0
      while i < q.rows do
        var fitted = 0.0
        var j = 0
        while j < q.cols do
          fitted += q(i, j) * fittedBasis(j, s)
          j += 1
        val r = effects(i, s) - fitted
        sse += r * r
        i += 1
      variance(s) = sse / df
      var j = 0
      while j < q.cols do
        se(j, s) = math.sqrt(inverse(j, j) * variance(s))
        j += 1
      s += 1
    val standardErrors = se.result()
    if !finite(coefficients) || !finite(inverse) || !finite(standardErrors) then
      Left(GroupError.NumericalFailure("OLS output exceeds finite precision"))
    else Right(OlsPieces(coefficients, standardErrors, inverse, variance.result(), df))

  /** Public WLS now returns typed global failures. Sample failures are attached
    * to the result; an entirely unavailable map is a Left(AllSamplesFailed).
    */
  def wls(design: DMat, effects: DMat, weights: DMat): Either[GroupError, WlsPieces] =
    for
      _ <- validateResponse(design, effects)
      _ <- validateWeights(effects, weights)
      prepared <- prepare(design)
      pieces <- weighted(prepared, effects, weights, directWeights = true, random = None)
    yield pieces.fit

  private[group] def meta(
      prepared: Prepared, effects: DMat, variances: DMat,
      random: Option[(TauEstimator, MetaInference)]
  ): Either[GroupError, MetaPieces] =
    weighted(prepared, effects, variances, directWeights = false, random)

  private final case class SampleSolution(beta: DMat, inverse: DMat, varianceScale: Double, q: Double, c: Double, derivative: Double)

  private def weighted(
      prepared: Prepared, effects: DMat, input: DMat,
      directWeights: Boolean, random: Option[(TauEstimator, MetaInference)]
  ): Either[GroupError, MetaPieces] =
    val x = prepared.basis
    val n = x.rows
    val p = x.cols
    val samples = effects.cols
    val df = n - p
    val tri = p * (p + 1) / 2
    val coefficients = Matrix.newBuilder(p, samples)
    val ses = Matrix.newBuilder(p, samples)
    val covariances = Matrix.newBuilder(samples, tri)
    val qs = Vec.newBuilder(samples)
    val cs = Vec.newBuilder(samples)
    val taus = Vec.newBuilder(samples)
    val fixedQs = Vec.newBuilder(samples)
    val failures = Vector.newBuilder[GroupSampleFailure]
    val identityMatrix = Matrix.eye(p)
    val weights = new Array[Double](n)
    val basis = Array.tabulate(n * p)(k => x(k / p, k % p))
    val gramScratch = new Array[Double](p * p)
    val squaredWeightGram = new Array[Double](p * p)
    val rhsScratch = new Array[Double](p)
    val covarianceScratch = new Array[Double](p * p)
    val intermediate = new Array[Double](p * p)
    val transformedBeta = new Array[Double](p)

    def solve(s: Int, tau: Double): Either[GroupError, SampleSolution] =
      // Normalize weights before accumulating. The scale is restored to the
      // covariance and Q, avoiding reciprocal overflow and arbitrary unit gates.
      var i = 0
      var weightScale = if directWeights then 0.0 else Double.PositiveInfinity
      while i < n do
        val v = if directWeights then input(i, s) else input(i, s) + tau
        weightScale = if directWeights then math.max(weightScale, v) else math.min(weightScale, v)
        i += 1
      if !weightScale.isFinite || weightScale <= 0.0 then
        Left(GroupError.NumericalFailure("non-finite or non-positive weight scale"))
      else
        val varianceScale = if directWeights then 1.0 / weightScale else weightScale
        var k = 0
        while k < p * p do
          gramScratch(k) = 0.0
          squaredWeightGram(k) = 0.0
          k += 1
        k = 0
        while k < p do
          rhsScratch(k) = 0.0
          k += 1
        var sumWeights = 0.0
        i = 0
        while i < n do
          val w = if directWeights then input(i, s) / weightScale else weightScale / (input(i, s) + tau)
          weights(i) = w
          sumWeights += w
          val yi = effects(i, s)
          val offset = i * p
          var a = 0
          while a < p do
            val weightedXa = basis(offset + a) * w
            rhsScratch(a) += weightedXa * yi
            var b = 0
            while b <= a do
              val value = weightedXa * basis(offset + b)
              gramScratch(a * p + b) += value
              squaredWeightGram(a * p + b) += value * w
              b += 1
            a += 1
          i += 1
        var a = 0
        while a < p do
          var b = 0
          while b < a do
            gramScratch(b * p + a) = gramScratch(a * p + b)
            squaredWeightGram(b * p + a) = squaredWeightGram(a * p + b)
            b += 1
          a += 1
        val system = Matrix.tabulate(p, p)((i, j) => gramScratch(i * p + j))
        val right = Matrix.tabulate(p, 1)((i, _) => rhsScratch(i))
        var scale = 0.0
        a = 0
        while a < p do
          scale = math.max(scale, gramScratch(a * p + a))
          a += 1
        val normal = for
          chol <- system.cholesky(CholeskyOptions(1e-10 * scale))
          beta <- chol.solve(right)
          inverse <- chol.solve(identityMatrix)
        yield (beta, inverse)
        // Severe weight imbalance can undo the shared preconditioning. Use QR
        // of sqrt(W)Q rather than loosening the normal-equation tolerance.
        val solved = normal.orElse {
          val wx = Matrix.tabulate(n, p)((i, j) => math.sqrt(weights(i)) * x(i, j))
          val wy = Matrix.tabulate(n, 1)((i, _) => math.sqrt(weights(i)) * effects(i, s))
          val qr = wx.qr(QROptions(pivoting = QRPivoting.Column))
          for
            beta <- qr.solveLeastSquares(wy)
            inverse <- qr.normalizedCovariance
          yield (beta, inverse)
        }
        solved.left.map(GroupError.SingularDesign.apply).flatMap { (beta, inv) =>
          var q = 0.0
          var c = sumWeights
          a = 0
          while a < p do
            var b = 0
            while b < p do
              c -= inv(a, b) * squaredWeightGram(b * p + a)
              b += 1
            a += 1
          var derivative = 0.0
          i = 0
          while i < n do
            var fitted = 0.0
            a = 0
            while a < p do
              fitted += basis(i * p + a) * beta(a, 0)
              a += 1
            val r = effects(i, s) - fitted
            val w = weights(i)
            q += w * r * r
            derivative += w * w * r * r
            i += 1
          q /= varianceScale
          c /= varianceScale
          derivative = (derivative / varianceScale) / varianceScale
          if !finite(beta) || !finite(inv) || !q.isFinite || !c.isFinite then
            Left(GroupError.NumericalFailure("weighted fit exceeds finite precision"))
          else
            Right(SampleSolution(beta, inv, varianceScale, q, c, derivative))
        }

    def estimate(s: Int, fixed: SampleSolution, estimator: TauEstimator): Either[GroupError, (Double, SampleSolution)] =
      estimator match
        case TauEstimator.DerSimonianLaird =>
          if fixed.q <= df then Right(0.0 -> fixed)
          else if fixed.c <= 0.0 then Left(GroupError.NumericalFailure("non-positive DL heterogeneity denominator"))
          else
            val tau = (fixed.q - df) / fixed.c
            solve(s, tau).map(tau -> _)
        case TauEstimator.PauleMandel =>
          if fixed.q <= df then Right(0.0 -> fixed)
          else
            var tau = 0.0
            var current: Either[GroupError, SampleSolution] = Right(fixed)
            var iterations = 0
            var converged = false
            while iterations < 64 && current.isRight && !converged do
              val fit = current.toOption.get
              val error = fit.q - df
              if math.abs(error) <= 1e-9 * df then converged = true
              else if !fit.derivative.isFinite || fit.derivative <= 0.0 then
                current = Left(GroupError.NumericalFailure("Paule-Mandel derivative is unavailable"))
              else
                val next = math.max(0.0, tau + error / fit.derivative)
                if !next.isFinite || next == tau then
                  current = Left(GroupError.NumericalFailure("Paule-Mandel iteration cannot advance"))
                else
                  tau = next
                  current = solve(s, tau)
              iterations += 1
            current.flatMap { fit =>
              if converged || math.abs(fit.q - df) <= 1e-9 * df then Right(tau -> fit)
              else Left(GroupError.NumericalFailure("Paule-Mandel did not converge in 64 iterations"))
            }

    var s = 0
    while s < samples do
      val fitted = for
        fixed <- solve(s, 0.0)
        estimated <- random match
          case None => Right(0.0 -> fixed)
          case Some((tau, _)) => estimate(s, fixed, tau)
        (tau, fit) = estimated
        scale = random match
          case Some((_, MetaInference.ModifiedKnappHartung)) => math.max(1.0, fit.q / df)
          case _ => 1.0
        _ <-
          val t = prepared.toOriginal
          var a = 0
          var valid = true
          while a < p do
            var beta = 0.0
            var k = 0
            while k < p do
              beta += t(a, k) * fit.beta(k, 0)
              k += 1
            transformedBeta(a) = beta
            valid &&= beta.isFinite
            var b = 0
            while b < p do
              var v = 0.0
              k = 0
              while k < p do
                v += t(a, k) * (fit.inverse(k, b) * fit.varianceScale)
                k += 1
              intermediate(a * p + b) = v
              b += 1
            a += 1
          a = 0
          while a < p do
            var b = 0
            while b <= a do
              var v = 0.0
              var k = 0
              while k < p do
                v += intermediate(a * p + k) * t(b, k)
                k += 1
              v *= scale
              covarianceScratch(a * p + b) = v
              valid &&= v.isFinite && (a != b || v > 0.0)
              b += 1
            a += 1
          if valid then Right(()) else Left(GroupError.NumericalFailure("coefficient or covariance transformation exceeds finite precision"))
      yield (fixed, tau, fit)
      fitted match
        case Left(error) =>
          failures += GroupSampleFailure(s, error)
          var a = 0
          while a < p do
            coefficients(a, s) = Double.NaN
            ses(a, s) = Double.NaN
            a += 1
          var k = 0
          while k < tri do
            covariances(s, k) = Double.NaN
            k += 1
          qs(s) = Double.NaN
          cs(s) = Double.NaN
          taus(s) = Double.NaN
          fixedQs(s) = Double.NaN
        case Right((fixed, tau, fit)) =>
          var a = 0
          var k = 0
          while a < p do
            coefficients(a, s) = transformedBeta(a)
            ses(a, s) = math.sqrt(covarianceScratch(a * p + a))
            var b = 0
            while b <= a do
              covariances(s, k) = covarianceScratch(a * p + b)
              k += 1
              b += 1
            a += 1
          qs(s) = fit.q
          cs(s) = fit.c
          taus(s) = tau
          fixedQs(s) = fixed.q
      s += 1
    val unavailable = failures.result()
    if unavailable.length == samples then Left(GroupError.AllSamplesFailed(unavailable))
    else Right(MetaPieces(WlsPieces(coefficients.result(), ses.result(), covariances.result(), p, qs.result(), cs.result(), unavailable), taus.result(), fixedQs.result()))

  private def validateResponse(design: DMat, effects: DMat): Either[GroupError, Unit] =
    if effects.rows != design.rows then Left(GroupError.subjectMismatch(effects.rows, design.rows))
    else if effects.cols == 0 then Left(GroupError.EmptyResponse)
    else if !finite(effects) then Left(GroupError.NonFiniteData("group effects"))
    else Right(())

  private def validateWeights(effects: DMat, weights: DMat): Either[GroupError, Unit] =
    if weights.rows != effects.rows then Left(GroupError.responseSubjectMismatch(effects.rows, weights.rows))
    else if weights.cols != effects.cols then Left(GroupError.sampleMismatch(effects.cols, weights.cols))
    else if !finite(weights) then Left(GroupError.NonFiniteData("group weights"))
    else if (0 until weights.rows).exists(i => (0 until weights.cols).exists(s => weights(i, s) <= 0.0)) then Left(GroupError.NonPositiveVariance)
    else Right(())

  private def finite(m: DMat): Boolean =
    var i = 0
    var ok = true
    while i < m.rows && ok do
      var j = 0
      while j < m.cols && ok do
        ok = m(i, j).isFinite
        j += 1
      i += 1
    ok
