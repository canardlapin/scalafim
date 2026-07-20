package scalafim.fmri.group

import gale.linalg.{CholeskyOptions, DMat, DVec, Matrix, Vec}

/** The second-level fitting kernels. Two paths behind one idea:
  *
  *   - `ols` — unweighted GLM. Weights are shared across samples, so a single
  *     Cholesky of `XᵀX` is amortized over all samples (as in `fit.Ols`).
  *   - `wls` — weighted GLM. Each sample has its own weights (inverse variances),
  *     so each solves its own `XᵀWX` system, and Cochran's `Q` and the
  *     DerSimonian–Laird scaling constant `C` are accumulated for heterogeneity.
  *
  * All arithmetic is primitive `while` loops over Gale builders and matrices, so the
  * kernel cross-compiles and allocates deliberately.
  */
object GroupGlm:

  final case class OlsPieces(
      coefficients: DMat,
      standardErrors: DMat,
      inverse: DMat,
      residualVariance: DVec,
      residualDf: Int
  )

  /** Weighted-fit outputs. `covariance` packs each sample's `(XᵀWX)⁻¹` lower
    * triangle into one row of a single `[samples × terms(terms+1)/2]` matrix, so
    * there is one backing array rather than one matrix object per sample.
    */
  final case class WlsPieces(
      coefficients: DMat,
      standardErrors: DMat,
      covariance: DMat,
      terms: Int,
      q: DVec,
      c: DVec
  )

  /** Unweighted ordinary least squares, `β = (XᵀX)⁻¹ XᵀY`, with residual-variance
    * scaled standard errors and `df = n − p`.
    */
  def ols(design: DMat, effects: DMat): Either[GroupError, OlsPieces] =
    val n = design.rows
    val p = design.cols
    val samples = effects.cols
    if n == 0 || p == 0 then Left(GroupError.EmptyDesign)
    else if samples == 0 then Left(GroupError.EmptyResponse)
    else if n <= p then Left(GroupError.InsufficientSubjects(n, p))
    else
      val xtx = design.t * design
      for
        chol <- xtx
          .cholesky(CholeskyOptions(relativeTolerance(diagonalMax(xtx))))
          .left
          .map(GroupError.SingularDesign.apply)
        coefficients <- chol.solve(design.t * effects).left.map(GroupError.SingularDesign.apply)
        inverse <- chol.solve(Matrix.eye(p)).left.map(GroupError.SingularDesign.apply)
      yield
        val df = n - p
        val residualVariance = Vec.newBuilder(samples)
        val standardErrors = Matrix.newBuilder(p, samples)

        var s = 0
        while s < samples do
          var sse = 0.0
          var i = 0
          while i < n do
            var fitted = 0.0
            var j = 0
            while j < p do
              fitted += design(i, j) * coefficients(j, s)
              j += 1
            val r = effects(i, s) - fitted
            sse += r * r
            i += 1
          val v = sse / df
          residualVariance(s) = v
          var j = 0
          while j < p do
            standardErrors(j, s) = safeSqrt(inverse(j, j) * v)
            j += 1
          s += 1

        OlsPieces(
          coefficients = coefficients,
          standardErrors = standardErrors.result(),
          inverse = inverse,
          residualVariance = residualVariance.result(),
          residualDf = df
        )

  /** Weighted least squares with a distinct weight per (subject, sample). Solves
    * one `XᵀWX` system per sample; variances are treated as known, so standard
    * errors are `sqrt(diag((XᵀWX)⁻¹))` with no residual scaling. Samples whose
    * system is singular are filled with `NaN`.
    */
  def wls(design: DMat, effects: DMat, weights: DMat): WlsPieces =
    val n = design.rows
    val p = design.cols
    val samples = effects.cols
    val tri = p * (p + 1) / 2
    val coefficients = Matrix.newBuilder(p, samples)
    val standardErrors = Matrix.newBuilder(p, samples)
    val covariance = Matrix.newBuilder(samples, tri)
    val q = Vec.newBuilder(samples)
    val c = Vec.newBuilder(samples)

    var s = 0
    while s < samples do
      val xtwx = Matrix.newBuilder(p, p)
      val xtwy = Matrix.newBuilder(p, 1)
      val xtw2x = new Array[Double](p * p)
      var i = 0
      while i < n do
        val w = weights(i, s)
        val yi = effects(i, s)
        var a = 0
        while a < p do
          val xa = design(i, a)
          xtwy(a, 0) = xtwy(a, 0) + xa * w * yi
          var b = 0
          while b < p do
            val xb = design(i, b)
            xtwx(a, b) = xtwx(a, b) + xa * w * xb
            xtw2x(a * p + b) += xa * w * w * xb
            b += 1
          a += 1
        i += 1

      val tolerance = relativeTolerance(diagonalMax(xtwx))
      val system = xtwx.result()
      val rhs = xtwy.result()
      val solved =
        for
          chol <- system.cholesky(CholeskyOptions(tolerance))
          beta <- chol.solve(rhs)
          inverse <- chol.solve(Matrix.eye(p))
        yield (beta, inverse)
      solved match
        case Left(_) =>
          var j = 0
          while j < p do
            coefficients(j, s) = Double.NaN
            standardErrors(j, s) = Double.NaN
            j += 1
          var k = 0
          while k < tri do
            covariance(s, k) = Double.NaN
            k += 1
          q(s) = Double.NaN
          c(s) = Double.NaN
        case Right((beta, inv)) =>
          var j = 0
          while j < p do
            coefficients(j, s) = beta(j, 0)
            standardErrors(j, s) = safeSqrt(inv(j, j))
            j += 1
          // Pack the lower triangle of (XᵀWX)⁻¹ for this sample.
          var k = 0
          var a = 0
          while a < p do
            var b = 0
            while b <= a do
              covariance(s, k) = inv(a, b)
              k += 1
              b += 1
            a += 1

          var qs = 0.0
          var ii = 0
          while ii < n do
            var fitted = 0.0
            var jj = 0
            while jj < p do
              fitted += design(ii, jj) * beta(jj, 0)
              jj += 1
            val r = effects(ii, s) - fitted
            qs += weights(ii, s) * r * r
            ii += 1
          q(s) = qs

          var trW = 0.0
          var iw = 0
          while iw < n do
            trW += weights(iw, s)
            iw += 1
          var trAB = 0.0
          var a2 = 0
          while a2 < p do
            var b2 = 0
            while b2 < p do
              trAB += inv(a2, b2) * xtw2x(b2 * p + a2)
              b2 += 1
            a2 += 1
          c(s) = trW - trAB
      s += 1

    WlsPieces(
      coefficients = coefficients.result(),
      standardErrors = standardErrors.result(),
      covariance = covariance.result(),
      terms = p,
      q = q.result(),
      c = c.result()
    )

  private def safeSqrt(variance: Double): Double =
    if variance < 0.0 && variance > -1e-12 then 0.0 else math.sqrt(variance)

  /** Positive-definiteness tolerance relative to the Gram matrix scale, so a
    * well-posed system is not falsely rejected merely because its weights (hence
    * its diagonal) are tiny — e.g. inverse-variance meta-analysis with very large
    * first-level variances.
    */
  private def relativeTolerance(diagonalMax: Double): Double = 1e-12 * diagonalMax

  private def diagonalMax(m: DMat): Double =
    var max = 0.0
    var i = 0
    while i < m.rows do
      val v = math.abs(m(i, i))
      if v > max then max = v
      i += 1
    max

  private def diagonalMax(m: gale.linalg.DMatBuilder): Double =
    var max = 0.0
    var d = 0
    while d < m.rows do
      val v = math.abs(m(d, d))
      if v > max then max = v
      d += 1
    max
