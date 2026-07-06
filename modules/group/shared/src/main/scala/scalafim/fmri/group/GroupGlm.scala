package scalafim.fmri.group

import scalafim.linalg.{Cholesky, DoubleMatrix, DoubleVector}

/** The second-level fitting kernels. Two paths behind one idea:
  *
  *   - `ols` — unweighted GLM. Weights are shared across samples, so a single
  *     Cholesky of `XᵀX` is amortized over all samples (as in `fit.Ols`).
  *   - `wls` — weighted GLM. Each sample has its own weights (inverse variances),
  *     so each solves its own `XᵀWX` system, and Cochran's `Q` and the
  *     DerSimonian–Laird scaling constant `C` are accumulated for heterogeneity.
  *
  * All arithmetic is primitive-array `while` loops over `scalafim.linalg`, so the
  * kernel cross-compiles and allocates deliberately.
  */
object GroupGlm:

  final case class OlsPieces(
      coefficients: DoubleMatrix,
      standardErrors: DoubleMatrix,
      inverse: DoubleMatrix,
      residualVariance: DoubleVector,
      residualDf: Int
  )

  /** Weighted-fit outputs. `covariance` packs each sample's `(XᵀWX)⁻¹` lower
    * triangle into one row of a single `[samples × terms(terms+1)/2]` matrix, so
    * there is one backing array rather than one matrix object per sample.
    */
  final case class WlsPieces(
      coefficients: DoubleMatrix,
      standardErrors: DoubleMatrix,
      covariance: DoubleMatrix,
      terms: Int,
      q: DoubleVector,
      c: DoubleVector
  )

  /** Unweighted ordinary least squares, `β = (XᵀX)⁻¹ XᵀY`, with residual-variance
    * scaled standard errors and `df = n − p`.
    */
  def ols(design: DoubleMatrix, effects: DoubleMatrix): Either[GroupError, OlsPieces] =
    val n = design.rows
    val p = design.cols
    val samples = effects.cols
    if n == 0 || p == 0 then Left(GroupError.EmptyDesign)
    else if samples == 0 then Left(GroupError.EmptyResponse)
    else if n <= p then Left(GroupError.InsufficientSubjects(n, p))
    else
      val xtx = DoubleMatrix.transposeMultiply(design, design)
      Cholesky.decompose(xtx, relativeTolerance(diagonalMax(xtx))).left.map(GroupError.SingularDesign.apply).map { chol =>
        val coefficients = chol.solve(DoubleMatrix.transposeMultiply(design, effects))
        val inverse = chol.solve(DoubleMatrix.eye(p))
        val df = n - p
        val residualVariance = new Array[Double](samples)
        val standardErrors = new Array[Double](p * samples)

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
            standardErrors(j * samples + s) = safeSqrt(inverse(j, j) * v)
            j += 1
          s += 1

        OlsPieces(
          coefficients = coefficients,
          standardErrors = DoubleMatrix.unsafe(p, samples, standardErrors),
          inverse = inverse,
          residualVariance = DoubleVector.unsafe(residualVariance),
          residualDf = df
        )
      }

  /** Weighted least squares with a distinct weight per (subject, sample). Solves
    * one `XᵀWX` system per sample; variances are treated as known, so standard
    * errors are `sqrt(diag((XᵀWX)⁻¹))` with no residual scaling. Samples whose
    * system is singular are filled with `NaN`.
    */
  def wls(design: DoubleMatrix, effects: DoubleMatrix, weights: DoubleMatrix): WlsPieces =
    val n = design.rows
    val p = design.cols
    val samples = effects.cols
    val tri = p * (p + 1) / 2
    val coefficients = new Array[Double](p * samples)
    val standardErrors = new Array[Double](p * samples)
    val covariance = new Array[Double](samples * tri)
    val q = new Array[Double](samples)
    val c = new Array[Double](samples)

    var s = 0
    while s < samples do
      val xtwx = new Array[Double](p * p)
      val xtwy = new Array[Double](p)
      val xtw2x = new Array[Double](p * p)
      var i = 0
      while i < n do
        val w = weights(i, s)
        val yi = effects(i, s)
        var a = 0
        while a < p do
          val xa = design(i, a)
          xtwy(a) += xa * w * yi
          var b = 0
          while b < p do
            val xb = design(i, b)
            xtwx(a * p + b) += xa * w * xb
            xtw2x(a * p + b) += xa * w * w * xb
            b += 1
          a += 1
        i += 1

      val triOffset = s * tri
      Cholesky.decompose(DoubleMatrix.unsafe(p, p, xtwx), relativeTolerance(diagonalMaxArray(xtwx, p))) match
        case Left(_) =>
          var j = 0
          while j < p do
            coefficients(j * samples + s) = Double.NaN
            standardErrors(j * samples + s) = Double.NaN
            j += 1
          var k = 0
          while k < tri do
            covariance(triOffset + k) = Double.NaN
            k += 1
          q(s) = Double.NaN
          c(s) = Double.NaN
        case Right(chol) =>
          val beta = chol.solve(DoubleMatrix.unsafe(p, 1, xtwy))
          val inv = chol.solve(DoubleMatrix.eye(p))
          var j = 0
          while j < p do
            coefficients(j * samples + s) = beta(j, 0)
            standardErrors(j * samples + s) = safeSqrt(inv(j, j))
            j += 1
          // Pack the lower triangle of (XᵀWX)⁻¹ for this sample.
          var k = 0
          var a = 0
          while a < p do
            var b = 0
            while b <= a do
              covariance(triOffset + k) = inv(a, b)
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
      coefficients = DoubleMatrix.unsafe(p, samples, coefficients),
      standardErrors = DoubleMatrix.unsafe(p, samples, standardErrors),
      covariance = DoubleMatrix.unsafe(samples, tri, covariance),
      terms = p,
      q = DoubleVector.unsafe(q),
      c = DoubleVector.unsafe(c)
    )

  private def safeSqrt(variance: Double): Double =
    if variance < 0.0 && variance > -1e-12 then 0.0 else math.sqrt(variance)

  /** Positive-definiteness tolerance relative to the Gram matrix scale, so a
    * well-posed system is not falsely rejected merely because its weights (hence
    * its diagonal) are tiny — e.g. inverse-variance meta-analysis with very large
    * first-level variances.
    */
  private def relativeTolerance(diagonalMax: Double): Double = 1e-12 * diagonalMax

  private def diagonalMax(m: DoubleMatrix): Double =
    var max = 0.0
    var i = 0
    while i < m.rows do
      val v = math.abs(m(i, i))
      if v > max then max = v
      i += 1
    max

  private def diagonalMaxArray(square: Array[Double], p: Int): Double =
    var max = 0.0
    var d = 0
    while d < p do
      val v = math.abs(square(d * p + d))
      if v > max then max = v
      d += 1
    max
