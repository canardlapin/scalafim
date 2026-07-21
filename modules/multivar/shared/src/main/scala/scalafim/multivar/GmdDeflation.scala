package scalafim.multivar

import scala.collection.mutable.ArrayBuffer

import gale.linalg.DMat
import gale.linalg.DVec

/** Matrix-free GMD backend: sequential rank-one extraction by generalized power
  * iteration with implicit deflation. Needs only products by X, X', M, and A, so
  * sparse data and sparse metrics never densify and no metric square root is formed.
  *
  * Initialization uses a seeded xorshift64* stream (uniform, no transcendentals) so
  * results are reproducible and identical across JVM and JS; the R reference uses
  * `rnorm`, which agrees within tolerance because the converged factors are
  * init-independent.
  *
  * `threshold` is purely the power-iteration convergence tolerance: the residual is
  * `(1 - |<u_new, M u_old>|) + (1 - |<v_new, A v_old>|)` between metric-normalized
  * iterates, which lies in [0, 2] whatever the metric scale. `rankTolerance` is the
  * separate relative rank/degeneracy cutoff (mirroring EigenGmd): iterate norms and
  * extracted singular values are compared against the largest ones seen so far, so
  * components below `rankTolerance` of the problem's own scale truncate the
  * extraction, mirroring the R deflation path.
  */
private[multivar] final case class DeflationGmd(
    threshold: Double = 1e-6,
    maxIterations: Int = 500,
    seed: Long = GmdBackend.DefaultSeed,
    rankTolerance: Double = 1e-12
) extends GmdEngine:

  override def decompose(
      x: MatrixView,
      rowMetric: MetricSpec,
      colMetric: MetricSpec,
      components: Int,
      eigenSolver: SymmetricEigenSolver,
      policy: StoragePolicy
  ): Either[MultivarError, GmdDecomposition] =
    for
      totalVariance <- DualityKernels.totalVariance(x, rowMetric, colMetric, policy)
      _ <- probe(x)
      result <- extract(x, rowMetric, colMetric, components)
    yield GmdDecomposition(result, totalVariance)

  /** One forward and one adjoint product up front, so the shape and finiteness
    * failure modes surface as typed errors before the unchecked iteration loops.
    */
  private def probe(x: MatrixView): Either[MultivarError, Unit] =
    for
      _ <- x.rightMultiply(GaleNumerics.matrixFromRowMajor(x.cols, 1, new Array[Double](x.cols)))
      _ <- x.transposeMultiply(DenseMatrixView(GaleNumerics.matrixFromRowMajor(x.rows, 1, new Array[Double](x.rows))))
    yield ()

  private def extract(
      x: MatrixView,
      rowMetric: MetricSpec,
      colMetric: MetricSpec,
      components: Int
  ): Either[MultivarError, GmdResult] =
    val n = x.rows
    val p = x.cols
    val applyRow = DeflationGmd.vectorApplier(rowMetric)
    val applyCol = DeflationGmd.vectorApplier(colMetric)

    val uCols = ArrayBuffer.empty[Array[Double]]
    val vCols = ArrayBuffer.empty[Array[Double]]
    val dVals = ArrayBuffer.empty[Double]

    // X_res w = X w - sum_j d_j u_j (v_j . w); the residual is never materialized.
    def forward(w: Array[Double]): Array[Double] =
      val out = DeflationGmd.expect(x.rightMultiply(GaleNumerics.matrixFromRowMajor(p, 1, w))).copyData
      var j = 0
      while j < dVals.length do
        DeflationGmd.axpy(-dVals(j) * DeflationGmd.dot(vCols(j), w), uCols(j), out)
        j += 1
      out

    def adjoint(z: Array[Double]): Array[Double] =
      val out = DeflationGmd
        .expect(x.transposeMultiply(DenseMatrixView(GaleNumerics.matrixFromRowMajor(n, 1, z))))
        .copyData
      var j = 0
      while j < dVals.length do
        DeflationGmd.axpy(-dVals(j) * DeflationGmd.dot(uCols(j), z), vCols(j), out)
        j += 1
      out

    var stop = false
    var lastResidual = 0.0
    var sawNonConvergence = false
    // Largest metric norms and singular value seen so far; they set the problem's own
    // scale so degeneracy and rank cutoffs are relative, never absolute.
    var uNormScale = 0.0
    var vNormScale = 0.0
    var dScale = 0.0
    while dVals.length < components && !stop do
      val rng = DeflationGmd.XorShift(seed ^ (0x9E3779B97F4A7C15L * (dVals.length + 1)))
      var u = DeflationGmd.metricNormalized(rng.fill(n), applyRow)
      var v = DeflationGmd.metricNormalized(rng.fill(p), applyCol)
      var iter = 0
      var converged = false
      var degenerate = false
      while iter < maxIterations && !converged && !degenerate do
        val uhat = forward(applyCol(v))
        val mu = applyRow(uhat)
        val uNorm = Math.sqrt(Math.max(DeflationGmd.dot(mu, uhat), 0.0))
        if !uNorm.isFinite || uNorm <= rankTolerance * uNormScale then degenerate = true
        else
          uNormScale = Math.max(uNormScale, uNorm)
          DeflationGmd.scaleInPlace(uhat, 1.0 / uNorm)
          // The identity-metric applier returns its argument, so guard against
          // scaling an aliased array twice.
          if !(mu eq uhat) then DeflationGmd.scaleInPlace(mu, 1.0 / uNorm)
          val uAlign = Math.abs(DeflationGmd.dot(mu, u))
          val vhat = adjoint(mu)
          val av = applyCol(vhat)
          val vNorm = Math.sqrt(Math.max(DeflationGmd.dot(av, vhat), 0.0))
          if !vNorm.isFinite || vNorm <= rankTolerance * vNormScale then degenerate = true
          else
            vNormScale = Math.max(vNormScale, vNorm)
            DeflationGmd.scaleInPlace(vhat, 1.0 / vNorm)
            if !(av eq vhat) then DeflationGmd.scaleInPlace(av, 1.0 / vNorm)
            val vAlign = Math.abs(DeflationGmd.dot(av, v))
            // 1 - |cos| per side in the fit metrics; in [0, 2] whatever the metric scale.
            val residual =
              (1.0 - Math.min(1.0, uAlign)) + (1.0 - Math.min(1.0, vAlign))
            u = uhat
            v = vhat
            lastResidual = residual
            if residual < threshold then converged = true
        iter += 1

      if degenerate then stop = true
      else if !converged then
        sawNonConvergence = true
        stop = true
      else
        var dValue = DeflationGmd.dot(applyRow(u), forward(applyCol(v)))
        if !dValue.isFinite || Math.abs(dValue) <= rankTolerance * dScale then stop = true
        else
          if dValue < 0.0 then
            DeflationGmd.scaleInPlace(u, -1.0)
            dValue = -dValue
          dScale = Math.max(dScale, dValue)
          uCols += u
          vCols += v
          dVals += dValue

    if dVals.isEmpty then
      if sawNonConvergence then
        Left(MultivarError.IterationLimitExceeded("gmd deflation", maxIterations, lastResidual))
      else Left(MultivarError.SolverFailed("no generalized components above tolerance"))
    else Right(DeflationGmd.assemble(n, p, uCols, vCols, dVals))

private[multivar] object DeflationGmd:

  /** Metric application specialized once per fit so the iteration loop is Either-free. */
  private def vectorApplier(metric: MetricSpec): Array[Double] => Array[Double] =
    metric match
      case MetricSpec.Identity(_, _) =>
        input => input
      case MetricSpec.Diagonal(weights, _) =>
        input =>
          val out = new Array[Double](input.length)
          var i = 0
          while i < input.length do
            out(i) = weights(i) * input(i)
            i += 1
          out
      case MetricSpec.DenseSymmetric(matrix, _) =>
        input =>
          val out = new Array[Double](input.length)
          var row = 0
          while row < input.length do
            var acc = 0.0
            var col = 0
            while col < input.length do
              acc += matrix(row, col) * input(col)
              col += 1
            out(row) = acc
            row += 1
          out
      case MetricSpec.SparseSymmetric(view, _) =>
        input =>
          val out = new Array[Double](input.length)
          view.foreachEntry((row, col, value) => out(row) += value * input(col))
          out

  /** Unwraps results whose failure modes were ruled out by the initial probe. */
  private def expect[T](result: Either[MultivarError, T]): T =
    result match
      case Right(value) => value
      case Left(error)  => throw IllegalStateException(s"deflation invariant violated: ${error.message}")

  private def assemble(
      n: Int,
      p: Int,
      uCols: ArrayBuffer[Array[Double]],
      vCols: ArrayBuffer[Array[Double]],
      dVals: ArrayBuffer[Double]
  ): GmdResult =
    val k = dVals.length
    val order = Array.tabulate(k)(identity)
    scala.util.Sorting.stableSort(order, (left: Int, right: Int) => dVals(left) > dVals(right))

    val ou = new Array[Double](n * k)
    val ov = new Array[Double](p * k)
    val d = new Array[Double](k)
    var col = 0
    while col < k do
      val source = order(col)
      d(col) = dVals(source)
      val uCol = uCols(source)
      var row = 0
      while row < n do
        ou(row * k + col) = uCol(row)
        row += 1
      val vCol = vCols(source)
      row = 0
      while row < p do
        ov(row * k + col) = vCol(row)
        row += 1
      col += 1
    GmdResult(GaleNumerics.matrixFromRowMajor(n, k, ou), GaleNumerics.vectorFromArray(d), GaleNumerics.matrixFromRowMajor(p, k, ov))

  private def dot(left: Array[Double], right: Array[Double]): Double =
    var acc = 0.0
    var i = 0
    while i < left.length do
      acc += left(i) * right(i)
      i += 1
    acc

  private def axpy(alpha: Double, source: Array[Double], target: Array[Double]): Unit =
    var i = 0
    while i < target.length do
      target(i) += alpha * source(i)
      i += 1

  private def scaleInPlace(values: Array[Double], factor: Double): Unit =
    var i = 0
    while i < values.length do
      values(i) *= factor
      i += 1

  /** Normalize to unit length in the metric induced by `applyMetric`, so alignment
    * inner products against later metric-normalized iterates stay within [-1, 1].
    */
  private def metricNormalized(
      values: Array[Double],
      applyMetric: Array[Double] => Array[Double]
  ): Array[Double] =
    val norm = Math.sqrt(Math.max(dot(applyMetric(values), values), 0.0))
    if norm > 0.0 then scaleInPlace(values, 1.0 / norm)
    values

  /** xorshift64* over uniform(-0.5, 0.5); deterministic and platform-identical. */
  private final class XorShift(initial: Long):
    private var state: Long =
      if initial == 0L then 0x6A09E667F3BCC909L else initial

    def nextDouble(): Double =
      state ^= state >>> 12
      state ^= state << 25
      state ^= state >>> 27
      val mixed = state * 0x2545F4914F6CDD1DL
      ((mixed >>> 11).toDouble / (1L << 53).toDouble) - 0.5

    def fill(length: Int): Array[Double] =
      val out = new Array[Double](length)
      var i = 0
      while i < length do
        out(i) = nextDouble()
        i += 1
      out
