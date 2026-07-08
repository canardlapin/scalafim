package scalafim.multivar

import scala.collection.mutable.ArrayBuffer

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

/** Matrix-free GMD backend: sequential rank-one extraction by generalized power
  * iteration with implicit deflation. Needs only products by X, X', M, and A, so
  * sparse data and sparse metrics never densify and no metric square root is formed.
  *
  * Initialization uses a seeded xorshift64* stream (uniform, no transcendentals) so
  * results are reproducible and identical across JVM and JS; the R reference uses
  * `rnorm`, which agrees within tolerance because the converged factors are
  * init-independent. Components that stop converging or fall below `threshold`
  * truncate the extraction, mirroring the R deflation path.
  */
private[multivar] final case class DeflationGmd(
    threshold: Double = 1e-6,
    maxIterations: Int = 500,
    seed: Long = GmdBackend.DefaultSeed
) extends GmdEngine:

  override def decompose(
      x: MatrixView,
      rowMetric: MvMetric,
      colMetric: MvMetric,
      components: Int,
      eigenSolver: SymmetricEigenSolver,
      policy: StoragePolicy
  ): Either[MultivarError, GmdDecomposition] =
    for
      totalVariance <- GenPcaKernels.totalVariance(x, rowMetric, colMetric, policy)
      _ <- probe(x)
      result <- extract(x, rowMetric, colMetric, components)
    yield GmdDecomposition(result, totalVariance)

  /** One forward and one adjoint product up front, so the shape and finiteness
    * failure modes surface as typed errors before the unchecked iteration loops.
    */
  private def probe(x: MatrixView): Either[MultivarError, Unit] =
    for
      _ <- x.rightMultiply(DoubleMatrix.unsafe(x.cols, 1, new Array[Double](x.cols)))
      _ <- x.transposeMultiply(DenseMatrixView(DoubleMatrix.unsafe(x.rows, 1, new Array[Double](x.rows))))
    yield ()

  private def extract(
      x: MatrixView,
      rowMetric: MvMetric,
      colMetric: MvMetric,
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
      val out = DeflationGmd.expect(x.rightMultiply(DoubleMatrix.unsafe(p, 1, w))).dataArray
      var j = 0
      while j < dVals.length do
        DeflationGmd.axpy(-dVals(j) * DeflationGmd.dot(vCols(j), w), uCols(j), out)
        j += 1
      out

    def adjoint(z: Array[Double]): Array[Double] =
      val out = DeflationGmd
        .expect(x.transposeMultiply(DenseMatrixView(DoubleMatrix.unsafe(n, 1, z))))
        .dataArray
      var j = 0
      while j < dVals.length do
        DeflationGmd.axpy(-dVals(j) * DeflationGmd.dot(uCols(j), z), vCols(j), out)
        j += 1
      out

    var stop = false
    var lastResidual = 0.0
    var sawNonConvergence = false
    while dVals.length < components && !stop do
      val rng = DeflationGmd.XorShift(seed ^ (0x9E3779B97F4A7C15L * (dVals.length + 1)))
      var u = DeflationGmd.normalized(rng.fill(n))
      var v = DeflationGmd.normalized(rng.fill(p))
      var iter = 0
      var converged = false
      var degenerate = false
      while iter < maxIterations && !converged && !degenerate do
        val uhat = forward(applyCol(v))
        val uNorm = Math.sqrt(Math.max(DeflationGmd.dot(applyRow(uhat), uhat), 0.0))
        if !uNorm.isFinite || uNorm <= threshold then degenerate = true
        else
          DeflationGmd.scaleInPlace(uhat, 1.0 / uNorm)
          val vhat = adjoint(applyRow(uhat))
          val vNorm = Math.sqrt(Math.max(DeflationGmd.dot(applyCol(vhat), vhat), 0.0))
          if !vNorm.isFinite || vNorm <= threshold then degenerate = true
          else
            DeflationGmd.scaleInPlace(vhat, 1.0 / vNorm)
            val residual = DeflationGmd.distanceSq(uhat, u) + DeflationGmd.distanceSq(vhat, v)
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
        if !dValue.isFinite || Math.abs(dValue) <= threshold then stop = true
        else
          if dValue < 0.0 then
            DeflationGmd.scaleInPlace(u, -1.0)
            dValue = -dValue
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
  private def vectorApplier(metric: MvMetric): Array[Double] => Array[Double] =
    metric match
      case MvMetric.Identity(_, _) =>
        input => input
      case MvMetric.Diagonal(weights, _) =>
        input =>
          val out = new Array[Double](input.length)
          var i = 0
          while i < input.length do
            out(i) = weights(i) * input(i)
            i += 1
          out
      case MvMetric.DenseSymmetric(matrix, _) =>
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
      case MvMetric.SparseSymmetric(view, _) =>
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
    GmdResult(DoubleMatrix.unsafe(n, k, ou), DoubleVector.unsafe(d), DoubleMatrix.unsafe(p, k, ov))

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

  private def distanceSq(left: Array[Double], right: Array[Double]): Double =
    var acc = 0.0
    var i = 0
    while i < left.length do
      val delta = left(i) - right(i)
      acc += delta * delta
      i += 1
    acc

  private def normalized(values: Array[Double]): Array[Double] =
    val norm = Math.sqrt(dot(values, values))
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
