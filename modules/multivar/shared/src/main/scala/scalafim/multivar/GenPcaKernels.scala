package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

/** Storage-aware Gram and trace kernels for metric-weighted decompositions.
  *
  * The supported storage combinations mirror the GenPca support matrix: dense and
  * lazy-affine data work with every metric kind; sparse-data Grams work with
  * diagonal-like metrics without densification and with dense/sparse metrics only
  * under `StoragePolicy.AllowDense`; the total-variance trace is always computable
  * matrix-free.
  */
private[multivar] object GenPcaKernels:

  /** X' M X (p x p) for a row metric M (n x n). */
  def rowGram(
      x: MatrixView,
      metric: MvMetric,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, DoubleMatrix] =
    if metric.dim != x.rows then
      Left(MultivarError.MetricShapeMismatch(IndexAxis.Row, x.rows, metric.dim))
    else
      x match
        case dense: DenseMatrixView =>
          denseRowGram(dense.value, metric)
        case sparse: SparseMatrixView =>
          metric match
            case MvMetric.Identity(_, _) =>
              sparse.crossProduct
            case MvMetric.Diagonal(weights, _) =>
              sparse.scaleRows(weights).flatMap(sparse.transposeMultiply)
            case _ if policy == StoragePolicy.AllowDense =>
              sparse.toDense(StoragePolicy.AllowDense).flatMap(denseRowGram(_, metric))
            case _ =>
              Left(MultivarError.DensificationRejected("row Gram with a non-diagonal metric", StorageKind.Sparse))
        case affine: AffineMatrixView =>
          affineRowGram(affine, metric, policy)
        case other =>
          other.toDense(policy).flatMap(denseRowGram(_, metric))

  /** X A X' (n x n) for a column metric A (p x p); dense data only. */
  def colGram(
      x: MatrixView,
      metric: MvMetric,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, DoubleMatrix] =
    if metric.dim != x.cols then
      Left(MultivarError.MetricShapeMismatch(IndexAxis.Column, x.cols, metric.dim))
    else
      x match
        case dense: DenseMatrixView =>
          denseColGram(dense.value, metric)
        case other =>
          other.toDense(policy) match
            case Right(dense) => denseColGram(dense, metric)
            case Left(MultivarError.DensificationRejected(_, storage)) =>
              Left(MultivarError.DensificationRejected("column Gram", storage))
            case Left(error) => Left(error)

  /** tr(X' M X A), the total generalized variance, without forming an n x n product. */
  def totalVariance(
      x: MatrixView,
      rowMetric: MvMetric,
      colMetric: MvMetric,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, Double] =
    if rowMetric.dim != x.rows then
      Left(MultivarError.MetricShapeMismatch(IndexAxis.Row, x.rows, rowMetric.dim))
    else if colMetric.dim != x.cols then
      Left(MultivarError.MetricShapeMismatch(IndexAxis.Column, x.cols, colMetric.dim))
    else
      x match
        case sparse: SparseMatrixView if rowMetric.isDiagonal && colMetric.isDiagonal =>
          var acc = 0.0
          sparse.foreachEntry { (row, col, value) =>
            acc += diagonalWeight(rowMetric, row) * diagonalWeight(colMetric, col) * value * value
          }
          Right(acc)
        case sparse: SparseMatrixView if policy != StoragePolicy.AllowDense =>
          Right(sparseTotalVariance(sparse, rowMetric, colMetric))
        case _: AffineMatrixView =>
          rowGram(x, rowMetric, policy).flatMap(colMetric.contract)
        case _ =>
          if x.cols <= x.rows then rowGram(x, rowMetric, policy).flatMap(colMetric.contract)
          else colGram(x, colMetric, policy).flatMap(rowMetric.contract)

  /** (g + g') / 2, guarding eigensolvers against asymmetry from roundoff. */
  def symmetrize(g: DoubleMatrix): DoubleMatrix =
    val n = g.rows
    val out = new Array[Double](n * n)
    var row = 0
    while row < n do
      var col = 0
      while col < n do
        out(row * n + col) = 0.5 * (g(row, col) + g(col, row))
        col += 1
      row += 1
    DoubleMatrix.unsafe(n, n, out)

  private def denseRowGram(xd: DoubleMatrix, metric: MvMetric): Either[MultivarError, DoubleMatrix] =
    metric match
      case MvMetric.Identity(_, _) =>
        Right(DoubleMatrix.crossProduct(xd))
      case MvMetric.Diagonal(weights, _) =>
        Right(DoubleMatrix.transposeMultiply(xd, MatrixView.scaleRows(xd, weights)))
      case _ =>
        metric.matvec(xd).map(mx => DoubleMatrix.transposeMultiply(xd, mx))

  private def denseColGram(xd: DoubleMatrix, metric: MvMetric): Either[MultivarError, DoubleMatrix] =
    applyMetricRight(xd, metric).map(xa => DoubleMatrix.multiply(xa, xd.transpose))

  /** X * A for a column metric A, exploiting symmetry for the sparse case. */
  private def applyMetricRight(xd: DoubleMatrix, metric: MvMetric): Either[MultivarError, DoubleMatrix] =
    metric match
      case MvMetric.Identity(_, _) =>
        Right(xd)
      case MvMetric.Diagonal(weights, _) =>
        Right(MetricOperator.scaleColumnsDense(xd, weights))
      case MvMetric.DenseSymmetric(matrix, _) =>
        Right(DoubleMatrix.multiply(xd, matrix))
      case MvMetric.SparseSymmetric(view, _) =>
        view.rightMultiply(xd.transpose).map(_.transpose)

  /** Row Gram of a lazy column-affine view, expanded analytically so the base never
    * materializes: with X = B S + 1 h' (S = diag(scale)),
    * X' M X = S G_B S + (s * t) h' + h (s * t)' + (1' M 1) h h', where G_B = B' M B
    * and t = B' (M 1).
    */
  private def affineRowGram(
      affine: AffineMatrixView,
      metric: MvMetric,
      policy: StoragePolicy
  ): Either[MultivarError, DoubleMatrix] =
    val ones = MatrixView.ones(affine.rows)
    for
      baseGram <- rowGram(affine.base, metric, policy)
      weightedOnes <- metric.applyVector(ones)
      t <- affine.base.transposeMultiply(
        DenseMatrixView(DoubleMatrix.unsafe(affine.rows, 1, weightedOnes.copyData))
      )
      total <- metric.innerProduct(ones, ones)
    yield
      val out = MatrixView.scaleRowsAndColumns(baseGram, affine.scale, affine.scale)
      val tVector = DoubleVector.unsafe(t.copyData)
      val scaledT = MatrixView.multiply(affine.scale, tVector)
      MatrixView.addOuterProductInPlace(out, scaledT, affine.shift)
      MatrixView.addOuterProductInPlace(out, affine.shift, scaledT)
      MatrixView.addOuterProductInPlace(out, affine.shift, affine.shift, factor = total)
      out

  private def diagonalWeight(metric: MvMetric, index: Int): Double =
    metric match
      case MvMetric.Diagonal(weights, _) => weights(index)
      case _                             => 1.0

  /** tr(X' M X A) = sum over M entries (i, k) of M_ik * (x_i' A x_k), driven entirely
    * by sparse row access — no densification. Dense M costs one row-pair product per
    * (i, k) pair, proportionate to the n x n metric itself.
    */
  private def sparseTotalVariance(
      x: SparseMatrixView,
      rowMetric: MvMetric,
      colMetric: MvMetric
  ): Double =
    rowMetric match
      case MvMetric.Identity(dim, _) =>
        var acc = 0.0
        var i = 0
        while i < dim do
          acc += rowPairThroughMetric(x, i, i, colMetric)
          i += 1
        acc
      case MvMetric.Diagonal(weights, _) =>
        var acc = 0.0
        var i = 0
        while i < weights.length do
          acc += weights(i) * rowPairThroughMetric(x, i, i, colMetric)
          i += 1
        acc
      case MvMetric.DenseSymmetric(matrix, _) =>
        var acc = 0.0
        var i = 0
        while i < matrix.rows do
          acc += matrix(i, i) * rowPairThroughMetric(x, i, i, colMetric)
          var j = i + 1
          while j < matrix.rows do
            val weight = matrix(i, j)
            if weight != 0.0 then acc += 2.0 * weight * rowPairThroughMetric(x, i, j, colMetric)
            j += 1
          i += 1
        acc
      case MvMetric.SparseSymmetric(view, _) =>
        var acc = 0.0
        view.foreachEntry { (i, j, weight) =>
          acc += weight * rowPairThroughMetric(x, i, j, colMetric)
        }
        acc

  /** x_left' A x_right over sparse rows of X. */
  private def rowPairThroughMetric(
      x: SparseMatrixView,
      left: Int,
      right: Int,
      metric: MvMetric
  ): Double =
    var acc = 0.0
    metric match
      case MvMetric.Identity(_, _) =>
        x.foreachEntryInRow(left)((col, value) => acc += value * x.valueAt(right, col))
      case MvMetric.Diagonal(weights, _) =>
        x.foreachEntryInRow(left)((col, value) => acc += weights(col) * value * x.valueAt(right, col))
      case MvMetric.DenseSymmetric(matrix, _) =>
        x.foreachEntryInRow(left) { (leftCol, leftValue) =>
          x.foreachEntryInRow(right) { (rightCol, rightValue) =>
            acc += leftValue * matrix(leftCol, rightCol) * rightValue
          }
        }
      case MvMetric.SparseSymmetric(view, _) =>
        x.foreachEntryInRow(left) { (leftCol, leftValue) =>
          x.foreachEntryInRow(right) { (rightCol, rightValue) =>
            acc += leftValue * view.valueAt(leftCol, rightCol) * rightValue
          }
        }
    acc
