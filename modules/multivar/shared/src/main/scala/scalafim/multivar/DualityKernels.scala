package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

/** Storage-aware Gram and trace kernels for metric-weighted tables.
  *
  * The supported storage combinations define the duality-diagram algebra: dense and
  * lazy-affine data work with every metric kind; sparse-data Grams work with
  * diagonal-like metrics without densification and with dense/sparse metrics only
  * under `StoragePolicy.AllowDense`; the total-variance trace is always computable
  * matrix-free.
  */
private[multivar] object DualityKernels:

  /** X' M X (p x p) for a row metric M (n x n). */
  def rowGram(
      x: MatrixView,
      metric: MvMetric,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, DMat] =
    DualityDiagram.from(x, rowMetric = Some(metric)).flatMap(rowGram(_, policy))

  def rowGram(
      diagram: DualityDiagram,
      policy: StoragePolicy
  ): Either[MultivarError, DMat] =
    gramContext("row Gram")(rowGramUnchecked(diagram.table, diagram.rowMetric, policy))

  /** X A X' (n x n) for a column metric A (p x p); dense data only. */
  def colGram(
      x: MatrixView,
      metric: MvMetric,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, DMat] =
    DualityDiagram.from(x, columnMetric = Some(metric)).flatMap(colGram(_, policy))

  def colGram(
      diagram: DualityDiagram,
      policy: StoragePolicy
  ): Either[MultivarError, DMat] =
    gramContext("column Gram")(colGramUnchecked(diagram.table, diagram.columnMetric, policy))

  /** X' D Y (p x q) for row-aligned paired diagrams sharing row metric D.
    *
    * Paired construction already guarantees one shared row metric by value; the
    * check here is defense in depth and compares values, not references, so two
    * separately built but numerically identical metrics are one metric.
    */
  def crossGram(
      paired: PairedDualityDiagram,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, DMat] =
    if !paired.x.rowMetric.sameValues(paired.y.rowMetric) then
      Left(
        MultivarError.MetricMismatch(
          "cross Gram requires paired diagrams that share one row metric, but the x and y row metrics differ in value"
        )
      )
    else gramContext("cross Gram")(crossGramUnchecked(paired.x.table, paired.y.table, paired.rowMetric, policy))

  /** tr(X' M X A), the total generalized variance, without forming an n x n product. */
  def totalVariance(
      x: MatrixView,
      rowMetric: MvMetric,
      colMetric: MvMetric,
      policy: StoragePolicy = StoragePolicy.AllowDense
  ): Either[MultivarError, Double] =
    DualityDiagram
      .from(x, rowMetric = Some(rowMetric), columnMetric = Some(colMetric))
      .flatMap(totalVariance(_, policy))

  def totalVariance(
      diagram: DualityDiagram,
      policy: StoragePolicy
  ): Either[MultivarError, Double] =
    totalVarianceUnchecked(diagram.table, diagram.rowMetric, diagram.columnMetric, policy)

  /** (g + g') / 2, guarding eigensolvers against asymmetry from roundoff. */
  def symmetrize(g: DMat): DMat =
    val n = g.rows
    val out = new Array[Double](n * n)
    var row = 0
    while row < n do
      var col = 0
      while col < n do
        out(row * n + col) = 0.5 * (g(row, col) + g(col, row))
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(n, n, out)

  /** Rewrap densification rejections under one operation context per Gram kind, so
    * equivalent policy failures surface identically no matter which storage seam
    * rejected first and callers can match on the operation reliably.
    */
  private def gramContext[A](operation: String)(result: Either[MultivarError, A]): Either[MultivarError, A] =
    result.left.map {
      case MultivarError.DensificationRejected(_, storage) =>
        MultivarError.DensificationRejected(operation, storage)
      case other =>
        other
    }

  private[multivar] def multiplyMetricRight(xd: DMat, metric: MvMetric): Either[MultivarError, DMat] =
    if metric.dim != xd.cols then
      Left(MultivarError.MetricShapeMismatch(IndexAxis.Column, xd.cols, metric.dim))
    else applyMetricRight(xd, metric)

  private def rowGramUnchecked(
      x: MatrixView,
      metric: MvMetric,
      policy: StoragePolicy
  ): Either[MultivarError, DMat] =
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
            Left(MultivarError.DensificationRejected("row Gram", StorageKind.Sparse))
      case transposed: TransposedMatrixView =>
        transposed.base match
          case sparse: SparseMatrixView =>
            metric match
              case MvMetric.Identity(_, _) =>
                sparse.rightMultiplySparseTranspose(sparse)
              case MvMetric.Diagonal(weights, _) =>
                sparse.scaleColumns(weights).flatMap(sparse.rightMultiplySparseTranspose)
              case _ if policy == StoragePolicy.AllowDense =>
                transposed.toDense(StoragePolicy.AllowDense).flatMap(denseRowGram(_, metric))
              case _ =>
                Left(MultivarError.DensificationRejected("row Gram", StorageKind.Sparse))
          case _ =>
            transposed.toDense(policy).flatMap(denseRowGram(_, metric))
      case affine: AffineMatrixView =>
        affineRowGram(affine, metric, policy)
      case other =>
        other.toDense(policy).flatMap(denseRowGram(_, metric))

  private def colGramUnchecked(
      x: MatrixView,
      metric: MvMetric,
      policy: StoragePolicy
  ): Either[MultivarError, DMat] =
    x match
      case dense: DenseMatrixView =>
        denseColGram(dense.value, metric)
      case transposed: TransposedMatrixView =>
        transposed.base match
          case sparse: SparseMatrixView =>
            metric match
              case MvMetric.Identity(_, _) | MvMetric.Diagonal(_, _) =>
                rowGramUnchecked(sparse, metric, policy)
              case _ if policy == StoragePolicy.AllowDense =>
                transposed.toDense(StoragePolicy.AllowDense).flatMap(denseColGram(_, metric))
              case _ =>
                Left(MultivarError.DensificationRejected("column Gram", StorageKind.Sparse))
          case _ =>
            transposed.toDense(policy).flatMap(denseColGram(_, metric))
      case other =>
        other.toDense(policy).flatMap(denseColGram(_, metric))

  private def crossGramUnchecked(
      x: MatrixView,
      y: MatrixView,
      rowMetric: MvMetric,
      policy: StoragePolicy
  ): Either[MultivarError, DMat] =
    if x.rows != y.rows then Left(MultivarError.MatrixShapeMismatch(s"cross Gram expected equal rows, got ${x.rows} and ${y.rows}"))
    else if rowMetric.dim != x.rows then Left(MultivarError.MetricShapeMismatch(IndexAxis.Row, x.rows, rowMetric.dim))
    else
      rowMetric match
        case MvMetric.Identity(_, _) =>
          x.transposeMultiply(y)
        case MvMetric.Diagonal(weights, _) =>
          y match
            case sparse: SparseMatrixView =>
              sparse.scaleRows(weights).flatMap(scaled => x.transposeMultiply(scaled))
            case _ =>
              y.toDense(policy).flatMap { denseY =>
                x.transposeMultiply(DenseMatrixView(MatrixView.scaleRows(denseY, weights)))
              }
        case _ =>
          y match
            case _: SparseMatrixView | _: AffineMatrixView | _: TransposedMatrixView if policy != StoragePolicy.AllowDense =>
              Left(MultivarError.DensificationRejected("cross Gram", y.storage))
            case _ =>
              for
                yd <- y.toDense(StoragePolicy.AllowDense)
                weighted <- rowMetric.matvec(yd)
                out <- x.transposeMultiply(DenseMatrixView(weighted))
              yield out

  private def totalVarianceUnchecked(
      x: MatrixView,
      rowMetric: MvMetric,
      colMetric: MvMetric,
      policy: StoragePolicy
  ): Either[MultivarError, Double] =
    x match
      case sparse: SparseMatrixView if rowMetric.isDiagonal && colMetric.isDiagonal =>
        var acc = 0.0
        sparse.foreachEntry { (row, col, value) =>
          acc += diagonalWeight(rowMetric, row) * diagonalWeight(colMetric, col) * value * value
        }
        Right(acc)
      case sparse: SparseMatrixView if policy != StoragePolicy.AllowDense =>
        Right(sparseTotalVariance(sparse, rowMetric, colMetric))
      case transposed: TransposedMatrixView =>
        transposed.base match
          case sparse: SparseMatrixView =>
            // tr(X' M X A) with X = S' equals tr(S' A S M): transposition swaps the
            // metric pairing, so the sparse kernels serve the base matrix directly.
            totalVarianceUnchecked(sparse, colMetric, rowMetric, policy)
          case _ =>
            orientedTotalVariance(transposed, rowMetric, colMetric, policy)
      case _: AffineMatrixView =>
        rowGramUnchecked(x, rowMetric, policy).flatMap(colMetric.contract)
      case _ =>
        orientedTotalVariance(x, rowMetric, colMetric, policy)

  /** Gram-then-contract on the smaller side of a dense-representable table. */
  private def orientedTotalVariance(
      x: MatrixView,
      rowMetric: MvMetric,
      colMetric: MvMetric,
      policy: StoragePolicy
  ): Either[MultivarError, Double] =
    if x.cols <= x.rows then rowGramUnchecked(x, rowMetric, policy).flatMap(colMetric.contract)
    else colGramUnchecked(x, colMetric, policy).flatMap(rowMetric.contract)

  private def denseRowGram(xd: DMat, metric: MvMetric): Either[MultivarError, DMat] =
    metric match
      case MvMetric.Identity(_, _) =>
        Right(GaleNumerics.crossProduct(xd))
      case MvMetric.Diagonal(weights, _) =>
        Right(GaleNumerics.transposeMultiply(xd, MatrixView.scaleRows(xd, weights)))
      case _ =>
        metric.matvec(xd).map(mx => GaleNumerics.transposeMultiply(xd, mx))

  private def denseColGram(xd: DMat, metric: MvMetric): Either[MultivarError, DMat] =
    applyMetricRight(xd, metric).map(xa => GaleNumerics.multiply(xa, xd.transpose))

  /** X * A for a column metric A, exploiting symmetry for the sparse case. */
  private def applyMetricRight(xd: DMat, metric: MvMetric): Either[MultivarError, DMat] =
    metric match
      case MvMetric.Identity(_, _) =>
        Right(xd)
      case MvMetric.Diagonal(weights, _) =>
        Right(MetricOperator.scaleColumnsDense(xd, weights))
      case MvMetric.DenseSymmetric(matrix, _) =>
        Right(GaleNumerics.multiply(xd, matrix))
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
  ): Either[MultivarError, DMat] =
    val ones = MatrixView.ones(affine.rows)
    for
      baseGram <- rowGram(affine.base, metric, policy)
      weightedOnes <- metric.applyVector(ones)
      t <- affine.base.transposeMultiply(
        DenseMatrixView(GaleNumerics.matrixFromRowMajor(affine.rows, 1, weightedOnes.copyData))
      )
      total <- metric.innerProduct(ones, ones)
    yield
      val scaled = MatrixView.scaleRowsAndColumns(baseGram, affine.scale, affine.scale)
      val out = scaled.copyData
      val tVector = GaleNumerics.vectorFromArray(t.copyData)
      val scaledT = MatrixView.multiply(affine.scale, tVector)
      MatrixView.addOuterProductInPlace(out, scaled.rows, scaled.cols, scaledT, affine.shift)
      MatrixView.addOuterProductInPlace(out, scaled.rows, scaled.cols, affine.shift, scaledT)
      MatrixView.addOuterProductInPlace(out, scaled.rows, scaled.cols, affine.shift, affine.shift, factor = total)
      GaleNumerics.matrixFromRowMajor(scaled.rows, scaled.cols, out)

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
