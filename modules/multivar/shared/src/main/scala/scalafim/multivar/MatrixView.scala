package scalafim.multivar

import scala.collection.mutable.ArrayBuffer

import gale.linalg.DMat
import gale.linalg.DVec

enum StorageKind:
  case Dense
  case Sparse
  case LazyAffine
  case Operator

  def label: String =
    this match
      case Dense      => "dense"
      case Sparse     => "sparse"
      case LazyAffine => "lazy affine"
      case Operator   => "operator"

enum StoragePolicy:
  case Operator
  case PreserveSparse
  case AllowDense

/** Per-column first and second raw moments, plus (when the source representation can
  * compute them exactly) two-pass centered sums of squares for cancellation-free
  * standard deviations. Views that only propagate moments (e.g. lazy affine bases)
  * may leave `centeredSumSquares` empty, in which case the raw-moment fallback is used.
  */
final case class ColumnStats(
    count: Int,
    sums: DVec,
    sumSquares: DVec,
    centeredSumSquares: Option[DVec] = None
):
  require(count >= 0, "column-stat count must be non-negative")
  require(sums.length == sumSquares.length, "column-stat vectors must have equal length")
  require(
    centeredSumSquares.forall(_.length == sums.length),
    "column-stat vectors must have equal length"
  )

  def cols: Int =
    sums.length

  def means: Either[MultivarError, DVec] =
    if count <= 0 then Left(MultivarError.InvalidDimension("row count for column means", count))
    else
      val out = new Array[Double](cols)
      var col = 0
      while col < cols do
        out(col) = sums(col) / count
        col += 1
      Right(GaleNumerics.vectorFromArray(out))

  def sampleStandardDeviations: Either[MultivarError, DVec] =
    if count <= 1 then Left(MultivarError.InsufficientRows("sample standard deviations", 2, count))
    else
      val out = new Array[Double](cols)
      centeredSumSquares match
        case Some(centered) =>
          var col = 0
          while col < cols do
            out(col) = Math.sqrt(Math.max(centered(col) / (count - 1), 0.0))
            col += 1
        case None =>
          var col = 0
          while col < cols do
            val mean = sums(col) / count
            val ss = sumSquares(col) - count * mean * mean
            out(col) = Math.sqrt(Math.max(ss / (count - 1), 0.0))
            col += 1
      Right(GaleNumerics.vectorFromArray(out))

object ColumnStats:
  def fromDense(matrix: DMat): Either[MultivarError, ColumnStats] =
    val data = matrix.copyData
    val sums = new Array[Double](matrix.cols)
    val sumSquares = new Array[Double](matrix.cols)
    var row = 0
    var error = Option.empty[MultivarError]
    while row < matrix.rows && error.isEmpty do
      val offset = row * matrix.cols
      var col = 0
      while col < matrix.cols && error.isEmpty do
        val value = data(offset + col)
        if !value.isFinite then error = Some(MultivarError.NonFiniteValue("matrix", offset + col, value))
        else
          sums(col) += value
          sumSquares(col) += value * value
        col += 1
      row += 1

    error match
      case Some(value) => Left(value)
      case None =>
        val centered = new Array[Double](matrix.cols)
        if matrix.rows > 0 then
          val means = new Array[Double](matrix.cols)
          var col = 0
          while col < matrix.cols do
            means(col) = sums(col) / matrix.rows
            col += 1
          row = 0
          while row < matrix.rows do
            val offset = row * matrix.cols
            var c = 0
            while c < matrix.cols do
              val deviation = data(offset + c) - means(c)
              centered(c) += deviation * deviation
              c += 1
            row += 1
        Right(
          ColumnStats(
            count = matrix.rows,
            sums = GaleNumerics.vectorFromArray(sums),
            sumSquares = GaleNumerics.vectorFromArray(sumSquares),
            centeredSumSquares = Some(GaleNumerics.vectorFromArray(centered))
          )
        )

  def fromDenseRows(matrix: DMat): Either[MultivarError, ColumnStats] =
    val data = matrix.copyData
    val sums = new Array[Double](matrix.rows)
    val sumSquares = new Array[Double](matrix.rows)
    var row = 0
    var error = Option.empty[MultivarError]
    while row < matrix.rows && error.isEmpty do
      val offset = row * matrix.cols
      var col = 0
      while col < matrix.cols && error.isEmpty do
        val value = data(offset + col)
        if !value.isFinite then error = Some(MultivarError.NonFiniteValue("matrix", offset + col, value))
        else
          sums(row) += value
          sumSquares(row) += value * value
        col += 1
      row += 1

    error match
      case Some(value) => Left(value)
      case None =>
        val centered = new Array[Double](matrix.rows)
        if matrix.cols > 0 then
          row = 0
          while row < matrix.rows do
            val offset = row * matrix.cols
            val mean = sums(row) / matrix.cols
            var c = 0
            while c < matrix.cols do
              val deviation = data(offset + c) - mean
              centered(row) += deviation * deviation
              c += 1
            row += 1
        Right(
          ColumnStats(
            count = matrix.cols,
            sums = GaleNumerics.vectorFromArray(sums),
            sumSquares = GaleNumerics.vectorFromArray(sumSquares),
            centeredSumSquares = Some(GaleNumerics.vectorFromArray(centered))
          )
        )

trait MatrixView:
  def rows: Int
  def cols: Int
  def storage: StorageKind

  def columnStats: Either[MultivarError, ColumnStats]

  private[multivar] def rowStats: Either[MultivarError, ColumnStats]

  def rightMultiply(weights: DMat): Either[MultivarError, DMat]

  def transposeMultiply(other: MatrixView): Either[MultivarError, DMat]

  def crossProduct: Either[MultivarError, DMat] =
    transposeMultiply(this)

  def selectColumns(columns: IndexSet): Either[MultivarError, MatrixView]

  def toDense(policy: StoragePolicy = StoragePolicy.AllowDense): Either[MultivarError, DMat]

  def transposeView: MatrixView =
    MatrixView.transpose(this)

final class DenseMatrixView private (val value: DMat) extends MatrixView:
  override def rows: Int =
    value.rows

  override def cols: Int =
    value.cols

  override def storage: StorageKind =
    StorageKind.Dense

  override def columnStats: Either[MultivarError, ColumnStats] =
    ColumnStats.fromDense(value)

  override private[multivar] def rowStats: Either[MultivarError, ColumnStats] =
    ColumnStats.fromDenseRows(value)

  override def rightMultiply(weights: DMat): Either[MultivarError, DMat] =
    MatrixView.requireWeightRows(cols, weights.rows).map { _ =>
      GaleNumerics.multiply(value, weights)
    }

  override def transposeMultiply(other: MatrixView): Either[MultivarError, DMat] =
    MatrixView.requireSharedRows(rows, other.rows).flatMap { _ =>
      other.toDense().map(denseOther => GaleNumerics.transposeMultiply(value, denseOther))
    }

  override def selectColumns(columns: IndexSet): Either[MultivarError, MatrixView] =
    MatrixView.requireColumnIndexSet(columns, cols).map { checked =>
      val indices = checked.indices
      val out = new Array[Double](rows * indices.length)
      var row = 0
      while row < rows do
        var col = 0
        while col < indices.length do
          out(row * indices.length + col) = value(row, indices(col))
          col += 1
        row += 1
      DenseMatrixView.unsafe(GaleNumerics.matrixFromRowMajor(rows, indices.length, out))
    }

  override def toDense(policy: StoragePolicy): Either[MultivarError, DMat] =
    Right(value)

object DenseMatrixView:
  def apply(value: DMat): DenseMatrixView =
    new DenseMatrixView(value)

  private[scalafim] def unsafe(value: DMat): DenseMatrixView =
    new DenseMatrixView(value)

final class SparseMatrixView private (
    val rows: Int,
    val cols: Int,
    private val rowPtr: Array[Int],
    private val colIndex: Array[Int],
    private val data: Array[Double]
) extends MatrixView:
  def nnz: Int =
    data.length

  override def storage: StorageKind =
    StorageKind.Sparse

  override def columnStats: Either[MultivarError, ColumnStats] =
    val sums = new Array[Double](cols)
    val sumSquares = new Array[Double](cols)
    val counts = new Array[Int](cols)
    var p = 0
    var error = Option.empty[MultivarError]
    while p < data.length && error.isEmpty do
      val value = data(p)
      if !value.isFinite then error = Some(MultivarError.NonFiniteValue("sparse matrix", p, value))
      else
        val col = colIndex(p)
        sums(col) += value
        sumSquares(col) += value * value
        counts(col) += 1
      p += 1

    error match
      case Some(value) => Left(value)
      case None =>
        val centered = new Array[Double](cols)
        if rows > 0 then
          val means = new Array[Double](cols)
          var col = 0
          while col < cols do
            means(col) = sums(col) / rows
            col += 1
          p = 0
          while p < data.length do
            val deviation = data(p) - means(colIndex(p))
            centered(colIndex(p)) += deviation * deviation
            p += 1
          col = 0
          while col < cols do
            val mean = means(col)
            centered(col) += (rows - counts(col)) * mean * mean
            col += 1
        Right(
          ColumnStats(
            rows,
            GaleNumerics.vectorFromArray(sums),
            GaleNumerics.vectorFromArray(sumSquares),
            Some(GaleNumerics.vectorFromArray(centered))
          )
        )

  override private[multivar] def rowStats: Either[MultivarError, ColumnStats] =
    val sums = new Array[Double](rows)
    val sumSquares = new Array[Double](rows)
    var row = 0
    var error = Option.empty[MultivarError]
    while row < rows && error.isEmpty do
      var p = rowPtr(row)
      val end = rowPtr(row + 1)
      while p < end && error.isEmpty do
        val value = data(p)
        if !value.isFinite then error = Some(MultivarError.NonFiniteValue("sparse matrix", p, value))
        else
          sums(row) += value
          sumSquares(row) += value * value
        p += 1
      row += 1

    error match
      case Some(value) => Left(value)
      case None =>
        val centered = new Array[Double](rows)
        if cols > 0 then
          row = 0
          while row < rows do
            val start = rowPtr(row)
            val end = rowPtr(row + 1)
            val mean = sums(row) / cols
            var p = start
            while p < end do
              val deviation = data(p) - mean
              centered(row) += deviation * deviation
              p += 1
            centered(row) += (cols - (end - start)) * mean * mean
            row += 1
        Right(
          ColumnStats(
            cols,
            GaleNumerics.vectorFromArray(sums),
            GaleNumerics.vectorFromArray(sumSquares),
            Some(GaleNumerics.vectorFromArray(centered))
          )
        )

  override def rightMultiply(weights: DMat): Either[MultivarError, DMat] =
    MatrixView.requireWeightRows(cols, weights.rows).map { _ =>
      val out = new Array[Double](rows * weights.cols)
      var row = 0
      while row < rows do
        var p = rowPtr(row)
        val end = rowPtr(row + 1)
        while p < end do
          val sourceCol = colIndex(p)
          val value = data(p)
          var col = 0
          while col < weights.cols do
            out(row * weights.cols + col) += value * weights(sourceCol, col)
            col += 1
          p += 1
        row += 1
      GaleNumerics.matrixFromRowMajor(rows, weights.cols, out)
    }

  override def transposeMultiply(other: MatrixView): Either[MultivarError, DMat] =
    MatrixView.requireSharedRows(rows, other.rows).flatMap { _ =>
      other match
        case dense: DenseMatrixView =>
          Right(transposeMultiplyDense(dense.value))
        case sparse: SparseMatrixView =>
          Right(transposeMultiplySparse(sparse))
        case _ =>
          other.toDense().map(transposeMultiplyDense)
    }

  override def selectColumns(columns: IndexSet): Either[MultivarError, MatrixView] =
    MatrixView.requireColumnIndexSet(columns, cols).flatMap { checked =>
      val positions = MatrixView.positionLookup(checked, cols)
      var selected = 0
      var p = 0
      while p < data.length do
        if positions(colIndex(p)) >= 0 then selected += 1
        p += 1

      val outRows = new Array[Int](selected)
      val outCols = new Array[Int](selected)
      val outVals = new Array[Double](selected)
      var k = 0
      var row = 0
      while row < rows do
        var q = rowPtr(row)
        val end = rowPtr(row + 1)
        while q < end do
          val localCol = positions(colIndex(q))
          if localCol >= 0 then
            outRows(k) = row
            outCols(k) = localCol
            outVals(k) = data(q)
            k += 1
          q += 1
        row += 1

      SparseMatrixView.fromTriplets(rows, checked.length, outRows, outCols, outVals)
    }

  override def toDense(policy: StoragePolicy): Either[MultivarError, DMat] =
    policy match
      case StoragePolicy.AllowDense =>
        val out = new Array[Double](rows * cols)
        var row = 0
        while row < rows do
          var p = rowPtr(row)
          val end = rowPtr(row + 1)
          while p < end do
            out(row * cols + colIndex(p)) = data(p)
            p += 1
          row += 1
        Right(GaleNumerics.matrixFromRowMajor(rows, cols, out))
      case _ =>
        Left(MultivarError.DensificationRejected("toDense", storage))

  private[multivar] def scaleRows(scale: DVec): Either[MultivarError, SparseMatrixView] =
    MatrixView.requireVectorLength("sparse row scale", scale, rows).map { _ =>
      val out = new Array[Double](data.length)
      var row = 0
      while row < rows do
        val factor = scale(row)
        var p = rowPtr(row)
        val end = rowPtr(row + 1)
        while p < end do
          out(p) = data(p) * factor
          p += 1
        row += 1
      SparseMatrixView.unsafe(rows, cols, rowPtr.clone, colIndex.clone, out)
    }

  private[multivar] def foreachEntry(f: (Int, Int, Double) => Unit): Unit =
    var row = 0
    while row < rows do
      var p = rowPtr(row)
      val end = rowPtr(row + 1)
      while p < end do
        f(row, colIndex(p), data(p))
        p += 1
      row += 1

  private[multivar] def foreachEntryInRow(row: Int)(f: (Int, Double) => Unit): Unit =
    var p = rowPtr(row)
    val end = rowPtr(row + 1)
    while p < end do
      f(colIndex(p), data(p))
      p += 1

  /** Stored value at (row, col), 0.0 for structural zeros; binary search within the row. */
  private[multivar] def valueAt(row: Int, col: Int): Double =
    var low = rowPtr(row)
    var high = rowPtr(row + 1) - 1
    var found = 0.0
    while low <= high do
      val mid = (low + high) >>> 1
      val midCol = colIndex(mid)
      if midCol == col then
        found = data(mid)
        low = high + 1
      else if midCol < col then low = mid + 1
      else high = mid - 1
    found

  private[multivar] def scaleColumns(scale: DVec): Either[MultivarError, SparseMatrixView] =
    MatrixView.requireVectorLength("sparse column scale", scale, cols).map { _ =>
      val out = new Array[Double](data.length)
      var p = 0
      while p < data.length do
        out(p) = data(p) * scale(colIndex(p))
        p += 1
      SparseMatrixView.unsafe(rows, cols, rowPtr.clone, colIndex.clone, out)
    }

  private[multivar] def rightMultiplySparse(weights: SparseMatrixView): Either[MultivarError, DMat] =
    MatrixView.requireWeightRows(cols, weights.rows).map { _ =>
      val out = new Array[Double](rows * weights.cols)
      var row = 0
      while row < rows do
        var p = rowPtr(row)
        val pEnd = rowPtr(row + 1)
        while p < pEnd do
          val sourceCol = colIndex(p)
          val leftValue = data(p)
          var q = weights.rowPtr(sourceCol)
          val qEnd = weights.rowPtr(sourceCol + 1)
          while q < qEnd do
            out(row * weights.cols + weights.colIndex(q)) += leftValue * weights.data(q)
            q += 1
          p += 1
        row += 1
      GaleNumerics.matrixFromRowMajor(rows, weights.cols, out)
    }

  /** Computes `left * this` with raw CSR loops; shapes are validated by the caller. */
  private[multivar] def leftMultiplyDense(left: DMat): DMat =
    val out = new Array[Double](left.rows * cols)
    var leftRow = 0
    while leftRow < left.rows do
      val outOffset = leftRow * cols
      var row = 0
      while row < rows do
        val leftValue = left(leftRow, row)
        if leftValue != 0.0 then
          var p = rowPtr(row)
          val end = rowPtr(row + 1)
          while p < end do
            out(outOffset + colIndex(p)) += leftValue * data(p)
            p += 1
        row += 1
      leftRow += 1
    GaleNumerics.matrixFromRowMajor(left.rows, cols, out)

  /** Computes `left * this.transpose` with raw CSR loops; shapes are validated by the caller. */
  private[multivar] def leftMultiplyDenseTranspose(left: DMat): DMat =
    val out = new Array[Double](left.rows * rows)
    var leftRow = 0
    while leftRow < left.rows do
      val outOffset = leftRow * rows
      var row = 0
      while row < rows do
        var p = rowPtr(row)
        val end = rowPtr(row + 1)
        var acc = 0.0
        while p < end do
          acc += left(leftRow, colIndex(p)) * data(p)
          p += 1
        out(outOffset + row) = acc
        row += 1
      leftRow += 1
    GaleNumerics.matrixFromRowMajor(left.rows, rows, out)

  /** Row moments of `this * diag(scale) + 1 shiftᵀ` computed from stored entries only:
    * row sums are `(X·s)ᵢ + Σⱼhⱼ` and row sums of squares are
    * `Σⱼ(xᵢⱼsⱼ)² + 2Σⱼxᵢⱼsⱼhⱼ + Σⱼhⱼ²`, so no densification is required.
    */
  private[multivar] def affineRowStats(
      scale: DVec,
      shift: DVec
  ): Either[MultivarError, ColumnStats] =
    var shiftSum = 0.0
    var shiftSumSq = 0.0
    var col = 0
    while col < cols do
      val h = shift(col)
      shiftSum += h
      shiftSumSq += h * h
      col += 1

    val sums = new Array[Double](rows)
    val sumSquares = new Array[Double](rows)
    var row = 0
    var error = Option.empty[MultivarError]
    while row < rows && error.isEmpty do
      var rowSum = shiftSum
      var rowSumSq = shiftSumSq
      var p = rowPtr(row)
      val end = rowPtr(row + 1)
      while p < end && error.isEmpty do
        val value = data(p)
        if !value.isFinite then error = Some(MultivarError.NonFiniteValue("sparse matrix", p, value))
        else
          val sourceCol = colIndex(p)
          val scaled = value * scale(sourceCol)
          rowSum += scaled
          rowSumSq += scaled * scaled + 2.0 * scaled * shift(sourceCol)
        p += 1
      sums(row) = rowSum
      sumSquares(row) = rowSumSq
      row += 1

    error match
      case Some(value) => Left(value)
      case None        => Right(ColumnStats(cols, GaleNumerics.vectorFromArray(sums), GaleNumerics.vectorFromArray(sumSquares)))

  private[multivar] def rightMultiplySparseTranspose(weights: SparseMatrixView): Either[MultivarError, DMat] =
    if weights.cols != cols then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"right multiply by transpose expected ${cols} weight columns, got ${weights.cols}"
        )
      )
    else
      val out = new Array[Double](rows * weights.rows)
      var row = 0
      while row < rows do
        var weightRow = 0
        while weightRow < weights.rows do
          val value = dotRows(weights, row, weightRow)
          if value != 0.0 then out(row * weights.rows + weightRow) = value
          weightRow += 1
        row += 1
      Right(GaleNumerics.matrixFromRowMajor(rows, weights.rows, out))

  private def dotRows(other: SparseMatrixView, leftRow: Int, rightRow: Int): Double =
    var p = rowPtr(leftRow)
    val pEnd = rowPtr(leftRow + 1)
    var q = other.rowPtr(rightRow)
    val qEnd = other.rowPtr(rightRow + 1)
    var acc = 0.0
    while p < pEnd && q < qEnd do
      val leftCol = colIndex(p)
      val rightCol = other.colIndex(q)
      if leftCol == rightCol then
        acc += data(p) * other.data(q)
        p += 1
        q += 1
      else if leftCol < rightCol then p += 1
      else q += 1
    acc

  private[multivar] def transposeSelectColumns(columns: IndexSet): Either[MultivarError, MatrixView] =
    MatrixView.requireColumnIndexSet(columns, rows).flatMap { checked =>
      val positions = MatrixView.positionLookup(checked, rows)
      var selected = 0
      var baseRow = 0
      while baseRow < rows do
        if positions(baseRow) >= 0 then selected += rowPtr(baseRow + 1) - rowPtr(baseRow)
        baseRow += 1

      val outRows = new Array[Int](selected)
      val outCols = new Array[Int](selected)
      val outVals = new Array[Double](selected)
      var k = 0
      baseRow = 0
      while baseRow < rows do
        val localCol = positions(baseRow)
        if localCol >= 0 then
          var p = rowPtr(baseRow)
          val end = rowPtr(baseRow + 1)
          while p < end do
            outRows(k) = colIndex(p)
            outCols(k) = localCol
            outVals(k) = data(p)
            k += 1
            p += 1
        baseRow += 1
      SparseMatrixView.fromTriplets(cols, checked.length, outRows, outCols, outVals)
    }

  private def transposeMultiplyDense(other: DMat): DMat =
    val out = new Array[Double](cols * other.cols)
    var row = 0
    while row < rows do
      var p = rowPtr(row)
      val end = rowPtr(row + 1)
      while p < end do
        val sourceCol = colIndex(p)
        val value = data(p)
        var otherCol = 0
        while otherCol < other.cols do
          out(sourceCol * other.cols + otherCol) += value * other(row, otherCol)
          otherCol += 1
        p += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(cols, other.cols, out)

  private def transposeMultiplySparse(other: SparseMatrixView): DMat =
    val out = new Array[Double](cols * other.cols)
    var row = 0
    while row < rows do
      var p = rowPtr(row)
      val pEnd = rowPtr(row + 1)
      while p < pEnd do
        val leftCol = colIndex(p)
        val leftValue = data(p)
        var q = other.rowPtr(row)
        val qEnd = other.rowPtr(row + 1)
        while q < qEnd do
          out(leftCol * other.cols + other.colIndex(q)) += leftValue * other.data(q)
          q += 1
        p += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(cols, other.cols, out)

object SparseMatrixView:
  def fromRows(rowsData: Seq[Seq[Double]]): Either[MultivarError, SparseMatrixView] =
    val indexedRows = rowsData.map(_.toIndexedSeq).toIndexedSeq
    val rowCount = indexedRows.length
    val colCount = if rowCount == 0 then 0 else indexedRows.head.length
    if rowCount <= 0 then Left(MultivarError.InvalidDimension("sparse matrix rows", rowCount))
    else if colCount <= 0 then Left(MultivarError.InvalidDimension("sparse matrix columns", colCount))
    else if !indexedRows.forall(_.length == colCount) then
      Left(MultivarError.MatrixShapeMismatch("sparse matrix rows must not be ragged"))
    else
      val rowIndices = ArrayBuffer.empty[Int]
      val colIndices = ArrayBuffer.empty[Int]
      val values = ArrayBuffer.empty[Double]
      var row = 0
      var error = Option.empty[MultivarError]
      while row < rowCount && error.isEmpty do
        var col = 0
        while col < colCount && error.isEmpty do
          val value = indexedRows(row)(col)
          if !value.isFinite then error = Some(MultivarError.NonFiniteValue("sparse matrix", row * colCount + col, value))
          else if value != 0.0 then
            rowIndices += row
            colIndices += col
            values += value
          col += 1
        row += 1

      error match
        case Some(value) => Left(value)
        case None        => fromTriplets(rowCount, colCount, rowIndices.toArray, colIndices.toArray, values.toArray)

  def fromTriplets(
      rows: Int,
      cols: Int,
      rowIndices: Array[Int],
      colIndices: Array[Int],
      values: Array[Double]
  ): Either[MultivarError, SparseMatrixView] =
    if rows <= 0 then Left(MultivarError.InvalidDimension("sparse matrix rows", rows))
    else if cols <= 0 then Left(MultivarError.InvalidDimension("sparse matrix columns", cols))
    else if rowIndices.length != colIndices.length || rowIndices.length != values.length then
      Left(MultivarError.MatrixShapeMismatch("sparse triplet arrays must have equal length"))
    else
      val order = Array.tabulate(values.length)(identity)
      var i = 0
      var error = Option.empty[MultivarError]
      while i < values.length && error.isEmpty do
        val row = rowIndices(i)
        val col = colIndices(i)
        val value = values(i)
        if row < 0 || row >= rows then error = Some(MultivarError.IndexOutOfBounds(IndexAxis.Row, row, rows))
        else if col < 0 || col >= cols then error = Some(MultivarError.IndexOutOfBounds(IndexAxis.Column, col, cols))
        else if !value.isFinite then error = Some(MultivarError.NonFiniteValue("sparse matrix", i, value))
        i += 1

      error match
        case Some(value) => Left(value)
        case None =>
          scala.util.Sorting.stableSort(
            order,
            (left: Int, right: Int) =>
              rowIndices(left) < rowIndices(right) ||
                (rowIndices(left) == rowIndices(right) && colIndices(left) < colIndices(right))
          )

          val outRows = ArrayBuffer.empty[Int]
          val outCols = ArrayBuffer.empty[Int]
          val outVals = ArrayBuffer.empty[Double]
          var k = 0
          while k < order.length do
            val first = order(k)
            val row = rowIndices(first)
            val col = colIndices(first)
            var value = values(first)
            k += 1
            while k < order.length && rowIndices(order(k)) == row && colIndices(order(k)) == col do
              value += values(order(k))
              k += 1
            if value != 0.0 then
              outRows += row
              outCols += col
              outVals += value

          val rowPtr = Array.ofDim[Int](rows + 1)
          i = 0
          while i < outRows.length do
            rowPtr(outRows(i) + 1) += 1
            i += 1

          var row = 0
          while row < rows do
            rowPtr(row + 1) += rowPtr(row)
            row += 1

          Right(unsafe(rows, cols, rowPtr, outCols.toArray, outVals.toArray))

  private[multivar] def unsafe(
      rows: Int,
      cols: Int,
      rowPtr: Array[Int],
      colIndex: Array[Int],
      data: Array[Double]
  ): SparseMatrixView =
    new SparseMatrixView(rows, cols, rowPtr, colIndex, data)

final class AffineMatrixView private (
    val base: MatrixView,
    val scale: DVec,
    val shift: DVec
) extends MatrixView:
  require(scale.length == base.cols, "affine scale must match base columns")
  require(shift.length == base.cols, "affine shift must match base columns")

  override def rows: Int =
    base.rows

  override def cols: Int =
    base.cols

  override def storage: StorageKind =
    StorageKind.LazyAffine

  override def columnStats: Either[MultivarError, ColumnStats] =
    base.columnStats.map { stats =>
      val sums = new Array[Double](cols)
      val sumSquares = new Array[Double](cols)
      var col = 0
      while col < cols do
        val baseSum = stats.sums(col)
        val baseSumSq = stats.sumSquares(col)
        val s = scale(col)
        val h = shift(col)
        sums(col) = baseSum * s + rows * h
        sumSquares(col) = s * s * baseSumSq + 2.0 * s * h * baseSum + rows * h * h
        col += 1
      // Centered sums of squares are shift-invariant and scale quadratically,
      // so the base's exact values propagate without cancellation.
      val centered = stats.centeredSumSquares.map { baseCentered =>
        val out = new Array[Double](cols)
        var c = 0
        while c < cols do
          val s = scale(c)
          out(c) = s * s * baseCentered(c)
          c += 1
        GaleNumerics.vectorFromArray(out)
      }
      ColumnStats(rows, GaleNumerics.vectorFromArray(sums), GaleNumerics.vectorFromArray(sumSquares), centered)
    }

  override private[multivar] def rowStats: Either[MultivarError, ColumnStats] =
    base match
      case sparse: SparseMatrixView =>
        sparse.affineRowStats(scale, shift)
      case _ =>
        toDense(StoragePolicy.AllowDense).flatMap(ColumnStats.fromDenseRows)

  override def rightMultiply(weights: DMat): Either[MultivarError, DMat] =
    MatrixView.requireWeightRows(cols, weights.rows).flatMap { _ =>
      val scaledWeights = MatrixView.scaleRows(weights, scale)
      base.rightMultiply(scaledWeights).map { out =>
        val shiftContribution = MatrixView.vectorTransposeMultiply(shift, weights)
        MatrixView.addRowVector(out, shiftContribution)
      }
    }

  override def transposeMultiply(other: MatrixView): Either[MultivarError, DMat] =
    MatrixView.requireSharedRows(rows, other.rows).flatMap { _ =>
      other match
        case affine: AffineMatrixView =>
          for
            baseCross <- base.transposeMultiply(affine.base)
            baseStats <- base.columnStats
            otherStats <- affine.base.columnStats
          yield
            val scaled = MatrixView.scaleRowsAndColumns(baseCross, scale, affine.scale)
            val out = scaled.copyData
            MatrixView.addOuterProductInPlace(out, scaled.rows, scaled.cols, MatrixView.multiply(baseStats.sums, scale), affine.shift)
            MatrixView.addOuterProductInPlace(out, scaled.rows, scaled.cols, shift, MatrixView.multiply(otherStats.sums, affine.scale))
            MatrixView.addOuterProductInPlace(out, scaled.rows, scaled.cols, shift, affine.shift, factor = rows.toDouble)
            GaleNumerics.matrixFromRowMajor(scaled.rows, scaled.cols, out)
        case _ =>
          for
            baseCross <- base.transposeMultiply(other)
            otherStats <- other.columnStats
          yield
            val scaled = MatrixView.scaleRows(baseCross, scale)
            val out = scaled.copyData
            MatrixView.addOuterProductInPlace(out, scaled.rows, scaled.cols, shift, otherStats.sums)
            GaleNumerics.matrixFromRowMajor(scaled.rows, scaled.cols, out)
    }

  override def selectColumns(columns: IndexSet): Either[MultivarError, MatrixView] =
    for
      checked <- MatrixView.requireColumnIndexSet(columns, cols)
      selectedBase <- base.selectColumns(checked)
      selectedScale = MatrixView.selectVector(scale, checked)
      selectedShift = MatrixView.selectVector(shift, checked)
      out <- MatrixView.affine(selectedBase, selectedScale, selectedShift, StoragePolicy.Operator, "affine column selection")
    yield out

  override def toDense(policy: StoragePolicy): Either[MultivarError, DMat] =
    policy match
      case StoragePolicy.AllowDense =>
        base.toDense(StoragePolicy.AllowDense).map(MatrixView.materializeAffine(_, scale, shift))
      case _ =>
        Left(MultivarError.DensificationRejected("toDense", storage))

object AffineMatrixView:
  private[multivar] def unsafe(base: MatrixView, scale: DVec, shift: DVec): AffineMatrixView =
    new AffineMatrixView(base, scale, shift)

final class TransposedMatrixView private (val base: MatrixView) extends MatrixView:
  override def rows: Int =
    base.cols

  override def cols: Int =
    base.rows

  override def storage: StorageKind =
    base match
      case _: DenseMatrixView  => StorageKind.Dense
      case _: SparseMatrixView => StorageKind.Sparse
      case _                   => StorageKind.Operator

  override def columnStats: Either[MultivarError, ColumnStats] =
    base.rowStats

  override def rightMultiply(weights: DMat): Either[MultivarError, DMat] =
    MatrixView.requireWeightRows(cols, weights.rows).flatMap { _ =>
      base.transposeMultiply(DenseMatrixView(weights))
    }

  override def transposeMultiply(other: MatrixView): Either[MultivarError, DMat] =
    MatrixView.rightMultiplyView(base, other)

  override def selectColumns(columns: IndexSet): Either[MultivarError, MatrixView] =
    base match
      case sparse: SparseMatrixView =>
        sparse.transposeSelectColumns(columns)
      case dense: DenseMatrixView =>
        MatrixView.requireColumnIndexSet(columns, cols).map { checked =>
          DenseMatrixView.unsafe(dense.value.selectRows(checked.indices).transpose)
        }
      case _ =>
        // Operator-backed bases are never silently densified; callers that can afford
        // materialization must do so explicitly via toDense(StoragePolicy.AllowDense).
        Left(MultivarError.DensificationRejected("transposed column selection", storage))

  override def toDense(policy: StoragePolicy): Either[MultivarError, DMat] =
    base.toDense(policy).map(_.transpose)

  override private[multivar] def rowStats: Either[MultivarError, ColumnStats] =
    base.columnStats

object TransposedMatrixView:
  private[multivar] def unsafe(base: MatrixView): TransposedMatrixView =
    new TransposedMatrixView(base)

object MatrixView:
  def dense(value: DMat): MatrixView =
    DenseMatrixView(value)

  def sparse(value: SparseMatrixView): MatrixView =
    value

  def transpose(base: MatrixView): MatrixView =
    base match
      case transposed: TransposedMatrixView => transposed.base
      case _                                => TransposedMatrixView.unsafe(base)

  private[multivar] def rightMultiplyView(left: MatrixView, right: MatrixView): Either[MultivarError, DMat] =
    requireWeightRows(left.cols, right.rows).flatMap { _ =>
      (left, right) match
        case (_, dense: DenseMatrixView) =>
          left.rightMultiply(dense.value)
        case (sparse: SparseMatrixView, weights: SparseMatrixView) =>
          sparse.rightMultiplySparse(weights)
        case (dense: DenseMatrixView, weights: SparseMatrixView) =>
          Right(weights.leftMultiplyDense(dense.value))
        case (sparse: SparseMatrixView, transposed: TransposedMatrixView) =>
          transposed.base match
            case weights: SparseMatrixView =>
              sparse.rightMultiplySparseTranspose(weights)
            case weights: DenseMatrixView =>
              sparse.rightMultiply(weights.value.transpose)
            case _ =>
              transposed.toDense(StoragePolicy.AllowDense).flatMap(sparse.rightMultiply)
        case (dense: DenseMatrixView, transposed: TransposedMatrixView) =>
          transposed.base match
            case weights: SparseMatrixView =>
              Right(weights.leftMultiplyDenseTranspose(dense.value))
            case weights: DenseMatrixView =>
              Right(GaleNumerics.multiply(dense.value, weights.value.transpose))
            case _ =>
              transposed.toDense(StoragePolicy.AllowDense).flatMap(dense.rightMultiply)
        case _ =>
          right.toDense(StoragePolicy.AllowDense).flatMap(left.rightMultiply)
    }

  def affine(
      base: MatrixView,
      scale: DVec,
      shift: DVec,
      policy: StoragePolicy = StoragePolicy.Operator,
      operation: String = "affine transform"
  ): Either[MultivarError, MatrixView] =
    for
      _ <- requireVectorLength("affine scale", scale, base.cols)
      _ <- requireVectorLength("affine shift", shift, base.cols)
      _ <- requireFinite("affine scale", scale)
      _ <- requireFinite("affine shift", shift)
      out <- base match
        case affineBase: AffineMatrixView =>
          affine(
            affineBase.base,
            multiply(affineBase.scale, scale),
            add(multiply(affineBase.shift, scale), shift),
            policy,
            operation
          )
        case sparse: SparseMatrixView if isZero(shift) =>
          sparse.scaleColumns(scale)
        case sparse: SparseMatrixView =>
          policy match
            case StoragePolicy.PreserveSparse =>
              Left(MultivarError.DensificationRejected(operation, sparse.storage))
            case StoragePolicy.AllowDense =>
              sparse.toDense(StoragePolicy.AllowDense).map { denseBase =>
                DenseMatrixView(materializeAffine(denseBase, scale, shift))
              }
            case StoragePolicy.Operator =>
              Right(AffineMatrixView.unsafe(base, scale, shift))
        case dense: DenseMatrixView =>
          Right(DenseMatrixView(materializeAffine(dense.value, scale, shift)))
        case _ =>
          if policy == StoragePolicy.AllowDense then
            base.toDense(StoragePolicy.AllowDense).map(dense => DenseMatrixView(materializeAffine(dense, scale, shift)))
          else Right(AffineMatrixView.unsafe(base, scale, shift))
    yield out

  /** Relative epsilon below which a column's sample standard deviation is treated as
    * numerically zero (degenerate) with respect to the magnitude of the column mean.
    */
  private[multivar] val DegenerateScaleEpsilon: Double = 1e-12

  private[multivar] def requireWeightRows(expected: Int, actual: Int): Either[MultivarError, Unit] =
    if actual == expected then Right(())
    else
      Left(
        MultivarError.MatrixShapeMismatch(
          s"right multiply expected $expected weight rows, got $actual"
        )
      )

  private[multivar] def requireSharedRows(expected: Int, actual: Int): Either[MultivarError, Unit] =
    if actual == expected then Right(())
    else
      Left(
        MultivarError.MatrixShapeMismatch(
          s"transpose multiply expected $expected rows on both inputs, got $actual"
        )
      )

  /** Global-index-to-local-position lookup for a validated index set: `lookup(global)`
    * is the local position, or -1 when the global index is not selected.
    */
  private[multivar] def positionLookup(columns: IndexSet, size: Int): Array[Int] =
    val out = Array.fill(size)(-1)
    val indices = columns.indices
    var i = 0
    while i < indices.length do
      out(indices(i)) = i
      i += 1
    out

  private[multivar] def requireColumnAxis(columns: IndexSet): Either[MultivarError, IndexSet] =
    if columns.axis != IndexAxis.Column && columns.axis != IndexAxis.Feature then
      Left(MultivarError.InvalidBlockPartition("matrix columns must be selected with column or feature indices"))
    else Right(columns)

  private[multivar] def requireColumnIndexSet(columns: IndexSet, limit: Int): Either[MultivarError, IndexSet] =
    requireColumnAxis(columns).flatMap(_.requireWithin(limit))

  private[multivar] def requireVectorLength(
      role: String,
      vector: DVec,
      expected: Int
  ): Either[MultivarError, Unit] =
    if vector.length == expected then Right(())
    else Left(MultivarError.MatrixShapeMismatch(s"$role length ${vector.length} != expected $expected"))

  private[multivar] def requireFinite(role: String, vector: DVec): Either[MultivarError, Unit] =
    var i = 0
    var error = Option.empty[MultivarError]
    while i < vector.length && error.isEmpty do
      val value = vector(i)
      if !value.isFinite then error = Some(MultivarError.NonFiniteValue(role, i, value))
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(())

  private[multivar] def ones(length: Int): DVec =
    val out = Array.fill(length)(1.0)
    GaleNumerics.vectorFromArray(out)

  private[multivar] def zeros(length: Int): DVec =
    GaleNumerics.vectorFromArray(new Array[Double](length))

  private[multivar] def selectVector(values: DVec, columns: IndexSet): DVec =
    val out = new Array[Double](columns.length)
    var i = 0
    val indices = columns.indices
    while i < indices.length do
      out(i) = values(indices(i))
      i += 1
    GaleNumerics.vectorFromArray(out)

  private[multivar] def negate(values: DVec): DVec =
    val out = new Array[Double](values.length)
    var i = 0
    while i < values.length do
      out(i) = -values(i)
      i += 1
    GaleNumerics.vectorFromArray(out)

  /** Reciprocal of every entry; fails when an entry is non-finite or its reciprocal
    * is not representable (zero and subnormal values), independent of data units.
    */
  private[multivar] def invert(values: DVec): Either[MultivarError, DVec] =
    val out = new Array[Double](values.length)
    var i = 0
    var error = Option.empty[MultivarError]
    while i < values.length && error.isEmpty do
      val value = values(i)
      if !value.isFinite then error = Some(MultivarError.NonFiniteValue("affine inverse scale", i, value))
      else
        val inverse = 1.0 / value
        if !inverse.isFinite then error = Some(MultivarError.NonInvertibleValue("affine inverse scale", i, value))
        else out(i) = inverse
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(GaleNumerics.vectorFromArray(out))

  private[multivar] def multiply(left: DVec, right: DVec): DVec =
    require(left.length == right.length, "vector lengths must match")
    val out = new Array[Double](left.length)
    var i = 0
    while i < out.length do
      out(i) = left(i) * right(i)
      i += 1
    GaleNumerics.vectorFromArray(out)

  private[multivar] def add(left: DVec, right: DVec): DVec =
    require(left.length == right.length, "vector lengths must match")
    val out = new Array[Double](left.length)
    var i = 0
    while i < out.length do
      out(i) = left(i) + right(i)
      i += 1
    GaleNumerics.vectorFromArray(out)

  /** True only for exact zeros: tiny nonzero shifts must be applied on every
    * representation so dense and sparse paths stay bitwise-consistent.
    */
  private[multivar] def isZero(values: DVec): Boolean =
    var i = 0
    var zero = true
    while i < values.length && zero do
      zero = values(i) == 0.0
      i += 1
    zero

  private[multivar] def materializeAffine(
      base: DMat,
      scale: DVec,
      shift: DVec
  ): DMat =
    val out = new Array[Double](base.rows * base.cols)
    var row = 0
    while row < base.rows do
      var col = 0
      while col < base.cols do
        out(row * base.cols + col) = base(row, col) * scale(col) + shift(col)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(base.rows, base.cols, out)

  private[multivar] def scaleRows(matrix: DMat, scale: DVec): DMat =
    require(matrix.rows == scale.length, "scale length must match matrix rows")
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) *= scale(row)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

  private[multivar] def scaleRowsAndColumns(
      matrix: DMat,
      rowScale: DVec,
      colScale: DVec
  ): DMat =
    require(matrix.rows == rowScale.length, "row scale length must match matrix rows")
    require(matrix.cols == colScale.length, "column scale length must match matrix columns")
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) *= rowScale(row) * colScale(col)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

  private[multivar] def vectorTransposeMultiply(vector: DVec, matrix: DMat): DVec =
    require(vector.length == matrix.rows, "vector length must match matrix rows")
    val out = new Array[Double](matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      val value = vector(row)
      while col < matrix.cols do
        out(col) += value * matrix(row, col)
        col += 1
      row += 1
    GaleNumerics.vectorFromArray(out)

  private[multivar] def addRowVector(matrix: DMat, rowVector: DVec): DMat =
    require(matrix.cols == rowVector.length, "row vector length must match matrix columns")
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) += rowVector(col)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

  private[multivar] def addOuterProductInPlace(
      matrix: Array[Double],
      rows: Int,
      cols: Int,
      left: DVec,
      right: DVec,
      factor: Double = 1.0
  ): Unit =
    require(matrix.length == rows * cols, "matrix data length must match its shape")
    require(rows == left.length, "left vector length must match matrix rows")
    require(cols == right.length, "right vector length must match matrix columns")
    var row = 0
    while row < rows do
      var col = 0
      while col < cols do
        matrix(row * cols + col) += factor * left(row) * right(col)
        col += 1
      row += 1
