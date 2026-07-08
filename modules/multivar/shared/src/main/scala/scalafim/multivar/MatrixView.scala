package scalafim.multivar

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

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

final case class ColumnStats(
    count: Int,
    sums: DoubleVector,
    sumSquares: DoubleVector
):
  require(count >= 0, "column-stat count must be non-negative")
  require(sums.length == sumSquares.length, "column-stat vectors must have equal length")

  def cols: Int =
    sums.length

  def means: Either[MultivarError, DoubleVector] =
    if count <= 0 then Left(MultivarError.InvalidDimension("row count for column means", count))
    else
      val out = new Array[Double](cols)
      var col = 0
      while col < cols do
        out(col) = sums(col) / count
        col += 1
      Right(DoubleVector.unsafe(out))

  def sampleStandardDeviations: Either[MultivarError, DoubleVector] =
    if count <= 1 then Left(MultivarError.InvalidDimension("row count for sample standard deviations", count))
    else
      val out = new Array[Double](cols)
      var col = 0
      while col < cols do
        val mean = sums(col) / count
        val ss = sumSquares(col) - count * mean * mean
        out(col) = Math.sqrt(Math.max(ss / (count - 1), 0.0))
        col += 1
      Right(DoubleVector.unsafe(out))

object ColumnStats:
  def fromDense(matrix: DoubleMatrix): Either[MultivarError, ColumnStats] =
    val sums = new Array[Double](matrix.cols)
    val sumSquares = new Array[Double](matrix.cols)
    var row = 0
    var error = Option.empty[MultivarError]
    while row < matrix.rows && error.isEmpty do
      val offset = row * matrix.cols
      var col = 0
      while col < matrix.cols && error.isEmpty do
        val value = matrix.dataArray(offset + col)
        if !value.isFinite then error = Some(MultivarError.NonFiniteValue("matrix", offset + col, value))
        else
          sums(col) += value
          sumSquares(col) += value * value
        col += 1
      row += 1

    error match
      case Some(value) => Left(value)
      case None =>
        Right(
          ColumnStats(
            count = matrix.rows,
            sums = DoubleVector.unsafe(sums),
            sumSquares = DoubleVector.unsafe(sumSquares)
          )
        )

trait MatrixView:
  def rows: Int
  def cols: Int
  def storage: StorageKind

  def columnStats: Either[MultivarError, ColumnStats]

  def rightMultiply(weights: DoubleMatrix): Either[MultivarError, DoubleMatrix]

  def transposeMultiply(other: MatrixView): Either[MultivarError, DoubleMatrix]

  def crossProduct: Either[MultivarError, DoubleMatrix] =
    transposeMultiply(this)

  def selectColumns(columns: IndexSet): Either[MultivarError, MatrixView]

  def toDense(policy: StoragePolicy = StoragePolicy.AllowDense): Either[MultivarError, DoubleMatrix]

final class DenseMatrixView private (val value: DoubleMatrix) extends MatrixView:
  override def rows: Int =
    value.rows

  override def cols: Int =
    value.cols

  override def storage: StorageKind =
    StorageKind.Dense

  override def columnStats: Either[MultivarError, ColumnStats] =
    ColumnStats.fromDense(value)

  override def rightMultiply(weights: DoubleMatrix): Either[MultivarError, DoubleMatrix] =
    if weights.rows != cols then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"right multiply expected ${cols} weight rows, got ${weights.rows}"
        )
      )
    else Right(DoubleMatrix.multiply(value, weights))

  override def transposeMultiply(other: MatrixView): Either[MultivarError, DoubleMatrix] =
    if other.rows != rows then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"transpose multiply expected ${rows} rows on both inputs, got ${other.rows}"
        )
      )
    else other.toDense().map(denseOther => DoubleMatrix.transposeMultiply(value, denseOther))

  override def selectColumns(columns: IndexSet): Either[MultivarError, MatrixView] =
    MatrixView.requireColumnIndexSet(columns, cols).map { checked =>
      val indices = checked.indices
      val out = new Array[Double](rows * indices.length)
      var row = 0
      while row < rows do
        var col = 0
        while col < indices.length do
          out(row * indices.length + col) = value.dataArray(row * cols + indices(col))
          col += 1
        row += 1
      DenseMatrixView.unsafe(DoubleMatrix.unsafe(rows, indices.length, out))
    }

  override def toDense(policy: StoragePolicy): Either[MultivarError, DoubleMatrix] =
    Right(value)

  override def equals(other: Any): Boolean =
    other match
      case that: DenseMatrixView => value.toRows == that.value.toRows
      case _                     => false

  override def hashCode(): Int =
    value.toRows.hashCode

object DenseMatrixView:
  def apply(value: DoubleMatrix): DenseMatrixView =
    new DenseMatrixView(value)

  private[scalafim] def unsafe(value: DoubleMatrix): DenseMatrixView =
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
    var p = 0
    var error = Option.empty[MultivarError]
    while p < data.length && error.isEmpty do
      val value = data(p)
      if !value.isFinite then error = Some(MultivarError.NonFiniteValue("sparse matrix", p, value))
      else
        val col = colIndex(p)
        sums(col) += value
        sumSquares(col) += value * value
      p += 1

    error match
      case Some(value) => Left(value)
      case None =>
        Right(ColumnStats(rows, DoubleVector.unsafe(sums), DoubleVector.unsafe(sumSquares)))

  override def rightMultiply(weights: DoubleMatrix): Either[MultivarError, DoubleMatrix] =
    if weights.rows != cols then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"right multiply expected ${cols} weight rows, got ${weights.rows}"
        )
      )
    else
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
      Right(DoubleMatrix.unsafe(rows, weights.cols, out))

  override def transposeMultiply(other: MatrixView): Either[MultivarError, DoubleMatrix] =
    if other.rows != rows then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"transpose multiply expected ${rows} rows on both inputs, got ${other.rows}"
        )
      )
    else
      other match
        case dense: DenseMatrixView =>
          Right(transposeMultiplyDense(dense.value))
        case sparse: SparseMatrixView =>
          Right(transposeMultiplySparse(sparse))
        case _ =>
          other.toDense().map(transposeMultiplyDense)

  override def selectColumns(columns: IndexSet): Either[MultivarError, MatrixView] =
    MatrixView.requireColumnIndexSet(columns, cols).flatMap { checked =>
      val positions = mutable.HashMap.empty[Int, Int]
      checked.indices.zipWithIndex.foreach { case (global, local) => positions(global) = local }

      val outRows = ArrayBuffer.empty[Int]
      val outCols = ArrayBuffer.empty[Int]
      val outVals = ArrayBuffer.empty[Double]
      var row = 0
      while row < rows do
        var p = rowPtr(row)
        val end = rowPtr(row + 1)
        while p < end do
          positions.get(colIndex(p)) match
            case Some(localCol) =>
              outRows += row
              outCols += localCol
              outVals += data(p)
            case None =>
          p += 1
        row += 1

      SparseMatrixView.fromTriplets(rows, checked.length, outRows.toArray, outCols.toArray, outVals.toArray)
    }

  override def toDense(policy: StoragePolicy): Either[MultivarError, DoubleMatrix] =
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
        Right(DoubleMatrix.unsafe(rows, cols, out))
      case _ =>
        Left(MultivarError.DensificationRejected("toDense", storage))

  private[multivar] def scaleRows(scale: DoubleVector): Either[MultivarError, SparseMatrixView] =
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

  private[multivar] def scaleColumns(scale: DoubleVector): Either[MultivarError, SparseMatrixView] =
    MatrixView.requireVectorLength("sparse column scale", scale, cols).map { _ =>
      val out = new Array[Double](data.length)
      var p = 0
      while p < data.length do
        out(p) = data(p) * scale(colIndex(p))
        p += 1
      SparseMatrixView.unsafe(rows, cols, rowPtr.clone, colIndex.clone, out)
    }

  private def transposeMultiplyDense(other: DoubleMatrix): DoubleMatrix =
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
    DoubleMatrix.unsafe(cols, other.cols, out)

  private def transposeMultiplySparse(other: SparseMatrixView): DoubleMatrix =
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
    DoubleMatrix.unsafe(cols, other.cols, out)

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
    val scale: DoubleVector,
    val shift: DoubleVector
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
      ColumnStats(rows, DoubleVector.unsafe(sums), DoubleVector.unsafe(sumSquares))
    }

  override def rightMultiply(weights: DoubleMatrix): Either[MultivarError, DoubleMatrix] =
    if weights.rows != cols then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"right multiply expected ${cols} weight rows, got ${weights.rows}"
        )
      )
    else
      val scaledWeights = MatrixView.scaleRows(weights, scale)
      base.rightMultiply(scaledWeights).map { out =>
        val shiftContribution = MatrixView.vectorTransposeMultiply(shift, weights)
        MatrixView.addRowVector(out, shiftContribution)
      }

  override def transposeMultiply(other: MatrixView): Either[MultivarError, DoubleMatrix] =
    if other.rows != rows then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"transpose multiply expected ${rows} rows on both inputs, got ${other.rows}"
        )
      )
    else
      other match
        case affine: AffineMatrixView =>
          for
            baseCross <- base.transposeMultiply(affine.base)
            baseStats <- base.columnStats
            otherStats <- affine.base.columnStats
          yield
            val out = MatrixView.scaleRowsAndColumns(baseCross, scale, affine.scale)
            MatrixView.addOuterProductInPlace(out, MatrixView.multiply(baseStats.sums, scale), affine.shift)
            MatrixView.addOuterProductInPlace(out, shift, MatrixView.multiply(otherStats.sums, affine.scale))
            MatrixView.addOuterProductInPlace(out, shift, affine.shift, factor = rows.toDouble)
            out
        case _ =>
          for
            baseCross <- base.transposeMultiply(other)
            otherStats <- other.columnStats
          yield
            val out = MatrixView.scaleRows(baseCross, scale)
            MatrixView.addOuterProductInPlace(out, shift, otherStats.sums)
            out

  override def selectColumns(columns: IndexSet): Either[MultivarError, MatrixView] =
    for
      checked <- MatrixView.requireColumnIndexSet(columns, cols)
      selectedBase <- base.selectColumns(checked)
      selectedScale = MatrixView.selectVector(scale, checked)
      selectedShift = MatrixView.selectVector(shift, checked)
      out <- MatrixView.affine(selectedBase, selectedScale, selectedShift, StoragePolicy.Operator, "affine column selection")
    yield out

  override def toDense(policy: StoragePolicy): Either[MultivarError, DoubleMatrix] =
    policy match
      case StoragePolicy.AllowDense =>
        base.toDense(StoragePolicy.AllowDense).map(MatrixView.materializeAffine(_, scale, shift))
      case _ =>
        Left(MultivarError.DensificationRejected("toDense", storage))

object AffineMatrixView:
  private[multivar] def unsafe(base: MatrixView, scale: DoubleVector, shift: DoubleVector): AffineMatrixView =
    new AffineMatrixView(base, scale, shift)

object MatrixView:
  def dense(value: DoubleMatrix): MatrixView =
    DenseMatrixView(value)

  def sparse(value: SparseMatrixView): MatrixView =
    value

  def affine(
      base: MatrixView,
      scale: DoubleVector,
      shift: DoubleVector,
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

  private[multivar] def requireColumnIndexSet(columns: IndexSet, limit: Int): Either[MultivarError, IndexSet] =
    if columns.axis != IndexAxis.Column && columns.axis != IndexAxis.Feature then
      Left(MultivarError.InvalidBlockPartition("matrix columns must be selected with column or feature indices"))
    else columns.requireWithin(limit)

  private[multivar] def requireVectorLength(
      role: String,
      vector: DoubleVector,
      expected: Int
  ): Either[MultivarError, Unit] =
    if vector.length == expected then Right(())
    else Left(MultivarError.MatrixShapeMismatch(s"$role length ${vector.length} != expected $expected"))

  private[multivar] def requireFinite(role: String, vector: DoubleVector): Either[MultivarError, Unit] =
    var i = 0
    var error = Option.empty[MultivarError]
    while i < vector.length && error.isEmpty do
      val value = vector(i)
      if !value.isFinite then error = Some(MultivarError.NonFiniteValue(role, i, value))
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(())

  private[multivar] def ones(length: Int): DoubleVector =
    val out = Array.fill(length)(1.0)
    DoubleVector.unsafe(out)

  private[multivar] def zeros(length: Int): DoubleVector =
    DoubleVector.unsafe(new Array[Double](length))

  private[multivar] def selectVector(values: DoubleVector, columns: IndexSet): DoubleVector =
    val out = new Array[Double](columns.length)
    var i = 0
    val indices = columns.indices
    while i < indices.length do
      out(i) = values(indices(i))
      i += 1
    DoubleVector.unsafe(out)

  private[multivar] def negate(values: DoubleVector): DoubleVector =
    val out = new Array[Double](values.length)
    var i = 0
    while i < values.length do
      out(i) = -values(i)
      i += 1
    DoubleVector.unsafe(out)

  private[multivar] def invert(values: DoubleVector): Either[MultivarError, DoubleVector] =
    val out = new Array[Double](values.length)
    var i = 0
    var error = Option.empty[MultivarError]
    while i < values.length && error.isEmpty do
      val value = values(i)
      if !value.isFinite || Math.abs(value) <= 1e-12 then
        error = Some(MultivarError.NonFiniteValue("affine inverse scale", i, value))
      else out(i) = 1.0 / value
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(DoubleVector.unsafe(out))

  private[multivar] def multiply(left: DoubleVector, right: DoubleVector): DoubleVector =
    require(left.length == right.length, "vector lengths must match")
    val out = new Array[Double](left.length)
    var i = 0
    while i < out.length do
      out(i) = left(i) * right(i)
      i += 1
    DoubleVector.unsafe(out)

  private[multivar] def add(left: DoubleVector, right: DoubleVector): DoubleVector =
    require(left.length == right.length, "vector lengths must match")
    val out = new Array[Double](left.length)
    var i = 0
    while i < out.length do
      out(i) = left(i) + right(i)
      i += 1
    DoubleVector.unsafe(out)

  private[multivar] def isZero(values: DoubleVector): Boolean =
    var i = 0
    var zero = true
    while i < values.length && zero do
      zero = Math.abs(values(i)) <= 1e-12
      i += 1
    zero

  private[multivar] def materializeAffine(
      base: DoubleMatrix,
      scale: DoubleVector,
      shift: DoubleVector
  ): DoubleMatrix =
    val out = new Array[Double](base.rows * base.cols)
    var row = 0
    while row < base.rows do
      var col = 0
      while col < base.cols do
        out(row * base.cols + col) = base(row, col) * scale(col) + shift(col)
        col += 1
      row += 1
    DoubleMatrix.unsafe(base.rows, base.cols, out)

  private[multivar] def scaleRows(matrix: DoubleMatrix, scale: DoubleVector): DoubleMatrix =
    require(matrix.rows == scale.length, "scale length must match matrix rows")
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) *= scale(row)
        col += 1
      row += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  private[multivar] def scaleRowsAndColumns(
      matrix: DoubleMatrix,
      rowScale: DoubleVector,
      colScale: DoubleVector
  ): DoubleMatrix =
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
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  private[multivar] def vectorTransposeMultiply(vector: DoubleVector, matrix: DoubleMatrix): DoubleVector =
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
    DoubleVector.unsafe(out)

  private[multivar] def addRowVector(matrix: DoubleMatrix, rowVector: DoubleVector): DoubleMatrix =
    require(matrix.cols == rowVector.length, "row vector length must match matrix columns")
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) += rowVector(col)
        col += 1
      row += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  private[multivar] def addOuterProductInPlace(
      matrix: DoubleMatrix,
      left: DoubleVector,
      right: DoubleVector,
      factor: Double = 1.0
  ): Unit =
    require(matrix.rows == left.length, "left vector length must match matrix rows")
    require(matrix.cols == right.length, "right vector length must match matrix columns")
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        matrix.dataArray(row * matrix.cols + col) += factor * left(row) * right(col)
        col += 1
      row += 1
