package scalafim.linalg

import scala.collection.mutable.ArrayBuffer

enum LinearMapError:
  case NegativeDimension(rows: Int, cols: Int)
  case MismatchedTripletLengths(rows: Int, cols: Int, values: Int)
  case IndexOutOfBounds(axis: String, index: Int, limit: Int)
  case NonFiniteValue(index: Int, value: Double)
  case DimensionMismatch(expectedRows: Int, actualRows: Int)
  case NonComposable(leftRows: Int, rightCols: Int)
  case OperatorApplicationFailed(detail: String)
  case InvalidBlockLayout(detail: String)
  case EmptyOperatorList
  case EmptySelection(axis: String)
  case DuplicateSelection(axis: String, index: Int)

  def message: String =
    this match
      case NegativeDimension(rows, cols) =>
        s"operator dimensions must be non-negative, got ${rows}x${cols}"
      case MismatchedTripletLengths(rows, cols, values) =>
        s"triplet arrays must have equal length, got rows=$rows cols=$cols values=$values"
      case IndexOutOfBounds(axis, index, limit) =>
        s"$axis index $index out of bounds for size $limit"
      case NonFiniteValue(index, value) =>
        s"triplet value at $index is not finite: $value"
      case DimensionMismatch(expectedRows, actualRows) =>
        s"operator expected $expectedRows input rows, got $actualRows"
      case NonComposable(leftRows, rightCols) =>
        s"operators are not composable: first target dimension $leftRows != second source dimension $rightCols"
      case OperatorApplicationFailed(detail) =>
        s"linear operator application failed: $detail"
      case InvalidBlockLayout(detail) =>
        s"invalid block operator layout: $detail"
      case EmptyOperatorList =>
        "operator list must be non-empty"
      case EmptySelection(axis) =>
        s"$axis selection must be non-empty when provided"
      case DuplicateSelection(axis, index) =>
        s"$axis selection contains duplicate index $index"

trait LinearMap:
  def rows: Int
  def cols: Int

  def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix]

  def forward(input: DoubleVector): Either[LinearMapError, DoubleVector] =
    if input.length != cols then Left(LinearMapError.DimensionMismatch(cols, input.length))
    else
      val matrix = DoubleMatrix.unsafe(input.length, 1, input.copyData)
      forward(matrix).map(out => DoubleVector.unsafe(out.copyData))

  def adjoint: LinearMap

final class SparseTriplets private (
    val rows: Int,
    val cols: Int,
    private[scalafim] val rowArray: Array[Int],
    private[scalafim] val colArray: Array[Int],
    private[scalafim] val valueArray: Array[Double]
):
  def nnz: Int =
    valueArray.length

  def rowIndices: Array[Int] =
    rowArray.clone

  def colIndices: Array[Int] =
    colArray.clone

  def values: Array[Double] =
    valueArray.clone

object SparseTriplets:
  def apply(
    rows: Int,
    cols: Int,
    rowIndices: Array[Int],
    colIndices: Array[Int],
    values: Array[Double]
  ): Either[LinearMapError, SparseTriplets] =
    validate(rows, cols, rowIndices, colIndices, values).map { _ =>
      unsafe(rows, cols, rowIndices.clone, colIndices.clone, values.clone)
    }

  def fromSeq(
    rows: Int,
    cols: Int,
    entries: Seq[(Int, Int, Double)]
  ): Either[LinearMapError, SparseTriplets] =
    val rowIndices = Array.ofDim[Int](entries.length)
    val colIndices = Array.ofDim[Int](entries.length)
    val values = Array.ofDim[Double](entries.length)
    var i = 0
    entries.foreach { case (row, col, value) =>
      rowIndices(i) = row
      colIndices(i) = col
      values(i) = value
      i += 1
    }
    apply(rows, cols, rowIndices, colIndices, values)

  private[scalafim] def unsafe(
    rows: Int,
    cols: Int,
    rowIndices: Array[Int],
    colIndices: Array[Int],
    values: Array[Double]
  ): SparseTriplets =
    new SparseTriplets(rows, cols, rowIndices, colIndices, values)

  private def validate(
    rows: Int,
    cols: Int,
    rowIndices: Array[Int],
    colIndices: Array[Int],
    values: Array[Double]
  ): Either[LinearMapError, Unit] =
    if rows < 0 || cols < 0 then Left(LinearMapError.NegativeDimension(rows, cols))
    else if rowIndices.length != colIndices.length || rowIndices.length != values.length then
      Left(LinearMapError.MismatchedTripletLengths(rowIndices.length, colIndices.length, values.length))
    else
      var i = 0
      var error = Option.empty[LinearMapError]
      while i < values.length && error.isEmpty do
        val row = rowIndices(i)
        val col = colIndices(i)
        val value = values(i)
        if row < 0 || row >= rows then error = Some(LinearMapError.IndexOutOfBounds("row", row, rows))
        else if col < 0 || col >= cols then error = Some(LinearMapError.IndexOutOfBounds("col", col, cols))
        else if !value.isFinite then error = Some(LinearMapError.NonFiniteValue(i, value))
        i += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(())

final class CsrMatrix private (
    val rows: Int,
    val cols: Int,
    private val rowPtr: Array[Int],
    private val colIndex: Array[Int],
    private val data: Array[Double]
) extends LinearMap:
  def nnz: Int =
    data.length

  override def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    if input.rows != cols then Left(LinearMapError.DimensionMismatch(cols, input.rows))
    else
      val out = new Array[Double](rows * input.cols)
      var row = 0
      while row < rows do
        var p = rowPtr(row)
        val end = rowPtr(row + 1)
        while p < end do
          val sourceRow = colIndex(p)
          val weight = data(p)
          val sourceOffset = sourceRow * input.cols
          val outOffset = row * input.cols
          var col = 0
          while col < input.cols do
            out(outOffset + col) += weight * input.dataArray(sourceOffset + col)
            col += 1
          p += 1
        row += 1
      Right(DoubleMatrix.unsafe(rows, input.cols, out))

  override def forward(input: DoubleVector): Either[LinearMapError, DoubleVector] =
    if input.length != cols then Left(LinearMapError.DimensionMismatch(cols, input.length))
    else
      val out = new Array[Double](rows)
      var row = 0
      while row < rows do
        var sum = 0.0
        var p = rowPtr(row)
        val end = rowPtr(row + 1)
        while p < end do
          sum += data(p) * input.dataArray(colIndex(p))
          p += 1
        out(row) = sum
        row += 1
      Right(DoubleVector.unsafe(out))

  override def adjoint: LinearMap =
    CsrMatrix.fromTriplets(toTriplets.transpose).toOption.get

  def toTriplets: SparseTriplets =
    val rowIndices = Array.ofDim[Int](nnz)
    val colIndices = Array.ofDim[Int](nnz)
    val values = data.clone
    var row = 0
    while row < rows do
      var p = rowPtr(row)
      val end = rowPtr(row + 1)
      while p < end do
        rowIndices(p) = row
        colIndices(p) = colIndex(p)
        p += 1
      row += 1
    SparseTriplets.unsafe(rows, cols, rowIndices, colIndices, values)

object CsrMatrix:
  def fromTriplets(triplets: SparseTriplets): Either[LinearMapError, CsrMatrix] =
    fromTriplets(
      triplets.rows,
      triplets.cols,
      triplets.rowArray,
      triplets.colArray,
      triplets.valueArray
    )

  def fromTriplets(
    rows: Int,
    cols: Int,
    rowIndices: Array[Int],
    colIndices: Array[Int],
    values: Array[Double]
  ): Either[LinearMapError, CsrMatrix] =
    SparseTriplets(rows, cols, rowIndices, colIndices, values).map { triplets =>
      val order = Array.tabulate(triplets.nnz)(i => i)
      scala.util.Sorting.stableSort(
        order,
        (left: Int, right: Int) =>
          val rowLeft = triplets.rowArray(left)
          val rowRight = triplets.rowArray(right)
          rowLeft < rowRight || (rowLeft == rowRight && triplets.colArray(left) < triplets.colArray(right))
      )

      val outRows = ArrayBuffer.empty[Int]
      val outCols = ArrayBuffer.empty[Int]
      val outVals = ArrayBuffer.empty[Double]

      var k = 0
      while k < order.length do
        val first = order(k)
        val row = triplets.rowArray(first)
        val col = triplets.colArray(first)
        var value = triplets.valueArray(first)
        k += 1
        while k < order.length &&
            triplets.rowArray(order(k)) == row &&
            triplets.colArray(order(k)) == col
        do
          value += triplets.valueArray(order(k))
          k += 1
        if value != 0.0 then
          outRows += row
          outCols += col
          outVals += value

      val rowPtr = Array.ofDim[Int](rows + 1)
      var i = 0
      while i < outRows.length do
        rowPtr(outRows(i) + 1) += 1
        i += 1

      var row = 0
      while row < rows do
        rowPtr(row + 1) += rowPtr(row)
        row += 1

      Right(new CsrMatrix(rows, cols, rowPtr, outCols.toArray, outVals.toArray))
    }.flatten

  def identity(size: Int): Either[LinearMapError, CsrMatrix] =
    if size < 0 then Left(LinearMapError.NegativeDimension(size, size))
    else
      val idx = Array.tabulate(size)(i => i)
      val values = Array.fill(size)(1.0)
      fromTriplets(size, size, idx, idx, values)

extension (triplets: SparseTriplets)
  def transpose: SparseTriplets =
    SparseTriplets.unsafe(
      rows = triplets.cols,
      cols = triplets.rows,
      rowIndices = triplets.colArray.clone,
      colIndices = triplets.rowArray.clone,
      values = triplets.valueArray.clone
    )

final class ComposedLinearMap private[scalafim] (first: LinearMap, second: LinearMap) extends LinearMap:
  override val rows: Int = second.rows
  override val cols: Int = first.cols

  override def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    first.forward(input).flatMap(second.forward)

  override def adjoint: LinearMap =
    new ComposedLinearMap(second.adjoint, first.adjoint)

final class ScaledLinearMap private[scalafim] (base: LinearMap, factor: Double) extends LinearMap:
  override def rows: Int = base.rows
  override def cols: Int = base.cols

  override def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    base.forward(input).map { result =>
      val out = result.copyData
      var index = 0
      while index < out.length do
        out(index) *= factor
        index += 1
      DoubleMatrix.unsafe(result.rows, result.cols, out)
    }

  override def adjoint: LinearMap =
    new ScaledLinearMap(base.adjoint, factor)

final class RestrictedLinearMap private[scalafim] (
    base: LinearMap,
    targetRows: Option[Array[Int]],
    sourceCols: Option[Array[Int]]
) extends LinearMap:
  override val rows: Int =
    targetRows.fold(base.rows)(_.length)

  override val cols: Int =
    sourceCols.fold(base.cols)(_.length)

  override def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    if input.rows != cols then Left(LinearMapError.DimensionMismatch(cols, input.rows))
    else
      val prepared =
        sourceCols match
          case None =>
            input
          case Some(source) =>
            val full = new Array[Double](base.cols * input.cols)
            var selectedRow = 0
            while selectedRow < source.length do
              val fullRow = source(selectedRow)
              System.arraycopy(input.dataArray, selectedRow * input.cols, full, fullRow * input.cols, input.cols)
              selectedRow += 1
            DoubleMatrix.unsafe(base.cols, input.cols, full)

      base.forward(prepared).map { fullOut =>
        targetRows match
          case None =>
            fullOut
          case Some(target) =>
            val out = new Array[Double](target.length * fullOut.cols)
            var outRow = 0
            while outRow < target.length do
              System.arraycopy(fullOut.dataArray, target(outRow) * fullOut.cols, out, outRow * fullOut.cols, fullOut.cols)
              outRow += 1
            DoubleMatrix.unsafe(target.length, fullOut.cols, out)
      }

  override def adjoint: LinearMap =
    new RestrictedLinearMap(base.adjoint, sourceCols, targetRows)

final class BlockDiagonalLinearMap private[scalafim] (operators: Vector[LinearMap]) extends LinearMap:
  override val rows: Int =
    operators.map(_.rows).sum

  override val cols: Int =
    operators.map(_.cols).sum

  override def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    if input.rows != cols then Left(LinearMapError.DimensionMismatch(cols, input.rows))
    else
      val out = new Array[Double](rows * input.cols)
      var sourceOffset = 0
      var targetOffset = 0
      var opIndex = 0
      var error = Option.empty[LinearMapError]
      while opIndex < operators.length && error.isEmpty do
        val op = operators(opIndex)
        val partInput = new Array[Double](op.cols * input.cols)
        var row = 0
        while row < op.cols do
          System.arraycopy(input.dataArray, (sourceOffset + row) * input.cols, partInput, row * input.cols, input.cols)
          row += 1
        op.forward(DoubleMatrix.unsafe(op.cols, input.cols, partInput)) match
          case Left(value) =>
            error = Some(value)
          case Right(partOutput) =>
            row = 0
            while row < op.rows do
              System.arraycopy(partOutput.dataArray, row * input.cols, out, (targetOffset + row) * input.cols, input.cols)
              row += 1
        sourceOffset += op.cols
        targetOffset += op.rows
        opIndex += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(DoubleMatrix.unsafe(rows, input.cols, out))

  override def adjoint: LinearMap =
    new BlockDiagonalLinearMap(operators.map(_.adjoint))

final case class LinearMapBlock(rowBlock: Int, columnBlock: Int, operator: LinearMap)

/** A structured rectangular block operator. Missing blocks are exact zeros; duplicate
  * block coordinates are summed. Application slices each source block once per edge
  * and never materializes the full operator.
  */
final class BlockLinearMap private[scalafim] (
    val rowBlockSizes: Vector[Int],
    val columnBlockSizes: Vector[Int],
    val blocks: Vector[LinearMapBlock]
) extends LinearMap:
  override val rows: Int =
    rowBlockSizes.sum

  override val cols: Int =
    columnBlockSizes.sum

  override def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    if input.rows != cols then Left(LinearMapError.DimensionMismatch(cols, input.rows))
    else
      val rowOffsets = BlockLinearMap.offsets(rowBlockSizes)
      val columnOffsets = BlockLinearMap.offsets(columnBlockSizes)
      val out = new Array[Double](rows * input.cols)
      var blockIndex = 0
      var error = Option.empty[LinearMapError]
      while blockIndex < blocks.length && error.isEmpty do
        val block = blocks(blockIndex)
        val sourceRows = columnBlockSizes(block.columnBlock)
        val source = new Array[Double](sourceRows * input.cols)
        var sourceRow = 0
        while sourceRow < sourceRows do
          System.arraycopy(
            input.dataArray,
            (columnOffsets(block.columnBlock) + sourceRow) * input.cols,
            source,
            sourceRow * input.cols,
            input.cols
          )
          sourceRow += 1
        block.operator.forward(DoubleMatrix.unsafe(sourceRows, input.cols, source)) match
          case Left(value) => error = Some(value)
          case Right(part) =>
            var targetRow = 0
            while targetRow < part.rows do
              var col = 0
              val outOffset = (rowOffsets(block.rowBlock) + targetRow) * input.cols
              val partOffset = targetRow * input.cols
              while col < input.cols do
                out(outOffset + col) += part.dataArray(partOffset + col)
                col += 1
              targetRow += 1
        blockIndex += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(DoubleMatrix.unsafe(rows, input.cols, out))

  override def adjoint: LinearMap =
    new BlockLinearMap(
      columnBlockSizes,
      rowBlockSizes,
      blocks.map(block => LinearMapBlock(block.columnBlock, block.rowBlock, block.operator.adjoint))
    )

object BlockLinearMap:
  private[scalafim] def offsets(sizes: Vector[Int]): Array[Int] =
    val out = new Array[Int](sizes.length)
    var index = 1
    while index < sizes.length do
      out(index) = out(index - 1) + sizes(index - 1)
      index += 1
    out

object LinearMap:
  def compose(first: LinearMap, second: LinearMap): Either[LinearMapError, LinearMap] =
    if first.rows != second.cols then Left(LinearMapError.NonComposable(first.rows, second.cols))
    else Right(new ComposedLinearMap(first, second))

  def scale(map: LinearMap, factor: Double): Either[LinearMapError, LinearMap] =
    if !factor.isFinite then Left(LinearMapError.NonFiniteValue(0, factor))
    else Right(new ScaledLinearMap(map, factor))

  def restrict(
    map: LinearMap,
    targetRows: Option[IndexedSeq[Int]] = None,
    sourceCols: Option[IndexedSeq[Int]] = None
  ): Either[LinearMapError, LinearMap] =
    for
      target <- validateSelection("target", targetRows, map.rows)
      source <- validateSelection("source", sourceCols, map.cols)
    yield new RestrictedLinearMap(map, target, source)

  def blockDiag(operators: Vector[LinearMap]): Either[LinearMapError, LinearMap] =
    if operators.isEmpty then Left(LinearMapError.EmptyOperatorList)
    else Right(new BlockDiagonalLinearMap(operators))

  def blockMatrix(
      rowBlockSizes: Vector[Int],
      columnBlockSizes: Vector[Int],
      blocks: Vector[LinearMapBlock]
  ): Either[LinearMapError, LinearMap] =
    if rowBlockSizes.isEmpty || columnBlockSizes.isEmpty then
      Left(LinearMapError.InvalidBlockLayout("row and column block partitions must be non-empty"))
    else if rowBlockSizes.exists(_ <= 0) || columnBlockSizes.exists(_ <= 0) then
      Left(LinearMapError.InvalidBlockLayout("every row and column block must have positive size"))
    else
      var index = 0
      var error = Option.empty[LinearMapError]
      while index < blocks.length && error.isEmpty do
        val block = blocks(index)
        if block.rowBlock < 0 || block.rowBlock >= rowBlockSizes.length then
          error = Some(LinearMapError.IndexOutOfBounds("row block", block.rowBlock, rowBlockSizes.length))
        else if block.columnBlock < 0 || block.columnBlock >= columnBlockSizes.length then
          error = Some(LinearMapError.IndexOutOfBounds("column block", block.columnBlock, columnBlockSizes.length))
        else if block.operator.rows != rowBlockSizes(block.rowBlock) ||
            block.operator.cols != columnBlockSizes(block.columnBlock)
        then
          error = Some(
            LinearMapError.InvalidBlockLayout(
              s"block (${block.rowBlock}, ${block.columnBlock}) has ${block.operator.rows}x${block.operator.cols}, " +
                s"expected ${rowBlockSizes(block.rowBlock)}x${columnBlockSizes(block.columnBlock)}"
            )
          )
        index += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(new BlockLinearMap(rowBlockSizes, columnBlockSizes, blocks))

  private def validateSelection(
    axis: String,
    selection: Option[IndexedSeq[Int]],
    limit: Int
  ): Either[LinearMapError, Option[Array[Int]]] =
    selection match
      case None =>
        Right(None)
      case Some(indices) =>
        if indices.isEmpty then Left(LinearMapError.EmptySelection(axis))
        else
          val seen = scala.collection.mutable.HashSet.empty[Int]
          val out = Array.ofDim[Int](indices.length)
          var i = 0
          var error = Option.empty[LinearMapError]
          while i < indices.length && error.isEmpty do
            val index = indices(i)
            if index < 0 || index >= limit then error = Some(LinearMapError.IndexOutOfBounds(axis, index, limit))
            else if seen.contains(index) then error = Some(LinearMapError.DuplicateSelection(axis, index))
            else
              seen += index
              out(i) = index
            i += 1
          error match
            case Some(value) => Left(value)
            case None        => Right(Some(out))
