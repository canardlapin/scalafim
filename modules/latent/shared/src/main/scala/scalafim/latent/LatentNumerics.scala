package scalafim.latent

import gale.backend.Backend.given
import gale.linalg.CholeskyOptions
import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.linalg.LinAlgError
import gale.linalg.LinearOperator
import gale.linalg.Matrix
import gale.linalg.Shape
import gale.linalg.Rows
import gale.linalg.Cols
import gale.linalg.Vec
import gale.sparse.CSR
import gale.sparse.Sparse

/** Allocation-aware Gale construction and the domain-specific normal-equation
  * solve used by latent basis projection.
  */
private[latent] object LatentNumerics:
  def matrixFromRowMajor(rows: Int, cols: Int, values: Array[Double]): DMat =
    require(values.length == rows * cols, "row-major data length must match matrix shape")
    val out = Matrix.newBuilder(rows, cols)
    var index = 0
    while index < values.length do
      out.updateRowMajor(index, values(index))
      index += 1
    out.result()

  def matrixFromRows(rows: Seq[Seq[Double]]): DMat =
    val rowCount = rows.length
    val colCount = rows.headOption.fold(0)(_.length)
    require(rows.forall(_.length == colCount), "matrix rows must have equal lengths")
    val out = Matrix.newBuilder(rowCount, colCount)
    var row = 0
    rows.foreach { values =>
      var col = 0
      values.foreach { value =>
        out(row, col) = value
        col += 1
      }
      row += 1
    }
    out.result()

  def vectorFromArray(values: Array[Double]): DVec =
    val out = Vec.newBuilder(values.length)
    var index = 0
    while index < values.length do
      out(index) = values(index)
      index += 1
    out.result()

  def multiply(left: DMat, right: DMat): DMat =
    left * right

  def crossProduct(matrix: DMat): DMat =
    matrix.t * matrix

  def transposeMultiply(left: DMat, right: DMat): DMat =
    left.t * right

  def solveGram(
      gram: DMat,
      rhs: DMat,
      ridge: Double = 0.0,
      tolerance: Double = 1e-12
  ): Either[LinAlgError, DMat] =
    if gram.rows != gram.cols then Left(LinAlgError.NonSquareMatrix(gram.shape))
    else if rhs.rows != gram.rows then
      Left(LinAlgError.DimensionMismatch(Shape(Rows(gram.rows), Cols(rhs.cols)), rhs.shape))
    else if ridge < 0.0 || !ridge.isFinite then
      Left(LinAlgError.InvalidArgument(s"ridge must be finite and non-negative, got $ridge"))
    else if tolerance < 0.0 || !tolerance.isFinite then
      Left(LinAlgError.InvalidArgument(s"tolerance must be finite and non-negative, got $tolerance"))
    else
      firstNonFinite("Gram matrix", gram)
        .orElse(firstNonFinite("right-hand side", rhs)) match
        case Some(error) => Left(error)
        case None =>
          val regularized =
            if ridge == 0.0 then gram
            else gram.addToDiagonal(ridge)
          regularized
            .cholesky(CholeskyOptions(pivotTolerance = tolerance))
            .flatMap(_.solve(rhs))

  def coefficients(
      data: DMat,
      basis: DMat,
      ridge: Double = 0.0,
      tolerance: Double = 1e-12
  ): Either[LinAlgError, DMat] =
    if data.rows != basis.rows then
      Left(LinAlgError.DimensionMismatch(Shape(Rows(basis.rows), Cols(data.cols)), data.shape))
    else
      firstNonFinite("data", data)
        .orElse(firstNonFinite("basis", basis)) match
        case Some(error) => Left(error)
        case None =>
          solveGram(crossProduct(basis), transposeMultiply(basis, data), ridge, tolerance)

  private def firstNonFinite(label: String, matrix: DMat): Option[LinAlgError] =
    val data = matrix.copyData
    var index = 0
    while index < data.length do
      val value = data(index)
      if !value.isFinite then
        return Some(LinAlgError.InvalidArgument(s"$label value $index is not finite: $value"))
      index += 1
    None

private[latent] object LatentOperators:
  def apply(operator: DoubleLinearOperator, input: DMat): Either[LinAlgError, DMat] =
    operator.applyTo(input)

  def compose(
      first: DoubleLinearOperator,
      second: DoubleLinearOperator
  ): Either[LinAlgError, DoubleLinearOperator] =
    LinearOperator.compose(second, first)

  def restrict(
      operator: DoubleLinearOperator,
      targetRows: Option[IndexedSeq[Int]] = None,
      sourceCols: Option[IndexedSeq[Int]] = None
  ): Either[LinAlgError, DoubleLinearOperator] =
    for
      columns <- sourceCols.fold[Either[LinAlgError, DoubleLinearOperator]](Right(operator))(operator.restrictColumns)
      rows <- targetRows.fold[Either[LinAlgError, DoubleLinearOperator]](Right(columns))(columns.restrictRows)
    yield rows

  def identity(size: Int): Either[LinAlgError, DoubleLinearOperator] =
    if size < 0 then Left(LinAlgError.InvalidArgument(s"identity size must be non-negative, got $size"))
    else Right(Sparse.identity(size))

  def csrFromTriplets(
      rows: Int,
      cols: Int,
      rowIndices: Array[Int],
      colIndices: Array[Int],
      values: Array[Double]
  ): Either[LinAlgError, CSR] =
    if rows < 0 || cols < 0 then Left(LinAlgError.InvalidArgument(s"operator dimensions must be non-negative, got ${rows}x${cols}"))
    else if rowIndices.length != colIndices.length || rowIndices.length != values.length then
      Left(LinAlgError.InvalidArgument("triplet arrays must have equal length"))
    else
      val builder = Sparse.coo(rows, cols)
      var index = 0
      var error = Option.empty[LinAlgError]
      while index < values.length && error.isEmpty do
        val row = rowIndices(index)
        val col = colIndices(index)
        val value = values(index)
        if row < 0 || row >= rows then error = Some(LinAlgError.IndexOutOfBounds(row, rows))
        else if col < 0 || col >= cols then error = Some(LinAlgError.IndexOutOfBounds(col, cols))
        else if !value.isFinite then error = Some(LinAlgError.InvalidArgument(s"triplet value $index is not finite: $value"))
        else builder.add(row, col, value)
        index += 1
      error.fold[Either[LinAlgError, CSR]](Right(builder.toCSR()))(Left(_))

extension (matrix: DMat)
  private[latent] def copyData: Array[Double] =
    matrix.valuesRowMajor.toArray

  private[latent] def transpose: DMat =
    matrix.t

  private[latent] def toRows: Vector[Vector[Double]] =
    Vector.tabulate(matrix.rows) { row =>
      Vector.tabulate(matrix.cols) { col => matrix(row, col) }
    }

  private[latent] def selectRows(indices: IndexedSeq[Int]): DMat =
    Matrix.tabulate(indices.length, matrix.cols) { (row, col) => matrix(indices(row), col) }

  private[latent] def addToDiagonal(amount: Double): DMat =
    require(matrix.rows == matrix.cols, "diagonal update requires a square matrix")
    Matrix.tabulate(matrix.rows, matrix.cols) { (row, col) =>
      matrix(row, col) + (if row == col then amount else 0.0)
    }

extension (vector: DVec)
  private[latent] def copyData: Array[Double] =
    vector.toSeq.toArray

  private[latent] def toVector: Vector[Double] =
    vector.toSeq.toVector

extension (error: LinAlgError)
  private[latent] def message: String =
    error.getMessage
