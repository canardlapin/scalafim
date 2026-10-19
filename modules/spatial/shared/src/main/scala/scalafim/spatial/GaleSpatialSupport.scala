package scalafim.spatial

import gale.backend.Backend.given
import gale.linalg.{DMat, LinAlgError}
import gale.sparse.{COO, CSR, CSRProductPlan, Sparse, SparseValuePolicy}

private[spatial] object GaleSpatialSupport:
  def matrixFromRows(rowsData: Seq[Seq[Double]]): DMat =
    val indexedRows = rowsData.map(_.toIndexedSeq).toIndexedSeq
    val rowCount = indexedRows.length
    val colCount = if rowCount == 0 then 0 else indexedRows.head.length
    require(indexedRows.forall(_.length == colCount), "ragged rows")
    val values = new Array[Double](rowCount * colCount)
    var row = 0
    while row < rowCount do
      var col = 0
      while col < colCount do
        values(row * colCount + col) = indexedRows(row)(col)
        col += 1
      row += 1
    unsafeOwnedMatrix(rowCount, colCount, values)

  def ownedMatrix(rows: Int, cols: Int, values: Array[Double]): Either[LinAlgError, DMat] =
    val expected = rows.toLong * cols.toLong
    if rows < 0 || cols < 0 then
      Left(LinAlgError.InvalidArgument(s"matrix shape must be non-negative, got ${rows}x${cols}"))
    else if expected > Int.MaxValue.toLong || values.length.toLong != expected then
      Left(
        LinAlgError.InvalidArgument(
          s"matrix ${rows}x${cols} requires $expected values, found ${values.length}"
        )
      )
    else
      val builder = DMat.newBuilder(rows, cols)
      var index = 0
      while index < values.length do
        builder.writeLinear(index, values(index))
        index += 1
      Right(builder.result())

  def unsafeOwnedMatrix(rows: Int, cols: Int, values: Array[Double]): DMat =
    ownedMatrix(rows, cols, values).fold(throw _, identity)

  def sparseTriplets(
      rows: Int,
      cols: Int,
      rowIndices: Array[Int],
      colIndices: Array[Int],
      values: Array[Double]
  ): Either[LinAlgError, COO] =
    if rowIndices.length != colIndices.length || rowIndices.length != values.length then
      Left(LinAlgError.InvalidArgument("sparse row, column, and value lengths must match"))
    else
      Sparse
        .cooChecked(rows, cols, SparseValuePolicy.RequireFinite)
        .flatMap: builder =>
          var index = 0
          var error = Option.empty[LinAlgError]
          while index < values.length && error.isEmpty do
            builder.tryAdd(rowIndices(index), colIndices(index), values(index)) match
              case Left(value) => error = Some(value)
              case Right(())   => ()
            index += 1
          error match
            case Some(value) => Left(value)
            case None        => builder.tryToCOO()

  def sparseCsr(
      rows: Int,
      cols: Int,
      rowIndices: Array[Int],
      colIndices: Array[Int],
      values: Array[Double]
  ): Either[LinAlgError, CSR] =
    sparseTriplets(rows, cols, rowIndices, colIndices, values).map(_.toCSR)

  /** Delegate sparse numeric composition to Gale's analyzed CSR product. */
  def product(left: CSR, right: CSR): Either[LinAlgError, CSR] =
    CSRProductPlan.analyze(left.pattern, right.pattern).flatMap(_.evaluate(left, right))

extension (matrix: DMat)
  private[spatial] def copyData: Array[Double] =
    val values = new Array[Double](matrix.rows * matrix.cols)
    matrix.copyRowMajorTo(values)
    values

  private[spatial] def toRows: Vector[Vector[Double]] =
    Vector.tabulate(matrix.rows): row =>
      Vector.tabulate(matrix.cols)(column => matrix(row, column))
