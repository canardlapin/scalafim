package scalafim.linalg

final case class MatrixShape private (rows: Int, cols: Int):
  require(rows >= 0 && cols >= 0, "matrix shape dimensions must be non-negative")
  require(rows.toLong * cols.toLong <= Int.MaxValue, "matrix shape is too large")

  def entries: Int =
    rows * cols

  def isSquare: Boolean =
    rows == cols

object MatrixShape:
  def from(rows: Int, cols: Int): Either[LinearAlgebraError, MatrixShape] =
    if rows < 0 || cols < 0 then Left(LinearAlgebraError.InvalidMatrixShape(rows, cols))
    else if rows.toLong * cols.toLong > Int.MaxValue then Left(LinearAlgebraError.MatrixTooLarge(rows, cols))
    else Right(unsafe(rows, cols))

  private[scalafim] def unsafe(rows: Int, cols: Int): MatrixShape =
    new MatrixShape(rows, cols)

final case class SquareMatrix private (value: DoubleMatrix):
  def size: Int =
    value.rows

  def shape: MatrixShape =
    value.shape

object SquareMatrix:
  def from(matrix: DoubleMatrix): Either[LinearAlgebraError, SquareMatrix] =
    if matrix.rows != matrix.cols then Left(LinearAlgebraError.NonSquareMatrix(matrix.rows, matrix.cols))
    else Right(unsafe(matrix))

  private[scalafim] def unsafe(matrix: DoubleMatrix): SquareMatrix =
    new SquareMatrix(matrix)

opaque type RowIndex = Int

object RowIndex:
  def apply(value: Int): Either[LinearAlgebraError, RowIndex] =
    if value >= 0 then Right(value)
    else Left(LinearAlgebraError.IndexOutOfBounds(MatrixAxis.Row, value, 0))

  def in(shape: MatrixShape, value: Int): Either[LinearAlgebraError, RowIndex] =
    if value >= 0 && value < shape.rows then Right(value)
    else Left(LinearAlgebraError.IndexOutOfBounds(MatrixAxis.Row, value, shape.rows))

  private[scalafim] def unsafe(value: Int): RowIndex =
    value

  extension (index: RowIndex)
    inline def value: Int = index

opaque type ColIndex = Int

object ColIndex:
  def apply(value: Int): Either[LinearAlgebraError, ColIndex] =
    if value >= 0 then Right(value)
    else Left(LinearAlgebraError.IndexOutOfBounds(MatrixAxis.Col, value, 0))

  def in(shape: MatrixShape, value: Int): Either[LinearAlgebraError, ColIndex] =
    if value >= 0 && value < shape.cols then Right(value)
    else Left(LinearAlgebraError.IndexOutOfBounds(MatrixAxis.Col, value, shape.cols))

  private[scalafim] def unsafe(value: Int): ColIndex =
    value

  extension (index: ColIndex)
    inline def value: Int = index

opaque type Tolerance = Double

object Tolerance:
  val DefaultQr: Tolerance =
    unsafe(1e-7)

  val DefaultCholesky: Tolerance =
    unsafe(1e-12)

  def apply(value: Double): Either[LinearAlgebraError, Tolerance] =
    if value >= 0.0 && value.isFinite then Right(value)
    else Left(LinearAlgebraError.InvalidParameter(NumericParameter.Tolerance, value))

  private[scalafim] def unsafe(value: Double): Tolerance =
    value

  extension (tolerance: Tolerance)
    inline def value: Double = tolerance

opaque type Ridge = Double

object Ridge:
  val Zero: Ridge =
    unsafe(0.0)

  def apply(value: Double): Either[LinearAlgebraError, Ridge] =
    if value >= 0.0 && value.isFinite then Right(value)
    else Left(LinearAlgebraError.InvalidParameter(NumericParameter.Ridge, value))

  private[scalafim] def unsafe(value: Double): Ridge =
    value

  extension (ridge: Ridge)
    inline def value: Double = ridge

enum Pivoting:
  case Enabled
  case Disabled

  def enabled: Boolean =
    this match
      case Enabled  => true
      case Disabled => false

object Pivoting:
  def fromBoolean(value: Boolean): Pivoting =
    if value then Enabled else Disabled
