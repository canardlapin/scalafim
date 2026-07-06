package scalafim.linalg

final class Cholesky private (val size: Int, private val lower: Array[Double]):
  require(size >= 0, "size must be non-negative")
  require(lower.length == size * size, "Cholesky storage length mismatch")

  def lowerMatrix: DoubleMatrix =
    DoubleMatrix.unsafe(size, size, lower.clone)

  def solve(rhs: DoubleMatrix): DoubleMatrix =
    require(rhs.rows == size, s"right-hand side rows ${rhs.rows} != factor size $size")
    val out = rhs.copyData

    var col = 0
    while col < rhs.cols do
      var row = 0
      while row < size do
        var sum = out(row * rhs.cols + col)
        var k = 0
        while k < row do
          sum -= lower(row * size + k) * out(k * rhs.cols + col)
          k += 1
        out(row * rhs.cols + col) = sum / lower(row * size + row)
        row += 1

      row = size - 1
      while row >= 0 do
        var sum = out(row * rhs.cols + col)
        var k = row + 1
        while k < size do
          sum -= lower(k * size + row) * out(k * rhs.cols + col)
          k += 1
        out(row * rhs.cols + col) = sum / lower(row * size + row)
        row -= 1

      col += 1

    DoubleMatrix.unsafe(size, rhs.cols, out)

object Cholesky:
  def decompose(matrix: DoubleMatrix, tol: Double = 1e-12): Either[LinearAlgebraError, Cholesky] =
    require(tol >= 0.0 && tol.isFinite, "tol must be non-negative and finite")
    if matrix.rows != matrix.cols then
      Left(LinearAlgebraError.NonSquareMatrix(matrix.rows, matrix.cols))
    else
      val size = matrix.rows
      val lower = new Array[Double](size * size)
      var failed: LinearAlgebraError | Null = null
      var row = 0
      while row < size && failed == null do
        var col = 0
        while col <= row && failed == null do
          var sum = matrix.dataArray(row * size + col)
          var k = 0
          while k < col do
            sum -= lower(row * size + k) * lower(col * size + k)
            k += 1
          if row == col then
            if sum <= tol then failed = LinearAlgebraError.NonPositiveDefinite(row, sum)
            else lower(row * size + col) = math.sqrt(sum)
          else
            lower(row * size + col) = sum / lower(col * size + col)
          col += 1
        row += 1

      failed match
        case null  => Right(new Cholesky(size, lower))
        case error => Left(error)
