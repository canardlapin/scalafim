package scalafim.linalg

import scala.util.Sorting

final case class SymmetricEigenResult(values: DoubleVector, vectors: DoubleMatrix):
  require(vectors.rows == vectors.cols, "eigenvector matrix must be square")
  require(values.length == vectors.cols, "eigenvalue count must match eigenvector columns")

final case class SvdResult(u: DoubleMatrix, singularValues: DoubleVector, v: DoubleMatrix):
  require(u.cols == singularValues.length, "left singular vector columns must match singular values")
  require(v.cols == singularValues.length, "right singular vector columns must match singular values")

trait SymmetricEigenSolver:
  def decompose(matrix: DoubleMatrix): Either[LinearAlgebraError, SymmetricEigenResult]

trait DenseSvdSolver:
  def decompose(input: DoubleMatrix, rank: DecompositionRank): Either[LinearAlgebraError, SvdResult]

trait GeneralizedEigenSolver:
  def decompose(
      a: DoubleMatrix,
      b: DoubleMatrix,
      rank: DecompositionRank
  ): Either[LinearAlgebraError, SymmetricEigenResult]

trait SpdInverseSolver:
  def invert(matrix: DoubleMatrix): Either[LinearAlgebraError, DoubleMatrix]

object LinalgSolvers:
  val symmetricEigen: SymmetricEigenSolver =
    JacobiSymmetricEigenSolver()

  val denseSvd: DenseSvdSolver =
    GramDenseSvdSolver(symmetricEigen)

  val generalizedEigen: GeneralizedEigenSolver =
    DenseGeneralizedEigenSolver(symmetricEigen)

  val spdInverse: SpdInverseSolver =
    CholeskySpdInverseSolver()

final case class JacobiSymmetricEigenSolver(
    tolerance: Tolerance = Tolerance.DefaultEigen,
    maxSweeps: Int = 100
) extends SymmetricEigenSolver:
  require(maxSweeps >= 0, "maxSweeps must be non-negative")

  override def decompose(matrix: DoubleMatrix): Either[LinearAlgebraError, SymmetricEigenResult] =
    SquareMatrix.from(matrix).flatMap { square =>
      DenseDecompositionOps.checkFinite(MatrixValueRole.Data, square.value).flatMap { _ =>
        DenseDecompositionOps.checkSymmetric(square.value, tolerance.value).flatMap { _ =>
          val n = square.size
          val a = square.value.copyData
          val v = DoubleMatrix.eye(n).copyData
          var sweep = 0
          var converged = n <= 1 || maxOffDiagonal(a, n) <= convergenceThreshold(a)

          while sweep < maxSweeps && !converged do
            val threshold = convergenceThreshold(a)
            var p = 0
            while p < n - 1 do
              var q = p + 1
              while q < n do
                if Math.abs(a(p * n + q)) > threshold then rotate(a, v, n, p, q)
                q += 1
              p += 1
            sweep += 1
            converged = maxOffDiagonal(a, n) <= convergenceThreshold(a)

          if !converged then Left(LinearAlgebraError.SolverDidNotConverge("Jacobi symmetric eigensolver", maxSweeps))
          else
            val values = new Array[Double](n)
            var i = 0
            while i < n do
              values(i) = a(i * n + i)
              i += 1
            Right(sortDescending(values, v, n))
        }
      }
    }

  private def convergenceThreshold(a: Array[Double]): Double =
    var scale = 0.0
    var i = 0
    while i < a.length do
      val value = Math.abs(a(i))
      if value > scale then scale = value
      i += 1
    tolerance.value * scale

  private def maxOffDiagonal(a: Array[Double], n: Int): Double =
    var best = 0.0
    var row = 0
    while row < n do
      var col = row + 1
      while col < n do
        val value = Math.abs(a(row * n + col))
        if value > best then best = value
        col += 1
      row += 1
    best

  private def rotate(a: Array[Double], v: Array[Double], n: Int, p: Int, q: Int): Unit =
    val app = a(p * n + p)
    val aqq = a(q * n + q)
    val apq = a(p * n + q)
    val theta = 0.5 * Math.atan2(2.0 * apq, aqq - app)
    val c = Math.cos(theta)
    val s = Math.sin(theta)

    var i = 0
    while i < n do
      if i != p && i != q then
        val aip = a(i * n + p)
        val aiq = a(i * n + q)
        val newIp = c * aip - s * aiq
        val newIq = s * aip + c * aiq
        a(i * n + p) = newIp
        a(p * n + i) = newIp
        a(i * n + q) = newIq
        a(q * n + i) = newIq
      i += 1

    val c2 = c * c
    val s2 = s * s
    val sc = s * c
    a(p * n + p) = c2 * app - 2.0 * sc * apq + s2 * aqq
    a(q * n + q) = s2 * app + 2.0 * sc * apq + c2 * aqq
    a(p * n + q) = 0.0
    a(q * n + p) = 0.0

    i = 0
    while i < n do
      val vip = v(i * n + p)
      val viq = v(i * n + q)
      v(i * n + p) = c * vip - s * viq
      v(i * n + q) = s * vip + c * viq
      i += 1

  private def sortDescending(values: Array[Double], vectors: Array[Double], n: Int): SymmetricEigenResult =
    val order = (0 until n).toArray
    Sorting.stableSort(order, (left: Int, right: Int) => values(left) > values(right))

    val outValues = new Array[Double](n)
    val outVectors = new Array[Double](n * n)
    var outCol = 0
    while outCol < n do
      val sourceCol = order(outCol)
      outValues(outCol) = values(sourceCol)
      var firstNonZero = 0
      while firstNonZero < n && Math.abs(vectors(firstNonZero * n + sourceCol)) <= tolerance.value do
        firstNonZero += 1
      val sign =
        if firstNonZero < n && vectors(firstNonZero * n + sourceCol) < 0.0 then -1.0
        else 1.0
      var row = 0
      while row < n do
        outVectors(row * n + outCol) = sign * vectors(row * n + sourceCol)
        row += 1
      outCol += 1

    SymmetricEigenResult(DoubleVector.unsafe(outValues), DoubleMatrix.unsafe(n, n, outVectors))

final case class GramDenseSvdSolver(
    eigenSolver: SymmetricEigenSolver,
    tolerance: Tolerance = Tolerance.DefaultEigen
) extends DenseSvdSolver:
  override def decompose(input: DoubleMatrix, rank: DecompositionRank): Either[LinearAlgebraError, SvdResult] =
    val limit = Math.min(input.rows, input.cols)
    if rank.value > limit then Left(LinearAlgebraError.InvalidDecompositionRank(rank.value, limit))
    else
      for
        _ <- DenseDecompositionOps.checkFinite(MatrixValueRole.Data, input)
        eigen <- eigenSolver.decompose(DoubleMatrix.crossProduct(input))
        v = DenseDecompositionOps.takeColumns(eigen.vectors, rank.value)
        scores = DoubleMatrix.multiply(input, v)
      yield
        val singularValues = new Array[Double](rank.value)
        var col = 0
        while col < rank.value do
          singularValues(col) = Math.sqrt(Math.max(eigen.values(col), 0.0))
          col += 1
        val uData = scores.copyData
        col = 0
        while col < rank.value do
          val sv = singularValues(col)
          var row = 0
          while row < scores.rows do
            uData(row * rank.value + col) =
              if sv > tolerance.value then scores(row, col) / sv
              else 0.0
            row += 1
          col += 1
        SvdResult(DoubleMatrix.unsafe(scores.rows, rank.value, uData), DoubleVector.unsafe(singularValues), v)

final case class DenseGeneralizedEigenSolver(
    eigenSolver: SymmetricEigenSolver,
    tolerance: Tolerance = Tolerance.DefaultEigen
) extends GeneralizedEigenSolver:
  override def decompose(
      a: DoubleMatrix,
      b: DoubleMatrix,
      rank: DecompositionRank
  ): Either[LinearAlgebraError, SymmetricEigenResult] =
    if a.rows != a.cols then Left(LinearAlgebraError.NonSquareMatrix(a.rows, a.cols))
    else if b.rows != b.cols then Left(LinearAlgebraError.NonSquareMatrix(b.rows, b.cols))
    else if a.rows != b.rows then Left(LinearAlgebraError.DimensionMismatch(DimensionRole.DataRows, a.rows, b.rows))
    else if rank.value > a.rows then Left(LinearAlgebraError.InvalidDecompositionRank(rank.value, a.rows))
    else
      for
        invSqrtB <- DenseDecompositionOps.inverseSquareRoot(b, eigenSolver, tolerance)
        reduced = DoubleMatrix.multiply(DoubleMatrix.multiply(invSqrtB, a), invSqrtB)
        eigen <- eigenSolver.decompose(reduced)
      yield
        val values = DenseDecompositionOps.takeVector(eigen.values, rank.value)
        val vectors = DoubleMatrix.multiply(invSqrtB, DenseDecompositionOps.takeColumns(eigen.vectors, rank.value))
        SymmetricEigenResult(values, vectors)

final case class CholeskySpdInverseSolver(
    tolerance: Tolerance = Tolerance.DefaultCholesky
) extends SpdInverseSolver:
  override def invert(matrix: DoubleMatrix): Either[LinearAlgebraError, DoubleMatrix] =
    for
      _ <- DenseDecompositionOps.checkFinite(MatrixValueRole.Gram, matrix)
      square <- SquareMatrix.from(matrix)
      factor <- Cholesky.decompose(square, tolerance)
    yield DenseDecompositionOps.symmetrize(factor.solve(DoubleMatrix.eye(square.size)))

private[scalafim] object DenseDecompositionOps:
  def checkFinite(role: MatrixValueRole, matrix: DoubleMatrix): Either[LinearAlgebraError, Unit] =
    var i = 0
    var error = Option.empty[LinearAlgebraError]
    while i < matrix.dataArray.length && error.isEmpty do
      val value = matrix.dataArray(i)
      if !value.isFinite then error = Some(LinearAlgebraError.NonFiniteValue(role, i, value))
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(())

  def checkSymmetric(matrix: DoubleMatrix, tolerance: Double): Either[LinearAlgebraError, Unit] =
    if matrix.rows != matrix.cols then Left(LinearAlgebraError.NonSquareMatrix(matrix.rows, matrix.cols))
    else
      var row = 0
      var error = Option.empty[LinearAlgebraError]
      while row < matrix.rows && error.isEmpty do
        var col = row + 1
        while col < matrix.cols && error.isEmpty do
          val left = matrix(row, col)
          val right = matrix(col, row)
          if Math.abs(left - right) > tolerance then
            error = Some(LinearAlgebraError.NonSymmetricMatrix(row, col, left, right))
          col += 1
        row += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(())

  def takeColumns(matrix: DoubleMatrix, count: Int): DoubleMatrix =
    require(count >= 0 && count <= matrix.cols, "invalid column count")
    val out = new Array[Double](matrix.rows * count)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < count do
        out(row * count + col) = matrix(row, col)
        col += 1
      row += 1
    DoubleMatrix.unsafe(matrix.rows, count, out)

  def takeVector(vector: DoubleVector, count: Int): DoubleVector =
    require(count >= 0 && count <= vector.length, "invalid vector count")
    val out = new Array[Double](count)
    var i = 0
    while i < count do
      out(i) = vector(i)
      i += 1
    DoubleVector.unsafe(out)

  def diagonal(values: DoubleVector): DoubleMatrix =
    val out = new Array[Double](values.length * values.length)
    var i = 0
    while i < values.length do
      out(i * values.length + i) = values(i)
      i += 1
    DoubleMatrix.unsafe(values.length, values.length, out)

  def symmetrize(matrix: DoubleMatrix): DoubleMatrix =
    require(matrix.rows == matrix.cols, "matrix must be square")
    val n = matrix.rows
    val out = matrix.copyData
    var row = 0
    while row < n do
      var col = row + 1
      while col < n do
        val value = 0.5 * (matrix(row, col) + matrix(col, row))
        out(row * n + col) = value
        out(col * n + row) = value
        col += 1
      row += 1
    DoubleMatrix.unsafe(n, n, out)

  def inverseSquareRoot(
      matrix: DoubleMatrix,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Tolerance
  ): Either[LinearAlgebraError, DoubleMatrix] =
    eigenSolver.decompose(matrix).flatMap { eigen =>
      val diag = new Array[Double](eigen.values.length)
      var i = 0
      var error = Option.empty[LinearAlgebraError]
      while i < diag.length && error.isEmpty do
        val value = eigen.values(i)
        if value <= tolerance.value then error = Some(LinearAlgebraError.NonPositiveDefinite(i, value))
        else diag(i) = 1.0 / Math.sqrt(value)
        i += 1
      error match
        case Some(value) => Left(value)
        case None =>
          val d = diagonal(DoubleVector.unsafe(diag))
          Right(DoubleMatrix.multiply(DoubleMatrix.multiply(eigen.vectors, d), eigen.vectors.transpose))
    }
