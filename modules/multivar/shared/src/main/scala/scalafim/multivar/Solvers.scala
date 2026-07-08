package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

final case class SymmetricEigenResult(values: DoubleVector, vectors: DoubleMatrix):
  require(vectors.rows == vectors.cols, "eigenvector matrix must be square")
  require(values.length == vectors.cols, "eigenvalue count must match eigenvector columns")

final case class SvdResult(u: DoubleMatrix, singularValues: DoubleVector, v: DoubleMatrix):
  require(u.cols == singularValues.length, "left singular vector columns must match singular values")
  require(v.cols == singularValues.length, "right singular vector columns must match singular values")

trait SymmetricEigenSolver:
  def decompose(matrix: DoubleMatrix): Either[MultivarError, SymmetricEigenResult]

trait SvdSolver:
  def decompose(input: MatrixView, components: ComponentCount): Either[MultivarError, SvdResult]

trait GeneralizedEigenSolver:
  def decompose(
      a: DoubleMatrix,
      b: DoubleMatrix,
      components: ComponentCount
  ): Either[MultivarError, SymmetricEigenResult]

object DenseSolvers:
  val symmetricEigen: SymmetricEigenSolver =
    JacobiSymmetricEigenSolver()

  val svd: SvdSolver =
    GramSvdSolver(symmetricEigen)

  val generalizedEigen: GeneralizedEigenSolver =
    DenseGeneralizedEigenSolver(symmetricEigen)

final case class JacobiSymmetricEigenSolver(
    tolerance: Double = 1e-10,
    maxSweeps: Int = 100
) extends SymmetricEigenSolver:
  override def decompose(matrix: DoubleMatrix): Either[MultivarError, SymmetricEigenResult] =
    if matrix.rows != matrix.cols then
      Left(MultivarError.MatrixShapeMismatch(s"symmetric eigen solver expected a square matrix, got ${matrix.rows}x${matrix.cols}"))
    else
      MatrixOps.checkFinite("symmetric matrix", matrix).flatMap { _ =>
        MatrixOps.checkSymmetric(matrix, tolerance).flatMap { _ =>
          val n = matrix.rows
          val a = matrix.copyData
          val v = DoubleMatrix.eye(n).copyData
          var sweep = 0
          var converged = n <= 1 || maxOffDiagonal(a, n) <= convergenceThreshold(a, n)

          while sweep < maxSweeps && !converged do
            val threshold = convergenceThreshold(a, n)
            var p = 0
            while p < n - 1 do
              var q = p + 1
              while q < n do
                if Math.abs(a(p * n + q)) > threshold then rotate(a, v, n, p, q)
                q += 1
              p += 1
            sweep += 1
            converged = maxOffDiagonal(a, n) <= convergenceThreshold(a, n)

          if !converged then Left(MultivarError.SolverFailed("Jacobi symmetric eigensolver did not converge"))
          else
            val values = new Array[Double](n)
            var i = 0
            while i < n do
              values(i) = a(i * n + i)
              i += 1
            Right(sortDescending(values, v, n))
        }
      }

  /** Absolute tolerance scaled by the diagonal magnitude, so large-norm matrices still converge. */
  private def convergenceThreshold(a: Array[Double], n: Int): Double =
    var maxDiag = 0.0
    var i = 0
    while i < n do
      val value = Math.abs(a(i * n + i))
      if value > maxDiag then maxDiag = value
      i += 1
    tolerance * Math.max(1.0, maxDiag)

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
    scala.util.Sorting.stableSort(order, (left: Int, right: Int) => values(left) > values(right))

    val outValues = new Array[Double](n)
    val outVectors = new Array[Double](n * n)
    var outCol = 0
    while outCol < n do
      val sourceCol = order(outCol)
      outValues(outCol) = values(sourceCol)
      var firstNonZero = 0
      while firstNonZero < n && Math.abs(vectors(firstNonZero * n + sourceCol)) <= tolerance do
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

final case class GramSvdSolver(
    eigenSolver: SymmetricEigenSolver,
    tolerance: Double = 1e-10
) extends SvdSolver:
  override def decompose(input: MatrixView, components: ComponentCount): Either[MultivarError, SvdResult] =
    val limit = Math.min(input.rows, input.cols)
    if components.value > limit then Left(MultivarError.InvalidComponentRequest(components.value, limit))
    else
      for
        gram <- input.crossProduct
        eigen <- eigenSolver.decompose(gram)
        v = MatrixOps.takeColumns(eigen.vectors, components.value)
        scores <- input.rightMultiply(v)
      yield
        val singularValues = new Array[Double](components.value)
        var col = 0
        while col < components.value do
          singularValues(col) = Math.sqrt(Math.max(eigen.values(col), 0.0))
          col += 1
        val uData = scores.copyData
        col = 0
        while col < components.value do
          val sv = singularValues(col)
          var row = 0
          while row < scores.rows do
            uData(row * components.value + col) =
              if sv > tolerance then scores(row, col) / sv
              else 0.0
            row += 1
          col += 1
        SvdResult(DoubleMatrix.unsafe(scores.rows, components.value, uData), DoubleVector.unsafe(singularValues), v)

final case class DenseGeneralizedEigenSolver(
    eigenSolver: SymmetricEigenSolver,
    tolerance: Double = 1e-10
) extends GeneralizedEigenSolver:
  override def decompose(
      a: DoubleMatrix,
      b: DoubleMatrix,
      components: ComponentCount
  ): Either[MultivarError, SymmetricEigenResult] =
    if a.rows != a.cols || b.rows != b.cols || a.rows != b.rows then
      Left(MultivarError.MatrixShapeMismatch("generalized eigen solver expects same-size square matrices"))
    else
      for
        invSqrtB <- MatrixOps.inverseSquareRoot(b, eigenSolver, tolerance)
        reduced = DoubleMatrix.multiply(DoubleMatrix.multiply(invSqrtB, a), invSqrtB)
        eigen <- eigenSolver.decompose(reduced)
      yield
        val values = MatrixOps.takeVector(eigen.values, components.value)
        val vectors = DoubleMatrix.multiply(invSqrtB, MatrixOps.takeColumns(eigen.vectors, components.value))
        SymmetricEigenResult(values, vectors)

private[multivar] object MatrixOps:
  def checkFinite(role: String, matrix: DoubleMatrix): Either[MultivarError, Unit] =
    var i = 0
    var error = Option.empty[MultivarError]
    while i < matrix.dataArray.length && error.isEmpty do
      val value = matrix.dataArray(i)
      if !value.isFinite then error = Some(MultivarError.NonFiniteValue(role, i, value))
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(())

  def checkSymmetric(matrix: DoubleMatrix, tolerance: Double): Either[MultivarError, Unit] =
    var row = 0
    var error = Option.empty[MultivarError]
    while row < matrix.rows && error.isEmpty do
      var col = row + 1
      while col < matrix.cols && error.isEmpty do
        val left = matrix(row, col)
        val right = matrix(col, row)
        if Math.abs(left - right) > tolerance then
          error = Some(MultivarError.NonSymmetricMatrix(row, col, left, right))
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

  def scale(matrix: DoubleMatrix, factor: Double): DoubleMatrix =
    val out = matrix.copyData
    var i = 0
    while i < out.length do
      out(i) *= factor
      i += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  def addRidge(matrix: DoubleMatrix, ridge: Double): DoubleMatrix =
    val out = matrix.copyData
    var i = 0
    while i < matrix.rows do
      out(i * matrix.cols + i) += ridge
      i += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  def inverseSquareRoot(
      matrix: DoubleMatrix,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double
  ): Either[MultivarError, DoubleMatrix] =
    eigenSolver.decompose(matrix).flatMap { eigen =>
      val diag = new Array[Double](eigen.values.length)
      var i = 0
      var error = Option.empty[MultivarError]
      while i < diag.length && error.isEmpty do
        val value = eigen.values(i)
        if value <= tolerance then error = Some(MultivarError.SolverFailed("matrix is not positive definite"))
        else diag(i) = 1.0 / Math.sqrt(value)
        i += 1
      error match
        case Some(value) => Left(value)
        case None =>
          val d = DoubleMatrix.unsafe(diag.length, diag.length, {
            val out = new Array[Double](diag.length * diag.length)
            var j = 0
            while j < diag.length do
              out(j * diag.length + j) = diag(j)
              j += 1
            out
          })
          Right(DoubleMatrix.multiply(DoubleMatrix.multiply(eigen.vectors, d), eigen.vectors.transpose))
    }

