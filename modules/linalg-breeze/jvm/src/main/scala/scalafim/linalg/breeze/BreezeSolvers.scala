package scalafim.linalg.breeze

import scala.util.control.NonFatal

import scalafim.linalg.*

object BreezeSolvers:
  val symmetricEigen: SymmetricEigenSolver =
    BreezeSymmetricEigenSolver()

  val partialSymmetricEigen: PartialSymmetricEigenSolver =
    BreezePartialSymmetricEigenSolver(symmetricEigen)

  val denseSvd: DenseSvdSolver =
    BreezeDenseSvdSolver()

  val generalizedEigen: GeneralizedEigenSolver =
    BreezeGeneralizedEigenSolver(symmetricEigen)

  val spdInverse: SpdInverseSolver =
    BreezeSpdInverseSolver()

  def choleskyLower(matrix: DoubleMatrix): Either[LinearAlgebraError, DoubleMatrix] =
    BreezeCholesky.lower(matrix)

  def solveLeastSquaresFullRank(
      design: DoubleMatrix,
      rhs: DoubleMatrix,
      tolerance: Tolerance = Tolerance.DefaultQr
  ): Either[LinearAlgebraError, QrLeastSquaresSolution] =
    BreezeLeastSquares.solveFullRank(design, rhs, tolerance)

final case class BreezeSymmetricEigenSolver(
    tolerance: Tolerance = Tolerance.DefaultEigen
) extends SymmetricEigenSolver:
  override def decompose(matrix: DoubleMatrix): Either[LinearAlgebraError, SymmetricEigenResult] =
    for
      _ <- BreezeInterop.checkFinite(MatrixValueRole.Data, matrix)
      square <- SquareMatrix.from(matrix)
      _ <- BreezeInterop.checkSymmetric(square.value, tolerance.value)
      result <- BreezeInterop.catchLinearAlgebra:
        val eigen = _root_.breeze.linalg.eigSym(BreezeInterop.toBreeze(square.value))
        BreezeInterop.sortedEigen(eigen.eigenvalues, eigen.eigenvectors, tolerance.value)
    yield result

final case class BreezePartialSymmetricEigenSolver(
    fullSolver: SymmetricEigenSolver = BreezeSymmetricEigenSolver()
) extends PartialSymmetricEigenSolver:
  override def decompose(
      operator: SymmetricOperator,
      rank: DecompositionRank,
      spectrum: PartialSpectrum
  ): Either[LinearAlgebraError, PartialSymmetricEigenResult] =
    if rank.value > operator.order then
      Left(LinearAlgebraError.InvalidDecompositionRank(rank.value, operator.order))
    else
      for
        dense <- operator.toDense
        full <- fullSolver.decompose(dense)
        partial <- PartialEigenOps.select(operator, full, rank.value, spectrum, "breeze-dense", None)
      yield partial

final case class BreezeDenseSvdSolver(
    tolerance: Tolerance = Tolerance.DefaultEigen
) extends DenseSvdSolver:
  override def decompose(input: DoubleMatrix, rank: DecompositionRank): Either[LinearAlgebraError, SvdResult] =
    val limit = Math.min(input.rows, input.cols)
    if rank.value > limit then Left(LinearAlgebraError.InvalidDecompositionRank(rank.value, limit))
    else
      for
        _ <- BreezeInterop.checkFinite(MatrixValueRole.Data, input)
        result <- BreezeInterop.catchLinearAlgebra:
          val svd = _root_.breeze.linalg.svd(BreezeInterop.toBreeze(input))
          BreezeInterop.truncatedSvd(svd.U, svd.S, svd.Vt, rank.value, tolerance.value)
      yield result

final case class BreezeGeneralizedEigenSolver(
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
        invSqrtB <- BreezeInterop.inverseSquareRoot(b, eigenSolver, tolerance)
        reduced = DoubleMatrix.multiply(DoubleMatrix.multiply(invSqrtB, a), invSqrtB)
        eigen <- eigenSolver.decompose(reduced)
      yield
        val values = BreezeInterop.takeVector(eigen.values, rank.value)
        val vectors = DoubleMatrix.multiply(invSqrtB, BreezeInterop.takeColumns(eigen.vectors, rank.value))
        SymmetricEigenResult(values, vectors)

final case class BreezeSpdInverseSolver(
    tolerance: Tolerance = Tolerance.DefaultCholesky
) extends SpdInverseSolver:
  override def invert(matrix: DoubleMatrix): Either[LinearAlgebraError, DoubleMatrix] =
    for
      _ <- BreezeInterop.checkFinite(MatrixValueRole.Gram, matrix)
      square <- SquareMatrix.from(matrix)
      _ <- BreezeInterop.catchLinearAlgebra:
        _root_.breeze.linalg.cholesky(BreezeInterop.toBreeze(square.value))
      inverse <- BreezeInterop.catchLinearAlgebra:
        BreezeInterop.symmetrize(BreezeInterop.fromBreeze(_root_.breeze.linalg.inv(BreezeInterop.toBreeze(square.value))))
    yield inverse

private object BreezeCholesky:
  def lower(matrix: DoubleMatrix): Either[LinearAlgebraError, DoubleMatrix] =
    for
      _ <- BreezeInterop.checkFinite(MatrixValueRole.Gram, matrix)
      square <- SquareMatrix.from(matrix)
      lower <- BreezeInterop.catchLinearAlgebra:
        BreezeInterop.fromBreeze(_root_.breeze.linalg.cholesky(BreezeInterop.toBreeze(square.value)))
    yield lower

private object BreezeLeastSquares:
  def solveFullRank(
      design: DoubleMatrix,
      rhs: DoubleMatrix,
      tolerance: Tolerance
  ): Either[LinearAlgebraError, QrLeastSquaresSolution] =
    if rhs.rows != design.rows then
      Left(LinearAlgebraError.DimensionMismatch(DimensionRole.RightHandSideRows, design.rows, rhs.rows))
    else
      for
        _ <- BreezeInterop.checkFinite(MatrixValueRole.Data, design)
        _ <- BreezeInterop.checkFinite(MatrixValueRole.RightHandSide, rhs)
        solution <- BreezeInterop.catchLinearAlgebra:
          val x = BreezeInterop.toBreeze(design)
          val y = BreezeInterop.toBreeze(rhs)
          val svd = _root_.breeze.linalg.svd(x)
          val rank = BreezeInterop.numericalRank(svd.S, tolerance.value)
          if rank < design.cols then Left(LinearAlgebraError.RankDeficient(design.cols, rank))
          else
            val coefficients = _root_.breeze.linalg.pinv(x) * y
            val covariance = _root_.breeze.linalg.inv(x.t * x)
            Right(
              QrLeastSquaresSolution(
                BreezeInterop.fromBreeze(coefficients),
                BreezeInterop.symmetrize(BreezeInterop.fromBreeze(covariance)),
                rank
              )
            )
        result <- solution
      yield result

private object BreezeInterop:
  type BDM = _root_.breeze.linalg.DenseMatrix[Double]
  type BDV = _root_.breeze.linalg.DenseVector[Double]

  def toBreeze(matrix: DoubleMatrix): BDM =
    val out = _root_.breeze.linalg.DenseMatrix.zeros[Double](matrix.rows, matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row, col) = matrix(row, col)
        col += 1
      row += 1
    out

  def fromBreeze(matrix: BDM): DoubleMatrix =
    val out = new Array[Double](matrix.rows * matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) = matrix(row, col)
        col += 1
      row += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  def fromBreeze(vector: BDV): DoubleVector =
    val out = new Array[Double](vector.length)
    var i = 0
    while i < vector.length do
      out(i) = vector(i)
      i += 1
    DoubleVector.unsafe(out)

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

  def catchLinearAlgebra[A](body: => A): Either[LinearAlgebraError, A] =
    try Right(body)
    catch
      case NonFatal(error) =>
        val detail = Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
        Left(LinearAlgebraError.BackendFailure("Breeze", detail))

  def sortedEigen(values: BDV, vectors: BDM, tolerance: Double): SymmetricEigenResult =
    val order = (0 until values.length).toArray
    scala.util.Sorting.stableSort(order, (left: Int, right: Int) => values(left) > values(right))

    val outValues = new Array[Double](values.length)
    val outVectors = new Array[Double](vectors.rows * vectors.cols)
    var outCol = 0
    while outCol < order.length do
      val sourceCol = order(outCol)
      outValues(outCol) = values(sourceCol)
      val sign = columnSign(vectors, sourceCol, tolerance)
      var row = 0
      while row < vectors.rows do
        outVectors(row * order.length + outCol) = sign * vectors(row, sourceCol)
        row += 1
      outCol += 1

    SymmetricEigenResult(DoubleVector.unsafe(outValues), DoubleMatrix.unsafe(vectors.rows, order.length, outVectors))

  def truncatedSvd(u: BDM, s: BDV, vt: BDM, rank: Int, tolerance: Double): SvdResult =
    val uData = new Array[Double](u.rows * rank)
    val vData = new Array[Double](vt.cols * rank)
    val sData = new Array[Double](rank)
    var col = 0
    while col < rank do
      val sign = svdColumnSign(vt, col, tolerance)
      sData(col) = s(col)
      var row = 0
      while row < u.rows do
        uData(row * rank + col) = sign * u(row, col)
        row += 1
      row = 0
      while row < vt.cols do
        vData(row * rank + col) = sign * vt(col, row)
        row += 1
      col += 1
    SvdResult(DoubleMatrix.unsafe(u.rows, rank, uData), DoubleVector.unsafe(sData), DoubleMatrix.unsafe(vt.cols, rank, vData))

  def numericalRank(values: BDV, tolerance: Double): Int =
    if values.length == 0 then 0
    else
      val cutoff = tolerance * Math.max(1.0, Math.abs(values(0)))
      var rank = 0
      while rank < values.length && values(rank) > cutoff do rank += 1
      rank

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

  def takeColumns(matrix: DoubleMatrix, count: Int): DoubleMatrix =
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

  private def columnSign(matrix: BDM, col: Int, tolerance: Double): Double =
    var row = 0
    while row < matrix.rows && Math.abs(matrix(row, col)) <= tolerance do row += 1
    if row < matrix.rows && matrix(row, col) < 0.0 then -1.0 else 1.0

  private def svdColumnSign(vt: BDM, row: Int, tolerance: Double): Double =
    var col = 0
    while col < vt.cols && Math.abs(vt(row, col)) <= tolerance do col += 1
    if col < vt.cols && vt(row, col) < 0.0 then -1.0 else 1.0
