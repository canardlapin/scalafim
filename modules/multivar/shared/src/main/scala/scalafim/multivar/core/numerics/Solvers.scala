package scalafim.multivar
package core

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.LinAlgError
import gale.linalg.Matrix
import gale.linalg.Vec
import gale.linalg.value
import gale.spectral.Eigen
import gale.spectral.EigenDecomposition
import gale.spectral.EigenOrder
import gale.spectral.EigenSelection

/** Multivar keeps leading components first even though Gale's symmetric
  * eigendecompositions are ascending. This carrier makes that adaptation
  * explicit at the module boundary.
  */
final case class SymmetricEigenResult(values: DVec, vectors: DMat):
  require(values.length == vectors.cols, "eigenvalue count must match eigenvector columns")

final case class SvdResult(u: DMat, singularValues: DVec, v: DMat):
  require(u.cols == singularValues.length, "left singular vector columns must match singular values")
  require(v.cols == singularValues.length, "right singular vector columns must match singular values")

trait SymmetricEigenSolver:
  def decompose(matrix: DMat): Either[LinAlgError, SymmetricEigenResult]

trait SvdSolver:
  def decompose(input: MatrixView, components: ComponentCount): Either[MultivarError, SvdResult]

trait GeneralizedEigenSolver:
  def decompose(a: DMat, b: DMat, components: ComponentCount): Either[LinAlgError, SymmetricEigenResult]

object DenseSolvers:
  val symmetricEigen: SymmetricEigenSolver =
    GaleSymmetricEigenSolver

  val svd: SvdSolver =
    GramSvdSolver(symmetricEigen)

  val generalizedEigen: GeneralizedEigenSolver =
    GaleGeneralizedEigenSolver

private object GaleSymmetricEigenSolver extends SymmetricEigenSolver:
  override def decompose(matrix: DMat): Either[LinAlgError, SymmetricEigenResult] =
    validateFinite(matrix).flatMap: _ =>
      Eigen
        .eigSymmetric(matrix, EigenSelection.All)
        .map(descending)

private object GaleGeneralizedEigenSolver extends GeneralizedEigenSolver:
  override def decompose(
      a: DMat,
      b: DMat,
      components: ComponentCount
  ): Either[LinAlgError, SymmetricEigenResult] =
    val selection =
      if components.value == a.rows then EigenSelection.All
      else EigenSelection.Count(components.value, EigenOrder.LargestAlgebraic)
    Eigen.eigSymmetricGeneralized(a, b, selection).map(descending)

private def descending(result: EigenDecomposition): SymmetricEigenResult =
  val count = result.size
  val values = Vec.newBuilder(count)
  val vectors = Matrix.newBuilder(result.eigenvectors.rows, count)
  var col = 0
  while col < count do
    val source = count - col - 1
    values(col) = result.eigenvalues(source)
    var row = 0
    while row < result.eigenvectors.rows do
      vectors(row, col) = result.eigenvectors(row, source)
      row += 1
    col += 1
  SymmetricEigenResult(values.result(), vectors.result())

private def validateFinite(matrix: DMat): Either[LinAlgError, Unit] =
  var row = 0
  var invalid = Option.empty[(Int, Double)]
  while row < matrix.rows && invalid.isEmpty do
    var col = 0
    while col < matrix.cols && invalid.isEmpty do
      val value = matrix(row, col)
      if !value.isFinite then invalid = Some(row * matrix.cols + col -> value)
      col += 1
    row += 1
  invalid match
    case Some((index, value)) => Left(LinAlgError.InvalidArgument(s"matrix value $index is not finite: $value"))
    case None                 => Right(())

/** Gram-based SVD over a MatrixView; the storage-aware `crossProduct` preserves
  * lazy and sparse views while Gale supplies the dense spectral decomposition.
  * Gram eigenvalues at or below `rankTolerance * lambdaMax` are
  * treated as rank noise — mirroring `EigenGmd`'s convention — so requesting more
  * components than the rank returns fewer, never zero-padded or noise-amplified
  * factors. An exactly zero spectrum yields a rank-0 result with empty factors,
  * leaving the "no components" decision to callers: fits with an empty-result
  * contract (effect operators, CPCA blocks) record it, whole-fit entry points
  * reject it with a typed error.
  */
final case class GramSvdSolver(
    eigenSolver: SymmetricEigenSolver,
    rankTolerance: Double = 1e-12
) extends SvdSolver:
  override def decompose(input: MatrixView, components: ComponentCount): Either[MultivarError, SvdResult] =
    val limit = Math.min(input.rows, input.cols)
    if components.value > limit then Left(MultivarError.InvalidComponentRequest(components.value, limit))
    else
      for
        gram <- input.crossProduct
        _ <- MatrixOps.checkFinite("svd gram", gram)
        eigen <- LinalgErrorAdapter.adapt(eigenSolver.decompose(gram))
        k = keptComponents(eigen.values, components.value)
        v = MatrixOps.takeColumns(eigen.vectors, k)
        scores <- input.rightMultiply(v)
      yield
        val singularValues = new Array[Double](k)
        var col = 0
        while col < k do
          singularValues(col) = Math.sqrt(eigen.values(col))
          col += 1
        val uData = scores.copyData
        col = 0
        while col < k do
          val sv = singularValues(col)
          var row = 0
          while row < scores.rows do
            uData(row * k + col) = scores(row, col) / sv
            row += 1
          col += 1
        SvdResult(GaleNumerics.matrixFromRowMajor(scores.rows, k, uData), GaleNumerics.vectorFromArray(singularValues), v)

  /** Rank cutoff relative to the leading Gram eigenvalue, per EigenGmd's convention;
    * zero when the whole spectrum is rank noise.
    */
  private def keptComponents(values: DVec, requested: Int): Int =
    val cutoff = rankTolerance * Math.max(values(0), 0.0)
    var kept = 0
    while kept < values.length && values(kept) > cutoff do kept += 1
    Math.min(requested, kept)

private[multivar] object LinalgErrorAdapter:
  def adapt[A](result: Either[LinAlgError, A]): Either[MultivarError, A] =
    result.left.map(toMultivarError)

  def toMultivarError(error: LinAlgError): MultivarError =
    error match
      case LinAlgError.DimensionMismatch(expected, actual) =>
        MultivarError.MatrixShapeMismatch(
          s"expected ${expected.rows.value}x${expected.cols.value} but got ${actual.rows.value}x${actual.cols.value}"
        )
      case LinAlgError.VectorLengthMismatch(expected, actual) =>
        MultivarError.MatrixShapeMismatch(s"expected vector length $expected but got $actual")
      case LinAlgError.IndexOutOfBounds(index, limit) =>
        MultivarError.IndexOutOfBounds(IndexAxis.Column, index, limit)
      case LinAlgError.NonSquareMatrix(shape) =>
        MultivarError.MatrixShapeMismatch(s"matrix must be square, got ${shape.rows.value}x${shape.cols.value}")
      case LinAlgError.SingularMatrix(index) =>
        MultivarError.NonInvertibleValue("matrix pivot", index, 0.0)
      case LinAlgError.NotPositiveDefinite(index) =>
        MultivarError.NonInvertibleValue("positive-definite leading minor", index, 0.0)
      case LinAlgError.RankDeficient(rank, cols) =>
        MultivarError.SolverFailed(s"matrix rank $rank is less than required rank $cols")
      case LinAlgError.DidNotConverge(iterations, residual) =>
        MultivarError.SolverFailed(s"solver did not converge after $iterations iterations; residual=$residual")
      case other =>
        MultivarError.SolverFailed(other.getMessage)

private[multivar] object MatrixOps:
  def symmetrize(matrix: DMat): DMat =
    require(matrix.rows == matrix.cols, "symmetrization requires a square matrix")
    val out = new Array[Double](matrix.rows * matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) = 0.5 * (matrix(row, col) + matrix(col, row))
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

  def checkFinite(role: String, matrix: DMat): Either[MultivarError, Unit] =
    var row = 0
    var error = Option.empty[MultivarError]
    while row < matrix.rows && error.isEmpty do
      var col = 0
      while col < matrix.cols && error.isEmpty do
        val value = matrix(row, col)
        if !value.isFinite then error = Some(MultivarError.NonFiniteValue(role, row * matrix.cols + col, value))
        col += 1
      row += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(())

  def checkSymmetric(matrix: DMat, tolerance: Double): Either[MultivarError, Unit] =
    if matrix.rows != matrix.cols then
      Left(MultivarError.MatrixShapeMismatch(s"matrix must be square, got ${matrix.rows}x${matrix.cols}"))
    else
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

  def takeColumns(matrix: DMat, count: Int): DMat =
    GaleNumerics.selectColumns(matrix, (0 until count).toVector)

  def takeVector(vector: DVec, count: Int): DVec =
    vector.slice(0, count).copy

  def diagonal(values: DVec): DMat =
    val out = Matrix.newBuilder(values.length, values.length)
    var index = 0
    while index < values.length do
      out(index, index) = values(index)
      index += 1
    out.result()

  def scale(matrix: DMat, factor: Double): DMat =
    val out = matrix.copyData
    var i = 0
    while i < out.length do
      out(i) *= factor
      i += 1
    GaleNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

  def addRidge(matrix: DMat, ridge: Double): DMat =
    val out = matrix.copyData
    var i = 0
    while i < matrix.rows do
      out(i * matrix.cols + i) += ridge
      i += 1
    GaleNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

  def inverseSquareRoot(
      matrix: DMat,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double
  ): Either[MultivarError, DMat] =
    for
      _ <- checkFinite("inverse-square-root matrix", matrix)
      _ <- checkSymmetric(matrix, tolerance)
      eigen <- LinalgErrorAdapter.adapt(eigenSolver.decompose(matrix))
      _ <-
        var index = 0
        var error = Option.empty[MultivarError]
        while index < eigen.values.length && error.isEmpty do
          if eigen.values(index) <= tolerance then
            error = Some(MultivarError.NonInvertibleValue("positive-definite eigenvalue", index, eigen.values(index)))
          index += 1
        error match
          case Some(value) => Left(value)
          case None        => Right(())
    yield
      val scaled = Matrix.newBuilder(eigen.vectors.rows, eigen.vectors.cols)
      var row = 0
      while row < eigen.vectors.rows do
        var col = 0
        while col < eigen.vectors.cols do
          scaled(row, col) = eigen.vectors(row, col) / Math.sqrt(eigen.values(col))
          col += 1
        row += 1
      GaleNumerics.multiply(scaled.result(), eigen.vectors.t)

  def scaleColumns(matrix: DMat, scale: DVec): DMat =
    require(matrix.cols == scale.length, "scale length must match matrix columns")
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) *= scale(col)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

  def subtract(left: DMat, right: DMat): DMat =
    require(left.rows == right.rows && left.cols == right.cols, "matrix subtraction requires matching shapes")
    val out = left.copyData
    var i = 0
    while i < out.length do
      out(i) -= right(i / right.cols, i % right.cols)
      i += 1
    GaleNumerics.matrixFromRowMajor(left.rows, left.cols, out)

  def traverse[A, B](values: Vector[A])(f: A => Either[MultivarError, B]): Either[MultivarError, Vector[B]] =
    val out = Vector.newBuilder[B]
    var i = 0
    var error = Option.empty[MultivarError]
    while i < values.length && error.isEmpty do
      f(values(i)) match
        case Left(value)  => error = Some(value)
        case Right(value) => out += value
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(out.result())
