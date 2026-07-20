package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector
import scalafim.linalg.LinearAlgebraError

type SymmetricEigenResult = scalafim.linalg.SymmetricEigenResult
val SymmetricEigenResult = scalafim.linalg.SymmetricEigenResult

type SvdResult = scalafim.linalg.SvdResult
val SvdResult = scalafim.linalg.SvdResult

type SymmetricEigenSolver = scalafim.linalg.SymmetricEigenSolver

trait SvdSolver:
  def decompose(input: MatrixView, components: ComponentCount): Either[MultivarError, SvdResult]

type GeneralizedEigenSolver = scalafim.linalg.GeneralizedEigenSolver

object DenseSolvers:
  val symmetricEigen: SymmetricEigenSolver =
    scalafim.linalg.LinalgSolvers.symmetricEigen

  val svd: SvdSolver =
    GramSvdSolver(symmetricEigen)

  val generalizedEigen: GeneralizedEigenSolver =
    scalafim.linalg.LinalgSolvers.generalizedEigen

/** Gram-based SVD over a MatrixView; the storage-aware `crossProduct` is what makes
  * this a legitimate multivar adaptation rather than a re-implementation of
  * `linalg.GramDenseSvdSolver` (which requires a dense input and always returns the
  * requested rank). Gram eigenvalues at or below `rankTolerance * lambdaMax` are
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
        SvdResult(DoubleMatrix.unsafe(scores.rows, k, uData), DoubleVector.unsafe(singularValues), v)

  /** Rank cutoff relative to the leading Gram eigenvalue, per EigenGmd's convention;
    * zero when the whole spectrum is rank noise.
    */
  private def keptComponents(values: DoubleVector, requested: Int): Int =
    val cutoff = rankTolerance * Math.max(values(0), 0.0)
    var kept = 0
    while kept < values.length && values(kept) > cutoff do kept += 1
    Math.min(requested, kept)

private[multivar] object LinalgErrorAdapter:
  def adapt[A](result: Either[LinearAlgebraError, A]): Either[MultivarError, A] =
    result.left.map(toMultivarError)

  def toMultivarError(error: LinearAlgebraError): MultivarError =
    error match
      case LinearAlgebraError.InvalidMatrixShape(rows, cols) =>
        MultivarError.MatrixShapeMismatch(s"invalid matrix shape ${rows}x${cols}")
      case LinearAlgebraError.MatrixTooLarge(rows, cols) =>
        MultivarError.DimensionOverflow(rows, cols)
      case LinearAlgebraError.MatrixStorageLengthMismatch(shape, actual) =>
        MultivarError.MatrixShapeMismatch(s"data length $actual != rows*cols ${shape.entries}")
      case LinearAlgebraError.NonSquareMatrix(rows, cols) =>
        MultivarError.MatrixShapeMismatch(s"matrix must be square, got ${rows}x${cols}")
      case LinearAlgebraError.NonSymmetricMatrix(row, col, left, right) =>
        MultivarError.NonSymmetricMatrix(row, col, left, right)
      case LinearAlgebraError.NonPositiveDefinite(index, value) =>
        MultivarError.NonInvertibleValue("positive-definite eigenvalue", index, value)
      case LinearAlgebraError.RankDeficient(requiredRank, actualRank) =>
        MultivarError.SolverFailed(s"matrix rank $actualRank is less than required rank $requiredRank")
      case LinearAlgebraError.InvalidDecompositionRank(requested, limit) =>
        MultivarError.InvalidComponentRequest(requested, limit)
      case LinearAlgebraError.DimensionMismatch(role, expected, actual) =>
        MultivarError.MatrixShapeMismatch(s"${role.label} expected $expected but got $actual")
      case LinearAlgebraError.IndexOutOfBounds(axis, index, limit) =>
        val mappedAxis =
          axis match
            case scalafim.linalg.MatrixAxis.Row => IndexAxis.Row
            case scalafim.linalg.MatrixAxis.Col => IndexAxis.Column
        MultivarError.IndexOutOfBounds(mappedAxis, index, limit)
      case LinearAlgebraError.InvalidParameter(parameter, value) =>
        MultivarError.InvalidTolerance(parameter.label, value)
      case LinearAlgebraError.NonFiniteValue(role, index, value) =>
        MultivarError.NonFiniteValue(role.label, index, value)
      case LinearAlgebraError.SolverDidNotConverge(method, maxIterations) =>
        MultivarError.SolverFailed(s"$method did not converge within $maxIterations iteration(s)")
      case LinearAlgebraError.OperatorOrderUnsupported(method, order, maximum) =>
        MultivarError.SolverFailed(s"$method supports operator order at most $maximum, got $order")
      case LinearAlgebraError.OperatorApplicationFailed(detail) =>
        MultivarError.SolverFailed(s"linear operator application failed: $detail")
      case LinearAlgebraError.BackendFailure(backend, detail) =>
        MultivarError.SolverFailed(s"$backend backend failed: $detail")

/** Multivar-facing façade over `linalg.DenseDecompositionOps`: identical numeric
  * kernels live in linalg only; this object adapts errors into `MultivarError` and
  * keeps caller-supplied role labels. `scale` and `addRidge` are genuinely
  * multivar-specific (no linalg counterpart) and stay implemented here.
  */
private[multivar] object MatrixOps:
  private val Ops = scalafim.linalg.DenseDecompositionOps

  def checkFinite(role: String, matrix: DoubleMatrix): Either[MultivarError, Unit] =
    Ops.checkFinite(scalafim.linalg.MatrixValueRole.Data, matrix).left.map {
      case LinearAlgebraError.NonFiniteValue(_, index, value) =>
        MultivarError.NonFiniteValue(role, index, value)
      case other =>
        LinalgErrorAdapter.toMultivarError(other)
    }

  def checkSymmetric(matrix: DoubleMatrix, tolerance: Double): Either[MultivarError, Unit] =
    LinalgErrorAdapter.adapt(Ops.checkSymmetric(matrix, tolerance))

  def takeColumns(matrix: DoubleMatrix, count: Int): DoubleMatrix =
    Ops.takeColumns(matrix, count)

  def takeVector(vector: DoubleVector, count: Int): DoubleVector =
    Ops.takeVector(vector, count)

  def diagonal(values: DoubleVector): DoubleMatrix =
    Ops.diagonal(values)

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
    LinalgErrorAdapter.adapt(
      Ops.inverseSquareRoot(matrix, eigenSolver, scalafim.linalg.Tolerance.unsafe(tolerance))
    )

  def scaleColumns(matrix: DoubleMatrix, scale: DoubleVector): DoubleMatrix =
    require(matrix.cols == scale.length, "scale length must match matrix columns")
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) *= scale(col)
        col += 1
      row += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  def subtract(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
    require(left.rows == right.rows && left.cols == right.cols, "matrix subtraction requires matching shapes")
    val out = left.copyData
    var i = 0
    while i < out.length do
      out(i) -= right.dataArray(i)
      i += 1
    DoubleMatrix.unsafe(left.rows, left.cols, out)

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
