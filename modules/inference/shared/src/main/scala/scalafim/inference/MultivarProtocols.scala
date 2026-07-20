package scalafim.inference

import scalafim.linalg.DecompositionRank
import scalafim.linalg.DenseSvdSolver
import scalafim.linalg.DoubleMatrix
import scalafim.linalg.LinalgSolvers
import scalafim.linalg.SvdResult

final case class PcaVarianceState private[inference] (
    residual: DoubleMatrix,
    removed: Int
)

object PcaVarianceState:
  def from(input: DoubleMatrix): Either[InferenceError, PcaVarianceState] =
    ProtocolMatrices.validate("PCA input", input).map { _ =>
      PcaVarianceState(ProtocolMatrices.centerColumns(input), removed = 0)
    }

final case class PcaVarianceProtocol(
    solver: DenseSvdSolver = LinalgSolvers.denseSvd
) extends ExactLadderProtocol[PcaVarianceState, TargetKind.VarianceRoots]:
  override val target: TargetSpec[TargetKind.VarianceRoots] = TargetSpec.VarianceRoots

  override def roots(initial: PcaVarianceState): Either[InferenceError, Vector[Double]] =
    ProtocolMatrices.singularValues(initial.residual, ProtocolMatrices.rankLimit(initial.residual), solver)
      .map(_.map(value => value * value))

  override def observed(state: PcaVarianceState): Either[InferenceError, Double] =
    val total = ProtocolMatrices.frobeniusSquared(state.residual)
    if total <= ProtocolMatrices.zeroTolerance(total) then Right(0.0)
    else ProtocolMatrices.singularValues(state.residual, 1, solver).map { values =>
      values.head * values.head / total
    }

  override def nullStatistic(
      state: PcaVarianceState,
      step: ComponentIx,
      replicate: ReplicateId,
      random: RandomSource
  ): Either[InferenceError, Double] =
    for
      permuted <- ProtocolMatrices.permuteColumns(state.residual, random)
      needed = state.removed + 1
      values <-
        if needed > ProtocolMatrices.rankLimit(permuted) then Right(Vector.empty[Double])
        else ProtocolMatrices.singularValues(permuted, needed, solver)
    yield
      if values.length < needed then 0.0
      else
        val total = ProtocolMatrices.frobeniusSquared(state.residual)
        var previous = 0.0
        var i = 0
        while i < needed - 1 do
          previous += values(i) * values(i)
          i += 1
        val tail = total - previous
        if tail <= ProtocolMatrices.zeroTolerance(total) then 0.0
        else values(needed - 1) * values(needed - 1) / tail

  override def remove(state: PcaVarianceState): Either[InferenceError, PcaVarianceState] =
    ProtocolMatrices.svd(state.residual, 1, solver).map { fit =>
      val residual = ProtocolMatrices.removeRankOne(
        state.residual,
        fit.u,
        fit.singularValues(0),
        fit.v
      )
      PcaVarianceState(ProtocolMatrices.zeroSmall(residual), state.removed + 1)
    }

final case class PlscCovarianceState private[inference] (
    x: DoubleMatrix,
    y: DoubleMatrix,
    removed: Int
)

object PlscCovarianceState:
  def from(x: DoubleMatrix, y: DoubleMatrix): Either[InferenceError, PlscCovarianceState] =
    for
      _ <- ProtocolMatrices.validate("PLSC X", x)
      _ <- ProtocolMatrices.validate("PLSC Y", y)
      _ <-
        if x.rows == y.rows then Right(())
        else Left(InferenceError.RowCountMismatch("PLSC paired blocks", x.rows, y.rows))
    yield PlscCovarianceState(
      ProtocolMatrices.centerColumns(x),
      ProtocolMatrices.centerColumns(y),
      removed = 0
    )

final case class PlscCovarianceProtocol(
    solver: DenseSvdSolver = LinalgSolvers.denseSvd
) extends ExactLadderProtocol[PlscCovarianceState, TargetKind.CovarianceRoots]:
  override val target: TargetSpec[TargetKind.CovarianceRoots] = TargetSpec.CovarianceRoots

  override def roots(initial: PlscCovarianceState): Either[InferenceError, Vector[Double]] =
    val cross = DoubleMatrix.transposeMultiply(initial.x, initial.y)
    ProtocolMatrices.singularValues(cross, ProtocolMatrices.rankLimit(cross), solver)
      .map(_.map(value => value * value))

  override def observed(state: PlscCovarianceState): Either[InferenceError, Double] =
    leadingCrossRoot(state.x, state.y)

  override def nullStatistic(
      state: PlscCovarianceState,
      step: ComponentIx,
      replicate: ReplicateId,
      random: RandomSource
  ): Either[InferenceError, Double] =
    random.permutation(state.y.rows).flatMap { case (permutation, _) =>
      leadingCrossRoot(state.x, state.y.selectRows(permutation))
    }

  override def remove(state: PlscCovarianceState): Either[InferenceError, PlscCovarianceState] =
    val cross = DoubleMatrix.transposeMultiply(state.x, state.y)
    ProtocolMatrices.svd(cross, 1, solver).map { fit =>
      val xNext = ProtocolMatrices.removeFeatureDirection(state.x, fit.u)
      val yNext = ProtocolMatrices.removeFeatureDirection(state.y, fit.v)
      PlscCovarianceState(
        ProtocolMatrices.zeroSmall(xNext),
        ProtocolMatrices.zeroSmall(yNext),
        state.removed + 1
      )
    }

  private def leadingCrossRoot(x: DoubleMatrix, y: DoubleMatrix): Either[InferenceError, Double] =
    ProtocolMatrices.singularValues(DoubleMatrix.transposeMultiply(x, y), 1, solver).map { values =>
      values.head * values.head
    }

private object ProtocolMatrices:
  def validate(role: String, matrix: DoubleMatrix): Either[InferenceError, Unit] =
    if matrix.rows < 2 then Left(InferenceError.InvalidCount(s"$role rows", matrix.rows))
    else if matrix.cols < 1 then Left(InferenceError.InvalidCount(s"$role columns", matrix.cols))
    else
      val values = matrix.copyData
      var i = 0
      while i < values.length do
        if !values(i).isFinite then return Left(InferenceError.NonFiniteStatistic(s"$role entry $i", values(i)))
        i += 1
      Right(())

  def centerColumns(matrix: DoubleMatrix): DoubleMatrix =
    val out = matrix.copyData
    var col = 0
    while col < matrix.cols do
      var mean = 0.0
      var row = 0
      while row < matrix.rows do
        mean += matrix(row, col)
        row += 1
      mean /= matrix.rows
      row = 0
      while row < matrix.rows do
        out(row * matrix.cols + col) -= mean
        row += 1
      col += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  def rankLimit(matrix: DoubleMatrix): Int =
    Math.min(matrix.rows, matrix.cols)

  def singularValues(
      matrix: DoubleMatrix,
      rank: Int,
      solver: DenseSvdSolver
  ): Either[InferenceError, Vector[Double]] =
    svd(matrix, rank, solver).map(_.singularValues.toVector)

  def svd(
      matrix: DoubleMatrix,
      rank: Int,
      solver: DenseSvdSolver
  ): Either[InferenceError, SvdResult] =
    DecompositionRank.bounded(rank, rankLimit(matrix))
      .left.map(error => InferenceError.NumericalFailure("SVD rank", error.message))
      .flatMap { requested =>
        solver.decompose(matrix, requested)
          .left.map(error => InferenceError.NumericalFailure("dense SVD", error.message))
      }

  def frobeniusSquared(matrix: DoubleMatrix): Double =
    val values = matrix.copyData
    var total = 0.0
    var i = 0
    while i < values.length do
      total += values(i) * values(i)
      i += 1
    total

  def zeroTolerance(scale: Double): Double =
    Math.max(1.0, scale) * Math.ulp(1.0)

  def zeroSmall(matrix: DoubleMatrix): DoubleMatrix =
    val scale = frobeniusSquared(matrix)
    if scale <= zeroTolerance(scale) then DoubleMatrix.zeros(matrix.rows, matrix.cols)
    else matrix

  def permuteColumns(
      matrix: DoubleMatrix,
      initial: RandomSource
  ): Either[InferenceError, DoubleMatrix] =
    val out = new Array[Double](matrix.rows * matrix.cols)
    var random = initial
    var col = 0
    while col < matrix.cols do
      random.permutation(matrix.rows) match
        case Left(error) => return Left(error)
        case Right((permutation, next)) =>
          var row = 0
          while row < matrix.rows do
            out(row * matrix.cols + col) = matrix(permutation(row), col)
            row += 1
          random = next
      col += 1
    Right(DoubleMatrix.unsafe(matrix.rows, matrix.cols, out))

  def removeRankOne(
      matrix: DoubleMatrix,
      u: DoubleMatrix,
      singularValue: Double,
      v: DoubleMatrix
  ): DoubleMatrix =
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) -= singularValue * u(row, 0) * v(col, 0)
        col += 1
      row += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  def removeFeatureDirection(matrix: DoubleMatrix, direction: DoubleMatrix): DoubleMatrix =
    val scores = DoubleMatrix.multiply(matrix, direction)
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) -= scores(row, 0) * direction(col, 0)
        col += 1
      row += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)
