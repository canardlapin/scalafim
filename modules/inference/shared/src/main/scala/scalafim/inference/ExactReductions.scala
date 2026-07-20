package scalafim.inference

import scalafim.linalg.DecompositionRank
import scalafim.linalg.DenseSvdSolver
import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector
import scalafim.linalg.LinalgSolvers
import scalafim.linalg.SvdResult

final case class PairedMatrixData private (
    x: DoubleMatrix,
    y: DoubleMatrix
)

object PairedMatrixData:
  def from(
      x: DoubleMatrix,
      y: DoubleMatrix
  ): Either[InferenceError, PairedMatrixData] =
    if x.rows != y.rows then
      Left(InferenceError.RowCountMismatch("paired matrix data", x.rows, y.rows))
    else if x.rows < 2 then Left(InferenceError.InvalidCount("paired matrix rows", x.rows))
    else if x.cols < 1 then Left(InferenceError.InvalidCount("paired X columns", x.cols))
    else if y.cols < 1 then Left(InferenceError.InvalidCount("paired Y columns", y.cols))
    else
      firstNonFinite(x, "paired X")
        .orElse(firstNonFinite(y, "paired Y"))
        .toLeft(PairedMatrixData(x, y))

  private def firstNonFinite(
      matrix: DoubleMatrix,
      role: String
  ): Option[InferenceError] =
    val values = matrix.copyData
    var i = 0
    while i < values.length do
      if !values(i).isFinite then
        return Some(InferenceError.NonFiniteStatistic(s"$role entry $i", values(i)))
      i += 1
    None

final case class CrossCovarianceFit private (
    roots: DoubleVector
)

object CrossCovarianceFit:
  private[inference] def unsafe(values: Vector[Double]): CrossCovarianceFit =
    CrossCovarianceFit(DoubleVector.fromSeq(values))

final case class PairedCrossCore private[inference] (
    xRows: DoubleMatrix,
    xValues: DoubleVector,
    yRows: DoubleMatrix,
    yValues: DoubleVector
)

final case class ReducedCrossFit private[inference] (
    roots: Vector[Double]
)

final case class ExactCrossCovarianceReduction(
    solver: DenseSvdSolver = LinalgSolvers.denseSvd
) extends ExactRefitReduction[PairedMatrixData, CrossCovarianceFit, RowPermutation]:
  override type Core = PairedCrossCore
  override type ReducedFit = ReducedCrossFit

  override val algorithm: String = "paired-thin-svd-cross-v1"

  override def core(
      data: PairedMatrixData,
      observed: CrossCovarianceFit
  ): Either[InferenceError, PairedCrossCore] =
    for
      x <- decompose(data.x)
      y <- decompose(data.y)
    yield PairedCrossCore(x.u, x.singularValues, y.u, y.singularValues)

  override def update(
      core: PairedCrossCore,
      action: RowPermutation
  ): Either[InferenceError, ReducedCrossFit] =
    for
      permutedY <- action.applyTo(core.yRows)
      overlap = DoubleMatrix.transposeMultiply(core.xRows, permutedY)
      reduced = scaleCross(overlap, core.xValues, core.yValues)
      fit <- roots(reduced)
    yield ReducedCrossFit(fit)

  override def lift(reduced: ReducedCrossFit): Either[InferenceError, CrossCovarianceFit] =
    Right(CrossCovarianceFit.unsafe(reduced.roots))

  private def decompose(matrix: DoubleMatrix): Either[InferenceError, SvdResult] =
    val rank = Math.min(matrix.rows, matrix.cols)
    DecompositionRank.bounded(rank, rank)
      .left.map(error => InferenceError.NumericalFailure("exact reduction rank", error.message))
      .flatMap(requested =>
        solver.decompose(matrix, requested)
          .left.map(error => InferenceError.NumericalFailure("exact reduction SVD", error.message))
      )

  private def roots(matrix: DoubleMatrix): Either[InferenceError, Vector[Double]] =
    decompose(matrix).map { fit =>
      Vector.tabulate(fit.singularValues.length) { index =>
        val value = fit.singularValues(index)
        value * value
      }
    }

  private def scaleCross(
      overlap: DoubleMatrix,
      xValues: DoubleVector,
      yValues: DoubleVector
  ): DoubleMatrix =
    val out = new Array[Double](overlap.rows * overlap.cols)
    var row = 0
    while row < overlap.rows do
      var col = 0
      while col < overlap.cols do
        out(row * overlap.cols + col) =
          xValues(row) * overlap(row, col) * yValues(col)
        col += 1
      row += 1
    DoubleMatrix.unsafe(overlap.rows, overlap.cols, out)

object CrossCovarianceRefit:
  def fit(
      data: PairedMatrixData,
      solver: DenseSvdSolver = LinalgSolvers.denseSvd
  ): Either[InferenceError, CrossCovarianceFit] =
    val cross = DoubleMatrix.transposeMultiply(data.x, data.y)
    val rank = Math.min(cross.rows, cross.cols)
    DecompositionRank.bounded(rank, rank)
      .left.map(error => InferenceError.NumericalFailure("cross-covariance rank", error.message))
      .flatMap(requested =>
        solver.decompose(cross, requested)
          .left.map(error => InferenceError.NumericalFailure("cross-covariance SVD", error.message))
      )
      .map { fit =>
        CrossCovarianceFit.unsafe(Vector.tabulate(fit.singularValues.length) { index =>
          val value = fit.singularValues(index)
          value * value
        })
      }

  given refit: Refit[PairedMatrixData, CrossCovarianceFit] with
    override def fit(data: PairedMatrixData): Either[InferenceError, CrossCovarianceFit] =
      CrossCovarianceRefit.fit(data)

  given rowPermutationAction: DataAction[PairedMatrixData, RowPermutation] with
    override def apply(
        data: PairedMatrixData,
        action: RowPermutation
    ): Either[InferenceError, PairedMatrixData] =
      action.applyTo(data.y).flatMap(PairedMatrixData.from(data.x, _))

  given exactReduction: ExactCrossCovarianceReduction =
    ExactCrossCovarianceReduction()
