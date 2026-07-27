package scalafim.inference

import multivar.core.{ComponentCount, MatrixView}
import multivar.family.paired.{Cca, CcaFit}

import gale.linalg.DMat
import gale.linalg.DVec

final case class CcaCorrelationState private[inference] (
    x: DMat,
    y: DMat,
    ridge: Double,
    removed: Int
)

object CcaCorrelationState:
  def from(
      x: DMat,
      y: DMat,
      ridge: Double = 1e-8
  ): Either[InferenceError, CcaCorrelationState] =
    if x.rows != y.rows then
      Left(InferenceError.RowCountMismatch("CCA paired blocks", x.rows, y.rows))
    else if x.rows < 3 then Left(InferenceError.InvalidCount("CCA rows", x.rows))
    else if x.cols < 1 then Left(InferenceError.InvalidCount("CCA X columns", x.cols))
    else if y.cols < 1 then Left(InferenceError.InvalidCount("CCA Y columns", y.cols))
    else if !ridge.isFinite || ridge < 0.0 then
      Left(InferenceError.InvalidTolerance("CCA ridge", ridge))
    else
      FamilyMatrices.validateFinite("CCA X", x)
        .flatMap(_ => FamilyMatrices.validateFinite("CCA Y", y))
        .map(_ => CcaCorrelationState(x, y, ridge, removed = 0))

final case class CcaCorrelationProtocol()
    extends ExactLadderProtocol[CcaCorrelationState, TargetKind.CanonicalCorrelations]:
  override val target: TargetSpec[TargetKind.CanonicalCorrelations] =
    TargetSpec.CanonicalCorrelations
  val nullHypothesis: NullSpec[NullKind.PairedIndependence] = NullSpec.BreakY
  val validity: ValidityClaim = ValidityClaim.Exact

  override def roots(initial: CcaCorrelationState): Either[InferenceError, Vector[Double]] =
    fit(initial.x, initial.y, initial.ridge, rankLimit(initial)).map { value =>
      value.result.singularValues.toVector
    }

  override def observed(state: CcaCorrelationState): Either[InferenceError, Double] =
    leading(state.x, state.y, state.ridge)

  override def nullStatistic(
      state: CcaCorrelationState,
      step: ComponentIx,
      replicate: ReplicateId,
      random: RandomSource
  ): Either[InferenceError, Double] =
    random.permutation(state.y.rows).flatMap { case (permutation, _) =>
      leading(state.x, state.y.selectRows(permutation), state.ridge)
    }

  override def remove(state: CcaCorrelationState): Either[InferenceError, CcaCorrelationState] =
    fit(state.x, state.y, state.ridge, 1).map { fitted =>
      val xScore = fitted.xScores.col(0)
      val yScore = fitted.yScores.col(0)
      CcaCorrelationState(
        FamilyMatrices.residualizeOn(state.x, xScore),
        FamilyMatrices.residualizeOn(state.y, yScore),
        state.ridge,
        state.removed + 1
      )
    }

  private def leading(
      x: DMat,
      y: DMat,
      ridge: Double
  ): Either[InferenceError, Double] =
    fit(x, y, ridge, 1).map(_.result.singularValues(0))

  private def fit(
      x: DMat,
      y: DMat,
      ridge: Double,
      rank: Int
  ): Either[InferenceError, CcaFit] =
    ComponentCount(rank)
      .left.map(error => InferenceError.NumericalFailure("CCA component count", error.message))
      .flatMap(components =>
        Cca.fit(MatrixView.dense(x), MatrixView.dense(y), components, ridge)
          .left.map(error => InferenceError.NumericalFailure("CCA fit", error.message))
      )

  private def rankLimit(state: CcaCorrelationState): Int =
    Math.min(state.x.rows - 1, Math.min(state.x.cols, state.y.cols))

final case class GeneralizedEigenState private[inference] (
    a: DMat,
    b: DMat,
    removed: Int
)

final case class GeneralizedEigenFit(
    roots: DVec,
    vectors: DMat
)

object GeneralizedEigenState:
  def from(
      a: DMat,
      b: DMat,
      solver: GeneralizedEigenSolver = LinalgSolvers.generalizedEigen
  ): Either[InferenceError, GeneralizedEigenState] =
    if a.rows != a.cols then
      Left(InferenceError.NumericalFailure("generalized eigen A", "matrix must be square"))
    else if b.rows != b.cols then
      Left(InferenceError.NumericalFailure("generalized eigen B", "matrix must be square"))
    else if a.rows != b.rows then
      Left(InferenceError.RowCountMismatch("generalized eigen pair", a.rows, b.rows))
    else if a.rows < 1 then Left(InferenceError.InvalidCount("generalized eigen dimension", a.rows))
    else
      for
        _ <- FamilyMatrices.validateFinite("generalized eigen A", a)
        _ <- FamilyMatrices.validateFinite("generalized eigen B", b)
        state = GeneralizedEigenState(a, b, removed = 0)
        _ <- GeneralizedEigenProtocol(solver).fit(state, a.rows)
      yield state

final case class GeneralizedEigenProtocol(
    solver: GeneralizedEigenSolver = LinalgSolvers.generalizedEigen,
    zeroTolerance: Double = 1e-10
) extends ExactLadderProtocol[GeneralizedEigenState, TargetKind.GeneralizedEigenRoots]:
  require(zeroTolerance >= 0.0 && zeroTolerance.isFinite)

  override val target: TargetSpec[TargetKind.GeneralizedEigenRoots] =
    TargetSpec.GeneralizedEigenRoots
  val nullHypothesis: NullSpec[NullKind.RowPermutation] = NullSpec.PermuteRows
  val validity: ValidityClaim = ValidityClaim.Exact

  override def roots(initial: GeneralizedEigenState): Either[InferenceError, Vector[Double]] =
    fit(initial, initial.a.rows).map(_.roots.toVector)

  override def observed(state: GeneralizedEigenState): Either[InferenceError, Double] =
    fit(state, 1).map(_.roots(0))

  override def nullStatistic(
      state: GeneralizedEigenState,
      step: ComponentIx,
      replicate: ReplicateId,
      random: RandomSource
  ): Either[InferenceError, Double] =
    random.permutation(state.a.rows).flatMap { case (permutation, _) =>
      val permuted = FamilyMatrices.symmetricPermutation(state.a, permutation)
      fit(GeneralizedEigenState(permuted, state.b, state.removed), 1).map(_.roots(0))
    }

  override def remove(
      state: GeneralizedEigenState
  ): Either[InferenceError, GeneralizedEigenState] =
    fit(state, 1).map { leading =>
      val lambda = leading.roots(0)
      val vector = leading.vectors.col(0)
      val bVector = FamilyMatrices.multiply(state.b, vector)
      val out = state.a.copyData
      var row = 0
      while row < state.a.rows do
        var col = 0
        while col < state.a.cols do
          out(row * state.a.cols + col) -= lambda * bVector(row) * bVector(col)
          col += 1
        row += 1
      GeneralizedEigenState(
        FamilyMatrices.symmetrize(InferenceNumerics.matrixFromRowMajor(state.a.rows, state.a.cols, out)),
        state.b,
        state.removed + 1
      )
    }

  private[inference] def fit(
      state: GeneralizedEigenState,
      rank: Int
  ): Either[InferenceError, GeneralizedEigenFit] =
    DecompositionRank.bounded(state.a.rows, state.a.rows)
      .left.map(error => InferenceError.NumericalFailure("generalized eigen rank", error.message))
      .flatMap(requested =>
        solver.decompose(state.a, state.b, requested)
          .left.map(error => InferenceError.NumericalFailure("generalized eigen solve", error.message))
      )
      .flatMap(result => validateRoots(result, rank))

  private def validateRoots(
      result: SymmetricEigenResult,
      rank: Int
  ): Either[InferenceError, GeneralizedEigenFit] =
    val values = new Array[Double](rank)
    val vectors = new Array[Double](result.vectors.rows * rank)
    var i = 0
    while i < values.length do
      val value = result.values(i)
      if value < -zeroTolerance then
        return Left(InferenceError.InvalidSpectrum(
          s"generalized eigen root $i is negative: $value"
        ))
      values(i) = Math.max(0.0, value)
      var row = 0
      while row < result.vectors.rows do
        vectors(row * rank + i) = result.vectors(row, i)
        row += 1
      i += 1
    Right(GeneralizedEigenFit(
      InferenceNumerics.vectorFromArray(values),
      InferenceNumerics.matrixFromRowMajor(result.vectors.rows, rank, vectors)
    ))

private object FamilyMatrices:
  def validateFinite(role: String, matrix: DMat): Either[InferenceError, Unit] =
    val values = matrix.copyData
    var i = 0
    while i < values.length do
      if !values(i).isFinite then
        return Left(InferenceError.NonFiniteStatistic(s"$role entry $i", values(i)))
      i += 1
    Right(())

  def residualizeOn(matrix: DMat, score: DVec): DMat =
    var denominator = 0.0
    var row = 0
    while row < matrix.rows do
      denominator += score(row) * score(row)
      row += 1
    if denominator <= Math.ulp(1.0) then matrix
    else
      val coefficients = new Array[Double](matrix.cols)
      var col = 0
      while col < matrix.cols do
        row = 0
        while row < matrix.rows do
          coefficients(col) += score(row) * matrix(row, col)
          row += 1
        coefficients(col) /= denominator
        col += 1
      val out = matrix.copyData
      row = 0
      while row < matrix.rows do
        col = 0
        while col < matrix.cols do
          out(row * matrix.cols + col) -= score(row) * coefficients(col)
          col += 1
        row += 1
      InferenceNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

  def symmetricPermutation(matrix: DMat, permutation: Vector[Int]): DMat =
    val out = new Array[Double](matrix.rows * matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) = matrix(permutation(row), permutation(col))
        col += 1
      row += 1
    InferenceNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

  def multiply(matrix: DMat, vector: DVec): DVec =
    val out = new Array[Double](matrix.rows)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row) += matrix(row, col) * vector(col)
        col += 1
      row += 1
    InferenceNumerics.vectorFromArray(out)

  def symmetrize(matrix: DMat): DMat =
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = row + 1
      while col < matrix.cols do
        val value = 0.5 * (matrix(row, col) + matrix(col, row))
        out(row * matrix.cols + col) = value
        out(col * matrix.cols + row) = value
        col += 1
      row += 1
    InferenceNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)
