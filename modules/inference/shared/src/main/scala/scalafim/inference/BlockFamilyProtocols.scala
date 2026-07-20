package scalafim.inference

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector
import scalafim.linalg.LinalgSolvers
import scalafim.linalg.SymmetricEigenSolver
import scalafim.multivar.BlockPartition
import scalafim.multivar.ComponentCount
import scalafim.multivar.Cpca
import scalafim.multivar.CpcaBlock
import scalafim.multivar.CpcaBlockRequest
import scalafim.multivar.CpcaProblem
import scalafim.multivar.DenseSolvers
import scalafim.multivar.DualityDiagram
import scalafim.multivar.MatrixView
import scalafim.multivar.StoragePolicy

final case class CpcaInferenceState private[inference] (
    problem: CpcaProblem,
    block: CpcaBlock,
    components: ComponentCount,
    removed: Int
)

final case class CpcaInferenceFit(
    block: CpcaBlock,
    roots: DoubleVector
)

object CpcaInferenceState:
  def from(
      problem: CpcaProblem,
      block: CpcaBlock,
      components: ComponentCount
  ): Either[InferenceError, CpcaInferenceState] =
    val state = CpcaInferenceState(problem, block, components, removed = 0)
    CpcaBlockProtocol().fit(state).map(_ => state)

final case class CpcaBlockProtocol(
    rankTolerance: Double = 1e-12
) extends ExactLadderProtocol[CpcaInferenceState, TargetKind.ConstrainedInertiaRoots]:
  require(rankTolerance >= 0.0 && rankTolerance.isFinite)

  override val target: TargetSpec[TargetKind.ConstrainedInertiaRoots] =
    TargetSpec.ConstrainedInertiaRoots
  val nullHypothesis: NullSpec[NullKind.RowPermutation] = NullSpec.PermuteRows
  val validity: ValidityClaim = ValidityClaim.Conditional

  override def roots(initial: CpcaInferenceState): Either[InferenceError, Vector[Double]] =
    fit(initial).map(_.roots.toVector)

  override def observed(state: CpcaInferenceState): Either[InferenceError, Double] =
    fit(state).flatMap { value =>
      if value.roots.length == 0 then Right(0.0)
      else Right(value.roots(0))
    }

  override def nullStatistic(
      state: CpcaInferenceState,
      step: ComponentIx,
      replicate: ReplicateId,
      random: RandomSource
  ): Either[InferenceError, Double] =
    random.permutation(state.problem.rows).flatMap { case (permutation, _) =>
      for
        dense <- adapt("CPCA table materialization", state.problem.diagram.table.toDense(StoragePolicy.AllowDense))
        permuted = dense.selectRows(permutation)
        next <- rebuild(state, permuted)
        value <- observed(next)
      yield value
    }

  override def remove(state: CpcaInferenceState): Either[InferenceError, CpcaInferenceState] =
    for
      fitted <- fitRaw(state)
      blockFit <- fitted.block(state.block).toRight(InferenceError.UnsupportedProblem(
        s"CPCA fit did not return requested block ${state.block.label}"
      ))
      residual <-
        if blockFit.rank == 0 then
          Left(InferenceError.RankLoss(1, 0))
        else
          ComponentCount(1)
            .left.map(error => InferenceError.NumericalFailure("CPCA removal component", error.message))
            .flatMap(one => adapt("CPCA block reconstruction", blockFit.reconstructOriginal(Some(one))))
            .flatMap { reconstruction =>
              adapt("CPCA table materialization", state.problem.diagram.table.toDense(StoragePolicy.AllowDense))
                .map(FamilyBlockMatrices.subtract(_, reconstruction))
            }
      next <- rebuild(state, residual)
    yield next.copy(removed = state.removed + 1)

  private[inference] def fit(state: CpcaInferenceState): Either[InferenceError, CpcaInferenceFit] =
    fitRaw(state).flatMap { fitted =>
      fitted.block(state.block) match
        case None => Left(InferenceError.UnsupportedProblem(
          s"CPCA fit did not return requested block ${state.block.label}"
        ))
        case Some(value) =>
          Right(CpcaInferenceFit(
            state.block,
            DoubleVector.fromSeq(Vector.tabulate(value.d.length) { index =>
              val root = value.d(index)
              root * root
            })
          ))
    }

  private def fitRaw(state: CpcaInferenceState): Either[InferenceError, scalafim.multivar.CpcaFit] =
    CpcaBlockRequest.from(
      Vector(state.block),
      defaultComponents = Some(state.components)
    )
      .left.map(error => InferenceError.NumericalFailure("CPCA block request", error.message))
      .flatMap(request =>
        Cpca.fit(
          state.problem,
          request,
          DenseSolvers.symmetricEigen,
          DenseSolvers.svd,
          rankTolerance,
          StoragePolicy.AllowDense
        ).left.map(error => InferenceError.NumericalFailure("CPCA inference fit", error.message))
      )

  private def rebuild(
      state: CpcaInferenceState,
      table: DoubleMatrix
  ): Either[InferenceError, CpcaInferenceState] =
    val current = state.problem.diagram
    for
      diagram <- adapt("CPCA null diagram", DualityDiagram.from(
        MatrixView.dense(table),
        rowMetric = Some(current.rowMetric),
        columnMetric = Some(current.columnMetric),
        rowSpace = Some(current.rowSpace),
        columnSpace = Some(current.columnSpace)
      ))
      problem <- adapt("CPCA null problem", CpcaProblem.from(
        diagram,
        state.problem.rowConstraint,
        state.problem.columnConstraint
      ))
    yield state.copy(problem = problem)

  private def adapt[A](
      role: String,
      value: Either[scalafim.multivar.MultivarError, A]
  ): Either[InferenceError, A] =
    value.left.map(error => InferenceError.NumericalFailure(role, error.message))

final case class MultiblockInferenceState private[inference] (
    data: DoubleMatrix,
    partition: BlockPartition,
    removed: Int
)

final case class MultiblockInferenceFit(
    roots: DoubleVector,
    rowVectors: DoubleMatrix
)

object MultiblockInferenceState:
  def from(
      data: DoubleMatrix,
      partition: BlockPartition
  ): Either[InferenceError, MultiblockInferenceState] =
    if data.rows < 2 then Left(InferenceError.InvalidCount("multiblock rows", data.rows))
    else if data.cols != partition.totalSize.value then
      Left(InferenceError.RowCountMismatch(
        "multiblock partition columns",
        data.cols,
        partition.totalSize.value
      ))
    else
      FamilyBlockMatrices.validateFinite("multiblock data", data).map { _ =>
        MultiblockInferenceState(
          FamilyBlockMatrices.centerColumns(data),
          partition,
          removed = 0
        )
      }

final case class MultiblockConsensusProtocol(
    solver: SymmetricEigenSolver = LinalgSolvers.symmetricEigen,
    zeroTolerance: Double = 1e-10
) extends ExactLadderProtocol[MultiblockInferenceState, TargetKind.MultiblockConsensusRoots]:
  require(zeroTolerance >= 0.0 && zeroTolerance.isFinite)

  override val target: TargetSpec[TargetKind.MultiblockConsensusRoots] =
    TargetSpec.MultiblockConsensusRoots
  val nullHypothesis: NullSpec[NullKind.BlockIndependence] = NullSpec.BreakBlocks
  val validity: ValidityClaim = ValidityClaim.Exact

  override def roots(initial: MultiblockInferenceState): Either[InferenceError, Vector[Double]] =
    fit(initial).map(_.roots.toVector)

  override def observed(state: MultiblockInferenceState): Either[InferenceError, Double] =
    fit(state).map(_.roots(0))

  override def nullStatistic(
      state: MultiblockInferenceState,
      step: ComponentIx,
      replicate: ReplicateId,
      random: RandomSource
  ): Either[InferenceError, Double] =
    independentlyPermuteBlocks(state, random).flatMap(permuted =>
      fit(state.copy(data = permuted)).map(_.roots(0))
    )

  override def remove(
      state: MultiblockInferenceState
  ): Either[InferenceError, MultiblockInferenceState] =
    fit(state).map { fitted =>
      val leading = fitted.rowVectors.col(0)
      state.copy(
        data = FamilyBlockMatrices.residualizeOn(state.data, leading),
        removed = state.removed + 1
      )
    }

  private[inference] def fit(
      state: MultiblockInferenceState
  ): Either[InferenceError, MultiblockInferenceFit] =
    val consensus = DoubleMatrix.zeros(state.data.rows, state.data.rows)
    var blockIndex = 0
    while blockIndex < state.partition.blocks.length do
      val block = state.partition.blocks(blockIndex)
      val blockData = FamilyBlockMatrices.selectColumns(state.data, block.columns.indices)
      val scale = FamilyBlockMatrices.frobeniusSquared(blockData)
      if scale > Math.ulp(1.0) then
        val gram = DoubleMatrix.multiply(blockData, blockData.transpose)
        FamilyBlockMatrices.addScaledInPlace(consensus, gram, 1.0 / scale)
      blockIndex += 1

    solver.decompose(consensus)
      .left.map(error => InferenceError.NumericalFailure("multiblock consensus eigen", error.message))
      .flatMap { eigen =>
        val values = new Array[Double](eigen.values.length)
        var i = 0
        var error = Option.empty[InferenceError]
        while i < values.length && error.isEmpty do
          if eigen.values(i) < -zeroTolerance then
            error = Some(InferenceError.InvalidSpectrum(
              s"multiblock consensus root $i is negative: ${eigen.values(i)}"
            ))
          else values(i) = Math.max(0.0, eigen.values(i))
          i += 1
        error.toLeft(MultiblockInferenceFit(DoubleVector.unsafe(values), eigen.vectors))
      }

  private def independentlyPermuteBlocks(
      state: MultiblockInferenceState,
      initial: RandomSource
  ): Either[InferenceError, DoubleMatrix] =
    val out = state.data.copyData
    var random = initial
    var blockIndex = 1
    while blockIndex < state.partition.blocks.length do
      random.permutation(state.data.rows) match
        case Left(error) => return Left(error)
        case Right((permutation, next)) =>
          val columns = state.partition.blocks(blockIndex).columns.indices
          var row = 0
          while row < state.data.rows do
            var local = 0
            while local < columns.length do
              val col = columns(local)
              out(row * state.data.cols + col) = state.data(permutation(row), col)
              local += 1
            row += 1
          random = next
      blockIndex += 1
    Right(DoubleMatrix.unsafe(state.data.rows, state.data.cols, out))

private object FamilyBlockMatrices:
  def validateFinite(role: String, matrix: DoubleMatrix): Either[InferenceError, Unit] =
    val values = matrix.copyData
    var i = 0
    while i < values.length do
      if !values(i).isFinite then
        return Left(InferenceError.NonFiniteStatistic(s"$role entry $i", values(i)))
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
      mean /= matrix.rows.toDouble
      row = 0
      while row < matrix.rows do
        out(row * matrix.cols + col) -= mean
        row += 1
      col += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  def selectColumns(matrix: DoubleMatrix, columns: Vector[Int]): DoubleMatrix =
    val out = new Array[Double](matrix.rows * columns.length)
    var row = 0
    while row < matrix.rows do
      var local = 0
      while local < columns.length do
        out(row * columns.length + local) = matrix(row, columns(local))
        local += 1
      row += 1
    DoubleMatrix.unsafe(matrix.rows, columns.length, out)

  def frobeniusSquared(matrix: DoubleMatrix): Double =
    val values = matrix.copyData
    var total = 0.0
    var i = 0
    while i < values.length do
      total += values(i) * values(i)
      i += 1
    total

  def addScaledInPlace(left: DoubleMatrix, right: DoubleMatrix, scale: Double): Unit =
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        left.dataArray(row * left.cols + col) += scale * right(row, col)
        col += 1
      row += 1

  def residualizeOn(matrix: DoubleMatrix, score: DoubleVector): DoubleMatrix =
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
      DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  def subtract(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
    require(left.rows == right.rows && left.cols == right.cols)
    val out = left.copyData
    var i = 0
    while i < out.length do
      out(i) -= right.dataArray(i)
      i += 1
    DoubleMatrix.unsafe(left.rows, left.cols, out)
