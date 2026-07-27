package scalafim.inference

import multivar.core.{BlockPartition, ComponentCount, DenseSolvers, MatrixView, StoragePolicy}
import multivar.family.cpca.{CpcaBlock, CpcaBlockRequest, PreparedCpcaOperatorFit, PreparedCpcaOperatorProblem}

import gale.linalg.DMat
import gale.linalg.DVec

final case class CpcaInferenceState private[inference] (
    problem: PreparedCpcaOperatorProblem,
    block: CpcaBlock,
    components: ComponentCount,
    removed: Int
)

final case class CpcaInferenceFit(
    block: CpcaBlock,
    roots: DVec
)

object CpcaInferenceState:
  def from(
      problem: PreparedCpcaOperatorProblem,
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
    random.permutation(state.problem.rows.descriptor.size).flatMap { case (permutation, _) =>
      for
        dense <- adapt("CPCA table materialization", state.problem.tableDense)
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
              adapt("CPCA table materialization", state.problem.tableDense)
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
            InferenceNumerics.vectorFromSeq(Vector.tabulate(value.d.length) { index =>
              val root = value.d(index)
              root * root
            })
          ))
    }

  private def fitRaw(state: CpcaInferenceState): Either[InferenceError, PreparedCpcaOperatorFit] =
    CpcaBlockRequest.from(
      Vector(state.block),
      defaultComponents = Some(state.components)
    )
      .left.map(error => InferenceError.NumericalFailure("CPCA block request", error.message))
      .flatMap(request =>
        state.problem.fit(
          request,
          eigenSolver = DenseSolvers.symmetricEigen,
          svdSolver = DenseSolvers.svd,
          rankTolerance = rankTolerance,
          policy = StoragePolicy.AllowDense
        ).left.map(error => InferenceError.NumericalFailure("CPCA inference fit", error.message))
      )

  private def rebuild(
      state: CpcaInferenceState,
      table: DMat
  ): Either[InferenceError, CpcaInferenceState] =
    adapt(
      "CPCA null operator problem",
      state.problem.withTable(MatrixView.dense(table), "cpca-inference-resample")
    ).map(problem => state.copy(problem = problem))

  private def adapt[A](
      role: String,
      value: Either[multivar.core.MultivarError, A]
  ): Either[InferenceError, A] =
    value.left.map(error => InferenceError.NumericalFailure(role, error.message))

final case class MultiblockInferenceState private[inference] (
    data: DMat,
    partition: BlockPartition,
    removed: Int
)

final case class MultiblockInferenceFit(
    roots: DVec,
    rowVectors: DMat
)

object MultiblockInferenceState:
  def from(
      data: DMat,
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
    val consensusData = new Array[Double](state.data.rows * state.data.rows)
    var blockIndex = 0
    while blockIndex < state.partition.blocks.length do
      val block = state.partition.blocks(blockIndex)
      val blockData = FamilyBlockMatrices.selectColumns(state.data, block.columns.indices)
      val scale = FamilyBlockMatrices.frobeniusSquared(blockData)
      if scale > Math.ulp(1.0) then
        val gram = InferenceNumerics.multiply(blockData, blockData.transpose)
        FamilyBlockMatrices.addScaled(consensusData, state.data.rows, gram, 1.0 / scale)
      blockIndex += 1

    val consensus = InferenceNumerics.matrixFromRowMajor(
      state.data.rows,
      state.data.rows,
      consensusData
    )
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
        error.toLeft(MultiblockInferenceFit(InferenceNumerics.vectorFromArray(values), eigen.vectors))
      }

  private def independentlyPermuteBlocks(
      state: MultiblockInferenceState,
      initial: RandomSource
  ): Either[InferenceError, DMat] =
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
    Right(InferenceNumerics.matrixFromRowMajor(state.data.rows, state.data.cols, out))

private object FamilyBlockMatrices:
  def validateFinite(role: String, matrix: DMat): Either[InferenceError, Unit] =
    val values = matrix.copyData
    var i = 0
    while i < values.length do
      if !values(i).isFinite then
        return Left(InferenceError.NonFiniteStatistic(s"$role entry $i", values(i)))
      i += 1
    Right(())

  def centerColumns(matrix: DMat): DMat =
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
    InferenceNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

  def selectColumns(matrix: DMat, columns: Vector[Int]): DMat =
    val out = new Array[Double](matrix.rows * columns.length)
    var row = 0
    while row < matrix.rows do
      var local = 0
      while local < columns.length do
        out(row * columns.length + local) = matrix(row, columns(local))
        local += 1
      row += 1
    InferenceNumerics.matrixFromRowMajor(matrix.rows, columns.length, out)

  def frobeniusSquared(matrix: DMat): Double =
    val values = matrix.copyData
    var total = 0.0
    var i = 0
    while i < values.length do
      total += values(i) * values(i)
      i += 1
    total

  def addScaled(left: Array[Double], columns: Int, right: DMat, scale: Double): Unit =
    require(left.length == right.rows * right.cols && columns == right.cols)
    var row = 0
    while row < right.rows do
      var col = 0
      while col < right.cols do
        left(row * columns + col) += scale * right(row, col)
        col += 1
      row += 1

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

  def subtract(left: DMat, right: DMat): DMat =
    require(left.rows == right.rows && left.cols == right.cols)
    val out = left.copyData
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        out(row * left.cols + col) -= right(row, col)
        col += 1
      row += 1
    InferenceNumerics.matrixFromRowMajor(left.rows, left.cols, out)
