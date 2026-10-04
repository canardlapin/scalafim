package scalafim.fmri.design

import gale.linalg.{DMat, DenseDecompositions, LinAlgError}
import gale.spectral.SpectralBackend.given

final case class ColumnContrast(weights: Vector[(ColumnId, Double)]):
  require(weights.nonEmpty, "a contrast must contain at least one weight")
  require(weights.map(_._1).distinct.length == weights.length, "contrast column ids must be unique")

final case class FColumnContrast(rows: Vector[ColumnContrast]):
  require(rows.nonEmpty, "an F contrast must contain at least one row")

/** Why a hypothesis cannot be estimated from one design. Reasons are checked
  * in declaration order.
  */
enum LostEstimability:
  /** A non-zero weight falls on columns that are absent from the run, or whose
    * Euclidean norm is at or below the design's rank cutoff.
    */
  case DroppedColumns(columns: Vector[ColumnId])

  /** The hypothesis leaves the numerical row space. `residual` is the
    * dimensionless norm of a unit hypothesis direction outside that space
    * (for F, the worst direction), compared against the row space's
    * membership tolerance.
    */
  case Aliased(residual: Double)

  /** Every weight is zero. */
  case EmptyHypothesis

enum ContrastEstimability:
  case Estimable
  case Lost(reason: LostEstimability)

final case class TContrastDiagnostic(
    contrast: ColumnContrast,
    estimability: ContrastEstimability,
    varianceOverSigmaSquared: Option[Double]
)

final case class FContrastDiagnostic(
    contrast: FColumnContrast,
    estimability: ContrastEstimability,
    effectiveDf: Int,
    worstDirectionSeOverSigma: Option[Double]
)

/** One dependency among design columns: `sum(coefficient * column) ≈ 0`.
  * `residual` is `||X c|| / (sigmaMax(X) ||c||)`, invariant to rescaling the
  * design and comparable with the rank policy's relative tolerance.
  */
final case class AliasEquation(coefficients: Vector[(ColumnId, Double)], residual: Double)

/** Numerical row space of a matrix under a [[RankPolicy]].
  *
  * `basis` holds the right singular vectors whose singular values exceed
  * `cutoff`; `tolerance` is the dimensionless membership tolerance
  * `relativeTolerance * sigmaMax / sigmaMinKept`, the first-order uncertainty
  * of the computed subspace.
  */
private[design] final case class RowSpace(
    basis: Vector[Array[Double]],
    singular: Array[Double],
    sigmaMax: Double,
    cutoff: Double,
    tolerance: Double
):
  def rank: Int = basis.length

private[design] final case class ContrastRunGeometry(
    ids: Vector[ColumnId],
    matrix: DMat,
    space: RowSpace,
    dropped: Set[ColumnId],
    rankPolicy: RankPolicy
)

final case class ContrastDiagnosticsResult(
    fingerprint: DesignFingerprint,
    selectedRows: Vector[ScanIndex],
    rankPolicy: RankPolicy,
    rank: Int,
    cutoff: Double,
    aliases: Vector[AliasEquation],
    t: Vector[TContrastDiagnostic],
    f: Vector[FContrastDiagnostic],
    commonSigmaAssumption: String = "Unwhitened OLS; reported variances are divided by one common residual sigma squared.",
    private[design] val geometry: ContrastRunGeometry
)

/** Run receipts use [[RunIndex]]: the one-based position of the run in the
  * supplied run vector.
  */
final case class FixedEffectsTDiagnostic(
    contrast: ColumnContrast,
    varianceOverSigmaSquared: Option[Double],
    contributingRuns: Vector[RunIndex],
    excludedRuns: Vector[(RunIndex, ContrastEstimability)],
    assumption: String = "Fixed effects assumes one common residual sigma squared across contributing runs."
)

final case class FixedEffectsFDiagnostic(
    contrast: FColumnContrast,
    worstDirectionSeOverSigma: Option[Double],
    effectiveDf: Int,
    contributingRuns: Vector[RunIndex],
    excludedRuns: Vector[(RunIndex, ContrastEstimability)],
    assumption: String = "Fixed effects assumes one common residual sigma squared across contributing runs."
)

final case class RunNuisanceDiagnostic(run: RunIndex, nuisanceRank: Int, residualDegreesOfFreedom: Int)

final case class ConcatenatedTaskDiagnostic(
    taskColumns: Vector[ColumnId],
    contrast: ColumnContrast,
    varianceOverSigmaSquared: Option[Double],
    contributingRuns: Vector[RunIndex],
    excludedRuns: Vector[RunIndex],
    runNuisance: Vector[RunNuisanceDiagnostic]
)

final case class ConcatenatedTaskFDiagnostic(
    taskColumns: Vector[ColumnId],
    contrast: FColumnContrast,
    worstDirectionSeOverSigma: Option[Double],
    effectiveDf: Int,
    contributingRuns: Vector[RunIndex],
    excludedRuns: Vector[RunIndex],
    runNuisance: Vector[RunNuisanceDiagnostic]
)

/** Variance cost of the selected cosine columns. For an F contrast both
  * variances are taken along one pinned direction: the worst (largest
  * variance) direction of the without-cosines model. When that direction is
  * not unique, the largest with-cosines variance within the tied eigenspace is
  * reported.
  */
final case class HighPassVarianceCost(
    selectedCosineColumns: Vector[ColumnId],
    withCosinesVarianceOverSigmaSquared: Double,
    withoutCosinesVarianceOverSigmaSquared: Double,
    varianceRatio: Double,
    excessVarianceRatio: Double
)

/** General schema-bound, common-sigma OLS contrast diagnostics. The declared
  * [[RankPolicy]] controls every rank, covariance, projection, and alias
  * decision.
  */
object ContrastDiagnostics:

  /** Reject only a rank-saturated selected design. Raw column count is not a
    * degrees-of-freedom calculation: duplicate columns can leave residual
    * degrees of freedom even when p >= n.
    */
  def preflight(
      schema: DesignSchema,
      selectedRows: Vector[ScanIndex] = Vector.empty,
      rankPolicy: RankPolicy = RankPolicy.Default
  ): Either[DesignDiagnosticsError, Unit] =
    for
      selected <- selectedDesign(schema, selectedRows)
      space <- rowSpace(selected._2, rankPolicy)
      _ <-
        if space.rank >= selected._1.length then
          Left(DesignDiagnosticsError.InsufficientResidualDof(selected._1.length, space.rank))
        else Right(())
    yield ()

  /** Separate construction budget for callers that need to stop an
    * accidentally expansive model before allocation.
    */
  def checkColumnBudget(schema: DesignSchema, maximumColumns: Int): Either[DesignDiagnosticsError, Unit] =
    if maximumColumns < 0 then Left(DesignDiagnosticsError.InvalidColumnBudget(maximumColumns))
    else if schema.matrixValues.cols > maximumColumns then
      Left(DesignDiagnosticsError.ColumnBudgetExceeded(schema.matrixValues.cols, maximumColumns))
    else Right(())

  def analyze(
      schema: DesignSchema,
      tContrasts: Vector[ColumnContrast],
      fContrasts: Vector[FColumnContrast],
      selectedRows: Vector[ScanIndex] = Vector.empty,
      rankPolicy: RankPolicy = RankPolicy.Default
  ): Either[DesignDiagnosticsError, ContrastDiagnosticsResult] =
    for
      selected <- selectedDesign(schema, selectedRows)
      space <- rowSpace(selected._2, rankPolicy)
      geometry = ContrastRunGeometry(
        schema.columnIds,
        selected._2,
        space,
        droppedColumns(schema.columnIds, selected._2, space.cutoff),
        rankPolicy
      )
      t <- DesignDiagnostics.traverse(tContrasts): contrast =>
        strictWeights(contrast, geometry.ids)
          .flatMap(evaluateT(contrast, _, geometry, 1.0))
          .map(evaluation => TContrastDiagnostic(contrast, evaluation.estimability, evaluation.variance))
      f <- DesignDiagnostics.traverse(fContrasts): contrast =>
        DesignDiagnostics
          .traverse(contrast.rows)(strictWeights(_, geometry.ids))
          .flatMap(evaluateF(contrast, _, geometry, 1.0))
          .flatMap: evaluation =>
            evaluation.covariance match
              case None => Right(FContrastDiagnostic(contrast, evaluation.estimability, 0, None))
              case Some(covariance) =>
                largestEigenvalue(covariance).map: value =>
                  FContrastDiagnostic(contrast, evaluation.estimability, covariance.rows, Some(math.sqrt(value)))
    yield ContrastDiagnosticsResult(
      schema.fingerprint,
      selected._1,
      rankPolicy,
      space.rank,
      space.cutoff,
      aliases(geometry),
      t,
      f,
      geometry = geometry
    )

  /** Fixed-effects (inverse-variance) combination of one t contrast. Each run
    * is evaluated from its own design geometry, so equivalent spellings of the
    * contrast (reordered weights, explicit zeros) combine identically.
    */
  def fixedEffectsT(
      perRun: Vector[ContrastDiagnosticsResult],
      contrast: ColumnContrast
  ): Either[DesignDiagnosticsError, FixedEffectsTDiagnostic] =
    combineFixedEffectsT(perRun, contrast, Vector.fill(perRun.length)(1.0)).map: combined =>
      FixedEffectsTDiagnostic(contrast, combined.variance, combined.contributing, combined.excluded)

  def fixedEffectsF(
      perRun: Vector[ContrastDiagnosticsResult],
      contrast: FColumnContrast
  ): Either[DesignDiagnosticsError, FixedEffectsFDiagnostic] =
    combineFixedEffectsF(perRun, contrast, Vector.fill(perRun.length)(1.0)).map: combined =>
      FixedEffectsFDiagnostic(contrast, combined.worstSe, combined.effectiveDf, combined.contributing, combined.excluded)

  /** Uses `sum_r T_r'(I - N_r N_r+)T_r`. This deliberately excludes a run
    * whose task is wholly nuisance-confounded, rather than inverting a
    * generalized inverse task covariance block and inventing information.
    */
  def concatenatedTaskT(
      runs: Vector[ContrastDiagnosticsResult],
      taskColumns: Vector[ColumnId],
      contrast: ColumnContrast
  ): Either[DesignDiagnosticsError, ConcatenatedTaskDiagnostic] =
    combineConcatenatedT(runs, taskColumns, contrast, Vector.fill(runs.length)(1.0)).map: combined =>
      ConcatenatedTaskDiagnostic(taskColumns, contrast, combined.variance, combined.contributing, combined.excluded, combined.runNuisance)

  def concatenatedTaskF(
      runs: Vector[ContrastDiagnosticsResult],
      taskColumns: Vector[ColumnId],
      contrast: FColumnContrast
  ): Either[DesignDiagnosticsError, ConcatenatedTaskFDiagnostic] =
    combineConcatenatedF(runs, taskColumns, contrast, Vector.fill(runs.length)(1.0)).map: combined =>
      ConcatenatedTaskFDiagnostic(
        taskColumns,
        contrast,
        combined.worstSe,
        combined.effectiveDf,
        combined.contributing,
        combined.excluded,
        combined.runNuisance
      )

  def highPassVarianceRatio(
      withCosines: ContrastDiagnosticsResult,
      withoutCosines: ContrastDiagnosticsResult,
      contrast: ColumnContrast,
      selectedCosineColumns: Vector[ColumnId]
  ): Either[DesignDiagnosticsError, HighPassVarianceCost] =
    for
      _ <- highPassPair(withCosines, withoutCosines, selectedCosineColumns)
      withWeights <- strictWeights(contrast, withCosines.geometry.ids)
      withoutWeights <- strictWeights(contrast, withoutCosines.geometry.ids)
      withEvaluation <- evaluateT(contrast, withWeights, withCosines.geometry, 1.0)
      withoutEvaluation <- evaluateT(contrast, withoutWeights, withoutCosines.geometry, 1.0)
      variances <- (withEvaluation.variance, withoutEvaluation.variance) match
        case (Some(withValue), Some(withoutValue)) if withoutValue > 0.0 => Right(withValue -> withoutValue)
        case _ => Left(DesignDiagnosticsError.HighPassContrastNotEstimable)
    yield cost(selectedCosineColumns, variances._1, variances._2)

  /** Variance ratio along the worst direction of the without-cosines model
    * (see [[HighPassVarianceCost]]). Both covariances use one orthonormal
    * hypothesis basis computed in the without-cosines coordinates.
    */
  def highPassFVarianceRatio(
      withCosines: ContrastDiagnosticsResult,
      withoutCosines: ContrastDiagnosticsResult,
      contrast: FColumnContrast,
      selectedCosineColumns: Vector[ColumnId]
  ): Either[DesignDiagnosticsError, HighPassVarianceCost] =
    val without = withoutCosines.geometry
    val withGeometry = withCosines.geometry
    for
      _ <- highPassPair(withCosines, withoutCosines, selectedCosineColumns)
      rows <- DesignDiagnostics.traverse(contrast.rows)(strictWeights(_, without.ids))
      withoutEvaluation <- evaluateF(contrast, rows, without, 1.0)
      withoutCovariance <- withoutEvaluation.covariance.toRight(DesignDiagnosticsError.HighPassContrastNotEstimable)
      embedded = withoutEvaluation.basis.map(row => embed(row, without.ids, withGeometry.ids))
      outside <- outsideFraction(embedded, withGeometry.space)
      withCovariance <-
        if outside > withGeometry.space.tolerance then Left(DesignDiagnosticsError.HighPassContrastNotEstimable)
        else Right(covariance(embedded, withGeometry.space, 1.0))
      pinned <- pinnedWorstDirection(withoutCovariance, withCovariance, without.rankPolicy)
      _ <- if pinned._2 > 0.0 then Right(()) else Left(DesignDiagnosticsError.HighPassContrastNotEstimable)
    yield cost(selectedCosineColumns, pinned._1, pinned._2)

  // Combination kernels shared with WeightedContrastDiagnostics. `sigmas` are
  // validated positive run residual standard deviations; all-ones sigmas give
  // the common-sigma (variance over sigma squared) receipts.

  private[design] final case class CombinedT(
      variance: Option[Double],
      contributing: Vector[RunIndex],
      excluded: Vector[(RunIndex, ContrastEstimability)]
  )

  private[design] final case class CombinedF(
      worstSe: Option[Double],
      effectiveDf: Int,
      contributing: Vector[RunIndex],
      excluded: Vector[(RunIndex, ContrastEstimability)]
  )

  private[design] final case class CombinedConcatenated(
      variance: Option[Double],
      contributing: Vector[RunIndex],
      excluded: Vector[RunIndex],
      runNuisance: Vector[RunNuisanceDiagnostic]
  )

  private[design] final case class CombinedConcatenatedF(
      worstSe: Option[Double],
      effectiveDf: Int,
      contributing: Vector[RunIndex],
      excluded: Vector[RunIndex],
      runNuisance: Vector[RunNuisanceDiagnostic]
  )

  private[design] def combineFixedEffectsT(
      runs: Vector[ContrastDiagnosticsResult],
      contrast: ColumnContrast,
      sigmas: Vector[Double]
  ): Either[DesignDiagnosticsError, CombinedT] =
    for
      _ <- commonPolicy(runs)
      evaluated <- DesignDiagnostics.traverse(runs.indices.toVector): index =>
        val geometry = runs(index).geometry
        lenientWeights(contrast, geometry.ids).flatMap: (weights, absent) =>
          val evaluation =
            if absent.nonEmpty then Right(Evaluation(lost(LostEstimability.DroppedColumns(absent)), None))
            else evaluateT(contrast, weights, geometry, sigmas(index))
          evaluation.map(runIndex(index) -> _)
    yield
      val accepted = evaluated.collect { case (run, Evaluation(_, Some(variance))) => run -> variance }
      val excluded = evaluated.collect { case (run, Evaluation(estimability, None)) => run -> estimability }
      var information = 0.0
      accepted.foreach((_, variance) => information += 1.0 / variance)
      CombinedT(if accepted.isEmpty then None else Some(1.0 / information), accepted.map(_._1), excluded)

  private[design] def combineFixedEffectsF(
      runs: Vector[ContrastDiagnosticsResult],
      contrast: FColumnContrast,
      sigmas: Vector[Double]
  ): Either[DesignDiagnosticsError, CombinedF] =
    for
      _ <- commonPolicy(runs)
      evaluated <- DesignDiagnostics.traverse(runs.indices.toVector): index =>
        val geometry = runs(index).geometry
        DesignDiagnostics.traverse(contrast.rows)(lenientWeights(_, geometry.ids)).flatMap: rows =>
          val absent = rows.flatMap(_._2).distinct
          val evaluation =
            if absent.nonEmpty then Right(FEvaluation(lost(LostEstimability.DroppedColumns(absent)), Vector.empty, None))
            else evaluateF(contrast, rows.map(_._1), geometry, sigmas(index))
          evaluation.map(runIndex(index) -> _)
      accepted = evaluated.collect { case (run, FEvaluation(_, _, Some(covariance))) => run -> covariance }
      excluded = evaluated.collect { case (run, FEvaluation(estimability, _, None)) => run -> estimability }
      dimensions = accepted.map(_._2.rows).distinct
      result <-
        if accepted.isEmpty then Right(CombinedF(None, 0, Vector.empty, excluded))
        // A shared rank policy can still give runs different hypothesis
        // dimensions; fixed effects then has no common coordinate system.
        else if dimensions.length > 1 then Left(DesignDiagnosticsError.HypothesisDimensionMismatch(dimensions))
        else
          for
            combined <- combineCovariances(accepted)
            worst <- largestEigenvalue(combined)
          yield CombinedF(Some(math.sqrt(worst)), combined.rows, accepted.map(_._1), excluded)
    yield result

  private[design] def combineConcatenatedT(
      runs: Vector[ContrastDiagnosticsResult],
      taskColumns: Vector[ColumnId],
      contrast: ColumnContrast,
      sigmas: Vector[Double]
  ): Either[DesignDiagnosticsError, CombinedConcatenated] =
    for
      _ <- validateTaskColumns(taskColumns)
      weights <- strictWeights(contrast, taskColumns)
      task <- concatenate(runs, taskColumns, sigmas)
      variance <- (task.space, normalized(weights)) match
        case (Some(space), Some((unit, _))) =>
          outsideFraction(Vector(unit), space).map: outside =>
            if outside > space.tolerance then None else Some(quadratic(weights, space))
        case _ => Right(None)
    yield CombinedConcatenated(variance, task.contributing, task.excluded, task.runNuisance)

  private[design] def combineConcatenatedF(
      runs: Vector[ContrastDiagnosticsResult],
      taskColumns: Vector[ColumnId],
      contrast: FColumnContrast,
      sigmas: Vector[Double]
  ): Either[DesignDiagnosticsError, CombinedConcatenatedF] =
    for
      _ <- validateTaskColumns(taskColumns)
      rows <- DesignDiagnostics.traverse(contrast.rows)(strictWeights(_, taskColumns))
      policy <- commonPolicy(runs)
      basis <- orthonormalRows(rows, taskColumns.length, policy)
      task <- concatenate(runs, taskColumns, sigmas)
      worst <- task.space match
        case Some(space) if basis.nonEmpty =>
          outsideFraction(basis, space).flatMap: outside =>
            if outside > space.tolerance then Right(None)
            else largestEigenvalue(covariance(basis, space, 1.0)).map(value => Some(math.sqrt(value)))
        case _ => Right(None)
    yield CombinedConcatenatedF(worst, if worst.isDefined then basis.length else 0, task.contributing, task.excluded, task.runNuisance)

  /** All combined runs must declare one rank policy; returns it (the default
    * policy for an empty run vector).
    */
  private[design] def commonPolicy(runs: Vector[ContrastDiagnosticsResult]): Either[DesignDiagnosticsError, RankPolicy] =
    runs.map(_.rankPolicy).distinct match
      case Vector() => Right(RankPolicy.Default)
      case Vector(policy) => Right(policy)
      case policies => Left(DesignDiagnosticsError.RankPolicyMismatch(policies.map(_.label)))

  // Single-design evaluation.

  private final case class Evaluation(estimability: ContrastEstimability, variance: Option[Double])

  /** `basis` is the orthonormal hypothesis basis (empty when the hypothesis is
    * empty or has dropped columns); `covariance` is present iff estimable.
    */
  private final case class FEvaluation(
      estimability: ContrastEstimability,
      basis: Vector[Array[Double]],
      covariance: Option[DMat]
  )

  private def lost(reason: LostEstimability): ContrastEstimability = ContrastEstimability.Lost(reason)

  private def evaluateT(
      contrast: ColumnContrast,
      weights: Array[Double],
      geometry: ContrastRunGeometry,
      sigma: Double
  ): Either[DesignDiagnosticsError, Evaluation] =
    val dropped = droppedWeights(Vector(contrast), geometry)
    if dropped.nonEmpty then Right(Evaluation(lost(LostEstimability.DroppedColumns(dropped)), None))
    else
      normalized(weights) match
        case None => Right(Evaluation(lost(LostEstimability.EmptyHypothesis), None))
        case Some((unit, _)) =>
          outsideFraction(Vector(unit), geometry.space).map: residual =>
            if residual > geometry.space.tolerance then Evaluation(lost(LostEstimability.Aliased(residual)), None)
            else Evaluation(ContrastEstimability.Estimable, Some(quadratic(weights, geometry.space) * sigma * sigma))

  private def evaluateF(
      contrast: FColumnContrast,
      rows: Vector[Array[Double]],
      geometry: ContrastRunGeometry,
      sigma: Double
  ): Either[DesignDiagnosticsError, FEvaluation] =
    val dropped = droppedWeights(contrast.rows, geometry)
    if dropped.nonEmpty then Right(FEvaluation(lost(LostEstimability.DroppedColumns(dropped)), Vector.empty, None))
    else
      orthonormalRows(rows, geometry.ids.length, geometry.rankPolicy).flatMap: basis =>
        if basis.isEmpty then Right(FEvaluation(lost(LostEstimability.EmptyHypothesis), basis, None))
        else
          outsideFraction(basis, geometry.space).map: residual =>
            if residual > geometry.space.tolerance then FEvaluation(lost(LostEstimability.Aliased(residual)), basis, None)
            else FEvaluation(ContrastEstimability.Estimable, basis, Some(covariance(basis, geometry.space, sigma * sigma)))

  private def droppedWeights(rows: Vector[ColumnContrast], geometry: ContrastRunGeometry): Vector[ColumnId] =
    rows.flatMap(_.weights.collect { case (id, weight) if weight != 0.0 && geometry.dropped.contains(id) => id }).distinct

  private def droppedColumns(ids: Vector[ColumnId], matrix: DMat, cutoff: Double): Set[ColumnId] =
    ids.indices.filter(column => matrix.col(column).norm2 <= cutoff).map(ids).toSet

  // Concatenated task information.

  private final case class ConcatenatedTask(
      space: Option[RowSpace],
      contributing: Vector[RunIndex],
      excluded: Vector[RunIndex],
      runNuisance: Vector[RunNuisanceDiagnostic]
  )

  private final case class RunTask(run: RunIndex, nuisance: Option[RunNuisanceDiagnostic], residual: Option[Array[Double]])

  private def concatenate(
      runs: Vector[ContrastDiagnosticsResult],
      taskColumns: Vector[ColumnId],
      sigmas: Vector[Double]
  ): Either[DesignDiagnosticsError, ConcatenatedTask] =
    for
      _ <- validateTaskColumns(taskColumns)
      policy <- commonPolicy(runs)
      perRun <- DesignDiagnostics.traverse(runs.indices.toVector)(index => runTask(runs(index).geometry, index, taskColumns, sigmas(index), policy))
      stacked = perRun.flatMap(_.residual)
      space <-
        if stacked.isEmpty then Right(None)
        else rowSpace(stack(stacked, taskColumns.length), policy).map(Some(_))
    yield ConcatenatedTask(
      space,
      perRun.collect { case RunTask(run, _, Some(_)) => run },
      perRun.collect { case RunTask(run, _, None) => run },
      perRun.flatMap(_.nuisance)
    )

  private def validateTaskColumns(taskColumns: Vector[ColumnId]): Either[DesignDiagnosticsError, Unit] =
    if taskColumns.isEmpty then Left(DesignDiagnosticsError.EmptyTaskColumns)
    else
      taskColumns.diff(taskColumns.distinct).headOption match
        case Some(duplicate) => Left(DesignDiagnosticsError.DuplicateTaskColumn(duplicate))
        case None => Right(())

  private def runTask(
      geometry: ContrastRunGeometry,
      index: Int,
      taskColumns: Vector[ColumnId],
      sigma: Double,
      policy: RankPolicy
  ): Either[DesignDiagnosticsError, RunTask] =
    val task = taskColumns.map(geometry.ids.indexOf)
    if task.exists(_ < 0) then Right(RunTask(runIndex(index), None, None))
    else
      residualizeTask(geometry.matrix, task, policy).map: (nuisanceRank, residual) =>
        val diagnostic = RunNuisanceDiagnostic(runIndex(index), nuisanceRank, geometry.matrix.rows - geometry.space.rank)
        val contributes = hasNumericalSignal(residual, geometry.matrix, task, policy)
        RunTask(runIndex(index), Some(diagnostic), if contributes then Some(scaleInPlace(residual, 1.0 / sigma)) else None)

  /** Residualize the task columns against the run's nuisance columns using the
    * nuisance SVD's own economy `U`: `T - U_k (U_k' T)`. Returns the nuisance
    * numerical rank and the row-major `n x t` residual.
    */
  private def residualizeTask(x: DMat, task: Vector[Int], policy: RankPolicy): Either[DesignDiagnosticsError, (Int, Array[Double])] =
    val n = x.rows
    val t = task.length
    val residual = new Array[Double](n * t)
    var row = 0
    while row < n do
      var column = 0
      while column < t do
        residual(row * t + column) = x(row, task(column))
        column += 1
      row += 1
    val taskSet = task.toSet
    val nuisance = (0 until x.cols).filterNot(taskSet.contains).toVector
    if nuisance.isEmpty then Right(0 -> residual)
    else
      DMat.tabulate(n, nuisance.length)((r, c) => x(r, nuisance(c))).svd.left.map(numerical).map: svd =>
        val kept = keptCount(svd.singularValues(_), svd.size, policy.cutoff(n, nuisance.length, if svd.size == 0 then 0.0 else svd.singularValues(0)))
        val projection = new Array[Double](t)
        var component = 0
        while component < kept do
          java.util.Arrays.fill(projection, 0.0)
          var r = 0
          while r < n do
            val u = svd.u(r, component)
            var column = 0
            while column < t do
              projection(column) += u * residual(r * t + column)
              column += 1
            r += 1
          r = 0
          while r < n do
            val u = svd.u(r, component)
            var column = 0
            while column < t do
              residual(r * t + column) -= u * projection(column)
              column += 1
            r += 1
          component += 1
        kept -> residual

  private def hasNumericalSignal(residual: Array[Double], original: DMat, task: Vector[Int], policy: RankPolicy): Boolean =
    val values = Array.tabulate(original.rows * task.length)(index => original(index / task.length, task(index % task.length)))
    DesignDiagnostics.norm(residual) > policy.cutoff(original.rows, task.length, DesignDiagnostics.norm(values))

  private def scaleInPlace(values: Array[Double], factor: Double): Array[Double] =
    var index = 0
    while index < values.length do
      values(index) *= factor
      index += 1
    values

  private def stack(blocks: Vector[Array[Double]], columns: Int): DMat =
    val all = new Array[Double](blocks.iterator.map(_.length).sum)
    var offset = 0
    blocks.foreach: block =>
      System.arraycopy(block, 0, all, offset, block.length)
      offset += block.length
    DMat.tabulate(all.length / columns, columns)((row, column) => all(row * columns + column))

  // Geometry kernels.

  private def selectedDesign(
      schema: DesignSchema,
      selectedRows: Vector[ScanIndex]
  ): Either[DesignDiagnosticsError, (Vector[ScanIndex], DMat)] =
    val rows = if selectedRows.isEmpty then schema.rows.selectedRows else selectedRows
    val source = schema.matrixValues
    if rows.isEmpty then Left(DesignDiagnosticsError.EmptyRows)
    else if rows.distinct.length != rows.length then Left(DesignDiagnosticsError.DuplicateRows)
    else
      rows.find(row => row.oneBased < 1 || row.oneBased > source.rows) match
        case Some(row) => Left(DesignDiagnosticsError.RowOutOfBounds(row, source.rows))
        case None =>
          val matrix = DMat.tabulate(rows.length, source.cols)((row, column) => source(rows(row).zeroBased, column))
          DesignDiagnostics.firstNonFinite(matrix) match
            case Some((row, column)) => Left(DesignDiagnosticsError.NonFiniteValue(rows(row), schema.columnIds(column), matrix(row, column)))
            case None => Right(rows -> matrix)

  private def keptCount(singular: Int => Double, size: Int, cutoff: Double): Int =
    var kept = 0
    while kept < size && singular(kept) > cutoff do kept += 1
    kept

  private[design] def rowSpace(x: DMat, policy: RankPolicy): Either[DesignDiagnosticsError, RowSpace] =
    x.svd.left.map(numerical).map: svd =>
      val sigmaMax = if svd.size == 0 then 0.0 else svd.singularValues(0)
      val cutoff = policy.cutoff(x.rows, x.cols, sigmaMax)
      val kept = keptCount(svd.singularValues(_), svd.size, cutoff)
      val basis = Vector.tabulate(kept)(component => Array.tabulate(x.cols)(column => svd.vt(component, column)))
      val singular = Array.tabulate(kept)(component => svd.singularValues(component))
      val relative = policy.relativeTolerance(x.rows, x.cols)
      val tolerance = if kept == 0 then relative else relative * (sigmaMax / singular(kept - 1))
      RowSpace(basis, singular, sigmaMax, cutoff, tolerance)

  /** Orthonormal basis of the hypothesis row space. Rows are first scaled to
    * unit norm (a row's scale does not change the hypothesis), then the SVD
    * keeps directions above the rank policy's cutoff, so duplicate and
    * numerically dependent rows collapse consistently.
    */
  private def orthonormalRows(rows: Vector[Array[Double]], columns: Int, policy: RankPolicy): Either[DesignDiagnosticsError, Vector[Array[Double]]] =
    val units = rows.flatMap(normalized(_).map(_._1))
    if units.isEmpty then Right(Vector.empty)
    else rowSpace(DMat.tabulate(units.length, columns)((row, column) => units(row)(column)), policy).map(_.basis)

  /** Unit vector and original norm, or `None` for a zero vector. */
  private def normalized(values: Array[Double]): Option[(Array[Double], Double)] =
    val length = DesignDiagnostics.norm(values)
    if length == 0.0 then None
    else
      val out = new Array[Double](values.length)
      var index = 0
      while index < values.length do
        out(index) = values(index) / length
        index += 1
      Some(out -> length)

  /** Largest singular value of `B (I - V_k V_k')` for orthonormal rows `B`:
    * the worst-case norm of a unit hypothesis direction outside the row space.
    */
  private def outsideFraction(basis: Vector[Array[Double]], space: RowSpace): Either[DesignDiagnosticsError, Double] =
    if basis.isEmpty then Right(0.0)
    else
      val columns = basis.head.length
      val residual = new Array[Double](basis.length * columns)
      var row = 0
      while row < basis.length do
        System.arraycopy(basis(row), 0, residual, row * columns, columns)
        var component = 0
        while component < space.rank do
          val direction = space.basis(component)
          var dot = 0.0
          var column = 0
          while column < columns do
            dot += residual(row * columns + column) * direction(column)
            column += 1
          column = 0
          while column < columns do
            residual(row * columns + column) -= dot * direction(column)
            column += 1
          component += 1
        row += 1
      if basis.length == 1 then Right(DesignDiagnostics.norm(residual))
      else
        DMat
          .tabulate(basis.length, columns)((r, c) => residual(r * columns + c))
          .svd
          .left
          .map(numerical)
          .map(svd => if svd.size == 0 then 0.0 else svd.singularValues(0))

  /** `w' (X'X)+ w` restricted to the kept spectrum. */
  private def quadratic(weights: Array[Double], space: RowSpace): Double =
    var total = 0.0
    var component = 0
    while component < space.rank do
      val direction = space.basis(component)
      var dot = 0.0
      var column = 0
      while column < weights.length do
        dot += weights(column) * direction(column)
        column += 1
      val scaled = dot / space.singular(component)
      total += scaled * scaled
      component += 1
    total

  /** `scale * B (X'X)+ B'` for hypothesis rows `B`, formed as `A A'` with
    * `A = B V_k S_k^-1`.
    */
  private def covariance(rows: Vector[Array[Double]], space: RowSpace, scale: Double): DMat =
    val q = rows.length
    val k = space.rank
    val a = new Array[Double](q * k)
    var row = 0
    while row < q do
      val source = rows(row)
      var component = 0
      while component < k do
        val direction = space.basis(component)
        var dot = 0.0
        var column = 0
        while column < source.length do
          dot += source(column) * direction(column)
          column += 1
        a(row * k + component) = dot / space.singular(component)
        component += 1
      row += 1
    val out = new Array[Double](q * q)
    var i = 0
    while i < q do
      var j = 0
      while j <= i do
        var sum = 0.0
        var component = 0
        while component < k do
          sum += a(i * k + component) * a(j * k + component)
          component += 1
        out(i * q + j) = sum * scale
        out(j * q + i) = sum * scale
        j += 1
      i += 1
    DMat.tabulate(q, q)((r, c) => out(r * q + c))

  /** Inverse-variance combination `(sum_r C_r^-1)^-1` through Gale Cholesky
    * solves. A covariance that is not positive definite is a typed failure.
    */
  private def combineCovariances(values: Vector[(RunIndex, DMat)]): Either[DesignDiagnosticsError, DMat] =
    val n = values.head._2.rows
    val identity = DMat.eye(n)
    def inverse(matrix: DMat, label: String): Either[DesignDiagnosticsError, DMat] =
      DenseDecompositions
        .cholesky(matrix)
        .flatMap(_.solve(identity))
        .left
        .map(error => DesignDiagnosticsError.NumericalFailure(s"$label is not numerically positive definite: ${error.getMessage}"))
    DesignDiagnostics
      .traverse(values)((run, matrix) => inverse(matrix, s"F hypothesis covariance for run ${run.oneBased}"))
      .flatMap: inverses =>
        val information = new Array[Double](n * n)
        inverses.foreach: inverted =>
          var index = 0
          while index < information.length do
            information(index) += inverted(index / n, index % n)
            index += 1
        inverse(DMat.tabulate(n, n)((r, c) => 0.5 * (information(r * n + c) + information(c * n + r))), "combined F information")

  private def largestEigenvalue(covariance: DMat): Either[DesignDiagnosticsError, Double] =
    covariance.svd.left.map(numerical).flatMap: svd =>
      val value = if svd.size == 0 then Double.NaN else svd.singularValues(0)
      if value.isFinite && value >= 0.0 then Right(value)
      else Left(DesignDiagnosticsError.NumericalFailure("F hypothesis covariance has no finite non-negative spectrum"))

  /** Returns `(withVariance, withoutVariance)` along the without-cosines worst
    * direction. A top eigenvalue tied within the rank policy's relative
    * tolerance makes that direction non-unique; the largest with-cosines
    * variance over the tied eigenspace is then reported.
    */
  private def pinnedWorstDirection(
      without: DMat,
      withCosines: DMat,
      policy: RankPolicy
  ): Either[DesignDiagnosticsError, (Double, Double)] =
    without.svd.left.map(numerical).flatMap: svd =>
      val q = without.rows
      val top = if svd.size == 0 then 0.0 else svd.singularValues(0)
      val tie = policy.relativeTolerance(q, q) * top
      var tied = 0
      while tied < svd.size && top - svd.singularValues(tied) <= tie do tied += 1
      val projected = DMat.tabulate(tied, tied): (a, b) =>
        var sum = 0.0
        var i = 0
        while i < q do
          var j = 0
          while j < q do
            sum += svd.u(i, a) * withCosines(i, j) * svd.u(j, b)
            j += 1
          i += 1
        sum
      if tied == 1 then Right(projected(0, 0) -> top)
      else largestEigenvalue(projected).map(_ -> top)

  /** Dependencies from the RREF of the null-space projector `I - V_k V_k'`.
    * Economy SVD omits wide-null directions; the projector recovers them. At
    * most `p - rank` pivots are taken, and entries at or below the row space's
    * membership tolerance are treated as zero.
    */
  private def aliases(geometry: ContrastRunGeometry): Vector[AliasEquation] =
    val ids = geometry.ids
    val space = geometry.space
    val p = ids.length
    val nullity = p - space.rank
    if nullity <= 0 then Vector.empty
    else
      val projector = new Array[Double](p * p)
      var r = 0
      while r < p do
        projector(r * p + r) = 1.0
        r += 1
      var component = 0
      while component < space.rank do
        val v = space.basis(component)
        r = 0
        while r < p do
          var c = 0
          while c < p do
            projector(r * p + c) -= v(r) * v(c)
            c += 1
          r += 1
        component += 1
      reducedRowEchelon(projector, p, p, space.tolerance, nullity)
        .flatMap: row =>
          val coefficients = row.indices.collect { case column if row(column) != 0.0 => ids(column) -> row(column) }.toVector
          if coefficients.isEmpty then None
          else Some(AliasEquation(coefficients, aliasResidual(geometry.matrix, row, space.sigmaMax)))
        .sortBy(_.coefficients.length)

  private def aliasResidual(x: DMat, coefficients: Array[Double], sigmaMax: Double): Double =
    if sigmaMax == 0.0 then 0.0
    else
      val combination = new Array[Double](x.rows)
      var row = 0
      while row < x.rows do
        var sum = 0.0
        var column = 0
        while column < x.cols do
          sum += x(row, column) * coefficients(column)
          column += 1
        combination(row) = sum
        row += 1
      DesignDiagnostics.norm(combination) / (sigmaMax * DesignDiagnostics.norm(coefficients))

  /** Gauss-Jordan reduction with partial pivoting of a row-major matrix,
    * taking at most `maximumPivots` pivots. Gale has no RREF capability; this
    * is the one remaining private dense elimination helper.
    */
  private def reducedRowEchelon(values: Array[Double], rows: Int, columns: Int, tolerance: Double, maximumPivots: Int): Vector[Array[Double]] =
    val a = values.clone()
    var pivotRow = 0
    var column = 0
    while pivotRow < rows && pivotRow < maximumPivots && column < columns do
      var best = pivotRow
      var r = pivotRow + 1
      while r < rows do
        if math.abs(a(r * columns + column)) > math.abs(a(best * columns + column)) then best = r
        r += 1
      if math.abs(a(best * columns + column)) <= tolerance then column += 1
      else
        var c = 0
        while c < columns do
          val held = a(pivotRow * columns + c)
          a(pivotRow * columns + c) = a(best * columns + c)
          a(best * columns + c) = held
          c += 1
        val pivot = a(pivotRow * columns + column)
        c = 0
        while c < columns do
          a(pivotRow * columns + c) /= pivot
          c += 1
        r = 0
        while r < rows do
          val factor = a(r * columns + column)
          if r != pivotRow && factor != 0.0 then
            c = 0
            while c < columns do
              a(r * columns + c) -= factor * a(pivotRow * columns + c)
              c += 1
          r += 1
        pivotRow += 1
        column += 1
    Vector.tabulate(pivotRow): row =>
      Array.tabulate(columns): c =>
        val value = a(row * columns + c)
        if math.abs(value) <= tolerance then 0.0 else value

  // Validation helpers.

  private def highPassPair(
      withCosines: ContrastDiagnosticsResult,
      withoutCosines: ContrastDiagnosticsResult,
      selected: Vector[ColumnId]
  ): Either[DesignDiagnosticsError, Unit] =
    val withIds = withCosines.geometry.ids
    val withoutIds = withoutCosines.geometry.ids
    if selected.isEmpty || selected.distinct.length != selected.length then Left(DesignDiagnosticsError.InvalidCosineSelection)
    else if withCosines.selectedRows != withoutCosines.selectedRows then Left(DesignDiagnosticsError.HighPassRowsDiffer)
    else if withCosines.rankPolicy != withoutCosines.rankPolicy then
      Left(DesignDiagnosticsError.RankPolicyMismatch(Vector(withCosines.rankPolicy.label, withoutCosines.rankPolicy.label)))
    else if selected.exists(id => !withIds.contains(id) || withoutIds.contains(id)) || withIds.filterNot(selected.contains) != withoutIds then
      Left(DesignDiagnosticsError.HighPassColumnsMismatch)
    else
      val withMatrix = withCosines.geometry.matrix
      val withoutMatrix = withoutCosines.geometry.matrix
      val changed = withoutIds.indices.find(column => !sameColumn(withMatrix, withIds.indexOf(withoutIds(column)), withoutMatrix, column))
      changed match
        case Some(column) => Left(DesignDiagnosticsError.HighPassRetainedColumnDiffers(withoutIds(column)))
        case None => Right(())

  private def sameColumn(left: DMat, leftColumn: Int, right: DMat, rightColumn: Int): Boolean =
    var row = 0
    var same = true
    while same && row < right.rows do
      same = left(row, leftColumn) == right(row, rightColumn)
      row += 1
    same

  private def cost(selected: Vector[ColumnId], withValue: Double, withoutValue: Double): HighPassVarianceCost =
    val ratio = withValue / withoutValue
    HighPassVarianceCost(selected, withValue, withoutValue, ratio, ratio - 1.0)

  private def embed(row: Array[Double], from: Vector[ColumnId], to: Vector[ColumnId]): Array[Double] =
    val out = new Array[Double](to.length)
    var index = 0
    while index < from.length do
      out(to.indexOf(from(index))) = row(index)
      index += 1
    out

  /** Weights over `ids`; any column absent from `ids` is an error. */
  private def strictWeights(contrast: ColumnContrast, ids: Vector[ColumnId]): Either[DesignDiagnosticsError, Array[Double]] =
    contrast.weights.find((id, _) => !ids.contains(id)) match
      case Some((id, _)) => Left(DesignDiagnosticsError.UnknownColumn(id))
      case None => lenientWeights(contrast, ids).map(_._1)

  /** Weights over `ids` and the absent columns carrying non-zero weight;
    * explicit zero weights on absent columns are ignored.
    */
  private def lenientWeights(contrast: ColumnContrast, ids: Vector[ColumnId]): Either[DesignDiagnosticsError, (Array[Double], Vector[ColumnId])] =
    contrast.weights.find((_, weight) => !weight.isFinite) match
      case Some((id, weight)) => Left(DesignDiagnosticsError.NonFiniteContrastWeight(id, weight))
      case None =>
        val out = new Array[Double](ids.length)
        val absent = Vector.newBuilder[ColumnId]
        contrast.weights.foreach: (id, weight) =>
          val index = ids.indexOf(id)
          if index >= 0 then out(index) = weight
          else if weight != 0.0 then absent += id
        Right(out -> absent.result())

  private def runIndex(position: Int): RunIndex = RunIndex.unsafeOneBased(position + 1)

  private def numerical(error: LinAlgError): DesignDiagnosticsError =
    DesignDiagnosticsError.NumericalFailure(String.valueOf(error.getMessage))
