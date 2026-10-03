package scalafim.fmri.design

import gale.linalg.{DMat, DVec}
import gale.spectral.SpectralBackend.given
import scalafim.fmri.hrf.linalg.Mat

/** A caller-named, disjoint set of columns whose explanatory power is reported
  * separately.  Block R² values intentionally do not sum to a joint R².
  */
final case class DiagnosticBlock(id: String, columns: Vector[ColumnId]):
  require(id.trim.nonEmpty, "diagnostic block id must be non-empty")
  require(columns.distinct.length == columns.length, "diagnostic block columns must be unique")

enum VifOutcome:
  case Finite(value: Double)
  case Aliased
  case NotApplicable(reason: String)

enum BlockR2Outcome:
  case Value(value: Double)
  case NotApplicable(reason: String)

enum CoefficientIdentifiability:
  case Unique, MinNormNonUnique

/** Numerical rank policy shared by every design and contrast diagnostic.
  *
  * The policy fixes one dimensionless relative tolerance `tau(m, n)`: a caller
  * multiplier, or `max(m, n) * eps` by default. Every decision is a
  * scale-invariant comparison against it:
  *
  *   - rank: a singular value of an `m x n` matrix counts iff it exceeds
  *     `cutoff = tau(m, n) * sigmaMax`;
  *   - degenerate columns: a column is constant when its centred norm is at
  *     most `tau(rows, columns)` times its uncentred norm;
  *   - row-space membership (contrast estimability, alias projector entries):
  *     a unit direction is inside the row space when its component outside
  *     the kept right singular vectors is at most `tau * sigmaMax / sigmaMinKept`,
  *     the first-order uncertainty of the computed subspace;
  *   - hypothesis rows: unit-scaled contrast rows are orthonormalized by SVD
  *     with the same rank cutoff.
  *
  * Policies are values: two separately constructed equal policies are equal.
  */
final case class RankPolicy private (relativeMultiplier: Option[Double]):
  def label: String =
    relativeMultiplier.fold("max-dimension-machine-epsilon")(value => s"relative=$value")

  /** Dimensionless tolerance `tau(rows, columns)`. */
  def relativeTolerance(rows: Int, columns: Int): Double =
    relativeMultiplier.getOrElse(math.max(rows, columns).toDouble * DesignDiagnostics.MachineEpsilon)

  /** Absolute singular-value cutoff `tau(rows, columns) * sigmaMax`. */
  def cutoff(rows: Int, columns: Int, sigmaMax: Double): Double =
    relativeTolerance(rows, columns) * sigmaMax

object RankPolicy:
  /** Scale-aware default: `max(rows, columns) * eps * sigmaMax`. */
  val Default: RankPolicy = new RankPolicy(None)

  def relative(multiplier: Double): Either[DesignDiagnosticsError, RankPolicy] =
    if multiplier.isFinite && multiplier > 0.0 then Right(new RankPolicy(Some(multiplier)))
    else Left(DesignDiagnosticsError.InvalidRankMultiplier(multiplier))

enum ProjectionKind:
  case Vif
  case BlockR2(block: String)
  case Joint

final case class ProjectionRank(
    target: ColumnId,
    kind: ProjectionKind,
    predictorCount: Int,
    cutoff: Double,
    effectiveRank: Int
)

final case class VifDiagnostic(column: ColumnId, outcome: VifOutcome)

final case class BlockR2Diagnostic(target: ColumnId, block: String, outcome: BlockR2Outcome)

final case class JointContribution(
    target: ColumnId,
    predictor: ColumnId,
    standardizedCoefficient: Double,
    identifiability: CoefficientIdentifiability
)

/** Diagnostics are tied to the immutable design identity and to the exact
  * source rows used for the unwhitened OLS calculations.
  */
final case class DesignDiagnosticsResult(
    fingerprint: DesignFingerprint,
    selectedRows: Vector[ScanIndex],
    rankPolicy: RankPolicy,
    vif: Vector[VifDiagnostic],
    blockR2: Vector[BlockR2Diagnostic],
    jointContributors: Vector[JointContribution],
    projections: Vector[ProjectionRank],
    correlations: CorrelationMapData
)

enum DesignDiagnosticsError:
  case EmptyRows
  case DuplicateRows
  case RowOutOfBounds(row: ScanIndex, rowCount: Int)
  case DuplicateBlock(id: String)
  case UnknownColumn(column: ColumnId)
  case OverlappingBlocks(column: ColumnId)
  case InvalidRankMultiplier(value: Double)
  case NonFiniteValue(row: ScanIndex, column: ColumnId, value: Double)
  case NonFiniteContrastWeight(column: ColumnId, value: Double)
  case InsufficientResidualDof(rowCount: Int, numericalRank: Int)
  case InvalidColumnBudget(maximumColumns: Int)
  case ColumnBudgetExceeded(columnCount: Int, maximumColumns: Int)
  case EmptyTaskColumns
  case DuplicateTaskColumn(column: ColumnId)
  case RankPolicyMismatch(labels: Vector[String])
  case HypothesisDimensionMismatch(dimensions: Vector[Int])
  case InvalidCosineSelection
  case HighPassRowsDiffer
  case HighPassColumnsMismatch
  case HighPassRetainedColumnDiffers(column: ColumnId)
  case HighPassContrastNotEstimable
  case ResidualSigmaCountMismatch(runCount: Int, sigmaCount: Int)
  case InvalidResidualSigma(run: RunIndex, value: Double)
  case NumericalFailure(detail: String)

  def message: String =
    this match
      case EmptyRows => "design diagnostics require at least one selected row"
      case DuplicateRows => "design diagnostics selected rows must be unique"
      case RowOutOfBounds(row, rowCount) => s"selected row ${row.oneBased} is outside design rows 1..$rowCount"
      case DuplicateBlock(id) => s"diagnostic block '$id' appears more than once"
      case UnknownColumn(column) => s"diagnostic column '${column.value}' is absent from the design schema"
      case OverlappingBlocks(column) => s"diagnostic blocks overlap at column '${column.value}'"
      case InvalidRankMultiplier(value) => s"rank cutoff multiplier must be finite and positive, got $value"
      case NonFiniteValue(row, column, value) => s"design value at row ${row.oneBased}, column '${column.value}' is non-finite: $value"
      case NonFiniteContrastWeight(column, value) => s"contrast weight for column '${column.value}' is non-finite: $value"
      case InsufficientResidualDof(rowCount, numericalRank) =>
        s"design has numerical rank $numericalRank for $rowCount selected scans and leaves no residual degrees of freedom"
      case InvalidColumnBudget(maximumColumns) => s"column budget must be non-negative, got $maximumColumns"
      case ColumnBudgetExceeded(columnCount, maximumColumns) =>
        s"design has $columnCount columns and exceeds the configured budget $maximumColumns"
      case EmptyTaskColumns => "concatenated task diagnostics require task columns"
      case DuplicateTaskColumn(column) => s"concatenated task column '${column.value}' appears more than once"
      case RankPolicyMismatch(labels) => s"combined diagnostics require one rank policy, got ${labels.mkString(", ")}"
      case HypothesisDimensionMismatch(dimensions) =>
        s"fixed-effects F runs disagree on hypothesis dimension: ${dimensions.mkString(", ")}"
      case InvalidCosineSelection => "high-pass comparison requires unique explicitly selected cosine columns"
      case HighPassRowsDiffer => "high-pass comparison requires the same selected rows"
      case HighPassColumnsMismatch =>
        "without-cosines columns must equal with-cosines columns minus the selected cosine columns"
      case HighPassRetainedColumnDiffers(column) => s"retained high-pass design column '${column.value}' differs between designs"
      case HighPassContrastNotEstimable =>
        "high-pass contrast must be estimable with positive variance both with and without the selected cosine columns"
      case ResidualSigmaCountMismatch(runCount, sigmaCount) => s"provide exactly one residual sigma per run: $runCount runs, $sigmaCount sigmas"
      case InvalidResidualSigma(run, value) => s"residual sigma for run ${run.oneBased} must be finite and strictly positive, got $value"
      case NumericalFailure(detail) => detail

/** Schema-bound, unwhitened OLS collinearity diagnostics.
  *
  * Every SVD-backed calculation uses the caller-visible [[RankPolicy]] for
  * both numerical rank and pseudo-inverse components.
  */
object DesignDiagnostics:

  val MachineEpsilon: Double = 2.220446049250313e-16

  def analyze(
      schema: DesignSchema,
      blocks: Vector[DiagnosticBlock]
  ): Either[DesignDiagnosticsError, DesignDiagnosticsResult] =
    analyze(schema, blocks, schema.rows.selectedRows, RankPolicy.Default)

  def analyze(
      schema: DesignSchema,
      blocks: Vector[DiagnosticBlock],
      rankPolicy: RankPolicy
  ): Either[DesignDiagnosticsError, DesignDiagnosticsResult] =
    analyze(schema, blocks, schema.rows.selectedRows, rankPolicy)

  def analyze(
      schema: DesignSchema,
      blocks: Vector[DiagnosticBlock],
      selectedRows: Vector[ScanIndex]
  ): Either[DesignDiagnosticsError, DesignDiagnosticsResult] =
    analyze(schema, blocks, selectedRows, RankPolicy.Default)

  def analyze(
      schema: DesignSchema,
      blocks: Vector[DiagnosticBlock],
      selectedRows: Vector[ScanIndex],
      rankPolicy: RankPolicy
  ): Either[DesignDiagnosticsError, DesignDiagnosticsResult] =
    val ids = schema.columnIds
    for
      _ <- validateRows(schema, selectedRows)
      _ <- validateBlocks(schema, blocks)
      matrix = selectedMatrix(schema.matrixValues, selectedRows)
      _ <- firstNonFinite(matrix) match
        case Some((row, column)) => Left(DesignDiagnosticsError.NonFiniteValue(selectedRows(row), ids(column), matrix(row, column)))
        case None => Right(())
      standardized <- standardize(matrix, rankPolicy)
      correlations <- DesignExports
        .correlationMapEither(Mat.unsafe(matrix.rows, matrix.cols, rowMajor(matrix)), schema.columnNames, CorrelationMethod.Pearson, halfMatrix = false, absoluteLimits = true)
        .left
        .map(error => DesignDiagnosticsError.NumericalFailure(error.message))
      // One factorization per target serves both VIF and the joint fit.
      joint <- traverse(ids.indices.toVector.filter(standardized.usable)): target =>
        val predictors = ids.indices.filter(index => index != target && standardized.usable(index)).toVector
        leastSquares(standardized.values, target, predictors, rankPolicy).map(fit => (target, predictors, fit))
      blockR2 <- blockR2Diagnostics(ids, blocks, standardized, rankPolicy)
    yield
      val fits = joint.map((target, predictors, fit) => target -> (predictors, fit)).toMap
      val vif = ids.indices.toVector.map: target =>
        fits.get(target) match
          case None => VifDiagnostic(ids(target), VifOutcome.NotApplicable("constant or zero-support column"))
          case Some((_, fit)) => VifDiagnostic(ids(target), vifOutcome(fit))
      val vifProjections = joint.map((target, predictors, fit) => projection(ids(target), ProjectionKind.Vif, predictors, fit))
      val contributions = joint.flatMap: (target, predictors, fit) =>
        val status = if fit.rank == predictors.length then CoefficientIdentifiability.Unique else CoefficientIdentifiability.MinNormNonUnique
        predictors.indices.map(index => JointContribution(ids(target), ids(predictors(index)), fit.coefficients(index), status))
      val jointProjections = joint.map((target, predictors, fit) => projection(ids(target), ProjectionKind.Joint, predictors, fit))
      DesignDiagnosticsResult(
        schema.fingerprint,
        selectedRows,
        rankPolicy,
        vif,
        blockR2._1,
        contributions,
        vifProjections ++ blockR2._2 ++ jointProjections,
        correlations
      )

  /** Sequence `f` over `values`, stopping at the first failure. */
  private[design] def traverse[A, B](values: Vector[A])(f: A => Either[DesignDiagnosticsError, B]): Either[DesignDiagnosticsError, Vector[B]] =
    val out = Vector.newBuilder[B]
    val iterator = values.iterator
    var failure: Option[DesignDiagnosticsError] = None
    while failure.isEmpty && iterator.hasNext do
      f(iterator.next()) match
        case Right(value) => out += value
        case Left(error) => failure = Some(error)
    failure.toLeft(out.result())

  /** Overflow-safe Euclidean norm through Gale's scaled `nrm2` kernel. */
  private[design] def norm(values: Array[Double]): Double =
    DVec.tabulate(values.length)(values(_)).norm2

  /** First non-finite entry as `(row, column)`. */
  private[design] def firstNonFinite(matrix: DMat): Option[(Int, Int)] =
    var row = 0
    var found: Option[(Int, Int)] = None
    while found.isEmpty && row < matrix.rows do
      var column = 0
      while found.isEmpty && column < matrix.cols do
        if !matrix(row, column).isFinite then found = Some(row -> column)
        column += 1
      row += 1
    found

  private final case class Standardized(values: DMat, usable: Vector[Boolean])

  private def validateRows(schema: DesignSchema, selectedRows: Vector[ScanIndex]): Either[DesignDiagnosticsError, Unit] =
    if selectedRows.isEmpty then Left(DesignDiagnosticsError.EmptyRows)
    else if selectedRows.distinct.length != selectedRows.length then Left(DesignDiagnosticsError.DuplicateRows)
    else
      selectedRows.find(row => row.oneBased < 1 || row.oneBased > schema.matrixValues.rows) match
        case Some(row) => Left(DesignDiagnosticsError.RowOutOfBounds(row, schema.matrixValues.rows))
        case None => Right(())

  private def validateBlocks(schema: DesignSchema, blocks: Vector[DiagnosticBlock]): Either[DesignDiagnosticsError, Unit] =
    blocks.find(block => blocks.count(_.id == block.id) > 1) match
      case Some(block) => Left(DesignDiagnosticsError.DuplicateBlock(block.id))
      case None =>
        val known = schema.columnIds.toSet
        blocks.iterator.flatMap(_.columns).find(column => !known.contains(column)) match
          case Some(column) => Left(DesignDiagnosticsError.UnknownColumn(column))
          case None =>
            blocks.iterator.flatMap(_.columns).find(column => blocks.count(_.columns.contains(column)) > 1) match
              case Some(column) => Left(DesignDiagnosticsError.OverlappingBlocks(column))
              case None => Right(())

  private def selectedMatrix(matrix: DMat, rows: Vector[ScanIndex]): DMat =
    DMat.tabulate(rows.length, matrix.cols)((row, column) => matrix(rows(row).zeroBased, column))

  private def rowMajor(matrix: DMat): Array[Double] =
    val out = new Array[Double](matrix.rows * matrix.cols)
    matrix.copyRowMajorTo(out)
    out

  /** Centre and unit-scale each column. A column is degenerate (constant or
    * zero) when its centred norm is at most the rank policy's relative
    * tolerance times its uncentred norm; this catches constants that are not
    * exactly representable, whose centred norm is rounding noise.
    */
  private def standardize(matrix: DMat, policy: RankPolicy): Either[DesignDiagnosticsError, Standardized] =
    val n = matrix.rows
    val tolerance = policy.relativeTolerance(n, matrix.cols)
    val means = new Array[Double](matrix.cols)
    val scales = new Array[Double](matrix.cols)
    val usable = new Array[Boolean](matrix.cols)
    val centred = new Array[Double](n)
    var failure: Option[DesignDiagnosticsError] = None
    var column = 0
    while failure.isEmpty && column < matrix.cols do
      var row = 0
      var sum = 0.0
      while row < n do
        sum += matrix(row, column)
        row += 1
      val mean = sum / n.toDouble
      row = 0
      while row < n do
        centred(row) = matrix(row, column) - mean
        row += 1
      val scale = norm(centred)
      val magnitude = matrix.col(column).norm2
      if !mean.isFinite || !scale.isFinite || !magnitude.isFinite then
        failure = Some(DesignDiagnosticsError.NumericalFailure(s"standardization overflow in column ${column + 1}"))
      means(column) = mean
      scales(column) = scale
      usable(column) = scale > tolerance * magnitude
      column += 1
    failure.toLeft(
      Standardized(
        DMat.tabulate(n, matrix.cols)((row, column) => if usable(column) then (matrix(row, column) - means(column)) / scales(column) else 0.0),
        usable.toVector
      )
    )

  private def vifOutcome(fit: LeastSquares): VifOutcome =
    if fit.residualNorm <= fit.cutoff then VifOutcome.Aliased
    else
      // `1 - R²` loses the small residual entirely for a nearly collinear but
      // still estimable column. The residual is already the orthogonal
      // projection computed by the SVD, so form VIF directly as
      // ||y||² / ||(I - P)y||².
      val value = fit.targetNorm * fit.targetNorm / (fit.residualNorm * fit.residualNorm)
      if value.isFinite then VifOutcome.Finite(value) else VifOutcome.Aliased

  private def blockR2Diagnostics(
      ids: Vector[ColumnId],
      blocks: Vector[DiagnosticBlock],
      standardized: Standardized,
      policy: RankPolicy
  ): Either[DesignDiagnosticsError, (Vector[BlockR2Diagnostic], Vector[ProjectionRank])] =
    val pairs = for
      target <- ids.indices.toVector
      block <- blocks
    yield target -> block
    traverse(pairs): (target, block) =>
      if !standardized.usable(target) then
        Right(BlockR2Diagnostic(ids(target), block.id, BlockR2Outcome.NotApplicable("constant or zero-support target")) -> None)
      else
        val predictors = block.columns.flatMap: column =>
          val index = ids.indexOf(column)
          if index != target && standardized.usable(index) then Some(index) else None
        leastSquares(standardized.values, target, predictors, policy).map: fit =>
          val r2 = clamp01(1.0 - (fit.residualNorm * fit.residualNorm) / (fit.targetNorm * fit.targetNorm))
          BlockR2Diagnostic(ids(target), block.id, BlockR2Outcome.Value(r2)) -> Some(projection(ids(target), ProjectionKind.BlockR2(block.id), predictors, fit))
    .map(values => values.map(_._1) -> values.flatMap(_._2))

  private final case class LeastSquares(coefficients: Array[Double], targetNorm: Double, residualNorm: Double, rank: Int, cutoff: Double)

  /** Minimum-norm least squares through Gale's economy SVD with the policy's
    * cutoff. `z = U_k' y` is formed once (O(nk)); coefficients are
    * `V_k S_k^-1 z`; the residual `y - U_k z` is projected through `U` rather
    * than rebuilt from `X beta`, which would make a small residual the
    * difference of two nearly equal vectors.
    */
  private def leastSquares(matrix: DMat, target: Int, predictors: Vector[Int], policy: RankPolicy): Either[DesignDiagnosticsError, LeastSquares] =
    val n = matrix.rows
    val y = new Array[Double](n)
    var row = 0
    while row < n do
      y(row) = matrix(row, target)
      row += 1
    val targetNorm = norm(y)
    if predictors.isEmpty then Right(LeastSquares(Array.emptyDoubleArray, targetNorm, targetNorm, 0, 0.0))
    else
      val x = DMat.tabulate(n, predictors.length)((r, column) => matrix(r, predictors(column)))
      x.svd.left.map(error => DesignDiagnosticsError.NumericalFailure(String.valueOf(error.getMessage))).map: svd =>
        val sigmaMax = if svd.size == 0 then 0.0 else svd.singularValues(0)
        val cutoff = policy.cutoff(x.rows, x.cols, sigmaMax)
        var kept = 0
        while kept < svd.size && svd.singularValues(kept) > cutoff do kept += 1
        val z = new Array[Double](kept)
        var component = 0
        while component < kept do
          var dot = 0.0
          var r = 0
          while r < n do
            dot += svd.u(r, component) * y(r)
            r += 1
          z(component) = dot
          component += 1
        val coefficients = new Array[Double](predictors.length)
        val residual = y.clone()
        component = 0
        while component < kept do
          val scaled = z(component) / svd.singularValues(component)
          var predictor = 0
          while predictor < predictors.length do
            coefficients(predictor) += svd.vt(component, predictor) * scaled
            predictor += 1
          var r = 0
          while r < n do
            residual(r) -= svd.u(r, component) * z(component)
            r += 1
          component += 1
        LeastSquares(coefficients, targetNorm, norm(residual), kept, cutoff)

  private def projection(target: ColumnId, kind: ProjectionKind, predictors: Vector[Int], fit: LeastSquares): ProjectionRank =
    ProjectionRank(target, kind, predictors.length, fit.cutoff, fit.rank)

  private def clamp01(value: Double): Double = math.max(0.0, math.min(1.0, value))
