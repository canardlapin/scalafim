package scalafim.fmri.design

import gale.linalg.DMat
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

/** Numerical threshold for rank and pseudo-inverse components. */
final class RankPolicy private (private val fixedRelativeMultiplier: Option[Double]):
  def relativeMultiplier: Option[Double] = fixedRelativeMultiplier

  def label: String =
    fixedRelativeMultiplier.fold("max-dimension-machine-epsilon")(value => s"relative=$value")

  private[design] def cutoff(rows: Int, columns: Int, sigmaMax: Double): Double =
    fixedRelativeMultiplier.getOrElse(math.max(rows, columns).toDouble * DesignDiagnostics.MachineEpsilon) * sigmaMax

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
  case NumericalFailure(detail: String)
  case InsufficientResidualDof(rowCount: Int, columnCount: Int)

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
      case NumericalFailure(detail) => detail
      case InsufficientResidualDof(rowCount, columnCount) => s"design has $columnCount columns for $rowCount selected scans and leaves no residual degrees of freedom"

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
    for
      _ <- validateRows(schema, selectedRows)
      _ <- validateBlocks(schema, blocks)
      matrix = selectedMatrix(schema.matrix, selectedRows)
      _ <- validateFinite(matrix, selectedRows, schema.columnIds)
      standardized <- standardize(matrix)
      correlations <- DesignExports
        .correlationMapEither(matrix, schema.columnNames, CorrelationMethod.Pearson, halfMatrix = false, absoluteLimits = true)
        .left
        .map(error => DesignDiagnosticsError.NumericalFailure(error.message))
      vif <- vifDiagnostics(schema.columnIds, standardized, rankPolicy)
      blockR2 <- blockR2Diagnostics(schema.columnIds, blocks, standardized, rankPolicy)
      joint <- jointDiagnostics(schema.columnIds, standardized, rankPolicy)
    yield
      val projections = vif._2 ++ blockR2._2 ++ joint._2
      DesignDiagnosticsResult(schema.fingerprint, selectedRows, rankPolicy, vif._1, blockR2._1, joint._1, projections, correlations)

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
            blocks.iterator.flatMap(_.columns).find { column =>
              blocks.count(_.columns.contains(column)) > 1
            } match
              case Some(column) => Left(DesignDiagnosticsError.OverlappingBlocks(column))
              case None => Right(())

  private def selectedMatrix(matrix: Mat, rows: Vector[ScanIndex]): Mat =
    Mat.unsafe(rows.length, matrix.cols, Array.tabulate(rows.length * matrix.cols) { index =>
      val row = index / matrix.cols
      val column = index % matrix.cols
      matrix(rows(row).oneBased - 1, column)
    })

  private def validateFinite(matrix: Mat, rows: Vector[ScanIndex], ids: Vector[ColumnId]): Either[DesignDiagnosticsError, Unit] =
    var index = 0
    while index < matrix.data.length do
      if !matrix.data(index).isFinite then
        return Left(DesignDiagnosticsError.NonFiniteValue(rows(index / matrix.cols), ids(index % matrix.cols), matrix.data(index)))
      index += 1
    Right(())

  private def standardize(matrix: Mat): Either[DesignDiagnosticsError, Standardized] =
    val means = new Array[Double](matrix.cols)
    val scales = new Array[Double](matrix.cols)
    val usable = new Array[Boolean](matrix.cols)
    var column = 0
    while column < matrix.cols do
      var row = 0
      var sum = 0.0
      while row < matrix.rows do
        sum += matrix(row, column)
        row += 1
      val mean = sum / matrix.rows.toDouble
      means(column) = mean
      row = 0
      var squared = 0.0
      while row < matrix.rows do
        val centered = matrix(row, column) - mean
        squared += centered * centered
        row += 1
      val scale = math.sqrt(squared)
      if !mean.isFinite || !scale.isFinite then
        return Left(DesignDiagnosticsError.NumericalFailure(s"standardization overflow in column ${column + 1}"))
      scales(column) = scale
      usable(column) = scale > 0.0 && scale.isFinite
      column += 1
    Right(Standardized(
      DMat.tabulate(matrix.rows, matrix.cols) { (row, column) =>
        if usable(column) then (matrix(row, column) - means(column)) / scales(column) else 0.0
      },
      usable.toVector
    ))

  private def vifDiagnostics(ids: Vector[ColumnId], standardized: Standardized, policy: RankPolicy): Either[DesignDiagnosticsError, (Vector[VifDiagnostic], Vector[ProjectionRank])] =
    ids.indices.foldLeft[Either[DesignDiagnosticsError, (Vector[VifDiagnostic], Vector[ProjectionRank])]](Right(Vector.empty -> Vector.empty)) { (acc, target) =>
      acc.flatMap { values =>
        if !standardized.usable(target) then
          Right((values._1 :+ VifDiagnostic(ids(target), VifOutcome.NotApplicable("constant or zero-support column"))) -> values._2)
        else
          val predictors = ids.indices.filter(index => index != target && standardized.usable(index)).toVector
          leastSquares(standardized.values, target, predictors, policy).map { fit =>
            val outcome =
              if fit.residualNorm <= fit.cutoff then VifOutcome.Aliased
              else
                // `1 - R²` loses the small residual entirely for a nearly
                // collinear but still estimable column.  The residual is
                // already the orthogonal projection computed by the SVD, so
                // form VIF directly as ||y||² / ||(I - P)x||².
                val value = fit.targetNorm * fit.targetNorm / (fit.residualNorm * fit.residualNorm)
                if value.isFinite then VifOutcome.Finite(value)
                else VifOutcome.Aliased
            (
              values._1 :+ VifDiagnostic(ids(target), outcome),
              values._2 :+ projection(ids(target), ProjectionKind.Vif, predictors, fit)
            )
          }
      }
    }

  private def blockR2Diagnostics(
      ids: Vector[ColumnId],
      blocks: Vector[DiagnosticBlock],
      standardized: Standardized,
      policy: RankPolicy
  ): Either[DesignDiagnosticsError, (Vector[BlockR2Diagnostic], Vector[ProjectionRank])] =
    ids.indices.foldLeft[Either[DesignDiagnosticsError, (Vector[BlockR2Diagnostic], Vector[ProjectionRank])]](Right(Vector.empty -> Vector.empty)) { (acc, target) =>
      acc.flatMap { values =>
        blocks.foldLeft[Either[DesignDiagnosticsError, (Vector[BlockR2Diagnostic], Vector[ProjectionRank])]](Right(values)) { (within, block) =>
          within.flatMap { current =>
            if !standardized.usable(target) then
              Right((current._1 :+ BlockR2Diagnostic(ids(target), block.id, BlockR2Outcome.NotApplicable("constant or zero-support target"))) -> current._2)
            else
              val predictors = block.columns.flatMap(column => ids.indexOf(column) match
                case index if index != target && standardized.usable(index) => Some(index)
                case _ => None
              )
              leastSquares(standardized.values, target, predictors, policy).map { fit =>
                val r2 = clamp01(1.0 - (fit.residualNorm * fit.residualNorm) / (fit.targetNorm * fit.targetNorm))
                (
                  current._1 :+ BlockR2Diagnostic(ids(target), block.id, BlockR2Outcome.Value(r2)),
                  current._2 :+ projection(ids(target), ProjectionKind.BlockR2(block.id), predictors, fit)
                )
              }
          }
        }
      }
    }

  private def jointDiagnostics(ids: Vector[ColumnId], standardized: Standardized, policy: RankPolicy): Either[DesignDiagnosticsError, (Vector[JointContribution], Vector[ProjectionRank])] =
    ids.indices.foldLeft[Either[DesignDiagnosticsError, (Vector[JointContribution], Vector[ProjectionRank])]](Right(Vector.empty -> Vector.empty)) { (acc, target) =>
      acc.flatMap { values =>
        if !standardized.usable(target) then Right(values)
        else
          val predictors = ids.indices.filter(index => index != target && standardized.usable(index)).toVector
          leastSquares(standardized.values, target, predictors, policy).map { fit =>
            val status = if fit.rank == predictors.length then CoefficientIdentifiability.Unique else CoefficientIdentifiability.MinNormNonUnique
            (
              values._1 ++ predictors.zip(fit.coefficients).map { (predictor, coefficient) =>
                JointContribution(ids(target), ids(predictor), coefficient, status)
              },
              values._2 :+ projection(ids(target), ProjectionKind.Joint, predictors, fit)
            )
          }
      }
    }

  private final case class LeastSquares(coefficients: Vector[Double], targetNorm: Double, residualNorm: Double, rank: Int, cutoff: Double)

  private def leastSquares(matrix: DMat, target: Int, predictors: Vector[Int], policy: RankPolicy): Either[DesignDiagnosticsError, LeastSquares] =
    val y = matrix.col(target)
    if predictors.isEmpty then
      val targetNorm = stableNorm((0 until matrix.rows).map(y.apply))
      Right(LeastSquares(Vector.empty, targetNorm, targetNorm, 0, 0.0))
    else
      val x = DMat.tabulate(matrix.rows, predictors.length)((row, column) => matrix(row, predictors(column)))
      x.svd.left.map(error => DesignDiagnosticsError.NumericalFailure(error.toString)).map { svd =>
        val sigmaMax = if svd.size == 0 then 0.0 else svd.singularValues(0)
        val cutoff = policy.cutoff(x.rows, x.cols, sigmaMax)
        val kept = (0 until svd.size).filter(index => svd.singularValues(index) > cutoff).toVector
        val coefficients = predictors.indices.map { predictor =>
          var value = 0.0
          kept.foreach { component =>
            var dot = 0.0
            var row = 0
            while row < x.rows do
              dot += svd.u(row, component) * y(row)
              row += 1
            value += svd.vt(component, predictor) * dot / svd.singularValues(component)
          }
          value
        }.toVector
        // Project through U rather than rebuilding X beta.  The latter makes
        // a small residual the difference of two nearly equal vectors.
        val residual = Array.tabulate(x.rows) { row =>
          var projected = 0.0
          kept.foreach { component =>
            var dot = 0.0
            var sourceRow = 0
            while sourceRow < x.rows do
              dot += svd.u(sourceRow, component) * y(sourceRow)
              sourceRow += 1
            projected += svd.u(row, component) * dot
          }
          y(row) - projected
        }
        LeastSquares(coefficients, stableNorm((0 until matrix.rows).map(y.apply)), stableNorm(residual), kept.length, cutoff)
      }

  private def projection(target: ColumnId, kind: ProjectionKind, predictors: Vector[Int], fit: LeastSquares): ProjectionRank =
    ProjectionRank(target, kind, predictors.length, fit.cutoff, fit.rank)

  private def clamp01(value: Double): Double = math.max(0.0, math.min(1.0, value))

  /** Blue's scaled sum of squares avoids overflow and underflow in diagnostic
    * magnitudes while preserving an exact zero for a zero vector. */
  private def stableNorm(values: IterableOnce[Double]): Double =
    var scale = 0.0
    var sum = 1.0
    values.iterator.foreach { value =>
      val absolute = math.abs(value)
      if absolute != 0.0 then
        if scale < absolute then
          val ratio = if scale == 0.0 then 0.0 else scale / absolute
          sum = 1.0 + sum * ratio * ratio
          scale = absolute
        else
          val ratio = absolute / scale
          sum += ratio * ratio
    }
    if scale == 0.0 then 0.0 else scale * math.sqrt(sum)
