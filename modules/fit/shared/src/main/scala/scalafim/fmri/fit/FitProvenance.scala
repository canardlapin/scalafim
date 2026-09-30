package scalafim.fmri.fit

import scalafim.fmri.design.{CoefficientAxis, ColumnId}
import scalafim.fmri.model.{FitEngine, FitSummary}

final case class CoefficientInferenceProvenance(
    method: CoefficientInferenceMethod,
    inferableColumns: Vector[String],
    scopeLabel: String,
    inferableColumnIds: Vector[ColumnId] = Vector.empty
):
  require(inferableColumns.nonEmpty, "coefficient inference provenance must contain inferable columns")
  require(inferableColumns.distinct.length == inferableColumns.length, "coefficient inference provenance columns must be unique")
  require(scopeLabel.trim.nonEmpty, "coefficient inference provenance scope label must be non-empty")
  require(inferableColumnIds.isEmpty || inferableColumnIds.length == inferableColumns.length, "structural inference ids must align with inferable columns")
  require(inferableColumnIds.distinct.length == inferableColumnIds.length, "structural inference ids must be unique")

final case class AnalysisProvenance(
    engine: FitEngine,
    summary: FitSummary,
    timepoints: SelectedTimepointIndices,
    columnNames: Vector[String],
    source: String,
    coefficientInference: Option[CoefficientInferenceProvenance] = None,
    notes: Vector[String] = Vector.empty,
    coefficientAxis: Option[CoefficientAxis] = None,
    responsePreparation: Option[ResponsePreparationProvenance] = None,
    rankReports: Vector[StructuralRankReport] = Vector.empty,
    voxelStatuses: Option[Vector[VoxelFitStatusRecord]] = None
):
  require(columnNames.nonEmpty, "analysis provenance column names must be non-empty")
  require(source.trim.nonEmpty, "analysis provenance source must be non-empty")
  require(coefficientInference.forall(value => value.inferableColumns.forall(columnNames.contains)), "inferable columns must belong to analysis columns")
  require(coefficientAxis.forall(_.predictors == columnNames.length), "coefficient axis must match analysis columns")
  require(coefficientInference.forall(value => value.inferableColumnIds.isEmpty || coefficientAxis.exists(axis => value.inferableColumnIds.forall(axis.columnIds.contains))), "structural inference ids must belong to the coefficient axis")
  require(notes.forall(_.trim.nonEmpty), "analysis provenance notes must be non-empty")
  require(rankReports.forall(_.predictorCount == columnNames.length), "rank reports must match analysis columns")
  require(rankReports.forall(report => coefficientAxis.exists(_.designFingerprint == report.designFingerprint)), "rank reports require the matching structural coefficient axis")
  require(voxelStatuses.forall(_.nonEmpty), "present voxel status provenance must be non-empty")
  require(voxelStatuses.forall(records => records.map(_.voxelIndex).distinct.length == records.length), "voxel status provenance indices must be unique")

object AnalysisProvenance:
  def fromResult(
      result: FmriFitResult,
      source: String = "fit",
      notes: Vector[String] = Vector.empty
  ): AnalysisProvenance =
    val coefficientInference =
      result match
        case dense: DenseFmriFitResult =>
          val allowed = dense.inferenceScope.allowedIndices(dense.predictors)
          Some(CoefficientInferenceProvenance(
            method = dense.inference.method,
            inferableColumns = allowed.map(dense.columnNames),
            scopeLabel = dense.inferenceScope.label,
            inferableColumnIds = dense.coefficientAxis.map(axis => allowed.map(index => axis.columns(index).id)).getOrElse(Vector.empty)
          ))
        case _ => None
    val rankReports =
      result match
        case dense: DenseFmriFitResult => dense.structuralRankReport.toOption.toVector
        case runwise: RunwiseFmriFitResult => runwise.structuralRankReports.toOption.getOrElse(Vector.empty)
        case _ => Vector.empty
    val voxelStatuses =
      val retained =
        result match
          case dense: DenseFmriFitResult =>
            dense.voxelIndices.zip(dense.resolvedVoxelStatuses).map(VoxelFitStatusRecord.apply)
          case runwise: RunwiseFmriFitResult =>
            runwise.voxelIndices.zipWithIndex.map { case (voxelIndex, position) =>
              val statuses = runwise.runs.map(_.resolvedVoxelStatuses(position))
              VoxelFitStatusRecord(voxelIndex, VoxelFitStatus.aggregate(statuses))
            }
          case fixed: FixedEffectsFmriFitResult =>
            fixed.voxelIndices.map(VoxelFitStatusRecord(_, VoxelFitStatus.Estimable))
          case patterned: PatternedFmriFitResult =>
            patterned.voxelIndices.map { voxelIndex =>
              val status = patterned.resultForVoxel(voxelIndex) match
                case Some(dense: DenseFmriFitResult) => dense.voxelStatus(voxelIndex).getOrElse(VoxelFitStatus.Estimable)
                case Some(runwise: RunwiseFmriFitResult) =>
                  val position = runwise.voxelIndices.indexOf(voxelIndex)
                  VoxelFitStatus.aggregate(runwise.runs.map(_.resolvedVoxelStatuses(position)))
                case _ => VoxelFitStatus.Estimable
              VoxelFitStatusRecord(voxelIndex, status)
            }
          case _: LssFmriFitResult => Vector.empty
      val records = retained ++ result.fitExclusions.map(_.statusRecord)
      Option.when(records.nonEmpty)(records)
    AnalysisProvenance(
      engine = result.engine,
      summary = result.summary,
      timepoints = SelectedTimepointIndices.unsafe(result.timepoints),
      columnNames = result.columnNames,
      source = source,
      coefficientInference = coefficientInference,
      notes = notes,
      coefficientAxis = result.coefficientAxis,
      responsePreparation = result.preparationProvenance,
      rankReports = rankReports,
      voxelStatuses = voxelStatuses
    )
