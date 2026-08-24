package scalafim.fmri.fit

import scalafim.dataset.DatasetShape
import scalafim.fmri.design.{CoefficientAxis, ColumnId}
import scalafim.fmri.model.{FitEngine, FitSummary}
import gale.linalg.{DMat, DVec, Matrix, Vec}

opaque type ResultMapName = String

object ResultMapName:
  def apply(value: String): Either[FitError, ResultMapName] =
    val trimmed = value.trim
    if trimmed.nonEmpty then Right(trimmed)
    else Left(FitError.InvalidFitAxis("result map name", "must be non-empty"))

  def unsafe(value: String): ResultMapName =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (name: ResultMapName)
    inline def value: String = name

opaque type ContrastId = String

object ContrastId:
  def apply(value: String): Either[FitError, ContrastId] =
    val trimmed = value.trim
    if trimmed.nonEmpty then Right(trimmed)
    else Left(FitError.InvalidFitAxis("contrast id", "must be non-empty"))

  def unsafe(value: String): ContrastId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ContrastId)
    inline def value: String = id

enum ParameterMapKind:
  case Coefficient
  case StandardError

  def label: String =
    this match
      case Coefficient   => "coefficient"
      case StandardError => "standard_error"

  def mapName(parameterName: String): String =
    this match
      case Coefficient   => parameterName
      case StandardError => s"${parameterName}_standard_error"

enum ContrastMapKind:
  case Estimate(component: Int)
  case StandardError
  case TStatistic
  case FStatistic

  def label: String =
    this match
      case Estimate(component) => s"estimate_$component"
      case StandardError       => "standard_error"
      case TStatistic          => "t"
      case FStatistic          => "f"

  def mapName(contrastId: ContrastId): String =
    this match
      case Estimate(component) if component <= 1 => s"${contrastId.value}_estimate"
      case Estimate(component)                   => s"${contrastId.value}_estimate_$component"
      case StandardError                         => s"${contrastId.value}_standard_error"
      case TStatistic                            => s"${contrastId.value}_t"
      case FStatistic                            => s"${contrastId.value}_f"

enum StatisticKind:
  case Parameter(kind: ParameterMapKind)
  case Contrast(kind: ContrastMapKind)
  case ResidualVariance
  case Custom(customLabel: String)

  def label: String =
    this match
      case Parameter(kind) => kind.label
      case Contrast(kind)  => kind.label
      case ResidualVariance => "residual_variance"
      case Custom(value)   => value

enum ResultExportIntent:
  case InMemory
  case NamedStem(stem: String)
  case BidsDerivative(suffix: String)

  def label: String =
    this match
      case InMemory             => "in_memory"
      case NamedStem(stem)      => stem
      case BidsDerivative(suffix) => suffix

  def validate: Either[FitError, Unit] =
    this match
      case InMemory =>
        Right(())
      case NamedStem(stem) if stem.trim.nonEmpty =>
        Right(())
      case NamedStem(_) =>
        Left(FitError.InvalidFitAxis("export stem", "must be non-empty"))
      case BidsDerivative(suffix) if suffix.trim.nonEmpty =>
        Right(())
      case BidsDerivative(_) =>
        Left(FitError.InvalidFitAxis("BIDS derivative suffix", "must be non-empty"))

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
        case _ =>
          None
    val rankReports =
      result match
        case dense: DenseFmriFitResult =>
          dense.structuralRankReport.toOption.toVector
        case runwise: RunwiseFmriFitResult =>
          runwise.structuralRankReports.toOption.getOrElse(Vector.empty)
        case _ =>
          Vector.empty
    val voxelStatuses =
      val retained =
        result match
          case dense: DenseFmriFitResult =>
            dense.voxelIndices
              .zip(dense.resolvedVoxelStatuses)
              .map(VoxelFitStatusRecord.apply)
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
                case Some(dense: DenseFmriFitResult) =>
                  dense.voxelStatus(voxelIndex).getOrElse(VoxelFitStatus.Estimable)
                case Some(runwise: RunwiseFmriFitResult) =>
                  val position = runwise.voxelIndices.indexOf(voxelIndex)
                  VoxelFitStatus.aggregate(runwise.runs.map(_.resolvedVoxelStatuses(position)))
                case _ => VoxelFitStatus.Estimable
              VoxelFitStatusRecord(voxelIndex, status)
            }
          case _: LssFmriFitResult =>
            Vector.empty
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

final case class StatMap private (
    name: ResultMapName,
    kind: StatisticKind,
    values: DVec,
    shape: DatasetShape,
    selectedVoxels: SelectedVoxelIndices,
    provenance: AnalysisProvenance,
    exportIntent: ResultExportIntent
):
  def label: String = name.value
  def voxelIndices: Vector[Int] = selectedVoxels.toVector
  def valueVector: Vector[Double] = values.toSeq.toVector

object StatMap:
  def make(
      name: String,
      kind: StatisticKind,
      values: DVec,
      shape: DatasetShape,
      selectedVoxels: SelectedVoxelIndices,
      provenance: AnalysisProvenance,
      exportIntent: ResultExportIntent = ResultExportIntent.InMemory
  ): Either[FitError, StatMap] =
    for
      typedName <- ResultMapName(name)
      _ <- validateValues(values, selectedVoxels, shape)
      _ <- exportIntent.validate
    yield new StatMap(typedName, kind, values, shape, selectedVoxels, provenance, exportIntent)

  def unsafe(
      name: String,
      kind: StatisticKind,
      values: DVec,
      shape: DatasetShape,
      selectedVoxels: SelectedVoxelIndices,
      provenance: AnalysisProvenance,
      exportIntent: ResultExportIntent = ResultExportIntent.InMemory
  ): StatMap =
    make(name, kind, values, shape, selectedVoxels, provenance, exportIntent)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def validateValues(
      values: DVec,
      selectedVoxels: SelectedVoxelIndices,
      shape: DatasetShape
  ): Either[FitError, Unit] =
    if values.length != selectedVoxels.length then
      Left(
        FitError.InvalidFitAxis(
          "result map values",
          s"length ${values.length} does not match selected voxels ${selectedVoxels.length}"
        )
      )
    else if selectedVoxels.toVector.exists(index => index < 0 || index >= shape.spatialSize) then
      Left(FitError.InvalidFitAxis("result map voxel indices", "out of bounds for dataset shape"))
    else if values.toSeq.exists(value => !value.isFinite) then
      Left(FitError.NonFiniteInput("result map values"))
    else Right(())

final case class ParameterMap(
    parameterName: ResultMapName,
    statistic: ParameterMapKind,
    map: StatMap
):
  require(map.kind == StatisticKind.Parameter(statistic), "parameter map statistic kind must match map kind")
  def parameter: String = parameterName.value

object ParameterMap:
  def make(
      parameterName: String,
      statistic: ParameterMapKind,
      values: DVec,
      shape: DatasetShape,
      selectedVoxels: SelectedVoxelIndices,
      provenance: AnalysisProvenance,
      exportIntent: ResultExportIntent = ResultExportIntent.InMemory
  ): Either[FitError, ParameterMap] =
    for
      parameter <- ResultMapName(parameterName)
      statMap <- StatMap.make(
        name = statistic.mapName(parameterName),
        kind = StatisticKind.Parameter(statistic),
        values = values,
        shape = shape,
        selectedVoxels = selectedVoxels,
        provenance = provenance,
        exportIntent = exportIntent
      )
    yield ParameterMap(parameter, statistic, statMap)

final case class ContrastDegreesOfFreedom(
    numerator: Option[Int],
    residual: ResidualDegreesOfFreedom
):
  require(numerator.forall(_ > 0), "contrast numerator degrees of freedom must be positive")

final case class ContrastMap(
    contrastId: ContrastId,
    statistic: ContrastMapKind,
    map: StatMap,
    degreesOfFreedom: Option[ContrastDegreesOfFreedom],
    hypothesis: Option[HypothesisMetadata] = None,
    excludedVoxels: Vector[VoxelInferenceExclusion] = Vector.empty
):
  require(map.kind == StatisticKind.Contrast(statistic), "contrast map statistic kind must match map kind")
  require(excludedVoxels.map(_.voxelIndex).distinct.length == excludedVoxels.length, "contrast map exclusions must be unique")
  require(excludedVoxels.forall(exclusion => !map.voxelIndices.contains(exclusion.voxelIndex)), "contrast map retained and excluded voxels must be disjoint")
  require(
    excludedVoxels.isEmpty || map.provenance.voxelStatuses.exists { records =>
      excludedVoxels.forall { exclusion =>
        records.exists(record => record.voxelIndex == exclusion.voxelIndex && record.status == exclusion.status)
      }
    },
    "contrast map exclusions must agree with voxel status provenance"
  )
  def contrast: String = contrastId.value

object ContrastMap:
  def make(
      contrastId: String,
      statistic: ContrastMapKind,
      values: DVec,
      shape: DatasetShape,
      selectedVoxels: SelectedVoxelIndices,
      provenance: AnalysisProvenance,
      degreesOfFreedom: Option[ContrastDegreesOfFreedom] = None,
      hypothesis: Option[HypothesisMetadata] = None,
      exportIntent: ResultExportIntent = ResultExportIntent.InMemory,
      excludedVoxels: Vector[VoxelInferenceExclusion] = Vector.empty
  ): Either[FitError, ContrastMap] =
    for
      id <- ContrastId(contrastId)
      statMap <- StatMap.make(
        name = statistic.mapName(id),
        kind = StatisticKind.Contrast(statistic),
        values = values,
        shape = shape,
        selectedVoxels = selectedVoxels,
        provenance = provenance,
        exportIntent = exportIntent
      )
    yield ContrastMap(id, statistic, statMap, degreesOfFreedom, hypothesis, excludedVoxels)

  def fromTContrast(
      result: TContrastResult,
      shape: DatasetShape,
      provenance: AnalysisProvenance,
      exportIntent: ResultExportIntent = ResultExportIntent.InMemory
  ): Either[FitError, Vector[ContrastMap]] =
    val df = Some(ContrastDegreesOfFreedom(numerator = Some(1), residual = result.residualDegreesOfFreedom))
    buildAll(
      Vector(
        ContrastMapKind.Estimate(1) -> result.estimates,
        ContrastMapKind.StandardError -> result.standardErrors,
        ContrastMapKind.TStatistic -> result.statistics
      )
    ) { case (kind, values) =>
      make(
        contrastId = result.name,
        statistic = kind,
        values = values,
        shape = shape,
        selectedVoxels = result.selectedVoxels,
        provenance = provenance,
        degreesOfFreedom = df,
        hypothesis = result.hypothesis,
        exportIntent = exportIntent,
        excludedVoxels = result.excludedVoxels
      )
    }

  def fromFContrast(
      result: FContrastResult,
      shape: DatasetShape,
      provenance: AnalysisProvenance,
      exportIntent: ResultExportIntent = ResultExportIntent.InMemory
  ): Either[FitError, Vector[ContrastMap]] =
    val df = Some(
      ContrastDegreesOfFreedom(
        numerator = Some(result.numeratorDegreesOfFreedom),
        residual = result.residualDegreesOfFreedom
      )
    )
    val estimateMaps: Vector[(ContrastMapKind, DVec)] =
      Vector.tabulate(result.estimates.rows) { row =>
        ContrastMapKind.Estimate(row + 1) -> matrixRow(result.estimates, row)
      }
    buildAll(estimateMaps :+ (ContrastMapKind.FStatistic -> result.statistics)) { case (kind, values) =>
      make(
        contrastId = result.name,
        statistic = kind,
        values = values,
        shape = shape,
        selectedVoxels = result.selectedVoxels,
        provenance = provenance,
        degreesOfFreedom = df,
        hypothesis = result.hypothesis,
        exportIntent = exportIntent,
        excludedVoxels = result.excludedVoxels
      )
    }

final case class CoefficientCovarianceArtifact private (
    parameterNames: Vector[ResultMapName],
    covariance: CoefficientCovariance,
    shape: DatasetShape,
    selectedVoxels: SelectedVoxelIndices,
    provenance: AnalysisProvenance,
    exportIntent: ResultExportIntent
):
  require(parameterNames.nonEmpty, "coefficient covariance parameter names must be non-empty")
  require(parameterNames.length == covariance.predictors, "coefficient covariance parameter names must match predictors")
  require(covariance.validateVoxelCount(selectedVoxels.length).isRight, "coefficient covariance must be shared or match selected voxels")

  def scope: CoefficientCovarianceScope =
    covariance.scope

  def voxelIndices: Vector[Int] =
    selectedVoxels.toVector

  def matrices: Vector[DMat] =
    covariance.matrices

object CoefficientCovarianceArtifact:
  def make(
      parameterNames: Vector[String],
      covariance: CoefficientCovariance,
      shape: DatasetShape,
      selectedVoxels: SelectedVoxelIndices,
      provenance: AnalysisProvenance,
      exportIntent: ResultExportIntent = ResultExportIntent.InMemory
  ): Either[FitError, CoefficientCovarianceArtifact] =
    for
      names <- buildAll(parameterNames)(ResultMapName.apply)
      _ <- validateSelectedVoxels(selectedVoxels, shape)
      _ <- covariance.validateVoxelCount(selectedVoxels.length)
      _ <- exportIntent.validate
    yield CoefficientCovarianceArtifact(names, covariance, shape, selectedVoxels, provenance, exportIntent)

  def fromDenseFit(
      result: DenseFmriFitResult,
      shape: DatasetShape,
      provenance: AnalysisProvenance,
      exportIntent: ResultExportIntent = ResultExportIntent.InMemory
  ): Either[FitError, CoefficientCovarianceArtifact] =
    val allowed = result.inferenceScope.allowedIndices(result.predictors)
    make(
      parameterNames = allowed.map(result.columnNames),
      covariance = selectCovariance(result.coefficientCovariance, allowed),
      shape = shape,
      selectedVoxels = result.selectedVoxels,
      provenance = provenance,
      exportIntent = exportIntent
    )

  private def selectCovariance(
      covariance: CoefficientCovariance,
      indices: Vector[Int]
  ): CoefficientCovariance =
    val matrices = covariance.matrices.map { matrix =>
      val out = Matrix.newBuilder(indices.length, indices.length)
      var row = 0
      while row < indices.length do
        var col = 0
        while col < indices.length do
          out(row, col) = matrix(indices(row), indices(col))
          col += 1
        row += 1
      out.result()
    }
    covariance.scope match
      case CoefficientCovarianceScope.Shared => CoefficientCovariance.unsafeShared(matrices.head)
      case CoefficientCovarianceScope.Voxelwise => CoefficientCovariance.unsafeVoxelwise(matrices)

  private def validateSelectedVoxels(
      selectedVoxels: SelectedVoxelIndices,
      shape: DatasetShape
  ): Either[FitError, Unit] =
    if selectedVoxels.toVector.exists(index => index < 0 || index >= shape.spatialSize) then
      Left(FitError.InvalidFitAxis("coefficient covariance voxel indices", "out of bounds for dataset shape"))
    else Right(())

final case class ResultManifest(
    provenance: AnalysisProvenance,
    parameters: Vector[ParameterMap],
    contrasts: Vector[ContrastMap],
    coefficientCovariance: Option[CoefficientCovarianceArtifact] = None,
    exportIntent: ResultExportIntent = ResultExportIntent.InMemory
):
  require(parameters.nonEmpty || contrasts.nonEmpty || coefficientCovariance.nonEmpty, "result manifest must contain at least one artifact")
  require(parameters.forall(_.map.provenance == provenance), "parameter map provenance must match manifest")
  require(contrasts.forall(_.map.provenance == provenance), "contrast map provenance must match manifest")
  require(coefficientCovariance.forall(_.provenance == provenance), "coefficient covariance provenance must match manifest")
  require(
    contrasts.forall { contrast =>
      contrasts
        .filter(_.contrastId == contrast.contrastId)
        .forall(_.excludedVoxels == contrast.excludedVoxels)
    },
    "maps for one contrast must carry the same voxel exclusions"
  )

  def maps: Vector[StatMap] =
    parameters.map(_.map) ++ contrasts.map(_.map)

  def parameterMaps(statistic: ParameterMapKind): Vector[ParameterMap] =
    parameters.filter(_.statistic == statistic)

  def contrastMaps(contrastId: String): Vector[ContrastMap] =
    contrasts.filter(_.contrastId.value == contrastId)

  def withContrasts(newContrasts: Vector[ContrastMap]): ResultManifest =
    copy(contrasts = contrasts ++ newContrasts)

object ResultManifest:
  def fromDenseFit(
      result: DenseFmriFitResult,
      shape: DatasetShape,
      exportIntent: ResultExportIntent = ResultExportIntent.InMemory,
      source: String = "dense-fit"
  ): Either[FitError, ResultManifest] =
    val provenance = AnalysisProvenance.fromResult(result, source)
    val selectedVoxels = result.selectedVoxels
    val coefficientSpecs: Vector[(String, ParameterMapKind, DVec)] =
      result.columnNames.zipWithIndex.map { case (parameter, row) =>
        (parameter, ParameterMapKind.Coefficient, matrixRow(result.coefficients.value, row))
      }
    val standardErrorSpecs: Vector[(String, ParameterMapKind, DVec)] =
      result.inferenceScope.allowedIndices(result.predictors).map { row =>
        (result.columnNames(row), ParameterMapKind.StandardError, matrixRow(result.standardErrors.value, row))
      }
    val parameterSpecs = coefficientSpecs ++ standardErrorSpecs

    for
      parameters <- buildAll(parameterSpecs) { case (parameter, kind, values) =>
        ParameterMap.make(parameter, kind, values, shape, selectedVoxels, provenance, exportIntent)
      }
      covariance <- CoefficientCovarianceArtifact.fromDenseFit(result, shape, provenance, exportIntent)
    yield ResultManifest(provenance, parameters, contrasts = Vector.empty, coefficientCovariance = Some(covariance), exportIntent = exportIntent)

private def matrixRow(matrix: DMat, row: Int): DVec =
  val out = Vec.newBuilder(matrix.cols)
  var col = 0
  while col < matrix.cols do
    out(col) = matrix(row, col)
    col += 1
  out.result()

private def buildAll[A, B](
    values: Vector[A]
)(
    f: A => Either[FitError, B]
): Either[FitError, Vector[B]] =
  val out = Vector.newBuilder[B]
  var index = 0
  while index < values.length do
    f(values(index)) match
      case Left(error) => return Left(error)
      case Right(value) => out += value
    index += 1
  Right(out.result())
