package scalafim.fmri.fit

import scalafim.dataset.DatasetShape
import scalafim.fmri.model.{FitEngine, FitSummary}
import scalafim.linalg.{DoubleMatrix, DoubleVector}

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
    scopeLabel: String
):
  require(inferableColumns.nonEmpty, "coefficient inference provenance must contain inferable columns")
  require(inferableColumns.distinct.length == inferableColumns.length, "coefficient inference provenance columns must be unique")
  require(scopeLabel.trim.nonEmpty, "coefficient inference provenance scope label must be non-empty")

final case class AnalysisProvenance(
    engine: FitEngine,
    summary: FitSummary,
    timepoints: SelectedTimepointIndices,
    columnNames: Vector[String],
    source: String,
    coefficientInference: Option[CoefficientInferenceProvenance] = None,
    notes: Vector[String] = Vector.empty
):
  require(columnNames.nonEmpty, "analysis provenance column names must be non-empty")
  require(source.trim.nonEmpty, "analysis provenance source must be non-empty")
  require(coefficientInference.forall(value => value.inferableColumns.forall(columnNames.contains)), "inferable columns must belong to analysis columns")
  require(notes.forall(_.trim.nonEmpty), "analysis provenance notes must be non-empty")

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
            scopeLabel = dense.inferenceScope.label
          ))
        case _ =>
          None
    AnalysisProvenance(
      engine = result.engine,
      summary = result.summary,
      timepoints = SelectedTimepointIndices.unsafe(result.timepoints),
      columnNames = result.columnNames,
      source = source,
      coefficientInference = coefficientInference,
      notes = notes
    )

final case class StatMap private (
    name: ResultMapName,
    kind: StatisticKind,
    values: DoubleVector,
    shape: DatasetShape,
    selectedVoxels: SelectedVoxelIndices,
    provenance: AnalysisProvenance,
    exportIntent: ResultExportIntent
):
  def label: String = name.value
  def voxelIndices: Vector[Int] = selectedVoxels.toVector
  def valueVector: Vector[Double] = values.toVector

object StatMap:
  def make(
      name: String,
      kind: StatisticKind,
      values: DoubleVector,
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
      values: DoubleVector,
      shape: DatasetShape,
      selectedVoxels: SelectedVoxelIndices,
      provenance: AnalysisProvenance,
      exportIntent: ResultExportIntent = ResultExportIntent.InMemory
  ): StatMap =
    make(name, kind, values, shape, selectedVoxels, provenance, exportIntent)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def validateValues(
      values: DoubleVector,
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
    else if values.toVector.exists(value => !value.isFinite) then
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
      values: DoubleVector,
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
    degreesOfFreedom: Option[ContrastDegreesOfFreedom]
):
  require(map.kind == StatisticKind.Contrast(statistic), "contrast map statistic kind must match map kind")
  def contrast: String = contrastId.value

object ContrastMap:
  def make(
      contrastId: String,
      statistic: ContrastMapKind,
      values: DoubleVector,
      shape: DatasetShape,
      selectedVoxels: SelectedVoxelIndices,
      provenance: AnalysisProvenance,
      degreesOfFreedom: Option[ContrastDegreesOfFreedom] = None,
      exportIntent: ResultExportIntent = ResultExportIntent.InMemory
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
    yield ContrastMap(id, statistic, statMap, degreesOfFreedom)

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
      make(result.name, kind, values, shape, result.selectedVoxels, provenance, df, exportIntent)
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
    val estimateMaps: Vector[(ContrastMapKind, DoubleVector)] =
      Vector.tabulate(result.estimates.rows) { row =>
        ContrastMapKind.Estimate(row + 1) -> matrixRow(result.estimates, row)
      }
    buildAll(estimateMaps :+ (ContrastMapKind.FStatistic -> result.statistics)) { case (kind, values) =>
      make(result.name, kind, values, shape, result.selectedVoxels, provenance, df, exportIntent)
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

  def matrices: Vector[DoubleMatrix] =
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
      val out = new Array[Double](indices.length * indices.length)
      var row = 0
      while row < indices.length do
        var col = 0
        while col < indices.length do
          out(row * indices.length + col) = matrix(indices(row), indices(col))
          col += 1
        row += 1
      DoubleMatrix.unsafe(indices.length, indices.length, out)
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
    val coefficientSpecs: Vector[(String, ParameterMapKind, DoubleVector)] =
      result.columnNames.zipWithIndex.map { case (parameter, row) =>
        (parameter, ParameterMapKind.Coefficient, matrixRow(result.coefficients.value, row))
      }
    val standardErrorSpecs: Vector[(String, ParameterMapKind, DoubleVector)] =
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

private def matrixRow(matrix: DoubleMatrix, row: Int): DoubleVector =
  val out = new Array[Double](matrix.cols)
  var col = 0
  while col < matrix.cols do
    out(col) = matrix(row, col)
    col += 1
  DoubleVector.unsafe(out)

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
