package scalafim.fmri.fit

import scalafim.fmri.ar.{NoisePooling, WhiteningMethod, WhiteningPlan, WhiteningTransform}
import scalafim.fmri.model.{
  ArStructure,
  MissingDataPolicy,
  NuisanceProjection,
  RobustPsi,
  VolumeWeighting
}
import gale.linalg.{DMat, Matrix}

opaque type TemporalNuisanceRank = Int

object TemporalNuisanceRank:
  def apply(value: Int): Either[FitError, TemporalNuisanceRank] =
    if value >= 0 then Right(value)
    else Left(FitError.InvalidFitAxis("temporal nuisance rank", s"value $value must be non-negative"))

  def unsafe(value: Int): TemporalNuisanceRank =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (rank: TemporalNuisanceRank)
    inline def value: Int = rank

final case class TrainingRunScope private (runs: Vector[RunIndex]):
  require(runs.nonEmpty, "training scope must contain at least one run")
  require(runs.map(_.value).distinct.length == runs.length, "training scope runs must be unique")

object TrainingRunScope:
  def fromRunIndices(runs: Vector[RunIndex]): Either[FitError, TrainingRunScope] =
    if runs.isEmpty then Left(FitError.InvalidFitAxis("training run scope", "must contain at least one run"))
    else if runs.map(_.value).distinct.length != runs.length then
      Left(FitError.InvalidFitAxis("training run scope", "run indices must be unique"))
    else Right(new TrainingRunScope(runs.sortBy(_.value)))

  def fromInts(runs: Vector[Int]): Either[FitError, TrainingRunScope] =
    val typed = Vector.newBuilder[RunIndex]
    var index = 0
    while index < runs.length do
      RunIndex(runs(index)) match
        case Left(error) => return Left(error)
        case Right(run)  => typed += run
      index += 1
    fromRunIndices(typed.result())

enum TemporalPreparationScope:
  case Fixed
  case PerRun(run: RunIndex)
  case TrainingFold(training: TrainingRunScope)

enum CanonicalTemporalWhitening:
  case Iid
  case Shared(plan: WhiteningPlan)

enum TemporalWhiteningReceipt:
  case Iid
  case Shared(
      method: WhiteningMethod,
      pooling: NoisePooling,
      arOrder: Int,
      segmentCount: Int,
      exactFirstAr1: Boolean
  )

final case class TemporalPreparationReceipt(
    selectedTimepoints: SelectedTimepointIndices,
    scope: TemporalPreparationScope,
    provenance: ResponsePreparationProvenance,
    nuisanceRank: TemporalNuisanceRank,
    designRank: Int,
    contrastRank: Int,
    contrastName: String,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    whitening: TemporalWhiteningReceipt
):
  require(designRank > 0, "temporal preparation design rank must be positive")
  require(contrastRank > 0, "temporal preparation contrast rank must be positive")
  require(contrastName.nonEmpty, "temporal preparation contrast name must be non-empty")
  require(nuisanceRank.value <= designRank, "temporal nuisance rank cannot exceed design rank")

trait PreparedCanonicalGeometry:
  def preparedDesign: DesignMatrix
  def inverseXtX: DMat
  def effectBasis: DMat
  def receipt: TemporalPreparationReceipt
  private[fit] def whitening: CanonicalTemporalWhitening

  final def prepareResponse(response: ResponseBlock): Either[FitError, ResponseBlock] =
    if response.timepoints != preparedDesign.timepoints then
      Left(FitError.RowMismatch(preparedDesign.timepoints, response.timepoints))
    else
      whitening match
        case CanonicalTemporalWhitening.Iid => Right(response)
        case CanonicalTemporalWhitening.Shared(plan) =>
          WhiteningTransform
            .matrix(plan, response.value)
            .left
            .map(error => FitError.UnsupportedAutocorrelation(error.message))
            .flatMap(ResponseBlock.fromMatrix)

/** Immutable, response-independent geometry for one prepared first-level contrast.
  *
  * `effectBasis` is the p-by-1 vector
  * `(X'X)^-1 C' / sqrt(C (X'X)^-1 C')`. Given `G = Z'X`, the contrast effect
  * matrix is therefore `(G effectBasis)(G effectBasis)'`; `inverseXtX` supplies
  * the residual correction `Z'Z - G inverseXtX G'` without a time-by-time
  * projector.
  */
final class PreparedContrastGeometry private[fit] (
    val preparedDesign: DesignMatrix,
    val alignedContrast: AlignedTContrast,
    val designCrossproduct: DMat,
    val inverseXtX: DMat,
    val effectBasis: DMat,
    val contrastVariance: Double,
    val receipt: TemporalPreparationReceipt,
    private[fit] val whitening: CanonicalTemporalWhitening
) extends PreparedCanonicalGeometry:
  require(effectBasis.rows == preparedDesign.predictors && effectBasis.cols == 1, "effect basis must be p-by-1")
  require(inverseXtX.rows == preparedDesign.predictors && inverseXtX.cols == preparedDesign.predictors, "inverse XtX must be p-by-p")
  require(contrastVariance > 0.0 && contrastVariance.isFinite, "contrast variance must be positive and finite")

object PreparedContrastGeometry:
  private[fit] def compile(
      preparation: ResponsePreparationPlan,
      design: DesignMatrix,
      columnNames: Vector[String],
      contrast: TContrast,
      selectedTimepoints: SelectedTimepointIndices,
      partitions: Vector[RunPartition],
      nuisanceRank: TemporalNuisanceRank,
      scope: TemporalPreparationScope,
      whitening: CanonicalTemporalWhitening,
      solvePolicy: OlsSolvePolicy
  ): Either[FitError, PreparedContrastGeometry] =
    for
      _ <- validateAxis(design, columnNames, selectedTimepoints, partitions, nuisanceRank)
      _ <- validatePreparation(preparation, design, partitions, scope, whitening)
      aligned <- contrast.align(columnNames)
      preparedDesign <- prepareDesign(design, whitening)
      prepared <- Ols.prepare(preparedDesign, solvePolicy)
      residualDf <- ResidualDegreesOfFreedom(preparedDesign.timepoints - prepared.diagnostics.rank)
      basisAndVariance <- effectBasis(prepared.normalizedCovariance, aligned)
      (basis, variance) = basisAndVariance
      provenance = compiledProvenance(preparation, whitening)
    yield
      new PreparedContrastGeometry(
        preparedDesign = preparedDesign,
        alignedContrast = aligned,
        designCrossproduct = prepared.crossproduct,
        inverseXtX = prepared.normalizedCovariance,
        effectBasis = basis,
        contrastVariance = variance,
        receipt = TemporalPreparationReceipt(
          selectedTimepoints = selectedTimepoints,
          scope = scope,
          provenance = provenance,
          nuisanceRank = nuisanceRank,
          designRank = prepared.diagnostics.rank,
          contrastRank = 1,
          contrastName = aligned.name,
          residualDegreesOfFreedom = residualDf,
          whitening = whiteningReceipt(whitening)
        ),
        whitening = whitening
      )

  private[fit] def validateAxis(
      design: DesignMatrix,
      columnNames: Vector[String],
      selectedTimepoints: SelectedTimepointIndices,
      partitions: Vector[RunPartition],
      nuisanceRank: TemporalNuisanceRank
  ): Either[FitError, Unit] =
    if columnNames.length != design.predictors then
      Left(FitError.InvalidFitAxis("design columns", s"expected ${design.predictors} names, got ${columnNames.length}"))
    else if columnNames.distinct.length != columnNames.length then
      Left(FitError.InvalidFitAxis("design columns", "names must be unique"))
    else if selectedTimepoints.length != design.timepoints then
      Left(FitError.InvalidFitAxis("selected timepoints", s"expected ${design.timepoints}, got ${selectedTimepoints.length}"))
    else if nuisanceRank.value > design.predictors then
      Left(FitError.InvalidFitAxis("temporal nuisance rank", s"value ${nuisanceRank.value} exceeds ${design.predictors} predictors"))
    else
      Gls.validatePartitions(partitions).flatMap: _ =>
        val ordered = partitions.flatMap(partition => partition.rowIndices.zip(partition.timepoints)).sortBy(_._1)
        val expectedRows = (0 until design.timepoints).toVector
        if ordered.map(_._1) != expectedRows then
          Left(FitError.InvalidFitAxis("run partitions", "rows must cover the prepared design exactly once"))
        else if ordered.map(_._2) != selectedTimepoints.toVector then
          Left(FitError.InvalidFitAxis("run partitions", "timepoints must match the selected timepoint axis"))
        else Right(())

  private[fit] def validatePreparation(
      preparation: ResponsePreparationPlan,
      design: DesignMatrix,
      partitions: Vector[RunPartition],
      scope: TemporalPreparationScope,
      whitening: CanonicalTemporalWhitening
  ): Either[FitError, Unit] =
    for
      _ <- preparation.missingData match
        case MissingDataPolicy.Error | MissingDataPolicy.ExcludeVoxel | MissingDataPolicy.Propagate => Right(())
        case MissingDataPolicy.OmitRowsPerVoxel =>
          Left(FitError.UnsupportedMissingDataPolicy(
            "response-independent contrast geometry cannot choose a voxel-specific observed-row pattern"
          ))
      _ <- preparation.volumeWeighting match
        case VolumeWeighting.Disabled => Right(())
        case _ => unsupportedPreparation("volume weighting is not executable in shared first-level preparation")
      _ <- preparation.nuisanceProjection match
        case NuisanceProjection.Disabled => Right(())
        case _ => unsupportedPreparation("matrix nuisance projection is not executable in shared first-level preparation")
      _ <- preparation.robust.psi match
        case RobustPsi.Disabled => Right(())
        case _ => unsupportedPreparation("robust response-dependent weights require a separately frozen transform")
      _ <- validateWhitening(preparation, design, partitions, scope, whitening)
    yield ()

  private def unsupportedPreparation(detail: String): Either[FitError, Unit] =
    Left(FitError.InvalidFitAxis("canonical temporal preparation", detail))

  private def validateWhitening(
      preparation: ResponsePreparationPlan,
      design: DesignMatrix,
      partitions: Vector[RunPartition],
      scope: TemporalPreparationScope,
      whitening: CanonicalTemporalWhitening
  ): Either[FitError, Unit] =
    preparation.autocorrelation.structure match
      case ArStructure.Iid =>
        whitening match
          case CanonicalTemporalWhitening.Iid =>
            if scope == TemporalPreparationScope.Fixed then Right(())
            else Left(FitError.InvalidFitAxis("temporal preparation scope", "iid preparation must have fixed scope"))
          case CanonicalTemporalWhitening.Shared(_) =>
            Left(FitError.InvalidFitAxis("canonical whitening", "iid preparation cannot carry a whitening plan"))
      case ArStructure.Ar(_) =>
        whitening match
          case CanonicalTemporalWhitening.Iid =>
            Left(FitError.InvalidFitAxis("canonical whitening", "AR preparation requires a shared whitening plan"))
          case CanonicalTemporalWhitening.Shared(plan) =>
            for
              expectedSegments <- Gls.timeSegments(partitions, preparation.autocorrelation.censoredTimepoints)
              _ <-
                if plan.nTimepoints != design.timepoints then
                  Left(FitError.InvalidFitAxis("canonical whitening", s"plan covers ${plan.nTimepoints} rows, expected ${design.timepoints}"))
                else if plan.segments != expectedSegments then
                  Left(FitError.InvalidFitAxis("canonical whitening", "plan segments do not match run partitions and censor resets"))
                else if plan.exactFirstAr1 != preparation.autocorrelation.exactFirst then
                  Left(FitError.InvalidFitAxis("canonical whitening", "initial-condition policy does not match first-level preparation"))
                else Right(())
              _ <- validateWhiteningOrigin(preparation, plan, scope)
            yield ()

  private def validateWhiteningOrigin(
      preparation: ResponsePreparationPlan,
      plan: WhiteningPlan,
      scope: TemporalPreparationScope
  ): Either[FitError, Unit] =
    val options = preparation.autocorrelation
    val expected = options.rho.map(Vector(_)).orElse(options.phi)
    expected match
      case Some(coefficients) =>
        if scope != TemporalPreparationScope.Fixed then
          Left(FitError.InvalidFitAxis("temporal preparation scope", "fixed AR coefficients require fixed scope"))
        else if plan.method != WhiteningMethod.Fixed then
          Left(FitError.InvalidFitAxis("canonical whitening", "fixed AR coefficients require a fixed whitening plan"))
        else if plan.coefficients.length != 1 || plan.coefficients.head.phi != coefficients then
          Left(FitError.InvalidFitAxis("canonical whitening", "plan coefficients do not match first-level AR coefficients"))
        else Right(())
      case None =>
        if plan.method != WhiteningMethod.Estimated then
          Left(FitError.InvalidFitAxis("canonical whitening", "estimated AR preparation requires an estimated whitening plan"))
        else if scope == TemporalPreparationScope.Fixed then
          Left(FitError.InvalidFitAxis("temporal preparation scope", "estimated whitening must declare per-run or training-fold scope"))
        else if options.voxelwise then
          Left(FitError.InvalidFitAxis("canonical whitening", "voxelwise whitening cannot define one shared feature geometry"))
        else
          val expectedPooling = if options.global then NoisePooling.Global else NoisePooling.Run
          if plan.pooling != expectedPooling then
            Left(FitError.InvalidFitAxis("canonical whitening", s"plan pooling ${plan.pooling} does not match $expectedPooling"))
          else Right(())

  private[fit] def prepareDesign(
      design: DesignMatrix,
      whitening: CanonicalTemporalWhitening
  ): Either[FitError, DesignMatrix] =
    whitening match
      case CanonicalTemporalWhitening.Iid => Right(design)
      case CanonicalTemporalWhitening.Shared(plan) =>
        WhiteningTransform
          .matrix(plan, design.value)
          .left
          .map(error => FitError.UnsupportedAutocorrelation(error.message))
          .flatMap(DesignMatrix.fromMatrix)

  private def effectBasis(inverseXtX: DMat, contrast: AlignedTContrast): Either[FitError, (DMat, Double)] =
    val unscaled = Matrix.newBuilder(inverseXtX.rows, 1)
    var row = 0
    while row < inverseXtX.rows do
      var value = 0.0
      var col = 0
      while col < inverseXtX.cols do
        value += inverseXtX(row, col) * contrast.weights(0, col)
        col += 1
      unscaled(row, 0) = value
      row += 1
    val vector = unscaled.result()
    var variance = 0.0
    row = 0
    while row < vector.rows do
      variance += contrast.weights(0, row) * vector(row, 0)
      row += 1
    if !(variance > 0.0 && variance.isFinite) then
      Left(FitError.NonEstimableContrast(contrast.name, s"contrast variance is $variance"))
    else
      val scale = 1.0 / Math.sqrt(variance)
      val out = Matrix.newBuilder(vector.rows, 1)
      row = 0
      while row < vector.rows do
        out(row, 0) = vector(row, 0) * scale
        row += 1
      Right(out.result() -> variance)

  private[fit] def compiledProvenance(
      preparation: ResponsePreparationPlan,
      whitening: CanonicalTemporalWhitening
  ): ResponsePreparationProvenance =
    val records = preparation.records.map:
      case ResponsePreparationRecord(step @ ResponsePreparationStep.Censoring(timepoints), _) if timepoints.nonEmpty =>
        ResponsePreparationRecord(step, ResponsePreparationDisposition.Applied("encoded as whitening segment resets"))
      case ResponsePreparationRecord(step @ ResponsePreparationStep.Whitening(_), _) =>
        whitening match
          case CanonicalTemporalWhitening.Iid =>
            ResponsePreparationRecord(step, ResponsePreparationDisposition.Disabled)
          case CanonicalTemporalWhitening.Shared(plan) =>
            ResponsePreparationRecord(step, ResponsePreparationDisposition.Applied(s"shared ${plan.method} AR(${plan.arOrder}) transform"))
      case record => record
    ResponsePreparationProvenance(records)

  private[fit] def whiteningReceipt(whitening: CanonicalTemporalWhitening): TemporalWhiteningReceipt =
    whitening match
      case CanonicalTemporalWhitening.Iid => TemporalWhiteningReceipt.Iid
      case CanonicalTemporalWhitening.Shared(plan) =>
        TemporalWhiteningReceipt.Shared(
          method = plan.method,
          pooling = plan.pooling,
          arOrder = plan.arOrder,
          segmentCount = plan.segments.length,
          exactFirstAr1 = plan.exactFirstAr1
        )
