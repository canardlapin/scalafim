package scalafim.fmri.fit

import gale.linalg.Matrix
import scalafim.fmri.model.{
  ArOptions,
  ArStructure,
  DvarsWeightEstimator,
  DvarsWeightFunction,
  DvarsWeightScope,
  FitConfig,
  FitPlan,
  FixedWeightAlignment,
  MissingDataPolicy,
  NuisanceProjection as ModelNuisanceProjection,
  RobustOptions,
  RobustPsi,
  VolumeWeighting
}

enum ResponsePreparationStep:
  case MissingData(policy: MissingDataPolicy)
  case Censoring(timepoints: Vector[Int])
  case VolumeWeights(weighting: VolumeWeighting)
  case NuisanceProjection(projection: ModelNuisanceProjection)
  case Whitening(autocorrelation: ArOptions)
  case RobustWeights(options: RobustOptions)

  def label: String =
    this match
      case MissingData(_)          => "missing_data"
      case Censoring(_)            => "censoring"
      case VolumeWeights(_)        => "volume_weights"
      case NuisanceProjection(_)   => "nuisance_projection"
      case Whitening(_)            => "whitening"
      case RobustWeights(_)        => "robust_weights"

enum ResponsePreparationDisposition:
  case Disabled
  case Planned(detail: String)
  case Applied(detail: String)
  case Deferred(detail: String)
  case Rejected(detail: String)

  def label: String =
    this match
      case Disabled      => "disabled"
      case Planned(_)    => "planned"
      case Applied(_)    => "applied"
      case Deferred(_)   => "deferred"
      case Rejected(_)   => "rejected"

  def detailText: String =
    this match
      case Disabled         => ""
      case Planned(value)   => value
      case Applied(value)   => value
      case Deferred(value)  => value
      case Rejected(value)  => value

enum VolumeWeightingSource:
  case Fixed(alignment: FixedWeightAlignment)
  case ResponseDvars(estimator: DvarsWeightEstimator)

enum VolumeWeightNormalization:
  case None
  case MeanOne(scope: DvarsWeightScope)

final case class VolumeWeightPartitionReceipt(
    runIndex: RunIndex,
    timepoints: Vector[Int]
):
  require(timepoints.nonEmpty, "volume-weight partition must contain timepoints")

/** The exact temporal weighting geometry executed before factorization. */
final case class VolumeWeightingReceipt(
    source: VolumeWeightingSource,
    normalization: VolumeWeightNormalization,
    inputTimepoints: Vector[Int],
    retainedTimepoints: Vector[Int],
    excludedTimepoints: Vector[Int],
    zeroWeightTimepoints: Vector[Int],
    weights: Vector[Double],
    qualityMetric: Option[Vector[Double]],
    partitions: Vector[VolumeWeightPartitionReceipt]
):
  require(inputTimepoints.nonEmpty, "volume-weight receipt must contain input timepoints")
  require(weights.length == inputTimepoints.length, "volume weights must align with receipt input timepoints")
  require(weights.forall(weight => weight >= 0.0 && weight.isFinite), "receipt weights must be non-negative and finite")
  require(qualityMetric.forall(_.length == inputTimepoints.length), "quality metric must align with receipt input timepoints")
  require(retainedTimepoints.nonEmpty, "volume weighting must retain at least one timepoint")
  require(retainedTimepoints.forall(inputTimepoints.contains), "retained timepoints must belong to the input axis")
  require(excludedTimepoints.forall(inputTimepoints.contains), "excluded timepoints must belong to the input axis")
  require(zeroWeightTimepoints.forall(excludedTimepoints.contains), "zero-weight timepoints must be reported as excluded")

final case class ResponsePreparationRecord(
    step: ResponsePreparationStep,
    disposition: ResponsePreparationDisposition
)

final case class ResponsePreparationProvenance(
    records: Vector[ResponsePreparationRecord],
    volumeWeighting: Option[VolumeWeightingReceipt] = None
):
  require(records.nonEmpty, "response preparation provenance must contain at least one record")

  def deferred: Vector[ResponsePreparationRecord] =
    records.collect { case record @ ResponsePreparationRecord(_, ResponsePreparationDisposition.Deferred(_)) => record }

  def applied: Vector[ResponsePreparationRecord] =
    records.collect { case record @ ResponsePreparationRecord(_, ResponsePreparationDisposition.Applied(_)) => record }

  def planned: Vector[ResponsePreparationRecord] =
    records.collect { case record @ ResponsePreparationRecord(_, ResponsePreparationDisposition.Planned(_)) => record }

  def rejected: Vector[ResponsePreparationRecord] =
    records.collect { case record @ ResponsePreparationRecord(_, ResponsePreparationDisposition.Rejected(_)) => record }

final case class PreparedFitBlockInput(
    input: FitBlockInput,
    provenance: ResponsePreparationProvenance
)

final case class ResponsePreparationPlan(
    missingData: MissingDataPolicy,
    censoredTimepoints: Vector[Int],
    volumeWeighting: VolumeWeighting,
    nuisanceProjection: ModelNuisanceProjection,
    autocorrelation: ArOptions,
    robust: RobustOptions
):
  def records: Vector[ResponsePreparationRecord] =
    recordsFor(None)

  private def recordsFor(
      weightingReceipt: Option[VolumeWeightingReceipt]
  ): Vector[ResponsePreparationRecord] =
    Vector(
      ResponsePreparationRecord(
        ResponsePreparationStep.MissingData(missingData),
        missingData match
          case MissingDataPolicy.Error =>
            ResponsePreparationDisposition.Applied("dense input constructors reject non-finite response values")
          case MissingDataPolicy.ExcludeVoxel | MissingDataPolicy.Propagate =>
            ResponsePreparationDisposition.Applied(
              "response columns containing any non-finite selected value are excluded before finite block construction"
            )
          case MissingDataPolicy.OmitRowsPerVoxel =>
            ResponsePreparationDisposition.Applied(
              "response columns are grouped by exact finite-row mask and each observation pattern is fit independently"
            )
      ),
      ResponsePreparationRecord(
        ResponsePreparationStep.Censoring(censoredTimepoints),
        if censoredTimepoints.isEmpty then ResponsePreparationDisposition.Disabled
        else ResponsePreparationDisposition.Deferred("censoring is consumed by AR/GLS preparation")
      ),
      ResponsePreparationRecord(
        ResponsePreparationStep.VolumeWeights(volumeWeighting),
        volumeWeighting match
          case VolumeWeighting.Disabled =>
            ResponsePreparationDisposition.Disabled
          case _ =>
            weightingReceipt match
              case None =>
                ResponsePreparationDisposition.Planned(
                  "resolve one temporal weight vector, apply sqrt(w) to design and response, and exclude exact zero-weight rows before factorization"
                )
              case Some(receipt) =>
                ResponsePreparationDisposition.Applied(
                  s"applied sqrt(w) to ${receipt.retainedTimepoints.length} rows and excluded ${receipt.excludedTimepoints.length} zero-weight rows"
                )
      ),
      ResponsePreparationRecord(
        ResponsePreparationStep.NuisanceProjection(nuisanceProjection),
        nuisanceProjection match
          case ModelNuisanceProjection.Disabled =>
            ResponsePreparationDisposition.Disabled
          case ModelNuisanceProjection.MatrixProjection(_, _) =>
            ResponsePreparationDisposition.Deferred("soft nuisance projection is represented but not applied in this slice")
      ),
      ResponsePreparationRecord(
        ResponsePreparationStep.Whitening(autocorrelation),
        autocorrelation.structure match
          case ArStructure.Iid if autocorrelation.rho.isEmpty && autocorrelation.phi.isEmpty && autocorrelation.censoredTimepoints.isEmpty =>
            ResponsePreparationDisposition.Disabled
          case _ =>
            ResponsePreparationDisposition.Deferred("autocorrelation whitening is handled by the GLS interpreter")
      ),
      ResponsePreparationRecord(
        ResponsePreparationStep.RobustWeights(robust),
        robust.psi match
          case RobustPsi.Disabled =>
            ResponsePreparationDisposition.Disabled
          case _ =>
            ResponsePreparationDisposition.Deferred("robust IWLS weights require the RobustLeastSquares interpreter")
      )
    )

  def provenance: ResponsePreparationProvenance =
    ResponsePreparationProvenance(records)

  def prepare(input: FitBlockInput): Either[FitError, PreparedFitBlockInput] =
    for
      resolved <- resolve(input)
      prepared <- resolved.prepare(input)
    yield prepared

  private[fit] def resolve(
      input: FitBlockInput
  ): Either[FitError, ResolvedResponsePreparation] =
    for
      _ <- validateFor(input)
      weighting <- ResolvedVolumeWeighting.resolve(volumeWeighting, input)
    yield ResolvedResponsePreparation(this, weighting)

  private[fit] def executedProvenance(
      receipt: Option[VolumeWeightingReceipt]
  ): ResponsePreparationProvenance =
    ResponsePreparationProvenance(recordsFor(receipt), receipt)

  /** Compile this declared temporal preparation into the response-independent
    * geometry used by canonical contrast analysis. Unsupported deferred transforms
    * are rejected rather than silently omitted.
    */
  def prepareContrast(
      design: DesignMatrix,
      columnNames: Vector[String],
      contrast: TContrast,
      selectedTimepoints: SelectedTimepointIndices,
      partitions: Vector[RunPartition],
      nuisanceRank: TemporalNuisanceRank,
      scope: TemporalPreparationScope = TemporalPreparationScope.Fixed,
      whitening: CanonicalTemporalWhitening = CanonicalTemporalWhitening.Iid,
      solvePolicy: OlsSolvePolicy = OlsSolvePolicy.Default
  ): Either[FitError, PreparedContrastGeometry] =
    PreparedContrastGeometry.compile(
      preparation = this,
      design = design,
      columnNames = columnNames,
      contrast = contrast,
      selectedTimepoints = selectedTimepoints,
      partitions = partitions,
      nuisanceRank = nuisanceRank,
      scope = scope,
      whitening = whitening,
      solvePolicy = solvePolicy
    )

  /** Compile a full-rank multi-contrast hypothesis into normalized temporal
    * geometry for held-out MANOVA.
    */
  def prepareManova(
      design: DesignMatrix,
      columnNames: Vector[String],
      contrast: FContrast,
      selectedTimepoints: SelectedTimepointIndices,
      partitions: Vector[RunPartition],
      nuisanceRank: TemporalNuisanceRank,
      scope: TemporalPreparationScope = TemporalPreparationScope.Fixed,
      whitening: CanonicalTemporalWhitening = CanonicalTemporalWhitening.Iid,
      solvePolicy: OlsSolvePolicy = OlsSolvePolicy.Default
  ): Either[FitError, PreparedManovaGeometry] =
    PreparedManovaGeometry.compile(
      preparation = this,
      design = design,
      columnNames = columnNames,
      contrast = contrast,
      selectedTimepoints = selectedTimepoints,
      partitions = partitions,
      nuisanceRank = nuisanceRank,
      scope = scope,
      whitening = whitening,
      solvePolicy = solvePolicy
    )

  private[fit] def validateFor(input: FitBlockInput): Either[FitError, Unit] =
    if volumeWeighting != VolumeWeighting.Disabled && input.lssDesign.nonEmpty then
      Left(FitError.UnsupportedVolumeWeighting("temporal weighting is not supported for prepared LSS designs"))
    else
      nuisanceProjection match
        case ModelNuisanceProjection.MatrixProjection(matrix, _) if matrix.rows != input.timepoints.length =>
          Left(
            FitError.InvalidFitAxis(
              "nuisance projection",
              s"matrix rows ${matrix.rows} do not match selected timepoints ${input.timepoints.length}"
            )
          )
        case _ =>
          Right(())

private[fit] final case class ResolvedResponsePreparation(
    plan: ResponsePreparationPlan,
    volumeWeighting: Option[ResolvedVolumeWeighting]
):
  def prepare(input: FitBlockInput): Either[FitError, PreparedFitBlockInput] =
    for
      _ <- plan.validateFor(input)
      weighted <- volumeWeighting match
        case None          => Right(input)
        case Some(resolved) => resolved.applyTo(input)
    yield
      val receipt = volumeWeighting.map(_.receipt)
      val provenance = plan.executedProvenance(receipt)
      PreparedFitBlockInput(
        input = weighted.copy(preparationProvenance = Some(provenance)),
        provenance = provenance
      )

private[fit] final case class ResolvedVolumeWeighting private (
    timepoints: Vector[Int],
    weights: Vector[Double],
    receipt: VolumeWeightingReceipt
):
  require(timepoints == receipt.inputTimepoints, "resolved volume-weight timepoints must match the receipt")
  require(weights == receipt.weights, "resolved volume weights must match the receipt")

  def applyTo(input: FitBlockInput): Either[FitError, FitBlockInput] =
    if input.timepoints != timepoints then
      Left(FitError.InvalidVolumeWeights(
        s"resolved timepoints ${timepoints.mkString(", ")} do not match response rows ${input.timepoints.mkString(", ")}"
      ))
    else
      ResolvedVolumeWeighting.scaleAndFilter(input, weights)

private[fit] object ResolvedVolumeWeighting:
  def resolve(
      weighting: VolumeWeighting,
      input: FitBlockInput
  ): Either[FitError, Option[ResolvedVolumeWeighting]] =
    weighting match
      case VolumeWeighting.Disabled =>
        Right(None)
      case VolumeWeighting.Fixed(values, alignment) =>
        resolveFixed(values, alignment, input).map(Some(_))
      case VolumeWeighting.Estimated(estimator) =>
        estimateDvars(estimator, input).map(Some(_))

  private def resolveFixed(
      values: Vector[Double],
      alignment: FixedWeightAlignment,
      input: FitBlockInput
  ): Either[FitError, ResolvedVolumeWeighting] =
    for
      partitions <- validatedPartitions(input)
      selected <- alignment match
        case FixedWeightAlignment.FullSeries =>
          if input.timepoints.exists(_ < 0) then
            Left(FitError.InvalidVolumeWeights("selected timepoints must be non-negative"))
          else if input.timepoints.exists(_ >= values.length) then
            Left(FitError.InvalidVolumeWeights(
              s"full-series length ${values.length} cannot address selected timepoints ${input.timepoints.mkString(", ")}"
            ))
          else Right(input.timepoints.map(values))
        case FixedWeightAlignment.SelectedRows =>
          if values.length == input.timepoints.length then Right(values)
          else
            Left(FitError.InvalidVolumeWeights(
              s"selected-row length ${values.length} does not match ${input.timepoints.length} response rows"
            ))
      _ <- validateWeights(selected, input.timepoints)
      resolved <- make(
        input = input,
        weights = selected,
        source = VolumeWeightingSource.Fixed(alignment),
        normalization = VolumeWeightNormalization.None,
        qualityMetric = None,
        partitions = partitions
      )
    yield resolved

  private def estimateDvars(
      estimator: DvarsWeightEstimator,
      input: FitBlockInput
  ): Either[FitError, ResolvedVolumeWeighting] =
    for
      partitions <- validatedPartitions(input)
      estimated <- DvarsVolumeWeightEstimator.estimate(input.response, partitions, estimator)
      (weights, dvars) = estimated
      _ <- validateWeights(weights, input.timepoints)
      resolved <- make(
        input = input,
        weights = weights,
        source = VolumeWeightingSource.ResponseDvars(estimator),
        normalization = VolumeWeightNormalization.MeanOne(estimator.scope),
        qualityMetric = Some(dvars),
        partitions = partitions
      )
    yield resolved

  private def make(
      input: FitBlockInput,
      weights: Vector[Double],
      source: VolumeWeightingSource,
      normalization: VolumeWeightNormalization,
      qualityMetric: Option[Vector[Double]],
      partitions: Vector[RunPartition]
  ): Either[FitError, ResolvedVolumeWeighting] =
    val retained = input.timepoints.zip(weights).collect { case (timepoint, weight) if weight > 0.0 => timepoint }
    val zeros = input.timepoints.zip(weights).collect { case (timepoint, weight) if weight == 0.0 => timepoint }
    if retained.isEmpty then
      Left(FitError.InvalidVolumeWeights("all selected timepoints have zero weight"))
    else
      val receipt = VolumeWeightingReceipt(
        source = source,
        normalization = normalization,
        inputTimepoints = input.timepoints,
        retainedTimepoints = retained,
        excludedTimepoints = zeros,
        zeroWeightTimepoints = zeros,
        weights = weights,
        qualityMetric = qualityMetric,
        partitions = partitions.map(partition => VolumeWeightPartitionReceipt(partition.typedRunIndex, partition.timepoints))
      )
      Right(new ResolvedVolumeWeighting(input.timepoints, weights, receipt))

  private def validateWeights(
      weights: Vector[Double],
      timepoints: Vector[Int]
  ): Either[FitError, Unit] =
    if weights.length != timepoints.length then
      Left(FitError.InvalidVolumeWeights(
        s"weight length ${weights.length} does not match selected rows ${timepoints.length}"
      ))
    else
      weights.zip(timepoints).collectFirst {
        case (weight, timepoint) if !weight.isFinite => s"timepoint $timepoint has non-finite weight $weight"
        case (weight, timepoint) if weight < 0.0 => s"timepoint $timepoint has negative weight $weight"
      } match
        case Some(detail) => Left(FitError.InvalidVolumeWeights(detail))
        case None         => Right(())

  private def validatedPartitions(input: FitBlockInput): Either[FitError, Vector[RunPartition]] =
    val partitions =
      if input.partitions.nonEmpty then input.partitions
      else Vector(RunPartition(0, input.timepoints.indices.toVector, input.timepoints))
    val seen = Array.fill(input.timepoints.length)(false)
    var partitionIndex = 0
    while partitionIndex < partitions.length do
      val partition = partitions(partitionIndex)
      if partition.rowIndices.length != partition.timepoints.length then
        return Left(FitError.InvalidVolumeWeights(s"run ${partition.runIndex} row/timepoint axes differ in length"))
      var index = 0
      var previousRow = -1
      var previousTimepoint = -1
      while index < partition.rowIndices.length do
        val row = partition.rowIndices(index)
        val timepoint = partition.timepoints(index)
        if row < 0 || row >= input.timepoints.length then
          return Left(FitError.InvalidVolumeWeights(s"run ${partition.runIndex} row $row is outside the selected response"))
        if seen(row) then
          return Left(FitError.InvalidVolumeWeights(s"selected row $row occurs in more than one run partition"))
        if input.timepoints(row) != timepoint then
          return Left(FitError.InvalidVolumeWeights(
            s"run ${partition.runIndex} row $row names timepoint $timepoint but the response axis names ${input.timepoints(row)}"
          ))
        if row <= previousRow || timepoint <= previousTimepoint then
          return Left(FitError.InvalidVolumeWeights(s"run ${partition.runIndex} rows and timepoints must be strictly increasing"))
        seen(row) = true
        previousRow = row
        previousTimepoint = timepoint
        index += 1
      partitionIndex += 1
    if seen.exists(value => !value) then
      val missing = seen.indices.filterNot(index => seen(index)).mkString(", ")
      Left(FitError.InvalidVolumeWeights(s"run partitions do not cover selected rows: $missing"))
    else Right(partitions)

  private def scaleAndFilter(
      input: FitBlockInput,
      weights: Vector[Double]
  ): Either[FitError, FitBlockInput] =
    val keptRows = weights.indices.filter(index => weights(index) > 0.0).toVector
    if keptRows.isEmpty then Left(FitError.InvalidVolumeWeights("all selected timepoints have zero weight"))
    else if keptRows.length != weights.length && input.runwiseProjections.nonEmpty then
      Left(FitError.UnsupportedVolumeWeighting("zero-row exclusion is not supported with runwise coefficient projections"))
    else
      val design = Matrix.newBuilder(keptRows.length, input.design.predictors)
      val response = Matrix.newBuilder(keptRows.length, input.response.voxels)
      val oldToNew = Array.fill(input.design.timepoints)(-1)
      var outputRow = 0
      while outputRow < keptRows.length do
        val inputRow = keptRows(outputRow)
        oldToNew(inputRow) = outputRow
        val scale = math.sqrt(weights(inputRow))
        var predictor = 0
        while predictor < input.design.predictors do
          design(outputRow, predictor) = input.design.value(inputRow, predictor) * scale
          predictor += 1
        var voxel = 0
        while voxel < input.response.voxels do
          response(outputRow, voxel) = input.response.value(inputRow, voxel) * scale
          voxel += 1
        outputRow += 1
      val partitions = input.partitions.flatMap { partition =>
        val kept = partition.rowIndices.zip(partition.timepoints).collect {
          case (row, timepoint) if oldToNew(row) >= 0 => oldToNew(row) -> timepoint
        }
        if kept.isEmpty then None
        else Some(RunPartition(partition.runIndex, kept.map(_._1), kept.map(_._2)))
      }
      val preparedResponse = ResponseBlock.unsafe(response.result())
      Right(input.copy(
        design = DesignMatrix.unsafe(design.result()),
        response = preparedResponse,
        timepoints = keptRows.map(input.timepoints),
        partitions = partitions,
        voxelStatuses = Some(VoxelFitStatus.classify(preparedResponse))
      ))

private[fit] object DvarsVolumeWeightEstimator:
  def estimate(
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      estimator: DvarsWeightEstimator
  ): Either[FitError, (Vector[Double], Vector[Double])] =
    val raw = Array.fill(response.timepoints)(Double.NaN)
    var partitionIndex = 0
    while partitionIndex < partitions.length do
      val partition = partitions(partitionIndex)
      var index = 1
      while index < partition.rowIndices.length do
        val previousTimepoint = partition.timepoints(index - 1)
        val timepoint = partition.timepoints(index)
        if timepoint == previousTimepoint + 1 then
          val row = partition.rowIndices(index)
          val previousRow = partition.rowIndices(index - 1)
          var sumSquares = 0.0
          var voxel = 0
          while voxel < response.voxels do
            val difference = response.value(row, voxel) - response.value(previousRow, voxel)
            sumSquares += difference * difference
            voxel += 1
          raw(row) = math.sqrt(sumSquares / response.voxels.toDouble)
        index += 1
      partitionIndex += 1

    val groups =
      estimator.scope match
        case DvarsWeightScope.WithinRun => partitions.map(_.rowIndices)
        case DvarsWeightScope.AcrossSelection => Vector(partitions.flatMap(_.rowIndices))
    val normalizedDvars = Array.fill(response.timepoints)(Double.NaN)
    val weights = Array.fill(response.timepoints)(Double.NaN)
    var groupIndex = 0
    while groupIndex < groups.length do
      val rows = groups(groupIndex)
      val derivatives = rows.iterator.map(raw).filter(_.isFinite).toVector
      if derivatives.isEmpty then
        return Left(FitError.InvalidVolumeWeights(
          s"DVARS group $groupIndex has no adjacent selected timepoints; at least one temporal difference is required"
        ))
      val fallback = median(derivatives)
      val completed = rows.map(row => if raw(row).isFinite then raw(row) else fallback)
      val scale = median(completed)
      var index = 0
      while index < rows.length do
        normalizedDvars(rows(index)) = if scale > 0.0 then completed(index) / scale else completed(index)
        index += 1
      val unnormalizedWeights = rows.map(row => transform(normalizedDvars(row), estimator.function))
      val meanWeight = unnormalizedWeights.sum / unnormalizedWeights.length.toDouble
      if !(meanWeight > 0.0 && meanWeight.isFinite) then
        return Left(FitError.InvalidVolumeWeights(s"DVARS group $groupIndex produced no positive finite weights"))
      index = 0
      while index < rows.length do
        weights(rows(index)) = unnormalizedWeights(index) / meanWeight
        index += 1
      groupIndex += 1
    if weights.exists(value => !value.isFinite) || normalizedDvars.exists(value => !value.isFinite) then
      Left(FitError.InvalidVolumeWeights("DVARS estimation did not cover every selected response row"))
    else Right(weights.toVector -> normalizedDvars.toVector)

  private def transform(value: Double, function: DvarsWeightFunction): Double =
    val raw =
      function match
        case DvarsWeightFunction.InverseSquared =>
          1.0 / (1.0 + value * value)
        case DvarsWeightFunction.SoftThreshold(threshold, steepness) =>
          val excess = (math.max(value, threshold.value) - threshold.value) / threshold.value
          1.0 / (1.0 + math.pow(excess, steepness.value))
        case DvarsWeightFunction.TukeyBisquare(threshold) =>
          val u = value / (threshold.value * 2.0)
          if math.abs(u) <= 1.0 then
            val oneMinus = 1.0 - u * u
            oneMinus * oneMinus
          else 0.0
    math.max(0.0, math.min(1.0, raw))

  private def median(values: Vector[Double]): Double =
    val sorted = values.sorted
    val middle = sorted.length / 2
    if sorted.length % 2 == 1 then sorted(middle)
    else (sorted(middle - 1) + sorted(middle)) / 2.0

object ResponsePreparationPlan:
  def fromPlan(plan: FitPlan): ResponsePreparationPlan =
    fromConfig(plan.config)

  def fromConfig(config: FitConfig): ResponsePreparationPlan =
    ResponsePreparationPlan(
      missingData = config.missingData,
      censoredTimepoints = config.autocorrelation.censoredTimepoints,
      volumeWeighting = config.volumeWeighting,
      nuisanceProjection = config.nuisanceProjection,
      autocorrelation = config.autocorrelation,
      robust = config.robust
    )
