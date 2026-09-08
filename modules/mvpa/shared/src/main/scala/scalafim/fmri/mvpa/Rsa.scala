package scalafim.fmri.mvpa

import multivar.core.SemanticSpace

enum SamplewiseRsaSourceError:
  case Identity(error: ScientificIdentityError)
  case SampleAxisMismatch(
      column: String,
      expected: AxisFingerprint,
      actual: AxisFingerprint
  )
  case SampleWitnessMismatch(column: String)
  case InvalidItemPurpose(actual: AxisPurpose)
  case InvalidBlockPurpose(actual: AxisPurpose)
  case UnknownItem(sample: SampleId, item: String)
  case UnknownBlock(sample: SampleId, block: String)
  case TooFewBlocks(actual: Int)

  def message: String =
    this match
      case Identity(error)                              => error.message
      case SampleAxisMismatch(column, expected, actual) =>
        s"samplewise RSA $column column belongs to ${actual.value}, expected ${expected.value}"
      case SampleWitnessMismatch(column) =>
        s"samplewise RSA $column column and observations use different nominal sample witnesses"
      case InvalidItemPurpose(actual) =>
        s"samplewise RSA item axis must have purpose '${AxisPurpose.Effects.value}', obtained '${actual.value}'"
      case InvalidBlockPurpose(actual) =>
        s"samplewise RSA block axis must have purpose '${AxisPurpose.Partitions.value}', obtained '${actual.value}'"
      case UnknownItem(sample, item) =>
        s"sample '${sample.value}' refers to item '$item' outside the declared item axis"
      case UnknownBlock(sample, block) =>
        s"sample '${sample.value}' refers to block '$block' outside the declared block axis"
      case TooFewBlocks(actual) =>
        s"samplewise RSA requires at least two represented blocks, obtained $actual"

/** Observation evidence plus the actual experimental-item and independence block assignments required by samplewise
  * RSA. No irrelevant response value can be supplied.
  */
final class SamplewiseRsaSource[
    S <: SemanticSpace,
    N <: SemanticSpace,
    E <: SemanticSpace,
    B <: SemanticSpace,
    NK,
    EK,
    BK
] private (
    val observations: Observations[S, N, NK],
    val itemAxisName: ScientificAxisName,
    val itemAxis: AxisRef.Aux[EK, E],
    val sampleItems: Column[S, EK],
    val blockAxisName: ScientificAxisName,
    val blockAxis: AxisRef.Aux[BK, B],
    val sampleBlocks: Column[S, BK],
    val identity: ScientificSourceIdentity
) extends ScientificSource:
  override type Neural = N
  override type NeuralKey = NK

  def samples: AxisRef.Aux[SampleId, S] = observations.samples
  def sampleAxisName: ScientificAxisName = observations.sampleAxisName
  def neuralAxisName: ScientificAxisName = observations.neuralAxisName

  override def neuralAxis: AxisRef.Aux[NK, N] = observations.neuralAxis

  def estimate(
      model: SecondOrderModel[SignalModelRole, E, EK],
      distance: ObservationDistance = ObservationDistance.Correlation,
      comparison: SamplewiseComparison = SamplewiseComparison.Pearson
  ): SamplewiseRsaEstimand[S, N, E, B, NK, EK, BK] =
    SamplewiseRsa.estimand(this, model, distance, comparison)

object SamplewiseRsaSource:
  private val Protocol = "scalafim-samplewise-rsa-source/v1"

  def apply[
      S <: SemanticSpace,
      N <: SemanticSpace,
      E <: SemanticSpace,
      B <: SemanticSpace,
      NK,
      EK,
      BK
  ](
      observations: Observations[S, N, NK],
      itemAxisName: ScientificAxisName,
      itemAxis: AxisRef.Aux[EK, E],
      sampleItems: Column[S, EK],
      blockAxisName: ScientificAxisName,
      blockAxis: AxisRef.Aux[BK, B],
      sampleBlocks: Column[S, BK]
  )(using
      itemCodec: AxisKeyCodec[EK],
      blockCodec: AxisKeyCodec[BK]
  ): Either[
    SamplewiseRsaSourceError,
    SamplewiseRsaSource[S, N, E, B, NK, EK, BK]
  ] =
    for
      _ <- validateOwner(observations, sampleItems, "item")
      _ <- validateOwner(observations, sampleBlocks, "block")
      _ <-
        if itemAxis.identity.purpose == AxisPurpose.Effects then Right(())
        else Left(SamplewiseRsaSourceError.InvalidItemPurpose(itemAxis.identity.purpose))
      _ <-
        if blockAxis.identity.purpose == AxisPurpose.Partitions then Right(())
        else Left(SamplewiseRsaSourceError.InvalidBlockPurpose(blockAxis.identity.purpose))
      _ <- validateAssignments(
        observations.samples,
        itemAxis,
        sampleItems,
        itemCodec,
        SamplewiseRsaSourceError.UnknownItem.apply
      )
      _ <- validateAssignments(
        observations.samples,
        blockAxis,
        sampleBlocks,
        blockCodec,
        SamplewiseRsaSourceError.UnknownBlock.apply
      )
      representedBlocks = sampleBlocks.toVector.distinct.length
      _ <-
        if representedBlocks >= 2 then Right(())
        else Left(SamplewiseRsaSourceError.TooFewBlocks(representedBlocks))
      identity <- ScientificSourceIdentity(
        ScientificSourceKind.unsafe("samplewise-rsa-observations"),
        Vector(
          ScientificSourceAxis(
            observations.sampleAxisName,
            observations.samples.identity
          ),
          ScientificSourceAxis(
            observations.neuralAxisName,
            observations.neuralAxis.identity
          ),
          ScientificSourceAxis(itemAxisName, itemAxis.identity),
          ScientificSourceAxis(blockAxisName, blockAxis.identity)
        ),
        Vector(
          "blocks" -> assignmentDigest(sampleBlocks, blockCodec),
          "items" -> assignmentDigest(sampleItems, itemCodec),
          "observations" -> observations.identity.fingerprint.value,
          "protocol" -> Protocol
        )
      ).left.map(SamplewiseRsaSourceError.Identity.apply)
    yield new SamplewiseRsaSource(
      observations,
      itemAxisName,
      itemAxis,
      sampleItems,
      blockAxisName,
      blockAxis,
      sampleBlocks,
      identity
    )

  private def validateOwner[
      S <: SemanticSpace,
      N <: SemanticSpace,
      NK,
      A
  ](
      observations: Observations[S, N, NK],
      column: Column[S, A],
      label: String
  ): Either[SamplewiseRsaSourceError, Unit] =
    if column.rowIdentity != observations.samples.identity then
      Left(
        SamplewiseRsaSourceError.SampleAxisMismatch(
          label,
          observations.samples.identity.fingerprint,
          column.rowIdentity.fingerprint
        )
      )
    else if !(column.rows eq observations.samples.evidence) then
      Left(SamplewiseRsaSourceError.SampleWitnessMismatch(label))
    else Right(())

  private def validateAssignments[S <: SemanticSpace, A, AS <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      axis: AxisRef.Aux[A, AS],
      assignments: Column[S, A],
      codec: AxisKeyCodec[A],
      failure: (SampleId, String) => SamplewiseRsaSourceError
  ): Either[SamplewiseRsaSourceError, Unit] =
    var position = 0
    while position < assignments.size do
      val value = assignments.values(position)
      if axis.positionOf(value).isEmpty then
        return Left(
          failure(samples.keys(position), codec.encode(value).value)
        )
      position += 1
    Right(())

  private def assignmentDigest[S <: SemanticSpace, A](
      values: Column[S, A],
      codec: AxisKeyCodec[A]
  ): String =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.string(values.rowIdentity.fingerprint.value)
    writer.int(values.size)
    var position = 0
    while position < values.size do
      writer.string(codec.encode(values.values(position)).value)
      position += 1
    AxisDigest.sha256Hex(writer.result())

final class SamplewiseRsaDesign[
    S <: SemanticSpace,
    B <: SemanticSpace
] private (
    val samples: AxisRef.Aux[SampleId, S],
    val sampleAxisName: ScientificAxisName,
    val blocks: AxisRef.Aux[?, B],
    val blockAxisName: ScientificAxisName,
    val identity: DesignIdentity,
    val referencedAxes: Vector[DesignAxisReference]
) extends EvidenceDesign

object SamplewiseRsaDesign:
  private val Protocol = "scalafim-samplewise-rsa-design/v1"

  def apply[
      S <: SemanticSpace,
      N <: SemanticSpace,
      E <: SemanticSpace,
      B <: SemanticSpace,
      NK,
      EK,
      BK
  ](
      source: SamplewiseRsaSource[S, N, E, B, NK, EK, BK]
  ): Either[ScientificIdentityError, SamplewiseRsaDesign[S, B]] =
    DesignIdentity(
      DesignKind.unsafe("samplewise-cross-block-comparison"),
      Vector(
        "blocks" -> source.blockAxis.identity.fingerprint.value,
        "comparison" -> "each-sample-against-other-blocks",
        "protocol" -> Protocol,
        "samples" -> source.samples.identity.fingerprint.value
      )
    ).map: identity =>
      new SamplewiseRsaDesign(
        source.samples,
        source.sampleAxisName,
        source.blockAxis,
        source.blockAxisName,
        identity,
        Vector(
          DesignAxisReference(source.sampleAxisName, source.samples.identity),
          DesignAxisReference(source.blockAxisName, source.blockAxis.identity)
        )
      )

enum SamplewiseComparison:
  case Pearson
  case Spearman

  def label: String =
    this match
      case Pearson  => "pearson"
      case Spearman => "spearman"

enum SamplewiseRsaBindRejection:
  case SampleAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case SampleWitnessMismatch
  case BlockAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case BlockWitnessMismatch
  case ModelAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case ModelWitnessMismatch
  case MeasurementTooSmall(
      measurement: MeasurementId,
      required: Int,
      actual: Int
  )

  def message: String =
    this match
      case SampleAxisMismatch(expected, actual) =>
        s"samplewise RSA design samples ${actual.value} do not match source samples ${expected.value}"
      case SampleWitnessMismatch =>
        "samplewise RSA design and source use different nominal sample witnesses"
      case BlockAxisMismatch(expected, actual) =>
        s"samplewise RSA design blocks ${actual.value} do not match source blocks ${expected.value}"
      case BlockWitnessMismatch =>
        "samplewise RSA design and source use different nominal block witnesses"
      case ModelAxisMismatch(expected, actual) =>
        s"samplewise RSA model items ${actual.value} do not match source items ${expected.value}"
      case ModelWitnessMismatch =>
        "samplewise RSA model and source use different nominal item witnesses"
      case MeasurementTooSmall(measurement, required, actual) =>
        s"measurement '${measurement.value}' has $actual coordinates; this distance requires at least $required"

enum SamplewiseRsaFailure:
  case Evidence(error: EvidenceTableError)
  case Column(error: ColumnError)
  case ObservationRdm(error: ObservationRdmFailure)
  case ExecutionEvidence(error: ExecutionReceiptError)
  case ModelQuery(error: RelationalQueryError)
  case NonFiniteValue(label: String, value: Double)

  def message: String =
    this match
      case Evidence(error)              => error.message
      case Column(error)                => error.message
      case ObservationRdm(error)        => error.message
      case ExecutionEvidence(error)     => error.message
      case ModelQuery(error)            => error.message
      case NonFiniteValue(label, value) =>
        s"samplewise RSA $label produced non-finite value $value"

final class SamplewiseRsaEstimand[
    S <: SemanticSpace,
    N <: SemanticSpace,
    E <: SemanticSpace,
    B <: SemanticSpace,
    NK,
    EK,
    BK
] private[mvpa] (
    val model: SecondOrderModel[SignalModelRole, E, EK],
    val distance: ObservationDistance,
    val comparison: SamplewiseComparison,
    val identity: EstimandIdentity
) extends Estimand[
      SamplewiseRsaSource[S, N, E, B, NK, EK, BK],
      SamplewiseRsaDesign[S, B]
    ]:
  override type Result = MeasuredSamplewiseRsa[S]
  override type Rejection = SamplewiseRsaBindRejection
  override type Failure = SamplewiseRsaFailure

  override val defaultBoundaries: RequestedBoundaries = SamplewiseRsa.Boundaries

  override def rejectionMessage(value: SamplewiseRsaBindRejection): String =
    value.message

  override def failureMessage(value: SamplewiseRsaFailure): String =
    value.message

final class MeasuredSamplewiseRsa[S <: SemanticSpace] private[mvpa] (
    val samples: AxisRef.Aux[SampleId, S],
    val scores: Column[S, Option[Double]]
):
  def meanScore: Option[Double] =
    val defined = scores.toVector.flatten
    if defined.isEmpty then None
    else Some(defined.sum / defined.length.toDouble)

object SamplewiseRsa:
  private val Kind = EstimandKind.unsafe("samplewise-rsa")

  private[mvpa] val Boundaries =
    RequestedBoundaries.trusted(
      Vector(
        OutputBoundaryIdentity.trusted(
          OutputBoundaryId.unsafe("samplewise-rsa-scores")
        )
      )
    )

  private[mvpa] def estimand[
      S <: SemanticSpace,
      N <: SemanticSpace,
      E <: SemanticSpace,
      B <: SemanticSpace,
      NK,
      EK,
      BK
  ](
      source: SamplewiseRsaSource[S, N, E, B, NK, EK, BK],
      model: SecondOrderModel[SignalModelRole, E, EK],
      distance: ObservationDistance,
      comparison: SamplewiseComparison
  ): SamplewiseRsaEstimand[S, N, E, B, NK, EK, BK] =
    val identity = EstimandIdentity.trusted(
      Kind,
      Vector(
        "comparison" -> comparison.label,
        "distance" -> distance.label,
        "model" -> model.identity.value,
        "pairing" -> "each-sample-against-other-blocks",
        "source" -> source.identity.fingerprint.value
      )
    )
    new SamplewiseRsaEstimand(model, distance, comparison, identity)

  given compiler[
      S <: SemanticSpace,
      N <: SemanticSpace,
      E <: SemanticSpace,
      B <: SemanticSpace,
      NK,
      EK,
      BK,
      R
  ]: Compile[
    SamplewiseRsaSource[S, N, E, B, NK, EK, BK],
    SamplewiseRsaDesign[S, B],
    SamplewiseRsaEstimand[S, N, E, B, NK, EK, BK],
    R
  ] with
    override type Prepared = Unit

    override def prepare(
        specification: ScientificSpecification[
          SamplewiseRsaSource[S, N, E, B, NK, EK, BK],
          SamplewiseRsaDesign[S, B],
          SamplewiseRsaEstimand[S, N, E, B, NK, EK, BK],
          R
        ]
    ): Either[specification.Rejection, Unit] =
      val source = specification.source
      val design = specification.design
      val model = specification.estimand.model
      if source.samples.identity != design.samples.identity then
        Left(
          SamplewiseRsaBindRejection.SampleAxisMismatch(
            source.samples.identity.fingerprint,
            design.samples.identity.fingerprint
          )
        )
      else if !(source.samples.evidence eq design.samples.evidence) then
        Left(SamplewiseRsaBindRejection.SampleWitnessMismatch)
      else if source.blockAxis.identity != design.blocks.identity then
        Left(
          SamplewiseRsaBindRejection.BlockAxisMismatch(
            source.blockAxis.identity.fingerprint,
            design.blocks.identity.fingerprint
          )
        )
      else if !(source.blockAxis.evidence eq design.blocks.evidence) then
        Left(SamplewiseRsaBindRejection.BlockWitnessMismatch)
      else if source.itemAxis.identity != model.domain.items.identity then
        Left(
          SamplewiseRsaBindRejection.ModelAxisMismatch(
            source.itemAxis.identity.fingerprint,
            model.domain.items.identity.fingerprint
          )
        )
      else if !(source.itemAxis.evidence eq model.domain.items.evidence) then
        Left(SamplewiseRsaBindRejection.ModelWitnessMismatch)
      else
        specification.frame.entries.find(
          _.measurement.local.size < specification.estimand.distance.minimumCoordinates
        ) match
          case Some(entry) =>
            Left(
              SamplewiseRsaBindRejection.MeasurementTooSmall(
                entry.measurement.identity.id,
                specification.estimand.distance.minimumCoordinates,
                entry.measurement.local.size
              )
            )
          case None => Right(())

  given task[
      S <: SemanticSpace,
      N <: SemanticSpace,
      E <: SemanticSpace,
      B <: SemanticSpace,
      NK,
      EK,
      BK,
      R
  ]: MeasurementTask[
    SamplewiseRsaSource[S, N, E, B, NK, EK, BK],
    SamplewiseRsaDesign[S, B],
    SamplewiseRsaEstimand[S, N, E, B, NK, EK, BK],
    R,
    Unit
  ] with
    override def validate(
        strategy: ExecutionStrategy
    ): Either[ExecutionPlanError, Unit] =
      if strategy.representation != ExecutionRepresentation.Dense then
        Left(
          ExecutionPlanError.UnsupportedRepresentation(
            strategy.representation,
            Vector(ExecutionRepresentation.Dense)
          )
        )
      else if strategy.precision != NumericPrecision.Binary64 then
        Left(
          ExecutionPlanError.UnsupportedPrecision(
            strategy.precision,
            Vector(NumericPrecision.Binary64)
          )
        )
      else if strategy.solver != SolverChoice.NotApplicable then
        Left(ExecutionPlanError.UnsupportedSolver(strategy.solver))
      else
        strategy.materialization match
          case MaterializationPolicy.Reject =>
            Left(
              ExecutionPlanError.MaterializationRequired(
                "samplewise RSA consumes one explicitly budgeted local observation table"
              )
            )
          case MaterializationPolicy.Allow(_) => Right(())

    override def execute(
        plan: BoundScientificPlan[
          SamplewiseRsaSource[S, N, E, B, NK, EK, BK],
          SamplewiseRsaDesign[S, B],
          SamplewiseRsaEstimand[S, N, E, B, NK, EK, BK],
          R,
          Unit
        ]
    )(
        entry: MeasurementEntry[N, NK, ?, R],
        context: TaskContext
    ): Either[
      TaskReportError,
      TaskReport[plan.Result, plan.Rejection, plan.Failure]
    ] =
      evaluate(
        plan.specification.source,
        plan.specification.estimand,
        entry.measurement,
        context.strategy.materialization
      ) match
        case Left(error)      => TaskReport.failed(error, context.strategy.target)
        case Right(execution) =>
          ExecutionMaterialization(
            ExecutionScope.Measurement(entry.measurement.identity.id),
            execution.materialization,
            "samplewise RSA requires the declared local observation table"
          ) match
            case Left(error) =>
              TaskReport.failed(
                SamplewiseRsaFailure.ExecutionEvidence(error),
                context.strategy.target,
                operatorApplications = execution.operatorApplications
              )
            case Right(materialization) =>
              TaskReport.success(
                execution.result,
                context.strategy.target,
                operatorApplications = execution.operatorApplications,
                materializations = Vector(materialization)
              )

  private final case class SamplewiseRsaExecution[S <: SemanticSpace](
      result: MeasuredSamplewiseRsa[S],
      materialization: MaterializationReceipt,
      operatorApplications: Long
  )

  private def evaluate[
      S <: SemanticSpace,
      N <: SemanticSpace,
      E <: SemanticSpace,
      B <: SemanticSpace,
      NK,
      EK,
      BK,
      L
  ](
      source: SamplewiseRsaSource[S, N, E, B, NK, EK, BK],
      estimand: SamplewiseRsaEstimand[S, N, E, B, NK, EK, BK],
      measurement: Measurement[N, NK, L],
      policy: MaterializationPolicy
  ): Either[SamplewiseRsaFailure, SamplewiseRsaExecution[S]] =
    for
      measured <- source.observations.evidence
        .measureColumns(measurement)
        .left
        .map(SamplewiseRsaFailure.Evidence.apply)
      materialized <- measured
        .materialize(policy)
        .left
        .map(SamplewiseRsaFailure.Evidence.apply)
      domain <- ObservationPairDomain(source.samples).left
        .map(SamplewiseRsaFailure.ObservationRdm.apply)
      observed <- ObservationDistanceKernel
        .compute(materialized.value, domain, estimand.distance)
        .left
        .map(SamplewiseRsaFailure.ObservationRdm.apply)
      values <- scoreRows(source, estimand, domain, observed)
      scores <- Column(source.samples, values).left
        .map(SamplewiseRsaFailure.Column.apply)
    yield SamplewiseRsaExecution(
      new MeasuredSamplewiseRsa(source.samples, scores),
      materialized.receipt,
      measurement.local.size.toLong
    )

  private def scoreRows[
      S <: SemanticSpace,
      N <: SemanticSpace,
      E <: SemanticSpace,
      B <: SemanticSpace,
      NK,
      EK,
      BK
  ](
      source: SamplewiseRsaSource[S, N, E, B, NK, EK, BK],
      estimand: SamplewiseRsaEstimand[S, N, E, B, NK, EK, BK],
      domain: ObservationPairDomain[S],
      observed: Vector[Double]
  ): Either[SamplewiseRsaFailure, Vector[Option[Double]]] =
    val observedRow = new Array[Double](source.samples.size)
    val modelRow = new Array[Double](source.samples.size)
    val output = Vector.newBuilder[Option[Double]]
    var row = 0
    while row < source.samples.size do
      var length = 0
      var column = 0
      while column < source.samples.size do
        if source.sampleBlocks.values(column) != source.sampleBlocks.values(row) then
          val observedPosition = domain
            .position(
              source.samples.keys(row),
              source.samples.keys(column)
            )
            .left
            .map(SamplewiseRsaFailure.ObservationRdm.apply) match
            case Left(error)  => return Left(error)
            case Right(value) => value
          observedRow(length) = observed(observedPosition)
          val firstItem = source.sampleItems.values(row)
          val secondItem = source.sampleItems.values(column)
          if firstItem == secondItem then modelRow(length) = 0.0
          else
            val modelPosition = estimand.model.domain
              .position(firstItem, secondItem)
              .left
              .map(SamplewiseRsaFailure.ModelQuery.apply) match
              case Left(error)  => return Left(error)
              case Right(value) => value
            estimand.model.values.at(modelPosition) match
              case Left(error) =>
                return Left(SamplewiseRsaFailure.Column(error))
              case Right(value) => modelRow(length) = value
          length += 1
        column += 1
      SamplewiseComparisonKernel.compare(
        observedRow,
        modelRow,
        length,
        estimand.comparison
      ) match
        case Left(error)  => return Left(error)
        case Right(value) => output += value
      row += 1
    Right(output.result())

private object SamplewiseComparisonKernel:
  def compare(
      observed: Array[Double],
      model: Array[Double],
      length: Int,
      comparison: SamplewiseComparison
  ): Either[SamplewiseRsaFailure, Option[Double]] =
    if length < 2 then Right(None)
    else
      comparison match
        case SamplewiseComparison.Pearson =>
          pearson(observed, model, length)
        case SamplewiseComparison.Spearman =>
          for
            observedRanks <- ranks(observed, length, "observed")
            modelRanks <- ranks(model, length, "model")
            value <- pearson(observedRanks, modelRanks, length)
          yield value

  private def pearson(
      observed: Array[Double],
      model: Array[Double],
      length: Int
  ): Either[SamplewiseRsaFailure, Option[Double]] =
    var observedSum = 0.0
    var modelSum = 0.0
    var position = 0
    while position < length do
      if !observed(position).isFinite then
        return Left(
          SamplewiseRsaFailure.NonFiniteValue("observed row", observed(position))
        )
      if !model(position).isFinite then
        return Left(
          SamplewiseRsaFailure.NonFiniteValue("model row", model(position))
        )
      observedSum += observed(position)
      modelSum += model(position)
      position += 1
    val observedMean = observedSum / length.toDouble
    val modelMean = modelSum / length.toDouble
    var numerator = 0.0
    var observedSumSquares = 0.0
    var modelSumSquares = 0.0
    position = 0
    while position < length do
      val left = observed(position) - observedMean
      val right = model(position) - modelMean
      numerator += left * right
      observedSumSquares += left * left
      modelSumSquares += right * right
      position += 1
    val denominator = math.sqrt(observedSumSquares * modelSumSquares)
    if denominator == 0.0 then Right(None)
    else Right(Some(numerator / denominator))

  private def ranks(
      values: Array[Double],
      length: Int,
      label: String
  ): Either[SamplewiseRsaFailure, Array[Double]] =
    val indexed = new Array[(Double, Int)](length)
    var position = 0
    while position < length do
      val value = values(position)
      if !value.isFinite then return Left(SamplewiseRsaFailure.NonFiniteValue(label, value))
      indexed(position) = value -> position
      position += 1
    val sorted = indexed.toVector.sortBy(_._1)
    val output = new Array[Double](length)
    var start = 0
    while start < sorted.length do
      var end = start + 1
      while end < sorted.length && sorted(end)._1 == sorted(start)._1 do end += 1
      val rank = (start + 1 + end).toDouble / 2.0
      position = start
      while position < end do
        output(sorted(position)._2) = rank
        position += 1
      start = end
    Right(output)
