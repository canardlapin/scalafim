package scalafim.fmri.mvpa

import gale.linalg.DMat
import multivar.core.SemanticSpace

enum ObservationDistance:
  case SquaredEuclidean(normalization: RdmNormalization)
  case Euclidean
  case Correlation

  def label: String =
    this match
      case SquaredEuclidean(normalization) =>
        s"squared-euclidean-${normalization.label}"
      case Euclidean   => "euclidean"
      case Correlation => "correlation"

  def minimumCoordinates: Int =
    this match
      case Correlation => 2
      case _           => 1

final case class ObservationPair(
    id: PairCoordinateId,
    first: SampleId,
    second: SampleId,
    firstPosition: Int,
    secondPosition: Int
)

/** Canonical unordered pairs from one exact sample axis. Unlike an effect RDM, this domain deliberately names samples
  * rather than pretending observations are experimental effects.
  */
final class ObservationPairDomain[S <: SemanticSpace] private (
    val samples: AxisRef.Aux[SampleId, S],
    val pairs: Vector[ObservationPair],
    val pairAxis: AxisRef[PairCoordinateId],
    val identity: ScientificComponentFingerprint,
    private val positions: Map[(SampleId, SampleId), Int]
):
  def size: Int = pairs.size

  def position(
      first: SampleId,
      second: SampleId
  ): Either[ObservationRdmFailure, Int] =
    positions
      .get(first -> second)
      .orElse(positions.get(second -> first))
      .toRight(ObservationRdmFailure.UnknownPair(first, second))

object ObservationPairDomain:
  val PairOrderProtocol = "canonical-sample-upper-triangle-v1"
  private val Kind = EstimandKind.unsafe("observation-pair-domain")

  def apply[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S]
  ): Either[ObservationRdmFailure, ObservationPairDomain[S]] =
    if samples.size < 2 then Left(ObservationRdmFailure.TooFewSamples(samples.size))
    else
      EstimandIdentity(
        Kind,
        Vector(
          "pair-order" -> PairOrderProtocol,
          "samples" -> samples.identity.fingerprint.value
        )
      ).left
        .map(ObservationRdmFailure.Identity.apply)
        .flatMap: component =>
          val pairs = Vector.newBuilder[ObservationPair]
          val keys = Vector.newBuilder[PairCoordinateId]
          val positions = Map.newBuilder[(SampleId, SampleId), Int]
          var first = 0
          var pairPosition = 0
          while first < samples.size - 1 do
            var second = first + 1
            while second < samples.size do
              val id = PairCoordinateId.unsafe(
                s"sample-pair-$pairPosition-${component.fingerprint.value.takeRight(16)}"
              )
              val left = samples.keys(first)
              val right = samples.keys(second)
              pairs += ObservationPair(
                id,
                left,
                right,
                first,
                second
              )
              keys += id
              positions += (left -> right) -> pairPosition
              pairPosition += 1
              second += 1
            first += 1
          AxisRef
            .create(
              AxisId.unsafe(
                s"sample-pairs-${component.fingerprint.value.takeRight(24)}"
              ),
              AxisPurpose.unsafe("sample-pairs"),
              keys.result(),
              CoordinateBasis.unsafe(
                "canonical-sample-pairs",
                "protocol" -> PairOrderProtocol,
                "samples" -> samples.identity.fingerprint.value
              ),
              None,
              AxisScale.nominal,
              CoordinateProvenance.unsafe(
                "scalafim-observation-rdm",
                PairOrderProtocol,
                component.fingerprint.value
              )
            )
            .left
            .map(ObservationRdmFailure.Axis.apply)
            .map: pairAxis =>
              new ObservationPairDomain(
                samples,
                pairs.result(),
                pairAxis,
                component.fingerprint,
                positions.result()
              )

enum ObservationRdmBindRejection:
  case SampleAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case SampleWitnessMismatch
  case TooFewFitSamples(actual: Int)
  case MeasurementTooSmall(
      measurement: MeasurementId,
      required: Int,
      actual: Int
  )

  def message: String =
    this match
      case SampleAxisMismatch(expected, actual) =>
        s"observation RDM design samples ${actual.value} do not match source samples ${expected.value}"
      case SampleWitnessMismatch =>
        "observation RDM design and source use different nominal sample witnesses"
      case TooFewFitSamples(actual) =>
        s"observation RDM requires at least two selected samples, obtained $actual"
      case MeasurementTooSmall(measurement, required, actual) =>
        s"measurement '${measurement.value}' has $actual coordinates; this distance requires at least $required"

enum ObservationRdmFailure:
  case Identity(error: ScientificIdentityError)
  case Axis(error: AxisRefError)
  case Evidence(error: EvidenceTableError)
  case Column(error: ColumnError)
  case ExecutionEvidence(error: ExecutionReceiptError)
  case TooFewSamples(actual: Int)
  case UnknownPair(first: SampleId, second: SampleId)
  case ZeroVarianceSample(sample: SampleId)
  case NonFiniteDistance(pair: PairCoordinateId, value: Double)

  def message: String =
    this match
      case Identity(error)          => error.message
      case Axis(error)              => error.message
      case Evidence(error)          => error.message
      case Column(error)            => error.message
      case ExecutionEvidence(error) => error.message
      case TooFewSamples(actual)    =>
        s"observation pair domain requires at least two samples, obtained $actual"
      case UnknownPair(first, second) =>
        s"observation pair domain does not contain '${first.value}' and '${second.value}'"
      case ZeroVarianceSample(sample) =>
        s"correlation distance is undefined for zero-variance sample '${sample.value}'"
      case NonFiniteDistance(pair, value) =>
        s"observation RDM produced non-finite distance $value for '${pair.value}'"

final class ObservationRdmEstimand[
    S <: SemanticSpace,
    N <: SemanticSpace,
    K
] private[mvpa] (
    val distance: ObservationDistance,
    val identity: EstimandIdentity
) extends Estimand[Observations[S, N, K], ObservationFitDesign[S]]:
  override type Result = MeasuredObservationRdm
  override type Rejection = ObservationRdmBindRejection
  override type Failure = ObservationRdmFailure

  override val defaultBoundaries: RequestedBoundaries =
    ObservationRdm.Boundaries

  override def rejectionMessage(value: ObservationRdmBindRejection): String =
    value.message

  override def failureMessage(value: ObservationRdmFailure): String =
    value.message

final class MeasuredObservationRdm private[mvpa] (
    val domain: ObservationPairDomain[?],
    val distances: Column[domain.pairAxis.Id, Double]
):
  def distance(
      first: SampleId,
      second: SampleId
  ): Either[ObservationRdmFailure, Double] =
    domain
      .position(first, second)
      .flatMap: position =>
        distances.at(position).left.map(ObservationRdmFailure.Column.apply)

object ObservationRdm:
  private val Kind = EstimandKind.unsafe("observation-rdm")

  private[mvpa] val Boundaries =
    RequestedBoundaries.trusted(
      Vector(
        OutputBoundaryIdentity.trusted(
          OutputBoundaryId.unsafe("sample-pair-distances")
        )
      )
    )

  def apply[S <: SemanticSpace, N <: SemanticSpace, K](
      source: Observations[S, N, K],
      distance: ObservationDistance
  ): ObservationRdmEstimand[S, N, K] =
    val identity = EstimandIdentity.trusted(
      Kind,
      Vector(
        "distance" -> distance.label,
        "pair-order" -> ObservationPairDomain.PairOrderProtocol,
        "source" -> source.identity.fingerprint.value
      )
    )
    new ObservationRdmEstimand(distance, identity)

  given compiler[S <: SemanticSpace, N <: SemanticSpace, K, R]: Compile[
    Observations[S, N, K],
    ObservationFitDesign[S],
    ObservationRdmEstimand[S, N, K],
    R
  ] with
    override type Prepared = Unit

    override def prepare(
        specification: ScientificSpecification[
          Observations[S, N, K],
          ObservationFitDesign[S],
          ObservationRdmEstimand[S, N, K],
          R
        ]
    ): Either[specification.Rejection, Unit] =
      val source = specification.source
      val design = specification.design
      if source.samples.identity != design.samples.identity then
        Left(
          ObservationRdmBindRejection.SampleAxisMismatch(
            source.samples.identity.fingerprint,
            design.samples.identity.fingerprint
          )
        )
      else if !(source.samples.evidence eq design.samples.evidence) then
        Left(ObservationRdmBindRejection.SampleWitnessMismatch)
      else if design.selection.size < 2 then
        Left(
          ObservationRdmBindRejection.TooFewFitSamples(
            design.selection.size
          )
        )
      else
        specification.frame.entries.find(
          _.measurement.local.size < specification.estimand.distance.minimumCoordinates
        ) match
          case Some(entry) =>
            Left(
              ObservationRdmBindRejection.MeasurementTooSmall(
                entry.measurement.identity.id,
                specification.estimand.distance.minimumCoordinates,
                entry.measurement.local.size
              )
            )
          case None => Right(())

  given task[S <: SemanticSpace, N <: SemanticSpace, K, R]: MeasurementTask[
    Observations[S, N, K],
    ObservationFitDesign[S],
    ObservationRdmEstimand[S, N, K],
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
                "observation RDM consumes one explicitly budgeted local sample table"
              )
            )
          case MaterializationPolicy.Allow(_) => Right(())

    override def execute(
        plan: BoundScientificPlan[
          Observations[S, N, K],
          ObservationFitDesign[S],
          ObservationRdmEstimand[S, N, K],
          R,
          Unit
        ]
    )(
        entry: MeasurementEntry[N, K, ?, R],
        context: TaskContext
    ): Either[
      TaskReportError,
      TaskReport[plan.Result, plan.Rejection, plan.Failure]
    ] =
      evaluate(
        plan.specification.source,
        plan.specification.design,
        plan.specification.estimand,
        entry.measurement,
        context.strategy.materialization
      ) match
        case Left(error) =>
          TaskReport.failed(error, context.strategy.target)
        case Right(execution) =>
          ExecutionMaterialization(
            ExecutionScope.Measurement(entry.measurement.identity.id),
            execution.materialization,
            "observation RDM requires the declared local sample table"
          ) match
            case Left(error) =>
              TaskReport.failed(
                ObservationRdmFailure.ExecutionEvidence(error),
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

  private final case class ObservationRdmExecution(
      result: MeasuredObservationRdm,
      materialization: MaterializationReceipt,
      operatorApplications: Long
  )

  private def evaluate[
      S <: SemanticSpace,
      N <: SemanticSpace,
      K,
      L
  ](
      source: Observations[S, N, K],
      design: ObservationFitDesign[S],
      estimand: ObservationRdmEstimand[S, N, K],
      measurement: Measurement[N, K, L],
      policy: MaterializationPolicy
  ): Either[ObservationRdmFailure, ObservationRdmExecution] =
    for
      selected <- source.evidence
        .restrictRows(design.selection)
        .left
        .map(ObservationRdmFailure.Evidence.apply)
      measured <- selected
        .measureColumns(measurement)
        .left
        .map(ObservationRdmFailure.Evidence.apply)
      materialized <- measured
        .materialize(policy)
        .left
        .map(ObservationRdmFailure.Evidence.apply)
      domain <- ObservationPairDomain(selected.rows)
      values <- ObservationDistanceKernel.compute(
        materialized.value,
        domain,
        estimand.distance
      )
      distances <- Column(domain.pairAxis, values).left
        .map(ObservationRdmFailure.Column.apply)
    yield ObservationRdmExecution(
      new MeasuredObservationRdm(domain, distances),
      materialized.receipt,
      measurement.local.size.toLong
    )

private[mvpa] object ObservationDistanceKernel:
  def compute[S <: SemanticSpace](
      matrix: DMat,
      domain: ObservationPairDomain[S],
      distance: ObservationDistance
  ): Either[ObservationRdmFailure, Vector[Double]] =
    distance match
      case ObservationDistance.SquaredEuclidean(normalization) =>
        squaredEuclidean(matrix, domain, normalization)
      case ObservationDistance.Euclidean =>
        squaredEuclidean(matrix, domain, RdmNormalization.Raw)
          .map(_.map(value => math.sqrt(math.max(value, 0.0))))
      case ObservationDistance.Correlation =>
        correlation(matrix, domain)

  private def squaredEuclidean[S <: SemanticSpace](
      matrix: DMat,
      domain: ObservationPairDomain[S],
      normalization: RdmNormalization
  ): Either[ObservationRdmFailure, Vector[Double]] =
    val output = Vector.newBuilder[Double]
    var pair = 0
    while pair < domain.pairs.length do
      val coordinate = domain.pairs(pair)
      var sum = 0.0
      var column = 0
      while column < matrix.cols do
        val difference =
          matrix(coordinate.firstPosition, column) -
            matrix(coordinate.secondPosition, column)
        sum += difference * difference
        column += 1
      val value = normalization match
        case RdmNormalization.Raw                     => sum
        case RdmNormalization.DivideByNeuralDimension =>
          sum / matrix.cols.toDouble
      if !value.isFinite then
        return Left(
          ObservationRdmFailure.NonFiniteDistance(coordinate.id, value)
        )
      output += value
      pair += 1
    Right(output.result())

  private def correlation[S <: SemanticSpace](
      matrix: DMat,
      domain: ObservationPairDomain[S]
  ): Either[ObservationRdmFailure, Vector[Double]] =
    val means = new Array[Double](matrix.rows)
    val norms = new Array[Double](matrix.rows)
    var row = 0
    while row < matrix.rows do
      var sum = 0.0
      var column = 0
      while column < matrix.cols do
        sum += matrix(row, column)
        column += 1
      means(row) = sum / matrix.cols.toDouble
      var sumSquares = 0.0
      column = 0
      while column < matrix.cols do
        val centered = matrix(row, column) - means(row)
        sumSquares += centered * centered
        column += 1
      norms(row) = math.sqrt(sumSquares)
      if norms(row) == 0.0 then
        return Left(
          ObservationRdmFailure.ZeroVarianceSample(domain.samples.keys(row))
        )
      row += 1

    val output = Vector.newBuilder[Double]
    var pair = 0
    while pair < domain.pairs.length do
      val coordinate = domain.pairs(pair)
      var dot = 0.0
      var column = 0
      while column < matrix.cols do
        dot +=
          (matrix(coordinate.firstPosition, column) - means(coordinate.firstPosition)) *
            (matrix(coordinate.secondPosition, column) - means(coordinate.secondPosition))
        column += 1
      val value = 1.0 - dot /
        (norms(coordinate.firstPosition) * norms(coordinate.secondPosition))
      if !value.isFinite then
        return Left(
          ObservationRdmFailure.NonFiniteDistance(coordinate.id, value)
        )
      output += value
      pair += 1
    Right(output.result())
