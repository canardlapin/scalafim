package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.Matrix
import multivar.core.SemanticSpace

enum FirstOrderBindRejection:
  case EffectAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case EffectWitnessMismatch
  case PartitionWitnessMismatch
  case EmptyQuerySupport(query: String)
  case NonEstimableEffect(partition: PartitionId, effect: String)

  def message: String =
    this match
      case EffectAxisMismatch(expected, actual) =>
        s"effect query axis ${actual.value} does not match relation axis ${expected.value}"
      case EffectWitnessMismatch =>
        "effect query and relations use different nominal effect witnesses"
      case PartitionWitnessMismatch =>
        "partition reduction and relations use different nominal partition witnesses"
      case EmptyQuerySupport(query) =>
        s"effect query '$query' has no non-zero coefficient"
      case NonEstimableEffect(partition, effect) =>
        s"partition '${partition.value}' cannot estimate queried effect '$effect'"

enum FirstOrderTaskFailure:
  case Evidence(error: EvidenceTableError)
  case Relation(error: RelationError)
  case Result(error: FirstOrderResultError)

  def message: String =
    this match
      case Evidence(error) => error.message
      case Relation(error) => error.message
      case Result(error)   => error.message

enum FirstOrderResultError:
  case UnknownQuery(key: String)
  case InvalidLocalPosition(position: Int, size: Int)
  case NonFiniteOutput(position: Int, value: Double)

  def message: String =
    this match
      case UnknownQuery(key)                    => s"effect map has no query coordinate '$key'"
      case InvalidLocalPosition(position, size) =>
        s"effect map local position $position is outside [0,$size)"
      case NonFiniteOutput(position, value) =>
        s"effect map contains non-finite value $value at row-major position $position"

enum EffectCoefficientUnits:
  case DimensionlessLinearCombination

  def label: String = "dimensionless-linear-combination"

final case class EffectMapUnits(
    sourceUnits: Option[AxisUnits],
    coefficientUnits: EffectCoefficientUnits
):
  def label: String =
    sourceUnits.fold("dimensionless")(units => units.value)

final case class PartitionEffectReceipt(
    partition: PartitionId,
    relationFit: ScientificComponentFingerprint,
    coefficient: Double
)

/** Method-specific work for one requested first-order effect map. The scientific plan and measurement identity live in
  * the enclosing result; the dense cell count is the declared output boundary, not a hidden intermediate
  * materialization.
  */
final class FirstOrderComputationReceipt private[mvpa] (
    val partitions: Vector[PartitionEffectReceipt],
    val outputCells: Long
)

/** A first-order query closes the experimental and partition boundaries but leaves the measured neural boundary open.
  * Its query axis remains nominally typed; the heterogeneous local axis is carried by exact measurement and axis
  * identities.
  */
final class MeasuredEffectMap[
    Query <: SemanticSpace,
    QueryKey
] private[mvpa] (
    val queries: AxisRef.Aux[QueryKey, Query],
    val local: AxisIdentity,
    val values: DMat,
    val units: EffectMapUnits,
    val computation: FirstOrderComputationReceipt
):
  def rows: Int = values.rows
  def columns: Int = values.cols

  def value(
      query: QueryKey,
      localPosition: Int
  ): Either[FirstOrderResultError, Double] =
    queries.positionOf(query) match
      case None =>
        Left(
          FirstOrderResultError.UnknownQuery(
            queryKey(query)
          )
        )
      case Some(_) if localPosition < 0 || localPosition >= columns =>
        Left(FirstOrderResultError.InvalidLocalPosition(localPosition, columns))
      case Some(queryPosition) => Right(values(queryPosition, localPosition))

  private def queryKey(key: QueryKey): String =
    queries
      .positionOf(key)
      .map(queries.identity.orderedKeys(_).value)
      .getOrElse(key.toString)

private[mvpa] final class FirstOrderPrepared(val queryTranspose: DMat)

final class ContrastEffectEstimand[
    P <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace,
    Q <: SemanticSpace,
    EK,
    NK,
    QK,
    C <: RelationCapabilities[N, NK]
] private[mvpa] (
    val query: EffectQuery[E, Q, EK, QK],
    val identity: EstimandIdentity
) extends Estimand[
      PartitionedRelations[P, E, N, EK, NK, C],
      PartitionReductionDesign[P]
    ]:
  override type Result = MeasuredEffectMap[Q, QK]
  override type Rejection = FirstOrderBindRejection
  override type Failure = FirstOrderTaskFailure

  override val defaultBoundaries: RequestedBoundaries =
    FirstOrderAnalysis.EffectMapBoundaries

  override def rejectionMessage(value: FirstOrderBindRejection): String = value.message
  override def failureMessage(value: FirstOrderTaskFailure): String = value.message

object FirstOrderAnalysis:
  private val Kind = EstimandKind.unsafe("contrast-effect-map")

  private[mvpa] val EffectMapBoundaries =
    val boundary = OutputBoundaryIdentity.trusted(
      OutputBoundaryId.unsafe("measured-effect-map")
    )
    RequestedBoundaries.trusted(Vector(boundary))

  def contrast[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      Q <: SemanticSpace,
      EK,
      NK,
      QK,
      C <: RelationCapabilities[N, NK]
  ](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      query: EffectQuery[E, Q, EK, QK]
  ): ContrastEffectEstimand[P, E, N, Q, EK, NK, QK, C] =
    val identity = EstimandIdentity.trusted(
      Kind,
      Vector(
        "operation" -> "first-order-linear-effect-query",
        "query" -> query.identity.value,
        "source" -> source.identity.fingerprint.value
      )
    )
    new ContrastEffectEstimand(query, identity)

  given compiler[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      Q <: SemanticSpace,
      EK,
      NK,
      QK,
      C <: RelationCapabilities[N, NK],
      R
  ]: Compile[
    PartitionedRelations[P, E, N, EK, NK, C],
    PartitionReductionDesign[P],
    ContrastEffectEstimand[P, E, N, Q, EK, NK, QK, C],
    R
  ] with
    override type Prepared = FirstOrderPrepared

    override def prepare(
        specification: ScientificSpecification[
          PartitionedRelations[P, E, N, EK, NK, C],
          PartitionReductionDesign[P],
          ContrastEffectEstimand[P, E, N, Q, EK, NK, QK, C],
          R
        ]
    ): Either[specification.Rejection, FirstOrderPrepared] =
      prepareQuery(
        specification.source,
        specification.design,
        specification.estimand.query
      )

  given task[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      Q <: SemanticSpace,
      EK,
      NK,
      QK,
      C <: RelationCapabilities[N, NK],
      R
  ]: MeasurementTask[
    PartitionedRelations[P, E, N, EK, NK, C],
    PartitionReductionDesign[P],
    ContrastEffectEstimand[P, E, N, Q, EK, NK, QK, C],
    R,
    FirstOrderPrepared
  ] with
    override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
      validateStrategy(strategy)

    override def execute(
        plan: BoundScientificPlan[
          PartitionedRelations[P, E, N, EK, NK, C],
          PartitionReductionDesign[P],
          ContrastEffectEstimand[P, E, N, Q, EK, NK, QK, C],
          R,
          FirstOrderPrepared
        ]
    )(
        entry: MeasurementEntry[N, NK, ?, R],
        context: TaskContext
    ): Either[TaskReportError, TaskReport[plan.Result, plan.Rejection, plan.Failure]] =
      evaluate(
        plan.specification.source,
        plan.specification.design,
        plan.specification.estimand,
        plan.prepared,
        entry.measurement
      ) match
        case Left(error) =>
          TaskReport.failed(error, context.strategy.target)
        case Right(result) =>
          val operatorApplications =
            result.computation.partitions.length.toLong *
              plan.specification.estimand.query.output.size.toLong
          TaskReport.success(
            result,
            context.strategy.target,
            operatorApplications = operatorApplications
          )

  private def prepareQuery[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      Q <: SemanticSpace,
      EK,
      NK,
      QK,
      C <: RelationCapabilities[N, NK]
  ](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      design: PartitionReductionDesign[P],
      query: EffectQuery[E, Q, EK, QK]
  ): Either[FirstOrderBindRejection, FirstOrderPrepared] =
    if query.source.identity != source.effects.identity then
      Left(
        FirstOrderBindRejection.EffectAxisMismatch(
          source.effects.identity.fingerprint,
          query.source.identity.fingerprint
        )
      )
    else if !(query.source.evidence eq source.effects.evidence) then Left(FirstOrderBindRejection.EffectWitnessMismatch)
    else if !(design.partitions.axis.evidence eq source.partitions.axis.evidence) then
      Left(FirstOrderBindRejection.PartitionWitnessMismatch)
    else
      val support = Vector.newBuilder[Int]
      var effect = 0
      while effect < query.coefficients.cols do
        var used = false
        var output = 0
        while output < query.coefficients.rows && !used do
          used = query.coefficients(output, effect) != 0.0
          output += 1
        if used then support += effect
        effect += 1
      val usedEffects = support.result()

      var queryPosition = 0
      while queryPosition < query.coefficients.rows do
        var hasCoefficient = false
        var effectPosition = 0
        while effectPosition < query.coefficients.cols && !hasCoefficient do
          hasCoefficient = query.coefficients(queryPosition, effectPosition) != 0.0
          effectPosition += 1
        if !hasCoefficient then
          return Left(
            FirstOrderBindRejection.EmptyQuerySupport(
              query.output.identity.orderedKeys(queryPosition).value
            )
          )
        queryPosition += 1

      var rejection: Option[FirstOrderBindRejection] = None
      source.foreachRelation: (partition, relation) =>
        if rejection.isEmpty then
          var supportPosition = 0
          while supportPosition < usedEffects.length && rejection.isEmpty do
            val position = usedEffects(supportPosition)
            if !relation.receipt.estimability.flags.values(position) then
              rejection = Some(
                FirstOrderBindRejection.NonEstimableEffect(
                  partition,
                  source.effects.identity.orderedKeys(position).value
                )
              )
            supportPosition += 1
      rejection match
        case Some(value) => Left(value)
        case None        =>
          Right(new FirstOrderPrepared(query.coefficients.t))

  private def evaluate[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      Q <: SemanticSpace,
      EK,
      NK,
      QK,
      LK,
      C <: RelationCapabilities[N, NK]
  ](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      design: PartitionReductionDesign[P],
      estimand: ContrastEffectEstimand[P, E, N, Q, EK, NK, QK, C],
      prepared: FirstOrderPrepared,
      measurement: Measurement[N, NK, LK]
  ): Either[FirstOrderTaskFailure, MeasuredEffectMap[Q, QK]] =
    val queries = estimand.query.output
    val cells = Array.fill(queries.size * measurement.local.size)(0.0)
    val receipts = Vector.newBuilder[PartitionEffectReceipt]
    var partitionPosition = 0
    while partitionPosition < source.partitionKeys.length do
      val partition = source.partitionKeys(partitionPosition)
      val relation = source.relation(partition) match
        case Left(error)  => return Left(FirstOrderTaskFailure.Relation(error))
        case Right(value) => value
      val measured = relation.estimate.measureColumns(measurement) match
        case Left(error)  => return Left(FirstOrderTaskFailure.Evidence(error))
        case Right(value) => value
      val localByQuery = measured.transposeMultiply(prepared.queryTranspose) match
        case Left(error)  => return Left(FirstOrderTaskFailure.Evidence(error))
        case Right(value) => value
      val coefficient = design.coefficient(partitionPosition)
      var queryPosition = 0
      while queryPosition < queries.size do
        var localPosition = 0
        while localPosition < measurement.local.size do
          val offset = queryPosition * measurement.local.size + localPosition
          cells(offset) += coefficient * localByQuery(localPosition, queryPosition)
          localPosition += 1
        queryPosition += 1
      receipts += PartitionEffectReceipt(
        partition,
        relation.receipt.identity,
        coefficient
      )
      partitionPosition += 1

    var position = 0
    while position < cells.length do
      if !cells(position).isFinite then
        return Left(
          FirstOrderTaskFailure.Result(
            FirstOrderResultError.NonFiniteOutput(position, cells(position))
          )
        )
      position += 1

    val builder = Matrix.newBuilder(queries.size, measurement.local.size)
    var row = 0
    while row < queries.size do
      var column = 0
      while column < measurement.local.size do
        builder(row, column) = cells(row * measurement.local.size + column)
        column += 1
      row += 1
    val values = builder.result()
    Right(
      new MeasuredEffectMap(
        queries,
        measurement.local.identity,
        values,
        EffectMapUnits(
          source.neuralAxis.identity.units,
          EffectCoefficientUnits.DimensionlessLinearCombination
        ),
        new FirstOrderComputationReceipt(receipts.result(), cells.length.toLong)
      )
    )

  private def validateStrategy(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
    val supported = Vector(
      ExecutionRepresentation.Operator,
      ExecutionRepresentation.SufficientStatistics
    )
    if !supported.contains(strategy.representation) then
      Left(ExecutionPlanError.UnsupportedRepresentation(strategy.representation, supported))
    else if strategy.precision != NumericPrecision.Binary64 then
      Left(
        ExecutionPlanError.UnsupportedPrecision(
          strategy.precision,
          Vector(NumericPrecision.Binary64)
        )
      )
    else if strategy.solver != SolverChoice.NotApplicable then
      Left(ExecutionPlanError.UnsupportedSolver(strategy.solver))
    else Right(())
