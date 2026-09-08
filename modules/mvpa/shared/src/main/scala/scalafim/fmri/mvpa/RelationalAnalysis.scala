package scalafim.fmri.mvpa

import multivar.core.SemanticSpace

enum RelationalBindRejection:
  case NonEstimableEffect(partition: PartitionId, effect: String)
  case Model(error: RelationalRsaError)

  def message: String =
    this match
      case NonEstimableEffect(partition, effect) =>
        s"partition '${partition.value}' does not provide an estimable '$effect' relation"
      case Model(error) => error.message

enum RelationalTaskFailure:
  case Fit(error: RelationalFitError)
  case Rsa(error: RelationalRsaError)
  case ExecutionEvidence(error: ExecutionReceiptError)

  def message: String =
    this match
      case Fit(error)               => error.message
      case Rsa(error)               => error.message
      case ExecutionEvidence(error) => error.message

/** Neural closure is complete, so the public RDM result needs only its exact effect-pair axis, scientific definition,
  * measurement identity, and work receipt. No existential local-neural type leaks through the result API.
  */
final class MeasuredRelationalRdm[
    P <: SemanticSpace,
    E <: SemanticSpace,
    EK
] private[mvpa] (
    val measurement: MeasurementIdentity,
    val domain: WithinPairDomain[E, EK],
    val distances: Column[domain.pairAxis.Id, Double],
    val definition: EstimandIdentity,
    val fit: ScientificComponentFingerprint,
    val units: DistanceUnits,
    val computation: RelationalComputationReceipt
):
  def distance(first: EK, second: EK): Either[RelationalQueryError, Double] =
    domain
      .position(first, second)
      .flatMap: position =>
        distances.at(position).left.map(RelationalQueryError.Column.apply)

final class MeasuredRankRsa private[mvpa] (
    val measurement: MeasurementIdentity,
    val estimate: RankRsaEstimate,
    val computation: RelationalComputationReceipt
)

final class RelationalPrepared private[mvpa] (
    val source: ScientificComponentFingerprint,
    val design: ScientificComponentFingerprint
)

sealed trait RelationalAnalysisQuery[
    P <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace,
    EK,
    NK,
    C <: RelationCapabilities[N, NK]
]:
  type Result

  def normalization: RdmNormalization

  private[mvpa] def boundaries: RequestedBoundaries

  private[mvpa] def validate(
      source: PartitionedRelations[P, E, N, EK, NK, C]
  ): Either[RelationalBindRejection, Unit]

  private[mvpa] def execute[LK](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK],
      materialization: MaterializationPolicy
  ): Either[RelationalTaskFailure, (Result, RelationalComputationReceipt)]

final class IdentityPrecisionRdmQuery[
    P <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace,
    EK,
    NK,
    C <: RelationCapabilities[N, NK]
] private[mvpa] (
    val normalization: RdmNormalization
) extends RelationalAnalysisQuery[P, E, N, EK, NK, C]:
  override type Result = MeasuredRelationalRdm[P, E, EK]

  private[mvpa] override val boundaries: RequestedBoundaries =
    RelationalAnalysis.RdmBoundaries

  private[mvpa] override def validate(
      source: PartitionedRelations[P, E, N, EK, NK, C]
  ): Either[RelationalBindRejection, Unit] = Right(())

  private[mvpa] override def execute[LK](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK],
      materialization: MaterializationPolicy
  ): Either[RelationalTaskFailure, (Result, RelationalComputationReceipt)] =
    for
      domain <- WithinPairDomain(source.effects).left
        .map(error => RelationalTaskFailure.Fit(RelationalFitError.Query(error)))
      request <- RelationalFitQuery
        .identityPrecision(source, domain, normalization)
        .left
        .map(RelationalTaskFailure.Fit.apply)
      fit <- RelationalFit
        .query(request, design, measurement, materialization)
        .left
        .map(RelationalTaskFailure.Fit.apply)
      rdm <- fit.rdm.left.map(RelationalTaskFailure.Fit.apply)
    yield (
      RelationalAnalysis.closeRdm(measurement.identity, fit, rdm),
      fit.computation
    )

final class CrossnobisRdmQuery[
    P <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace,
    EK,
    NK,
    C <: HasNoisePrecision[N, NK]
] private[mvpa] (
    val normalization: RdmNormalization
) extends RelationalAnalysisQuery[P, E, N, EK, NK, C]:
  override type Result = MeasuredRelationalRdm[P, E, EK]

  private[mvpa] override val boundaries: RequestedBoundaries =
    RelationalAnalysis.RdmBoundaries

  private[mvpa] override def validate(
      source: PartitionedRelations[P, E, N, EK, NK, C]
  ): Either[RelationalBindRejection, Unit] = Right(())

  private[mvpa] override def execute[LK](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK],
      materialization: MaterializationPolicy
  ): Either[RelationalTaskFailure, (Result, RelationalComputationReceipt)] =
    for
      domain <- WithinPairDomain(source.effects).left
        .map(error => RelationalTaskFailure.Fit(RelationalFitError.Query(error)))
      request <- RelationalFitQuery
        .crossnobis(source, domain, normalization)
        .left
        .map(RelationalTaskFailure.Fit.apply)
      fit <- RelationalFit
        .query(request, design, measurement, materialization)
        .left
        .map(RelationalTaskFailure.Fit.apply)
      rdm <- fit.rdm.left.map(RelationalTaskFailure.Fit.apply)
    yield (
      RelationalAnalysis.closeRdm(measurement.identity, fit, rdm),
      fit.computation
    )

final class IdentityPrecisionRankRsaQuery[
    P <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace,
    EK,
    NK,
    C <: RelationCapabilities[N, NK]
] private[mvpa] (
    val model: SecondOrderModel[SignalModelRole, E, EK],
    val normalization: RdmNormalization
) extends RelationalAnalysisQuery[P, E, N, EK, NK, C]:
  override type Result = MeasuredRankRsa

  private[mvpa] override val boundaries: RequestedBoundaries =
    RelationalAnalysis.RankBoundaries

  private[mvpa] override def validate(
      source: PartitionedRelations[P, E, N, EK, NK, C]
  ): Either[RelationalBindRejection, Unit] =
    RelationalAnalysis.validateModelDomain(source, model)

  private[mvpa] override def execute[LK](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK],
      materialization: MaterializationPolicy
  ): Either[RelationalTaskFailure, (Result, RelationalComputationReceipt)] =
    for
      request <- RelationalFitQuery
        .identityPrecision(source, model.domain, normalization)
        .left
        .map(RelationalTaskFailure.Fit.apply)
      fit <- RelationalFit
        .query(request, design, measurement, materialization)
        .left
        .map(RelationalTaskFailure.Fit.apply)
      estimate <- RelationalRsa
        .rank(fit, model)
        .left
        .map(RelationalTaskFailure.Rsa.apply)
    yield (
      new MeasuredRankRsa(measurement.identity, estimate, fit.computation),
      fit.computation
    )

/** The one public estimand route for relational analyses. Scientific variants are inspectable typed queries; binding
  * and execution compile every variant through the same capability instances.
  */
final class RelationalQueryEstimand[
    P <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace,
    EK,
    NK,
    C <: RelationCapabilities[N, NK],
    Q <: RelationalAnalysisQuery[P, E, N, EK, NK, C]
] private[mvpa] (
    val query: Q,
    val identity: EstimandIdentity
) extends Estimand[
      PartitionedRelations[P, E, N, EK, NK, C],
      PairingDesign[P, P]
    ]:
  override type Result = query.Result
  override type Rejection = RelationalBindRejection
  override type Failure = RelationalTaskFailure

  override def defaultBoundaries: RequestedBoundaries = query.boundaries

  override def rejectionMessage(value: RelationalBindRejection): String = value.message
  override def failureMessage(value: RelationalTaskFailure): String = value.message

object RelationalAnalysis:
  private val IdentityRdmKind = EstimandKind.unsafe("identity-precision-rdm-frame")
  private val CrossnobisRdmKind = EstimandKind.unsafe("crossnobis-rdm-frame")
  private val IdentityRankKind = EstimandKind.unsafe("identity-precision-rank-rsa-frame")

  private[mvpa] val RdmBoundaries = boundaries("effect-pair-distances")
  private[mvpa] val RankBoundaries = boundaries("rank-rsa-correlation")

  def identityRdm[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: RelationCapabilities[N, NK]](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      normalization: RdmNormalization
  ): RelationalQueryEstimand[
    P,
    E,
    N,
    EK,
    NK,
    C,
    IdentityPrecisionRdmQuery[P, E, N, EK, NK, C]
  ] =
    val identity = identified(
      IdentityRdmKind,
      Vector(
        "geometry" -> "identity-precision-crossvalidated-squared-distance",
        "normalization" -> normalization.label,
        "source" -> source.identity.fingerprint.value
      )
    )
    val query = new IdentityPrecisionRdmQuery[P, E, N, EK, NK, C](normalization)
    new RelationalQueryEstimand(query, identity)

  def crossnobisRdm[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: HasNoisePrecision[N, NK]](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      normalization: RdmNormalization
  ): RelationalQueryEstimand[
    P,
    E,
    N,
    EK,
    NK,
    C,
    CrossnobisRdmQuery[P, E, N, EK, NK, C]
  ] =
    val identity = identified(
      CrossnobisRdmKind,
      Vector(
        "geometry" -> "certified-noise-whitened-crossnobis",
        "normalization" -> normalization.label,
        "pooling" -> "equal-partition-mean",
        "source" -> source.identity.fingerprint.value
      )
    )
    val query = new CrossnobisRdmQuery[P, E, N, EK, NK, C](normalization)
    new RelationalQueryEstimand(query, identity)

  def rankRsa[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: RelationCapabilities[N, NK]](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      model: SecondOrderModel[SignalModelRole, E, EK],
      normalization: RdmNormalization
  ): RelationalQueryEstimand[
    P,
    E,
    N,
    EK,
    NK,
    C,
    IdentityPrecisionRankRsaQuery[P, E, N, EK, NK, C]
  ] =
    val identity = identified(
      IdentityRankKind,
      Vector(
        "comparison" -> "spearman-correlation-of-canonical-pair-distances",
        "model" -> model.identity.value,
        "normalization" -> normalization.label,
        "source" -> source.identity.fingerprint.value
      )
    )
    val query =
      new IdentityPrecisionRankRsaQuery[P, E, N, EK, NK, C](model, normalization)
    new RelationalQueryEstimand(query, identity)

  given relationalQueryCompiler[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: RelationCapabilities[N, NK],
      Q <: RelationalAnalysisQuery[P, E, N, EK, NK, C],
      R
  ]: Compile[
    PartitionedRelations[P, E, N, EK, NK, C],
    PairingDesign[P, P],
    RelationalQueryEstimand[P, E, N, EK, NK, C, Q],
    R
  ] with
    override type Prepared = RelationalPrepared

    override def prepare(
        specification: ScientificSpecification[
          PartitionedRelations[P, E, N, EK, NK, C],
          PairingDesign[P, P],
          RelationalQueryEstimand[P, E, N, EK, NK, C, Q],
          R
        ]
    ): Either[specification.Rejection, RelationalPrepared] =
      prepareRelations(specification.source, specification.design).flatMap: prepared =>
        specification.estimand.query
          .validate(specification.source)
          .map(_ => prepared)

  given relationalQueryTask[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: RelationCapabilities[N, NK],
      Q <: RelationalAnalysisQuery[P, E, N, EK, NK, C],
      R
  ]: MeasurementTask[
    PartitionedRelations[P, E, N, EK, NK, C],
    PairingDesign[P, P],
    RelationalQueryEstimand[P, E, N, EK, NK, C, Q],
    R,
    RelationalPrepared
  ] with
    override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
      validateStrategy(strategy)

    override def execute(
        plan: BoundScientificPlan[
          PartitionedRelations[P, E, N, EK, NK, C],
          PairingDesign[P, P],
          RelationalQueryEstimand[P, E, N, EK, NK, C, Q],
          R,
          RelationalPrepared
        ]
    )(
        entry: MeasurementEntry[N, NK, ?, R],
        context: TaskContext
    ): Either[TaskReportError, TaskReport[plan.Result, plan.Rejection, plan.Failure]] =
      plan.specification.estimand.query
        .execute(
          plan.specification.source,
          plan.specification.design,
          entry.measurement,
          context.strategy.materialization
        ) match
        case Left(failure) =>
          TaskReport.failed(failure, context.strategy.target)
        case Right((result, receipt)) =>
          successful(result, receipt, entry.measurement.identity.id, context)

  private def prepareRelations[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: RelationCapabilities[N, NK]
  ](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      design: PairingDesign[P, P]
  ): Either[RelationalBindRejection, RelationalPrepared] =
    var rejection: Option[RelationalBindRejection] = None
    source.foreachRelation: (partition, relation) =>
      if rejection.isEmpty then
        var effect = 0
        while effect < source.effects.size && rejection.isEmpty do
          if !relation.receipt.estimability.flags.values(effect) then
            rejection = Some(
              RelationalBindRejection.NonEstimableEffect(
                partition,
                summonAxisKey(source.effects, effect)
              )
            )
          effect += 1
    rejection match
      case Some(value) => Left(value)
      case None        =>
        Right(
          new RelationalPrepared(
            source.identity.fingerprint,
            design.identity.fingerprint
          )
        )

  private[mvpa] def validateModelDomain[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: RelationCapabilities[N, NK]
  ](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      model: SecondOrderModel[SignalModelRole, E, EK]
  ): Either[RelationalBindRejection, Unit] =
    if source.effects.identity != model.domain.items.identity then
      Left(
        RelationalBindRejection.Model(
          RelationalRsaError.ModelDomainMismatch(
            source.effects.identity.fingerprint,
            model.domain.items.identity.fingerprint
          )
        )
      )
    else if !(source.effects.evidence eq model.domain.items.evidence) then
      Left(RelationalBindRejection.Model(RelationalRsaError.ModelWitnessMismatch))
    else Right(())

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

  private[mvpa] def closeRdm[P <: SemanticSpace, E <: SemanticSpace, L <: SemanticSpace, EK, LK](
      measurement: MeasurementIdentity,
      fit: CompatibleSecondOrderFit[P, E, L, EK, LK],
      rdm: IdentifiedRdm[P, P, E, L, EK, LK]
  ): MeasuredRelationalRdm[P, E, EK] =
    new MeasuredRelationalRdm(
      measurement,
      rdm.definition.domain,
      rdm.distances,
      rdm.definition.identity,
      fit.identity,
      fit.units,
      fit.computation
    )

  private def successful[A](
      value: A,
      receipt: RelationalComputationReceipt,
      measurement: MeasurementId,
      context: TaskContext
  ): Either[TaskReportError, TaskReport[A, Nothing, RelationalTaskFailure]] =
    executionMaterializations(receipt, measurement) match
      case Left(error) =>
        TaskReport.failed(
          RelationalTaskFailure.ExecutionEvidence(error),
          context.strategy.target,
          operatorApplications = receipt.operatorApplications
        )
      case Right(materializations) =>
        TaskReport.success(
          value,
          context.strategy.target,
          operatorApplications = receipt.operatorApplications,
          materializations = materializations
        )

  private def executionMaterializations(
      receipt: RelationalComputationReceipt,
      measurement: MeasurementId
  ): Either[ExecutionReceiptError, Vector[ExecutionMaterialization]] =
    val output = Vector.newBuilder[ExecutionMaterialization]
    var position = 0
    while position < receipt.materializations.length do
      ExecutionMaterialization(
        ExecutionScope.Measurement(measurement),
        receipt.materializations(position),
        "relational sufficient statistic requested explicit local relation materialization"
      ) match
        case Left(error)  => return Left(error)
        case Right(value) => output += value
      position += 1
    Right(output.result())

  private def summonAxisKey[K](axis: AxisRef[K], position: Int): String =
    axis.identity.orderedKeys(position).value

  private def boundaries(id: String): RequestedBoundaries =
    val boundary = OutputBoundaryIdentity.trusted(OutputBoundaryId.unsafe(id))
    RequestedBoundaries.trusted(Vector(boundary))

  private def identified(
      kind: EstimandKind,
      fields: Vector[(String, String)]
  ): EstimandIdentity =
    EstimandIdentity.trusted(kind, fields)
