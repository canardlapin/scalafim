package scalafim.fmri.mvpa

import multivar.core.SemanticSpace

enum RelationalFitError:
  case Identity(error: ScientificIdentityError)
  case Query(error: RelationalQueryError)
  case Compiler(error: RelationalCompilationError)
  case Distance(error: DistanceEstimandError)

  def message: String =
    this match
      case Identity(error) => error.message
      case Query(error)    => error.message
      case Compiler(error) => error.message
      case Distance(error) => error.message

sealed trait CompatibleSecondOrderFit[
    P <: SemanticSpace,
    E <: SemanticSpace,
    Local <: SemanticSpace,
    EK,
    LK
]:
  def sourceIdentity: ScientificSourceIdentity
  def definition: RdmDefinition[P, P, E, Local, EK, LK]
  private[mvpa] def effectForm: PairedEffectForm[E, EK]
  def units: DistanceUnits
  def identity: ScientificComponentFingerprint

  final def computation: RelationalComputationReceipt = effectForm.receipt

  final def rdm: Either[
    RelationalFitError,
    IdentifiedRdm[P, P, E, Local, EK, LK]
  ] =
    val values = Vector.newBuilder[Double]
    var pair = 0
    while pair < definition.domain.size do
      val coordinate = definition.domain.pairs(pair)
      val raw = effectForm.value(coordinate.firstPosition, coordinate.firstPosition) +
        effectForm.value(coordinate.secondPosition, coordinate.secondPosition) -
        effectForm.value(coordinate.firstPosition, coordinate.secondPosition) -
        effectForm.value(coordinate.secondPosition, coordinate.firstPosition)
      values += raw
      pair += 1
    IdentifiedRdm(
      definition,
      DistanceEstimands.normalize(
        values.result(),
        definition.metric.neural.size,
        definition.normalization
      )
    ).left.map(RelationalFitError.Query.apply)

/** A typed request for the one public relational-fit compiler. The request owns its exact source and effect-pair
  * domain; its `Output` member preserves the scientific distinction between identity-precision and crossnobis fits.
  */
sealed trait RelationalFitQuery[
    P <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace,
    EK,
    NK,
    C <: RelationCapabilities[N, NK]
]:
  type Output[Local <: SemanticSpace, LocalKey] <: CompatibleSecondOrderFit[
    P,
    E,
    Local,
    EK,
    LocalKey
  ]

  def source: PartitionedRelations[P, E, N, EK, NK, C]
  def domain: WithinPairDomain[E, EK]
  def normalization: RdmNormalization

  private[mvpa] def compile[LK](
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK],
      materialization: MaterializationPolicy
  ): Either[RelationalFitError, Output[measurement.local.Id, LK]]

final class IdentityPrecisionFitQuery[
    P <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace,
    EK,
    NK,
    C <: RelationCapabilities[N, NK]
] private[mvpa] (
    val source: PartitionedRelations[P, E, N, EK, NK, C],
    val domain: WithinPairDomain[E, EK],
    val normalization: RdmNormalization
) extends RelationalFitQuery[P, E, N, EK, NK, C]:
  type Output[Local <: SemanticSpace, LocalKey] =
    IdentityPrecisionRelationFit[P, E, Local, EK, LocalKey]

  private[mvpa] override def compile[LK](
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK],
      materialization: MaterializationPolicy
  ): Either[
    RelationalFitError,
    IdentityPrecisionRelationFit[P, E, measurement.local.Id, EK, LK]
  ] =
    RelationalFit.compileIdentity(this, design, measurement, materialization)

final class CrossnobisFitQuery[
    P <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace,
    EK,
    NK,
    C <: HasNoisePrecision[N, NK]
] private[mvpa] (
    val source: PartitionedRelations[P, E, N, EK, NK, C],
    val domain: WithinPairDomain[E, EK],
    val normalization: RdmNormalization
) extends RelationalFitQuery[P, E, N, EK, NK, C]:
  type Output[Local <: SemanticSpace, LocalKey] =
    CrossnobisRelationFit[P, E, Local, EK, LocalKey]

  private[mvpa] override def compile[LK](
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK],
      materialization: MaterializationPolicy
  ): Either[
    RelationalFitError,
    CrossnobisRelationFit[P, E, measurement.local.Id, EK, LK]
  ] =
    RelationalFit.compileCrossnobis(this, design, measurement, materialization)

object RelationalFitQuery:
  def identityPrecision[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: RelationCapabilities[N, NK]
  ](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      domain: WithinPairDomain[E, EK],
      normalization: RdmNormalization
  ): Either[
    RelationalFitError,
    IdentityPrecisionFitQuery[P, E, N, EK, NK, C]
  ] =
    RelationalFit
      .validateDomain(source.effects, domain)
      .map(_ => new IdentityPrecisionFitQuery(source, domain, normalization))

  def crossnobis[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: HasNoisePrecision[N, NK]
  ](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      domain: WithinPairDomain[E, EK],
      normalization: RdmNormalization
  ): Either[
    RelationalFitError,
    CrossnobisFitQuery[P, E, N, EK, NK, C]
  ] =
    RelationalFit
      .validateDomain(source.effects, domain)
      .map(_ => new CrossnobisFitQuery(source, domain, normalization))

final class IdentityPrecisionRelationFit[
    P <: SemanticSpace,
    E <: SemanticSpace,
    Local <: SemanticSpace,
    EK,
    LK
] private[mvpa] (
    val sourceIdentity: ScientificSourceIdentity,
    val definition: RdmDefinition[P, P, E, Local, EK, LK],
    private[mvpa] val effectForm: PairedEffectForm[E, EK],
    val units: DistanceUnits,
    val identity: ScientificComponentFingerprint
) extends CompatibleSecondOrderFit[P, E, Local, EK, LK]:
  def distance: Either[
    RelationalFitError,
    IdentityPrecisionSquaredDistance[P, E, Local, EK, LK]
  ] =
    rdm.map: value =>
      new IdentityPrecisionSquaredDistance(
        value,
        units,
        effectForm.receipt
      )

final class CrossnobisRelationFit[
    P <: SemanticSpace,
    E <: SemanticSpace,
    Local <: SemanticSpace,
    EK,
    LK
] private[mvpa] (
    val sourceIdentity: ScientificSourceIdentity,
    val definition: RdmDefinition[P, P, E, Local, EK, LK],
    private[mvpa] val effectForm: PairedEffectForm[E, EK],
    val units: DistanceUnits,
    val precision: CrossnobisPrecisionReceipt,
    val identity: ScientificComponentFingerprint
) extends CompatibleSecondOrderFit[P, E, Local, EK, LK]:
  def distance: Either[
    RelationalFitError,
    CrossnobisDistance[P, E, Local, EK, LK]
  ] =
    rdm.map: value =>
      new CrossnobisDistance(
        value,
        units,
        precision,
        effectForm.receipt
      )

object RelationalFit:
  private val IdentityKind = EstimandKind.unsafe("identity-precision-relation-fit")
  private val CrossnobisKind = EstimandKind.unsafe("crossnobis-relation-fit")

  def query[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      LK,
      C <: RelationCapabilities[N, NK]
  ](
      request: RelationalFitQuery[P, E, N, EK, NK, C],
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK],
      materialization: MaterializationPolicy = MaterializationPolicy.Reject
  ): Either[
    RelationalFitError,
    request.Output[measurement.local.Id, LK]
  ] =
    request.compile(design, measurement, materialization)

  private[mvpa] def compileIdentity[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      LK,
      C <: RelationCapabilities[N, NK]
  ](
      request: IdentityPrecisionFitQuery[P, E, N, EK, NK, C],
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK],
      materialization: MaterializationPolicy
  ): Either[
    RelationalFitError,
    IdentityPrecisionRelationFit[P, E, measurement.local.Id, EK, LK]
  ] =
    for
      _ <- DistanceEstimands
        .validateIndependent(request.source, design)
        .left
        .map(RelationalFitError.Distance.apply)
      metric <- NeuralQuery
        .identity(measurement.local)
        .left
        .map(RelationalFitError.Query.apply)
      definition <- RdmDefinition(
        request.domain,
        metric,
        design,
        normalization = request.normalization
      ).left.map(RelationalFitError.Query.apply)
      form <- RelationalCompiler
        .effectForm(request.source, design, measurement, metric, materialization)
        .left
        .map(RelationalFitError.Compiler.apply)
      fitIdentity <- identity(
        IdentityKind,
        request.source.identity,
        definition,
        Vector.empty
      )
    yield new IdentityPrecisionRelationFit(
      request.source.identity,
      definition,
      form,
      DistanceUnits(
        request.source.neuralAxis.identity.units,
        whitened = false,
        request.normalization
      ),
      fitIdentity
    )

  private[mvpa] def compileCrossnobis[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      LK,
      C <: HasNoisePrecision[N, NK]
  ](
      request: CrossnobisFitQuery[P, E, N, EK, NK, C],
      design: PairingDesign[P, P],
      measurement: Measurement[N, NK, LK],
      materialization: MaterializationPolicy
  ): Either[
    RelationalFitError,
    CrossnobisRelationFit[P, E, measurement.local.Id, EK, LK]
  ] =
    for
      _ <- DistanceEstimands
        .validateIndependent(request.source, design)
        .left
        .map(RelationalFitError.Distance.apply)
      certified <- DistanceEstimands
        .pooledRelationPrecision(request.source, design, measurement)
        .left
        .map(RelationalFitError.Distance.apply)
      definition <- RdmDefinition(
        request.domain,
        certified.query,
        design,
        normalization = request.normalization
      ).left.map(RelationalFitError.Query.apply)
      form <- RelationalCompiler
        .effectForm(request.source, design, measurement, certified.query, materialization)
        .left
        .map(RelationalFitError.Compiler.apply)
      fitIdentity <- identity(
        CrossnobisKind,
        request.source.identity,
        definition,
        Vector(
          "pooling" -> "equal-partition-mean",
          "precision-certificate" -> certified.certificate.identity.value
        )
      )
    yield new CrossnobisRelationFit(
      request.source.identity,
      definition,
      form,
      DistanceUnits(
        request.source.neuralAxis.identity.units,
        whitened = true,
        request.normalization
      ),
      certified.receipt,
      fitIdentity
    )

  private[mvpa] def validateDomain[E <: SemanticSpace, EK](
      expected: AxisRef.Aux[EK, E],
      domain: WithinPairDomain[E, EK]
  ): Either[RelationalFitError, Unit] =
    if domain.items.identity != expected.identity then
      Left(
        RelationalFitError.Query(
          RelationalQueryError.PairQuerySourceMismatch(
            expected.identity.fingerprint,
            domain.items.identity.fingerprint
          )
        )
      )
    else if !(domain.items.evidence eq expected.evidence) then
      Left(
        RelationalFitError.Query(
          RelationalQueryError.PairQueryWitnessMismatch("effect-domain")
        )
      )
    else Right(())

  private def identity[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK
  ](
      kind: EstimandKind,
      source: ScientificSourceIdentity,
      definition: RdmDefinition[P, P, E, N, EK, NK],
      fields: Vector[(String, String)]
  ): Either[RelationalFitError, ScientificComponentFingerprint] =
    EstimandIdentity(
      kind,
      Vector(
        "definition" -> definition.identity.fingerprint.value,
        "source" -> source.fingerprint.value
      ) ++ fields
    ).left
      .map(RelationalFitError.Identity.apply)
      .map(_.fingerprint)
