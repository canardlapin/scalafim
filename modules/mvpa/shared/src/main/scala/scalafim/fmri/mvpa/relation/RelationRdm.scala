package scalafim.fmri.mvpa.relation

import gale.linalg.{DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{CoordinateEvidence, FormOperator, Lin, MetricSpec, SemanticProvenance, SemanticSpace, ValueId, ValueIdentity}
import resample4s.core.{IndexSpace, Injection}
import scalafim.fmri.mvpa.{AxisRef, EvidenceError, ReindexingLeg}

/** Fixed baseline policy.  This is deliberately named for its actual metric:
  * identity, signed squared Euclidean cross-products, and optional division by
  * the neural feature count.  It does not claim estimated noise precision.
  */
final case class IdentityRdmPolicy(normalizeByFeatures: Boolean = true)

enum RelationRdmCell:
  case Estimated(value: Double)
  case NotEstimable(effectOrdinals: Vector[Int], reasons: Vector[String])

final case class RelationRdm(
    effectKeys: Vector[String],
    cells: Vector[RelationRdmCell],
    pairing: PairingAssessment,
    policy: IdentityRdmPolicy,
    endpointOrigins: Vector[RelationOrigins],
    residualProvenance: Option[ResidualMetricProvenance] = None
)

object RelationRdm:
  /** Every directed pair of distinct partitions is present exactly once. */
  def allDistinctOrdered[PK](partitions: AxisRef[PK]): Either[EvidenceError, PairingDesign[partitions.Id, PK]] =
    val index = IndexSpace.of(partitions.size).left.map(error => EvidenceError.InvalidAxis("partition index", error.toString))
    index.flatMap: space =>
      val edges = Vector.newBuilder[PairingEdge[partitions.Id, PK]]
      var left = 0
      var failure: Option[EvidenceError] = None
      while left < partitions.size && failure.isEmpty do
        var right = 0
        while right < partitions.size && failure.isEmpty do
          if left != right then
            val edge = for
              leftInjection <- Injection.from(IArray(left), space).left.map(error => EvidenceError.InvalidAxis("left partition", error.toString))
              rightInjection <- Injection.from(IArray(right), space).left.map(error => EvidenceError.InvalidAxis("right partition", error.toString))
              leftLeg <- ReindexingLeg.bind(partitions, leftInjection)
              leftCoordinate <- PartitionCoordinate(leftLeg)
              rightLeg <- ReindexingLeg.bind(partitions, rightInjection)
              rightCoordinate <- PartitionCoordinate(rightLeg)
              pairing <- PairingEdge(leftCoordinate, rightCoordinate, 1.0, partitions.descriptor)
            yield pairing
            edge match
              case Left(error) => failure = Some(error)
              case Right(value) => edges += value
          right += 1
        left += 1
      failure.toLeft(edges.result()).flatMap(value => PairingDesign(partitions, value, EdgeReducer.WeightedMean))

  def identity[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, K](
      relations: RelationSet[P, E, N],
      pairing: PairingDesign[P, K],
      declaredErrorIndependence: Vector[ConditionalErrorIndependence] = Vector.empty,
      policy: IdentityRdmPolicy = IdentityRdmPolicy()
  ): Either[EvidenceError, RelationRdm] =
    if !isAllDistinctUnitMean(pairing, relations.partitionAxis.size) then
      Left(EvidenceError.InvalidSource("fixed identity RDM requires exactly the all-distinct ordered unit-weight pairing and weighted mean reduction"))
    else
      val identity = MetricSpec.identity(relations.neuralAxis.size, Some(relations.neural.descriptor))
        .left.map(EvidenceError.MultivarFailure.apply)
        .flatMap(metric => FormOperator.primal(metric, relations.neural, ValueIdentity.source(ValueId.unsafe(s"relation-rdm-fixed-identity-${relations.neuralAxis.stableKey}")))
          .left.map(EvidenceError.SemanticFailure.apply))
      identity.flatMap(metric => compute(relations, pairing, metric, MetricAdmission.Fixed(scalafim.fmri.mvpa.EvidenceOrigins.Unknown), declaredErrorIndependence, policy))

  /** Separately admitted residual normalization. Descriptive metric evidence
    * remains descriptive even if endpoint-error assumptions are supplied.
    */
  def residual[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, K, NK](
      relations: RelationSet[P, E, N], pairing: PairingDesign[P, K],
      precision: ResidualPrecisionMetric[NK],
      declaredErrorIndependence: Vector[ConditionalErrorIndependence] = Vector.empty,
      policy: IdentityRdmPolicy = IdentityRdmPolicy()
  ): Either[EvidenceError, RelationRdm] =
    for
      metric <- precision.closureFor(relations.neural, relations.neuralAxis)
      admission <- precision.metricAdmission
      result <- compute(relations, pairing, metric, admission, declaredErrorIndependence, policy)
    yield
      val assessment = precision.admission match
        case ResidualMetricAdmission.Descriptive(_, _, _) =>
          result.pairing.copy(claim = PairingClaim.Descriptive(Vector(ScientificRefusal.MissingMetricReasoning)))
        case _ => result.pairing
      result.copy(pairing = assessment, residualProvenance = precision.provenance)

  /** The metric admission is supplied by the caller, so a residual precision
    * route cannot inherit the fixed-identity claim. */
  def compute[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, K](
      relations: RelationSet[P, E, N],
      pairing: PairingDesign[P, K],
      metric: CrossClosure[N, N],
      metricAdmission: MetricAdmission,
      declaredErrorIndependence: Vector[ConditionalErrorIndependence],
      policy: IdentityRdmPolicy
  ): Either[EvidenceError, RelationRdm] =
    if pairing.partitions != relations.partitionAxis then Left(EvidenceError.AxisMismatch("RDM partitions", relations.partitionAxis.stableKey, pairing.partitions.stableKey))
    else pairing.assess(relations, metricAdmission, declaredErrorIndependence).flatMap: assessment =>
      val effectKeys = relations.effectKeys
      val pairs = for
        right <- 0 until relations.effectAxis.size
        left <- right + 1 until relations.effectAxis.size
      yield (left, right)
      val cells = Vector.newBuilder[RelationRdmCell]
      var index = 0
      var failure: Option[EvidenceError] = None
      while index < pairs.length && failure.isEmpty do
        val (left, right) = pairs(index)
        val unavailable = pairing.edges.flatMap: edge =>
          Vector(left, right).flatMap: effect =>
            Vector(relations.relations(edge.left.ordinal).estimability(effect), relations.relations(edge.right.ordinal).estimability(effect)).collect:
              case EffectEstimability.NotEstimable(reason) => effect -> reason
        if unavailable.nonEmpty then
          cells += RelationRdmCell.NotEstimable(unavailable.map(_._1).distinct, unavailable.map(_._2).distinct)
        else
          val contributions = pairing.edges.map: edge =>
            val effectQuery = Lin.fromLinearMap(
              differenceForm(relations.effectAxis.size, left, right),
              CoordinateEvidence.primal(relations.effects), CoordinateEvidence.dual(relations.effects),
              ValueIdentity.source(ValueId.unsafe(s"relation-rdm-${relations.effectAxis.stableKey}-difference-$left-$right")), SemanticProvenance.source("relation-rdm"))
              .left.map(EvidenceError.SemanticFailure.apply)
            effectQuery.flatMap: query =>
              SecondOrderQuery(RelationPair(relations.relations(edge.left.ordinal), relations.relations(edge.right.ordinal)), Some(query), Some(metric)).scalar.map(_.value).flatMap: value =>
                if value.isFinite then Right(edge -> value) else Left(EvidenceError.InvalidSource("signed RDM contribution is non-finite"))
          contributions.foldLeft[Either[EvidenceError, Vector[(PairingEdge[P, K], Double)]]](Right(Vector.empty))((acc, next) => for values <- acc; value <- next yield values :+ value).flatMap(pairing.reduce) match
            case Left(error) => failure = Some(error)
            case Right(reduced) =>
              val value = if policy.normalizeByFeatures then reduced.value / relations.neuralAxis.size else reduced.value
              if value.isFinite then cells += RelationRdmCell.Estimated(value) else failure = Some(EvidenceError.InvalidSource("normalized signed RDM is non-finite"))
        index += 1
      failure.toLeft(RelationRdm(effectKeys, cells.result(), assessment, policy, relations.relations.map(_.origins)))

  private def differenceForm(size: Int, left: Int, right: Int): DoubleLinearOperator =
    new DoubleLinearOperator:
      val rows = size
      val cols = size
      def applyTo(input: DVec, output: MutableDVec): Unit =
        var row = 0
        while row < size do
          output(row) = 0.0
          row += 1
        val difference = input(left) - input(right)
        output(left) = difference
        output(right) = -difference
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = applyTo(input, output)

  private def isAllDistinctUnitMean[P <: SemanticSpace, K](pairing: PairingDesign[P, K], partitions: Int): Boolean =
    val expected = (for left <- 0 until partitions; right <- 0 until partitions if left != right yield left -> right).toSet
    pairing.reducer == EdgeReducer.WeightedMean && pairing.edges.length == expected.size &&
      pairing.edges.forall(edge => edge.weight == 1.0) && pairing.edges.map(edge => edge.left.ordinal -> edge.right.ordinal).toSet == expected
