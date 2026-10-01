package scalafim.fmri.mvpa.relation

import multivar.core.SemanticSpace
import resample4s.core.Injection
import scalafim.fmri.mvpa.{AxisDescriptor, AxisRef, EvidenceError, EvidenceOrigins, PreparationSupport, ValueSupport, ReindexingLeg}

/** A one-element, injectively selected coordinate in an identified partition
  * axis. It is deliberately not a validation fold.
  */
final class PartitionCoordinate[P <: SemanticSpace, K] private[relation] (
    val selection: ReindexingLeg[P, K, Injection]
):
  def ordinal: Int = selection.responseSelection.values.head

object PartitionCoordinate:
  def apply[P <: SemanticSpace, K](
      selection: ReindexingLeg[P, K, Injection]
  ): Either[EvidenceError, PartitionCoordinate[P, K]] =
    if selection.size != 1 then
      Left(EvidenceError.InvalidAxis("partition coordinate", "must select exactly one partition"))
    else Right(new PartitionCoordinate(selection))

/** An ordered edge. `reverse` is a different edge: callers must elect to add
  * it and cannot obtain it by silently symmetrizing the endpoints.
  */
final class PairingEdge[P <: SemanticSpace, K] private[relation] (
    val left: PartitionCoordinate[P, K],
    val right: PartitionCoordinate[P, K],
    val weight: Double,
    val generalizesOver: AxisDescriptor
):
  def reverse: PairingEdge[P, K] =
    new PairingEdge(right, left, weight, generalizesOver)

object PairingEdge:
  def apply[P <: SemanticSpace, K](
      left: PartitionCoordinate[P, K],
      right: PartitionCoordinate[P, K],
      weight: Double,
      generalizesOver: AxisDescriptor
  ): Either[EvidenceError, PairingEdge[P, K]] =
    if left.selection.parentAxis != right.selection.parentAxis then
      Left(EvidenceError.AxisMismatch("pairing endpoints", left.selection.parentAxis.stableKey, right.selection.parentAxis.stableKey))
    else if left.ordinal == right.ordinal then
      Left(EvidenceError.InvalidAxis("pairing edge", "left and right endpoints must be distinct"))
    else if !java.lang.Double.isFinite(weight) then
      Left(EvidenceError.InvalidAxis("pairing weight", "must be finite"))
    else Right(new PairingEdge(left, right, weight, generalizesOver))

enum EdgeReducer:
  case WeightedSum
  case WeightedMean

/** Reasons an output remains a descriptive signed statistic. These are
  * scientific refusals, distinct from whether a numerical contraction can run.
  */
enum ScientificRefusal:
  case MissingDeclaredErrorIndependence
  case SharedAcquisition(left: RelationOrigins, right: RelationOrigins)
  case SharedPreparation(left: RelationOrigins, right: RelationOrigins)
  case EndpointLearnedMetricWithoutConditionalArgument
  case MetricProvenanceMatchesEndpoint
  case SharedPartitionReplicates
  case ForeignConditionalEvidence
  case UnknownOriginSupport
  case MissingMetricReasoning

/** An explicitly supplied conditional-error argument. This binds the declared
  * reasoning and its evidence to the exact ordered endpoint origins; it does
  * not authenticate the scientific assumption.
  */
final class ConditionalErrorIndependence private[relation] (
    val left: RelationOrigins,
    val right: RelationOrigins,
    val leftEvidence: EvidenceOrigins,
    val rightEvidence: EvidenceOrigins,
    val conditionedMetric: Option[EvidenceOrigins],
    val reasoning: String
)

object ConditionalErrorIndependence:
  def apply(
      left: RelationOrigins,
      right: RelationOrigins,
      leftEvidence: EvidenceOrigins,
      rightEvidence: EvidenceOrigins,
      reasoning: String,
      conditionedMetric: Option[EvidenceOrigins] = None
  ): Either[ScientificRefusal, ConditionalErrorIndependence] =
    if leftEvidence != left.support || rightEvidence != right.support then
      Left(ScientificRefusal.ForeignConditionalEvidence)
    else if leftEvidence == EvidenceOrigins.Unknown || rightEvidence == EvidenceOrigins.Unknown || reasoning.trim.isEmpty then
      Left(ScientificRefusal.MissingDeclaredErrorIndependence)
    else Right(new ConditionalErrorIndependence(left, right, leftEvidence, rightEvidence, conditionedMetric, reasoning))

/** Metric provenance is kept as evidence rather than a string label. An
  * independently sourced metric is still a declared provenance admission, not
  * a proof of stochastic independence.
  */
enum MetricAdmission:
  case Fixed(metricOrigins: EvidenceOrigins)
  case IndependentlySourced(metricOrigins: EvidenceOrigins, conditional: ConditionalErrorIndependence)
  case EndpointLearned(metricOrigins: EvidenceOrigins, conditional: ConditionalErrorIndependence)

enum PairingClaim:
  case Descriptive(reasons: Vector[ScientificRefusal])
  case DeclaredUnbiased(
      errorIndependence: Vector[ConditionalErrorIndependence],
      metric: MetricAdmission,
      replicateReasons: Vector[ScientificRefusal]
  )

final case class PairingAssessment(
    claim: PairingClaim,
    reducer: EdgeReducer,
    totalWeight: Double
)

/** An identified directed graph over partitions. Its coordinates are bound
  * resample4s injections, while its endpoint evidence is supplied separately
  * at assessment time so a numerical Relation table is never forced eagerly.
  */
final class PairingDesign[P <: SemanticSpace, K] private[relation] (
    val partitions: AxisDescriptor,
    val edges: Vector[PairingEdge[P, K]],
    val reducer: EdgeReducer
):
  def assess[E <: SemanticSpace, N <: SemanticSpace](
      relations: RelationSet[P, E, N],
      metric: MetricAdmission,
      declaredErrorIndependence: Vector[ConditionalErrorIndependence]
  ): Either[EvidenceError, PairingAssessment] =
    if relations.partitionAxis != partitions then
      Left(EvidenceError.AxisMismatch("pairing relation partitions", partitions.stableKey, relations.partitionAxis.stableKey))
    else assessOrigins(relations.relations.map(_.origins), metric, declaredErrorIndependence)

  private[relation] def assessOrigins(
      origins: Vector[RelationOrigins],
      metric: MetricAdmission,
      declaredErrorIndependence: Vector[ConditionalErrorIndependence]
  ): Either[EvidenceError, PairingAssessment] =
    if origins.length != partitions.size then
      Left(EvidenceError.ShapeMismatch("pairing relation origins", partitions.size, origins.length))
    else
      val reasons = Vector.newBuilder[ScientificRefusal]
      var edgeIndex = 0
      while edgeIndex < edges.length do
        val edge = edges(edgeIndex)
        val left = origins(edge.left.ordinal)
        val right = origins(edge.right.ordinal)
        if left.source.acquisitionRevision == right.source.acquisitionRevision || sharesDirect(left.support, right.support) then
          reasons += ScientificRefusal.SharedAcquisition(left, right)
        if left.source.preparationRevision == right.source.preparationRevision || sharesPreparation(left.support, right.support) then
          reasons += ScientificRefusal.SharedPreparation(left, right)
        if !knownSupport(left.support) || !knownSupport(right.support) then reasons += ScientificRefusal.UnknownOriginSupport
        if !declaredErrorIndependence.exists(argument => argument.left == left && argument.right == right) then
          reasons += ScientificRefusal.MissingDeclaredErrorIndependence
        metric match
          case MetricAdmission.EndpointLearned(metricOrigins, argument) =>
            if argument.left != left || argument.right != right || !argument.conditionedMetric.contains(metricOrigins) || metricOrigins == EvidenceOrigins.Unknown then
              reasons += ScientificRefusal.EndpointLearnedMetricWithoutConditionalArgument
          case MetricAdmission.IndependentlySourced(metricOrigins, argument) =>
            if argument.left != left || argument.right != right || !argument.conditionedMetric.contains(metricOrigins) || metricOrigins == EvidenceOrigins.Unknown then
              reasons += ScientificRefusal.MissingMetricReasoning
            if metricOrigins == left.support || metricOrigins == right.support then
              reasons += ScientificRefusal.MetricProvenanceMatchesEndpoint
          case MetricAdmission.Fixed(_) => ()
        edgeIndex += 1
      val repeated = hasSharedPartition
      if repeated then reasons += ScientificRefusal.SharedPartitionReplicates
      val allReasons = reasons.result().distinct
      val claim =
        if allReasons.forall(_ == ScientificRefusal.SharedPartitionReplicates) then
          PairingClaim.DeclaredUnbiased(declaredErrorIndependence, metric, allReasons)
        else PairingClaim.Descriptive(allReasons)
      Right(PairingAssessment(claim, reducer, edges.map(_.weight).sum))

  /** Evaluates exactly one signed contribution for each ordered edge. The
    * graph does not turn these contributions into independent replicates.
    */
  def reduce(contributions: Vector[(PairingEdge[P, K], Double)]): Either[EvidenceError, PairingReduction] =
    def key(edge: PairingEdge[P, K]): (Int, Int) = (edge.left.ordinal, edge.right.ordinal)
    val expected = edges.map(key)
    val found = contributions.map((edge, _) => key(edge))
    if contributions.exists((edge, value) => edge.left.selection.parentAxis != partitions || !value.isFinite) then
      Left(EvidenceError.InvalidSource("pairing contributions must be finite and bound to the partition axis"))
    else if found.distinct.length != found.length || found.toSet != expected.toSet then
      Left(EvidenceError.InvalidSource("pairing reduction requires exactly one contribution per ordered edge"))
    else
      val byEdge = contributions.map((edge, value) => key(edge) -> value).toMap
      val numerator = edges.foldLeft(0.0)((total, edge) => total + edge.weight * byEdge(key(edge)))
      val denominator = edges.map(_.weight).sum
      val value = reducer match
        case EdgeReducer.WeightedSum => numerator
        case EdgeReducer.WeightedMean => numerator / denominator
      if !numerator.isFinite || !value.isFinite then Left(EvidenceError.InvalidSource("pairing reduction overflowed"))
      else Right(PairingReduction(value, numerator, denominator, expected))

  private def knownSupport(origins: EvidenceOrigins): Boolean = origins match
    case EvidenceOrigins.Bounded(value) =>
      value.direct != ValueSupport.Unknown && (value.preparation match
        case PreparationSupport.FixedShared(inputs) => inputs != ValueSupport.Unknown
        case PreparationSupport.JointlyLearned(inputs, targets) => inputs != ValueSupport.Unknown && targets != ValueSupport.Unknown
        case PreparationSupport.Unknown => false)
    case EvidenceOrigins.Unknown => false

  private def overlaps(left: ValueSupport, right: ValueSupport): Boolean = (left, right) match
    case (ValueSupport.Bounded(a, _, x), ValueSupport.Bounded(b, _, y)) => a == b && x.exists(y.toSet.contains)
    case _ => false

  private def sharesDirect(left: EvidenceOrigins, right: EvidenceOrigins): Boolean = (left, right) match
    case (EvidenceOrigins.Bounded(a), EvidenceOrigins.Bounded(b)) => overlaps(a.direct, b.direct)
    case _ => false

  private def preparationInputs(origins: EvidenceOrigins): Vector[ValueSupport] = origins match
    case EvidenceOrigins.Bounded(value) => value.preparation match
      case PreparationSupport.FixedShared(inputs) => Vector(inputs)
      case PreparationSupport.JointlyLearned(inputs, targets) => Vector(inputs, targets)
      case PreparationSupport.Unknown => Vector.empty
    case _ => Vector.empty

  private def sharesPreparation(left: EvidenceOrigins, right: EvidenceOrigins): Boolean =
    val a = preparationInputs(left)
    val b = preparationInputs(right)
    a.exists(x => b.exists(y => overlaps(x, y)))

  private def hasSharedPartition: Boolean =
    val seen = scala.collection.mutable.HashSet.empty[Int]
    var index = 0
    while index < edges.length do
      val edge = edges(index)
      if seen.contains(edge.left.ordinal) || seen.contains(edge.right.ordinal) then return true
      seen += edge.left.ordinal
      seen += edge.right.ordinal
      index += 1
    false

final case class PairingReduction(value: Double, numerator: Double, totalWeight: Double, orderedEdges: Vector[(Int, Int)])

object PairingDesign:
  def apply[PK, K](
      partitions: AxisRef[PK],
      edges: Vector[PairingEdge[partitions.Id, K]],
      reducer: EdgeReducer
  ): Either[EvidenceError, PairingDesign[partitions.Id, K]] =
    if edges.isEmpty then Left(EvidenceError.InvalidAxis("pairing edges", "must contain at least one edge"))
    else if edges.exists(_.left.selection.parentAxis != partitions.descriptor) then
      Left(EvidenceError.AxisMismatch("pairing partitions", partitions.descriptor.stableKey, edges.head.left.selection.parentAxis.stableKey))
    else
      val ordered = edges.map(edge => (edge.left.ordinal, edge.right.ordinal))
      if ordered.distinct.length != ordered.length then
        Left(EvidenceError.InvalidAxis("pairing edges", "duplicate ordered endpoints"))
      else if !edges.map(_.weight).sum.isFinite then
        Left(EvidenceError.InvalidAxis("pairing weights", "total weight must be finite"))
      else if reducer == EdgeReducer.WeightedMean && edges.map(_.weight).sum == 0.0 then
        Left(EvidenceError.InvalidAxis("pairing weights", "weighted mean requires a non-zero total weight"))
      else Right(new PairingDesign(partitions.descriptor, edges, reducer))
