package scalafim.fmri.mvpa.relation

import multivar.core.{SemanticSpace, SpaceEvidence, Table}
import resample4s.core.Reindexing
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, AxisRef, EvidenceError, EvidenceOrigins, ReindexingLeg}

final case class RelationSource(
    acquisitionRevision: String,
    responseRevision: String,
    readoutRevision: String,
    preparationRevision: String,
    noiseRevision: String
):
  require(acquisitionRevision.nonEmpty, "acquisition revision must be non-empty")
  require(responseRevision.nonEmpty, "response revision must be non-empty")
  require(readoutRevision.nonEmpty, "readout revision must be non-empty")
  require(preparationRevision.nonEmpty, "preparation revision must be non-empty")
  require(noiseRevision.nonEmpty, "noise revision must be non-empty")

enum RelationAccess:
  case OneShot
  case OwnedReplay(readerOwner: String)
  case ScopedReplay(witness: ScopeReplay)

object RelationAccess:
  def admitImmediate(value: RelationAccess): Either[EvidenceError, Unit] = value match
    case RelationAccess.OwnedReplay(_) => Right(())
    case RelationAccess.ScopedReplay(witness) if witness.isActive => Right(())
    case RelationAccess.ScopedReplay(_) => Left(EvidenceError.InvalidSource("scoped relation replay has expired"))
    case RelationAccess.OneShot => Left(EvidenceError.InvalidSource("repeated evaluation requires explicit replay permission"))

  def admitsRetained(value: RelationAccess): Boolean = value match
    case RelationAccess.OwnedReplay(_) => true
    case RelationAccess.OneShot | RelationAccess.ScopedReplay(_) => false

  private[mvpa] def writeFramed(writer: AxisDigest.Writer, value: RelationAccess): Unit = value match
    case RelationAccess.OneShot => writer.string("one-shot")
    case RelationAccess.OwnedReplay(owner) => writer.string("owned-replay"); writer.string(owner)
    case RelationAccess.ScopedReplay(witness) =>
      writer.string("scoped-replay")
      writer.string(witness.owner)
      writer.string(witness.revision)
      writer.string(witness.sourceBinding)

final case class RelationOrigins(
    source: RelationSource,
    access: RelationAccess,
    support: EvidenceOrigins = EvidenceOrigins.Unknown
):
  access match
    case RelationAccess.OneShot => ()
    case RelationAccess.OwnedReplay(owner) => require(owner.nonEmpty, "replay reader owner must be non-empty")
    case RelationAccess.ScopedReplay(_) => ()

enum EffectEstimability:
  case Estimable
  case NotEstimable(reason: String)

trait Estimability[-A]:
  def effectEstimability(value: A): Vector[EffectEstimability]

trait ResidualMoments[-A]:
  type Moment
  def residualMoments(value: A): Moment

trait NoisePrecision[-A]:
  type Precision
  def noisePrecision(value: A): Precision

trait ResidualDegreesOfFreedom[-A]:
  def residualDegreesOfFreedom(value: A): Double

final class PointRelationBinding[E <: SemanticSpace, N <: SemanticSpace, A] private[relation] (
    val relation: Relation[E, N],
    val source: RelationSource,
    val value: A,
    val estimability: Estimability[A]
)

/** Retains admitted providers without eagerly calculating residual statistics. */
final class ResidualRelationBinding[E <: SemanticSpace, N <: SemanticSpace, A] private[relation] (
    val point: PointRelationBinding[E, N, A],
    val moments: ResidualMoments[A],
    val precision: NoisePrecision[A],
    val degreesOfFreedom: ResidualDegreesOfFreedom[A]
)

final class Relation[E <: SemanticSpace, N <: SemanticSpace] private[relation] (
    val effects: SpaceEvidence[E],
    val neural: SpaceEvidence[N],
    val effectAxis: AxisDescriptor,
    val neuralAxis: AxisDescriptor,
    val estimate: Table[E, N],
    val origins: RelationOrigins,
    val estimability: Vector[EffectEstimability]
):
  require(estimability.length == effectAxis.size, "estimability must match effect axis")

  def restrict[K, R <: Reindexing](by: ReindexingLeg[E, K, R]): Relation[by.Child, N] =
    new Relation(by.child.evidence, neural, by.child.descriptor, neuralAxis, estimate.andThen(by.leg), origins.copy(
      support = origins.support.reindexOutput(by.child.descriptor)
    ),
      by.ordinals.toVector.map(estimability))

  def bind[A](source: RelationSource, value: A)(using evidence: Estimability[A]): Either[EvidenceError, PointRelationBinding[E, N, A]] =
    if source != origins.source then
      Left(EvidenceError.InvalidSource("relation binding source does not match the admitted relation source"))
    else
      val statuses = evidence.effectEstimability(value)
      if statuses.length != effectAxis.size then
        Left(EvidenceError.ShapeMismatch("bound relation estimability", effectAxis.size, statuses.length))
      else Right(new PointRelationBinding(this, source, value, evidence))

  def bindResidual[A](source: RelationSource, value: A)(using
      estimabilityEvidence: Estimability[A],
      momentsEvidence: ResidualMoments[A],
      precisionEvidence: NoisePrecision[A],
      dfEvidence: ResidualDegreesOfFreedom[A]
  ): Either[EvidenceError, ResidualRelationBinding[E, N, A]] =
    bind(source, value).map(point => new ResidualRelationBinding(point, momentsEvidence, precisionEvidence, dfEvidence))

object Relation:
  /** Exact axis witnesses prevent same-sized foreign descriptors entering. */
  def apply[EK, NK](
      effects: AxisRef[EK],
      neural: AxisRef[NK],
      estimate: Table[effects.Id, neural.Id],
      origins: RelationOrigins,
      estimability: Vector[EffectEstimability]
  ): Either[EvidenceError, Relation[effects.Id, neural.Id]] =
    origins.access match
      case RelationAccess.ScopedReplay(_) => Left(EvidenceError.InvalidSource("scoped replay attachment belongs to the acquiring provider"))
      case _ => fromAcquired(effects, neural, estimate, origins, estimability)

  /** Only an acquiring provider may attach its live scoped capability. Public
    * callers cannot transplant an observed witness onto another operator.
    * Restriction preserves the original guarded operator and lifetime.
    */
  private[mvpa] def fromAcquired[EK, NK](
      effects: AxisRef[EK], neural: AxisRef[NK], estimate: Table[effects.Id, neural.Id],
      origins: RelationOrigins, estimability: Vector[EffectEstimability]
  ): Either[EvidenceError, Relation[effects.Id, neural.Id]] =
    if origins.access match
        case RelationAccess.ScopedReplay(witness) => !witness.isActive
        case _ => false
    then Left(EvidenceError.InvalidSource("cannot attach expired scoped replay"))
    else if !origins.support.outputAssociation.forall(_ == effects.descriptor) then
      Left(EvidenceError.AxisMismatch("relation support output", effects.descriptor.stableKey, origins.support.outputAssociation.get.stableKey))
    else if estimate.rows != effects.size then Left(EvidenceError.ShapeMismatch("relation effects", effects.size, estimate.rows))
    else if estimate.cols != neural.size then Left(EvidenceError.ShapeMismatch("relation neural", neural.size, estimate.cols))
    else if estimability.length != effects.size then Left(EvidenceError.ShapeMismatch("relation estimability", effects.size, estimability.length))
    else Right(new Relation(effects.evidence, neural.evidence, effects.descriptor, neural.descriptor, estimate, origins, estimability))

final class RelationSet[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace] private[relation] (
    val partitions: SpaceEvidence[P],
    val partitionAxis: AxisDescriptor,
    val effects: SpaceEvidence[E],
    val effectAxis: AxisDescriptor,
    /** Stable effect keys preserve the frozen declared effect order for RDMs. */
    val effectKeys: Vector[String],
    val neural: SpaceEvidence[N],
    val neuralAxis: AxisDescriptor,
    val relations: Vector[Relation[E, N]]
)

object RelationSet:
  def apply[PK, EK, NK](
      partitions: AxisRef[PK],
      effects: AxisRef[EK],
      neural: AxisRef[NK],
      relations: Vector[Relation[effects.Id, neural.Id]]
  ): Either[EvidenceError, RelationSet[partitions.Id, effects.Id, neural.Id]] =
    if relations.isEmpty then Left(EvidenceError.InvalidAxis("relations", "must contain at least one partition"))
    else if relations.length != partitions.size then Left(EvidenceError.ShapeMismatch("partition relations", partitions.size, relations.length))
    else
      val keys = (0 until effects.size).toVector.map(effects.index.stableKeyAt).foldLeft[Either[EvidenceError, Vector[String]]](Right(Vector.empty)):
        case (acc, next) => for values <- acc; key <- next yield values :+ key
      keys.map(effectKeys => new RelationSet(partitions.evidence, partitions.descriptor, effects.evidence, effects.descriptor, effectKeys,
        neural.evidence, neural.descriptor, relations))
