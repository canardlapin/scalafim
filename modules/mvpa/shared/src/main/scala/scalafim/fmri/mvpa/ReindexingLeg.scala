package scalafim.fmri.mvpa

import gale.linalg.LinearOperator
import multivar.core.{
  CoordinateEvidence,
  Lin,
  Primal,
  SemanticProvenance,
  SemanticProvenanceEvent,
  SemanticSpace,
  SpaceEvidence,
  ValueId,
  ValueIdentity
}
import resample4s.core.*
import scalafim.response.{DuplicatePolicy, OrderedIndices}
import scala.annotation.implicitNotFound

enum ReindexingKind derives CanEqual:
  case Selection
  case Injection
  case Draw
  case Permutation

  def tag: String =
    this match
      case Selection   => "selection"
      case Injection   => "injection"
      case Draw        => "draw"
      case Permutation => "permutation"

opaque type OccurrenceAddress = Vector[Int]

object OccurrenceAddress:
  val root: OccurrenceAddress = Vector.empty

  private[mvpa] def append(
      address: OccurrenceAddress,
      position: Int
  ): Either[EvidenceError, OccurrenceAddress] =
    if position < 0 then
      Left(EvidenceError.InvalidAxis("occurrence address", s"negative position $position"))
    else if address.length >= 4096 then
      Left(EvidenceError.InvalidAxis("occurrence address", "must contain at most 4096 positions"))
    else Right(address :+ position)

  extension (address: OccurrenceAddress)
    def depth: Int =
      address.length

    def at(index: Int): Either[EvidenceError, Int] =
      if index < 0 || index >= address.length then Left(EvidenceError.InvalidOrdinal(index, address.length))
      else Right(address(index))

    def toVector: Vector[Int] =
      address

final case class AxisMember[K] private[mvpa] (
    parentKey: K,
    parentStableKey: String,
    occurrence: OccurrenceAddress
)

object AxisMember:
  private[mvpa] def stableKey[K](member: AxisMember[K]): String =
    val digest = AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.mvpa.axis-member.v1")
      writer.string(member.parentStableKey)
      val address = member.occurrence.toVector
      writer.intLE(address.length)
      address.foreach(writer.intLE)
    s"member-sha256-$digest"

opaque type ReindexingSignature = String

object ReindexingSignature:
  private[mvpa] def unsafe(value: String): ReindexingSignature =
    value

  extension (signature: ReindexingSignature)
    def value: String =
      signature

final case class ReindexingStep(
    kind: ReindexingKind,
    domain: Int,
    codomain: Int,
    mappingSignature: ReindexingSignature
)

final case class ReindexingIdentity(
    parent: AxisDescriptor,
    child: AxisDescriptor,
    steps: Vector[ReindexingStep]
)

@implicitNotFound(
  "Cannot bind abstract reindexing ${R}. Keep the value typed as resample4s.core.Selection, Injection, Draw, or Permutation."
)
sealed trait ReindexingKindEvidence[R <: Reindexing]:
  def kind: ReindexingKind
  private[mvpa] def duplicatePolicy: DuplicatePolicy

object ReindexingKindEvidence:
  given selection: ReindexingKindEvidence[Selection] with
    val kind: ReindexingKind = ReindexingKind.Selection
    private[mvpa] val duplicatePolicy: DuplicatePolicy = DuplicatePolicy.Reject

  given injection: ReindexingKindEvidence[Injection] with
    val kind: ReindexingKind = ReindexingKind.Injection
    private[mvpa] val duplicatePolicy: DuplicatePolicy = DuplicatePolicy.Reject

  given draw: ReindexingKindEvidence[Draw] with
    val kind: ReindexingKind = ReindexingKind.Draw
    private[mvpa] val duplicatePolicy: DuplicatePolicy = DuplicatePolicy.Allow

  given permutation: ReindexingKindEvidence[Permutation] with
    val kind: ReindexingKind = ReindexingKind.Permutation
    private[mvpa] val duplicatePolicy: DuplicatePolicy = DuplicatePolicy.Reject

/** A Resample4s ordinal map bound to exact parent and child scientific axes.
  *
  * The child keys retain the typed root key and a draw-occurrence address. The numerical leg is a matrix-free Gale
  * gather/scatter operator wrapped once by Multivar's semantic `Lin`.
  */
sealed trait ReindexingLeg[
    P <: SemanticSpace,
    K,
    R <: Reindexing
] :
  type Child <: SemanticSpace

  val parent: SpaceEvidence[P]
  val parentAxis: AxisDescriptor
  private[mvpa] val parentRecord: AxisRecord
  val child: AxisRef[AxisMember[K]] { type Id = Child }
  val ordinals: R
  val responseSelection: OrderedIndices[P]
  val steps: Vector[ReindexingStep]
  private[mvpa] val childMembers: Vector[AxisMember[K]]
  val leg: Lin[Primal[P], Primal[Child]]

  val identity: ReindexingIdentity =
    ReindexingIdentity(parentAxis, child.descriptor, steps)

  def kind: ReindexingKind =
    ReindexingLeg.kindOf(ordinals)

  def size: Int =
    child.size

  def members: Vector[AxisMember[K]] =
    childMembers

  def sameIdentityAs[P2 <: SemanticSpace, R2 <: Reindexing](
      that: ReindexingLeg[P2, K, R2]
  ): Boolean =
    identity == that.identity

  /** Continue from this exact child. Draws append an occurrence position; injective maps retain inherited addresses. */
  def continue[R2 <: Reindexing](
      by: R2
  )(using evidence: ReindexingKindEvidence[R2]): Either[EvidenceError, ReindexingLeg[Child, K, R2]] =
    ReindexingLeg.build(child, childMembers, by, evidence)

  /** Compose two already-bound legs while retaining the final child's complete staged identity and strongest provider
    * reindexing type.
    */
  def andThen[R2 <: Reindexing](
      next: ReindexingLeg[Child, K, R2]
  )(using compose: Compose[R, R2]): Either[
    EvidenceError,
    ReindexingLeg[P, K, compose.Out] { type Child = next.Child }
  ] =
    ordinals
      .after(next.ordinals)
      .left
      .map(EvidenceError.ReindexingCompositionFailure.apply)
      .flatMap: composed =>
        ReindexingLeg
          .responseIndices(parentRecord, parent, composed)
          .map: selected =>
            val result = new ReindexingLeg.Bound[P, K, compose.Out, next.Child](
              parent,
              parentAxis,
              parentRecord,
              next.child,
              composed,
              selected,
              steps ++ next.steps,
              next.childMembers
            )(leg.andThen(next.leg))
            result

object ReindexingLeg:
  private final class Bound[
      P <: SemanticSpace,
      K,
      R <: Reindexing,
      C <: SemanticSpace
  ](
      val parent: SpaceEvidence[P],
      val parentAxis: AxisDescriptor,
      private[mvpa] val parentRecord: AxisRecord,
      val child: AxisRef[AxisMember[K]] { type Id = C },
      val ordinals: R,
      val responseSelection: OrderedIndices[P],
      val steps: Vector[ReindexingStep],
      private[mvpa] val childMembers: Vector[AxisMember[K]]
  )(val leg: Lin[Primal[P], Primal[C]])
      extends ReindexingLeg[P, K, R]:
    type Child = C

  def bind[K, R <: Reindexing](
      parent: AxisRef[K],
      by: R
  )(using evidence: ReindexingKindEvidence[R]): Either[EvidenceError, ReindexingLeg[parent.Id, K, R]] =
    rootMembers(parent).flatMap(build(parent, _, by, evidence))

  private def rootMembers[K](parent: AxisRef[K]): Either[EvidenceError, Vector[AxisMember[K]]] =
    val result = Vector.newBuilder[AxisMember[K]]
    var ordinal = 0
    while ordinal < parent.size do
      val member =
        for
          key <- parent.keyAt(ordinal)
          stableKey <- parent.index.stableKeyAt(ordinal)
        yield AxisMember(key, stableKey, OccurrenceAddress.root)
      member match
        case Left(error)  => return Left(error)
        case Right(value) => result += value
      ordinal += 1
    Right(result.result())

  private def build[PK, K, R <: Reindexing](
      parent: AxisRef[PK],
      sourceMembers: Vector[AxisMember[K]],
      by: R,
      evidence: ReindexingKindEvidence[R]
  ): Either[EvidenceError, ReindexingLeg[parent.Id, K, R]] =
    if by.codomain != parent.size then
      Left(EvidenceError.ShapeMismatch("reindexing parent", parent.size, by.codomain))
    else if by.domain == 0 then
      Left(EvidenceError.InvalidAxis("reindexing child", "must contain at least one coordinate"))
    else if sourceMembers.length != parent.size then
      Left(EvidenceError.ShapeMismatch("reindexing source members", parent.size, sourceMembers.length))
    else
      val mapping = by.toVector
      for
        members <- mappedMembers(sourceMembers, mapping, evidence.kind)
        step = ReindexingStep(
          evidence.kind,
          by.domain,
          by.codomain,
          mappingSignature(evidence.kind, by.codomain, mapping)
        )
        child <- AxisRef.fromKeys(
          parent.toRecord.namespace,
          parent.toRecord.role,
          members,
          parent.toRecord.basis,
          parent.toRecord.units,
          parent.toRecord.scale,
          parent.toRecord.lineage ++ stepLineage(parent.descriptor, step)
        )(AxisMember.stableKey)
        selected <- OrderedIndices
          .fromInts(
            parent.responseDomain,
            parent.size,
            mapping,
            evidence.duplicatePolicy
          )
          .left
          .map(EvidenceError.ResponseIndexFailure.apply)
        semanticLeg <- semanticLeg(parent, child, mapping)
      yield new Bound[parent.Id, K, R, child.Id](
        parent.evidence,
        parent.descriptor,
        parent.toRecord,
        child,
        by,
        selected,
        Vector(step),
        members
      )(semanticLeg)

  private def mappedMembers[K](
      source: Vector[AxisMember[K]],
      mapping: Vector[Int],
      kind: ReindexingKind
  ): Either[EvidenceError, Vector[AxisMember[K]]] =
    val result = Vector.newBuilder[AxisMember[K]]
    var position = 0
    while position < mapping.length do
      val parent = source(mapping(position))
      val occurrence =
        if kind == ReindexingKind.Draw then OccurrenceAddress.append(parent.occurrence, position)
        else Right(parent.occurrence)
      occurrence match
        case Left(error) => return Left(error)
        case Right(address) =>
          result += AxisMember(parent.parentKey, parent.parentStableKey, address)
      position += 1
    Right(result.result())

  private def mappingSignature(
      kind: ReindexingKind,
      codomain: Int,
      mapping: Vector[Int]
  ): ReindexingSignature =
    ReindexingSignature.unsafe(
      AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.mvpa.reindexing.v1")
        writer.string(kind.tag)
        writer.intLE(codomain)
        writer.intLE(mapping.length)
        mapping.foreach(writer.intLE)
    )

  private def stepLineage(
      parent: AxisDescriptor,
      step: ReindexingStep
  ): Vector[String] =
    Vector(
      "scalafim.mvpa.reindex/v1",
      s"parent=${parent.coordinateSignature.value}",
      s"kind=${step.kind.tag}",
      s"mapping=${step.mappingSignature.value}"
    )

  private def semanticLeg[PK, K](
      parent: AxisRef[PK],
      child: AxisRef[AxisMember[K]],
      mapping: Vector[Int]
  ): Either[EvidenceError, Lin[Primal[parent.Id], Primal[child.Id]]] =
    val operator = LinearOperator.fromFunctions(mapping.length, parent.size)(
      (input, output) =>
        var position = 0
        while position < mapping.length do
          output(position) = input(mapping(position))
          position += 1,
      (input, output) =>
        output.clear()
        var position = 0
        while position < mapping.length do
          val source = mapping(position)
          output(source) = output(source) + input(position)
          position += 1
    )
    val identity =
      ValueIdentity.source(ValueId.unsafe(s"reindex-${child.descriptor.coordinateSignature.value}"))
    Lin
      .fromLinearMap(
        operator,
        CoordinateEvidence.primal(parent.evidence),
        CoordinateEvidence.primal(child.evidence),
        identity,
        SemanticProvenance
          .source("scalafim-mvpa-reindex")
          .append(SemanticProvenanceEvent.Derived("axis-reindex", Vector(identity)))
      )
      .left
      .map(EvidenceError.SemanticFailure.apply)

  private def responseIndices[P <: SemanticSpace](
      parentRecord: AxisRecord,
      parent: SpaceEvidence[P],
      reindexing: Reindexing
  ): Either[EvidenceError, OrderedIndices[P]] =
    val values = reindexing.toVector
    val policy =
      if values.distinct.length == values.length then DuplicatePolicy.Reject
      else DuplicatePolicy.Allow
    scalafim.response.DomainId
      .fromString[P](parent.descriptor.id.value)
      .left
      .map(EvidenceError.ResponseIdentityFailure.apply)
      .flatMap: domain =>
        OrderedIndices
          .fromInts(domain, parentRecord.stableKeys.length, values, policy)
          .left
          .map(EvidenceError.ResponseIndexFailure.apply)

  private[mvpa] def kindOf(value: Reindexing): ReindexingKind =
    value match
      case _: Selection   => ReindexingKind.Selection
      case _: Injection   => ReindexingKind.Injection
      case _: Draw        => ReindexingKind.Draw
      case _: Permutation => ReindexingKind.Permutation
