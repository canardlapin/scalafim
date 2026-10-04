package scalafim.fmri.mvpa

import multivar.core.SemanticSpace
import resample4s.core.*
import resample4s.designs.FixedPartitions
import scala.collection.mutable

enum ValidationRole:
  case Analysis, Assessment

  def label: String =
    this match
      case Analysis   => "analysis"
      case Assessment => "assessment"

enum GroupingError:
  case InvalidStableGroupKey(row: Int, detail: String)
  case TooFewGroups(actual: Int, minimum: Int)
  case SplitMembershipMismatch(
      unit: UnitKey,
      role: ValidationRole,
      expectedSize: Int,
      actualSize: Int,
      firstDifference: Int
  )

  def message: String =
    this match
      case InvalidStableGroupKey(row, detail) =>
        s"invalid stable group key at row $row: $detail"
      case TooFewGroups(actual, minimum) =>
        s"leave-one-group-out validation requires at least $minimum groups, obtained $actual"
      case SplitMembershipMismatch(unit, role, expectedSize, actualSize, firstDifference) =>
        s"${role.label} membership for resampling unit $unit differs at position $firstDifference: expected size $expectedSize, obtained $actualSize"

opaque type GroupingSignature = String

object GroupingSignature:
  private[mvpa] def unsafe(value: String): GroupingSignature =
    value

  extension (signature: GroupingSignature)
    def value: String =
      signature

final case class GroupRef[G] private[mvpa] (
    value: G,
    stableKey: String
)

final case class GroupMembership[K, G] private[mvpa] (
    sampleKey: K,
    sampleStableKey: String,
    group: GroupRef[G]
)

/** Complete scientific identity of a grouping column and its sample-to-group membership relation. */
final case class GroupingIdentity(
    column: ColumnIdentity,
    stableGroupKeys: Vector[String],
    membershipSignature: GroupingSignature
)

/** An identified grouping column over one exact sample axis.
  *
  * Group equivalence is defined by `GroupRef.stableKey`, not by display spelling or implementation codes. Provider
  * labels are a private ordinal encoding of this complete keyed relation.
  */
final class GroupingColumn[
    S <: SemanticSpace,
    K,
    G
] private[mvpa] (
    val samples: AxisRef[K] { type Id = S },
    val column: Column[S, G],
    val groups: Vector[GroupRef[G]],
    val memberships: Vector[GroupMembership[K, G]],
    val identity: GroupingIdentity,
    private[mvpa] val labels: Labels,
    private val memberOrdinals: Vector[Vector[Int]]
):
  def groupCount: Int =
    groups.length

  def groupAt(ordinal: Int): Either[EvidenceError, GroupRef[G]] =
    if ordinal < 0 || ordinal >= groups.length then Left(EvidenceError.InvalidOrdinal(ordinal, groups.length))
    else Right(groups(ordinal))

  def membershipAt(ordinal: Int): Either[EvidenceError, GroupMembership[K, G]] =
    if ordinal < 0 || ordinal >= memberships.length then Left(EvidenceError.InvalidOrdinal(ordinal, memberships.length))
    else Right(memberships(ordinal))

  private[mvpa] def assessmentOrdinals(group: Int): Either[EvidenceError, Vector[Int]] =
    if group < 0 || group >= memberOrdinals.length then Left(EvidenceError.InvalidOrdinal(group, memberOrdinals.length))
    else Right(memberOrdinals(group))

  private[mvpa] def analysisOrdinals(group: Int): Either[EvidenceError, Vector[Int]] =
    assessmentOrdinals(group).map: assessment =>
      val excluded = assessment.toSet
      Vector.range(0, samples.size).filterNot(excluded)

object GroupingColumn:
  def bind[K, G](
      samples: AxisRef[K],
      column: Column[samples.Id, G]
  )(
      stableGroupKey: G => String
  ): Either[EvidenceError, GroupingColumn[samples.Id, K, G]] =
    samples
      .validateDeclared("grouping column rows", column.toRecord.rows)
      .flatMap: _ =>
        build(samples, column, stableGroupKey)

  private def build[K, G](
      samples: AxisRef[K],
      column: Column[samples.Id, G],
      stableGroupKey: G => String
  ): Either[EvidenceError, GroupingColumn[samples.Id, K, G]] =
    val groupOrdinals = mutable.HashMap.empty[String, Int]
    val groupBuffer = mutable.ArrayBuffer.empty[GroupRef[G]]
    val memberBuffers = mutable.ArrayBuffer.empty[mutable.ArrayBuffer[Int]]
    val membershipBuffer = Vector.newBuilder[GroupMembership[K, G]]
    val codes = new Array[Int](samples.size)
    val values = column.values
    var row = 0
    while row < samples.size do
      val value = values(row)
      val stableKey = stableGroupKey(value)
      validateStableKey(row, stableKey) match
        case Left(error) => return Left(EvidenceError.GroupingFailure(error))
        case Right(_)    => ()
      val groupOrdinal =
        groupOrdinals.get(stableKey) match
          case Some(ordinal) => ordinal
          case None          =>
            val ordinal = groupBuffer.length
            groupOrdinals.update(stableKey, ordinal)
            groupBuffer += GroupRef(value, stableKey)
            memberBuffers += mutable.ArrayBuffer.empty[Int]
            ordinal
      val membership =
        for
          sampleKey <- samples.keyAt(row)
          sampleStableKey <- samples.index.stableKeyAt(row)
        yield GroupMembership(sampleKey, sampleStableKey, groupBuffer(groupOrdinal))
      membership match
        case Left(error)  => return Left(error)
        case Right(value) => membershipBuffer += value
      codes(row) = groupOrdinal
      memberBuffers(groupOrdinal) += row
      row += 1

    val groups = groupBuffer.toVector
    val memberships = membershipBuffer.result()
    Labels
      .of(IArray.unsafeFromArray(codes), groups.length)
      .left
      .map(EvidenceError.ResampleFailure.apply)
      .map: labels =>
        val signature = membershipSignature(samples, groups, memberships)
        val identity = GroupingIdentity(
          column.identity,
          groups.map(_.stableKey),
          signature
        )
        new GroupingColumn(
          samples,
          column,
          groups,
          memberships,
          identity,
          labels,
          memberBuffers.map(_.toVector).toVector
        )

  private def validateStableKey(row: Int, value: String): Either[GroupingError, Unit] =
    if value.isEmpty then Left(GroupingError.InvalidStableGroupKey(row, "must be non-empty"))
    else if value != value.trim then
      Left(GroupingError.InvalidStableGroupKey(row, "must not have surrounding whitespace"))
    else if value.length > 4096 then
      Left(GroupingError.InvalidStableGroupKey(row, "must contain at most 4096 characters"))
    else if value.exists(_.isControl) then
      Left(GroupingError.InvalidStableGroupKey(row, "must not contain control characters"))
    else Right(())

  private def membershipSignature[K, G](
      samples: AxisRef[K],
      groups: Vector[GroupRef[G]],
      memberships: Vector[GroupMembership[K, G]]
  ): GroupingSignature =
    GroupingSignature.unsafe(
      AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.mvpa.grouping.v1")
        writer.string(samples.descriptor.coordinateSignature.value)
        writer.intLE(groups.length)
        groups.foreach(group => writer.string(group.stableKey))
        writer.intLE(memberships.length)
        memberships.foreach: membership =>
          writer.string(membership.sampleStableKey)
          writer.string(membership.group.stableKey)
    )

final case class LeaveOneGroupOutReceipt(
    samples: AxisDescriptor,
    grouping: GroupingIdentity,
    plan: PlanReceipt
)

final case class LeaveOneGroupOutUnitIdentity(
    key: UnitKey,
    heldOutGroupStableKey: String,
    grouping: GroupingIdentity,
    planAssignment: ContentDigest,
    analysis: ReindexingIdentity,
    assessment: ReindexingIdentity
)

final class LeaveOneGroupOutUnit[S <: SemanticSpace, K, G] private[mvpa] (
    val key: UnitKey,
    val heldOut: GroupRef[G],
    val analysis: ReindexingLeg[S, K, Selection],
    val assessment: ReindexingLeg[S, K, Selection],
    val receipt: LeaveOneGroupOutReceipt
):
  val identity: LeaveOneGroupOutUnitIdentity =
    LeaveOneGroupOutUnitIdentity(
      key,
      heldOut.stableKey,
      receipt.grouping,
      receipt.plan.assignment,
      analysis.identity,
      assessment.identity
    )

/** Exact-once validation holding out one complete identified group per unit. */
final class LeaveOneGroupOutDesign[S <: SemanticSpace, K, G] private[mvpa] (
    val grouping: GroupingColumn[S, K, G],
    val validation: ValidationDesign[S, K, Coverage.ExactOnce],
    val receipt: LeaveOneGroupOutReceipt
):
  val samples: AxisRef[K] { type Id = S } =
    grouping.samples

  val plan: Plan[Split[Selection], Coverage.ExactOnce] =
    validation.plan

  val diagnostics: PlanDiagnostics =
    validation.diagnostics

  val cost: PlanCost =
    validation.cost

  def keys: IndexedSeq[UnitKey] =
    validation.keys

  def at(key: UnitKey): Either[EvidenceError, LeaveOneGroupOutUnit[S, K, G]] =
    for
      unit <- validation.at(key)
      heldOut <- grouping.groupAt(key.fold)
      expectedAnalysis <- grouping.analysisOrdinals(key.fold)
      expectedAssessment <- grouping.assessmentOrdinals(key.fold)
      _ <- validateMembership(key, ValidationRole.Analysis, expectedAnalysis, unit.analysis.ordinals.toVector)
      _ <- validateMembership(key, ValidationRole.Assessment, expectedAssessment, unit.assessment.ordinals.toVector)
    yield new LeaveOneGroupOutUnit(key, heldOut, unit.analysis, unit.assessment, receipt)

  private def validateMembership(
      key: UnitKey,
      role: ValidationRole,
      expected: Vector[Int],
      actual: Vector[Int]
  ): Either[EvidenceError, Unit] =
    if expected == actual then Right(())
    else
      val common = math.min(expected.length, actual.length)
      var index = 0
      while index < common && expected(index) == actual(index) do index += 1
      Left(
        EvidenceError.GroupingFailure(
          GroupingError.SplitMembershipMismatch(
            key,
            role,
            expected.length,
            actual.length,
            index
          )
        )
      )

object LeaveOneGroupOutDesign:
  def bind[K, G](
      samples: AxisRef[K],
      groups: Column[samples.Id, G],
      seed: ScientificSeed
  )(
      stableGroupKey: G => String
  )(using algorithm: DigestAlgorithm): Either[EvidenceError, LeaveOneGroupOutDesign[samples.Id, K, G]] =
    GroupingColumn
      .bind(samples, groups)(stableGroupKey)
      .flatMap(fromGrouping(_, seed))

  def fromGrouping[S <: SemanticSpace, K, G](
      grouping: GroupingColumn[S, K, G],
      seed: ScientificSeed
  )(using algorithm: DigestAlgorithm): Either[EvidenceError, LeaveOneGroupOutDesign[S, K, G]] =
    if grouping.groupCount < 2 then
      Left(EvidenceError.GroupingFailure(GroupingError.TooFewGroups(grouping.groupCount, 2)))
    else
      for
        fixed <- FixedPartitions
          .once(grouping.labels)
          .left
          .map(EvidenceError.ResampleFailure.apply)
        validation <- ValidationDesign.bind(grouping.samples, fixed, seed)
        receipt = LeaveOneGroupOutReceipt(
          grouping.samples.descriptor,
          grouping.identity,
          validation.receipt
        )
      yield new LeaveOneGroupOutDesign(grouping, validation, receipt)
