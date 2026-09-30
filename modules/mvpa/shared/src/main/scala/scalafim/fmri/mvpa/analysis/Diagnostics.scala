package scalafim.fmri.mvpa.analysis

import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, AxisRef, AxisSignature, EvidenceError}

/** Stable, machine-readable failure facts for bounded diagnostic requests. */
enum DiagnosticError:
  case InvalidLimit(value: Int)
  case LimitExceedsMaximum(value: Int, maximum: Int)
  case InvalidContinuation(value: Int, condition: String)
  case ContinuationOutOfRange(value: Int, maximum: Int)
  case ContinuationTargetMismatch
  case AxisLookup(error: EvidenceError)

  def code: String =
    this match
      case InvalidLimit(_)                 => "diagnostic.invalid-limit"
      case LimitExceedsMaximum(_, _)       => "diagnostic.limit-exceeds-maximum"
      case InvalidContinuation(_, _)       => "diagnostic.invalid-continuation"
      case ContinuationOutOfRange(_, _)    => "diagnostic.continuation-out-of-range"
      case ContinuationTargetMismatch       => "diagnostic.continuation-target-mismatch"
      case AxisLookup(_)                    => "diagnostic.axis-lookup"

  def stage: String = "inspection"

  def parameter: String =
    this match
      case InvalidLimit(_)                 => "limit"
      case LimitExceedsMaximum(_, _)       => "limit"
      case InvalidContinuation(_, _)       => "continuation"
      case ContinuationOutOfRange(_, _)    => "continuation"
      case ContinuationTargetMismatch       => "continuation"
      case AxisLookup(_)                    => "axis"

  def observed: String =
    this match
      case InvalidLimit(value)                 => value.toString
      case LimitExceedsMaximum(value, _)       => value.toString
      case InvalidContinuation(value, _)       => value.toString
      case ContinuationOutOfRange(value, _)    => value.toString
      case ContinuationTargetMismatch           => "foreign continuation"
      case AxisLookup(error)                    => error.message

  def required: String =
    this match
      case InvalidLimit(_)                 => "a positive page limit"
      case LimitExceedsMaximum(_, maximum) => s"a page limit no greater than $maximum"
      case InvalidContinuation(_, value)   => value
      case ContinuationOutOfRange(_, value) => s"an ordinal from 0 through $value"
      case ContinuationTargetMismatch       => "a continuation issued for this exact diagnostic target"
      case AxisLookup(_)                    => "a readable declared axis entry"

  def causalPath: Vector[String] =
    this match
      case InvalidLimit(_) => Vector("inspect", "page-limit")
      case LimitExceedsMaximum(_, _) => Vector("inspect", "page-limit")
      case InvalidContinuation(_, _) | ContinuationOutOfRange(_, _) => Vector("inspect", "continuation")
      case ContinuationTargetMismatch => Vector("inspect", "continuation", "target")
      case AxisLookup(_) => Vector("inspect", "axis", "stable-key")

final class AxisContinuation private[analysis] (val axis: AxisSignature, val ordinal: Int)

/** Page result whose continuation is bound to the inspected axis. */
final case class AxisPage(
    axis: AxisDescriptor,
    entries: Vector[(Int, String)],
    next: Option[AxisContinuation]
)

object AxisPage:
  def inspect[K](axis: AxisRef[K], limit: Int, continuation: Option[AxisContinuation] = None): Either[DiagnosticError, AxisPage] =
    if limit < 1 then Left(DiagnosticError.InvalidLimit(limit))
    else if limit > Diagnostics.MaximumLimit then Left(DiagnosticError.LimitExceedsMaximum(limit, Diagnostics.MaximumLimit))
    else
      if continuation.exists(_.axis != axis.descriptor.coordinateSignature) then return Left(DiagnosticError.ContinuationTargetMismatch)
      val start = continuation.fold(0)(_.ordinal)
      if start < 0 then Left(DiagnosticError.InvalidContinuation(start, "a non-negative ordinal"))
      else if start > axis.size then Left(DiagnosticError.ContinuationOutOfRange(start, axis.size))
      else
        val until = math.min(axis.size, start.toLong + limit.toLong).toInt
        val builder = Vector.newBuilder[(Int, String)]
        var ordinal = start
        while ordinal < until do
          axis.index.stableKeyAt(ordinal) match
            case Left(error) => return Left(DiagnosticError.AxisLookup(error))
            case Right(key)             => builder += ordinal -> key
          ordinal += 1
        Right(AxisPage(axis.descriptor, builder.result(), if until < axis.size then Some(new AxisContinuation(axis.descriptor.coordinateSignature, until)) else None))

enum ContentIdentity:
  case Declared(revision: String)
  case ProviderReportedBlocks(revision: String, blocks: Vector[String])
  case ProviderReportedComplete(revision: String, blocks: Vector[String], content: String)

object ContentIdentity:
  def from(receipt: EvidenceReceipt): ContentIdentity =
    receipt.verifiedCompleteContent match
      case Some(content) => ProviderReportedComplete(receipt.declaredRevision, receipt.verifiedReadBlocks, content)
      case None if receipt.verifiedReadBlocks.nonEmpty => ProviderReportedBlocks(receipt.declaredRevision, receipt.verifiedReadBlocks)
      case None => Declared(receipt.declaredRevision)

final case class PlanDescription(
    plan: PlanId,
    estimand: EstimandId,
    sourceAxes: Vector[AxisSignature],
    designAxis: AxisSignature,
    frameAxis: AxisSignature,
    sourceIdentity: String,
    designIdentity: String,
    frameIdentity: String,
    question: String,
    assumptions: Vector[String],
    preparation: Vector[String],
    reduction: String,
    parameters: Vector[(String, String)],
    requiredCapabilities: Set[CapabilityId],
    providedCapabilities: CapabilitySet,
    unknowns: Vector[String],
    metadataSize: Int,
    nextMetadata: Option[PlanMetadataContinuation]
)

final class PlanMetadataContinuation private[analysis] (val plan: PlanId, val context: String, val ordinal: Int)
final class PlanExplanationContinuation private[analysis] (val plan: PlanId, val context: String, val ordinal: Int)

enum PlanChange:
  case SourceIdentity, DesignIdentity, FrameIdentity, Question, Assumptions, Preparation, Reduction, Parameters, Estimand, RequiredCapabilities, SourceCapabilities

enum RepairKind:
  case EquivalentExecution
  case ChangedScientificFidelity
  case ChangedEstimator
  case ChangedPopulation
  case ChangedClaim
  case RoiShrink

final class RepairAdvice private (val kind: RepairKind, val rebindRequired: Boolean, val legal: Boolean, val explanation: String)

object RepairAdvice:
  def forKind(kind: RepairKind): RepairAdvice =
    kind match
      case RepairKind.EquivalentExecution => new RepairAdvice(kind, false, true, "execution implementation may change while the scientific plan remains bound")
      case RepairKind.ChangedScientificFidelity => new RepairAdvice(kind, true, true, "scientific fidelity changed; create and bind a new specification")
      case RepairKind.ChangedEstimator => new RepairAdvice(kind, true, true, "estimator changed; create and bind a new specification")
      case RepairKind.ChangedPopulation => new RepairAdvice(kind, true, true, "population changed; create and bind a new specification")
      case RepairKind.ChangedClaim => new RepairAdvice(kind, true, true, "claim changed; create and bind a new specification")
      case RepairKind.RoiShrink => new RepairAdvice(kind, true, false, "ROI scope is not inferable from a frame hash; declare a new measurement scope and rebind")

final class PlanDiff private (
    val changes: Set[PlanChange],
    val left: PlanId,
    val right: PlanId
):
  val rebindRequired: Boolean = changes.nonEmpty

final case class DiagnosticIssue(
    code: String,
    stage: String,
    parameter: String,
    observed: String,
    required: String,
    causalPath: Vector[String]
)

final case class PlanExplanation(
    blockers: Vector[DiagnosticIssue],
    blockerCount: Int,
    nextBlocker: Option[PlanExplanationContinuation],
    nextOperations: Vector[String]
)

/** Bounded inspection of an already-passed exposure record. This is metadata:
  * it never invokes an evidence operator or any numerical callback. */
final case class ExposureDescription(
    identity: ExposureRecordId,
    reference: ExposureReference,
    initialScope: ExposureScope,
    events: Vector[ExposureEvent],
    eventCount: Int,
    nextEvent: Option[ExposureContinuation]
)

final class ExposureContinuation private[analysis] (val exposure: ExposureRecordId, val ordinal: Int)

object Diagnostics:
  private val DefaultLimit = 128
  private[analysis] val MaximumLimit = 1024

  /** Bounded metadata view. Neither this operation nor its paginated counterpart
    * invokes open source, estimand, compiler, or numerical callbacks.
    */
  def describe[S, D, F, E <: Estimand[S, D, F]](
      specification: AnalysisSpecification[S, D, F, E],
      available: CapabilitySet
  ): PlanDescription =
    inspect(specification, available, DefaultLimit).fold(error => throw new IllegalStateException(error.code), identity)

  def inspect[S, D, F, E <: Estimand[S, D, F]](
      specification: AnalysisSpecification[S, D, F, E],
      available: CapabilitySet,
      limit: Int,
      continuation: Option[PlanMetadataContinuation] = None
  ): Either[DiagnosticError, PlanDescription] =
    val required = specification.requiredCapabilities.toVector.sortBy(_.text)
    val provided = specification.sourceCapabilities.values.values.toVector.sortBy(_.id.text)
    val admittedCapabilities = admitted(specification, available, provided).values.values.toVector.sortBy(_.id.text)
    val context = contextFingerprint(specification, available)
    val size = Vector(specification.sourceAxes.size, specification.assumptions.size,
      specification.preparation.size, specification.estimandParameters.size, required.size, provided.size).max
    planPage(specification.plan, context, size, limit, continuation.map(value => (value.plan, value.context, value.ordinal))).map: (start, until) =>
      PlanDescription(
        specification.plan, specification.estimandId, specification.sourceAxes.slice(start, until),
        specification.designAxis, specification.frameAxis, specification.sourceIdentity,
        specification.designIdentity, specification.frameIdentity, specification.question,
        specification.assumptions.slice(start, until), specification.preparation.slice(start, until),
        specification.reduction, specification.estimandParameters.slice(start, until),
        required.slice(start, until).toSet, CapabilitySet.from(admittedCapabilities.slice(start, until)),
        Vector("neural values, fitted statistics, payload verification, and realization cost are unknown until an explicit scoped operation"),
        size, if until < size then Some(new PlanMetadataContinuation(specification.plan, context, until)) else None
      )

  def inspectIdentity(receipt: EvidenceReceipt): ContentIdentity = ContentIdentity.from(receipt)

  def inspectExposure(
      exposure: EvidenceExposure,
      limit: Int,
      continuation: Option[ExposureContinuation] = None
  ): Either[DiagnosticError, ExposureDescription] =
    if continuation.exists(_.exposure != exposure.identity) then Left(DiagnosticError.ContinuationTargetMismatch)
    else page(exposure.events.size, limit, continuation.map(_.ordinal)).map: (start, until) =>
      ExposureDescription(
        exposure.identity,
        exposure.reference,
        exposure.initialScope,
        exposure.events.slice(start, until),
        exposure.events.size,
        if until < exposure.events.size then Some(new ExposureContinuation(exposure.identity, until)) else None
      )

  def explain[S, D, F, E <: Estimand[S, D, F]](
      specification: AnalysisSpecification[S, D, F, E],
      available: CapabilitySet
  ): PlanExplanation =
    inspectExplanation(specification, available, DefaultLimit).fold(error => throw new IllegalStateException(error.code), identity)

  def inspectExplanation[S, D, F, E <: Estimand[S, D, F]](
      specification: AnalysisSpecification[S, D, F, E],
      available: CapabilitySet,
      limit: Int,
      continuation: Option[PlanExplanationContinuation] = None
  ): Either[DiagnosticError, PlanExplanation] =
    val missing = admitted(specification, available, specification.sourceCapabilities.values.values.toVector).missing(specification.requiredCapabilities).toVector.sortBy(_.text)
    val context = contextFingerprint(specification, available)
    planPage(specification.plan, context, missing.size, limit, continuation.map(value => (value.plan, value.context, value.ordinal))).map: (start, until) =>
      val blockers = missing.slice(start, until).map: capability =>
        val sourceAbsent = !specification.sourceCapabilities.contains(capability)
        DiagnosticIssue("analysis.missing-source-capability", "binding", capability.text,
          if sourceAbsent then "capability absent from captured source metadata" else "capability absent from caller available capabilities",
          if sourceAbsent then "a source-declared capability" else "a caller-available capability",
          Vector("specification", "estimand", "required-capabilities", capability.text, if sourceAbsent then "source" else "available"))
      val operations =
        if missing.isEmpty then Vector("inspect metadata", "capability preflight passed; method binding remains a separate operation")
        else Vector("inspect metadata", "obtain the missing source declaration or caller-available capability, then rerun capability preflight")
      PlanExplanation(blockers, missing.size, if until < missing.size then Some(new PlanExplanationContinuation(specification.plan, context, until)) else None, operations)

  private def contextFingerprint[S, D, F, E <: Estimand[S, D, F]](specification: AnalysisSpecification[S, D, F, E], available: CapabilitySet): String =
    AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.mvpa.diagnostics-capability-context.v1")
      def write(values: CapabilitySet): Unit =
        val ordered = values.values.values.toVector.sortBy(_.id.text)
        writer.intLE(ordered.size)
        ordered.foreach: capability =>
          writer.string(capability.id.text)
          writer.string(capability.detail)
      write(specification.sourceCapabilities)
      write(available)

  private def admitted[S, D, F, E <: Estimand[S, D, F]](
      specification: AnalysisSpecification[S, D, F, E],
      available: CapabilitySet,
      provided: Vector[Capability]
  ): CapabilitySet =
    CapabilitySet.from(provided.filter(capability => specification.requiredCapabilities.contains(capability.id) && available.contains(capability.id)))

  private def planPage(
      plan: PlanId,
      context: String,
      size: Int,
      limit: Int,
      continuation: Option[(PlanId, String, Int)]
  ): Either[DiagnosticError, (Int, Int)] =
    if continuation.exists(value => value._1 != plan || value._2 != context) then Left(DiagnosticError.ContinuationTargetMismatch)
    else page(size, limit, continuation.map(_._3))

  private def page(size: Int, limit: Int, continuation: Option[Int]): Either[DiagnosticError, (Int, Int)] =
    val start = continuation.getOrElse(0)
    if limit < 1 then Left(DiagnosticError.InvalidLimit(limit))
    else if limit > MaximumLimit then Left(DiagnosticError.LimitExceedsMaximum(limit, MaximumLimit))
    else if start < 0 then Left(DiagnosticError.InvalidContinuation(start, "a non-negative ordinal"))
    else if start > size then Left(DiagnosticError.ContinuationOutOfRange(start, size))
    else Right(start -> math.min(size.toLong, start.toLong + limit.toLong).toInt)

  def diff[LS, LD, LF, LE <: Estimand[LS, LD, LF], RS, RD, RF, RE <: Estimand[RS, RD, RF]](
      left: AnalysisSpecification[LS, LD, LF, LE],
      right: AnalysisSpecification[RS, RD, RF, RE]
  ): PlanDiff =
    val changes = Set.newBuilder[PlanChange]
    if left.estimandId != right.estimandId then changes += PlanChange.Estimand
    if left.requiredCapabilities != right.requiredCapabilities then changes += PlanChange.RequiredCapabilities
    if left.sourceCapabilities != right.sourceCapabilities then changes += PlanChange.SourceCapabilities
    if left.sourceIdentity != right.sourceIdentity || left.sourceAxes != right.sourceAxes then changes += PlanChange.SourceIdentity
    if left.designIdentity != right.designIdentity || left.designAxis != right.designAxis then changes += PlanChange.DesignIdentity
    if left.frameIdentity != right.frameIdentity || left.frameAxis != right.frameAxis then changes += PlanChange.FrameIdentity
    if left.question != right.question then changes += PlanChange.Question
    if left.assumptions != right.assumptions then changes += PlanChange.Assumptions
    if left.preparation != right.preparation then changes += PlanChange.Preparation
    if left.reduction != right.reduction then changes += PlanChange.Reduction
    if left.estimandParameters != right.estimandParameters then changes += PlanChange.Parameters
    val result = changes.result()
    new PlanDiff(result, left.plan, right.plan)
