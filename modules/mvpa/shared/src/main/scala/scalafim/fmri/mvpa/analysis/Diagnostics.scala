package scalafim.fmri.mvpa.analysis

import scalafim.fmri.mvpa.{AxisDescriptor, AxisRef, AxisSignature, EvidenceError}

/** Stable, machine-readable failure facts for bounded diagnostic requests. */
enum DiagnosticError:
  case InvalidLimit(value: Int)
  case InvalidContinuation(value: Int, condition: String)
  case ContinuationOutOfRange(value: Int, maximum: Int)

  def code: String =
    this match
      case InvalidLimit(_)                 => "diagnostic.invalid-limit"
      case InvalidContinuation(_, _)       => "diagnostic.invalid-continuation"
      case ContinuationOutOfRange(_, _)    => "diagnostic.continuation-out-of-range"

  def stage: String = "inspection"

  def parameter: String =
    this match
      case InvalidLimit(_)                 => "limit"
      case InvalidContinuation(_, _)       => "continuation"
      case ContinuationOutOfRange(_, _)    => "continuation"

  def observed: String =
    this match
      case InvalidLimit(value)                 => value.toString
      case InvalidContinuation(value, _)       => value.toString
      case ContinuationOutOfRange(value, _)    => value.toString

  def required: String =
    this match
      case InvalidLimit(_)                 => "a positive page limit"
      case InvalidContinuation(_, value)   => value
      case ContinuationOutOfRange(_, value) => s"an ordinal from 0 through $value"

  def causalPath: Vector[String] =
    this match
      case InvalidLimit(_) => Vector("inspect", "page-limit")
      case InvalidContinuation(_, _) | ContinuationOutOfRange(_, _) => Vector("inspect", "continuation")

/** Page result whose continuation is an ordinal, never a hidden cursor over values. */
final case class AxisPage(
    axis: AxisDescriptor,
    entries: Vector[(Int, String)],
    next: Option[Int]
)

object AxisPage:
  def inspect[K](axis: AxisRef[K], limit: Int, continuation: Option[Int] = None): Either[DiagnosticError, AxisPage] =
    if limit < 1 then Left(DiagnosticError.InvalidLimit(limit))
    else
      val start = continuation.getOrElse(0)
      if start < 0 then Left(DiagnosticError.InvalidContinuation(start, "a non-negative ordinal"))
      else if start > axis.size then Left(DiagnosticError.ContinuationOutOfRange(start, axis.size))
      else
        val until = math.min(axis.size, start.toLong + limit.toLong).toInt
        val builder = Vector.newBuilder[(Int, String)]
        var ordinal = start
        while ordinal < until do
          axis.index.stableKeyAt(ordinal) match
            case Left(_: EvidenceError) => return Left(DiagnosticError.ContinuationOutOfRange(ordinal, axis.size))
            case Right(key)             => builder += ordinal -> key
          ordinal += 1
        Right(AxisPage(axis.descriptor, builder.result(), if until < axis.size then Some(until) else None))

enum ContentIdentity:
  case Declared(revision: String)
  case BlockVerified(revision: String, verifiedBlocks: Vector[String])
  case FullyVerified(revision: String, verifiedBlocks: Vector[String], content: String)

object ContentIdentity:
  def from(receipt: EvidenceReceipt): ContentIdentity =
    receipt.verifiedCompleteContent match
      case Some(content) => FullyVerified(receipt.declaredRevision, receipt.verifiedReadBlocks, content)
      case None if receipt.verifiedReadBlocks.nonEmpty => BlockVerified(receipt.declaredRevision, receipt.verifiedReadBlocks)
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
    nextMetadata: Option[Int]
)

enum PlanChange:
  case SourceIdentity, DesignIdentity, FrameIdentity, Question, Assumptions, Preparation, Reduction, Parameters, Estimand, RequiredCapabilities, SourceCapabilities

enum RepairKind:
  case EquivalentExecution
  case ChangedScientificFidelity
  case ChangedEstimator
  case ChangedPopulation
  case ChangedClaim
  case RoiShrink

final case class RepairAdvice(kind: RepairKind, rebindRequired: Boolean, legal: Boolean, explanation: String)

object RepairAdvice:
  def forKind(kind: RepairKind): RepairAdvice =
    kind match
      case RepairKind.EquivalentExecution => RepairAdvice(kind, false, true, "execution implementation may change while the scientific plan remains bound")
      case RepairKind.ChangedScientificFidelity => RepairAdvice(kind, true, true, "scientific fidelity changed; create and bind a new specification")
      case RepairKind.ChangedEstimator => RepairAdvice(kind, true, true, "estimator changed; create and bind a new specification")
      case RepairKind.ChangedPopulation => RepairAdvice(kind, true, true, "population changed; create and bind a new specification")
      case RepairKind.ChangedClaim => RepairAdvice(kind, true, true, "claim changed; create and bind a new specification")
      case RepairKind.RoiShrink => RepairAdvice(kind, true, false, "ROI shrinking is not a repair; declare a new measurement scope and rebind")

final case class PlanDiff(changes: Set[PlanChange], rebindRequired: Boolean, left: PlanId, right: PlanId)

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
    nextBlocker: Option[Int],
    nextOperations: Vector[String]
)

object Diagnostics:
  private val DefaultLimit = 128

  /** Bounded metadata view. Neither this operation nor its paginated counterpart
    * invokes open source, estimand, compiler, or numerical callbacks.
    */
  def describe[S, D, F, E <: Estimand[S, D, F]](
      specification: AnalysisSpecification[S, D, F, E]
  ): PlanDescription =
    inspect(specification, DefaultLimit).fold(error => throw new IllegalStateException(error.code), identity)

  def inspect[S, D, F, E <: Estimand[S, D, F]](
      specification: AnalysisSpecification[S, D, F, E],
      limit: Int,
      continuation: Option[Int] = None
  ): Either[DiagnosticError, PlanDescription] =
    val required = specification.requiredCapabilities.toVector.sortBy(_.text)
    val provided = specification.sourceCapabilities.values.values.toVector.sortBy(_.id.text)
    val size = Vector(specification.sourceAxes.size, specification.assumptions.size,
      specification.preparation.size, specification.estimandParameters.size, required.size, provided.size).max
    page(size, limit, continuation).map: (start, until) =>
      PlanDescription(
        specification.plan, specification.estimandId, specification.sourceAxes.slice(start, until),
        specification.designAxis, specification.frameAxis, specification.sourceIdentity,
        specification.designIdentity, specification.frameIdentity, specification.question,
        specification.assumptions.slice(start, until), specification.preparation.slice(start, until),
        specification.reduction, specification.estimandParameters.slice(start, until),
        required.slice(start, until).toSet, CapabilitySet.from(provided.slice(start, until)),
        Vector("neural values, fitted statistics, payload verification, and realization cost are unknown until an explicit scoped operation"),
        size, if until < size then Some(until) else None
      )

  def inspectIdentity(receipt: EvidenceReceipt): ContentIdentity = ContentIdentity.from(receipt)

  def explain[S, D, F, E <: Estimand[S, D, F]](
      specification: AnalysisSpecification[S, D, F, E]
  ): PlanExplanation =
    inspectExplanation(specification, DefaultLimit).fold(error => throw new IllegalStateException(error.code), identity)

  def inspectExplanation[S, D, F, E <: Estimand[S, D, F]](
      specification: AnalysisSpecification[S, D, F, E],
      limit: Int,
      continuation: Option[Int] = None
  ): Either[DiagnosticError, PlanExplanation] =
    val missing = specification.sourceCapabilities.missing(specification.requiredCapabilities).toVector.sortBy(_.text)
    page(missing.size, limit, continuation).map: (start, until) =>
      val blockers = missing.slice(start, until).map: capability =>
        DiagnosticIssue("analysis.missing-source-capability", "binding", capability.text,
          "capability absent from captured source metadata", "an admitted source capability",
          Vector("specification", "estimand", "required-capabilities", capability.text, "source"))
      val operations =
        if missing.isEmpty then Vector("inspect metadata", "bind the unchanged specification")
        else Vector("inspect metadata", "obtain an admitted source capability and bind a new specification")
      PlanExplanation(blockers, missing.size, if until < missing.size then Some(until) else None, operations)

  private def page(size: Int, limit: Int, continuation: Option[Int]): Either[DiagnosticError, (Int, Int)] =
    val start = continuation.getOrElse(0)
    if limit < 1 then Left(DiagnosticError.InvalidLimit(limit))
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
    PlanDiff(result, result.nonEmpty, left.plan, right.plan)
