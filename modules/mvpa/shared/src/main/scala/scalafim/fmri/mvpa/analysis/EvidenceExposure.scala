package scalafim.fmri.mvpa.analysis

import scalafim.fmri.mvpa.AxisDigest
import scala.util.control.NonFatal

/** Identity of a result which may have been exposed to an adaptive decision.
  * It is supplied by the producing boundary; this module has no registry of
  * results or external evidence.
  */
opaque type ResultIdentity = String

object ResultIdentity:
  def apply(value: String): ResultIdentity =
    require(value.nonEmpty && value == value.trim, "result identity must be non-empty and trimmed")
    value

  extension (value: ResultIdentity) inline def text: String = value

opaque type ExposureRecordId = String

object ExposureRecordId:
  extension (value: ExposureRecordId) inline def text: String = value

/** The immutable identities to which an exposure account belongs. */
final case class ExposureReference(
    plan: PlanId,
    evidenceIdentity: String,
    provenanceIdentity: String,
    result: ResultIdentity
):
  require(evidenceIdentity.nonEmpty && evidenceIdentity == evidenceIdentity.trim, "evidence identity must be non-empty and trimmed")
  require(provenanceIdentity.nonEmpty && provenanceIdentity == provenanceIdentity.trim, "provenance identity must be non-empty and trimmed")

enum ExposurePurpose:
  case PayloadRead
  case DerivedScoreView
  case RoiSelection
  case ModelSelection
  case Handoff

enum ExposureActorRole:
  case Analyst
  case Model
  case Service

enum ExposureScope:
  /** The footprint of external evidence is not known. */
  case Unknown
  case Training
  case Holdout
  case WholePopulation

  def canOverlap(other: ExposureScope): Boolean =
    (this, other) match
      case (Unknown, _) | (_, Unknown) => true
      case (WholePopulation, _) | (_, WholePopulation) => true
      case _ => this == other

enum ExposurePayload:
  case Payload
  case DerivedScore

enum ExposureAssurance:
  /** A caller's declaration; it establishes no operator constraint. */
  case Declared
  /** The adapter counted a concrete attempt. */
  case Instrumented
  /** Reserved for a provider capability which proves the constrained read. */
  case Constrained

enum ExposureOutcome:
  case Succeeded
  case Failed(detail: String)
  case PartiallyReturned(returnedCells: Long, detail: String)

enum AdaptiveSelectionDependency:
  case NoneObserved
  case DependsOnObservedScores

final case class ExposureRequest(
    purpose: ExposurePurpose,
    actor: ExposureActorRole,
    scope: ExposureScope,
    payload: ExposurePayload,
    assurance: ExposureAssurance,
    maximumCells: Long
):
  require(maximumCells >= 0L, "exposure budget must be non-negative")
  require(!(payload == ExposurePayload.DerivedScore && purpose == ExposurePurpose.PayloadRead), "payload reads cannot be labelled derived scores")

final case class ExposureEvent(
    request: ExposureRequest,
    outcome: ExposureOutcome,
    adaptiveDependency: AdaptiveSelectionDependency
)

/** Passed snapshots are values. They are deliberately not synchronized with
  * an external store and cannot claim to discover changes made elsewhere.
  */
final class EvidenceExposure private (
    val reference: ExposureReference,
    val initialScope: ExposureScope,
    val events: Vector[ExposureEvent],
    val identity: ExposureRecordId
):
  private[analysis] def append(event: ExposureEvent): EvidenceExposure =
    EvidenceExposure(reference, initialScope, events :+ event)

  def hasObservedScores(scope: ExposureScope): Boolean =
    events.exists: event =>
      event.request.payload == ExposurePayload.DerivedScore && event.request.scope.canOverlap(scope)

  /** A callback failure is still an access attempt. It cannot support an
    * untouched claim for any potentially overlapping population. */
  def hasPayloadAccess(scope: ExposureScope): Boolean =
    events.exists: event =>
      event.request.payload == ExposurePayload.Payload && event.request.scope.canOverlap(scope)

object EvidenceExposure:
  /** External evidence starts unknown. A declaration alone never makes an
    * unseen external payload eligible for an untouched confirmation claim.
    */
  def external(reference: ExposureReference): EvidenceExposure =
    apply(reference, ExposureScope.Unknown, Vector.empty)

  def internal(reference: ExposureReference): EvidenceExposure =
    apply(reference, ExposureScope.Training, Vector.empty)

  private[analysis] def apply(reference: ExposureReference, initialScope: ExposureScope, events: Vector[ExposureEvent]): EvidenceExposure =
    val identity = AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.mvpa.evidence-exposure.v1")
      writer.string(reference.plan.text)
      writer.string(reference.evidenceIdentity)
      writer.string(reference.provenanceIdentity)
      writer.string(reference.result.text)
      writer.string(initialScope.toString)
      writer.intLE(events.length)
      events.foreach: event =>
        writer.string(event.request.purpose.toString)
        writer.string(event.request.actor.toString)
        writer.string(event.request.scope.toString)
        writer.string(event.request.payload.toString)
        writer.string(event.request.assurance.toString)
        writer.string(event.request.maximumCells.toString)
        writer.string(event.outcome.toString)
        writer.string(event.adaptiveDependency.toString)
    new EvidenceExposure(reference, initialScope, events, identity)

/** A permit binds exactly one passed snapshot and one bounded request. */
final class ExposurePermit private[analysis] (val record: ExposureRecordId, val request: ExposureRequest)

enum ExposureError:
  case UnknownFootprintForTrainingProbe
  case TrainingOnlyConstraintUnsupported
  case NativeAssuranceUnsupported
  case ReferenceMismatch
  case StalePermit(expected: ExposureRecordId, supplied: ExposureRecordId)
  case RequestMismatch
  case BudgetExceeded(requested: Long, permitted: Long)

enum ExposureAttempt[+A]:
  case Refused(error: ExposureError, exposure: EvidenceExposure)
  case Completed(value: A, exposure: EvidenceExposure)
  case Failed(detail: String, exposure: EvidenceExposure)

object ExposureControl:
  /** Issues a permit without executing an operator. Native providers without
    * a real constrained-read capability must use WholePopulation here.
    */
  def permit(current: EvidenceExposure, request: ExposureRequest): Either[ExposureError, ExposurePermit] =
    if current.initialScope == ExposureScope.Unknown && request.scope == ExposureScope.Training then Left(ExposureError.UnknownFootprintForTrainingProbe)
    else if request.scope == ExposureScope.Training && request.assurance == ExposureAssurance.Constrained then Left(ExposureError.TrainingOnlyConstraintUnsupported)
    else Right(new ExposurePermit(current.identity, request))

  def read[A](
      current: EvidenceExposure,
      permit: ExposurePermit,
      request: ExposureRequest
  )(callback: => Either[String, A]): ExposureAttempt[A] =
    authorize(current, permit, request) match
      case Left(error) => ExposureAttempt.Refused(error, current)
      case Right(()) =>
        val dependency =
          if request.purpose == ExposurePurpose.RoiSelection || request.purpose == ExposurePurpose.ModelSelection || request.purpose == ExposurePurpose.Handoff then
            if current.hasObservedScores(request.scope) then AdaptiveSelectionDependency.DependsOnObservedScores else AdaptiveSelectionDependency.NoneObserved
          else AdaptiveSelectionDependency.NoneObserved
        try
          callback match
            case Right(value) => ExposureAttempt.Completed(value, current.append(ExposureEvent(request, ExposureOutcome.Succeeded, dependency)))
            case Left(detail) => ExposureAttempt.Failed(detail, current.append(ExposureEvent(request, ExposureOutcome.Failed(detail), dependency)))
        catch
          case NonFatal(error) =>
            val detail = Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getName)
            ExposureAttempt.Failed(detail, current.append(ExposureEvent(request, ExposureOutcome.Failed(detail), dependency)) )

  def authorize(current: EvidenceExposure, permit: ExposurePermit, request: ExposureRequest): Either[ExposureError, Unit] =
    if current.identity != permit.record then Left(ExposureError.StalePermit(permit.record, current.identity))
    else if permit.request != request then Left(ExposureError.RequestMismatch)
    else if request.maximumCells > permit.request.maximumCells then Left(ExposureError.BudgetExceeded(request.maximumCells, permit.request.maximumCells))
    else Right(())

  /** A selection after a viewed score cannot be represented as untouched. */
  def untouchedConfirmation(current: EvidenceExposure, scope: ExposureScope): Either[AdaptiveSelectionDependency, Unit] =
    if current.initialScope == ExposureScope.Unknown || current.hasObservedScores(scope) || current.hasPayloadAccess(scope) then Left(AdaptiveSelectionDependency.DependsOnObservedScores)
    else Right(())

/** Data-only form for durable handoff. Reconstruction recalculates identity. */
final case class EvidenceExposureDto(
    reference: ExposureReference,
    initialScope: ExposureScope,
    events: Vector[ExposureEvent],
    identity: ExposureRecordId
)

object EvidenceExposureDto:
  def from(exposure: EvidenceExposure): EvidenceExposureDto =
    EvidenceExposureDto(exposure.reference, exposure.initialScope, exposure.events, exposure.identity)

  def reconstruct(dto: EvidenceExposureDto): Either[String, EvidenceExposure] =
    val rebuilt = EvidenceExposure(dto.reference, dto.initialScope, dto.events)
    if rebuilt.identity == dto.identity then Right(rebuilt)
    else Left("exposure DTO identity does not match its immutable contents")
