package scalafim.fmri.design

import scalafim.fmri.hrf.Hrf

/** The HRF family assigned to one conceptual trial phase. */
enum HrfAssignment:
  /** One basis shared by every structural cell in the phase term. */
  case Shared(hrf: Hrf)
  /** An exhaustive basis assignment over the phase term's declared cells. */
  case ByCell(assignments: HrfByCell)

  def canonical: String =
    this match
      case Shared(hrf)          => s"shared(${HrfAssignment.hrfCanonical(hrf)})"
      case ByCell(assignments) => assignments.canonical

object HrfAssignment:
  private[design] def validate(value: HrfAssignment): Either[DesignError, Unit] =
    value match
      case Shared(hrf) => validateHrf(hrf)
      case ByCell(assignments) =>
        validateHrfs(assignments.assignments.map(_._2))

  private[design] def validateHrfs(values: Vector[Hrf]): Either[DesignError, Unit] =
    values.foldLeft[Either[DesignError, Unit]](Right(())) { (validated, hrf) =>
      validated.flatMap(_ => validateHrf(hrf))
    }

  private[design] def hrfCanonical(hrf: Hrf): String =
    val elements = hrf.basisElementsValidated.fold(
      error => Vector(s"invalid:${error.message}"),
      _.map(_.id.value)
    )
    val elementIdentity = elements.map(token).mkString("[", ",", "]")
    s"descriptor=${token(hrf.descriptor.canonicalId)}|name=${token(hrf.name)}|nbasis=${hrf.nbasis}|elements=$elementIdentity"

  private[design] def token(value: String): String =
    s"${value.length}:$value"

  private def validateHrf(hrf: Hrf): Either[DesignError, Unit] =
    hrf.basisElementsValidated
      .left.map(error => DesignError.InvalidSchema(s"HRF '${hrf.name}' has invalid basis identity: ${error.message}"))
      .map(_ => ())

/** Exhaustive HRF assignment for the phase identities present in a formula.
  *
  * Unphased terms remain governed by their own formula basis. Once a phase
  * plan is supplied, however, every realized phase must be assigned and every
  * declared assignment must be used.
  */
final case class HrfByPhase private[design] (
    assignments: Vector[(PhaseId, HrfAssignment)]
):
  require(assignments.nonEmpty, "an HRF-by-phase assignment must not be empty")
  require(assignments.map(_._1).distinct.length == assignments.length, "HRF-by-phase assignments must have unique phases")

  private val byPhase = assignments.toMap

  def resolve(phase: PhaseId): Either[DesignError, HrfAssignment] =
    byPhase.get(phase).toRight(
      DesignError.InvalidPhaseHrfAssignment(missing = Vector(phase), extra = Vector.empty)
    )

  def validateCoverage(realized: Vector[PhaseId]): Either[DesignError, Unit] =
    val phases = realized.distinct
    val missing = phases.filterNot(byPhase.contains).sortBy(_.value)
    val extra = byPhase.keySet.filterNot(phases.contains).toVector.sortBy(_.value)
    if missing.nonEmpty || extra.nonEmpty then
      Left(DesignError.InvalidPhaseHrfAssignment(missing, extra))
    else Right(())

  def canonical: String =
    assignments
      .sortBy(_._1.value)
      .map { case (phase, assignment) => s"${HrfAssignment.token(phase.value)}=${assignment.canonical}" }
      .mkString("hrf-by-phase[", ";", "]")

object HrfByPhase:
  def of(assignments: (PhaseId, HrfAssignment)*): Either[DesignError, HrfByPhase] =
    validated(assignments.toVector)

  /** Checked string boundary for compact public model specifications. */
  def named(assignments: (String, HrfAssignment)*): Either[DesignError, HrfByPhase] =
    val out = Vector.newBuilder[(PhaseId, HrfAssignment)]
    var error: Option[DesignError] = None
    val iterator = assignments.iterator
    while iterator.hasNext && error.isEmpty do
      val (name, assignment) = iterator.next()
      PhaseId(name) match
        case Left(value) => error = Some(value)
        case Right(id)   => out += id -> assignment
    error match
      case Some(value) => Left(value)
      case None        => validated(out.result())

  private def validated(values: Vector[(PhaseId, HrfAssignment)]): Either[DesignError, HrfByPhase] =
    if values.isEmpty then Left(DesignError.InvalidSchema("HRF-by-phase assignment must contain at least one phase"))
    else if values.map(_._1).distinct.length != values.length then
      Left(DesignError.InvalidSchema("HRF-by-phase assignments must have unique phases"))
    else
      values.foldLeft[Either[DesignError, Unit]](Right(())) { case (validated, (_, assignment)) =>
        validated.flatMap(_ => HrfAssignment.validate(assignment))
      }.map(_ => HrfByPhase(values))

/** Optional scan-space scaling applied after event convolution. */
enum HrfColumnScaling:
  /** Preserve the numerical columns in the coordinates of the assigned HRF. */
  case AsConvolved
  /** Divide each realized column by its own maximum absolute scan value. */
  case UnitMaximumAbsolute

  def label: String =
    this match
      case AsConvolved         => "as-convolved"
      case UnitMaximumAbsolute => "unit-maximum-absolute"

object HrfColumnScaling:
  def parse(value: String): Either[DesignError, HrfColumnScaling] =
    value.trim.toLowerCase.replace('_', '-') match
      case "as-convolved"         => Right(AsConvolved)
      case "unit-maximum-absolute" => Right(UnitMaximumAbsolute)
      case other =>
        Left(
          DesignError.FormulaBinding(
            s"unknown HRF column scaling '$other' (expected as-convolved or unit-maximum-absolute)"
          )
        )

/** The exact coordinate transform applied to one realized HRF column.
  *
  * If `X' = X / divisor`, fitted coefficients satisfy `beta' = divisor * beta`.
  * Response-level linear weights therefore divide by the same value.
  */
final case class HrfColumnScale private[design] (
    policy: HrfColumnScaling,
    divisor: Double
):
  require(divisor.isFinite && divisor > 0.0, "HRF column scaling divisor must be finite and positive")

  def transportLinearWeight(value: Double): Double =
    value / divisor

  def canonical: String =
    s"${policy.label}:${java.lang.Double.doubleToLongBits(divisor)}"

object HrfColumnScale:
  val identity: HrfColumnScale = HrfColumnScale(HrfColumnScaling.AsConvolved, 1.0)

  private[design] def applied(policy: HrfColumnScaling, divisor: Double): HrfColumnScale =
    HrfColumnScale(policy, divisor)
