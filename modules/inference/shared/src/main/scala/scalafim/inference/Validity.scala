package scalafim.inference

enum ValidityClaim:
  case Exact
  case Conditional
  case Asymptotic
  case Heuristic

final case class DeclaredAssumption private (
    id: AssumptionId,
    statement: String
)

object DeclaredAssumption:
  def from(id: AssumptionId, statement: String): Either[InferenceError, DeclaredAssumption] =
    if statement.nonEmpty && statement == statement.trim then
      Right(DeclaredAssumption(id, statement))
    else Left(InferenceError.InvalidDescription("assumption statement", statement))

enum AssumptionOutcome:
  case Passed(detail: String)
  case Failed(detail: String)
  case NotChecked(reason: String)

final case class AssumptionCheck(
    assumption: DeclaredAssumption,
    outcome: AssumptionOutcome
)

final case class ValidityDowngrade private (
    from: ValidityClaim,
    to: ValidityClaim,
    reason: String
)

object ValidityDowngrade:
  def from(
      previous: ValidityClaim,
      next: ValidityClaim,
      reason: String
  ): Either[InferenceError, ValidityDowngrade] =
    if previous == next then
      Left(InferenceError.InvalidValidity("a downgrade must change the claim"))
    else if reason.isEmpty || reason != reason.trim then
      Left(InferenceError.InvalidDescription("validity downgrade reason", reason))
    else Right(ValidityDowngrade(previous, next, reason))

enum ValidityStatus:
  case Undowngraded
  case Downgraded(value: ValidityDowngrade)

final case class ValidityReport(
    claim: ValidityClaim,
    assumptions: Vector[DeclaredAssumption],
    checks: Vector[AssumptionCheck],
    status: ValidityStatus
)

enum UnavailableReason:
  case Unsupported(detail: String)
  case InsufficientRank(expected: Int, actual: Int)
  case FailedAssumption(id: AssumptionId, detail: String)
  case ReplicateFailures(count: Int)

enum Evidence[+A]:
  case NotRequested
  case Unavailable(reason: UnavailableReason)
  case Computed(value: A)
