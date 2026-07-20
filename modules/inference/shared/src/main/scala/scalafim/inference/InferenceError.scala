package scalafim.inference

enum InferenceError:
  case InvalidIdentifier(role: String, value: String)
  case InvalidDescription(role: String, value: String)
  case InvalidCount(role: String, value: Int)
  case InvalidNonNegativeCount(role: String, value: Int)
  case InvalidProbability(role: String, value: Double, inclusiveZero: Boolean)
  case InvalidTolerance(role: String, value: Double)
  case InvalidComponent(value: Int)
  case EmptyComponentSet(role: String)
  case DuplicateComponent(value: Int)
  case UnorderedComponents(previous: Int, next: Int)
  case ComponentOutOfRange(value: Int, rank: Int)
  case InvalidUnit(detail: String)
  case InvalidPartition(detail: String)
  case RowCountMismatch(role: String, expected: Int, actual: Int)
  case InvalidSpectrum(detail: String)
  case InvalidReplicatePlan(detail: String)
  case NumericalFailure(role: String, detail: String)
  case InvalidValidity(detail: String)
  case UnsupportedProblem(detail: String)
  case FailedAssumption(id: AssumptionId, detail: String)
  case NonFiniteStatistic(role: String, value: Double)
  case RankLoss(expected: Int, actual: Int)
  case UnitBeyondRank(unit: UnitId, rank: Int)
  case ReplicateFailure(replicate: ReplicateId, detail: String)
  case BudgetExhausted(consumed: Int, allocated: Int)
  case UnsupportedEvidence(detail: String)

  def message: String =
    this match
      case InvalidIdentifier(role, value) =>
        s"$role must be non-empty and trimmed, got '$value'"
      case InvalidDescription(role, value) =>
        s"$role must be non-empty and trimmed, got '$value'"
      case InvalidCount(role, value) =>
        s"$role must be positive, got $value"
      case InvalidNonNegativeCount(role, value) =>
        s"$role must be non-negative, got $value"
      case InvalidProbability(role, value, inclusiveZero) =>
        val interval = if inclusiveZero then "[0, 1]" else "(0, 1)"
        s"$role must be finite and in $interval, got $value"
      case InvalidTolerance(role, value) =>
        s"$role must be finite and non-negative, got $value"
      case InvalidComponent(value) =>
        s"component index must be non-negative, got $value"
      case EmptyComponentSet(role) =>
        s"$role must contain at least one component"
      case DuplicateComponent(value) =>
        s"component set contains duplicate index $value"
      case UnorderedComponents(previous, next) =>
        s"component set must be strictly increasing, got $previous before $next"
      case ComponentOutOfRange(value, rank) =>
        s"component index $value is outside fitted rank $rank"
      case InvalidUnit(detail) =>
        s"invalid latent unit: $detail"
      case InvalidPartition(detail) =>
        s"invalid row partition: $detail"
      case RowCountMismatch(role, expected, actual) =>
        s"$role expected $expected rows, got $actual"
      case InvalidSpectrum(detail) =>
        s"invalid ordered spectrum: $detail"
      case InvalidReplicatePlan(detail) =>
        s"invalid replicate plan: $detail"
      case NumericalFailure(role, detail) =>
        s"$role failed: $detail"
      case InvalidValidity(detail) =>
        s"invalid validity declaration: $detail"
      case UnsupportedProblem(detail) =>
        s"unsupported inference problem: $detail"
      case FailedAssumption(id, detail) =>
        s"assumption ${id.value} failed: $detail"
      case NonFiniteStatistic(role, value) =>
        s"$role must be finite, got $value"
      case RankLoss(expected, actual) =>
        s"replicate rank dropped from $expected to $actual"
      case UnitBeyondRank(unit, rank) =>
        s"unit ${unit.value} is beyond fitted rank $rank"
      case ReplicateFailure(replicate, detail) =>
        s"replicate ${replicate.value} failed: $detail"
      case BudgetExhausted(consumed, allocated) =>
        s"Monte Carlo budget exhausted after $consumed of $allocated draws"
      case UnsupportedEvidence(detail) =>
        s"requested evidence is unavailable: $detail"
