package scalafim.fmri.design

enum DesignError:
  case InvalidId(kind: String, value: String, reason: String)
  case InvalidSchedule(detail: String)
  case ResponseSupportRejected(receipt: EventSupportReceipt)
  case MissingColumn(name: String)
  case UnknownTable(name: String)
  case InvalidColumnType(name: String, expected: String, actual: String)
  case UnknownBasis(name: String)
  case UnknownBasisFunction(name: String, known: Vector[String])
  case DegenerateBasis(detail: String)
  case UnknownContrast(name: String, known: Vector[String])
  case InvalidSubset(detail: String)
  case InvalidHrfFun(term: String, detail: String)
  case InvalidHrfAssignment(term: String, missing: Vector[String], extra: Vector[String])
  case InvalidPhaseHrfAssignment(missing: Vector[PhaseId], extra: Vector[PhaseId])
  case MissingModulatorValue(term: String, column: String, eventIndex: Int, policy: String)
  case InvalidOrthogonalization(term: String, detail: String)
  case DegenerateModulator(term: String, modulator: ModulatorId, scope: String, policy: DegenerateModulatorPolicy)
  case UnknownFactorLevel(factor: String, observed: String, declared: Vector[String])
  case IncompatibleFactorSchema(
      scope: String,
      source: String,
      factor: String,
      expected: Vector[String],
      observed: Vector[String]
  )
  case EmptyFactorCell(term: String, cell: String, policy: String)
  case EmptyFactorCellInRun(term: String, cell: CellKey, run: RunIndex, policy: EmptyCellPolicy)
  case UnsupportedContrastTarget(term: String, found: String)
  case FormulaParse(detail: String, pos: Int)
  case FormulaBinding(detail: String)
  case InvalidSchema(detail: String)
  case BuildFailed(detail: String)

  def message: String =
    this match
      case InvalidId(kind, value, reason) =>
        s"invalid $kind id '$value': $reason"
      case InvalidSchedule(detail) =>
        detail
      case ResponseSupportRejected(receipt) =>
        val rows = receipt.decisions.filter(_.disposition == EventSupportDisposition.Rejected)
        s"Response-support policy ${receipt.request.policy} rejected source rows ${rows.map(_.sourceRow + 1).mkString(", ")} in ${receipt.term.getOrElse("term")}"
      case MissingColumn(name) =>
        s"Unknown column: '$name'"
      case UnknownTable(name) =>
        s"Unknown data table: '$name'"
      case InvalidColumnType(name, expected, actual) =>
        s"Column '$name' is not $expected: $actual"
      case UnknownBasis(name) =>
        s"Unknown HRF basis: '$name'"
      case UnknownBasisFunction(name, known) =>
        val suffix = if known.isEmpty then "" else s" (known: ${known.mkString(", ")})"
        s"Unknown basis call '$name' in formula$suffix"
      case DegenerateBasis(detail) =>
        detail
      case UnknownContrast(name, known) =>
        val suffix = if known.isEmpty then "" else s" (known: ${known.mkString(", ")})"
        s"Unknown contrast set '$name'$suffix"
      case InvalidSubset(detail) =>
        s"Invalid subset expression: $detail"
      case InvalidHrfFun(term, detail) =>
        s"Invalid hrf_fun for term '$term': $detail"
      case InvalidHrfAssignment(term, missing, extra) =>
        val missingText = if missing.isEmpty then "none" else missing.mkString(", ")
        val extraText = if extra.isEmpty then "none" else extra.mkString(", ")
        s"Invalid HRF-by-cell assignment for term '$term' (missing: $missingText; extra: $extraText)"
      case InvalidPhaseHrfAssignment(missing, extra) =>
        val missingText = if missing.isEmpty then "none" else missing.map(_.value).mkString(", ")
        val extraText = if extra.isEmpty then "none" else extra.map(_.value).mkString(", ")
        s"Invalid HRF-by-phase assignment (missing: $missingText; extra: $extraText)"
      case MissingModulatorValue(term, column, eventIndex, policy) =>
        s"Non-finite modulator '$column' at event ${eventIndex + 1} in term '$term' cannot be handled by policy '$policy'"
      case InvalidOrthogonalization(term, detail) =>
        s"Invalid ordered orthogonalization for term '$term': $detail"
      case DegenerateModulator(term, modulator, scope, policy) =>
        s"Degenerate modulator '${modulator.value}' in term '$term' and scope '$scope' is rejected by policy '${policy.label}'"
      case UnknownFactorLevel(factor, observed, declared) =>
        s"factor '$factor' observed unknown level '$observed' (declared: ${declared.mkString(", ")})"
      case IncompatibleFactorSchema(scope, source, factor, expected, observed) =>
        s"incompatible $scope factor schema from '$source' for '$factor' (expected: ${expected.mkString(", ")}; observed: ${observed.mkString(", ")})"
      case EmptyFactorCell(term, cell, policy) =>
        s"term '$term' contains empty factor cell '$cell', rejected by policy '$policy'"
      case EmptyFactorCellInRun(term, cell, run, policy) =>
        s"term '$term' contains empty factor cell '${cell.canonical}' in run ${run.oneBased}, rejected by policy '${policy.label}'"
      case UnsupportedContrastTarget(term, found) =>
        s"Term '$term' does not support contrasts (found $found)"
      case FormulaParse(detail, pos) =>
        s"$detail (at char $pos)"
      case FormulaBinding(detail) =>
        detail
      case InvalidSchema(detail) =>
        s"Invalid design schema: $detail"
      case BuildFailed(detail) =>
        detail

object DesignError:
  def fromThrowable(t: Throwable): DesignError =
    val msg = Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.toString)
    BuildFailed(msg)

/** Parse a design identifier.
  *
  * Parsing is total on valid input, injective, and rejecting: an accepted value
  * is returned unchanged, so `Id(s).map(_.value) == Right(s)` for every `s` this
  * accepts. An id is a lookup key, and rewriting a key means it may no longer
  * name the thing the caller named.
  *
  * Making a *generated* name R-safe is the opposite job — lossy by design — and
  * lives in [[Names]], applied where output names are produced.
  */
private def validateDesignId(kind: String, value: String): Either[DesignError, String] =
  def reject(reason: String): Either[DesignError, String] =
    Left(DesignError.InvalidId(kind, value, reason))

  if value.trim.isEmpty then reject("must be non-empty")
  else if value.exists(_.isControl) then reject("must not contain control characters")
  else if value.trim != value then reject("must not have leading or trailing whitespace")
  else Right(value)

private def validateOneBasedIndex(kind: String, value: Int): Either[DesignError, Int] =
  if value >= 1 then Right(value)
  else Left(DesignError.InvalidId(kind, value.toString, "must be >= 1"))

/** A design identifier, distinguished from other kinds by a phantom `Tag`.
  *
  * `DesignId[IdTag.Event]` and `DesignId[IdTag.Term]` are different types and
  * cannot be interchanged; only the *definition* is shared. See
  * [[validateDesignId]] for what parsing one means.
  */
opaque type DesignId[Tag] = String

object DesignId:
  def parse[Tag](kind: String, value: String): Either[DesignError, DesignId[Tag]] =
    validateDesignId(kind, value)

  inline def unsafe[Tag](value: String): DesignId[Tag] = value

  extension [Tag](id: DesignId[Tag])
    inline def value: String = id

/** A one-based position, distinguished from other kinds by a phantom `Tag`. */
opaque type OneBasedIndex[Tag] = Int

object OneBasedIndex:
  def fromOneBased[Tag](kind: String, value: Int): Either[DesignError, OneBasedIndex[Tag]] =
    validateOneBasedIndex(kind, value)

  inline def unsafe[Tag](value: Int): OneBasedIndex[Tag] = value

  extension [Tag](index: OneBasedIndex[Tag])
    inline def oneBased: Int = index
    inline def zeroBased: Int = index - 1

/** The companion each concrete id type is; `kind` is what its errors call it. */
sealed abstract class IdCompanion[Tag](kind: String):
  def apply(value: String): Either[DesignError, DesignId[Tag]] =
    DesignId.parse(kind, value)

  def unsafe(value: String): DesignId[Tag] =
    DesignId.unsafe(value)

sealed abstract class IndexCompanion[Tag](kind: String):
  def fromOneBased(value: Int): Either[DesignError, OneBasedIndex[Tag]] =
    OneBasedIndex.fromOneBased(kind, value)

  def fromZeroBased(value: Int): Either[DesignError, OneBasedIndex[Tag]] =
    fromOneBased(value + 1)

  def unsafeOneBased(value: Int): OneBasedIndex[Tag] =
    OneBasedIndex.unsafe(value)

/** Phantom tags. They have no instances; they exist to keep the ids apart. */
object IdTag:
  sealed trait Event
  sealed trait Trial
  sealed trait Phase
  sealed trait Condition
  sealed trait Factor
  sealed trait Modulator
  sealed trait Term
  sealed trait Column

object IndexTag:
  sealed trait DesignColumn
  sealed trait Scan
  sealed trait Run
  sealed trait Basis
  sealed trait Term

type EventId = DesignId[IdTag.Event]
object EventId extends IdCompanion[IdTag.Event]("event")

/** Identity of one conceptual source observation/trial.
  *
  * This is deliberately distinct from [[EventId]]: an event term may lower
  * one trial into several phase-specific event rows, so using the event id as
  * the parent identity would make provenance ambiguous.
  */
type TrialId = DesignId[IdTag.Trial]
object TrialId extends IdCompanion[IdTag.Trial]("trial")

/** Identity of one phase in a multiphase trial definition. */
type PhaseId = DesignId[IdTag.Phase]
object PhaseId extends IdCompanion[IdTag.Phase]("phase")

type ConditionId = DesignId[IdTag.Condition]
object ConditionId extends IdCompanion[IdTag.Condition]("condition")

type FactorId = DesignId[IdTag.Factor]
object FactorId extends IdCompanion[IdTag.Factor]("factor")

type ModulatorId = DesignId[IdTag.Modulator]
object ModulatorId extends IdCompanion[IdTag.Modulator]("modulator")

type TermId = DesignId[IdTag.Term]
object TermId extends IdCompanion[IdTag.Term]("term")

type ColumnId = DesignId[IdTag.Column]
object ColumnId extends IdCompanion[IdTag.Column]("column")

type DesignColumnIndex = OneBasedIndex[IndexTag.DesignColumn]
object DesignColumnIndex extends IndexCompanion[IndexTag.DesignColumn]("design column index")

type ScanIndex = OneBasedIndex[IndexTag.Scan]
object ScanIndex extends IndexCompanion[IndexTag.Scan]("scan index")

type RunIndex = OneBasedIndex[IndexTag.Run]
object RunIndex extends IndexCompanion[IndexTag.Run]("run index")

type BasisIndex = OneBasedIndex[IndexTag.Basis]
object BasisIndex extends IndexCompanion[IndexTag.Basis]("basis index")

type TermIndex = OneBasedIndex[IndexTag.Term]
object TermIndex extends IndexCompanion[IndexTag.Term]("term index")
