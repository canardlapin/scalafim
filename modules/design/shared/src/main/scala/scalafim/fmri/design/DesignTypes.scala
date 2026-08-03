package scalafim.fmri.design

enum DesignError:
  case InvalidId(kind: String, value: String, reason: String)
  case InvalidSchedule(detail: String)
  case MissingColumn(name: String)
  case UnknownTable(name: String)
  case InvalidColumnType(name: String, expected: String, actual: String)
  case UnknownBasis(name: String)
  case UnknownBasisFunction(name: String, known: Vector[String])
  case DegenerateBasis(detail: String)
  case UnknownContrast(name: String, known: Vector[String])
  case InvalidSubset(detail: String)
  case InvalidHrfFun(term: String, detail: String)
  case UnsupportedContrastTarget(term: String, found: String)
  case FormulaParse(detail: String, pos: Int)
  case FormulaBinding(detail: String)
  case BuildFailed(detail: String)

  def message: String =
    this match
      case InvalidId(kind, value, reason) =>
        s"invalid $kind id '$value': $reason"
      case InvalidSchedule(detail) =>
        detail
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
      case UnsupportedContrastTarget(term, found) =>
        s"Term '$term' does not support contrasts (found $found)"
      case FormulaParse(detail, pos) =>
        s"$detail (at char $pos)"
      case FormulaBinding(detail) =>
        detail
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
  sealed trait Condition
  sealed trait Factor
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

type ConditionId = DesignId[IdTag.Condition]
object ConditionId extends IdCompanion[IdTag.Condition]("condition")

type FactorId = DesignId[IdTag.Factor]
object FactorId extends IdCompanion[IdTag.Factor]("factor")

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
