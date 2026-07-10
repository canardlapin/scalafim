package scalafim.fmri.design

enum DesignError:
  case InvalidId(kind: String, value: String, reason: String)
  case InvalidSchedule(detail: String)
  case MissingColumn(name: String)
  case UnknownTable(name: String)
  case InvalidColumnType(name: String, expected: String, actual: String)
  case UnknownBasis(name: String)
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

private def validateDesignId(kind: String, value: String, allowDot: Boolean): Either[DesignError, String] =
  val trimmed = value.trim
  if trimmed.isEmpty then Left(DesignError.InvalidId(kind, value, "must be non-empty"))
  else Right(Names.sanitize(trimmed, allowDot = allowDot))

private def validateOneBasedIndex(kind: String, value: Int): Either[DesignError, Int] =
  if value >= 1 then Right(value)
  else Left(DesignError.InvalidId(kind, value.toString, "must be >= 1"))

opaque type DesignColumnIndex = Int

object DesignColumnIndex:
  def fromOneBased(value: Int): Either[DesignError, DesignColumnIndex] =
    validateOneBasedIndex("design column index", value)

  def fromZeroBased(value: Int): Either[DesignError, DesignColumnIndex] =
    fromOneBased(value + 1)

  inline def unsafeOneBased(value: Int): DesignColumnIndex = value

  extension (index: DesignColumnIndex)
    inline def oneBased: Int = index
    inline def zeroBased: Int = index - 1

opaque type ScanIndex = Int

object ScanIndex:
  def fromOneBased(value: Int): Either[DesignError, ScanIndex] =
    validateOneBasedIndex("scan index", value)

  def fromZeroBased(value: Int): Either[DesignError, ScanIndex] =
    fromOneBased(value + 1)

  inline def unsafeOneBased(value: Int): ScanIndex = value

  extension (index: ScanIndex)
    inline def oneBased: Int = index
    inline def zeroBased: Int = index - 1

opaque type RunIndex = Int

object RunIndex:
  def fromOneBased(value: Int): Either[DesignError, RunIndex] =
    validateOneBasedIndex("run index", value)

  def fromZeroBased(value: Int): Either[DesignError, RunIndex] =
    fromOneBased(value + 1)

  inline def unsafeOneBased(value: Int): RunIndex = value

  extension (index: RunIndex)
    inline def oneBased: Int = index
    inline def zeroBased: Int = index - 1

opaque type BasisIndex = Int

object BasisIndex:
  def fromOneBased(value: Int): Either[DesignError, BasisIndex] =
    validateOneBasedIndex("basis index", value)

  def fromZeroBased(value: Int): Either[DesignError, BasisIndex] =
    fromOneBased(value + 1)

  inline def unsafeOneBased(value: Int): BasisIndex = value

  extension (index: BasisIndex)
    inline def oneBased: Int = index
    inline def zeroBased: Int = index - 1

opaque type TermIndex = Int

object TermIndex:
  def fromOneBased(value: Int): Either[DesignError, TermIndex] =
    validateOneBasedIndex("term index", value)

  def fromZeroBased(value: Int): Either[DesignError, TermIndex] =
    fromOneBased(value + 1)

  inline def unsafeOneBased(value: Int): TermIndex = value

  extension (index: TermIndex)
    inline def oneBased: Int = index
    inline def zeroBased: Int = index - 1

opaque type EventId = String

object EventId:
  def apply(value: String): Either[DesignError, EventId] =
    validateDesignId("event", value, allowDot = false)

  inline def unsafe(value: String): EventId = value

  extension (id: EventId)
    inline def value: String = id

opaque type ConditionId = String

object ConditionId:
  def apply(value: String): Either[DesignError, ConditionId] =
    validateDesignId("condition", value, allowDot = true)

  inline def unsafe(value: String): ConditionId = value

  extension (id: ConditionId)
    inline def value: String = id

opaque type FactorId = String

object FactorId:
  def apply(value: String): Either[DesignError, FactorId] =
    validateDesignId("factor", value, allowDot = true)

  inline def unsafe(value: String): FactorId = value

  extension (id: FactorId)
    inline def value: String = id

opaque type TermId = String

object TermId:
  def apply(value: String): Either[DesignError, TermId] =
    validateDesignId("term", value, allowDot = false)

  inline def unsafe(value: String): TermId = value

  extension (id: TermId)
    inline def value: String = id

opaque type ColumnId = String

object ColumnId:
  def apply(value: String): Either[DesignError, ColumnId] =
    validateDesignId("column", value, allowDot = true)

  inline def unsafe(value: String): ColumnId = value

  extension (id: ColumnId)
    inline def value: String = id
