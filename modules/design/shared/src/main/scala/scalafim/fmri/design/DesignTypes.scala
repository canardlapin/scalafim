package scalafim.fmri.design

enum DesignError:
  case InvalidId(kind: String, value: String, reason: String)
  case InvalidSchedule(detail: String)
  case MissingColumn(name: String)
  case UnknownBasis(name: String)
  case UnknownContrast(name: String, known: Vector[String])
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
      case UnknownBasis(name) =>
        s"Unknown HRF basis: '$name'"
      case UnknownContrast(name, known) =>
        val suffix = if known.isEmpty then "" else s" (known: ${known.mkString(", ")})"
        s"Unknown contrast set '$name'$suffix"
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
