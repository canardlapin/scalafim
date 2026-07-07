package scalafim.pipeline

private[pipeline] object Identifier:
  def validate(kind: String, value: String): Either[PipelineError, String] =
    val trimmed = value.trim
    if trimmed.isEmpty then Left(PipelineError.InvalidId(kind, value, "must be non-empty"))
    else if !trimmed.forall(isAllowed) then
      Left(PipelineError.InvalidId(kind, value, "may contain only letters, digits, '.', '_', and '-'"))
    else Right(trimmed)

  private def isAllowed(ch: Char): Boolean =
    ch.isLetterOrDigit || ch == '.' || ch == '_' || ch == '-'

opaque type PipelineId = String

object PipelineId:
  def apply(value: String): Either[PipelineError, PipelineId] =
    Identifier.validate("pipeline id", value)

  def unsafe(value: String): PipelineId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: PipelineId)
    inline def value: String = id

opaque type NodeId = String

object NodeId:
  def apply(value: String): Either[PipelineError, NodeId] =
    Identifier.validate("node id", value)

  def unsafe(value: String): NodeId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: NodeId)
    inline def value: String = id

opaque type StepId = String

object StepId:
  def apply(value: String): Either[PipelineError, StepId] =
    Identifier.validate("step id", value)

  def unsafe(value: String): StepId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: StepId)
    inline def value: String = id

opaque type PortName = String

object PortName:
  def apply(value: String): Either[PipelineError, PortName] =
    Identifier.validate("port name", value)

  def unsafe(value: String): PortName =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (name: PortName)
    inline def value: String = name
