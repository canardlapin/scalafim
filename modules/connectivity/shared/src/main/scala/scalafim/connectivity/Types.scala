package scalafim.connectivity

private[connectivity] object ConnectivityIdentifier:
  def validate(kind: String, value: String): Either[ConnectivityError, String] =
    val trimmed = value.trim
    if trimmed.isEmpty then Left(ConnectivityError.InvalidId(kind, value, "must be non-empty"))
    else if !trimmed.forall(isAllowed) then
      Left(ConnectivityError.InvalidId(kind, value, "may contain only letters, digits, '.', '_', and '-'"))
    else Right(trimmed)

  private def isAllowed(ch: Char): Boolean =
    ch.isLetterOrDigit || ch == '.' || ch == '_' || ch == '-'

opaque type NodeId = String

object NodeId:
  def apply(value: String): Either[ConnectivityError, NodeId] =
    ConnectivityIdentifier.validate("node", value)

  def unsafe(value: String): NodeId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: NodeId)
    inline def value: String = id

opaque type SystemId = String

object SystemId:
  def apply(value: String): Either[ConnectivityError, SystemId] =
    ConnectivityIdentifier.validate("system", value)

  def unsafe(value: String): SystemId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: SystemId)
    inline def value: String = id

opaque type RunId = String

object RunId:
  def apply(value: String): Either[ConnectivityError, RunId] =
    ConnectivityIdentifier.validate("run", value)

  def unsafe(value: String): RunId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: RunId)
    inline def value: String = id

opaque type SubjectId = String

object SubjectId:
  def apply(value: String): Either[ConnectivityError, SubjectId] =
    ConnectivityIdentifier.validate("subject", value)

  def unsafe(value: String): SubjectId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: SubjectId)
    inline def value: String = id

opaque type CovariateName = String

object CovariateName:
  def apply(value: String): Either[ConnectivityError, CovariateName] =
    ConnectivityIdentifier.validate("covariate", value)

  def unsafe(value: String): CovariateName =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (name: CovariateName)
    inline def value: String = name

opaque type SampleIndex = Int

object SampleIndex:
  def apply(value: Int): Either[ConnectivityError, SampleIndex] =
    if value >= 0 then Right(value)
    else Left(ConnectivityError.InvalidDimension("sample index", value))

  def unsafe(value: Int): SampleIndex =
    require(value >= 0, "sample index must be non-negative")
    value

  extension (index: SampleIndex)
    inline def value: Int = index

opaque type SamplePeriod = Double

object SamplePeriod:
  def fromSeconds(value: Double): Either[ConnectivityError, SamplePeriod] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(ConnectivityError.InvalidScalar("sample period", value, "must be finite and positive"))

  def fromHz(hz: Double): Either[ConnectivityError, SamplePeriod] =
    if hz.isFinite && hz > 0.0 then Right(1.0 / hz)
    else Left(ConnectivityError.InvalidScalar("sample rate", hz, "must be finite and positive"))

  def unsafeSeconds(value: Double): SamplePeriod =
    fromSeconds(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (period: SamplePeriod)
    inline def seconds: Double = period
    inline def hz: Double = 1.0 / period
