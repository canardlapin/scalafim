package scalafim.response

final class ReconstructionErrorBounds private (
    val absolute: Double,
    val relative: Double
):
  def agrees(left: Double, right: Double): Boolean =
    if DecodeConsistency.ExactBits.agrees(left, right) then true
    else if !left.isFinite || !right.isFinite then false
    else
      val difference = math.abs(left - right)
      difference <= absolute + relative * math.max(math.abs(left), math.abs(right))

  override def equals(other: Any): Boolean =
    other match
      case that: ReconstructionErrorBounds =>
        java.lang.Double.doubleToRawLongBits(absolute) ==
          java.lang.Double.doubleToRawLongBits(that.absolute) &&
          java.lang.Double.doubleToRawLongBits(relative) ==
            java.lang.Double.doubleToRawLongBits(that.relative)
      case _ =>
        false

  override def hashCode(): Int =
    31 * java.lang.Double.hashCode(absolute) +
      java.lang.Double.hashCode(relative)

object ReconstructionErrorBounds:
  def make(
      absolute: Double,
      relative: Double
  ): Either[ConsistencyError, ReconstructionErrorBounds] =
    if !absolute.isFinite || absolute < 0.0 then
      Left(ConsistencyError.InvalidTolerance("reconstruction absolute", absolute))
    else if !relative.isFinite || relative < 0.0 then
      Left(ConsistencyError.InvalidTolerance("reconstruction relative", relative))
    else Right(new ReconstructionErrorBounds(absolute, relative))

  def unsafe(
      absolute: Double,
      relative: Double
  ): ReconstructionErrorBounds =
    make(absolute, relative)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

opaque type ScientificProfileId = String

object ScientificProfileId:
  def fromString(value: String): Either[IdentityError, ScientificProfileId] =
    ResponseIdentity.validate("scientific profile id", value)

  def unsafe(value: String): ScientificProfileId =
    fromString(value)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ScientificProfileId)
    inline def value: String =
      id

opaque type ValidationReportRef = String

object ValidationReportRef:
  def fromString(value: String): Either[IdentityError, ValidationReportRef] =
    ResponseIdentity.validate("validation report reference", value)

  def unsafe(value: String): ValidationReportRef =
    fromString(value)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (reference: ValidationReportRef)
    inline def value: String =
      reference

enum ReconstructionContract:
  case Exact
  case DeterministicBounded(bounds: ReconstructionErrorBounds)
  case ValidatedScientific(
      profile: ScientificProfileId,
      report: ValidationReportRef
  )
