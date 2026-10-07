package scalafim.fmri.hrf

enum HrfConstructorError:
  case InvalidParameter(name: String, value: Double)
  case InvalidBasisCount(value: Int)
  case InvalidCalibration(error: HrfNormalizationError)
  case NonFiniteCalibrationValue(name: String, sample: Int, basis: Int, value: Double)
  case NonFiniteCalibrationScale(name: String, basis: Int, value: Double)

  def message: String = this match
    case InvalidParameter(name, value) => s"Invalid HRF constructor parameter '$name': $value"
    case InvalidBasisCount(value) => s"nbasis must be >= 1, got $value"
    case InvalidCalibration(error) => error.message
    case NonFiniteCalibrationValue(name, sample, basis, value) =>
      s"HRF '$name' has non-finite calibration value $value at sample $sample, basis $basis"
    case NonFiniteCalibrationScale(name, basis, value) =>
      s"HRF '$name' has non-finite calibration scale $value for basis $basis"

/** Portable admission for the fixed grids used by built-in constructors.
  * Ceil endpoints are retained, including the first sample beyond a nonmultiple
  * span. The normalization policy limits grids to one million samples and ten
  * million scalar work units. LWU charges its two Gaussian terms; Daguerre
  * charges array initialization, recurrence updates and the peak scan. These
  * counts bound known loops and allocations, rather than elapsed time.
  */
object HrfConstructorCalibration:
  val LwuStep: Seconds = 0.005.s
  val DaguerreStep: Seconds = 0.1.s

  private def spanValid(name: String, span: Seconds): Either[HrfConstructorError, Unit] =
    if !span.value.isFinite || span.value < 0.0 then
      Left(HrfConstructorError.InvalidCalibration(HrfNormalizationError.InvalidReferenceSpan(name, span)))
    else Right(())

  private def finite(name: String, value: Double): Either[HrfConstructorError, Unit] =
    if value.isFinite then Right(()) else Left(HrfConstructorError.InvalidParameter(name, value))

  private def positive(name: String, value: Double): Either[HrfConstructorError, Unit] =
    if value.isFinite && value > 0.0 then Right(())
    else Left(HrfConstructorError.InvalidParameter(name, value))

  def lwu(
      tau: Double,
      sigma: Double,
      rho: Double,
      normalize: HrfFunctions.LwuNormalize,
      span: Seconds
  ): Either[HrfConstructorError, Int] =
    for
      _ <- spanValid("lwu", span)
      _ <- finite("tau", tau)
      _ <- positive("sigma", sigma)
      _ <- finite("rho", rho)
      _ <- finite("LWU second center", tau + 2.0 * sigma)
      _ <- finite("LWU second width", 1.6 * sigma)
      count <- normalize match
        case HrfFunctions.LwuNormalize.None => Right(0)
        case _ => NormalizationReferenceGrid.stepped("lwu", span, LwuStep, 2.0)
          .left.map(HrfConstructorError.InvalidCalibration.apply)
    yield count

  def daguerre(nBasis: Int, scale: Double, span: Seconds): Either[HrfConstructorError, Int] =
    for
      _ <- spanValid("daguerre", span)
      _ <- if nBasis >= 1 then Right(()) else Left(HrfConstructorError.InvalidBasisCount(nBasis))
      _ <- positive("scale", scale)
      count <- NormalizationReferenceGrid.stepped("daguerre", span, DaguerreStep,
        2.0 * nBasis.toDouble + math.max(0.0, nBasis.toDouble - 2.0))
        .left.map(HrfConstructorError.InvalidCalibration.apply)
      _ <- finite("Daguerre final scaled lag", (count - 1).toDouble * DaguerreStep.value / scale)
    yield count
