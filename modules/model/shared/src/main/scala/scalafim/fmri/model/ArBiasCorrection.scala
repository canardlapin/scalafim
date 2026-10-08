package scalafim.fmri.model

/** Largest lag budget admitted for the OLS residual-bias correction. */
opaque type ArCorrectionMaxLag = Int

object ArCorrectionMaxLag:
  def apply(value: Int): Either[ModelError, ArCorrectionMaxLag] =
    if value > 0 then Right(value)
    else Left(ModelError.InvalidParameter("AR correction lag ceiling", "must be positive"))

  def unsafe(value: Int): ArCorrectionMaxLag =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (lag: ArCorrectionMaxLag)
    inline def value: Int = lag

/** The correction is valid for the initial OLS projection only. Corrected
  * estimation therefore requires one estimation pass and estimated coefficients.
  * The budget follows fmrireg's adaptive rule, bounded by the supplied ceiling.
  */
enum ArBiasCorrection:
  case Raw
  case OlsDesign(ceiling: ArCorrectionMaxLag)

object ArBiasCorrection:
  def olsDesign(ceiling: Int = 25): Either[ModelError, ArBiasCorrection] =
    ArCorrectionMaxLag(ceiling).map(OlsDesign.apply)

  val Ols: ArBiasCorrection = OlsDesign(ArCorrectionMaxLag.unsafe(25))
