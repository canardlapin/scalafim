package scalafim.fmri.hrf

enum HrfSpecError:
  case UnknownKind(name: String, available: Vector[String])
  case UnsupportedKind(kind: HrfKind, detail: String)
  case InvalidBasisCount(value: Int)
  case InvalidSpan(error: TimeError)
  case InvalidWidth(error: TimeError)
  case InvalidPrecision(error: TimeError)
  case InvalidLag(error: TimeError)
  case InvalidNormalization(error: HrfNormalizationError)
  case ExpectedScalar(name: String, nbasis: Int)

  def message: String =
    this match
      case UnknownKind(name, available) =>
        s"Unknown HRF kind '$name' (available: ${available.mkString(", ")})"
      case UnsupportedKind(kind, detail) =>
        s"HRF kind '${kind.canonicalName}' cannot be built from HrfSpec: $detail"
      case InvalidBasisCount(value) =>
        s"nbasis must be >= 1, got $value"
      case InvalidSpan(error) =>
        error.message
      case InvalidWidth(error) =>
        error.message
      case InvalidPrecision(error) =>
        error.message
      case InvalidLag(error) =>
        error.message
      case InvalidNormalization(error) =>
        error.message
      case ExpectedScalar(name, nbasis) =>
        s"HRF '$name' must have exactly one basis column for scalar evaluation, got $nbasis"

enum HrfKind(val canonicalName: String, val aliases: Vector[String]):
  case Spmg1 extends HrfKind("spmg1", Vector("spmg1", "spmg"))
  case Spmg2 extends HrfKind("spmg2", Vector("spmg2"))
  case Spmg3 extends HrfKind("spmg3", Vector("spmg3"))
  case Gamma extends HrfKind("gamma", Vector("gamma", "gam"))
  case Gaussian extends HrfKind("gaussian", Vector("gaussian"))
  case Lwu extends HrfKind("lwu", Vector("lwu"))
  case Cascade34 extends HrfKind("cascade34", Vector("cascade34"))
  case Mexhat extends HrfKind("mexhat", Vector("mexhat"))
  case InvLogit extends HrfKind("inv_logit", Vector("inv_logit", "invlogit"))
  case HalfCosine extends HrfKind("half_cosine", Vector("half_cosine", "halfcosine"))
  case Fir extends HrfKind("fir", Vector("fir"))
  case Bspline extends HrfKind("bspline", Vector("bspline", "bs"))
  case Tent extends HrfKind("tent", Vector("tent"))
  case Fourier extends HrfKind("fourier", Vector("fourier"))
  case Daguerre extends HrfKind("daguerre", Vector("daguerre"))
  case Sine extends HrfKind("sine", Vector("sine"))
  case Boxcar extends HrfKind("boxcar", Vector("boxcar"))
  case Weighted extends HrfKind("weighted", Vector("weighted"))

  def isScalarByDefault: Boolean =
    this match
      case Spmg1 | Gamma | Gaussian | Lwu | Cascade34 | Mexhat | InvLogit | HalfCosine | Boxcar | Weighted => true
      case Spmg2 | Spmg3 | Fir | Bspline | Tent | Fourier | Daguerre | Sine => false

object HrfKind:
  val all: Vector[HrfKind] = HrfKind.values.toVector

  def availableNames: Vector[String] =
    all.map(_.canonicalName)

  def fromString(name: String): Either[HrfSpecError, HrfKind] =
    val normalized = name.trim.toLowerCase
    all.find(kind => kind.aliases.exists(_.toLowerCase == normalized)) match
      case Some(kind) => Right(kind)
      case None       => Left(HrfSpecError.UnknownKind(name, availableNames))

final case class HrfSpec private (
    kind: HrfKind,
    nbasis: Int,
    span: PositiveSeconds,
    lag: Seconds,
    width: NonNegativeSeconds,
    precision: PositiveSeconds,
    summate: Boolean,
    normalize: Boolean,
    normalization: HrfNormalization
):
  def basis: BasisCount =
    BasisCount.unsafe(nbasis)

  def toHrf: Either[HrfSpecError, Hrf] =
    build(applySpecSpan = true)

  private[hrf] def toLegacyHrf: Either[HrfSpecError, Hrf] =
    build(applySpecSpan = false)

  private def build(applySpecSpan: Boolean): Either[HrfSpecError, Hrf] =
    val base =
      kind match
        case HrfKind.Spmg1     => Right(Hrfs.spmg1(span = span.seconds))
        case HrfKind.Spmg2     => Right(Hrfs.SPMG2)
        case HrfKind.Spmg3     => Right(Hrfs.SPMG3)
        case HrfKind.Gamma     => Right(Hrfs.gamma(span = span.seconds))
        case HrfKind.Gaussian  => Right(Hrfs.gaussian(span = span.seconds))
        case HrfKind.Lwu       => Right(Hrfs.lwu(span = span.seconds))
        case HrfKind.Cascade34 => Right(Hrfs.cascade34(span = span.seconds))
        case HrfKind.Mexhat    => Right(Hrfs.mexhat(span = span.seconds))
        case HrfKind.InvLogit  => Right(Hrfs.invLogit(span = span.seconds))
        case HrfKind.HalfCosine => Right(Hrfs.halfCosine())
        case HrfKind.Fir       => Right(Hrfs.fir(nBasis = nbasis, span = span.seconds))
        case HrfKind.Bspline   => Right(Hrfs.bspline(nBasis = nbasis, span = span.seconds))
        case HrfKind.Tent      => Right(Hrfs.tent(nBasis = nbasis, span = span.seconds))
        case HrfKind.Fourier   => Right(Hrfs.fourier(nBasis = nbasis, span = span.seconds))
        case HrfKind.Daguerre  => Right(Hrfs.daguerre(nBasis = nbasis, span = span.seconds))
        case HrfKind.Sine      => Right(Hrfs.sine(nBasis = nbasis, span = span.seconds))
        case HrfKind.Boxcar =>
          Left(HrfSpecError.UnsupportedKind(kind, "boxcar requires an explicit width parameter"))
        case HrfKind.Weighted =>
          Left(HrfSpecError.UnsupportedKind(kind, "weighted HRFs require explicit times and weights"))

    base.flatMap { hrf =>
      HrfCombinators
        .genEither(
          base = hrf,
          lag = lag,
          width = width.seconds,
          precision = precision.seconds,
          halfLife = Double.PositiveInfinity,
          summate = summate,
          normalize = normalize,
          name = None,
          normalization = normalization,
          span = if applySpecSpan then Some(span.seconds) else None
        )
        .left
        .map(HrfSpecError.InvalidNormalization.apply)
    }

  def toScalarHrf: Either[HrfSpecError, ScalarHrf] =
    toHrf.flatMap(ScalarHrf.from)

object HrfSpec:
  def apply(
      kind: HrfKind,
      nbasis: Int = 1,
      span: Seconds = 24.0.s,
      lag: Seconds = 0.0.s,
      width: Seconds = 0.0.s,
      precision: Seconds = 0.1.s,
      summate: Boolean = true,
      normalize: Boolean = false,
      normalization: HrfNormalization = HrfNormalization.None
  ): Either[HrfSpecError, HrfSpec] =
    if normalize && normalization != HrfNormalization.None then
      Left(HrfSpecError.InvalidNormalization(HrfNormalizationError.ConflictingModes))
    else
      BasisCount.fromInt(nbasis).left.map(_ => HrfSpecError.InvalidBasisCount(nbasis)).flatMap { basis =>
        for
          span0 <- PositiveSeconds.fromSeconds(span, "span").left.map(HrfSpecError.InvalidSpan.apply)
          lag0 <- Seconds.fromDouble(lag.value, "lag").left.map(HrfSpecError.InvalidLag.apply)
          width0 <- NonNegativeSeconds.fromSeconds(width, "width").left.map(HrfSpecError.InvalidWidth.apply)
          precision0 <- PositiveSeconds.fromSeconds(precision, "precision").left.map(HrfSpecError.InvalidPrecision.apply)
        yield HrfSpec(kind, basis.value, span0, lag0, width0, precision0, summate, normalize, normalization)
      }

  def fromName(
      name: String,
      nbasis: Int = 1,
      span: Seconds = 24.0.s,
      lag: Seconds = 0.0.s,
      width: Seconds = 0.0.s,
      precision: Seconds = 0.1.s,
      summate: Boolean = true,
      normalize: Boolean = false,
      normalization: HrfNormalization = HrfNormalization.None
  ): Either[HrfSpecError, HrfSpec] =
    HrfKind.fromString(name).flatMap { kind =>
      HrfSpec(
        kind = kind,
        nbasis = nbasis,
        span = span,
        lag = lag,
        width = width,
        precision = precision,
        summate = summate,
        normalize = normalize,
        normalization = normalization
      )
    }
