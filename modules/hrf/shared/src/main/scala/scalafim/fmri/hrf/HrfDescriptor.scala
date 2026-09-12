package scalafim.fmri.hrf

opaque type BasisCount = Int

object BasisCount:
  def fromInt(value: Int): Either[BasisCountError, BasisCount] =
    if value >= 1 then Right(value)
    else Left(BasisCountError.NonPositive(value))

  def apply(value: Int): BasisCount =
    fromInt(value).fold(err => throw new IllegalArgumentException(err.message), identity)

  inline def unsafe(value: Int): BasisCount =
    value

  val One: BasisCount =
    1

  extension (basis: BasisCount)
    inline def value: Int =
      basis

    inline def isScalar: Boolean =
      basis == 1

enum BasisCountError:
  case NonPositive(value: Int)

  def message: String =
    this match
      case NonPositive(value) => s"nbasis must be >= 1, got $value"

final case class SpmgParams(
    p1: Double = 5.0,
    p2: Double = 15.0,
    a1: Double = 0.0833
)

enum SampledProfileError:
  case TooShort(label: String, minimum: Int, actual: Int)
  case LengthMismatch(left: String, right: String, expected: Int, actual: Int)
  case NegativeStart(label: String, value: Seconds)
  case NonIncreasing(label: String, previous: Seconds, current: Seconds)
  case NonFiniteValue(label: String, index: Int, value: Double)

  def message: String =
    this match
      case TooShort(label, minimum, actual) =>
        s"`$label` must have at least $minimum values, got $actual"
      case LengthMismatch(left, right, expected, actual) =>
        s"`$right` must match `$left` length $expected, got $actual"
      case NegativeStart(label, value) =>
        s"`$label` must start at >= 0, got ${value.value}"
      case NonIncreasing(label, previous, current) =>
        s"`$label` must be strictly increasing, got ${previous.value} then ${current.value}"
      case NonFiniteValue(label, index, value) =>
        s"`$label` value at index ${index + 1} must be finite, got $value"

final case class StrictlyIncreasingTimes private (
    values: Vector[Seconds]
):
  def length: Int =
    values.length

  def head: Seconds =
    values.head

  def last: Seconds =
    values.last

  def toVector: Vector[Seconds] =
    values

object StrictlyIncreasingTimes:
  def make(
      values: Vector[Seconds],
      label: String = "times",
      minimum: Int = 1,
      requireNonNegativeStart: Boolean = false
  ): Either[SampledProfileError, StrictlyIncreasingTimes] =
    if values.length < minimum then Left(SampledProfileError.TooShort(label, minimum, values.length))
    else if requireNonNegativeStart && values.head.value < 0.0 then Left(SampledProfileError.NegativeStart(label, values.head))
    else
      var i = 1
      while i < values.length do
        if values(i).value <= values(i - 1).value then return Left(SampledProfileError.NonIncreasing(label, values(i - 1), values(i)))
        i += 1
      Right(new StrictlyIncreasingTimes(values))

  private[hrf] def unsafe(values: Vector[Seconds]): StrictlyIncreasingTimes =
    new StrictlyIncreasingTimes(values)

final case class WeightedProfile private (
    times: StrictlyIncreasingTimes,
    weights: Vector[Double]
):
  def span: Seconds =
    times.last

object WeightedProfile:
  def fromExplicit(
      weights: Vector[Double],
      times: Vector[Seconds]
  ): Either[SampledProfileError, WeightedProfile] =
    validateWeights(weights).flatMap { ws =>
      if times.length != ws.length then Left(SampledProfileError.LengthMismatch("weights", "times", ws.length, times.length))
      else
        StrictlyIncreasingTimes
          .make(times, label = "times", minimum = 2, requireNonNegativeStart = true)
          .map(ts => new WeightedProfile(ts, ws))
    }

  def fromUniform(
      weights: Vector[Double],
      width: Seconds
  ): Either[SampledProfileError, WeightedProfile] =
    validateWeights(weights).flatMap { ws =>
      PositiveSeconds.fromSeconds(width, "width") match
        case Left(_) => Left(SampledProfileError.NonFiniteValue("width", 0, width.value))
        case Right(width0) =>
          val n = ws.length
          val times = Vector.tabulate(n)(i => Seconds(i.toDouble * width0.value / (n - 1).toDouble))
          StrictlyIncreasingTimes
            .make(times, label = "times", minimum = 2, requireNonNegativeStart = true)
            .map(ts => new WeightedProfile(ts, ws))
    }

  private def validateWeights(weights: Vector[Double]): Either[SampledProfileError, Vector[Double]] =
    if weights.length < 2 then Left(SampledProfileError.TooShort("weights", 2, weights.length))
    else
      var i = 0
      while i < weights.length do
        val value = weights(i)
        if !value.isFinite then return Left(SampledProfileError.NonFiniteValue("weights", i, value))
        i += 1
      Right(weights)

final case class SampledCurve private (
    times: StrictlyIncreasingTimes,
    values: Vector[Double]
):
  def span: Seconds =
    times.last

object SampledCurve:
  def fromUnsorted(
      times: Vector[Seconds],
      values: Vector[Double]
  ): Either[SampledProfileError, SampledCurve] =
    if times.isEmpty then Left(SampledProfileError.TooShort("times", 1, 0))
    else if times.length != values.length then Left(SampledProfileError.LengthMismatch("times", "values", times.length, values.length))
    else
      var i = 0
      while i < values.length do
        val value = values(i)
        if !value.isFinite then return Left(SampledProfileError.NonFiniteValue("values", i, value))
        i += 1
      val pairs = times.zip(values).sortBy(_._1.value)
      StrictlyIncreasingTimes
        .make(pairs.map(_._1), label = "times")
        .map(ts => new SampledCurve(ts, pairs.map(_._2)))

enum HrfParams:
  case Empty
  case Gamma(shape: Double, rate: Double)
  case Gaussian(mean: Double, sd: Double)
  case Spmg(params: SpmgParams)
  case Mexhat(mean: Double, sd: Double)
  case InvLogit(mu1: Double, s1: Double, mu2: Double, s2: Double, lag: Seconds)
  case HalfCosine(h1: Seconds, h2: Seconds, h3: Seconds, h4: Seconds, f1: Double, f2: Double)
  case Lwu(params: LwuParams, normalize: HrfFunctions.LwuNormalize)
  case Cascade34(params: Cascade34Params)
  case Boxcar(width: Seconds, amplitude: Double, normalize: Boolean)
  case Weighted(profile: WeightedProfile, method: Hrfs.WeightedMethod, normalize: Boolean)
  case Empirical(curve: SampledCurve)
  case Sine(nBasis: BasisCount)
  case Fourier(nBasis: BasisCount)
  case Daguerre(nBasis: BasisCount, scale: Double)
  case Fir(nBasis: BasisCount)
  case Bspline(requested: BasisCount, degree: Int)
  case Tent(requested: BasisCount)
  case Coefficients(baseName: String, coefficients: Vector[Double])

enum DerivativePolicy:
  case Numeric
  case Spmg(params: SpmgParams, columns: BasisCount)

enum PenaltyPolicy:
  case Identity
  case SpmgDerivatives
  case Roughness
  case FourierFrequency
  case DaguerreDecay

enum HrfFamily:
  case Known(kind: HrfKind)
  case Composite(name: String)
  case Derived(name: String)
  case Custom(name: String)

  def label: String =
    this match
      case Known(kind) => kind.canonicalName
      case Composite(name) => name
      case Derived(name) => name
      case Custom(name) => name

final case class HrfDescriptor(
    family: HrfFamily,
    basis: BasisCount,
    span: Seconds,
    params: HrfParams = HrfParams.Empty,
    derivative: DerivativePolicy = DerivativePolicy.Numeric,
    penalty: PenaltyPolicy = PenaltyPolicy.Identity,
    integration: IntegrationPolicy = IntegrationPolicy.Quadrature,
    components: Vector[HrfDescriptor] = Vector.empty
):
  def nbasis: Int =
    basis.value

  def isScalar: Boolean =
    basis.isScalar

  def name: String =
    family.label

  /** Stable, structural identity used by basis elements and provenance.
    *
    * This is intentionally independent of a rendered column label. Descriptor
    * case-class rendering is deterministic for the closed parameter ADTs and
    * keeps custom parameter values in the identity as well.
    */
  def canonicalId: String =
    val childIds = components.map(_.canonicalId).mkString("[", ",", "]")
    s"family=${family.toString}|basis=${basis.value}|span=${span.value}|params=$params|derivative=$derivative|penalty=$penalty|integration=$integration|components=$childIds"

  def withSpan(span: Seconds): HrfDescriptor =
    copy(span = span)

  /** Mark this as a kernel derived from the current one.
    *
    * `derivative` and `integration` reset rather than carry over: lagging,
    * blocking or rescaling a kernel produces a different function, and an
    * inherited primitive would then be a closed form for the wrong shape.
    */
  def derived(
      label: String,
      span: Seconds = span,
      derivative: DerivativePolicy = DerivativePolicy.Numeric,
      integration: IntegrationPolicy = IntegrationPolicy.Quadrature
  ): HrfDescriptor =
    copy(family = HrfFamily.Derived(label), span = span, derivative = derivative, integration = integration)

object HrfDescriptor:
  def custom(
      name: String,
      nbasis: Int,
      span: Seconds,
      params: HrfParams = HrfParams.Empty
  ): HrfDescriptor =
    HrfDescriptor(
      family = HrfFamily.Custom(name),
      basis = BasisCount(nbasis),
      span = span,
      params = params
    )

  def known(
      kind: HrfKind,
      nbasis: Int,
      span: Seconds,
      params: HrfParams = HrfParams.Empty,
      derivative: DerivativePolicy = DerivativePolicy.Numeric,
      penalty: PenaltyPolicy = PenaltyPolicy.Identity,
      integration: IntegrationPolicy = IntegrationPolicy.Quadrature
  ): HrfDescriptor =
    HrfDescriptor(
      family = HrfFamily.Known(kind),
      basis = BasisCount(nbasis),
      span = span,
      params = params,
      derivative = derivative,
      penalty = penalty,
      integration = integration
    )

  def scalar(
      kind: HrfKind,
      span: Seconds,
      params: HrfParams = HrfParams.Empty,
      derivative: DerivativePolicy = DerivativePolicy.Numeric,
      penalty: PenaltyPolicy = PenaltyPolicy.Identity,
      integration: IntegrationPolicy = IntegrationPolicy.Quadrature
  ): HrfDescriptor =
    known(
      kind,
      nbasis = 1,
      span = span,
      params = params,
      derivative = derivative,
      penalty = penalty,
      integration = integration
    )

  def spmg(
      kind: HrfKind,
      columns: Int,
      span: Seconds,
      params: SpmgParams
  ): HrfDescriptor =
    val basis = BasisCount(columns)
    known(
      kind = kind,
      nbasis = columns,
      span = span,
      params = HrfParams.Spmg(params),
      derivative = DerivativePolicy.Spmg(params, basis),
      penalty = if columns >= 2 then PenaltyPolicy.SpmgDerivatives else PenaltyPolicy.Identity,
      // One column is the canonical kernel itself; more columns are the
      // canonical stacked with its derivatives, each of which has its own
      // primitive.
      integration =
        if columns == 1 then IntegrationPolicy.Spmg1(params) else IntegrationPolicy.Stacked
    )

  def derived(
      name: String,
      nbasis: Int,
      span: Seconds,
      params: HrfParams = HrfParams.Empty,
      derivative: DerivativePolicy = DerivativePolicy.Numeric,
      penalty: PenaltyPolicy = PenaltyPolicy.Identity,
      integration: IntegrationPolicy = IntegrationPolicy.Quadrature,
      components: Vector[HrfDescriptor] = Vector.empty
  ): HrfDescriptor =
    HrfDescriptor(
      family = HrfFamily.Derived(name),
      basis = BasisCount(nbasis),
      span = span,
      params = params,
      derivative = derivative,
      penalty = penalty,
      integration = integration,
      components = components
    )

  def composite(
      name: String,
      components: Vector[HrfDescriptor],
      span: Seconds
  ): HrfDescriptor =
    val nbasis = components.map(_.basis.value).sum
    spmgComposite(name, components, span).getOrElse {
      HrfDescriptor(
        family = HrfFamily.Composite(name),
        basis = BasisCount(nbasis),
        span = span,
        integration = IntegrationPolicy.Stacked,
        components = components
      )
    }

  private def spmgComposite(
      name: String,
      components: Vector[HrfDescriptor],
      span: Seconds
  ): Option[HrfDescriptor] =
    val normalized = name.trim.toLowerCase
    val columns =
      normalized match
        case "spmg2" => Some(2)
        case "spmg3" => Some(3)
        case _ => None
    columns.flatMap { n =>
      if components.length == n then
        components.headOption.flatMap(_.params match
          case HrfParams.Spmg(params) =>
            Some(spmg(if n == 2 then HrfKind.Spmg2 else HrfKind.Spmg3, n, span, params).copy(components = components))
          case _ => None
        )
      else None
    }
