package scalafim.fmri.hrf

/** Versioned structural encoding, independent of platform and display rendering.
  * Numbers retain their IEEE-754 bits (including signed zero, with canonical
  * NaN); length-framed fields keep names and nested sequences unambiguous.
  */
private[hrf] object HrfIdentity:
  def number(value: Double): String =
    s"bits:${java.lang.Double.doubleToLongBits(value)}"

  def index(value: Int): String =
    val text = value.toString
    if text.length < 2 then s"0$text" else text

  def descriptor(value: HrfDescriptor): String =
    s"hrf-descriptor/v2|family=${family(value.family)}|basis=${value.basis.value}|span=${number(value.span.value)}|" +
      s"params=${params(value.params)}|derivative=${derivative(value.derivative)}|penalty=${penalty(value.penalty)}|" +
      s"integration=${integration(value.integration)}|derivation=${value.derivation.fold("none")(derivation)}|" +
      s"components=${sequence(value.components.map(descriptor))}"

  private def record(tag: String, fields: String*): String =
    fields.map(field => s"${field.length}:$field").mkString(s"$tag(", ",", ")")

  private def sequence(fields: Vector[String]): String =
    record("sequence", fields*)

  private def times(values: Vector[Seconds]): String =
    sequence(values.map(value => number(value.value)))

  private def numbers(values: Vector[Double]): String =
    sequence(values.map(number))

  private def spmg(value: SpmgParams): String =
    record("spmg", number(value.p1), number(value.p2), number(value.a1))

  private def cascade(value: Cascade34Params): String =
    record("cascade34", number(value.kappaP), number(value.kappaU), number(value.rho))

  private def family(value: HrfFamily): String =
    value match
      case HrfFamily.Known(kind) => record("known", kind.canonicalName)
      case HrfFamily.Composite(name) => record("composite", name)
      case HrfFamily.Derived(name) => record("derived", name)
      case HrfFamily.Custom(name) => record("custom", name)

  private def params(value: HrfParams): String =
    value match
      case HrfParams.Empty => "empty"
      case HrfParams.Gamma(shape, rate) => record("gamma", number(shape), number(rate))
      case HrfParams.Gaussian(mean, sd) => record("gaussian", number(mean), number(sd))
      case HrfParams.Spmg(value) => spmg(value)
      case HrfParams.Mexhat(mean, sd) => record("mexhat", number(mean), number(sd))
      case HrfParams.InvLogit(mu1, s1, mu2, s2, lag) =>
        record("inv-logit", number(mu1), number(s1), number(mu2), number(s2), number(lag.value))
      case HrfParams.HalfCosine(h1, h2, h3, h4, f1, f2) =>
        record("half-cosine", number(h1.value), number(h2.value), number(h3.value), number(h4.value), number(f1), number(f2))
      case HrfParams.Lwu(value, normalize) =>
        val normalization = normalize match
          case HrfFunctions.LwuNormalize.None => "none"
          case HrfFunctions.LwuNormalize.Height => "height"
          case HrfFunctions.LwuNormalize.Area => "area"
        record("lwu", number(value.tau), number(value.sigma), number(value.rho), normalization)
      case HrfParams.Cascade34(value) => cascade(value)
      case HrfParams.Boxcar(width, amplitude, normalize) =>
        record("boxcar", number(width.value), number(amplitude), normalize.toString)
      case HrfParams.Weighted(profile, method, normalize) =>
        val interpolation = method match
          case Hrfs.WeightedMethod.Constant => "constant"
          case Hrfs.WeightedMethod.Linear => "linear"
        record("weighted", times(profile.times.values), numbers(profile.weights), interpolation, normalize.toString)
      case HrfParams.Empirical(curve) => record("empirical", times(curve.times.values), numbers(curve.values))
      case HrfParams.Sine(count) => record("sine", count.value.toString)
      case HrfParams.Fourier(count) => record("fourier", count.value.toString)
      case HrfParams.Daguerre(count, scale) => record("daguerre", count.value.toString, number(scale))
      case HrfParams.Fir(count) => record("fir", count.value.toString)
      case HrfParams.Bspline(count, degree, includeIntercept) =>
        record("bspline", count.value.toString, degree.toString, includeIntercept.toString)
      case HrfParams.Tent(count) => record("tent", count.value.toString)
      case HrfParams.Coefficients(baseName, coefficients) => record("coefficients", baseName, numbers(coefficients))

  private def derivative(value: DerivativePolicy): String =
    value match
      case DerivativePolicy.Numeric => "numeric"
      case DerivativePolicy.Spmg(value, columns) => record("spmg", spmg(value), columns.value.toString)

  private def penalty(value: PenaltyPolicy): String =
    value match
      case PenaltyPolicy.Identity => "identity"
      case PenaltyPolicy.SpmgDerivatives => "spmg-derivatives"
      case PenaltyPolicy.Roughness => "roughness"
      case PenaltyPolicy.FourierFrequency => "fourier-frequency"
      case PenaltyPolicy.DaguerreDecay => "daguerre-decay"

  private def derivation(value: HrfDerivation): String =
    value match
      case HrfDerivation.Lagged(by) => record("lagged", number(by.value))
      case HrfDerivation.Blocked(width, precision, halfLife, summate, normalize, integration) =>
        val rule = integration match
          case Integration.Exact => "exact"
          case Integration.Trapezoid => "trapezoid"
        record("blocked", number(width.value), number(precision.value), number(halfLife),
          summate.toString, normalize.toString, rule)
      case HrfDerivation.PeakNormalized(step) => record("peak-normalized", number(step.value))
      case HrfDerivation.Normalized(mode) => record("normalized", mode.label)

  private def integration(value: IntegrationPolicy): String =
    value match
      case IntegrationPolicy.Quadrature => "quadrature"
      case IntegrationPolicy.Gamma(shape, rate) => record("gamma", number(shape), number(rate))
      case IntegrationPolicy.Gaussian(mean, sd) => record("gaussian", number(mean), number(sd))
      case IntegrationPolicy.Cascade34(value) => cascade(value)
      case IntegrationPolicy.Spmg1(value) => record("spmg1", spmg(value))
      case IntegrationPolicy.SpmgTemporalDeriv(value) => record("spmg-temporal-derivative", spmg(value))
      case IntegrationPolicy.SpmgDispersionDeriv(value) => record("spmg-dispersion-derivative", spmg(value))
      case IntegrationPolicy.Boxcar(width, amplitude) => record("boxcar", number(width.value), number(amplitude))
      case IntegrationPolicy.PiecewisePolynomial(breaks, degree) => record("piecewise-polynomial", times(breaks), degree.toString)
      case IntegrationPolicy.Stacked => "stacked"
