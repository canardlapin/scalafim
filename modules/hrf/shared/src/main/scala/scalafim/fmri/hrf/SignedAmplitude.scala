package scalafim.fmri.hrf

/** How the coordinates of an informed basis are weighted before they are
  * pooled into a [[NonlinearResponseSummary.SignedAmplitude]].
  *
  * The published derivative boost pools raw coefficients, which presumes the
  * derivative columns are on the same scale as the canonical column. They
  * usually are not: an SPM-style temporal derivative has a much smaller norm
  * than the canonical response, so the raw formula depends on how the basis
  * happened to be scaled. [[BasisL2Norm]] removes that gauge dependence at the
  * cost of a declared quadrature; [[Unweighted]] stays the default so the
  * summary matches the formula as published.
  */
enum AmplitudeWeighting:
  /** `A = sign(b_c) * sqrt(sum_k b_k^2)` on the coefficients as fitted. */
  case Unweighted

  /** `A = sign(b_c) * sqrt(sum_k (n_k b_k)^2) / n_c`, where
    * `n_k = sqrt(integral_0^span phi_k(t)^2 dt)` is the L2 norm of basis
    * function `k` over the kernel's `[0, span]` (trapezoid rule with steps no
    * wider than `maxStep`). Dividing by the canonical norm `n_c` keeps the
    * result in canonical-coefficient units, so it equals `b_c` whenever every
    * derivative coefficient is zero.
    */
  case BasisL2Norm(maxStep: PositiveSeconds)

/** Sign of the canonical coefficient, which orients a signed amplitude. */
enum AmplitudeSign:
  case Positive
  case Negative

  /** The canonical coefficient is exactly zero (either signed zero). */
  case Zero

  def factor: Double =
    this match
      case Positive => 1.0
      case Negative => -1.0
      case Zero     => 0.0

object AmplitudeSign:
  def of(value: Double): AmplitudeSign =
    if value > 0.0 then Positive
    else if value < 0.0 then Negative
    else Zero

/** What kind of statement a nonlinear summary supports. */
enum SummaryInference:
  /** A point description of fitted coefficients. It carries no standard
    * error, degrees of freedom, null distribution or p-value, and none can be
    * obtained by treating it as a linear contrast: the summary is not linear
    * in the coefficients.
    */
  case Descriptive

/** Provenance of a signed amplitude: which coordinate oriented it and how the
  * coordinates were weighted.
  *
  * @param canonical the element whose role is [[BasisRole.Canonical]]
  * @param elements every pooled element, in basis order
  * @param columnWeights the factor `n_k` applied to each coefficient (all
  *   `1.0` for [[AmplitudeWeighting.Unweighted]])
  * @param normDiscretization the quadrature behind `columnWeights`, when one
  *   was needed
  */
final case class SignedAmplitudeReceipt(
    canonical: BasisElement,
    elements: Vector[BasisElement],
    weighting: AmplitudeWeighting,
    columnWeights: Vector[Double],
    normDiscretization: Option[FunctionalDiscretizationReceipt]
)

/** A derivative-boost signed amplitude, labelled descriptive by type.
  *
  * Sign convention: the sign is that of the canonical coefficient `b_c`. When
  * `b_c` is exactly zero the sign is [[AmplitudeSign.Zero]] and `value` is
  * `0.0`, following `sign(0) = 0` in the published formula; the pooled
  * `magnitude` is still reported so a readout can show that the response is
  * carried entirely by derivative columns rather than mistaking it for no
  * response. No `+1` tie-break is applied, since that would invent a
  * direction the canonical coordinate does not supply.
  *
  * @param value `sign.factor * magnitude`
  * @param magnitude the pooled, non-negative norm `sqrt(sum_k (n_k b_k)^2) / n_c`
  * @param inference always [[SummaryInference.Descriptive]]
  */
final case class SignedAmplitudeEstimate private[hrf] (
    summary: NonlinearResponseSummary,
    value: Double,
    magnitude: Double,
    sign: AmplitudeSign,
    inference: SummaryInference,
    receipt: SignedAmplitudeReceipt
)

private[hrf] object SignedAmplitudeEvaluation:
  def evaluate(
      kernel: Hrf,
      coefficients: Vector[Double],
      weighting: AmplitudeWeighting
  ): Either[BasisError, SignedAmplitudeEstimate] =
    for
      elements <- kernel.basisElementsValidated.left.map(error => BasisError.BasisIdentityFailure(error.message))
      _ <-
        if coefficients.length != elements.length then
          Left(BasisError.DimensionMismatch(kernel.name, elements.length, coefficients.length))
        else Right(())
      _ <- coefficients.indices.find(i => !coefficients(i).isFinite) match
        case Some(i) => Left(BasisError.NonFiniteValue("coefficients", i, coefficients(i)))
        case None    => Right(())
      canonical <- canonicalElement(elements)
      weights <- columnWeights(kernel, weighting)
      column = canonical.index - 1
      canonicalWeight = weights._1(column)
      _ <-
        if canonicalWeight > 0.0 && canonicalWeight.isFinite then Right(())
        else Left(BasisError.InvalidSummary(s"canonical basis function has norm $canonicalWeight; it cannot orient an amplitude"))
    yield
      val scaled = Vector.tabulate(coefficients.length)(k => weights._1(k) * coefficients(k))
      val magnitude = euclidean(scaled) / canonicalWeight
      val sign = AmplitudeSign.of(coefficients(column))
      val value = sign match
        case AmplitudeSign.Zero => 0.0
        case other              => other.factor * magnitude
      SignedAmplitudeEstimate(
        NonlinearResponseSummary.SignedAmplitude(weighting),
        value,
        magnitude,
        sign,
        SummaryInference.Descriptive,
        SignedAmplitudeReceipt(canonical, elements, weighting, weights._1, weights._2)
      )

  private def canonicalElement(elements: Vector[BasisElement]): Either[BasisError, BasisElement] =
    elements.find(element => !informed(element.role)) match
      case Some(element) =>
        Left(BasisError.InvalidSummary(
          s"signed amplitude needs an informed basis (canonical plus derivatives); element '${element.label}' has role '${element.role.stableLabel}'"
        ))
      case None =>
        elements.filter(_.role == BasisRole.Canonical) match
          case Vector(canonical) => Right(canonical)
          case Vector()          => Left(BasisError.UnknownRole(BasisRole.Canonical.stableLabel))
          case many =>
            Left(BasisError.InvalidSummary(s"signed amplitude needs exactly one canonical element, found ${many.length}"))

  private def informed(role: BasisRole): Boolean =
    role match
      case BasisRole.Canonical | BasisRole.TemporalDerivative | BasisRole.DispersionDerivative => true
      case _ => false

  private def columnWeights(
      kernel: Hrf,
      weighting: AmplitudeWeighting
  ): Either[BasisError, (Vector[Double], Option[FunctionalDiscretizationReceipt])] =
    weighting match
      case AmplitudeWeighting.Unweighted => Right((Vector.fill(kernel.nbasis)(1.0), None))
      case AmplitudeWeighting.BasisL2Norm(maxStep) =>
        val width = kernel.span.value
        if !maxStep.value.isFinite || maxStep.value <= 0.0 then
          Left(BasisError.InvalidDiscretization(s"basis-norm maxStep must be finite and > 0, got ${maxStep.value}"))
        else if !width.isFinite || width <= 0.0 then
          Left(BasisError.InvalidDiscretization(s"basis-norm window [0, $width] must have finite positive width"))
        else
          val ceiling = math.max(1.0, math.ceil(width / maxStep.value))
          val requested = if ceiling.isFinite && width / ceiling > maxStep.value then ceiling + 1.0 else ceiling
          if !requested.isFinite || requested > FunctionalDiscretization.MaxTrapezoidIntervals.toDouble then
            Left(BasisError.InvalidDiscretization(
              s"basis-norm maxStep ${maxStep.value} over [0, $width] needs $requested intervals, " +
                s"more than the limit of ${FunctionalDiscretization.MaxTrapezoidIntervals}"
            ))
          else
            val intervals = requested.toInt
            val dt = width / intervals.toDouble
            val sums = Array.fill(kernel.nbasis)(0.0)
            var previous = kernel(Lag.zero).data
            var i = 1
            while i <= intervals do
              val current = kernel(Lag(i.toDouble * dt)).data
              var k = 0
              while k < sums.length do
                sums(k) += 0.5 * dt * (previous(k) * previous(k) + current(k) * current(k))
                k += 1
              previous = current
              i += 1
            val norms = sums.toVector.map(math.sqrt)
            norms.indexWhere(n => !n.isFinite) match
              case -1 =>
                Right((
                  norms,
                  Some(FunctionalDiscretizationReceipt(
                    FunctionalDiscretization.Trapezoid(maxStep),
                    samples = intervals + 1,
                    effectiveStep = Some(Seconds(dt))
                  ))
                ))
              case k => Left(BasisError.NonFiniteValue("basis norm", k, norms(k)))

  /** Overflow-safe Euclidean norm. */
  private def euclidean(values: Vector[Double]): Double =
    val scale = values.foldLeft(0.0)((acc, v) => math.max(acc, math.abs(v)))
    if scale == 0.0 then 0.0
    else
      var sum = 0.0
      var k = 0
      while k < values.length do
        val r = values(k) / scale
        sum += r * r
        k += 1
      scale * math.sqrt(sum)
