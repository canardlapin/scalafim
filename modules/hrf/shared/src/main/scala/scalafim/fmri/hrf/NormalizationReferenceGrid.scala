package scalafim.fmri.hrf

/** Admission for normalization grids; requested grids are refused, never coarsened.
  * The work estimate counts scalar basis evaluations or closed primitive endpoint
  * values. Exact piecewise rules conservatively charge every segment at every
  * sample. Custom kernel internals cannot be inferred from their descriptor.
  */
object NormalizationReferenceGrid:
  val MaxSamples: Int = 1_000_000
  val MaxScalarEvaluations: Int = 10_000_000

  private[hrf] def evaluationWork(descriptor: HrfDescriptor): Double =
    val estimated =
      descriptor.derivation match
        case Some(HrfDerivation.Blocked(width, precision, halfLife, _, _, integration)) =>
          descriptor.components.headOption match
            case Some(source) => blockedSampleWork(source, width, precision, halfLife, integration)
            case None => descriptor.nbasis.toDouble
        case _ =>
          if descriptor.components.isEmpty then descriptor.nbasis.toDouble
          else descriptor.components.iterator.map(evaluationWork).sum
    // Returned vectors and normalization scans always visit the declared width,
    // including empty exact pieces or incomplete custom component metadata.
    math.max(descriptor.nbasis.toDouble, estimated)

  private[hrf] def blockedSampleWork(
      source: HrfDescriptor,
      width: Seconds,
      precision: Seconds,
      halfLife: Double,
      integration: Integration
  ): Double =
    val base = evaluationWork(source)
    val offsets = Quadrature.boxIntervalCount(width.value, precision.value)
      .fold(_ => Double.PositiveInfinity, intervals => intervals.toDouble + 1.0)
    val estimated =
      if halfLife.isInfinite && integration == Integration.Exact then
        source.integration match
          case IntegrationPolicy.PiecewisePolynomial(breaks, degree) =>
            // Exact piecewise integration can visit every segment at every sample.
            val points = math.max(1.0, math.floor(degree.toDouble / 2.0) + 1.0)
            math.max(0, breaks.length - 1).toDouble * points * base
          case _ => closedPrimitiveWork(source).getOrElse(base * offsets)
      else base * offsets
    math.max(source.nbasis.toDouble, estimated)

  private def closedPrimitiveWork(descriptor: HrfDescriptor): Option[Double] =
    descriptor.integration match
      case IntegrationPolicy.Quadrature | IntegrationPolicy.PiecewisePolynomial(_, _) => None
      case IntegrationPolicy.Stacked =>
        if descriptor.components.isEmpty || descriptor.components.map(_.nbasis.toLong).sum != descriptor.nbasis.toLong then None
        else
          val parts = descriptor.components.map(closedPrimitiveWork)
          if parts.exists(_.isEmpty) then None else Some(parts.iterator.map(_.get).sum)
      case IntegrationPolicy.Gamma(_, _) | IntegrationPolicy.Gaussian(_, _) |
          IntegrationPolicy.Cascade34(_) | IntegrationPolicy.Spmg1(_) |
          IntegrationPolicy.SpmgTemporalDeriv(_) | IntegrationPolicy.SpmgDispersionDeriv(_) |
          IntegrationPolicy.Boxcar(_, _) => Some(2.0 * descriptor.nbasis.toDouble)

  private[hrf] def stepped(
      name: String,
      span: Seconds,
      step: Seconds,
      workPerSample: Double
  ): Either[HrfNormalizationError, Int] =
    if !span.value.isFinite || span.value < 0.0 then
      Left(HrfNormalizationError.InvalidReferenceSpan(name, span))
    else if !step.value.isFinite || step.value <= 0.0 then
      Left(HrfNormalizationError.InvalidReferenceStep(name, step))
    else
      val intervals = math.ceil(span.value / step.value)
      checked(name, intervals + 1.0, intervals * step.value, workPerSample)

  private[hrf] def fixed(
      name: String,
      span: Seconds,
      mode: HrfNormalization,
      workPerSample: Double
  ): Either[HrfNormalizationError, (Int, Double)] =
    if !span.value.isFinite || span.value < 0.0 then
      Left(HrfNormalizationError.InvalidReferenceSpan(name, span))
    else
      val end = if mode == HrfNormalization.Spm then 32.0 else span.value
      // Keep round(span * 50) semantics, but inspect the Double count first.
      val count = if mode == HrfNormalization.Spm then 1600.0
        else math.max(math.floor(span.value * 50.0 + 0.5) + 1.0, 2.0)
      checked(name, count, end, workPerSample).flatMap: samples =>
        val step = end / (samples - 1).toDouble
        val last = (samples - 1).toDouble * step
        if !last.isFinite then Left(HrfNormalizationError.NonFiniteReferenceTime(name, last))
        else Right((samples, step))

  private def checked(
      name: String,
      count: Double,
      last: Double,
      workPerSample: Double
  ): Either[HrfNormalizationError, Int] =
    if !count.isFinite || count > MaxSamples.toDouble then
      Left(HrfNormalizationError.ReferenceSampleLimitExceeded(name, count, MaxSamples))
    else if !last.isFinite then
      Left(HrfNormalizationError.NonFiniteReferenceTime(name, last))
    else
      val work = count * workPerSample
      if !work.isFinite || work > MaxScalarEvaluations.toDouble then
        Left(HrfNormalizationError.ReferenceWorkLimitExceeded(name, work, MaxScalarEvaluations))
      else Right(count.toInt)
