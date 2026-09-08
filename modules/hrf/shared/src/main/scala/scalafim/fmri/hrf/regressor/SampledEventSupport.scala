package scalafim.fmri.hrf.regressor

import scalafim.fmri.hrf.*

/** Numeric support on a particular acquired/retained sample grid. This is not
  * a test of estimability, physiological completeness, or inference validity.
  * Indices are zero-based and refer to the supplied grid and original events.
  */
final case class BasisSampleSupport private[regressor] (
    basisIndex: Int,
    sampleCount: Int,
    firstSample: Option[Int],
    lastSample: Option[Int],
    maximumAbsoluteResponse: Double
)

final case class EventSampleSupport private[regressor] (
    eventIndex: Int,
    event: StimulusEvent,
    basis: Vector[BasisSampleSupport],
    envelopeStartsBeforeGrid: Boolean,
    envelopeEndsAfterGrid: Boolean
):
  def hasSampledResponse: Boolean = basis.exists(_.sampleCount > 0)

/** Envelope flags use [onset, onset + duration + configured span]. They do not
  * certify full HRF support: a computational span can truncate an unbounded
  * kernel, and missing interior samples remain missing. Amplitudes are honored.
  */
final case class SampledEventSupport private[regressor] (
    grid: Vector[Seconds],
    precision: Seconds,
    method: Regressor.EvalMethod,
    integration: Integration,
    absoluteTolerance: Double,
    span: Seconds,
    events: Vector[EventSampleSupport]
)

enum SampleSupportError:
  case InvalidGrid(message: String)
  case InvalidPrecision(message: String)
  case InvalidSpan(message: String)
  case InvalidTolerance(value: Double)
  case InvalidRegressor(error: RegressorError)
  case NonFiniteEnvelope(eventIndex: Int)
  case NonFiniteResponse(eventIndex: Int, sampleIndex: Int, basisIndex: Int)

object SampledEventSupport:
  /** Assess each event separately, before events can cancel in a summed design.
    * Samples must be strictly increasing so indices keep their caller identity.
    * Conv shares the native prepared kernel; per-event assignments retain the
    * native per-event evaluation path. Tolerance is in response-amplitude units;
    * zero means exact numeric nonzero, not an approximation to analytic support.
    * In particular FFT roundoff may need an explicit positive tolerance.
    */
  def assess(
      regressor: Regressor,
      grid: Seq[Seconds],
      precision: Seconds = 0.33.s,
      method: Regressor.EvalMethod = Regressor.EvalMethod.Conv,
      integration: Integration = Integration.Exact,
      absoluteTolerance: Double = 0.0
  ): Either[SampleSupportError, SampledEventSupport] =
    val times = grid.toVector
    if times.isEmpty || times.exists(t => !t.value.isFinite) then
      return Left(SampleSupportError.InvalidGrid("sample grid must be nonempty and finite"))
    if times.indices.drop(1).exists(i => times(i).value <= times(i - 1).value) then
      return Left(SampleSupportError.InvalidGrid("sample grid must be strictly increasing"))
    if !precision.value.isFinite || precision.value <= 0.0 then
      return Left(SampleSupportError.InvalidPrecision("precision must be finite and positive"))
    if !regressor.span.value.isFinite || regressor.span.value <= 0.0 then
      return Left(SampleSupportError.InvalidSpan("span must be finite and positive"))
    if !absoluteTolerance.isFinite || absoluteTolerance < 0.0 then
      return Left(SampleSupportError.InvalidTolerance(absoluteTolerance))

    val prepared = (regressor.hrf, method) match
      case (HrfAssignment.Shared(hrf), Regressor.EvalMethod.Conv) =>
        Regressor.prepareConvolution(hrf, regressor.span, precision) match
          case Left(error) => return Left(SampleSupportError.InvalidPrecision(error.message))
          case Right(kernel) => Some(kernel)
      case _ => None
    val rawGrid = times.map(_.value)
    val result = Vector.newBuilder[EventSampleSupport]
    var eventIndex = 0
    while eventIndex < regressor.events.length do
      val event = regressor.events(eventIndex)
      val end = event.onset.value + event.duration.value + regressor.span.value
      if !end.isFinite then return Left(SampleSupportError.NonFiniteEnvelope(eventIndex))
      val assignment = regressor.hrf match
        case shared: HrfAssignment.Shared => shared
        case HrfAssignment.PerEvent(hrfs) => HrfAssignment.PerEvent(Vector(hrfs(eventIndex)))
      val single = Regressor.fromEvents(Vector(event), assignment, regressor.span, regressor.summate) match
        case Left(error) => return Left(SampleSupportError.InvalidRegressor(error))
        case Right(value) => value
      val response = prepared match
        case Some(kernel) => kernel.evaluate(single, rawGrid)
        case None => Regressor.evaluate(single, rawGrid, precision.value, method, integration)
      val bases = Vector.newBuilder[BasisSampleSupport]
      var basis = 0
      while basis < response.cols do
        var count = 0
        var first: Option[Int] = None
        var last: Option[Int] = None
        var maximum = 0.0
        var sample = 0
        while sample < response.rows do
          val value = math.abs(response(sample, basis))
          if !value.isFinite then
            return Left(SampleSupportError.NonFiniteResponse(eventIndex, sample, basis))
          maximum = math.max(maximum, value)
          if value > absoluteTolerance then
            count += 1
            if first.isEmpty then first = Some(sample)
            last = Some(sample)
          sample += 1
        bases += BasisSampleSupport(basis, count, first, last, maximum)
        basis += 1
      result += EventSampleSupport(eventIndex, event, bases.result(),
        event.onset.value < times.head.value, end > times.last.value)
      eventIndex += 1
    Right(SampledEventSupport(times, precision, method, integration,
      absoluteTolerance, regressor.span, result.result()))
