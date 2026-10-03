package scalafim.fmri.hrf

import scalafim.fmri.hrf.linalg.{Mat, Vec}

/** Scaling applied to one event's pulse response. It is separate from kernel
  * and design-column normalization: changing an event duration can therefore
  * receive its own declared peak convention.
  *
  * Naming: event-level `UnitPeak` ("unit-peak" in formulas) divides **each
  * basis column independently** by the peak absolute value of that column's
  * pulse response `(q_d * h_b)`, matching fmrihrf `block_hrf(normalize = TRUE)`.
  * Kernel-level [[HrfNormalization.UnitPeak]] ("unit_peak") instead divides
  * every basis column of the kernel by one factor taken from the canonical
  * column, so relative basis amplitudes are preserved. The two are not
  * interchangeable for multi-column bases. */
enum EventResponseNormalization:
  /** The pulse response as convolved; numerically identical to declaring no
    * event normalization. */
  case PreservePulseScale
  /** Per-event, per-basis unit peak on a reference grid with `referenceStep`. */
  case UnitPeak(referenceStep: PositiveSeconds)

object EventResponseNormalization:
  def unitPeak(referenceStep: Seconds): Either[TimeError, EventResponseNormalization] =
    PositiveSeconds.fromSeconds(referenceStep, "event response peak reference step").map(UnitPeak.apply)

enum EventResponseNormalizationError:
  case ZeroPeak(basis: Int)
  case NonFiniteResponse(basis: Int, sample: Int, value: Double)
  case InvalidResponseSpan(value: Double)
  case ReferenceGridTooLarge(samples: Double, maximum: Int)

  def message: String = this match
    case ZeroPeak(basis) => s"event response basis ${basis + 1} has zero peak over its declared response span"
    case NonFiniteResponse(basis, sample, value) => s"event response basis ${basis + 1} is non-finite at reference sample ${sample + 1}: $value"
    case InvalidResponseSpan(value) => s"event response span must be finite and non-negative, got $value"
    case ReferenceGridTooLarge(samples, maximum) => s"event response reference grid needs $samples samples, exceeding maximum $maximum"

/** A prepared pulse response whose optional scale belongs to this event alone. */
final case class EventResponse private (
    kernel: Hrf,
    pulse: Pulse,
    normalization: EventResponseNormalization,
    peakScales: Vector[Double],
    precision: Seconds,
    integration: Integration
):
  require(peakScales.length == kernel.nbasis, "event response scales must match HRF basis columns")

  /** Evaluate with the same integration convention and precision used to obtain
    * the scale (the reference step for `UnitPeak`, the declared `precision`
    * otherwise). Prepare a separate response to change either setting. */
  def at(lag: Lag): Vec =
    val raw = PulseResponse.at(pulse, kernel, lag, precision, integration).data
    val scaled = new Array[Double](raw.length)
    var basis = 0
    while basis < raw.length do
      scaled(basis) = raw(basis) / peakScales(basis)
      basis += 1
    Vec.unsafe(scaled)

  def evaluate(grid: Seq[Lag]): Mat =
    val data = new Array[Double](grid.length * kernel.nbasis)
    var row = 0
    while row < grid.length do
      val value = at(grid(row)).data
      System.arraycopy(value, 0, data, row * kernel.nbasis, kernel.nbasis)
      row += 1
    Mat.unsafe(grid.length, kernel.nbasis, data)

object EventResponse:
  private val MaximumReferenceSamples = 1000000

  def prepare(
      kernel: Hrf,
      pulse: Pulse,
      normalization: EventResponseNormalization = EventResponseNormalization.PreservePulseScale,
      integration: Integration = Integration.Exact,
      precision: PositiveSeconds = PositiveSeconds.unsafe(0.2.s)
  ): Either[EventResponseNormalizationError, EventResponse] =
    normalization match
      case EventResponseNormalization.PreservePulseScale =>
        Right(EventResponse(kernel, pulse, normalization, Vector.fill(kernel.nbasis)(1.0), precision.seconds, integration))
      case EventResponseNormalization.UnitPeak(referenceStep) =>
        val responseSpan = kernel.span.value + pulse.durationSeconds.value
        if !responseSpan.isFinite || responseSpan < 0.0 then Left(EventResponseNormalizationError.InvalidResponseSpan(responseSpan))
        else
          val intervals = math.ceil(responseSpan / referenceStep.value)
          if !intervals.isFinite || intervals + 1.0 > MaximumReferenceSamples.toDouble then Left(EventResponseNormalizationError.ReferenceGridTooLarge(intervals + 1.0, MaximumReferenceSamples))
          else
            val samples = intervals.toInt + 1
            val peaks = Array.fill(kernel.nbasis)(0.0)
            var sample = 0
            var failure = Option.empty[EventResponseNormalizationError]
            while sample < samples && failure.isEmpty do
              val response = PulseResponse.at(pulse, kernel, Lag(sample * referenceStep.value), referenceStep.seconds, integration).data
              var basis = 0
              while basis < response.length && failure.isEmpty do
                val value = response(basis)
                if !value.isFinite then failure = Some(EventResponseNormalizationError.NonFiniteResponse(basis, sample, value))
                else
                  val magnitude = math.abs(value)
                  if magnitude > peaks(basis) then peaks(basis) = magnitude
                basis += 1
              sample += 1
            failure match
              case Some(error) => Left(error)
              case None =>
                peaks.indexWhere(_ <= 0.0) match
                  case basis if basis >= 0 => Left(EventResponseNormalizationError.ZeroPeak(basis))
                  case _ => Right(EventResponse(kernel, pulse, normalization, peaks.toVector, referenceStep.seconds, integration))
