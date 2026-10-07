package scalafim.fmri.design.event

import scalafim.fmri.design.HrfColumnScaling
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.regressor.ConvolutionError

import scala.util.control.NonFatal

/** A declared event-level scale, before optional scan-column scaling. */
final case class EventPeakScaleReceipt(eventIndex: Int, duration: Seconds, divisors: Vector[Double])

/** The distinct event-level divisors of the events of one run (zero-based
  * block) that contribute to a realized column's scans in that run. */
final case class RunColumnEventScale(block: Int, divisors: Vector[Double]):
  require(block >= 0, "run block must be non-negative")
  require(divisors.nonEmpty, "a run entry lists at least one contributing divisor")
  require(divisors.forall(value => value.isFinite && value > 0.0), "column event divisors must be finite and positive")
  require(divisors == divisors.distinct.sorted, "column event divisors must be distinct and sorted")

/** The event-level divisors of the events that contribute to one realized
  * column, per run, under a named event-response policy. Runs whose scans no
  * event of the column reaches are absent. One divisor in a run means the
  * column's rows in that run are its kernel-unit rows divided by that
  * constant; one divisor across all runs means the whole column is. */
final case class ColumnEventScale(policy: String, runs: Vector[RunColumnEventScale]):
  require(policy.trim.nonEmpty, "column event-scale policy must be named")
  require(runs.map(_.block) == runs.map(_.block).distinct.sorted, "column event-scale runs must be distinct and sorted")

  /** Distinct divisors across all runs. */
  def divisors: Vector[Double] = runs.flatMap(_.divisors).distinct.sorted

  def uniformDivisor: Option[Double] = divisors match
    case Vector(value) => Some(value)
    case _             => None

/** The convolved columns and the response convention applied to each event. */
final case class EventResponseConvolution(
    convolved: ConvolvedTerm,
    eventPeakScales: Vector[EventPeakScaleReceipt]
)

enum EventResponseConvolutionError:
  case BlockOutOfRange(eventIndex: Int, block: Int, blocks: Int)
  case InvalidEventResponse(eventIndex: Int, detail: String)
  case NonFiniteAmplitude(eventIndex: Int, condition: Int, value: Double)
  case NonFiniteAggregate(row: Int, column: Int, value: Double)
  case InvalidPrecision(value: Double)
  case InvalidKernelSpan(value: Double)
  case InvalidConvolution(error: ConvolutionError)
  case ConvolutionFailed(detail: String)

  def message: String = this match
    case BlockOutOfRange(eventIndex, block, blocks) => s"event ${eventIndex + 1} refers to block $block, but frame has $blocks blocks"
    case InvalidEventResponse(eventIndex, detail) => s"event ${eventIndex + 1} response is invalid: $detail"
    case NonFiniteAmplitude(eventIndex, condition, value) => s"event ${eventIndex + 1}, condition ${condition + 1} has non-finite amplitude $value"
    case NonFiniteAggregate(row, column, value) => s"convolved value at row ${row + 1}, column ${column + 1} is non-finite: $value"
    case InvalidPrecision(value) => s"convolution precision must be finite and positive, got $value"
    case InvalidKernelSpan(value) => s"kernel span must be finite and positive, got $value"
    case InvalidConvolution(error) => error.message
    case ConvolutionFailed(detail) => s"event response convolution failed: $detail"

/** Convolution with event-duration normalization kept separate from kernel and
  * final-column scaling.
  *
  * Both conventions use the same numerical path as [[EventTerm.convolve]]:
  * the shared kernel is sampled at `precision`, truncated at `hrf.span`, and
  * events with negative global onset are excluded. `PreservePulseScale` is
  * therefore numerically identical to plain convolution and records no
  * per-event scales. `UnitPeak` divides each event's response, per basis
  * column, by the peak absolute value of that event's pulse response, measured
  * on the policy's reference grid (`integration` applies to that reference
  * measurement only); the division is applied to event amplitudes before the
  * shared convolution.
  */
object EventResponseConvolution:
  def convolve(
      term: EventTerm,
      hrf: Hrf,
      frame: SamplingFrame,
      normalization: EventResponseNormalization = EventResponseNormalization.PreservePulseScale,
      dropEmpty: Boolean = true,
      summate: Boolean = true,
      scaling: HrfColumnScaling = HrfColumnScaling.AsConvolved,
      integration: Integration = Integration.Exact,
      precision: Seconds = 0.3.s
  ): Either[EventResponseConvolutionError, EventResponseConvolution] =
    val dm = term.designMatrix(dropEmpty)
    val nConditions = dm.conditionTags.length
    if !precision.value.isFinite || precision.value <= 0.0 then Left(EventResponseConvolutionError.InvalidPrecision(precision.value))
    else if !hrf.span.value.isFinite || hrf.span.value <= 0.0 then Left(EventResponseConvolutionError.InvalidKernelSpan(hrf.span.value))
    else
      val invalidBlock = term.blockIds0.zipWithIndex.collectFirst {
        case (block, event) if block < 0 || block >= frame.nBlocks => EventResponseConvolutionError.BlockOutOfRange(event, block, frame.nBlocks)
      }
      val invalidAmplitude =
        (0 until term.onsets.length).iterator.flatMap { event =>
          (0 until nConditions).iterator.collect {
            case condition if !dm.data.data(event * nConditions + condition).isFinite =>
              EventResponseConvolutionError.NonFiniteAmplitude(event, condition, dm.data.data(event * nConditions + condition))
          }
        }.nextOption()
      invalidBlock.orElse(invalidAmplitude) match
        case Some(error) => Left(error)
        case None =>
          term.validateSharedConvolution(hrf, frame, precision, dropEmpty, summate)
            .left.map(EventResponseConvolutionError.InvalidConvolution.apply)
            .flatMap: _ =>
              normalization match
                case EventResponseNormalization.PreservePulseScale =>
                  attempt(term.convolve(hrf, frame, precision, dropEmpty, summate, scaling)).flatMap(checkFinite).map { convolved =>
                    val unit = Vector.fill(hrf.nbasis)(1.0)
                    EventResponseConvolution(convolved, term.durations0.zipWithIndex.map((duration, event) => EventPeakScaleReceipt(event, duration, unit)))
                  }
                case unitPeak: EventResponseNormalization.UnitPeak =>
                  for
                    scales <- prepareScales(term, hrf, unitPeak, summate, integration)
                    divisors = scales.map(_.divisors)
                    convolved0 <- attempt(term.convolveWithEventDivisors(hrf, frame, precision, dropEmpty, summate, scaling, divisors))
                    convolved <- checkFinite(convolved0)
                  yield
                    val columnScales = columnEventScales(term, dm, divisors, hrf.nbasis, frame, "unit-peak", hrf.span)
                    EventResponseConvolution(convolved.copy(eventPeakScales = scales, columnEventScales = columnScales), scales)

  private def attempt(value: => ConvolvedTerm): Either[EventResponseConvolutionError, ConvolvedTerm] =
    try Right(value)
    catch case NonFatal(error) => Left(EventResponseConvolutionError.ConvolutionFailed(Option(error.getMessage).getOrElse(error.toString)))

  private def checkFinite(term: ConvolvedTerm): Either[EventResponseConvolutionError, ConvolvedTerm] =
    val data = term.data.data
    var index = 0
    while index < data.length do
      if !data(index).isFinite then
        return Left(EventResponseConvolutionError.NonFiniteAggregate(index / term.data.cols, index % term.data.cols, data(index)))
      index += 1
    Right(term)

  private def prepareScales(
      term: EventTerm,
      hrf: Hrf,
      normalization: EventResponseNormalization,
      summate: Boolean,
      integration: Integration
  ): Either[EventResponseConvolutionError, Vector[EventPeakScaleReceipt]] =
    val cache = scala.collection.mutable.Map.empty[Double, EventResponse]
    val result = Vector.newBuilder[EventPeakScaleReceipt]
    var event = 0
    while event < term.durations0.length do
      val duration = term.durations0(event)
      val index = event
      val response = cache.get(duration.value) match
        case Some(value) => Right(value)
        case None =>
          Pulse.fromSummate(duration, summate)
            .left.map(error => EventResponseConvolutionError.InvalidEventResponse(index, error.message))
            .flatMap(pulse => EventResponse.prepare(hrf, pulse, normalization, integration).left.map(error => EventResponseConvolutionError.InvalidEventResponse(index, error.message)))
            .map { value => cache(duration.value) = value; value }
      response match
        case Left(error) => return Left(error)
        case Right(value) => result += EventPeakScaleReceipt(event, duration, value.peakScales)
      event += 1
    Right(result.result())

  /** Per run, the distinct divisors of the events that actually reach each
    * basis-major column's scans in that run: non-zero amplitude for the
    * column's condition, a non-negative global onset (events before the first
    * run are excluded by convolution), and a response window
    * `[onset, onset + duration + span]` that meets at least one of the run's
    * scan times (convolution renders each event only on its own run's scans).
    * An event whose window misses every scan contributes nothing and does not
    * make a column mixed. The window test is conservative: an event whose
    * window only touches a scan at its boundary is still counted. */
  private[event] def columnEventScales(
      term: EventTerm,
      dm: TermDesignMatrix,
      divisors: Vector[Vector[Double]],
      nbasis: Int,
      frame: SamplingFrame,
      policy: String,
      span: Seconds
  ): Vector[ColumnEventScale] =
    val nConditions = dm.conditionTags.length
    val globalOnsets = frame.globalOnsets(term.onsets, term.blockIds0)
    val scanTimes = Vector.tabulate(frame.nBlocks)(block => frame.samples(blocks = Seq(block), global = true).map(_.value))
    val reaches = Vector.tabulate(term.onsets.length) { event =>
      val onset = globalOnsets(event).value
      val end = onset + term.durations0(event).value + span.value
      onset >= 0.0 && scanTimes(term.blockIds0(event)).exists(time => time >= onset && time <= end)
    }
    Vector.tabulate(nConditions * nbasis) { column =>
      val condition = column % nConditions
      val basis = column / nConditions
      val contributing = term.onsets.indices.filter(event => reaches(event) && dm.data.data(event * nConditions + condition) != 0.0)
      val runs = contributing.groupBy(term.blockIds0).toVector.sortBy(_._1).map { (block, events) =>
        RunColumnEventScale(block, events.map(event => divisors(event)(basis)).distinct.sorted.toVector)
      }
      ColumnEventScale(policy, runs)
    }
