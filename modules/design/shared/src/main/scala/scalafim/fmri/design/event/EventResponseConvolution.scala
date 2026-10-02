package scalafim.fmri.design.event

import scalafim.fmri.design.{ConditionProvenance, HrfColumnScale, HrfColumnScaling, Names}
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

/** A declared event-level scale, before optional scan-column scaling. */
final case class EventPeakScaleReceipt(eventIndex: Int, duration: Seconds, divisors: Vector[Double])

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

  def message: String = this match
    case BlockOutOfRange(eventIndex, block, blocks) => s"event ${eventIndex + 1} refers to block $block, but frame has $blocks blocks"
    case InvalidEventResponse(eventIndex, detail) => s"event ${eventIndex + 1} response is invalid: $detail"
    case NonFiniteAmplitude(eventIndex, condition, value) => s"event ${eventIndex + 1}, condition ${condition + 1} has non-finite amplitude $value"
    case NonFiniteAggregate(row, column, value) => s"convolved value at row ${row + 1}, column ${column + 1} is non-finite: $value"

/** Convolution through a prepared pulse response, with event-duration normalization
  * deliberately separate from kernel and final-column scaling. */
object EventResponseConvolution:
  def convolve(
      term: EventTerm,
      hrf: Hrf,
      frame: SamplingFrame,
      normalization: EventResponseNormalization = EventResponseNormalization.PreservePulseScale,
      dropEmpty: Boolean = true,
      summate: Boolean = true,
      scaling: HrfColumnScaling = HrfColumnScaling.AsConvolved,
      integration: Integration = Integration.Exact
  ): Either[EventResponseConvolutionError, EventResponseConvolution] =
    val dm = term.designMatrix(dropEmpty)
    val conditions = dm.conditionTags
    val nConditions = conditions.length
    val basis = hrf.nbasis
    val rows = frame.blockLens.sum
    val columns = nConditions * basis
    val names = Names.makeColumnNames(term.termTag, conditions, basis)
    val provenance = term.conditionProvenance(dropEmpty)
    if provenance.length != nConditions then
      Left(EventResponseConvolutionError.InvalidEventResponse(0, "condition provenance and design columns disagree"))
    else
      val invalidBlock = term.blockIds0.zipWithIndex.collectFirst {
        case (block, event) if block < 0 || block >= frame.nBlocks => EventResponseConvolutionError.BlockOutOfRange(event, block, frame.nBlocks)
      }
      invalidBlock match
        case Some(error) => Left(error)
        case None =>
          prepareResponses(term, hrf, normalization, summate, integration).flatMap { prepared =>
            val data = new Array[Double](rows * columns)
            val globalOnsets = frame.globalOnsets(term.onsets, term.blockIds0)
            var rowOffset = 0
            var block = 0
            var error = Option.empty[EventResponseConvolutionError]
            while block < frame.nBlocks && error.isEmpty do
              val samples = frame.samples(Seq(block), global = true)
              var localRow = 0
              while localRow < samples.length && error.isEmpty do
                val sample = samples(localRow)
                var event = 0
                while event < term.onsets.length && error.isEmpty do
                  if term.blockIds0(event) == block then
                    val response = prepared(event)._1.at(Lag.unsafe(sample.value - globalOnsets(event).value)).data
                    var condition = 0
                    while condition < nConditions && error.isEmpty do
                      val amplitude = dm.data.data(event * nConditions + condition)
                      if !amplitude.isFinite then error = Some(EventResponseConvolutionError.NonFiniteAmplitude(event, condition, amplitude))
                      else if amplitude != 0.0 then
                        var basisIndex = 0
                        while basisIndex < basis && error.isEmpty do
                          val column = basisIndex * nConditions + condition
                          val index = (rowOffset + localRow) * columns + column
                          val value = data(index) + amplitude * response(basisIndex)
                          if !value.isFinite then error = Some(EventResponseConvolutionError.NonFiniteAggregate(rowOffset + localRow, column, value))
                          else data(index) = value
                          basisIndex += 1
                      condition += 1
                  event += 1
                localRow += 1
              rowOffset += samples.length
              block += 1
            error match
              case Some(value) => Left(value)
              case None =>
                scaleColumns(data, rows, columns, scaling).map { scales =>
                  val convolved = ConvolvedTerm(
                    term,
                    hrf,
                    Mat.unsafe(rows, columns, data),
                    names,
                    columnConditions = columnConditions(conditions, basis),
                    columnBasisIx = basisIndices(nConditions, basis),
                    columnCells = provenanceFor(provenance, p => Some(p.cell), basis),
                    columnModulators = provenanceFor(provenance, _.modulator, basis),
                    columnScales = scales,
                    eventPeakScales = prepared.map(_._2)
                  )
                  EventResponseConvolution(convolved, prepared.map(_._2))
                }
          }

  private def prepareResponses(
      term: EventTerm,
      hrf: Hrf,
      normalization: EventResponseNormalization,
      summate: Boolean,
      integration: Integration
  ): Either[EventResponseConvolutionError, Vector[(EventResponse, EventPeakScaleReceipt)]] =
    val cache = scala.collection.mutable.Map.empty[Double, EventResponse]
    val result = Vector.newBuilder[(EventResponse, EventPeakScaleReceipt)]
    var event = 0
    while event < term.durations0.length do
      val duration = term.durations0(event)
      val response = cache.get(duration.value) match
        case Some(value) => Right(value)
        case None =>
          Pulse.fromSummate(duration, summate)
            .left.map(error => EventResponseConvolutionError.InvalidEventResponse(event, error.message))
            .flatMap(pulse => EventResponse.prepare(hrf, pulse, normalization, integration).left.map(error => EventResponseConvolutionError.InvalidEventResponse(event, error.message)))
            .map { value => cache(duration.value) = value; value }
      response match
        case Left(error) => return Left(error)
        case Right(value) => result += value -> EventPeakScaleReceipt(event, duration, value.peakScales)
      event += 1
    Right(result.result())

  private def scaleColumns(data: Array[Double], rows: Int, columns: Int, policy: HrfColumnScaling): Either[EventResponseConvolutionError, Vector[HrfColumnScale]] =
    if policy == HrfColumnScaling.AsConvolved then Right(Vector.fill(columns)(HrfColumnScale.identity))
    else
      val scales = Vector.newBuilder[HrfColumnScale]
      var column = 0
      while column < columns do
        var maximum = 0.0
        var row = 0
        while row < rows do
          val value = data(row * columns + column)
          if !value.isFinite then return Left(EventResponseConvolutionError.NonFiniteAggregate(row, column, value))
          maximum = math.max(maximum, math.abs(value))
          row += 1
        val divisor = if maximum > 0.0 then maximum else 1.0
        row = 0
        while row < rows do
          val index = row * columns + column
          data(index) = data(index) / divisor
          row += 1
        scales += HrfColumnScale.applied(policy, divisor)
        column += 1
      Right(scales.result())

  private def columnConditions(conditions: Vector[String], basis: Int): Vector[Option[String]] =
    Vector.tabulate(conditions.length * basis)(column => Some(conditions(column % conditions.length)))

  private def basisIndices(conditions: Int, basis: Int): Vector[Option[Int]] =
    Vector.tabulate(conditions * basis)(column => Some(column / conditions + 1))

  private def provenanceFor[A](provenance: Vector[ConditionProvenance], select: ConditionProvenance => Option[A], basis: Int): Vector[Option[A]] =
    Vector.tabulate(provenance.length * basis)(column => select(provenance(column % provenance.length)))
