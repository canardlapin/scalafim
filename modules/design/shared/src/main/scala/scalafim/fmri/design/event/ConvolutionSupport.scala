package scalafim.fmri.design.event

import scalafim.fmri.design.{HrfColumnScale, ScanIndex}
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.regressor.{HrfAssignment, SampledEventSupport, SampleSupportError, RegressorError}

/** The native run/condition input, captured at convolution rather than inferred
  * from a representative HRF or rendered column label.
  */
private[event] final case class ConvolutionChannel(
    blockId: Int,
    rowOffset: Int,
    grid: Vector[Seconds],
    eventIndices: Vector[Int],
    columns: Vector[Int],
    regressor: Regressor,
    basisIndices: Vector[Int]
)

/** Coverage of the finite computational window, not physiological completeness. */
final case class ResponseWindowCoverage(span: Seconds, startsBeforeGrid: Boolean, endsAfterGrid: Boolean)

final case class ObservedResponseSupport(
    sampleCount: Int,
    firstScan: Option[ScanIndex],
    lastScan: Option[ScanIndex],
    maximumAbsoluteResponse: Double,
    window: Option[ResponseWindowCoverage] = None
)

final case class EventColumnSupport(
    eventIndex: Int,
    sourceRow: Option[Int],
    blockId: Int,
    columnIndex: Int,
    columnName: String,
    inputAmplitude: Double,
    globalOnset: Seconds,
    duration: Seconds,
    columnScale: HrfColumnScale,
    actual: ObservedResponseSupport,
    unitInput: ObservedResponseSupport
):
  /** Maximum contribution in the coordinates of the scaled design column. */
  def scaledMaximumAbsoluteResponse: Double = actual.maximumAbsoluteResponse / columnScale.divisor

final case class ConvolvedSupport(
    retainedScans: Vector[ScanIndex],
    precision: Seconds,
    unscaledAbsoluteTolerance: Double,
    contributions: Vector[EventColumnSupport]
)

enum ConvolutionSupportError:
  case MissingRecipe
  case ChangedTerm
  case InvalidSelection(message: String)
  case InvalidTolerance(value: Double)
  case RegressorFailure(error: RegressorError)
  case EvaluationFailure(blockId: Int, columns: Vector[Int], error: SampleSupportError)

/** A runtime recipe for the as-convolved term. It is not a serialized HRF
  * identity or an exclusion policy. The recipe keeps private numeric snapshots and refuses stale data, source
  * terms, or column metadata. HRF functions must remain deterministic; arbitrary
  * user closures cannot be certified or serialized by this runtime receipt.
  */
final class ConvolutionRecipe private[event] (
    private val original: ConvolvedTerm,
    val samplingFrame: SamplingFrame,
    val precision: Seconds,
    private val channels: Vector[ConvolutionChannel]
):
  private val matrixSnapshot = original.data.data.clone()
  private val inputSnapshots = original.term.events.collect {
    case continuous: ContinuousEvent => continuous.value.data.clone()
  }

  private def sameBits(left: Array[Double], right: Array[Double]): Boolean =
    left.length == right.length && left.indices.forall(i =>
      java.lang.Double.doubleToLongBits(left(i)) == java.lang.Double.doubleToLongBits(right(i)))

  private def matches(current: ConvolvedTerm): Boolean =
    (current.term eq original.term) && (current.data eq original.data) && (current.hrf eq original.hrf) &&
      current.columnNames == original.columnNames && current.columnScales == original.columnScales &&
      current.columnConditions == original.columnConditions && current.columnBasisIx == original.columnBasisIx &&
      current.columnHrfs == original.columnHrfs && current.columnCells == original.columnCells &&
      current.columnModulators == original.columnModulators && sameBits(current.data.data, matrixSnapshot) &&
      current.term.events.collect { case continuous: ContinuousEvent => continuous.value.data }
        .zip(inputSnapshots).forall((current, snapshot) => sameBits(current, snapshot))

  /** Only source-row provenance changes here; channels and numerical values are retained. */
  private[design] def remapSources(current: ConvolvedTerm, updated: ConvolvedTerm): Option[ConvolutionRecipe] =
    Option.when(matches(current))(new ConvolutionRecipe(updated, samplingFrame, precision, channels))

  /** Structural provenance of the captured runtime recipe, using native HRF
    * descriptors. This does not serialize or certify arbitrary user closures.
    */
  def canonical: String =
    def bits(value: Double): String = java.lang.Double.doubleToLongBits(value).toString
    val parts = channels.map { channel =>
      val reg = channel.regressor
      val assignment = reg.hrf match
        case HrfAssignment.Shared(hrf) => "shared:" + scalafim.fmri.design.HrfAssignment.hrfCanonical(hrf)
        case HrfAssignment.PerEvent(hrfs) =>
          hrfs.map(scalafim.fmri.design.HrfAssignment.hrfCanonical).mkString("per-event:[", ",", "]")
      val events = reg.events.map(e => s"${bits(e.onset.value)}:${bits(e.duration.value)}:${bits(e.amplitude)}").mkString(",")
      s"block=${channel.blockId};offset=${channel.rowOffset};columns=${channel.columns.mkString(",")};" +
        s"basis=${channel.basisIndices.mkString(",")};events=${channel.eventIndices.mkString(",")};grid=${channel.grid.map(t => bits(t.value)).mkString(",")};" +
        s"span=${bits(reg.span.value)};summate=${reg.summate};assignment=$assignment;inputs=$events"
    }
    s"convolution/v2;precision=${bits(precision.value)};" + parts.mkString("channels=[", "|", "]")

  private[event] def inspect(
      current: ConvolvedTerm,
      retainedScans: Vector[ScanIndex],
      tolerance: Double
  ): Either[ConvolutionSupportError, ConvolvedSupport] =
    if !matches(current) then return Left(ConvolutionSupportError.ChangedTerm)
    if !tolerance.isFinite || tolerance < 0.0 then return Left(ConvolutionSupportError.InvalidTolerance(tolerance))
    val indices = retainedScans.map(_.zeroBased)
    if indices.exists(i => i < 0 || i >= original.data.rows) ||
        indices.indices.drop(1).exists(i => indices(i) <= indices(i - 1)) then
      return Left(ConvolutionSupportError.InvalidSelection("retained scans must be in range and strictly increasing"))
    val selected = indices.toSet
    val out = Vector.newBuilder[EventColumnSupport]
    val empty = ObservedResponseSupport(0, None, None, 0.0)
    var channelIndex = 0
    while channelIndex < channels.length do
      val channel = channels(channelIndex)
      val positions = channel.grid.indices.filter(i => selected.contains(channel.rowOffset + i)).toVector
      val grid = positions.map(channel.grid)
      val scans = positions.map(i => ScanIndex.unsafeOneBased(channel.rowOffset + i + 1))
      val reg = channel.regressor
      if reg.events.nonEmpty then
        val evaluated = if grid.isEmpty then None else
          val unitSpan = reg.hrf match
            case HrfAssignment.Shared(_) => reg.span
            case HrfAssignment.PerEvent(hrfs) => hrfs.map(_.span).max
          val unitReg = Regressor.fromParts(reg.onsets, reg.durations, Vector.fill(reg.events.length)(1.0),
            reg.hrf, unitSpan, reg.summate) match
            case Left(error) => return Left(ConvolutionSupportError.RegressorFailure(error))
            case Right(value) => value
          val actual = SampledEventSupport.assess(reg, grid, precision, absoluteTolerance = tolerance) match
            case Left(error) => return Left(ConvolutionSupportError.EvaluationFailure(channel.blockId, channel.columns, error))
            case Right(value) => value
          val potential = SampledEventSupport.assess(unitReg, grid, precision, absoluteTolerance = tolerance) match
            case Left(error) => return Left(ConvolutionSupportError.EvaluationFailure(channel.blockId, channel.columns, error))
            case Right(value) => value
          Some((actual, potential))
        var event = 0
        while event < reg.events.length do
          var basis = 0
          while basis < channel.columns.length do
            def response(receipt: SampledEventSupport): ObservedResponseSupport =
              val support = receipt.events(event).basis(channel.basisIndices(basis))
              ObservedResponseSupport(support.sampleCount, support.firstSample.map(scans),
                support.lastSample.map(scans), support.maximumAbsoluteResponse,
                Some(ResponseWindowCoverage(receipt.span, receipt.events(event).envelopeStartsBeforeGrid,
                  receipt.events(event).envelopeEndsAfterGrid)))
            val column = channel.columns(basis)
            val eventIndex = channel.eventIndices(event)
            out += EventColumnSupport(eventIndex, original.term.sourceRowIndices.lift(eventIndex),
              channel.blockId, column, original.columnNames(column), reg.events(event).amplitude,
              reg.events(event).onsetSeconds, reg.events(event).durationSeconds,
              original.scaleForColumn(column), evaluated.fold(empty)(pair => response(pair._1)),
              evaluated.fold(empty)(pair => response(pair._2)))
            basis += 1
          event += 1
      channelIndex += 1
    Right(ConvolvedSupport(retainedScans, precision, tolerance, out.result()))

object ConvolutionRecipe:
  /** Join already evaluated main and slope streams without inventing kernels
    * for their combined columns. Both streams share the same event schedule.
    */
  private[design] def joinColumns(left: ConvolvedTerm, right: ConvolvedTerm, joined: ConvolvedTerm): Option[ConvolutionRecipe] =
    for
      a <- left.convolutionRecipe if a.matches(left)
      b <- right.convolutionRecipe if b.matches(right)
      if a.samplingFrame == b.samplingFrame && a.precision == b.precision
      if left.term.schedule == right.term.schedule && left.term.sourceRowIndices == right.term.sourceRowIndices
    yield new ConvolutionRecipe(joined, a.samplingFrame, a.precision,
      a.channels ++ b.channels.map(channel => channel.copy(columns = channel.columns.map(_ + left.data.cols))))
