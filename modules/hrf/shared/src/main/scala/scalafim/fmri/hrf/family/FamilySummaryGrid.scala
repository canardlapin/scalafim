package scalafim.fmri.hrf.family

import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scala.util.control.NonFatal

enum ShapeSummaryField(val label: String):
  case PeakLatency extends ShapeSummaryField("peak latency")
  case Fwhm extends ShapeSummaryField("FWHM")
  case UndershootRatio extends ShapeSummaryField("undershoot ratio")

enum FamilySummaryError:
  case Chart(error: ShapeChartError)
  case InvalidExtent(value: Double)
  case InvalidHorizon(value: Seconds)
  case InvalidPrecision(value: Seconds)
  case NonFiniteRange(value: Double)
  case NonFiniteLastTime(value: Double)
  case SampleLimitExceeded(requested: Double, maximum: Int)
  case NonFiniteValue(index: Int, value: Double)
  case NonFiniteEnergy
  case NonFiniteSummary(field: ShapeSummaryField, value: Double)
  case EvaluationFailed(detail: String)

  def message: String = this match
    case Chart(error) => error.message
    case InvalidExtent(value) => s"tail extent must be finite and >= 1, got $value"
    case InvalidHorizon(value) => s"summary horizon must be finite and > 0, got ${value.value}"
    case InvalidPrecision(value) => s"summary precision must be finite and > 0, got ${value.value}"
    case NonFiniteRange(value) => s"summary range must be finite, got $value"
    case NonFiniteLastTime(value) => s"last summary lag must be finite, got $value"
    case SampleLimitExceeded(requested, maximum) => s"summary grid requests $requested samples, maximum is $maximum"
    case NonFiniteValue(index, value) => s"summary kernel value at sample $index must be finite, got $value"
    case NonFiniteEnergy => "summary squared energy must be finite"
    case NonFiniteSummary(field, value) => s"summary ${field.label} must be finite, got $value"
    case EvaluationFailed(detail) => s"family summary evaluation failed: $detail"

/** Portable admission for scalar family summaries. Each admitted sample allocates
  * two Double cells (lag and value) and evaluates one scalar kernel value. No
  * basis or jet arrays are constructed; chart dimensions remain bounded by
  * ShapeChart.MaxDimension. Custom family evaluation internals are not bounded.
  */
object FamilySummaryGrid:
  val MaxSamples: Int = 1_000_000
  val MaxArrayCells: Int = 2 * MaxSamples
  val MaxScalarEvaluations: Int = MaxSamples

  private[family] final case class Admitted private[FamilySummaryGrid] (samples: Int, step: PositiveSeconds):
    def evaluate(family: ParametricHrfFamily, point: ShapePoint): Either[FamilySummaryError, (Array[Double], Array[Double])] =
      val lags = Array.tabulate(samples)(i => i * step.value)
      val values = new Array[Double](samples)
      try family.evalInto(lags, point, values)
      catch
        case NonFatal(error) => return Left(FamilySummaryError.EvaluationFailed(Option(error.getMessage).getOrElse(error.toString)))
      var i = 0
      while i < samples do
        if !values(i).isFinite then return Left(FamilySummaryError.NonFiniteValue(i, values(i)))
        i += 1
      Right((lags, values))

  private[family] def tail(horizon: PositiveSeconds, precision: PositiveSeconds, extent: Double): Either[FamilySummaryError, Admitted] =
    if !extent.isFinite || extent < 1.0 then Left(FamilySummaryError.InvalidExtent(extent))
    else admit(horizon, precision, extent, roundUp = true)

  private[family] def summary(horizon: PositiveSeconds): Either[FamilySummaryError, Admitted] =
    admit(horizon, PositiveSeconds.unsafe(Seconds.unsafe(0.01)), 1.0, roundUp = false)

  private def admit(horizon: PositiveSeconds, precision: PositiveSeconds, extent: Double, roundUp: Boolean): Either[FamilySummaryError, Admitted] =
    val h = horizon.value
    val dt = precision.value
    if !h.isFinite || h <= 0.0 then Left(FamilySummaryError.InvalidHorizon(horizon.seconds))
    else if !dt.isFinite || dt <= 0.0 then Left(FamilySummaryError.InvalidPrecision(precision.seconds))
    else
      val end = extent * h
      if !end.isFinite then Left(FamilySummaryError.NonFiniteRange(end))
      else
        val intervals = if roundUp then math.max(1.0, math.ceil(end / dt)) else math.floor(end / dt)
        val count = intervals + 1.0
        if !count.isFinite || count > MaxSamples.toDouble then Left(FamilySummaryError.SampleLimitExceeded(count, MaxSamples))
        else
          // Tail quadrature deliberately preserves its ceil endpoint, including
          // overshoot when end is not a multiple of dt. Summaries retain floor.
          val last = intervals * dt
          if !last.isFinite then Left(FamilySummaryError.NonFiniteLastTime(last))
          else Right(Admitted(count.toInt, precision))
