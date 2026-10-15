package scalafim.fmri.hrf.regressor

import scalafim.fmri.hrf.TimeError

/** A refusal to construct the sampled convolution plan. */
enum ConvolutionError:
  case InvalidSpan(error: TimeError)
  case InvalidPrecision(error: TimeError)
  case InvalidGrid(detail: String)
  case InvalidPreparedKernel(detail: String)
  case SampleLimitExceeded(grid: String, requested: Double, maximum: Int)
  case CellLimitExceeded(array: String, requested: Double, maximum: Int)

  def message: String = this match
    case InvalidSpan(error) => error.message
    case InvalidPrecision(error) => error.message
    case InvalidGrid(detail) => detail
    case InvalidPreparedKernel(detail) => detail
    case SampleLimitExceeded(grid, requested, maximum) =>
      s"$grid requires $requested samples; sampled convolution permits at most $maximum"
    case CellLimitExceeded(array, requested, maximum) =>
      s"$array requires $requested cells; sampled convolution permits at most $maximum"

/** Allocation policy for portable sampled convolution, shared by preparation
  * and execution. Limits refuse work rather than silently coarsening precision.
  */
object ConvolutionDiscretization:
  /** One million microtime intervals, including the sample at zero. This also
    * bounds the drive's difference array to 1,000,003 cells including its
    * two padding cells, and a full FFT convolution to at most 2,000,001
    * cells (2^21 cells in each of the four FFT scratch arrays).
    */
  val MaxGridSamples: Int = 1_000_001

  /** At most four million and four doubles (about 32 MB) in the complete
    * sampled kernel or rendered output. A sample cap alone would leave a
    * many-column kernel unbounded. This is an allocation budget; direct
    * convolution's arithmetic still depends on the occupied drive bins.
    */
  val MaxArrayCells: Int = 4_000_004

  private[scalafim] def sampleCount(width: Double, precision: Double, grid: String): Either[ConvolutionError, Int] =
    if !width.isFinite then Left(ConvolutionError.InvalidSpan(TimeError.NonFinite(grid, width)))
    else if width < 0.0 then Left(ConvolutionError.InvalidSpan(TimeError.Negative(grid, width)))
    else if !precision.isFinite then Left(ConvolutionError.InvalidPrecision(TimeError.NonFinite("precision", precision)))
    else if precision <= 0.0 then Left(ConvolutionError.InvalidPrecision(TimeError.NonPositive("precision", precision)))
    else
      // Add in Double before conversion: a saturated toInt followed by +1
      // used to overflow, and finite ratios could request enormous arrays.
      val requested = math.floor(width / precision) + 1.0
      if !requested.isFinite || requested > MaxGridSamples.toDouble then
        Left(ConvolutionError.SampleLimitExceeded(grid, requested, MaxGridSamples))
      else Right(requested.toInt)

  private[scalafim] def cellCount(rows: Double, columns: Double, array: String): Either[ConvolutionError, Int] =
    val requested = rows * columns
    if !requested.isFinite || requested > MaxArrayCells.toDouble then
      Left(ConvolutionError.CellLimitExceeded(array, requested, MaxArrayCells))
    else Right(requested.toInt)
