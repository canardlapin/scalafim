package scalafim.fmri.design.baseline

import gale.linalg.DctBasis
import scalafim.fmri.design.PolicyReceipt
import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

/** Positive finite cutoff period in seconds. Longer-period DCT components enter the drift design. */
opaque type DctCutoffPeriod = Double

object DctCutoffPeriod:
  def fromSeconds(value: Double): Either[BaselineError, DctCutoffPeriod] =
    if !value.isFinite || value <= 0.0 then
      Left(BaselineError.InvalidInput(s"DCT cutoff period must be positive and finite seconds, got $value"))
    else Right(value)

  def unsafeSeconds(value: Double): DctCutoffPeriod =
    fromSeconds(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (period: DctCutoffPeriod)
    def seconds: Double = period

/** Native cutoff selection on each run's complete original scan grid; component zero is always excluded. */
object DctDrift:
  /** Include components k = 1 ... floor(2 * samples * TR / cutoff), including a component exactly at the cutoff.
    * A request that reaches component `samples` is rejected; the finite DCT-II basis ends at `samples - 1`.
    * The count can be zero for a short run. No default cutoff or intercept is supplied by this policy.
    */
  def componentCount(samples: Int, tr: Seconds, cutoff: DctCutoffPeriod): Either[BaselineError, Int] =
    if samples <= 0 then Left(BaselineError.InvalidInput(s"DCT scan count must be positive, got $samples"))
    else if !tr.value.isFinite || tr.value <= 0.0 then
      Left(BaselineError.InvalidInput(s"DCT TR must be positive and finite seconds, got ${tr.value}"))
    else
      // Divide before multiplying to avoid overflowing a representable ratio for large physical units.
      val extent = (2.0 * samples.toDouble) * (tr.value / cutoff.seconds)
      if !extent.isFinite || extent >= samples.toDouble then
        Left(BaselineError.InvalidInput(
          s"DCT cutoff ${cutoff.seconds} seconds requests unavailable components for $samples scans at TR=${tr.value} seconds; cutoff must exceed twice TR"
        ))
      else Right(math.floor(extent).toInt)

  private[baseline] def matrix(samples: Int, tr: Seconds, cutoff: DctCutoffPeriod): Mat =
    val count = componentCount(samples, tr, cutoff)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val basis = DctBasis.columns(samples, first = 1, count = count)
      .fold(error => throw new IllegalArgumentException(error.getMessage), identity)
    val out = new Array[Double](samples * count)
    if count > 0 then basis.copyRowMajorTo(out)
    Mat.unsafe(samples, count, out)

  private[baseline] def receipts(frame: SamplingFrame, cutoff: DctCutoffPeriod): Vector[PolicyReceipt] =
    frame.blockLens.indices.map { block =>
      val count = componentCount(frame.blockLens(block), frame.tr(block), cutoff)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
      PolicyReceipt(
        "dct-drift",
        s"run=${block + 1};samples=${frame.blockLens(block)};tr_seconds=${frame.tr(block).value};acquisition_start_seconds=${frame.startTime(block).value};cutoff_seconds=${cutoff.seconds};cutoff_bits=${java.lang.Double.doubleToLongBits(cutoff.seconds)};components=1..$count;count=$count;normalization=orthonormal;grid=original-scan-index;cutoff_endpoint=inclusive;constant=excluded"
      )
    }.toVector
