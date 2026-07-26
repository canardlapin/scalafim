package scalafim.dataset.zarr

import scalafim.archive.zarr.{AcquisitionTiming, TimeUnits}
import scalafim.dataset.{DatasetError, DatasetProvenance}
import scalafim.fmri.hrf.design.SamplingFrame

enum SamplingPrecisionPolicy:
  case Default
  case Fixed(seconds: Double)

  private[zarr] def precisionFor(trSeconds: Double): Either[DatasetError, Double] =
    this match
      case Default =>
        val precision = math.min(0.1, trSeconds / 10.0)
        if precision.isFinite && precision > 0.0 && precision < trSeconds then Right(precision)
        else Left(DatasetError.InvalidTimeAxis(
          s"default modeling precision cannot represent TR $trSeconds seconds"
        ))
      case Fixed(seconds) =>
        if seconds.isFinite && seconds > 0.0 && seconds < trSeconds then Right(seconds)
        else Left(DatasetError.InvalidTimeAxis(
          s"modeling precision must be finite, positive, and less than TR $trSeconds seconds; got $seconds"
        ))

object CanonicalBoldSampling:
  def refine(
      timing: AcquisitionTiming,
      policy: SamplingPrecisionPolicy
  ): Either[DatasetError, SamplingFrame] =
    timing match
      case AcquisitionTiming.Explicit(_, _) =>
        Left(DatasetError.InvalidTimeAxis(
          "explicit NeuroArchive timing cannot be represented by SamplingFrame"
        ))

      case AcquisitionTiming.Regular(origin, step, count, units) =>
        val scale =
          units match
            case TimeUnits.Second      => 1.0
            case TimeUnits.Millisecond => 0.001
        val originSeconds = origin * scale
        val stepSeconds = step * scale
        for
          _ <-
            if originSeconds.isFinite && originSeconds >= 0.0 then Right(())
            else Left(DatasetError.InvalidTimeAxis(
              s"regular timing origin must be finite and non-negative after conversion to seconds; got $originSeconds"
            ))
          _ <-
            if stepSeconds.isFinite && stepSeconds > 0.0 then Right(())
            else Left(DatasetError.InvalidTimeAxis(
              s"regular timing step must be finite and positive after conversion to seconds; got $stepSeconds"
            ))
          nScans <-
            if count > 0L && count <= Int.MaxValue.toLong then Right(count.toInt)
            else Left(DatasetError.InvalidTimeAxis(
              s"regular timing count must be between 1 and ${Int.MaxValue}; got $count"
            ))
          precision <- policy.precisionFor(stepSeconds)
          frame <- SamplingFrame
            .regular(
              tr = stepSeconds,
              nScans = nScans,
              startTime = originSeconds,
              precision = precision
            )
            .left
            .map(error => DatasetError.InvalidTimeAxis(error.message))
        yield frame

final case class NeuroArchiveZarrProvenance(
    acquisitionId: String,
    payloadId: String,
    contentRevision: String,
    logicalPayloadHash: String,
    precisionPolicy: SamplingPrecisionPolicy
) extends DatasetProvenance:
  val source: String = "neuroarchive-zarr"
