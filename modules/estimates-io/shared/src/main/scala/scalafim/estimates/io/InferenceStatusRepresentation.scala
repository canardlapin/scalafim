package scalafim.estimates.io

import scalafim.estimates.*

/** One unscaled UInt8 NIfTI, volume order exactly the declaration order. */
final case class InferenceStatusRepresentation(file: FileReference, planes: Vector[InferenceStatusScope]):
  require(file.path.endsWith(".nii") || file.path.endsWith(".nii.gz"))
  require(planes.nonEmpty && planes.distinct.size == planes.size)
  def validate(unit: EstimateUnit): Either[EstimateError, Unit] =
    if unit.inferenceEvidence.exists(_.planes == planes) then Right(())
    else Left(EstimateError.Integrity("status representation differs from declared ordered planes"))

object InferenceStatusRepresentation:
  val maximumPlanes = 32
  val maximumPayloadBytes = 8L * 1024L * 1024L * 1024L // qualified NIfTI provider limit
  // Numeric writer pairs own data, validity and coverage; status owns two.
  val maximumWriterHandles = 96
  // Numeric reader pairs own data and validity; status owns one.
  val maximumReaderHandles = 64

  def preflight(unit: EstimateUnit): Either[EstimateError, Long] =
    unit.inferenceEvidence.toRight(EstimateError.Unsupported("unit has no inference evidence")).flatMap: evidence =>
      if evidence.planes.size > maximumPlanes then Left(EstimateError.Unsupported("at most 32 inference status planes are supported"))
      else
        // Both factors are bounded Int values; checked before any staging.
        val cells = unit.domain.sampleCount.toLong * evidence.planes.size.toLong
        if cells > maximumPayloadBytes then Left(EstimateError.Unsupported("status exceeds 8 GiB NIfTI payload budget"))
        else if cells > Long.MaxValue - 352L then Left(EstimateError.Unsupported("status byte count exceeds Long range"))
        else Right(cells)
