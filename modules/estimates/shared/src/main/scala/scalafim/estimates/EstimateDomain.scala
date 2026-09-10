package scalafim.estimates

import image4s.SomeSampleSpace
import image4s.geometry.{CoordinateConvention, LengthUnit}
import scalafim.image.SampleSpaces
import scalafim.image.SampleSpaces.*

/** Exact provider geometry, with wire samples always zero-based and x fastest.
  * Support is separate from each product's validity. No orientation is imposed.
  */
final class EstimateDomain private (
    val space: SomeSampleSpace,
    val support: Vector[Int],
    val worldFrame: String
):
  val dimensions: Vector[Int] = space.spatialDims
  val sampleCount: Int = dimensions.foldLeft(1)(_ * _)
  private val supported = support.toSet
  def contains(sample: Int): Boolean = supported.contains(sample)

object EstimateDomain:
  def make(space: SomeSampleSpace, support: Vector[Int], worldFrame: String): Either[EstimateError, EstimateDomain] =
    SampleSpaces.requireVolumeD3(space).left.map(e => EstimateError.Invalid(e.message)).flatMap: volume =>
      val size = volume.grid.shape.foldLeft(1L)(_ * _)
      if volume.grid.frame.convention != CoordinateConvention.RAS || volume.grid.frame.unit != LengthUnit.Millimeter then
        Left(EstimateError.Invalid("estimate geometry must declare RAS world coordinates in millimetres"))
      else if !Invariants.text(worldFrame) then Left(EstimateError.Invalid("a scientific world frame is required"))
      else if size > Int.MaxValue || size <= 0 then Left(EstimateError.Unsupported("V0 sample identity exceeds Int range"))
      else if !Invariants.unique(support) || support.exists(i => i < 0 || i.toLong >= size) then
        Left(EstimateError.Invalid("support must contain unique in-grid x-fastest sample indices"))
      else Right(new EstimateDomain(space, support, worldFrame))

enum Validity(val code: Byte):
  case Valid extends Validity(0)
  case OutsideSupport extends Validity(1)
  case MissingInput extends Validity(2)
  case NonEstimable extends Validity(3)
  case NumericalFailure extends Validity(4)
  case NotComputed extends Validity(5)

object Validity:
  def fromCode(code: Byte): Either[EstimateError, Validity] =
    values.find(_.code == code).toRight(EstimateError.Invalid(s"unknown core validity code $code"))

enum PoolingScope:
  case Run, JointRuns, PooledRuns, Trialwise

final case class Observation(id: ObservationId, participant: ParticipantId, acquisitions: Vector[AcquisitionId]):
  require(Invariants.unique(acquisitions))
