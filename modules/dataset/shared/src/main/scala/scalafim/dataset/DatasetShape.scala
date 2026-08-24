package scalafim.dataset

import scalafim.image.{SomeSampleSpace, VolumeSpace}
import scalafim.image.SampleSpaces.*

final case class DatasetShape(space: SomeSampleSpace, timepoints: Int):
  require(timepoints > 0, "timepoints must be positive")
  require(space.ndim == 3, "dataset space must be exactly 3D")

  def volumeSpace: VolumeSpace =
    VolumeSpace.unsafe(space)
  def spatialDims: Vector[Int] = space.spatialDims
  def spatialSize: Int = volumeSpace.nVoxels

object DatasetShape:
  def make(space: SomeSampleSpace, timepoints: Int): Either[DatasetError, DatasetShape] =
    if timepoints <= 0 then Left(DatasetError.NonPositiveTimepoints(timepoints))
    else
      space.asVolumeSpace
        .left
        .map(DatasetError.InvalidSpace.apply)
        .map(volumeSpace => DatasetShape(volumeSpace.toSampleSpace, timepoints))

  def unsafe(space: SomeSampleSpace, timepoints: Int): DatasetShape =
    make(space, timepoints).fold(error => throw new IllegalArgumentException(error.message), identity)
