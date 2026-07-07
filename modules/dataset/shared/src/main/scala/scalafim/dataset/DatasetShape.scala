package scalafim.dataset

import scalafim.image.{NeuroSpace, VolumeSpace}

final case class DatasetShape(space: NeuroSpace, timepoints: Int):
  require(timepoints > 0, "timepoints must be positive")
  require(space.ndim == 3, "dataset space must be exactly 3D")

  def volumeSpace: VolumeSpace =
    VolumeSpace.unsafe(space)
  def spatialDims: Vector[Int] = space.spatialDims
  def spatialSize: Int = volumeSpace.nVoxels

object DatasetShape:
  def make(space: NeuroSpace, timepoints: Int): Either[DatasetError, DatasetShape] =
    if timepoints <= 0 then Left(DatasetError.NonPositiveTimepoints(timepoints))
    else
      space.asVolumeSpace
        .left
        .map(DatasetError.InvalidSpace.apply)
        .map(volumeSpace => DatasetShape(volumeSpace.toNeuroSpace, timepoints))

  def unsafe(space: NeuroSpace, timepoints: Int): DatasetShape =
    make(space, timepoints).fold(error => throw new IllegalArgumentException(error.message), identity)
