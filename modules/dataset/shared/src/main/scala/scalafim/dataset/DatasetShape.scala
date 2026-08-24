package scalafim.dataset

import image4s.SampleSpace
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid
import scalafim.image.SomeSampleSpace
import scalafim.image.SampleSpaces
import scalafim.image.SampleSpaces.*

final case class DatasetShape private (
    space: SampleSpace[? <: Frame[D3], D3],
    timepoints: Int
):
  require(timepoints > 0, "timepoints must be positive")

  def grid: Grid[? <: Frame[D3], D3] = space.grid
  def spatialDims: Vector[Int] = space.spatialDims
  def spatialSize: Int = grid.shape.product

object DatasetShape:
  def make(space: SomeSampleSpace, timepoints: Int): Either[DatasetError, DatasetShape] =
    if timepoints <= 0 then Left(DatasetError.NonPositiveTimepoints(timepoints))
    else
      SampleSpaces
        .requireVolumeD3(space)
        .left
        .map(DatasetError.InvalidSpace.apply)
        .map(sampleSpace => DatasetShape(sampleSpace, timepoints))

  def unsafe(space: SomeSampleSpace, timepoints: Int): DatasetShape =
    make(space, timepoints).fold(error => throw new IllegalArgumentException(error.message), identity)
