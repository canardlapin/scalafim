package scalafim.dataset

import scalafim.image.NeuroSpace

final case class DatasetShape(space: NeuroSpace, timepoints: Int):
  require(timepoints > 0, "timepoints must be positive")
  require(space.spatialDims.length == 3, "dataset space must be 3D")

  def spatialDims: Vector[Int] = space.spatialDims
  def spatialSize: Int = spatialDims.product
