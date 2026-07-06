package scalafim.dataset

import scalafim.image.DMat

final case class FmriSeries(
    data: DMat,
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    shape: DatasetShape,
    metadata: DatasetMetadata = DatasetMetadata.Empty
):
  require(data.rows == timepoints.length, "series rows must match selected timepoints")
  require(data.cols == voxelIndices.length, "series columns must match selected voxels")
  require(timepoints.forall(t => t >= 0 && t < shape.timepoints), "timepoint index out of bounds")
  require(voxelIndices.forall(v => v >= 0 && v < shape.spatialSize), "voxel index out of bounds")

  def nTimepoints: Int = data.rows
  def nVoxels: Int = data.cols
