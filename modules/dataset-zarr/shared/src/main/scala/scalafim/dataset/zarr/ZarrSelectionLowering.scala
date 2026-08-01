package scalafim.dataset.zarr

import scalafim.dataset.*
import zarr4s.*

object ZarrSelectionLowering:
  /** Lowers ordered dataset selections into canonical BOLD [t,z,y,x] points.
    * Point order is time-major and then caller voxel order, exactly matching
    * the row-major `FmriSeries` result contract.
    */
  def canonicalPoints(
      canonicalShape: Shape,
      selection: ResolvedDataSelection
  ): Either[DatasetError, CoordinateBatch] =
    if canonicalShape.rank.toInt != 4 then
      Left(DatasetError.ShapeMismatch("canonical Zarr array must have shape [t,z,y,x]"))
    else
      val zSize = canonicalShape.axis(1)
      val ySize = canonicalShape.axis(2)
      val xSize = canonicalShape.axis(3)
      val plane = checkedProduct(xSize, ySize, "x by y") match
        case Left(error) => return Left(error)
        case Right(found) => found
      val spatial = checkedProduct(plane, zSize, "x by y by z") match
        case Left(error) => return Left(error)
        case Right(found) => found
      if spatial > Int.MaxValue.toLong then
        return Left(DatasetError.ShapeMismatch(
          s"canonical spatial size $spatial exceeds the dataset index boundary"
        ))
      val coordinates = Vector.newBuilder[Coordinate]
      var timeIndex = 0
      while timeIndex < selection.timepoints.length do
        val time = selection.timepoints(timeIndex).toLong
        var voxelIndex = 0
        while voxelIndex < selection.voxels.length do
          val voxel = selection.voxels(voxelIndex).toLong
          if voxel < 0L || voxel >= spatial then
            return Left(DatasetError.IndexOutOfBounds(DatasetAxis.Voxel, selection.voxels(voxelIndex), spatial.toInt))
          val z = voxel / plane
          val withinPlane = voxel % plane
          val y = withinPlane / xSize
          val x = withinPlane % xSize
          Coordinate(time, z, y, x) match
            case Left(error) => return Left(DatasetError.StorageFailure(error.message))
            case Right(found) => coordinates += found
          voxelIndex += 1
        timeIndex += 1
      CoordinateBatch.within(canonicalShape, coordinates.result())
        .left.map(error => DatasetError.StorageFailure(error.message))

  private def checkedProduct(
      left: Long,
      right: Long,
      subject: String
  ): Either[DatasetError, Long] =
    if left != 0L && right > Long.MaxValue / left then
      Left(DatasetError.ShapeMismatch(s"canonical spatial size overflows Long while computing $subject"))
    else Right(left * right)
