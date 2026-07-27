package scalafim.dataset

import scalafim.image.Mask

final class VoxelSampleMap private (
    val sampleVoxels: Vector[VoxelIndex],
    private val voxelToSample: Array[Int],
    val spatialSize: Int
):
  def nSamples: Int =
    sampleVoxels.length

  def sampleFor(voxel: VoxelIndex): Either[DatasetError, Int] =
    val value = VoxelIndex.raw(voxel)
    if value >= spatialSize then Left(DatasetError.IndexOutOfBounds(DatasetAxis.Voxel, value, spatialSize))
    else
      val sample = voxelToSample(value)
      if sample < 0 then Left(DatasetError.VoxelOutsideMask(value))
      else Right(sample)

  def samplesFor(voxels: Vector[VoxelIndex]): Either[DatasetError, Vector[Int]] =
    val out = Vector.newBuilder[Int]
    out.sizeHint(voxels.length)
    var i = 0
    var error = Option.empty[DatasetError]
    while i < voxels.length && error.isEmpty do
      sampleFor(voxels(i)) match
        case Left(err) => error = Some(err)
        case Right(sample) => out += sample
      i += 1

    error match
      case Some(err) => Left(err)
      case None => Right(out.result())

object VoxelSampleMap:
  def fromMask(
      mask: Mask.MaskVol,
      expectedSamples: Int
  ): Either[DatasetError, VoxelSampleMap] =
    for
      volume <- mask.space.asVolumeSpace.left.map(DatasetError.InvalidSpace.apply)
      map <- fromMaskIndices(Mask.indices(mask), volume.nVoxels, expectedSamples)
    yield map

  private def fromMaskIndices(
      indices: narr.NArray[Int],
      spatialSize: Int,
      expectedSamples: Int
  ): Either[DatasetError, VoxelSampleMap] =
    if expectedSamples <= 0 then Left(DatasetError.ShapeMismatch(s"response sample count must be positive, got $expectedSamples"))
    else if indices.length != expectedSamples then
      Left(DatasetError.ShapeMismatch(s"response sample count must match mask cardinality: expected $expectedSamples samples but mask has ${indices.length}"))
    else
      val lookup = Array.fill(spatialSize)(-1)
      val sampleVoxels = Vector.newBuilder[VoxelIndex]
      sampleVoxels.sizeHint(indices.length)
      var i = 0
      var error = Option.empty[DatasetError]
      while i < indices.length && error.isEmpty do
        val voxel = indices(i)
        if voxel < 0 || voxel >= spatialSize then
          error = Some(DatasetError.IndexOutOfBounds(DatasetAxis.Voxel, voxel, spatialSize))
        else if lookup(voxel) >= 0 then
          error = Some(DatasetError.DuplicateSelection(DatasetAxis.Voxel, voxel))
        else
          lookup(voxel) = i
          sampleVoxels += VoxelIndex.unsafe(voxel)
        i += 1

      error match
        case Some(err) => Left(err)
        case None => Right(new VoxelSampleMap(sampleVoxels.result(), lookup, spatialSize))
