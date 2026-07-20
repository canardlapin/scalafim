package scalafim.dataset.io

import scalafim.dataset.{DatasetAxis, DatasetError}

import java.util.Arrays

private[io] final class NiftiReadWindow private[io] (
    val startVoxel: Int,
    val endVoxelExclusive: Int,
    val byteCount: Int,
    val voxelOffsets: Array[Int],
    val outputColumns: Array[Int]
):
  require(startVoxel >= 0, "read window start voxel must be non-negative")
  require(endVoxelExclusive > startVoxel, "read window must contain at least one voxel")
  require(byteCount > 0, "read window byte count must be positive")
  require(voxelOffsets.nonEmpty, "read window must contain selected voxels")
  require(voxelOffsets.length == outputColumns.length, "read window mappings must align")

  def selectedVoxels: Int =
    voxelOffsets.length

private[io] final case class NiftiReadPlanStats(
    selectedVoxels: Int,
    windowsPerTimepoint: Int,
    plannedReads: Long,
    bytesPerTimepoint: Long,
    plannedBytes: Long,
    maxBufferBytes: Int
)

private[io] final class NiftiReadPlan private (
    val windows: Vector[NiftiReadWindow],
    val selectedVoxels: Int,
    val bytesPerValue: Int,
    val maxBufferBytes: Int
):
  require(windows.nonEmpty, "NIfTI read plan must contain windows")
  require(selectedVoxels > 0, "NIfTI read plan must select voxels")
  require(bytesPerValue > 0, "NIfTI read plan bytes per value must be positive")
  require(maxBufferBytes > 0, "NIfTI read plan buffer must be positive")

  def stats(timepoints: Int): NiftiReadPlanStats =
    require(timepoints >= 0, "timepoint count must be non-negative")
    val bytesPerTimepoint = windows.foldLeft(0L)(_ + _.byteCount.toLong)
    NiftiReadPlanStats(
      selectedVoxels = selectedVoxels,
      windowsPerTimepoint = windows.length,
      plannedReads = windows.length.toLong * timepoints.toLong,
      bytesPerTimepoint = bytesPerTimepoint,
      plannedBytes = bytesPerTimepoint * timepoints.toLong,
      maxBufferBytes = maxBufferBytes
    )

private[io] object NiftiReadPlan:
  val DefaultMaxGapBytes: Int = 4096
  val DefaultMaxWindowBytes: Int = 8 * 1024 * 1024

  def make(
      voxels: Vector[Int],
      bytesPerValue: Int,
      maxGapBytes: Int = DefaultMaxGapBytes,
      maxWindowBytes: Int = DefaultMaxWindowBytes
  ): Either[DatasetError, NiftiReadPlan] =
    if voxels.isEmpty then Left(DatasetError.EmptySelection(DatasetAxis.Voxel))
    else if bytesPerValue <= 0 then Left(DatasetError.StorageFailure(s"NIfTI bytes per value must be positive, got $bytesPerValue"))
    else if maxGapBytes < 0 then Left(DatasetError.StorageFailure(s"NIfTI maximum coalesced gap must be non-negative, got $maxGapBytes"))
    else if maxWindowBytes < bytesPerValue then
      Left(DatasetError.StorageFailure(s"NIfTI maximum read window $maxWindowBytes is smaller than one $bytesPerValue-byte value"))
    else
      val encoded = new Array[Long](voxels.length)
      var index = 0
      while index < voxels.length do
        val voxel = voxels(index)
        if voxel < 0 then return Left(DatasetError.NegativeIndex(DatasetAxis.Voxel, voxel))
        if voxel == Int.MaxValue then
          return Left(DatasetError.StorageFailure("NIfTI voxel index cannot be represented as an exclusive read-window bound"))
        encoded(index) = (voxel.toLong << 32) | (index.toLong & 0xffffffffL)
        index += 1
      Arrays.sort(encoded)

      index = 1
      while index < encoded.length do
        val previous = voxelOf(encoded(index - 1))
        val current = voxelOf(encoded(index))
        if current == previous then return Left(DatasetError.DuplicateSelection(DatasetAxis.Voxel, current))
        index += 1

      val windows = Vector.newBuilder[NiftiReadWindow]
      var from = 0
      var startVoxel = voxelOf(encoded(0))
      var previousVoxel = startVoxel
      index = 1
      while index < encoded.length do
        val voxel = voxelOf(encoded(index))
        val gapBytes = (voxel.toLong - previousVoxel.toLong - 1L) * bytesPerValue.toLong
        val candidateBytes = (voxel.toLong - startVoxel.toLong + 1L) * bytesPerValue.toLong
        if gapBytes > maxGapBytes.toLong || candidateBytes > maxWindowBytes.toLong then
          windows += buildWindow(encoded, from, index, bytesPerValue)
          from = index
          startVoxel = voxel
        previousVoxel = voxel
        index += 1
      windows += buildWindow(encoded, from, encoded.length, bytesPerValue)

      val built = windows.result()
      val largest = built.iterator.map(_.byteCount).max
      Right(new NiftiReadPlan(built, voxels.length, bytesPerValue, largest))

  private def buildWindow(
      encoded: Array[Long],
      from: Int,
      until: Int,
      bytesPerValue: Int
  ): NiftiReadWindow =
    val start = voxelOf(encoded(from))
    val end = voxelOf(encoded(until - 1)) + 1
    val bytes = Math.multiplyExact(end - start, bytesPerValue)
    val length = until - from
    val offsets = new Array[Int](length)
    val columns = new Array[Int](length)
    var index = 0
    while index < length do
      val value = encoded(from + index)
      offsets(index) = voxelOf(value) - start
      columns(index) = outputColumnOf(value)
      index += 1
    new NiftiReadWindow(start, end, bytes, offsets, columns)

  private inline def voxelOf(encoded: Long): Int =
    (encoded >>> 32).toInt

  private inline def outputColumnOf(encoded: Long): Int =
    encoded.toInt
