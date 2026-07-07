package scalafim.image

import narr.NArray

enum VoxelIndexSetError:
  case OutOfBounds(position: Int, index: Int, size: Int)
  case DuplicateIndex(index: Int)
  case InvalidSpace(error: NeuroSpaceError)

  def message: String =
    this match
      case OutOfBounds(position, index, size) =>
        s"voxel index at position $position out of bounds: $index not in [0, $size)"
      case DuplicateIndex(index) =>
        s"voxel index set contains duplicate index $index"
      case InvalidSpace(error) =>
        error.message

final class VoxelIndexSet private (
    val space: VolumeSpace,
    private val values: NArray[Int]
):
  def size: Int =
    values.length

  def isEmpty: Boolean =
    size == 0

  def apply(i: Int): Int =
    values(i)

  def indices: NArray[Int] =
    val out = narr.NArray.ofSize[Int](values.length)
    var i = 0
    while i < values.length do
      out(i) = values(i)
      i += 1
    out

  private[image] def unsafeArray: NArray[Int] =
    values

  def toVector: Vector[Int] =
    Vector.tabulate(values.length)(i => values(i))

  def voxelCoords: Vector[VoxelCoord] =
    val shape = space.shape
    Vector.tabulate(values.length)(i => Indexing.indexToGrid3D(shape, values(i)))

  override def equals(other: Any): Boolean =
    other match
      case that: VoxelIndexSet =>
        this.space == that.space && this.toVector == that.toVector
      case _ => false

  override def hashCode(): Int =
    31 * space.hashCode() + toVector.hashCode()

  override def toString: String =
    s"VoxelIndexSet(size=$size, dims=${space.dims})"

object VoxelIndexSet:
  def make(space: VolumeSpace, rawIndices: NArray[Int]): Either[VoxelIndexSetError, VoxelIndexSet] =
    build(space, rawIndices, canonicalize = true, rejectDuplicates = false)

  def make(space: NeuroSpace, rawIndices: NArray[Int]): Either[VoxelIndexSetError, VoxelIndexSet] =
    VolumeSpace.fromSpatialPart(space).left.map(VoxelIndexSetError.InvalidSpace.apply).flatMap(make(_, rawIndices))

  def makeUnique(space: VolumeSpace, rawIndices: NArray[Int]): Either[VoxelIndexSetError, VoxelIndexSet] =
    build(space, rawIndices, canonicalize = false, rejectDuplicates = true)

  def makeUnique(space: NeuroSpace, rawIndices: NArray[Int]): Either[VoxelIndexSetError, VoxelIndexSet] =
    VolumeSpace.fromSpatialPart(space).left.map(VoxelIndexSetError.InvalidSpace.apply).flatMap(makeUnique(_, rawIndices))

  def apply(space: VolumeSpace, rawIndices: NArray[Int]): VoxelIndexSet =
    make(space, rawIndices).fold(err => throw new IllegalArgumentException(err.message), indexSet => indexSet)

  def apply(space: NeuroSpace, rawIndices: NArray[Int]): VoxelIndexSet =
    make(space, rawIndices).fold(err => throw new IllegalArgumentException(err.message), indexSet => indexSet)

  def unique(space: VolumeSpace, rawIndices: NArray[Int]): VoxelIndexSet =
    makeUnique(space, rawIndices).fold(err => throw new IllegalArgumentException(err.message), indexSet => indexSet)

  def unique(space: NeuroSpace, rawIndices: NArray[Int]): VoxelIndexSet =
    makeUnique(space, rawIndices).fold(err => throw new IllegalArgumentException(err.message), indexSet => indexSet)

  private[image] def unsafe(space: VolumeSpace, values: NArray[Int]): VoxelIndexSet =
    new VoxelIndexSet(space, values)

  private def build(
      space: VolumeSpace,
      rawIndices: NArray[Int],
      canonicalize: Boolean,
      rejectDuplicates: Boolean
  ): Either[VoxelIndexSetError, VoxelIndexSet] =
    val nVoxels = space.nVoxels
    val seen = scala.collection.mutable.HashSet.empty[Int]
    val tmp = Array.ofDim[Int](rawIndices.length)
    var outLen = 0
    var i = 0
    var error = Option.empty[VoxelIndexSetError]

    while i < rawIndices.length && error.isEmpty do
      val idx = rawIndices(i)
      if idx < 0 || idx >= nVoxels then
        error = Some(VoxelIndexSetError.OutOfBounds(i, idx, nVoxels))
      else if seen.contains(idx) then
        if rejectDuplicates then error = Some(VoxelIndexSetError.DuplicateIndex(idx))
      else
        seen += idx
        tmp(outLen) = idx
        outLen += 1
      i += 1

    error match
      case Some(err) => Left(err)
      case None =>
        val out =
          if canonicalize then tmp.take(outLen).sorted
          else tmp.take(outLen)
        Right(new VoxelIndexSet(space, NArrayUtil.fromArray(out)))
