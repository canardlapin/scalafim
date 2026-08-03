package scalafim.image

import ravel.Array1
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
import scala.annotation.targetName

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
    private val values: Array1[Int]
):
  def size: Int =
    values.size

  def isEmpty: Boolean =
    size == 0

  def apply(i: Int): Int =
    values(i)

  /** Immutable canonical storage; returning it is a zero-copy operation. */
  def indices: Array1[Int] =
    values

  private[image] def unsafeArray: Array1[Int] =
    values

  def toVector: Vector[Int] =
    Vector.tabulate(values.size)(i => values(i))

  def voxelCoords: Vector[VoxelCoord] =
    val shape = space.shape
    Vector.tabulate(values.size)(i => Indexing.indexToGrid3D(shape, values(i)))

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
  private def ravelIndices(rawIndices: Array[Int]): Array1[Int] =
    NDArray.fromSeq(Shape(rawIndices.length), rawIndices)

  def make(space: VolumeSpace, rawIndices: Array1[Int]): Either[VoxelIndexSetError, VoxelIndexSet] =
    build(space, rawIndices, canonicalize = true, rejectDuplicates = false)

  def make(space: VolumeSpace, rawIndices: Array[Int]): Either[VoxelIndexSetError, VoxelIndexSet] =
    make(space, ravelIndices(rawIndices))

  @targetName("makeFromNeuroSpace")
  def make(space: NeuroSpace, rawIndices: Array1[Int]): Either[VoxelIndexSetError, VoxelIndexSet] =
    VolumeSpace.fromSpatialPart(space).left.map(VoxelIndexSetError.InvalidSpace.apply).flatMap(make(_, rawIndices))

  @targetName("makeArrayFromNeuroSpace")
  def make(space: NeuroSpace, rawIndices: Array[Int]): Either[VoxelIndexSetError, VoxelIndexSet] =
    make(space, ravelIndices(rawIndices))

  def makeUnique(space: VolumeSpace, rawIndices: Array1[Int]): Either[VoxelIndexSetError, VoxelIndexSet] =
    build(space, rawIndices, canonicalize = false, rejectDuplicates = true)

  def makeUnique(space: VolumeSpace, rawIndices: Array[Int]): Either[VoxelIndexSetError, VoxelIndexSet] =
    makeUnique(space, ravelIndices(rawIndices))

  @targetName("makeUniqueFromNeuroSpace")
  def makeUnique(space: NeuroSpace, rawIndices: Array1[Int]): Either[VoxelIndexSetError, VoxelIndexSet] =
    VolumeSpace.fromSpatialPart(space).left.map(VoxelIndexSetError.InvalidSpace.apply).flatMap(makeUnique(_, rawIndices))

  @targetName("makeUniqueArrayFromNeuroSpace")
  def makeUnique(space: NeuroSpace, rawIndices: Array[Int]): Either[VoxelIndexSetError, VoxelIndexSet] =
    makeUnique(space, ravelIndices(rawIndices))

  def apply(space: VolumeSpace, rawIndices: Array1[Int]): VoxelIndexSet =
    make(space, rawIndices).fold(err => throw new IllegalArgumentException(err.message), indexSet => indexSet)

  def apply(space: VolumeSpace, rawIndices: Array[Int]): VoxelIndexSet =
    apply(space, ravelIndices(rawIndices))

  @targetName("applyNeuroSpace")
  def apply(space: NeuroSpace, rawIndices: Array1[Int]): VoxelIndexSet =
    make(space, rawIndices).fold(err => throw new IllegalArgumentException(err.message), indexSet => indexSet)

  @targetName("applyArrayNeuroSpace")
  def apply(space: NeuroSpace, rawIndices: Array[Int]): VoxelIndexSet =
    apply(space, ravelIndices(rawIndices))

  def unique(space: VolumeSpace, rawIndices: Array1[Int]): VoxelIndexSet =
    makeUnique(space, rawIndices).fold(err => throw new IllegalArgumentException(err.message), indexSet => indexSet)

  def unique(space: VolumeSpace, rawIndices: Array[Int]): VoxelIndexSet =
    unique(space, ravelIndices(rawIndices))

  @targetName("uniqueFromNeuroSpace")
  def unique(space: NeuroSpace, rawIndices: Array1[Int]): VoxelIndexSet =
    makeUnique(space, rawIndices).fold(err => throw new IllegalArgumentException(err.message), indexSet => indexSet)

  @targetName("uniqueArrayFromNeuroSpace")
  def unique(space: NeuroSpace, rawIndices: Array[Int]): VoxelIndexSet =
    unique(space, ravelIndices(rawIndices))

  private[image] def unsafe(space: VolumeSpace, values: Array1[Int]): VoxelIndexSet =
    new VoxelIndexSet(space, values)

  private def build(
      space: VolumeSpace,
      rawIndices: Array1[Int],
      canonicalize: Boolean,
      rejectDuplicates: Boolean
  ): Either[VoxelIndexSetError, VoxelIndexSet] =
    val nVoxels = space.nVoxels
    val seen = scala.collection.mutable.HashSet.empty[Int]
    val tmp = Array.ofDim[Int](rawIndices.size)
    var outLen = 0
    var i = 0
    var error = Option.empty[VoxelIndexSetError]

    while i < rawIndices.size && error.isEmpty do
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
        val retained =
          if !canonicalize && outLen == rawIndices.size then rawIndices
          else NDArray.fromSeq(Shape(outLen), out)
        Right(new VoxelIndexSet(space, retained))
