package scalafim.image

import narr.NArray

final class VoxelRegion private (
    val space: VolumeSpace,
    private val members: VoxelIndexSet
):
  def size: Int =
    members.size

  def isEmpty: Boolean =
    members.isEmpty

  def contains(index: Int): Boolean =
    val values = members.unsafeArray
    var low = 0
    var high = values.length - 1
    var found = false
    while low <= high && !found do
      val middle = low + (high - low) / 2
      val value = values(middle)
      if value == index then found = true
      else if value < index then low = middle + 1
      else high = middle - 1
    found

  def contains(coord: VoxelCoord): Boolean =
    Indexing.gridToIndexChecked(space.shape, coord).exists(contains)

  def linearIndices: NArray[Int] =
    members.indices

  def voxelCoords: Vector[VoxelCoord] =
    members.voxelCoords

  def union(that: VoxelRegion): Either[GridMismatch, VoxelRegion] =
    combine(that, VoxelRegion.Operation.Union)

  def intersect(that: VoxelRegion): Either[GridMismatch, VoxelRegion] =
    combine(that, VoxelRegion.Operation.Intersection)

  def diff(that: VoxelRegion): Either[GridMismatch, VoxelRegion] =
    combine(that, VoxelRegion.Operation.Difference)

  def xor(that: VoxelRegion): Either[GridMismatch, VoxelRegion] =
    combine(that, VoxelRegion.Operation.SymmetricDifference)

  def complement: VoxelRegion =
    val values = members.unsafeArray
    val out = Array.newBuilder[Int]
    out.sizeHint(space.nVoxels - values.length)
    var voxel = 0
    var member = 0
    while voxel < space.nVoxels do
      if member < values.length && values(member) == voxel then member += 1
      else out += voxel
      voxel += 1
    VoxelRegion.fromSorted(space, out.result())

  def toSelection: VoxelSelection =
    VoxelSelection.fromRegion(this)

  private[image] def indexSet: VoxelIndexSet =
    members

  private def combine(
      that: VoxelRegion,
      operation: VoxelRegion.Operation
  ): Either[GridMismatch, VoxelRegion] =
    GridCompatibility.volume(space, that.space).map: _ =>
      val left = members.unsafeArray
      val right = that.members.unsafeArray
      val out = Array.newBuilder[Int]
      out.sizeHint(operation.maximumSize(left.length, right.length))
      var i = 0
      var j = 0

      while i < left.length && j < right.length do
        val leftValue = left(i)
        val rightValue = right(j)
        if leftValue < rightValue then
          if operation.includeLeftOnly then out += leftValue
          i += 1
        else if rightValue < leftValue then
          if operation.includeRightOnly then out += rightValue
          j += 1
        else
          if operation.includeShared then out += leftValue
          i += 1
          j += 1

      if operation.includeLeftOnly then
        while i < left.length do
          out += left(i)
          i += 1

      if operation.includeRightOnly then
        while j < right.length do
          out += right(j)
          j += 1

      VoxelRegion.fromSorted(space, out.result())

  override def equals(other: Any): Boolean =
    other match
      case that: VoxelRegion =>
        space == that.space && members == that.members
      case _ => false

  override def hashCode(): Int =
    31 * space.hashCode() + members.hashCode()

  override def toString: String =
    s"VoxelRegion(size=$size, dims=${space.dims})"

object VoxelRegion:
  private enum Operation:
    case Union
    case Intersection
    case Difference
    case SymmetricDifference

    def includeLeftOnly: Boolean =
      this match
        case Union | Difference | SymmetricDifference => true
        case Intersection => false

    def includeRightOnly: Boolean =
      this match
        case Union | SymmetricDifference => true
        case Intersection | Difference => false

    def includeShared: Boolean =
      this match
        case Union | Intersection => true
        case Difference | SymmetricDifference => false

    def maximumSize(left: Int, right: Int): Int =
      this match
        case Union | SymmetricDifference => left + right
        case Intersection => math.min(left, right)
        case Difference => left

  def make(
      space: VolumeSpace,
      indices: NArray[Int]
  ): Either[VoxelIndexSetError, VoxelRegion] =
    VoxelIndexSet.make(space, indices).map(indexSet => new VoxelRegion(space, indexSet))

  def make(
      space: NeuroSpace,
      indices: NArray[Int]
  ): Either[VoxelIndexSetError, VoxelRegion] =
    VoxelIndexSet.make(space, indices).map(indexSet => new VoxelRegion(indexSet.space, indexSet))

  def fromRoi(roi: VoxelRoi): VoxelRegion =
    fromValidated(roi.linearIndexSet)

  def fromSelection(selection: VoxelSelection): VoxelRegion =
    selection.region

  def fromROICoords(
      space: VolumeSpace,
      coords: ROICoords
  ): Either[VoxelRoiError, VoxelRegion] =
    VoxelRoi.fromRaw(space, coords.coords).map(fromRoi)

  def fromROICoords(
      space: NeuroSpace,
      coords: ROICoords
  ): Either[VoxelRoiError, VoxelRegion] =
    VoxelRoi.fromRaw(space, coords.coords).map(fromRoi)

  private[image] def fromValidated(indexSet: VoxelIndexSet): VoxelRegion =
    val sorted = indexSet.toVector.sorted.toArray
    fromSorted(indexSet.space, sorted)

  private def fromSorted(space: VolumeSpace, values: Array[Int]): VoxelRegion =
    new VoxelRegion(space, VoxelIndexSet.unsafe(space, NArrayUtil.fromArray(values)))

final class VoxelSelection private (
    val space: VolumeSpace,
    private val ordered: VoxelIndexSet,
    val region: VoxelRegion
):
  def size: Int =
    ordered.size

  def isEmpty: Boolean =
    ordered.isEmpty

  def linearIndices: NArray[Int] =
    ordered.indices

  def voxelCoords: Vector[VoxelCoord] =
    ordered.voxelCoords

  def toVoxelRoi: VoxelRoi =
    VoxelRoi.fromSelection(this)

  private[image] def indexSet: VoxelIndexSet =
    ordered

  override def equals(other: Any): Boolean =
    other match
      case that: VoxelSelection =>
        space == that.space && ordered == that.ordered
      case _ => false

  override def hashCode(): Int =
    31 * space.hashCode() + ordered.hashCode()

  override def toString: String =
    s"VoxelSelection(size=$size, dims=${space.dims})"

object VoxelSelection:
  def make(
      space: VolumeSpace,
      indices: NArray[Int]
  ): Either[VoxelIndexSetError, VoxelSelection] =
    VoxelIndexSet.makeUnique(space, indices).map(fromValidated)

  def make(
      space: NeuroSpace,
      indices: NArray[Int]
  ): Either[VoxelIndexSetError, VoxelSelection] =
    VoxelIndexSet.makeUnique(space, indices).map(fromValidated)

  def fromRegion(region: VoxelRegion): VoxelSelection =
    new VoxelSelection(region.space, region.indexSet, region)

  def fromRoi(roi: VoxelRoi): VoxelSelection =
    fromValidated(roi.linearIndexSet)

  def fromROICoords(
      space: VolumeSpace,
      coords: ROICoords
  ): Either[VoxelRoiError, VoxelSelection] =
    VoxelRoi.fromRaw(space, coords.coords).map(fromRoi)

  def fromROICoords(
      space: NeuroSpace,
      coords: ROICoords
  ): Either[VoxelRoiError, VoxelSelection] =
    VoxelRoi.fromRaw(space, coords.coords).map(fromRoi)

  private def fromValidated(indexSet: VoxelIndexSet): VoxelSelection =
    new VoxelSelection(indexSet.space, indexSet, VoxelRegion.fromValidated(indexSet))
