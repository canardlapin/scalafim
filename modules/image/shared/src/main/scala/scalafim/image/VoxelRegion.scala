package scalafim.image

import ravel.Array1
import scala.annotation.targetName

final class VoxelRegion private (
    val space: VolumeSpace,
    private val members: VoxelIndexSet
):
  def size: Int =
    members.size

  def isEmpty: Boolean =
    members.isEmpty

  def contains(index: Int): Boolean =
    members.toVector.contains(index)

  def contains(coord: VoxelCoord): Boolean =
    Indexing.gridToIndexChecked(space.shape, coord).exists(contains)

  def linearIndices: Array1[Int] =
    members.indices

  def voxelCoords: Vector[VoxelCoord] =
    members.voxelCoords

  def union(that: VoxelRegion): Either[GridMismatch, VoxelRegion] =
    combine(that)(_ union _)

  def intersect(that: VoxelRegion): Either[GridMismatch, VoxelRegion] =
    combine(that)(_ intersect _)

  def diff(that: VoxelRegion): Either[GridMismatch, VoxelRegion] =
    combine(that)(_ diff _)

  def xor(that: VoxelRegion): Either[GridMismatch, VoxelRegion] =
    combine(that)((left, right) => (left diff right) union (right diff left))

  def complement: VoxelRegion =
    val selected = members.toVector.toSet
    val complement =
      Array.tabulate(space.nVoxels)(identity).filterNot(selected)
    new VoxelRegion(
      space,
      VoxelIndexSet.make(space, complement).toOption.get
    )

  def toSelection: VoxelSelection =
    VoxelSelection.fromRegion(this)

  private[image] def indexSet: VoxelIndexSet =
    members

  private def combine(
      that: VoxelRegion
  )(
      operation: (Set[Int], Set[Int]) => Set[Int]
  ): Either[GridMismatch, VoxelRegion] =
    GridCompatibility.volume(space, that.space).map: _ =>
      val combined =
        operation(members.toVector.toSet, that.members.toVector.toSet)
          .toArray
          .sorted
      new VoxelRegion(
        space,
        VoxelIndexSet.make(space, combined).toOption.get
      )

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
  def make(
      space: VolumeSpace,
      indices: Array1[Int]
  ): Either[VoxelIndexSetError, VoxelRegion] =
    VoxelIndexSet.make(space, indices).map(fromValidated)

  def make(
      space: VolumeSpace,
      indices: Array[Int]
  ): Either[VoxelIndexSetError, VoxelRegion] =
    VoxelIndexSet.make(space, indices).map(fromValidated)

  @targetName("makeFromNeuroSpace")
  def make(
      space: NeuroSpace,
      indices: Array1[Int]
  ): Either[VoxelIndexSetError, VoxelRegion] =
    VoxelIndexSet.make(space, indices).map(fromValidated)

  @targetName("makeArrayFromNeuroSpace")
  def make(
      space: NeuroSpace,
      indices: Array[Int]
  ): Either[VoxelIndexSetError, VoxelRegion] =
    VoxelIndexSet.make(space, indices).map(fromValidated)

  def fromRoi(roi: VoxelRoi): VoxelRegion =
    fromValidated(roi.linearIndexSet)

  def fromSelection(selection: VoxelSelection): VoxelRegion =
    selection.region

  def fromROICoords(
      space: VolumeSpace,
      coords: ROICoords
  ): Either[VoxelRoiError, VoxelRegion] =
    VoxelRoi.fromRaw(space, coords.coords).map(fromRoi)

  @targetName("fromROICoordsNeuroSpace")
  def fromROICoords(
      space: NeuroSpace,
      coords: ROICoords
  ): Either[VoxelRoiError, VoxelRegion] =
    VoxelRoi.fromRaw(space, coords.coords).map(fromRoi)

  private[image] def fromValidated(indexSet: VoxelIndexSet): VoxelRegion =
    new VoxelRegion(indexSet.space, indexSet)

final class VoxelSelection private (
    val space: VolumeSpace,
    private val ordered: VoxelIndexSet,
    val region: VoxelRegion
):
  def size: Int =
    ordered.size

  def isEmpty: Boolean =
    ordered.isEmpty

  def linearIndices: Array1[Int] =
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
      indices: Array1[Int]
  ): Either[VoxelIndexSetError, VoxelSelection] =
    VoxelIndexSet.makeUnique(space, indices).map(fromValidated)

  def make(
      space: VolumeSpace,
      indices: Array[Int]
  ): Either[VoxelIndexSetError, VoxelSelection] =
    VoxelIndexSet.makeUnique(space, indices).map(fromValidated)

  @targetName("makeFromNeuroSpace")
  def make(
      space: NeuroSpace,
      indices: Array1[Int]
  ): Either[VoxelIndexSetError, VoxelSelection] =
    VoxelIndexSet.makeUnique(space, indices).map(fromValidated)

  @targetName("makeArrayFromNeuroSpace")
  def make(
      space: NeuroSpace,
      indices: Array[Int]
  ): Either[VoxelIndexSetError, VoxelSelection] =
    VoxelIndexSet.makeUnique(space, indices).map(fromValidated)

  def fromRegion(region: VoxelRegion): VoxelSelection =
    val selection =
      VoxelIndexSet.makeUnique(region.space, region.linearIndices).toOption.get
    new VoxelSelection(region.space, selection, region)

  def fromRoi(roi: VoxelRoi): VoxelSelection =
    fromValidated(roi.linearIndexSet)

  def fromROICoords(
      space: VolumeSpace,
      coords: ROICoords
  ): Either[VoxelRoiError, VoxelSelection] =
    VoxelRoi.fromRaw(space, coords.coords).map(fromRoi)

  @targetName("fromROICoordsNeuroSpace")
  def fromROICoords(
      space: NeuroSpace,
      coords: ROICoords
  ): Either[VoxelRoiError, VoxelSelection] =
    VoxelRoi.fromRaw(space, coords.coords).map(fromRoi)

  private def fromValidated(indexSet: VoxelIndexSet): VoxelSelection =
    val canonical =
      VoxelIndexSet.make(indexSet.space, indexSet.indices).toOption.get
    new VoxelSelection(
      indexSet.space,
      indexSet,
      VoxelRegion.fromValidated(canonical)
    )
