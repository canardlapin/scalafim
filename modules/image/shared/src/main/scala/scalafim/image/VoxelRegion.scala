package scalafim.image

import narr.NArray
import scalafim.locus.{Region as LocusRegion, Selection as LocusSelection}

private[image] sealed trait StructuralVolumeVoxel

private[image] object StructuralVolumeLocus:
  def space(volumeSpace: VolumeSpace): scalafim.locus.FiniteSpace[StructuralVolumeVoxel] =
    scalafim.locus.FiniteSpace
      .make[StructuralVolumeVoxel](key(volumeSpace), volumeSpace.nVoxels)
      .toOption
      .get

  def key(volumeSpace: VolumeSpace): scalafim.locus.SpaceKey =
    scalafim.locus.SpaceKey.unsafe:
      s"scalafim:image:structural:${volumeSpace.toNeuroSpace}"

final class VoxelRegion private (
    val space: VolumeSpace,
    private val members: LocusRegion[StructuralVolumeVoxel]
):
  def size: Int =
    members.cardinality

  def isEmpty: Boolean =
    members.isEmpty

  def contains(index: Int): Boolean =
    members.space.point(index).exists(members.contains)

  def contains(coord: VoxelCoord): Boolean =
    Indexing.gridToIndexChecked(space.shape, coord).exists(contains)

  def linearIndices: NArray[Int] =
    NArrayUtil.fromArray(members.ordinalsInDomainOrder)

  def voxelCoords: Vector[VoxelCoord] =
    members.ordinalsInDomainOrder.toVector.map(Indexing.indexToGrid3D(space.shape, _))

  def union(that: VoxelRegion): Either[GridMismatch, VoxelRegion] =
    combine(that)(_.union(_))

  def intersect(that: VoxelRegion): Either[GridMismatch, VoxelRegion] =
    combine(that)(_.intersect(_))

  def diff(that: VoxelRegion): Either[GridMismatch, VoxelRegion] =
    combine(that)(_.diff(_))

  def xor(that: VoxelRegion): Either[GridMismatch, VoxelRegion] =
    combine(that)(_.xor(_))

  def complement: VoxelRegion =
    new VoxelRegion(space, members.complement)

  def toSelection: VoxelSelection =
    VoxelSelection.fromRegion(this)

  private[image] def indexSet: VoxelIndexSet =
    VoxelIndexSet.unsafe(space, linearIndices)

  private[image] def locusRegion: LocusRegion[StructuralVolumeVoxel] =
    members

  private def combine(
      that: VoxelRegion
  )(
      operation: (
          LocusRegion[StructuralVolumeVoxel],
          LocusRegion[StructuralVolumeVoxel]
      ) => Either[scalafim.locus.SpaceMismatch, LocusRegion[StructuralVolumeVoxel]]
  ): Either[GridMismatch, VoxelRegion] =
    GridCompatibility.volume(space, that.space).map: _ =>
      val combined = operation(members, that.members).toOption.get
      new VoxelRegion(space, combined)

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
      indices: NArray[Int]
  ): Either[VoxelIndexSetError, VoxelRegion] =
    VoxelIndexSet.make(space, indices).map(fromValidated)

  def make(
      space: NeuroSpace,
      indices: NArray[Int]
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

  def fromROICoords(
      space: NeuroSpace,
      coords: ROICoords
  ): Either[VoxelRoiError, VoxelRegion] =
    VoxelRoi.fromRaw(space, coords.coords).map(fromRoi)

  private[image] def fromValidated(indexSet: VoxelIndexSet): VoxelRegion =
    val locusSpace = StructuralVolumeLocus.space(indexSet.space)
    val region =
      LocusRegion.fromOrdinals(locusSpace, indexSet.toVector).toOption.get
    new VoxelRegion(indexSet.space, region)

  private[image] def fromLocus(
      space: VolumeSpace,
      region: LocusRegion[StructuralVolumeVoxel]
  ): VoxelRegion =
    new VoxelRegion(space, region)

final class VoxelSelection private (
    val space: VolumeSpace,
    private val ordered: LocusSelection[StructuralVolumeVoxel],
    val region: VoxelRegion
):
  def size: Int =
    ordered.size

  def isEmpty: Boolean =
    ordered.isEmpty

  def linearIndices: NArray[Int] =
    NArrayUtil.fromArray(ordered.ordinals)

  def voxelCoords: Vector[VoxelCoord] =
    ordered.ordinals.toVector.map(Indexing.indexToGrid3D(space.shape, _))

  def toVoxelRoi: VoxelRoi =
    VoxelRoi.fromSelection(this)

  private[image] def indexSet: VoxelIndexSet =
    VoxelIndexSet.unsafe(space, linearIndices)

  private[image] def locusSelection: LocusSelection[StructuralVolumeVoxel] =
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
    val selection = LocusSelection.fromRegion(region.locusRegion)
    new VoxelSelection(region.space, selection, region)

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
    val locusSpace = StructuralVolumeLocus.space(indexSet.space)
    val selection =
      LocusSelection.fromOrdinals(locusSpace, indexSet.toVector).toOption.get
    val region = VoxelRegion.fromLocus(
      indexSet.space,
      selection.region
    )
    new VoxelSelection(indexSet.space, selection, region)
