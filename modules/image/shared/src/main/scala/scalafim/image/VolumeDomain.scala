package scalafim.image

import scalafim.locus.*

enum VolumeIdentityBasis:
  case Semantic
  case StructuralCompatibility

sealed trait StructuralCompatibilityVoxel

final class VolumeDomain[S] private (
    val volumeSpace: VolumeSpace,
    val finiteSpace: FiniteSpace[S],
    val identityBasis: VolumeIdentityBasis
):
  def region(voxelRegion: VoxelRegion): Either[GridMismatch, Region[S]] =
    GridCompatibility.volume(volumeSpace, voxelRegion.space).map: _ =>
      Region
        .fromOrdinals(finiteSpace, voxelRegion.linearIndices.iterator)
        .toOption
        .get

  def selection(voxelSelection: VoxelSelection): Either[GridMismatch, Selection[S]] =
    GridCompatibility.volume(volumeSpace, voxelSelection.space).map: _ =>
      Selection
        .fromOrdinals(finiteSpace, voxelSelection.linearIndices.iterator)
        .toOption
        .get

  def voxelRegion(region: Region[S]): Either[SpaceMismatch, VoxelRegion] =
    checkSpace(region.space).map: _ =>
      VoxelRegion
        .make(volumeSpace, NArrayUtil.fromArray(region.ordinalsInDomainOrder))
        .toOption
        .get

  def voxelSelection(selection: Selection[S]): Either[SpaceMismatch, VoxelSelection] =
    checkSpace(selection.space).map: _ =>
      VoxelSelection
        .make(volumeSpace, NArrayUtil.fromArray(selection.ordinals))
        .toOption
        .get

  def indexedField[A](
      volume: NeuroVol[A]
  ): Either[GridMismatch, IndexedField[S, A]] =
    GridCompatibility.volume(volumeSpace, volume.volumeSpace).map: _ =>
      IndexedField.tabulate(finiteSpace)(point => volume.linear(point.ordinal))

  def supportWhere[A](
      field: IndexedField[S, A]
  )(
      predicate: A => Boolean
  ): Either[SpaceMismatch, Region[S]] =
    checkSpace(field.space).map: _ =>
      Region.tabulate(finiteSpace)(point => predicate(field(point)))

  private def checkSpace[T](actual: FiniteSpace[T]): Either[SpaceMismatch, Unit] =
    if finiteSpace.sameIdentityAs(actual) then Right(())
    else
      Left:
        SpaceMismatch(
          finiteSpace.key,
          finiteSpace.size,
          actual.key,
          actual.size
        )

object VolumeDomain:
  def semantic[S](
      key: SpaceKey,
      volumeSpace: VolumeSpace
  ): VolumeDomain[S] =
    new VolumeDomain(
      volumeSpace,
      FiniteSpace.make[S](key, volumeSpace.nVoxels).toOption.get,
      VolumeIdentityBasis.Semantic
    )

  def structuralCompatibility(
      volumeSpace: VolumeSpace
  ): VolumeDomain[StructuralCompatibilityVoxel] =
    new VolumeDomain(
      volumeSpace,
      FiniteSpace
        .make[StructuralCompatibilityVoxel](
          StructuralVolumeLocus.key(volumeSpace),
          volumeSpace.nVoxels
        )
        .toOption
        .get,
      VolumeIdentityBasis.StructuralCompatibility
    )

trait SomeVolumeDomain:
  type S
  val value: VolumeDomain[S]

object SomeVolumeDomain:
  def semantic(
      key: SpaceKey,
      volumeSpace: VolumeSpace
  ): SomeVolumeDomain =
    final class VolumeVoxel
    val domain = VolumeDomain.semantic[VolumeVoxel](key, volumeSpace)
    new SomeVolumeDomain:
      type S = VolumeVoxel
      val value: VolumeDomain[VolumeVoxel] = domain
