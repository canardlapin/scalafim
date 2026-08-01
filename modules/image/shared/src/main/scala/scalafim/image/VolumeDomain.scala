package scalafim.image

import ravel.NDArray
import ravel.Shape
import scalafim.locus.*

enum VolumeIdentityBasis:
  case Semantic
  case StructuralCompatibility

final class VolumeDomain[S] private[image] (
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
        .make(
          volumeSpace,
          NDArray.fromSeq(
            Shape(region.cardinality),
            region.ordinalsInDomainOrder
          )
        )
        .toOption
        .get

  def voxelSelection(selection: Selection[S]): Either[SpaceMismatch, VoxelSelection] =
    checkSpace(selection.space).map: _ =>
      VoxelSelection
        .make(
          volumeSpace,
          NDArray.fromSeq(Shape(selection.size), selection.ordinals)
        )
        .toOption
        .get

  def indexedField[A](
      volume: NeuroVol[A]
  ): Either[GridMismatch, IndexedField[S, A]] =
    GridCompatibility.volume(volumeSpace, volume.volumeSpace).map: _ =>
      IndexedField.tabulate(finiteSpace)(point => volume.linear(point.value))

  def supportWhere[A](
      field: IndexedField[S, A]
  )(
      predicate: A => Boolean
  ): Either[SpaceMismatch, Region[S]] =
    checkSpace(field.space).map: _ =>
      Region.tabulate(finiteSpace)(point => predicate(field(point)))

  /** Regions, selections and fields are indexed by a [[FiniteDomain]], which
    * need not be a persistent [[FiniteSpace]]. Identity is checked by runtime
    * owner either way, so the weaker bound is the honest one.
    */
  private def checkSpace[T](actual: FiniteDomain[T]): Either[SpaceMismatch, Unit] =
    if finiteSpace.sameIdentityAs(actual) then Right(())
    else
      Left(mismatch(finiteSpace, actual))

object VolumeDomain:
  def semantic(
      key: SpaceKey,
      volumeSpace: VolumeSpace
  ): SomeVolumeDomain =
    SomeVolumeDomain.make(key, volumeSpace, VolumeIdentityBasis.Semantic)

  def structuralCompatibility(
      volumeSpace: VolumeSpace
  ): SomeVolumeDomain =
    SomeVolumeDomain.make(
      SpaceKey.unsafe:
        s"scalafim:image:structural:${volumeSpace.toNeuroSpace}",
      volumeSpace,
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
    make(key, volumeSpace, VolumeIdentityBasis.Semantic)

  private[image] def make(
      key: SpaceKey,
      volumeSpace: VolumeSpace,
      basis: VolumeIdentityBasis
  ): SomeVolumeDomain =
    val resolution =
      DomainFactory.unsafeRestore(key, volumeSpace.nVoxels)
    new SomeVolumeDomain:
      type S = resolution.S
      val value: VolumeDomain[S] =
        new VolumeDomain(
          volumeSpace,
          resolution.space,
          basis
        )
