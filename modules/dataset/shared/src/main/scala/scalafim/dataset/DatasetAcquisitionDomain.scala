package scalafim.dataset

import scalafim.locus.{
  FiniteSpace,
  Injection,
  Point,
  Selection,
  SpaceKey,
  SpaceMismatch,
  TotalMap
}

enum DatasetIdentityBasis:
  case Semantic(dataset: DatasetId)
  case StructuralCompatibility

trait DatasetAcquisitionDomain:
  type T
  type X
  type A

  val shape: DatasetShape
  val voxelDomain: VoxelDomain
  val identityBasis: DatasetIdentityBasis
  val timeSpace: FiniteSpace[T]
  val fullVoxelSpace: FiniteSpace[X]
  val activeVoxelSpace: FiniteSpace[A]
  val activeToFull: Injection[A, X]

  def activePointFor(
      full: Point[X]
  ): Either[DatasetError, Point[A]]

  final def fullPointFor(
      active: Point[A]
  ): Point[X] =
    activeToFull.mapping(active)

  final def resolveTimepoints(
      requested: TimepointSelection
  ): Either[DatasetError, Selection[T]] =
    requested
      .resolve(shape.timepoints)
      .flatMap: values =>
        Selection
          .fromOrdinals(timeSpace, values.map(TimepointIndex.raw))
          .left
          .map(error => DatasetError.InvalidTimeAxis(error.message))

  final def resolveVoxels(
      requested: VoxelSelection
  ): Either[DatasetError, Selection[X]] =
    voxelDomain
      .resolve(requested, shape.space)
      .flatMap: values =>
        Selection
          .fromOrdinals(fullVoxelSpace, values.map(VoxelIndex.raw))
          .left
          .map(error => DatasetError.ShapeMismatch(error.message))

  final def resolve(
      time: TimepointSelection,
      voxels: VoxelSelection
  ): Either[DatasetError, ResolvedDataSelection] =
    for
      resolvedTime <- resolveTimepoints(time)
      resolvedVoxels <- resolveVoxels(voxels)
    yield
      ResolvedDataSelection.fromLocus(
        this,
        resolvedTime,
        resolvedVoxels
      )

  final def fromSelections(
      timepoints: Selection[T],
      voxels: Selection[X]
  ): Either[DatasetError, ResolvedDataSelection] =
    if !timeSpace.sameIdentityAs(timepoints.space) then
      Left(
        DatasetError.ShapeMismatch(
          SpaceMismatch(
            timeSpace.key,
            timeSpace.size,
            timepoints.space.key,
            timepoints.space.size
          ).message
        )
      )
    else if !fullVoxelSpace.sameIdentityAs(voxels.space) then
      Left(
        DatasetError.ShapeMismatch(
          SpaceMismatch(
            fullVoxelSpace.key,
            fullVoxelSpace.size,
            voxels.space.key,
            voxels.space.size
          ).message
        )
      )
    else
      Right(ResolvedDataSelection.fromLocus(this, timepoints, voxels))

object DatasetAcquisitionDomain:
  def semantic(
      dataset: DatasetId,
      shape: DatasetShape,
      voxelDomain: VoxelDomain
  ): Either[DatasetError, DatasetAcquisitionDomain] =
    make(
      SpaceKey.unsafe(
        s"scalafim:dataset:${dataset.value}:${shape.space.hashCode}:${shape.spatialSize}"
      ),
      shape,
      voxelDomain,
      DatasetIdentityBasis.Semantic(dataset)
    )

  def structuralCompatibility(
      shape: DatasetShape,
      voxelDomain: VoxelDomain
  ): Either[DatasetError, DatasetAcquisitionDomain] =
    make(
      SpaceKey.unsafe(
        s"scalafim:dataset:structural:${shape.space.hashCode}:${shape.spatialSize}"
      ),
      shape,
      voxelDomain,
      DatasetIdentityBasis.StructuralCompatibility
    )

  private def make(
      key: SpaceKey,
      requestedShape: DatasetShape,
      requestedVoxelDomain: VoxelDomain,
      basis: DatasetIdentityBasis
  ): Either[DatasetError, DatasetAcquisitionDomain] =
    if requestedVoxelDomain.spatialSize != requestedShape.spatialSize then
      Left(
        DatasetError.ShapeMismatch(
          s"voxel domain has spatial size ${requestedVoxelDomain.spatialSize}, expected ${requestedShape.spatialSize}"
        )
      )
    else
      final class Timepoint
      final class FullVoxel
      final class ActiveVoxel

      val times =
        FiniteSpace
          .make[Timepoint](
            SpaceKey.unsafe(s"${key.value}:time"),
            requestedShape.timepoints
          )
          .toOption
          .get
      val full =
        FiniteSpace
          .make[FullVoxel](
            SpaceKey.unsafe(s"${key.value}:voxels"),
            requestedShape.spatialSize
          )
          .toOption
          .get
      val active =
        FiniteSpace
          .make[ActiveVoxel](
            SpaceKey.unsafe(
              s"${key.value}:active:${requestedVoxelDomain.indices.mkString(",")}"
            ),
            requestedVoxelDomain.nVoxels
          )
          .toOption
          .get
      val mapping =
        TotalMap
          .fromTargetOrdinals(
            active,
            full,
            requestedVoxelDomain.indices.toArray
          )
          .toOption
          .get
      val injection =
        Injection.validate(mapping).toOption.get
      val reverse = Array.fill(requestedShape.spatialSize)(-1)
      var sample = 0
      while sample < requestedVoxelDomain.indices.length do
        reverse(requestedVoxelDomain.indices(sample)) = sample
        sample += 1

      Right:
        new DatasetAcquisitionDomain:
          type T = Timepoint
          type X = FullVoxel
          type A = ActiveVoxel
          val shape: DatasetShape = requestedShape
          val voxelDomain: VoxelDomain = requestedVoxelDomain
          val identityBasis: DatasetIdentityBasis = basis
          val timeSpace: FiniteSpace[Timepoint] = times
          val fullVoxelSpace: FiniteSpace[FullVoxel] = full
          val activeVoxelSpace: FiniteSpace[ActiveVoxel] = active
          val activeToFull: Injection[ActiveVoxel, FullVoxel] = injection

          def activePointFor(
              fullPoint: Point[FullVoxel]
          ): Either[DatasetError, Point[ActiveVoxel]] =
            val activeOrdinal = reverse(fullPoint.ordinal)
            if activeOrdinal < 0 then
              Left(DatasetError.VoxelOutsideMask(fullPoint.ordinal))
            else
              Right(active.point(activeOrdinal).get)

trait ResolvedLocusSelection:
  type T
  type X
  val timepoints: Selection[T]
  val voxels: Selection[X]
