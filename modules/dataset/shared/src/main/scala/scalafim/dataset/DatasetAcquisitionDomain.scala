package scalafim.dataset

import scalafim.locus.{
  DomainFactory,
  FiniteDomain,
  FiniteSpace,
  Injection,
  Point,
  Selection,
  SpaceKey,
  SpaceMismatch,
  TotalMap,
  mapping,
  mismatch
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
  val activeVoxelSpace: FiniteDomain[A]
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
    if !timeSpace.sameRuntimeOwnerAs(timepoints.space) then
      Left(
        DatasetError.ShapeMismatch(
          mismatch(timeSpace, timepoints.space).message
        )
      )
    else if !fullVoxelSpace.sameRuntimeOwnerAs(voxels.space) then
      Left(
        DatasetError.ShapeMismatch(
          mismatch(fullVoxelSpace, voxels.space).message
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
      val timeResolution =
        DomainFactory.unsafeRestore(
          // The base key carries the spatial identity only, so the timepoint
          // count has to appear here: two acquisitions over the same grid but
          // different run lengths are different time domains, and a key that
          // did not say so named two sizes at once.
          SpaceKey.unsafe(s"${key.value}:time:${requestedShape.timepoints}"),
          requestedShape.timepoints
        )
      val fullResolution =
        DomainFactory.unsafeRestore(
          SpaceKey.unsafe(s"${key.value}:voxels"),
          requestedShape.spatialSize
        )
      // The time and full-voxel domains are structural and keyed, so they
      // canonicalize. The active domain is a *selection* over the full grid —
      // derived, meaningful only through `injection` below — so it is ephemeral
      // rather than keyed by its own index list.
      val activeDomain =
        DomainFactory.unsafeEphemeral("dataset-active", requestedVoxelDomain.nVoxels)
      val times = timeResolution.space
      val full = fullResolution.space
      val active = activeDomain.value
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
          type T = timeResolution.S
          type X = fullResolution.S
          type A = activeDomain.S
          val shape: DatasetShape = requestedShape
          val voxelDomain: VoxelDomain = requestedVoxelDomain
          val identityBasis: DatasetIdentityBasis = basis
          val timeSpace: FiniteSpace[T] = times
          val fullVoxelSpace: FiniteSpace[X] = full
          val activeVoxelSpace: FiniteDomain[A] = active
          val activeToFull: Injection[A, X] = injection

          def activePointFor(
              fullPoint: Point[X]
          ): Either[DatasetError, Point[A]] =
            val activeOrdinal = reverse(fullPoint.value)
            if activeOrdinal < 0 then
              Left(DatasetError.VoxelOutsideMask(fullPoint.value))
            else
              Right(active.indexOption(activeOrdinal).get)

trait ResolvedLocusSelection:
  type T
  type X
  val timepoints: Selection[T]
  val voxels: Selection[X]
