package scalafim.dataset

import locus4s.{
  DomainRecord,
  DomainRegistry,
  FiniteDomain,
  Index,
  Injection,
  Selection,
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
  val timeSpace: FiniteDomain[T]
  val fullVoxelSpace: FiniteDomain[X]
  val activeVoxelSpace: FiniteDomain[A]
  val activeToFull: Injection[A, X]

  def activePointFor(
      full: Index[X]
  ): Either[DatasetError, Index[A]]

  final def fullPointFor(
      active: Index[A]
  ): Index[X] =
    activeToFull.toTotalMap(active)

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
          SpaceMismatch.between(timeSpace, timepoints.space).message
        )
      )
    else if !fullVoxelSpace.sameRuntimeOwnerAs(voxels.space) then
      Left(
        DatasetError.ShapeMismatch(
          SpaceMismatch.between(fullVoxelSpace, voxels.space).message
        )
      )
    else
      Right(ResolvedDataSelection.fromLocus(this, timepoints, voxels))

object DatasetAcquisitionDomain:
  /** Persistent identity follows the dataset id and the exact persistent grid.
    *
    * image4s binds one persistent grid id to one structural grid key (frame,
    * shape, affine); grids minted by `SampleSpaces` are content-addressed over
    * rank, frame, shape and exact affine bits. A grid without a persistent
    * record yields ephemeral domains: such an acquisition is usable in-process
    * but makes no cross-load identity claim. The dataset id is length-prefixed
    * so no dataset name can collide with the structural basis.
    */
  def semantic(
      dataset: DatasetId,
      shape: DatasetShape,
      voxelDomain: VoxelDomain
  ): Either[DatasetError, DatasetAcquisitionDomain] =
    make(
      persistentGrid(shape).map(grid =>
        s"scalafim:dataset:semantic:${dataset.value.length}:${dataset.value}:$grid"
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
      persistentGrid(shape).map(grid => s"scalafim:dataset:structural:$grid"),
      shape,
      voxelDomain,
      DatasetIdentityBasis.StructuralCompatibility
    )

  /** Either a restored persistent owner or an ephemeral one, with its owner type. */
  private trait Owned:
    type S
    val value: FiniteDomain[S]

  private def restored(id: String, size: Int): Owned =
    val record = DomainRecord.parse(id, id, size).fold(error => throw new IllegalArgumentException(error.message), identity)
    val resolution = DomainRegistry.empty.restore(record).fold(error => throw new IllegalArgumentException(error.message), identity)
    new Owned:
      type S = resolution.S
      val value: FiniteDomain[S] = resolution.space

  private def ephemeral(name: String, size: Int): Owned =
    val domain = FiniteDomain.ephemeral(name, size).fold(error => throw new IllegalArgumentException(error.message), identity)
    new Owned:
      type S = domain.S
      val value: FiniteDomain[S] = domain.value

  private def persistentGrid(shape: DatasetShape): Option[String] =
    shape.grid.record.toOption.map(_.key.id.value)

  private def make(
      key: Option[String],
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
      // The base key carries the spatial identity only, so the timepoint count
      // has to appear in the time key: two acquisitions over the same grid but
      // different run lengths are different time domains. Each construction
      // restores independent owners; equal keys align explicitly.
      val timeDomain =
        key.fold(ephemeral("dataset-time", requestedShape.timepoints)): base =>
          restored(s"$base:time:${requestedShape.timepoints}", requestedShape.timepoints)
      val fullDomain =
        key.fold(ephemeral("dataset-voxels", requestedShape.spatialSize)): base =>
          restored(s"$base:voxels", requestedShape.spatialSize)
      // The active domain is a *selection* over the full grid — derived,
      // meaningful only through `injection` below — so it is ephemeral rather
      // than keyed by its own index list.
      val activeDomain =
        FiniteDomain.ephemeral("dataset-active", requestedVoxelDomain.nVoxels)
          .fold(error => throw new IllegalArgumentException(error.message), identity)
      val times = timeDomain.value
      val full = fullDomain.value
      val active = activeDomain.value
      // VoxelDomain.indices materializes a vector. Reuse its ordinals for
      // both maps instead of rebuilding the whole vector for every voxel.
      val activeIndices = requestedVoxelDomain.indices.toArray
      val mapping =
        TotalMap
          .fromTargetOrdinals(
            active,
            full,
            activeIndices
          )
          .toOption
          .get
      val injection =
        Injection.fromTotalMap(mapping).toOption.get
      val reverse = Array.fill(requestedShape.spatialSize)(-1)
      var sample = 0
      while sample < activeIndices.length do
        reverse(activeIndices(sample)) = sample
        sample += 1

      Right:
        new DatasetAcquisitionDomain:
          type T = timeDomain.S
          type X = fullDomain.S
          type A = activeDomain.S
          val shape: DatasetShape = requestedShape
          val voxelDomain: VoxelDomain = requestedVoxelDomain
          val identityBasis: DatasetIdentityBasis = basis
          val timeSpace: FiniteDomain[T] = times
          val fullVoxelSpace: FiniteDomain[X] = full
          val activeVoxelSpace: FiniteDomain[A] = active
          val activeToFull: Injection[A, X] = injection

          def activePointFor(
              fullPoint: Index[X]
          ): Either[DatasetError, Index[A]] =
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
