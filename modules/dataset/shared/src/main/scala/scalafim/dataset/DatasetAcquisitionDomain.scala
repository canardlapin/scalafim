package scalafim.dataset

import locus4s.DomainRegistry
import scalafim.image.{
  VolumeDomain as ImageVolumeDomain,
  VoxelSelection as ImageVoxelSelection
}
import scalafim.locus.{
  DomainFactory,
  DomainFactoryError,
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
  val registry: DomainRegistry
  val timeSpace: FiniteSpace[T]
  val volumeDomain: ImageVolumeDomain[X]
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
    for
      values <- voxelDomain.resolve(requested, shape.space)
      imageSelection <- ImageVoxelSelection
        .make(
          shape.volumeSpace,
          values.map(VoxelIndex.raw).toArray
        )
        .left
        .map(error => DatasetError.ShapeMismatch(error.message))
      selection <- volumeDomain
        .selection(imageSelection)
        .left
        .map(error => DatasetError.ShapeMismatch(error.message))
    yield selection

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
    semanticIn(DomainRegistry.empty, dataset, shape, voxelDomain)

  def semanticIn(
      registry: DomainRegistry,
      dataset: DatasetId,
      shape: DatasetShape,
      voxelDomain: VoxelDomain
  ): Either[DatasetError, DatasetAcquisitionDomain] =
    make(
      registry,
      SpaceKey.unsafe(
        s"scalafim:dataset:${dataset.value}"
      ),
      shape,
      voxelDomain,
      DatasetIdentityBasis.Semantic(dataset)
    )

  def structuralCompatibility(
      shape: DatasetShape,
      voxelDomain: VoxelDomain
  ): Either[DatasetError, DatasetAcquisitionDomain] =
    structuralCompatibilityIn(DomainRegistry.empty, shape, voxelDomain)

  def structuralCompatibilityIn(
      registry: DomainRegistry,
      shape: DatasetShape,
      voxelDomain: VoxelDomain
  ): Either[DatasetError, DatasetAcquisitionDomain] =
    make(
      registry,
      SpaceKey.unsafe(
        "scalafim:dataset:structural"
      ),
      shape,
      voxelDomain,
      DatasetIdentityBasis.StructuralCompatibility
    )

  private def make(
      registry: DomainRegistry,
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
      DomainFactory
        .restore(
          registry,
          // The base key carries the spatial identity only, so the timepoint
          // count has to appear here: two acquisitions over the same grid but
          // different run lengths are different time domains, and a key that
          // did not say so named two sizes at once.
          SpaceKey.unsafe(s"${key.value}:time:${requestedShape.timepoints}"),
          requestedShape.timepoints
        )
        .left
        .map(DatasetError.DomainRestoreFailed.apply)
        .flatMap: timeResolution =>
          ImageVolumeDomain
            .canonicalIn(
              timeResolution.registry,
              requestedShape.volumeSpace
            )
            .left
            .map(DatasetError.VolumeDomainFailed.apply)
            .map: packedVolumeDomain =>
              // The time and full-voxel domains are structural and keyed, so
              // they canonicalize inside the acquisition-owned registry. The
              // active domain is a derived selection over the full grid.
              val activeDomain =
                DomainFactory.unsafeEphemeral(
                  "dataset-active",
                  requestedVoxelDomain.nVoxels
                )
              val times = timeResolution.space
              val volumeDomain = packedVolumeDomain.value
              val full = volumeDomain.finiteSpace
              val active = activeDomain.value
              val canonicalTargets =
                requestedVoxelDomain.indices.map: legacyOrdinal =>
                  volumeDomain
                    .pointAtLegacyOrdinal(legacyOrdinal)
                    .toOption
                    .get
                    .ordinal
              val mapping =
                TotalMap
                  .fromTargetOrdinals(
                    active,
                    full,
                    canonicalTargets.toArray
                  )
                  .toOption
                  .get
              val injection =
                Injection.validate(mapping).toOption.get
              val reverse = Array.fill(requestedShape.spatialSize)(-1)
              var sample = 0
              while sample < requestedVoxelDomain.indices.length do
                reverse(canonicalTargets(sample)) = sample
                sample += 1

              new DatasetAcquisitionDomain:
                type T = timeResolution.S
                type X = packedVolumeDomain.S
                type A = activeDomain.S
                val shape: DatasetShape = requestedShape
                val voxelDomain: VoxelDomain = requestedVoxelDomain
                val identityBasis: DatasetIdentityBasis = basis
                val registry: DomainRegistry = packedVolumeDomain.registry
                val timeSpace: FiniteSpace[T] = times
                val volumeDomain: ImageVolumeDomain[X] =
                  packedVolumeDomain.value
                val fullVoxelSpace: FiniteSpace[X] = full
                val activeVoxelSpace: FiniteDomain[A] = active
                val activeToFull: Injection[A, X] = injection

                def activePointFor(
                    fullPoint: Point[X]
                ): Either[DatasetError, Point[A]] =
                  val activeOrdinal = reverse(fullPoint.value)
                  if activeOrdinal < 0 then
                    Left(
                      DatasetError.VoxelOutsideMask(
                        volumeDomain.legacyOrdinalOf(fullPoint)
                      )
                    )
                  else
                    Right(active.indexOption(activeOrdinal).get)

trait ResolvedLocusSelection:
  type T
  type X
  val timepoints: Selection[T]
  val voxels: Selection[X]
