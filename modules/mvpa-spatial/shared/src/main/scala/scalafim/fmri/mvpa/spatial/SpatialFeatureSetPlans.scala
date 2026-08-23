package scalafim.fmri.mvpa.spatial

import scalafim.atlas.*
import scalafim.fmri.mvpa.*
import scalafim.image.{
  ExactVolumeSearchlight,
  Indexing,
  Mask,
  SelectedVolumeWindow,
  SearchlightRadius,
  SomeLabelVolume,
  VolumeDomain,
  VolumeNeighborhoods,
  VolumeSpace
}
import scalafim.image.SomeNeuroVolume.*
import scalafim.image.VolumeSpace.*
import scalafim.surface.{FragmentedParcelPolicy, LabeledSurface, MeshTopology, ParcelUnit, SurfaceParcels}
import locus4s.DomainRegistry
import locus4s.Index
import locus4s.Region

import scala.util.control.NonFatal

object SpatialFeatureSetPlans:

  def volumeLabels(
      name: String,
      labels: SomeLabelVolume[Int],
    background: Set[Int] = Set(0)
  ): Either[SpatialPlanError, SpatialFeaturePlan] =
    val byLabel = scala.collection.mutable.Map.empty[Int, scala.collection.mutable.ArrayBuffer[LinearVoxelIndex]]
    var lin = 0
    while lin < labels.data.size do
      val coordinate = Indexing.indexToGrid3D(labels.grid.shape, lin)
      val label = labels(coordinate(0), coordinate(1), coordinate(2))
      if !background.contains(label) then
        if label < 0 then return Left(SpatialPlanError.InvalidVolumeLabel(label))
        byLabel.getOrElseUpdate(label, scala.collection.mutable.ArrayBuffer.empty) += LinearVoxelIndex.unsafe(lin)
      lin += 1

    val ids = byLabel.keys.toVector.sorted
    buildVector(ids) { id =>
      featureSet(
        RoiId(id),
        byLabel(id).toVector.map(_.value),
        label = Some(id.toString)
      )
    }.flatMap { sets =>
      regionalPlan(
        name,
        SpatialFeatureDomain.VolumeLabels(
          labels.volumeSpace.toNeuroSpace,
          background
        ),
        sets
      )
    }

  def fromVolumeLabels(
      name: String,
      labels: SomeLabelVolume[Int],
      background: Set[Int] = Set(0)
  ): Either[MvpaError, FeatureSetPlan] =
    toMvpaPlan(volumeLabels(name, labels, background))

  def volumeAtlas(
      name: String,
      atlas: VolumeAtlas,
      coveragePolicy: ParcelCoveragePolicy = ParcelCoveragePolicy.RequireEveryRegion
  ): Either[SpatialPlanError, SpatialFeaturePlan] =
    val covered =
      Vector.newBuilder[
        (AtlasRegionMetadata, Vector[LinearVoxelIndex])
      ]
    val regions = atlas.regions.regions
    var i = 0
    while i < regions.length do
      val region = regions(i)
      val indices =
        atlas.realization
          .region(region.id)
          .fold(Vector.empty[LinearVoxelIndex])(
            _.ordinalsInDomainOrder.map(LinearVoxelIndex.unsafe).toVector
          )
      if indices.isEmpty then
        coveragePolicy match
          case ParcelCoveragePolicy.RequireEveryRegion =>
            return Left(SpatialPlanError.MissingAtlasRegion(region.id.value, region.label))
          case ParcelCoveragePolicy.KeepCoveredRegions =>
            ()
      else covered += ((region, indices))
      i += 1

    val items = covered.result()
    if items.isEmpty then Left(SpatialPlanError.EmptyAtlasAfterCoveragePolicy)
    else
      buildVector(items) { case (region, indices) =>
        featureSet(
          RoiId(region.id.value),
          indices.map(_.value),
          label = Some(region.label)
        )
      }.flatMap { sets =>
        regionalPlan(name, SpatialFeatureDomain.VolumeAtlas(atlas.ref, coveragePolicy), sets)
      }

  def fromVolumeAtlas(
      name: String,
      atlas: VolumeAtlas,
      coveragePolicy: ParcelCoveragePolicy = ParcelCoveragePolicy.RequireEveryRegion
  ): Either[MvpaError, FeatureSetPlan] =
    toMvpaPlan(volumeAtlas(name, atlas, coveragePolicy))

  def selectedWindows[F <: image4s.geometry.Frame[image4s.geometry.D3], S, A, Sem](
      name: String,
      windows: Seq[SelectedVolumeWindow[F, S, A, Sem]]
  ): Either[SpatialPlanError, SpatialFeaturePlan] =
    if windows.isEmpty then Left(SpatialPlanError.EmptySearchlightWindows)
    else
      val owner = windows.head.values.domain.space
      val seen = scala.collection.mutable.HashSet.empty[Int]
      buildVector(windows): window =>
        val actual = window.values.domain.space
        if !owner.sameRuntimeOwnerAs(actual) then
          Left(
            SpatialPlanError.InvalidLocusSearchlight(
              locus4s.SpaceMismatch.between(owner, actual).message
            )
          )
        else if seen.contains(window.center.ordinal) then
          Left(
            SpatialPlanError.InvalidLocusSearchlight(
              s"duplicate center ${window.center.ordinal}"
            )
          )
        else
          seen += window.center.ordinal
          featureSet(
            RoiId(window.center.ordinal),
            window.values.selection.ordinals.toVector,
            center = Some(window.center.ordinal),
            label = Some(
              Option(window.values.metadata.label)
                .filter(_.nonEmpty)
                .getOrElse(s"searchlight_${window.center.ordinal}")
            )
          )
      .flatMap: sets =>
        FeatureSetPlan
          .searchlight(name, sets)
          .left
          .map(SpatialPlanError.InvalidFeatureSetPlan.apply)
          .map: plan =>
            SpatialFeaturePlan(
              SpatialFeatureDomain.LocusSearchlight(
                owner.descriptor.toString,
                windows.size
              ),
              plan
            )

  def fromSelectedWindows[F <: image4s.geometry.Frame[image4s.geometry.D3], S, A, Sem](
      name: String,
      windows: Seq[SelectedVolumeWindow[F, S, A, Sem]]
  ): Either[MvpaError, FeatureSetPlan] =
    toMvpaPlan(selectedWindows(name, windows))

  /** Build a feature plan directly from one exact voxel-domain relation. */
  def volumeSearchlight[S](
      name: String,
      neighborhoods: VolumeNeighborhoods[S],
      label: Index[S] => Option[String] =
        (center: Index[S]) => Some(center.ordinal.toString)
  ): Either[SpatialPlanError, SpatialFeaturePlan] =
    val centerOrdinals =
      neighborhoods.centers.ordinalsInDomainOrder.toVector
    buildVector(centerOrdinals): ordinal =>
      val center =
        neighborhoods.centers.space.indexAtValidatedOrdinal(ordinal)
      featureSet(
        RoiId(ordinal),
        neighborhoods.relation
          .row(center)
          .ordinalsInDomainOrder
          .toVector,
        center = Some(ordinal),
        label = label(center)
      )
    .flatMap: sets =>
      FeatureSetPlan
        .searchlight(name, sets)
        .left
        .map(SpatialPlanError.InvalidFeatureSetPlan.apply)
        .map: plan =>
          SpatialFeaturePlan(
            SpatialFeatureDomain.LocusSearchlight(
              neighborhoods.centers.space.descriptor.toString,
              neighborhoods.centers.cardinality
            ),
            plan
          )

  def fromVolumeSearchlight[S](
      name: String,
      neighborhoods: VolumeNeighborhoods[S],
      label: Index[S] => Option[String] =
        (center: Index[S]) => Some(center.ordinal.toString)
  ): Either[MvpaError, FeatureSetPlan] =
    toMvpaPlan(volumeSearchlight(name, neighborhoods, label))

  def searchlightMask(
      name: String,
      mask: Mask.MaskVol,
      radius: Double,
      constrainToMask: Boolean = true,
      label: String = ""
  ): Either[SpatialPlanError, SpatialFeaturePlan] =
    captureSpatial("searchlight mask") {
      for
        checkedRadius <- SearchlightRadius
          .make(radius)
          .left
          .map(error => SpatialPlanError.AdapterFailure("searchlight radius", error.message))
        plan <- exactMaskSearchlight(
          name,
          mask,
          checkedRadius,
          constrainToMask,
          label
        )
      yield plan
    }

  def fromSearchlightMask(
      name: String,
      mask: Mask.MaskVol,
      radius: Double,
      constrainToMask: Boolean = true,
      label: String = ""
  ): Either[MvpaError, FeatureSetPlan] =
    toMvpaPlan(searchlightMask(name, mask, radius, constrainToMask, label))

  def surfaceParcels(
      name: String,
      parcels: Seq[ParcelUnit],
      identityPolicy: SurfaceParcelIdentityPolicy = SurfaceParcelIdentityPolicy.StableOrdinal
  ): Either[SpatialPlanError, SpatialFeaturePlan] =
    val parcelVector = parcels.toVector
    if parcelVector.isEmpty then Left(SpatialPlanError.EmptyParcelSet)
    else
      val identities = parcelVector.zipWithIndex.map { case (parcel, ordinal) =>
        parcel -> surfaceParcelIdentity(parcel, ordinal, identityPolicy)
      }
      duplicateIdentity(identities) match
        case Some((id, label)) =>
          Left(SpatialPlanError.DuplicateParcelIdentity(id, label))
        case None =>
          buildVector(identities) { case (parcel, id) =>
            val label = parcel.info.map(_.name).getOrElse(parcel.key.display)
            featureSet(
              RoiId(id),
              parcel.vertices.map(_.index),
              label = Some(label)
            )
          }.flatMap { sets =>
            regionalPlan(name, SpatialFeatureDomain.SurfaceParcels(identityPolicy), sets)
          }

  def fromSurfaceParcels(
      name: String,
      parcels: Seq[ParcelUnit],
      identityPolicy: SurfaceParcelIdentityPolicy = SurfaceParcelIdentityPolicy.StableOrdinal
  ): Either[MvpaError, FeatureSetPlan] =
    toMvpaPlan(surfaceParcels(name, parcels, identityPolicy))

  def labeledSurface(
      name: String,
      labeled: LabeledSurface,
      topology: MeshTopology,
      policy: FragmentedParcelPolicy = FragmentedParcelPolicy.Error,
      ignoredLabels: Set[Int] = Set.empty,
      identityPolicy: SurfaceParcelIdentityPolicy = SurfaceParcelIdentityPolicy.StableOrdinal
  ): Either[SpatialPlanError, SpatialFeaturePlan] =
    captureSpatial("labeled surface") {
      surfaceParcels(
        name,
        SurfaceParcels.units(labeled, topology, policy, ignoredLabels),
        identityPolicy
      )
    }

  def fromLabeledSurface(
      name: String,
      labeled: LabeledSurface,
      topology: MeshTopology,
      policy: FragmentedParcelPolicy = FragmentedParcelPolicy.Error,
      ignoredLabels: Set[Int] = Set.empty,
      identityPolicy: SurfaceParcelIdentityPolicy = SurfaceParcelIdentityPolicy.StableOrdinal
  ): Either[MvpaError, FeatureSetPlan] =
    toMvpaPlan(labeledSurface(name, labeled, topology, policy, ignoredLabels, identityPolicy))

  private def exactMaskSearchlight(
      name: String,
      mask: Mask.MaskVol,
      radius: SearchlightRadius,
      constrainToMask: Boolean,
      label: String
  ): Either[SpatialPlanError, SpatialFeaturePlan] =
    for
      volumeSpace <- VolumeSpace
        .fromSpatialPart(mask.space)
        .left
        .map(error => SpatialPlanError.AdapterFailure("mask space", error.message))
      packedDomain <- VolumeDomain
        .register(
          volumeSpace,
          "MVPA searchlight voxels",
          DomainRegistry.empty
        )
        .left
        .map(error => SpatialPlanError.AdapterFailure("voxel domain", error.message))
      result <-
        type Voxel = packedDomain.S
        val domain: VolumeDomain[Voxel] = packedDomain.value
        val maskOrdinals = Mask.indices(mask)
        for
          centers <- Region
            .fromOrdinals(
              domain.space,
              Iterator.tabulate(maskOrdinals.size)(maskOrdinals.apply)
            )
            .left
            .map(error => SpatialPlanError.InvalidLocusSearchlight(error.message))
          neighborhoods <- ExactVolumeSearchlight
            .metricBalls(domain, radius, centers)
            .left
            .map(error => SpatialPlanError.AdapterFailure("searchlight geometry", error.message))
          selectedNeighborhoods <-
            if !constrainToMask then Right(neighborhoods)
            else
              ExactVolumeSearchlight
                .restrictTargets(neighborhoods, centers)
                .left
                .map(error =>
                  SpatialPlanError.InvalidLocusSearchlight(error.message)
                )
          plan <- volumeSearchlight(
            name,
            selectedNeighborhoods,
            center =>
              Some(
                Option(label)
                  .filter(_.nonEmpty)
                  .getOrElse(s"searchlight_${center.ordinal}")
              )
          )
        yield plan
    yield result

  private def featureSet(
      id: RoiId,
      indices: Seq[Int],
      center: Option[Int] = None,
      label: Option[String] = None
  ): Either[SpatialPlanError, FeatureSet] =
    FeatureSet(id, indices, center, label).left.map(SpatialPlanError.InvalidFeatureSet.apply)

  private def regionalPlan(
      name: String,
      domain: SpatialFeatureDomain,
      sets: Seq[FeatureSet]
  ): Either[SpatialPlanError, SpatialFeaturePlan] =
    FeatureSetPlan
      .regional(name, sets)
      .left.map(SpatialPlanError.InvalidFeatureSetPlan.apply)
      .map(plan => SpatialFeaturePlan(domain, plan))

  private def buildVector[A, B](
      items: Seq[A]
  )(build: A => Either[SpatialPlanError, B]): Either[SpatialPlanError, Vector[B]] =
    val out = Vector.newBuilder[B]
    val iterator = items.iterator
    while iterator.hasNext do
      build(iterator.next()) match
        case Right(value) => out += value
        case Left(error) => return Left(error)
    Right(out.result())

  private def captureSpatial[A](context: String)(body: => Either[SpatialPlanError, A]): Either[SpatialPlanError, A] =
    try body
    catch
      case NonFatal(error) =>
        Left(SpatialPlanError.AdapterFailure(context, error.getMessage))

  private def surfaceParcelIdentity(
      parcel: ParcelUnit,
      ordinal: Int,
      policy: SurfaceParcelIdentityPolicy
  ): Int =
    policy match
      case SurfaceParcelIdentityPolicy.StableOrdinal => ordinal
      case SurfaceParcelIdentityPolicy.ParcelKey => parcel.label

  private def duplicateIdentity(items: Vector[(ParcelUnit, Int)]): Option[(Int, String)] =
    val seen = scala.collection.mutable.Set.empty[Int]
    val iterator = items.iterator
    while iterator.hasNext do
      val (parcel, id) = iterator.next()
      if seen(id) then return Some((id, parcel.key.display))
      seen += id
    None

  private def toMvpaPlan(result: Either[SpatialPlanError, SpatialFeaturePlan]): Either[MvpaError, FeatureSetPlan] =
    result.left.map(_.toMvpaError).map(_.plan)
