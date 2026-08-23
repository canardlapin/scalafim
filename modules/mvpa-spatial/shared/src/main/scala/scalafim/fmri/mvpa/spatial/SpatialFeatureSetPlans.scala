package scalafim.fmri.mvpa.spatial

import scalafim.atlas.*
import scalafim.fmri.mvpa.*
import scalafim.image.{
  GridCompatibility,
  Mask,
  NeuroVol,
  ROIVolWindow,
  Searchlight,
  SearchlightCenterDomain,
  SearchlightRadius,
  SearchlightSupport,
  VolumeDomain,
  VolumeSpace
}
import scalafim.locus.{
  CenteredSearchlight,
  Region as LocusRegion,
  Relation as LocusRelation,
  Searchlight as LocusSearchlight
}
import scalafim.surface.{FragmentedParcelPolicy, LabeledSurface, MeshTopology, ParcelUnit, SurfaceParcels}
import locus4s.DomainRegistry

import scala.util.control.NonFatal

object SpatialFeatureSetPlans:

  def volumeLabels(
      name: String,
      labels: NeuroVol[Int],
    background: Set[Int] = Set(0)
  ): Either[SpatialPlanError, SpatialFeaturePlan] =
    val byLabel = scala.collection.mutable.Map.empty[Int, scala.collection.mutable.ArrayBuffer[LinearVoxelIndex]]
    var lin = 0
    while lin < labels.values.size do
      val label = labels.valueAtCanonicalOrdinal(lin)
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
      regionalPlan(name, SpatialFeatureDomain.VolumeLabels(labels.space, background), sets)
    }

  def fromVolumeLabels(
      name: String,
      labels: NeuroVol[Int],
      background: Set[Int] = Set(0)
  ): Either[MvpaError, FeatureSetPlan] =
    toMvpaPlan(volumeLabels(name, labels, background))

  def volumeAtlas(
      name: String,
      atlas: VolumeAtlas,
      coveragePolicy: ParcelCoveragePolicy = ParcelCoveragePolicy.RequireEveryRegion
  ): Either[SpatialPlanError, SpatialFeaturePlan] =
    val byLabel = scala.collection.mutable.Map.empty[Int, scala.collection.mutable.ArrayBuffer[LinearVoxelIndex]]
    var lin = 0
    while lin < atlas.labelVolume.values.size do
      val label = atlas.labelVolume.valueAtCanonicalOrdinal(lin)
      if label != 0 then
        byLabel.getOrElseUpdate(label, scala.collection.mutable.ArrayBuffer.empty) += LinearVoxelIndex.unsafe(lin)
      lin += 1

    val covered =
      Vector.newBuilder[
        (AtlasRegionMetadata, Vector[LinearVoxelIndex])
      ]
    val regions = atlas.regions.regions
    var i = 0
    while i < regions.length do
      val region = regions(i)
      val indices = byLabel.get(region.id.value).map(_.toVector).getOrElse(Vector.empty)
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

  def roiWindows(
      name: String,
      windows: Seq[ROIVolWindow[?]]
  ): Either[SpatialPlanError, SpatialFeaturePlan] =
    captureSpatial("ROI windows") {
      locusSearchlight(windows.toVector).flatMap: canonical =>
        val centered = canonical.centered
        LocusFeatureSetPlans
          .fromSearchlight(
            name,
            centered,
            center =>
              Some(
                canonical.labels
                  .get(center.value)
                  .filter(_.nonEmpty)
                  .getOrElse(s"searchlight_${center.value}")
              )
          )
          .left
          .map(SpatialPlanError.InvalidFeatureSetPlan.apply)
          .map: plan =>
            SpatialFeaturePlan(
              SpatialFeatureDomain.LocusSearchlight(
                centered.searchlight.centers.space.descriptor.toString,
                centered.searchlight.centers.cardinality
              ),
              plan
            )
    }

  def fromRoiWindows(
      name: String,
      windows: Seq[ROIVolWindow[?]]
  ): Either[MvpaError, FeatureSetPlan] =
    toMvpaPlan(roiWindows(name, windows))

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
        windows <- Searchlight
          .searchlightChecked(
            mask,
            checkedRadius,
            SearchlightCenterDomain.MaskVoxels,
            if constrainToMask then SearchlightSupport.InsideMask
            else SearchlightSupport.FullNeighborhood,
            label
          )
          .left
          .map(error => SpatialPlanError.AdapterFailure("searchlight mask", error.message))
        plan <- roiWindows(name, windows.toVector)
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

  private def locusSearchlight(
      windows: Vector[ROIVolWindow[?]]
  ): Either[SpatialPlanError, CanonicalSearchlight] =
    if windows.isEmpty then Left(SpatialPlanError.EmptySearchlightWindows)
    else
      for
        volumeSpace <- VolumeSpace
          .fromSpatialPart(windows.head.space)
          .left
          .map(error => SpatialPlanError.AdapterFailure("ROI window space", error.message))
        result <- buildLocusSearchlight(volumeSpace, windows)
      yield result

  private def buildLocusSearchlight(
      volumeSpace: VolumeSpace,
      windows: Vector[ROIVolWindow[?]]
  ): Either[SpatialPlanError, CanonicalSearchlight] =
    val packedDomain =
      VolumeDomain
        .register(volumeSpace, "MVPA searchlight voxels", DomainRegistry.empty)
        .toOption
        .get
    type Voxel = packedDomain.S
    val domain: VolumeDomain[Voxel] = packedDomain.value
    val rows = Array.fill(domain.space.size)(Array.emptyIntArray)
    val centerOrdinals = Array.ofDim[Int](windows.length)
    val centerLabels = scala.collection.mutable.Map.empty[Int, String]
    val seenCenters = scala.collection.mutable.HashSet.empty[Int]
    var index = 0
    while index < windows.length do
      val window = windows(index)
      GridCompatibility.spatial(volumeSpace.toNeuroSpace, window.space) match
        case Left(error) =>
          return Left(SpatialPlanError.AdapterFailure("ROI window grid", error.message))
        case Right(_) =>
          ()
      val center = window.parentIndex
      if seenCenters.contains(center) then
        return Left(
          SpatialPlanError.InvalidLocusSearchlight(
            s"duplicate center $center"
          )
        )
      val linearIndices = window.selection.linearIndices
      val members = Array.tabulate(linearIndices.size)(i => linearIndices(i))
      if members.distinct.length != members.length then
        return Left(
          SpatialPlanError.InvalidLocusSearchlight(
            s"neighborhood at center $center contains duplicate points"
          )
        )
      if !members.contains(center) then
        return Left(
          SpatialPlanError.InvalidLocusSearchlight(
            s"neighborhood at center $center does not contain its center"
          )
        )
      seenCenters += center
      centerOrdinals(index) = center
      rows(center) = members
      centerLabels(center) = window.label
      index += 1

    for
      centers <- LocusRegion
        .fromOrdinals(domain.space, centerOrdinals)
        .left
        .map(error => SpatialPlanError.InvalidLocusSearchlight(error.message))
      relation <- LocusRelation
        .fromOrdinalRows(
          domain.space,
          domain.space,
          rows.iterator.map(_.iterator)
        )
        .left
        .map(error => SpatialPlanError.InvalidLocusSearchlight(error.message))
      searchlight <- LocusSearchlight
        .make(centers, relation)
        .left
        .map(error => SpatialPlanError.InvalidLocusSearchlight(error.message))
      validated <- CenteredSearchlight
        .validate(searchlight)
        .left
        .map(error => SpatialPlanError.InvalidLocusSearchlight(error.message))
    yield
      new CanonicalSearchlight:
        type S = Voxel
        val centered: CenteredSearchlight[Voxel] = validated
        val labels: Map[Int, String] = centerLabels.toMap

  private trait CanonicalSearchlight:
    type S
    val centered: CenteredSearchlight[S]
    val labels: Map[Int, String]

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
