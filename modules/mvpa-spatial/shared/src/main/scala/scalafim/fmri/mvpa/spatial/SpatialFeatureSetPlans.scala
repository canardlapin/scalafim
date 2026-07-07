package scalafim.fmri.mvpa.spatial

import scalafim.atlas.*
import scalafim.fmri.mvpa.*
import scalafim.image.{Mask, NeuroVol, ROIVolWindow, Searchlight}
import scalafim.surface.{FragmentedParcelPolicy, LabeledSurface, MeshTopology, ParcelUnit, SurfaceParcels}

import scala.util.control.NonFatal

object SpatialFeatureSetPlans:

  def volumeLabels(
      name: String,
      labels: NeuroVol[Int],
      background: Set[Int] = Set(0)
  ): Either[SpatialPlanError, SpatialFeaturePlan] =
    val byLabel = scala.collection.mutable.Map.empty[Int, scala.collection.mutable.ArrayBuffer[LinearVoxelIndex]]
    val values = labels.values.data
    var lin = 0
    while lin < values.length do
      val label = values(lin)
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
    val labels = atlas.labelVolume.values.data
    var lin = 0
    while lin < labels.length do
      val label = labels(lin)
      if label != 0 then
        byLabel.getOrElseUpdate(label, scala.collection.mutable.ArrayBuffer.empty) += LinearVoxelIndex.unsafe(lin)
      lin += 1

    val covered = Vector.newBuilder[(Region, Vector[LinearVoxelIndex])]
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
      buildVector(windows.toVector)(searchlightWindow).flatMap { parsed =>
        SearchlightWindowSet(parsed).flatMap { windowSet =>
          windowSet.toFeatureSets.flatMap { sets =>
            searchlightPlan(name, SpatialFeatureDomain.Searchlight(windowSet), sets)
          }
        }
      }
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
      roiWindows(
        name,
        Searchlight.searchlight(mask, radius, nonzero = constrainToMask, label = label).toVector
      )
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

  private def searchlightWindow(window: ROIVolWindow[?]): Either[SpatialPlanError, SearchlightWindow] =
    val linear = window.coords.linearIndices(window.space)
    val indices = Vector.tabulate(linear.length)(i => linear(i))
    for
      center <- SearchlightCenter(window.parentIndex)
      parsed <- LinearVoxelIndex.fromInts(indices)
      typed <- SearchlightWindow(center, parsed, Some(window.label))
    yield typed

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

  private def searchlightPlan(
      name: String,
      domain: SpatialFeatureDomain,
      sets: Seq[FeatureSet]
  ): Either[SpatialPlanError, SpatialFeaturePlan] =
    FeatureSetPlan
      .searchlight(name, sets)
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
