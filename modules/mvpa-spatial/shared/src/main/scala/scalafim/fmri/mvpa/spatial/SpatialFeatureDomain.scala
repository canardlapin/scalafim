package scalafim.fmri.mvpa.spatial

import scalafim.atlas.AtlasRef
import scalafim.fmri.mvpa.{FeatureSet, FeatureSetPlan, MvpaError, RoiId}
import scalafim.image.NeuroSpace

enum ParcelCoveragePolicy:
  case RequireEveryRegion
  case KeepCoveredRegions

enum SurfaceParcelIdentityPolicy:
  case StableOrdinal
  case ParcelKey

enum SpatialFeatureDomain:
  case VolumeLabels(space: NeuroSpace, background: Set[Int])
  case VolumeAtlas(ref: AtlasRef, coveragePolicy: ParcelCoveragePolicy)
  case Searchlight(windows: SearchlightWindowSet)
  case SurfaceParcels(identityPolicy: SurfaceParcelIdentityPolicy)

final case class SpatialFeaturePlan(domain: SpatialFeatureDomain, plan: FeatureSetPlan):
  def toFeatureSetPlan: FeatureSetPlan =
    plan

enum SpatialPlanError:
  case InvalidVolumeLabel(label: Int)
  case MissingAtlasRegion(regionId: Int, label: String)
  case EmptyAtlasAfterCoveragePolicy
  case InvalidLinearVoxelIndex(value: Int)
  case EmptySearchlightWindows
  case EmptySearchlightWindow(center: SearchlightCenter)
  case DuplicateSearchlightVoxels(center: SearchlightCenter)
  case SearchlightCenterMissing(center: SearchlightCenter)
  case DuplicateSearchlightCenter(center: SearchlightCenter)
  case EmptyParcelSet
  case DuplicateParcelIdentity(id: Int, label: String)
  case InvalidFeatureSet(error: MvpaError)
  case InvalidFeatureSetPlan(error: MvpaError)
  case AdapterFailure(context: String, detail: String)

  def message: String =
    this match
      case InvalidVolumeLabel(label) =>
        s"volume labels must be non-negative after background removal; found $label"
      case MissingAtlasRegion(regionId, label) =>
        s"atlas region $regionId ($label) has no covered voxels"
      case EmptyAtlasAfterCoveragePolicy =>
        "atlas coverage policy removed every region"
      case InvalidLinearVoxelIndex(value) =>
        s"linear voxel index must be non-negative; found $value"
      case EmptySearchlightWindows =>
        "searchlight plan must contain at least one window"
      case EmptySearchlightWindow(center) =>
        s"searchlight window at center ${center.value} must contain at least one voxel"
      case DuplicateSearchlightVoxels(center) =>
        s"searchlight window at center ${center.value} contains duplicate voxel indices"
      case SearchlightCenterMissing(center) =>
        s"searchlight window at center ${center.value} must include its center voxel"
      case DuplicateSearchlightCenter(center) =>
        s"searchlight plan contains duplicate center ${center.value}"
      case EmptyParcelSet =>
        "surface parcel plan must contain at least one parcel"
      case DuplicateParcelIdentity(id, label) =>
        s"surface parcel identity policy produced duplicate ROI id $id for parcel $label"
      case InvalidFeatureSet(error) =>
        error.message
      case InvalidFeatureSetPlan(error) =>
        error.message
      case AdapterFailure(context, detail) =>
        s"$context: $detail"

  def toMvpaError: MvpaError =
    this match
      case InvalidFeatureSet(error) => error
      case InvalidFeatureSetPlan(error) => error
      case _ => MvpaError.InvalidFeatureSetPlan(message)

opaque type LinearVoxelIndex = Int

object LinearVoxelIndex:
  def apply(value: Int): Either[SpatialPlanError, LinearVoxelIndex] =
    if value < 0 then Left(SpatialPlanError.InvalidLinearVoxelIndex(value))
    else Right(value)

  def unsafe(value: Int): LinearVoxelIndex =
    require(value >= 0, "linear voxel index must be non-negative")
    value

  def fromInts(values: Seq[Int]): Either[SpatialPlanError, Vector[LinearVoxelIndex]] =
    val out = Vector.newBuilder[LinearVoxelIndex]
    val iterator = values.iterator
    while iterator.hasNext do
      val value = iterator.next()
      if value < 0 then return Left(SpatialPlanError.InvalidLinearVoxelIndex(value))
      out += value
    Right(out.result())

  extension (index: LinearVoxelIndex)
    inline def value: Int = index

opaque type SearchlightCenter = Int

object SearchlightCenter:
  def apply(value: Int): Either[SpatialPlanError, SearchlightCenter] =
    if value < 0 then Left(SpatialPlanError.InvalidLinearVoxelIndex(value))
    else Right(value)

  def unsafe(value: Int): SearchlightCenter =
    require(value >= 0, "searchlight center must be non-negative")
    value

  def fromLinear(index: LinearVoxelIndex): SearchlightCenter =
    index.value

  extension (center: SearchlightCenter)
    inline def value: Int = center
    inline def linearVoxelIndex: LinearVoxelIndex = LinearVoxelIndex.unsafe(center)

final case class SearchlightWindow private (
    center: SearchlightCenter,
    indices: Vector[LinearVoxelIndex],
    label: Option[String]
):
  require(indices.nonEmpty, "searchlight window must contain at least one voxel")
  require(indices.map(_.value).distinct.length == indices.length, "searchlight window voxel indices must be unique")
  require(indices.exists(_.value == center.value), "searchlight window must include its center")
  require(label.forall(_.nonEmpty), "searchlight window label must be non-empty")

  def featureIndices: Vector[Int] =
    indices.map(_.value)

  def featureLabel: String =
    label.getOrElse(s"searchlight_${center.value}")

object SearchlightWindow:
  def apply(
      center: SearchlightCenter,
      indices: Seq[LinearVoxelIndex],
      label: Option[String] = None
  ): Either[SpatialPlanError, SearchlightWindow] =
    val parsed = indices.toVector
    if parsed.isEmpty then Left(SpatialPlanError.EmptySearchlightWindow(center))
    else if parsed.map(_.value).distinct.length != parsed.length then Left(SpatialPlanError.DuplicateSearchlightVoxels(center))
    else if !parsed.exists(_.value == center.value) then Left(SpatialPlanError.SearchlightCenterMissing(center))
    else
      val cleanLabel = label.map(_.trim).filter(_.nonEmpty)
      Right(new SearchlightWindow(center, parsed, cleanLabel))

final class SearchlightWindowSet private (val windows: Vector[SearchlightWindow]):
  require(windows.nonEmpty, "searchlight window set must be non-empty")
  require(windows.map(_.center.value).distinct.length == windows.length, "searchlight centers must be unique")

  def size: Int =
    windows.length

  def toFeatureSets: Either[SpatialPlanError, Vector[FeatureSet]] =
    val out = Vector.newBuilder[FeatureSet]
    val iterator = windows.iterator
    while iterator.hasNext do
      val window = iterator.next()
      FeatureSet(
        RoiId(window.center.value),
        window.featureIndices,
        center = Some(window.center.value),
        label = Some(window.featureLabel)
      ) match
        case Right(featureSet) => out += featureSet
        case Left(error) => return Left(SpatialPlanError.InvalidFeatureSet(error))
    Right(out.result())

  override def equals(other: Any): Boolean =
    other match
      case that: SearchlightWindowSet => windows == that.windows
      case _ => false

  override def hashCode(): Int =
    windows.hashCode()

object SearchlightWindowSet:
  def apply(windows: Seq[SearchlightWindow]): Either[SpatialPlanError, SearchlightWindowSet] =
    val parsed = windows.toVector
    if parsed.isEmpty then Left(SpatialPlanError.EmptySearchlightWindows)
    else
      val seen = scala.collection.mutable.Set.empty[Int]
      val iterator = parsed.iterator
      while iterator.hasNext do
        val center = iterator.next().center
        if seen(center.value) then return Left(SpatialPlanError.DuplicateSearchlightCenter(center))
        seen += center.value
      Right(new SearchlightWindowSet(parsed))
