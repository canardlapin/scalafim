package scalafim.fmri.mvpa.spatial

import scalafim.atlas.AtlasRef
import scalafim.fmri.mvpa.{FeatureSetPlan, MvpaError}
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
  case LocusSearchlight(spaceKey: String, centerCount: Int)
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
  case InvalidLocusSearchlight(detail: String)
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
      case InvalidLocusSearchlight(detail) =>
        s"invalid locus searchlight: $detail"
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
