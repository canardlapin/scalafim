package scalafim.image

enum ClusterIdError:
  case NonPositive(value: Int)

  def message: String =
    this match
      case NonPositive(value) =>
        s"cluster id must be positive; got $value"

opaque type ClusterId = Int

object ClusterId:
  def make(value: Int): Either[ClusterIdError, ClusterId] =
    if value > 0 then Right(value)
    else Left(ClusterIdError.NonPositive(value))

  def apply(value: Int): ClusterId =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ClusterId)
    inline def value: Int = id

  private[image] inline def unsafe(value: Int): ClusterId =
    value

enum SpatialCoordinateFrame:
  case Grid
  case World

enum SearchlightSupport:
  case FullNeighborhood
  case InsideMask

enum SearchlightCenterDomain:
  case AllVoxels
  case MaskVoxels

enum SearchlightValueSupport:
  case AllValues
  case NonZero

enum SearchlightError:
  case InvalidRadius(value: Double)
  case RadiusBelowVoxelSpacing(radius: Double, minimumSpacing: Double)
  case InvalidCenter(error: GeometryError)
  case InvalidSpace(error: NeuroSpaceError)
  case Grid(error: GridMismatch)
  case CenterExcluded(center: VoxelCoord)
  case IncompatiblePolicies(
      centerDomain: SearchlightCenterDomain,
      support: SearchlightSupport
  )

  def message: String =
    this match
      case InvalidRadius(value) =>
        s"searchlight radius must be finite and positive; got $value"
      case RadiusBelowVoxelSpacing(radius, minimumSpacing) =>
        s"searchlight radius $radius is smaller than minimum voxel spacing $minimumSpacing"
      case InvalidCenter(error) =>
        error.message
      case InvalidSpace(error) =>
        error.message
      case Grid(error) =>
        error.message
      case CenterExcluded(center) =>
        s"searchlight center ${center.toVector} is excluded by the selected support policy"
      case IncompatiblePolicies(centerDomain, support) =>
        s"searchlight center domain $centerDomain is incompatible with support policy $support"

opaque type SearchlightRadius = Double

object SearchlightRadius:
  def make(value: Double): Either[SearchlightError, SearchlightRadius] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(SearchlightError.InvalidRadius(value))

  def apply(value: Double): SearchlightRadius =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (radius: SearchlightRadius)
    inline def millimeters: Double = radius

final class SearchlightCenter private (
    val space: VolumeSpace,
    val voxel: VoxelCoord,
    val linearIndex: Int
):
  override def equals(other: Any): Boolean =
    other match
      case that: SearchlightCenter =>
        space == that.space && voxel == that.voxel
      case _ => false

  override def hashCode(): Int =
    31 * space.hashCode() + voxel.hashCode()

object SearchlightCenter:
  def make(
      space: VolumeSpace,
      voxel: VoxelCoord
  ): Either[SearchlightError, SearchlightCenter] =
    Indexing
      .gridToIndexChecked(space.shape, voxel)
      .left
      .map(SearchlightError.InvalidCenter.apply)
      .map(index => new SearchlightCenter(space, voxel, index))

  def make(
      space: NeuroSpace,
      voxel: VoxelCoord
  ): Either[SearchlightError, SearchlightCenter] =
    VolumeSpace
      .fromSpatialPart(space)
      .left
      .map(SearchlightError.InvalidSpace.apply)
      .flatMap(make(_, voxel))
