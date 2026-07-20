package scalafim.image.view

import scalafim.graphics.RasterImage
import scalafim.image.SliceGrid

private[view] final case class SliceCacheKey(
  layer: SliceLayer,
  grid: SliceGrid,
  timepoint: Int,
  window: Option[DisplayWindow]
)

/** Immutable, model-scoped LRU cache for sampled and colorized slice rasters. */
final case class ViewerCache private (
  capacity: Int,
  private val entries: Map[SliceCacheKey, RasterImage],
  private val recency: Vector[SliceCacheKey]
):
  def size: Int =
    entries.size

  private[view] def lookup(key: SliceCacheKey): (Option[RasterImage], ViewerCache) =
    entries.get(key) match
      case None => None -> this
      case hit =>
        val refreshed = recency.filterNot(_ == key) :+ key
        hit -> copy(recency = refreshed)

  private[view] def store(key: SliceCacheKey, raster: RasterImage): ViewerCache =
    if capacity == 0 then this
    else
      val withoutKey = recency.filterNot(_ == key)
      val (baseEntries, baseRecency) =
        if !entries.contains(key) && entries.size >= capacity then
          val evicted = withoutKey.head
          entries.removed(evicted) -> withoutKey.tail
        else entries -> withoutKey
      copy(
        entries = baseEntries.updated(key, raster),
        recency = baseRecency :+ key
      )

object ViewerCache:
  val Disabled: ViewerCache =
    new ViewerCache(0, Map.empty, Vector.empty)

  def make(capacity: Int): Either[ImageViewError, ViewerCache] =
    if capacity < 0 then Left(ImageViewError.InvalidCacheCapacity(capacity))
    else Right(new ViewerCache(capacity, Map.empty, Vector.empty))

  def empty(capacity: Int): ViewerCache =
    make(capacity).fold(err => throw new IllegalArgumentException(err.message), identity)

final case class ViewerProfile(
  panelCount: Int,
  layerRequests: Int,
  cacheHits: Int,
  cacheMisses: Int,
  sampledPixels: Long
):
  def hitRate: Double =
    if layerRequests == 0 then 1.0 else cacheHits.toDouble / layerRequests

  private[view] def +(other: ViewerProfile): ViewerProfile =
    ViewerProfile(
      panelCount + other.panelCount,
      layerRequests + other.layerRequests,
      cacheHits + other.cacheHits,
      cacheMisses + other.cacheMisses,
      sampledPixels + other.sampledPixels
    )

object ViewerProfile:
  val Zero: ViewerProfile =
    ViewerProfile(0, 0, 0, 0, 0L)

final case class ViewerCompilation(
  frame: ViewerFrame,
  cache: ViewerCache,
  profile: ViewerProfile
)

enum LinkedViewProperty:
  case Cursor, Timepoint, Convention

final case class ViewLink(properties: Set[LinkedViewProperty]):
  def synchronize(
    source: ViewerState,
    target: ViewerState,
    targetModel: ViewerModel
  ): Either[ImageViewError, ViewerState] =
    val linkedTimepoint =
      if properties.contains(LinkedViewProperty.Timepoint) then source.timepoint
      else target.timepoint
    if linkedTimepoint < 0 || linkedTimepoint >= targetModel.timepointCount then
      Left(ImageViewError.TimepointOutOfBounds(linkedTimepoint, targetModel.timepointCount))
    else
      Right(
        target.copy(
          cursor = if properties.contains(LinkedViewProperty.Cursor) then source.cursor else target.cursor,
          timepoint = linkedTimepoint,
          convention =
            if properties.contains(LinkedViewProperty.Convention) then source.convention
            else target.convention
        )
      )

object ViewLink:
  val Spatial: ViewLink =
    ViewLink(Set(LinkedViewProperty.Cursor, LinkedViewProperty.Convention))

  val SpatialAndTime: ViewLink =
    ViewLink(Set(LinkedViewProperty.Cursor, LinkedViewProperty.Timepoint, LinkedViewProperty.Convention))
