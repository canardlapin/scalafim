package scalafim.image.view

import intaglio.RasterImage
import scalafim.image.*

private[view] final case class SliceGeometryKey(
  anatomicalPlane: AnatomicalPlane,
  screenRight: UnitWorldVector,
  screenUp: UnitWorldVector,
  normal: UnitWorldVector,
  planeOffset: Double,
  dimensions: SliceDimensions,
  spacing: PixelSpacing,
  topLeftCenter: WorldPoint
)

private[view] object SliceGeometryKey:
  def from(grid: SliceGrid): SliceGeometryKey =
    val normal = grid.plane.normal
    val through = grid.plane.through
    SliceGeometryKey(
      anatomicalPlane = grid.plane.anatomicalPlane,
      screenRight = grid.plane.screenRight,
      screenUp = grid.plane.screenUp,
      normal = normal,
      planeOffset = through.x * normal.x + through.y * normal.y + through.z * normal.z,
      dimensions = grid.dimensions,
      spacing = grid.spacing,
      topLeftCenter = grid.topLeftCenter
    )

private[view] final case class SliceSampleKey(
  layer: SliceLayer,
  geometry: SliceGeometryKey,
  timepoint: Int
)

private[view] object SliceSampleKey:
  def from(layer: SliceLayer, grid: SliceGrid, timepoint: Int): SliceSampleKey =
    SliceSampleKey(
      layer,
      SliceGeometryKey.from(grid),
      if layer.timeInvariant then 0 else timepoint
    )

private[view] final case class SliceRasterKey(
  sample: SliceSampleKey,
  window: Option[DisplayWindow],
  threshold: Option[DisplayThreshold]
)

/** Immutable, model-scoped LRU caches for sampled slices and colorized rasters.
  * Sampling identity deliberately ignores the in-plane component of
  * `SlicePlane.through`; the covering grid origin still protects cropped or
  * otherwise distinct grids from false hits.
  */
final case class ViewerCache private (
  capacity: Int,
  private val samples: Map[SliceSampleKey, SampledLayerSlice],
  private val sampleRecency: Vector[SliceSampleKey],
  private val rasters: Map[SliceRasterKey, RasterImage],
  private val rasterRecency: Vector[SliceRasterKey]
):
  def size: Int =
    rasters.size

  def sampledSliceCount: Int =
    samples.size

  private[view] def lookupSample(key: SliceSampleKey): (Option[SampledLayerSlice], ViewerCache) =
    samples.get(key) match
      case None => None -> this
      case hit =>
        val refreshed = sampleRecency.filterNot(_ == key) :+ key
        hit -> copy(sampleRecency = refreshed)

  private[view] def storeSample(key: SliceSampleKey, sample: SampledLayerSlice): ViewerCache =
    if capacity == 0 then this
    else
      val withoutKey = sampleRecency.filterNot(_ == key)
      val (baseEntries, baseRecency) =
        if !samples.contains(key) && samples.size >= capacity then
          val evicted = withoutKey.head
          samples.removed(evicted) -> withoutKey.tail
        else samples -> withoutKey
      copy(
        samples = baseEntries.updated(key, sample),
        sampleRecency = baseRecency :+ key
      )

  private[view] def lookupRaster(key: SliceRasterKey): (Option[RasterImage], ViewerCache) =
    rasters.get(key) match
      case None => None -> this
      case hit =>
        val refreshed = rasterRecency.filterNot(_ == key) :+ key
        hit -> copy(rasterRecency = refreshed)

  private[view] def storeRaster(key: SliceRasterKey, raster: RasterImage): ViewerCache =
    if capacity == 0 then this
    else
      val withoutKey = rasterRecency.filterNot(_ == key)
      val (baseEntries, baseRecency) =
        if !rasters.contains(key) && rasters.size >= capacity then
          val evicted = withoutKey.head
          rasters.removed(evicted) -> withoutKey.tail
        else rasters -> withoutKey
      copy(
        rasters = baseEntries.updated(key, raster),
        rasterRecency = baseRecency :+ key
      )

object ViewerCache:
  val Disabled: ViewerCache =
    new ViewerCache(0, Map.empty, Vector.empty, Map.empty, Vector.empty)

  def make(capacity: Int): Either[ImageViewError, ViewerCache] =
    if capacity < 0 then Left(ImageViewError.InvalidCacheCapacity(capacity))
    else Right(new ViewerCache(capacity, Map.empty, Vector.empty, Map.empty, Vector.empty))

  def empty(capacity: Int): ViewerCache =
    make(capacity).fold(err => throw new IllegalArgumentException(err.message), identity)

final case class ViewerProfile(
  panelCount: Int,
  layerRequests: Int,
  cacheHits: Int,
  cacheMisses: Int,
  sampledPixels: Long,
  sampleCacheHits: Int = 0,
  sampleCacheMisses: Int = 0,
  colorizedPixels: Long = 0L,
  sourceReads: Int = 0
):
  def hitRate: Double =
    if layerRequests == 0 then 1.0 else cacheHits.toDouble / layerRequests

  private[view] def +(other: ViewerProfile): ViewerProfile =
    ViewerProfile(
      panelCount + other.panelCount,
      layerRequests + other.layerRequests,
      cacheHits + other.cacheHits,
      cacheMisses + other.cacheMisses,
      sampledPixels + other.sampledPixels,
      sampleCacheHits + other.sampleCacheHits,
      sampleCacheMisses + other.sampleCacheMisses,
      colorizedPixels + other.colorizedPixels,
      sourceReads + other.sourceReads
    )

  def sampleHitRate: Double =
    val requests = sampleCacheHits + sampleCacheMisses
    if requests == 0 then 1.0 else sampleCacheHits.toDouble / requests

object ViewerProfile:
  val Zero: ViewerProfile =
    ViewerProfile(0, 0, 0, 0, 0L, 0, 0, 0L, 0)

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
