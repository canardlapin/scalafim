package scalafim.image.view

import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import intaglio.*
import scalafim.image.*

import scala.reflect.ClassTag

enum ImageViewError:
  case BlankLayerId
  case InvalidOpacity(value: Double)
  case EmptyLayers
  case DuplicateLayerId(id: LayerId)
  case UnknownLayer(id: LayerId)
  case WindowUnsupported(id: LayerId)
  case ThresholdUnsupported(id: LayerId)
  case IncompatibleFrameCounts(counts: Vector[Int])
  case TimepointOutOfBounds(index: Int, count: Int)
  case PointerOutsidePanel(plane: AnatomicalPlane)
  case PointerOutsideViewer
  case InvalidPointer(x: Double, y: Double)
  case InvalidSliceStep(value: Double)
  case InvalidZoom(value: Double)
  case InvalidViewCenter(x: Double, y: Double, zoom: Double)
  case SourceFailed(id: LayerId, cause: VolumeSourceError)
  case InvalidCacheCapacity(value: Int)
  case InvalidOrthogonalLayout(margin: Double, gap: Double)
  case SamplingFailed(id: LayerId, cause: SlicePlanError)
  case GeometryFailure(cause: GeometryError)
  case GraphicsFailure(cause: GraphicsError)

  def message: String =
    this match
      case BlankLayerId =>
        "layer id must not be blank"
      case InvalidOpacity(value) =>
        s"layer opacity must be finite and in [0, 1]; got $value"
      case EmptyLayers =>
        "a viewer model requires at least one layer"
      case DuplicateLayerId(id) =>
        s"viewer layer id '${id.asString}' is duplicated"
      case UnknownLayer(id) =>
        s"viewer layer '${id.asString}' does not exist"
      case WindowUnsupported(id) =>
        s"viewer layer '${id.asString}' does not support display windows"
      case ThresholdUnsupported(id) =>
        s"viewer layer '${id.asString}' does not support display thresholds"
      case IncompatibleFrameCounts(counts) =>
        s"temporal viewer layers must have the same frame count; got ${counts.mkString(", ")}"
      case TimepointOutOfBounds(index, count) =>
        s"timepoint $index is outside 0..${count - 1}"
      case PointerOutsidePanel(plane) =>
        s"pointer is outside the $plane panel"
      case PointerOutsideViewer =>
        "pointer is outside every viewer panel"
      case InvalidPointer(x, y) =>
        s"viewer pointer coordinates must be finite; got ($x, $y)"
      case InvalidSliceStep(value) =>
        s"slice step must be finite and positive; got $value"
      case InvalidZoom(value) =>
        s"viewer zoom must be finite and at least 1; got $value"
      case InvalidViewCenter(x, y, zoom) =>
        s"viewer center ($x, $y) must keep zoom $zoom inside the image"
      case SourceFailed(id, cause) =>
        s"layer '${id.asString}' source failed: ${cause.message}"
      case InvalidCacheCapacity(value) =>
        s"viewer cache capacity must be non-negative; got $value"
      case InvalidOrthogonalLayout(margin, gap) =>
        s"viewer margin and gap must be finite, non-negative, and leave positive panels; got ($margin, $gap)"
      case SamplingFailed(id, cause) =>
        s"layer '${id.asString}' could not be sampled: ${cause.message}"
      case GeometryFailure(cause) =>
        cause.message
      case GraphicsFailure(cause) =>
        cause.message

opaque type LayerId = String

object LayerId:
  def make(value: String): Either[ImageViewError, LayerId] =
    val trimmed = value.trim
    if trimmed.isEmpty then Left(ImageViewError.BlankLayerId) else Right(trimmed)

  def unsafe(value: String): LayerId =
    make(value).fold(err => throw new IllegalArgumentException(err.message), identity)

extension (id: LayerId)
  def asString: String = id

opaque type LayerOpacity = Double

object LayerOpacity:
  def make(value: Double): Either[ImageViewError, LayerOpacity] =
    DisplayOpacity.make(value)
      .left
      .map(_ => ImageViewError.InvalidOpacity(value))
      .map(DisplayOpacity.value)

  def unsafe(value: Double): LayerOpacity =
    make(value).fold(err => throw new IllegalArgumentException(err.message), identity)

  val Opaque: LayerOpacity =
    1.0

extension (opacity: LayerOpacity)
  def toDouble: Double = opacity

enum VolumeSourceError:
  case InvalidFrameCount(value: Int)
  case TimepointOutOfBounds(index: Int, count: Int)
  case ReadFailed(index: Int, reason: String)
  case Geometry(cause: GeometryError)

  def message: String =
    this match
      case InvalidFrameCount(value) =>
        s"volume source frame count must be positive; got $value"
      case TimepointOutOfBounds(index, count) =>
        s"source timepoint $index is outside 0..${count - 1}"
      case ReadFailed(index, reason) =>
        s"could not read source timepoint $index: $reason"
      case Geometry(cause) =>
        cause.message

final class VolumeSource[A, Sem] private (
  val space: Grid[? <: Frame[D3], D3],
  val frameCount: Int,
  val timeInvariant: Boolean,
  readFrame: Int => Either[String, SomeNeuroVolume[A, Sem]]
):
  def volumeAt(timepoint: Int): Either[VolumeSourceError, SomeNeuroVolume[A, Sem]] =
    val index = if timeInvariant then 0 else timepoint
    if index < 0 || index >= frameCount then
      Left(VolumeSourceError.TimepointOutOfBounds(index, frameCount))
    else
      readFrame(index)
        .left
        .map(reason => VolumeSourceError.ReadFailed(index, reason))
        .flatMap { volume =>
          Grid
            .exactCongruence(space, volume.grid)
            .left
            .map(VolumeSourceError.Geometry.apply)
            .map(_ => volume)
        }

object VolumeSource:
  def lazyFrames[A, Sem](
    space: Grid[? <: Frame[D3], D3],
    frameCount: Int
  )(
    readFrame: Int => Either[String, SomeNeuroVolume[A, Sem]]
  ): Either[VolumeSourceError, VolumeSource[A, Sem]] =
    if frameCount <= 0 then Left(VolumeSourceError.InvalidFrameCount(frameCount))
    else Right(new VolumeSource(space, frameCount, timeInvariant = false, readFrame))

  def static[A, Sem](volume: SomeNeuroVolume[A, Sem]): VolumeSource[A, Sem] =
    new VolumeSource(volume.grid, 1, timeInvariant = true, _ => Right(volume))

  def series[A: ClassTag, Sem](series: SomeNeuroSeries[A, Sem]): VolumeSource[A, Sem] =
    new VolumeSource(
      series.grid,
      series.nVolumes,
      timeInvariant = series.nVolumes == 1,
      index => series.volumeAt(index).left.map(_.message)
    )

enum LayerMapping:
  case WorldAligned
  case Pullback(referenceToSource: SpatialPullback)

enum LayerSampleValue:
  case Scalar(value: Double)
  case Label(value: Int)
  case Mask(value: Boolean)

trait LayerValue[A]:
  def sampleValue(value: A): LayerSampleValue

object LayerValue:
  given LayerValue[Double] with
    def sampleValue(value: Double): LayerSampleValue =
      LayerSampleValue.Scalar(value)

  given LayerValue[Int] with
    def sampleValue(value: Int): LayerSampleValue =
      LayerSampleValue.Label(value)

  given LayerValue[Boolean] with
    def sampleValue(value: Boolean): LayerSampleValue =
      LayerSampleValue.Mask(value)

private[view] sealed trait SampledLayerSlice:
  def dimensions: SliceDimensions
  def readout(column: Int, row: Int): Option[LayerSampleValue]
  def colorize(
    window: Option[DisplayWindow],
    threshold: Option[DisplayThreshold]
  ): RasterImage

private[view] sealed trait ResolvedLayerFrame:
  def sample(grid: SliceGrid): Either[ImageViewError, SampledLayerSlice]

sealed trait SliceLayer:
  def id: LayerId
  def opacity: LayerOpacity
  def displayInterpolation: RasterInterpolation
  def frameCount: Int
  def timeInvariant: Boolean
  def supportsWindow: Boolean
  def supportsThreshold: Boolean
  private[view] def sourceSpace: Grid[? <: Frame[D3], D3]
  private[view] def resolve(timepoint: Int): Either[ImageViewError, ResolvedLayerFrame]
  private[view] def sample(
    grid: SliceGrid,
    timepoint: Int
  ): Either[ImageViewError, SampledLayerSlice] =
    resolve(timepoint).flatMap(_.sample(grid))

  private[view] final def raster(
    grid: SliceGrid,
    timepoint: Int,
    window: Option[DisplayWindow],
    threshold: Option[DisplayThreshold]
  ): Either[ImageViewError, RasterImage] =
    sample(grid, timepoint).map(_.colorize(window, threshold))

object SliceLayer:
  def apply[A: ClassTag: LayerValue, Sem](
    id: LayerId,
    volume: SomeNeuroVolume[A, Sem],
    sampling: SliceSampling[A, Sem],
    colorizer: Colorizer[A],
    opacity: LayerOpacity = LayerOpacity.Opaque,
    displayInterpolation: RasterInterpolation = RasterInterpolation.Nearest,
    mapping: LayerMapping = LayerMapping.WorldAligned
  ): SliceLayer =
    fromSource(id, VolumeSource.static(volume), sampling, colorizer, opacity, displayInterpolation, mapping)

  def series[A: ClassTag: LayerValue, Sem](
    id: LayerId,
    series: SomeNeuroSeries[A, Sem],
    sampling: SliceSampling[A, Sem],
    colorizer: Colorizer[A],
    opacity: LayerOpacity = LayerOpacity.Opaque,
    displayInterpolation: RasterInterpolation = RasterInterpolation.Nearest,
    mapping: LayerMapping = LayerMapping.WorldAligned
  ): SliceLayer =
    fromSource(id, VolumeSource.series(series), sampling, colorizer, opacity, displayInterpolation, mapping)

  def fromSource[A: ClassTag: LayerValue, Sem](
    id: LayerId,
    source: VolumeSource[A, Sem],
    sampling: SliceSampling[A, Sem],
    colorizer: Colorizer[A],
    opacity: LayerOpacity = LayerOpacity.Opaque,
    displayInterpolation: RasterInterpolation = RasterInterpolation.Nearest,
    mapping: LayerMapping = LayerMapping.WorldAligned
  ): SliceLayer =
    Typed(id, source, sampling, colorizer, opacity, displayInterpolation, mapping)

  private final case class Typed[A: ClassTag: LayerValue, Sem](
    id: LayerId,
    source: VolumeSource[A, Sem],
    sampling: SliceSampling[A, Sem],
    colorizer: Colorizer[A],
    opacity: LayerOpacity,
    displayInterpolation: RasterInterpolation,
    mapping: LayerMapping
  ) extends SliceLayer:
    def frameCount: Int =
      source.frameCount

    def timeInvariant: Boolean =
      source.timeInvariant

    def supportsWindow: Boolean =
      colorizer.supportsWindow

    def supportsThreshold: Boolean =
      colorizer.supportsThreshold

    private[view] def sourceSpace: Grid[? <: Frame[D3], D3] =
      source.space

    private[view] def resolve(
      timepoint: Int
    ): Either[ImageViewError, ResolvedLayerFrame] =
      source.volumeAt(timepoint)
        .left
        .map(error => ImageViewError.SourceFailed(id, error))
        .map(volume => TypedFrame(id, volume, sampling, colorizer, mapping))

  private final case class TypedFrame[A: ClassTag: LayerValue, Sem](
    id: LayerId,
    volume: SomeNeuroVolume[A, Sem],
    sampling: SliceSampling[A, Sem],
    colorizer: Colorizer[A],
    mapping: LayerMapping
  ) extends ResolvedLayerFrame:
    def sample(grid: SliceGrid): Either[ImageViewError, SampledLayerSlice] =
      val sampled =
        mapping match
          case LayerMapping.WorldAligned =>
            SlicePlan.make(volume.grid, grid).sample(volume, sampling)
          case LayerMapping.Pullback(referenceToSource) =>
            MappedSlicePlan
              .make(volume.grid, grid, referenceToSource)
              .flatMap(_.sample(volume, sampling))
      sampled
        .left
        .map(error => ImageViewError.SamplingFailed(id, error))
        .map(slice => TypedSample(slice, colorizer))

  private final case class TypedSample[A](
    slice: SliceImage[A],
    colorizer: Colorizer[A]
  )(using layerValue: LayerValue[A]) extends SampledLayerSlice:
    def dimensions: SliceDimensions =
      slice.dimensions

    def readout(column: Int, row: Int): Option[LayerSampleValue] =
      if column < 0 || column >= dimensions.width || row < 0 || row >= dimensions.height then None
      else Some(layerValue.sampleValue(slice(column, row)))

    def colorize(
      window: Option[DisplayWindow],
      threshold: Option[DisplayThreshold]
    ): RasterImage =
      val windowed = window.flatMap(colorizer.withWindow).getOrElse(colorizer)
      val activeColorizer = threshold.flatMap(windowed.withThreshold).getOrElse(windowed)
      val dimensions = RasterDimensions.unsafe(slice.dimensions.width, slice.dimensions.height)
      val pixels = new Array[Int](dimensions.pixelCount)
      var index = 0
      while index < pixels.length do
        pixels(index) =
          activeColorizer
            .color(slice.valueAtCanonicalOrdinal(index))
            .toPackedInt
        index += 1
      RasterImage.unsafeFromOwnedPackedArray(dimensions, pixels)

final case class ViewerModel private (
  referenceSpace: Grid[? <: Frame[D3], D3],
  layers: Vector[SliceLayer]
): 
  val timepointCount: Int =
    layers.iterator.filterNot(_.timeInvariant).map(_.frameCount).toVector.headOption.getOrElse(1)

  def layer(id: LayerId): Option[SliceLayer] =
    layers.find(_.id == id)

object ViewerModel:
  def make(
    referenceSpace: Grid[? <: Frame[D3], D3],
    layers: Vector[SliceLayer]
  ): Either[ImageViewError, ViewerModel] =
    if layers.isEmpty then Left(ImageViewError.EmptyLayers)
    else
      layers.groupBy(_.id).collectFirst { case (id, duplicates) if duplicates.lengthCompare(1) > 0 => id } match
        case Some(id) => Left(ImageViewError.DuplicateLayerId(id))
        case None =>
          val temporalCounts = layers.filterNot(_.timeInvariant).map(_.frameCount).distinct
          if temporalCounts.lengthCompare(1) > 0 then
            Left(ImageViewError.IncompatibleFrameCounts(temporalCounts.sorted))
          else Right(new ViewerModel(referenceSpace, layers))

  def fromLayers(first: SliceLayer, rest: SliceLayer*): Either[ImageViewError, ViewerModel] =
    make(first.sourceSpace, first +: rest.toVector)

  def unsafe(
      referenceSpace: Grid[? <: Frame[D3], D3],
      layers: Vector[SliceLayer]
  ): ViewerModel =
    make(referenceSpace, layers).fold(err => throw new IllegalArgumentException(err.message), identity)
