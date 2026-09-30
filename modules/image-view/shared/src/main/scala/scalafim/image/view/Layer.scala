package scalafim.image.view

import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameAlignment
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import intaglio.*
import reframe4s.core.MapError
import reframe4s.core.SpatialMap
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
  case CursorFrameMismatch(cause: GeometryError)
  case GraphicsFailure(cause: GraphicsError)
  case LayerFrameMismatch(id: LayerId, cause: GeometryError)
  case LayerMappingMismatch(id: LayerId, cause: MapError)

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
      case CursorFrameMismatch(cause) =>
        s"viewer cursor is not in the reference frame: ${cause.message}"
      case GraphicsFailure(cause) =>
        cause.message
      case LayerFrameMismatch(id, cause) =>
        s"layer '${id.asString}' is not in the reference frame: ${cause.message}"
      case LayerMappingMismatch(id, cause) =>
        s"layer '${id.asString}' pullback does not join the reference and layer frames: ${cause.message}"

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
  case Pullback(referenceToSource: SpatialPullback[?, ?])

/** Checked evidence of how a layer's frame meets the viewer's reference frame. */
enum LayerAlignment:
  /** The layer's frame is the reference frame (one runtime owner or one persistent key), so world points are shared. */
  case SharedWorld(evidence: FrameAlignment[D3, ?, ?])

  /** A pullback whose source is the reference frame's owner and whose result is the layer frame's owner. */
  case Mapped(referenceToSource: SpatialPullback[?, ?])

object LayerAlignment:
  /** Check a layer's frame (and mapping, if any) against the reference frame. */
  def check(reference: Frame[D3], layer: SliceLayer): Either[ImageViewError, LayerAlignment] =
    layer.mapping match
      case LayerMapping.WorldAligned =>
        Frame
          .alignOwners[D3, Frame[D3], Frame[D3]](layer.frame, reference)
          .left
          .map(error => ImageViewError.LayerFrameMismatch(layer.id, error))
          .map(LayerAlignment.SharedWorld.apply)
      case LayerMapping.Pullback(referenceToSource) =>
        val checked =
          for
            _ <- SpatialMap.validateSourceFrame(referenceToSource.source, reference)
            _ <- SpatialMap.validateResultFrame(layer.frame, referenceToSource.target)
          yield LayerAlignment.Mapped(referenceToSource)
        checked.left.map(error => ImageViewError.LayerMappingMismatch(layer.id, error))

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
  /** How this layer's frame meets the viewer's reference frame. */
  def mapping: LayerMapping

  /** The frame this layer's samples live in. */
  def frame: Frame[D3] = sourceSpace.frame

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

/** Pure model edits. Reordering promotes listed ids in the requested order;
  * unmentioned layers follow in their previous relative order. Replacement
  * takes an already typed SliceLayer, never an existential colorizer cast.
  */
enum ViewerModelUpdate:
  case ReorderLayers(ids: Vector[LayerId])
  case ReplaceLayer(layer: SliceLayer)

/** A viewer scene: a reference grid and layers, each carrying its own frame and a checked alignment to the
  * reference frame (`alignments(i)` belongs to `layers(i)`).
  */
final case class ViewerModel private (
  referenceSpace: Grid[? <: Frame[D3], D3],
  layers: Vector[SliceLayer],
  alignments: Vector[LayerAlignment]
): 
  val timepointCount: Int =
    layers.iterator.filterNot(_.timeInvariant).map(_.frameCount).toVector.headOption.getOrElse(1)

  def layer(id: LayerId): Option[SliceLayer] =
    layers.find(_.id == id)

  def alignment(id: LayerId): Option[LayerAlignment] =
    val index = layers.indexWhere(_.id == id)
    if index < 0 then None else Some(alignments(index))

  /** Alignments are derived from the reference grid and the layers, and their frame evidence compares by identity,
    * so model equality is equality of the reference grid and layers.
    */
  override def equals(other: Any): Boolean =
    other match
      case that: ViewerModel => referenceSpace == that.referenceSpace && layers == that.layers
      case _                 => false

  override def hashCode: Int =
    (referenceSpace, layers).##

  def updated(update: ViewerModelUpdate): Either[ImageViewError, ViewerModel] =
    update match
      case ViewerModelUpdate.ReorderLayers(ids) =>
        ids.groupBy(identity).collectFirst { case (id, occurrences) if occurrences.size > 1 => id } match
          case Some(id) => Left(ImageViewError.DuplicateLayerId(id))
          case None =>
            ids.find(id => layer(id).isEmpty) match
              case Some(id) => Left(ImageViewError.UnknownLayer(id))
              case None =>
                val selected = ids.toSet
                ViewerModel.make(referenceSpace, ids.flatMap(layer) ++ layers.filterNot(l => selected(l.id)))
      case ViewerModelUpdate.ReplaceLayer(replacement) =>
        val index = layers.indexWhere(_.id == replacement.id)
        if index < 0 then Left(ImageViewError.UnknownLayer(replacement.id))
        else ViewerModel.make(referenceSpace, layers.updated(index, replacement))


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
          else
            sequence(layers.map(layer => LayerAlignment.check(referenceSpace.frame, layer)))
              .map(alignments => new ViewerModel(referenceSpace, layers, alignments))

  private def sequence[A](values: Vector[Either[ImageViewError, A]]): Either[ImageViewError, Vector[A]] =
    values.foldLeft[Either[ImageViewError, Vector[A]]](Right(Vector.empty)): (acc, value) =>
      acc.flatMap(built => value.map(built :+ _))

  def fromLayers(first: SliceLayer, rest: SliceLayer*): Either[ImageViewError, ViewerModel] =
    make(first.sourceSpace, first +: rest.toVector)

  def unsafe(
      referenceSpace: Grid[? <: Frame[D3], D3],
      layers: Vector[SliceLayer]
  ): ViewerModel =
    make(referenceSpace, layers).fold(err => throw new IllegalArgumentException(err.message), identity)
