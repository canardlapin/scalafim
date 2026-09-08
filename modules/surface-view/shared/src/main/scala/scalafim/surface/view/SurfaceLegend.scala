package scalafim.surface.view

import intaglio.*

enum SurfaceLegendError:
  case InvalidRequest(reason: String)
  case UnknownLayer(layer: SurfaceLayerId)
  case MissingPresentation(layer: SurfaceLayerId)
  case HiddenLayer(layer: SurfaceLayerId)
  case UndisplayedLayer(layer: SurfaceLayerId)
  case OpaqueMapping(layer: SurfaceLayerId)
  case WrongScale(layer: SurfaceLayerId, expected: String, actual: ScalarScaleKind)
  case InterpolatedCategories(layer: SurfaceLayerId)
  case IncompatibleMappings(layers: Vector[SurfaceLayerId])
  case InvalidMapping(error: SurfaceViewError)
  case InvalidCalibration(error: LegendError)
  case StaleLegend

  def message: String = this match
    case InvalidRequest(reason) => s"invalid surface legend: $reason"
    case UnknownLayer(layer) => s"unknown legend layer: ${layer.value}"
    case MissingPresentation(layer) => s"missing legend layer presentation: ${layer.value}"
    case HiddenLayer(layer) => s"legend layer ${layer.value} is hidden"
    case UndisplayedLayer(layer) => s"legend layer ${layer.value} is absent from the displayed layout or layer order"
    case OpaqueMapping(layer) => s"layer ${layer.value} has no inspectable mapping for this legend"
    case WrongScale(layer, expected, actual) => s"layer ${layer.value} needs a $expected legend, got $actual scale"
    case InterpolatedCategories(layer) => s"categorical legend for ${layer.value} requires face-constant or nearest-sample colors"
    case IncompatibleMappings(layers) => s"shared legend layers have different effective mappings: ${layers.map(_.value).mkString(", ")}"
    case InvalidMapping(error) => error.message
    case InvalidCalibration(error) => error.message
    case StaleLegend => "legend binding no longer matches the effective layer mappings; bind the request again"

/** A nonempty, unique set. Ordering does not alter shared-legend identity. */
final class SurfaceLegendLayers private (val ids: Vector[SurfaceLayerId])

object SurfaceLegendLayers:
  def make(ids: Vector[SurfaceLayerId]): Either[SurfaceLegendError, SurfaceLegendLayers] =
    if ids.isEmpty || ids.distinct.length != ids.length then
      Left(SurfaceLegendError.InvalidRequest("legend layers must be nonempty and unique"))
    else Right(new SurfaceLegendLayers(ids.sortBy(_.value)))

  def one(id: SurfaceLayerId): SurfaceLegendLayers = new SurfaceLegendLayers(Vector(id))

/** Automatic requests contain no editable limits or colors. Those are resolved
  * from the named layers every time the request is bound. Empty ticks select
  * mapping-derived ticks; explicit ticks are checked against the current window.
  */
enum SurfaceLegendRequest:
  case Continuous(layers: SurfaceLegendLayers, title: LegendTitle,
    ticks: Vector[AxisTick] = Vector.empty, showHidden: Boolean = true, showInvalid: Boolean = true)
  case Split(layers: SurfaceLegendLayers, title: LegendTitle,
    ticks: Vector[AxisTick] = Vector.empty, showHidden: Boolean = true, showInvalid: Boolean = true)
  case Categorical(layers: SurfaceLegendLayers, title: LegendTitle,
    labels: Map[Int, String], fallbackLabel: String = "Other / unmapped")
  case Manual(title: LegendTitle, entries: Vector[SurfaceLegendItem])

final case class SurfaceCategoryLegendEntry(id: Int, label: String, color: Rgba32)

enum SurfaceLegendContent:
  case Continuous(calibration: ScalarLegend)
  case Split(calibration: ScalarLegend)
  case Categorical(title: LegendTitle, entries: Vector[SurfaceCategoryLegendEntry], fallback: SurfaceLegendItem)
  case Manual(title: LegendTitle, entries: Vector[SurfaceLegendItem])

final case class SurfaceLegendSource(
  layer: SurfaceLayerId,
  mappingKey: String,
  interpolation: SurfaceMapInterpolation
)

/** Validated snapshot, including its request so stale snapshots can be checked.
  * The key includes mapping, metadata, source layers and interpolation, but no
  * camera, lighting, opacity, viewport, or typography.
  */
final class SurfaceLegend private (
  val request: SurfaceLegendRequest,
  val content: SurfaceLegendContent,
  val sources: Vector[SurfaceLegendSource],
  val canonicalKey: String,
  val notes: Vector[String]
):
  def validateCurrent(model: SurfaceViewerModel, state: SurfaceViewerState): Either[SurfaceLegendError, Unit] =
    SurfaceLegend.bind(request, model, state).flatMap: current =>
      if current.canonicalKey == canonicalKey then Right(()) else Left(SurfaceLegendError.StaleLegend)

object SurfaceLegend:
  private def text(value: String): String = s"${value.length}:$value"
  private def titleKey(title: LegendTitle): String = text(title.quantity) + title.units.fold("none")(text)
  private def swatchKey(entry: SurfaceLegendItem): String = s"${text(entry.label)}:${entry.color.toPackedInt}"
  private def paletteKey(palette: LabelColorizer): String =
    val entries = palette.colors.toVector.sortBy(_._1).map((id, color) => s"$id:${color.toPackedInt}").mkString(";")
    s"surface-category-mapping-v1|$entries|fallback:${palette.fallback.toPackedInt}"

  def bind(request: SurfaceLegendRequest, model: SurfaceViewerModel, state: SurfaceViewerState)
      : Either[SurfaceLegendError, SurfaceLegend] = request match
    case SurfaceLegendRequest.Continuous(layers, title, ticks, hidden, invalid) =>
      scalar(request, layers, title, ticks, hidden, invalid, split = false, model, state)
    case SurfaceLegendRequest.Split(layers, title, ticks, hidden, invalid) =>
      scalar(request, layers, title, ticks, hidden, invalid, split = true, model, state)
    case SurfaceLegendRequest.Categorical(layers, title, labels, fallbackLabel) =>
      categorical(request, layers, title, labels, fallbackLabel, model, state)
    case SurfaceLegendRequest.Manual(title, entries) =>
      if entries.isEmpty then Left(SurfaceLegendError.InvalidRequest("manual legend needs at least one entry"))
      else Right(finish(request, SurfaceLegendContent.Manual(title, entries), Vector.empty,
        s"manual|${titleKey(title)}|${entries.map(swatchKey).mkString(";")}", Vector("Manual color key")))

  private def selected(layers: SurfaceLegendLayers, model: SurfaceViewerModel, state: SurfaceViewerState)
      : Either[SurfaceLegendError, Vector[(SurfaceLayer, SurfaceLayerPresentation)]] =
    layers.ids.foldLeft[Either[SurfaceLegendError, Vector[(SurfaceLayer, SurfaceLayerPresentation)]]](Right(Vector.empty)):
      (result, id) =>
        for
          previous <- result
          layer <- model.layer(id).toRight(SurfaceLegendError.UnknownLayer(id))
          presentation <- state.presentations.get(id).toRight(SurfaceLegendError.MissingPresentation(id))
          _ <- if presentation.id != id then Left(SurfaceLegendError.InvalidRequest("presentation id differs from its layer key"))
            else if !presentation.visible then Left(SurfaceLegendError.HiddenLayer(id)) else Right(())
          displayed = state.layout match
            case SurfaceLayout.Single(surface) => surface == layer.surfaceId
            case SurfaceLayout.Bilateral(left, right, _) => left == layer.surfaceId || right == layer.surfaceId
          _ <- if displayed && state.layerOrder.contains(id) then Right(()) else Left(SurfaceLegendError.UndisplayedLayer(id))
        yield previous :+ (layer -> presentation)

  private def scalar(request: SurfaceLegendRequest, layers: SurfaceLegendLayers, title: LegendTitle,
    ticks: Vector[AxisTick], hidden: Boolean, invalid: Boolean, split: Boolean,
    model: SurfaceViewerModel, state: SurfaceViewerState): Either[SurfaceLegendError, SurfaceLegend] =
    for
      selectedLayers <- selected(layers, model, state)
      mappings <- selectedLayers.foldLeft[Either[SurfaceLegendError, Vector[(SurfaceLayer, ScalarMapping)]]](Right(Vector.empty)):
        case (result, (layer, presentation)) =>
          for
            previous <- result
            inspected <- layer.effectiveScalarMapping(presentation).left.map(SurfaceLegendError.InvalidMapping.apply)
            mapping <- inspected.toRight(SurfaceLegendError.OpaqueMapping(layer.id))
            _ <- if (mapping.scale.kind == ScalarScaleKind.Split) == split then Right(())
              else Left(SurfaceLegendError.WrongScale(layer.id, if split then "split" else "continuous", mapping.scale.kind))
          yield previous :+ (layer -> mapping)
      _ <- if mappings.map(_._2.canonicalKey).distinct.length == 1 then Right(())
        else Left(SurfaceLegendError.IncompatibleMappings(layers.ids))
      legend <- ScalarLegend.make(mappings.head._2, title, ticks, hidden, invalid)
        .left.map(SurfaceLegendError.InvalidCalibration.apply)
    yield
      val sources = mappings.map((layer, mapping) => SurfaceLegendSource(layer.id, mapping.canonicalKey, layer.interpolation))
      val content = if split then SurfaceLegendContent.Split(legend) else SurfaceLegendContent.Continuous(legend)
      val interpolationNote = if sources.exists(_.interpolation == SurfaceMapInterpolation.VertexColor) then
        Vector("Mapped vertex colors interpolate between samples") else Vector.empty
      finish(request, content, sources, legend.canonicalKey, legend.notes ++ interpolationNote ++
        Vector("Layer opacity and compositing can change displayed colors"))

  private def categorical(request: SurfaceLegendRequest, layers: SurfaceLegendLayers, title: LegendTitle,
    labels: Map[Int, String], fallbackLabel: String, model: SurfaceViewerModel, state: SurfaceViewerState)
      : Either[SurfaceLegendError, SurfaceLegend] =
    for
      selectedLayers <- selected(layers, model, state)
      palettes <- selectedLayers.foldLeft[Either[SurfaceLegendError, Vector[(SurfaceLayer, LabelColorizer)]]](Right(Vector.empty)):
        case (result, (layer, _)) =>
          for
            previous <- result
            palette <- layer.labelMapping.toRight(SurfaceLegendError.OpaqueMapping(layer.id))
            _ <- if layer.interpolation == SurfaceMapInterpolation.VertexColor then
              Left(SurfaceLegendError.InterpolatedCategories(layer.id)) else Right(())
          yield previous :+ (layer -> palette)
      _ <- if palettes.map(p => paletteKey(p._2)).distinct.length == 1 then Right(())
        else Left(SurfaceLegendError.IncompatibleMappings(layers.ids))
      palette = palettes.head._2
      _ <- if labels.isEmpty || labels.keySet != palette.colors.keySet || labels.values.exists(_.trim.isEmpty) || fallbackLabel.trim.isEmpty then
        Left(SurfaceLegendError.InvalidRequest("categorical labels must name every palette key exactly once; labels and fallback must be nonempty"))
        else Right(())
    yield
      val entries = palette.colors.toVector.sortBy(_._1).map((id, color) => SurfaceCategoryLegendEntry(id, labels(id).trim, color))
      val fallback = SurfaceLegendItem.unsafe(fallbackLabel, palette.fallback)
      val sources = palettes.map((layer, colors) => SurfaceLegendSource(layer.id, paletteKey(colors), layer.interpolation))
      val key = s"categorical|${titleKey(title)}|${entries.map(e => s"${e.id}:${text(e.label)}:${e.color.toPackedInt}").mkString(";")}|${swatchKey(fallback)}"
      finish(request, SurfaceLegendContent.Categorical(title, entries, fallback), sources, key,
        Vector("Unlit mapping colors", "Layer opacity and compositing can change displayed colors"))

  private def finish(request: SurfaceLegendRequest, content: SurfaceLegendContent, sources: Vector[SurfaceLegendSource],
    contentKey: String, notes: Vector[String]): SurfaceLegend =
    val sourceKey = sources.map(s => s"${text(s.layer.value)}:${text(s.mappingKey)}:${s.interpolation}").mkString(";")
    new SurfaceLegend(request, content, sources, s"surface-legend-v1|${text(contentKey)}|$sourceKey", notes)
