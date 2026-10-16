package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

/** Supplementary scalar-key export specimen. It declares the source mapping
  * before either renderer is invoked; it does not implement frozen C10. */
object SurfaceScalarKeyQualificationFixture:
  final case class Variant(id: String, window: DisplayWindow, threshold: DisplayThreshold,
      palette: Vector[Rgba32], request: SurfaceLegendRequest)
  final case class Endpoint(value: Double, position: Double, color: Rgba32)
  final case class Prepared(variant: Variant, model: SurfaceViewerModel, state: SurfaceViewerState,
      legend: SurfaceLegend, endpoints: Vector[Endpoint], threshold: Option[Vector[Endpoint]])

  val Layer = SurfaceLayerId.unsafe("scalar-key")
  val Values = Array(-4.0, -1.0, 2.0, 8.0)
  private def palette(first: Rgba32, last: Rgba32): Vector[Rgba32] =
    Vector.tabulate(256): index =>
      val t = index.toDouble / 255
      Rgba32.unsafe(math.round(first.red * (1 - t) + last.red * t).toInt,
        math.round(first.green * (1 - t) + last.green * t).toInt,
        math.round(first.blue * (1 - t) + last.blue * t).toInt)
  private val Blue = Rgba32.unsafe(0, 32, 255); private val Red = Rgba32.unsafe(255, 96, 0)
  private val Cyan = Rgba32.unsafe(0, 210, 190); private val Orange = Rgba32.unsafe(255, 150, 20)
  private val title = LegendTitle.make("Synthetic scalar", Some("a.u.")).toOption.get

  val Baseline = Variant("baseline", DisplayWindow.unsafe(-4, 8), DisplayThreshold.Disabled,
    palette(Blue, Red), SurfaceLegendRequest.Continuous(SurfaceLegendLayers.one(Layer), title))
  val Changed = Variant("changed", DisplayWindow.unsafe(-8, 16),
    DisplayThreshold.TransparentBand(ThresholdBand.unsafe(-2, 4)), palette(Cyan, Orange),
    SurfaceLegendRequest.Continuous(SurfaceLegendLayers.one(Layer), title))
  val Variants = Vector(Baseline, Changed)
  val Presets = Vector(SurfacePublicationPreset.ManuscriptSingleColumn, SurfacePublicationPreset.ManuscriptDoubleColumn)

  def prepare(variant: Variant): Either[String, Prepared] =
    val mapping = ScalarMapping(ScalarScale.sequential(variant.window, ScalarRamp.discrete(variant.palette).toOption.get))
    for
      layer <- SurfaceLayer.scalar(Layer, SurfaceLinkedFixture.Surface, SurfaceLinkedFixture.Geometry, Values.clone(), mapping.colorizer).left.map(_.message)
      model <- SurfaceViewerModel.make(Vector(SurfaceLinkedFixture.Asset), Vector(layer)).left.map(_.message)
      initial = SurfaceViewerState.initial(model).copy(camera = SurfaceCamera.unsafe(SurfaceViewpoint.Dorsal,
        CameraProjection.Perspective(FieldOfViewDegrees.unsafe(35))), lighting = SurfaceLighting.Unlit)
      state <- SurfaceViewer.reduce(model, initial, SurfaceViewerAction.SetLayerThreshold(Layer, variant.threshold)).left.map(_.message)
      legend <- SurfaceLegend.bind(variant.request, model, state).left.map(_.message)
      scalar <- legend.content match
        case SurfaceLegendContent.Continuous(value) => Right(value)
        case other => Left(s"expected continuous scalar legend, got $other")
    yield
      val endpoints = Vector(variant.window.lower, variant.window.upper).map(value => Endpoint(value, scalar.position(value), scalar.mapping.color(value)))
      val threshold = variant.threshold match
        case DisplayThreshold.TransparentBand(band) => Some(Vector(band.lower, band.upper).map(value => Endpoint(value, scalar.position(value), scalar.colorAt(scalar.position(value)))))
        case _ => None
      Prepared(variant, model, state, legend, endpoints, threshold)

  def publication(variant: Variant, preset: SurfacePublicationPreset): Either[String, SurfaceLegendPublication] =
    prepare(variant).flatMap: value =>
      SurfaceLegendPublication.prepare(value.model, value.state,
        SurfacePublicationSpec(preset, Some("Supplementary scalar key"), SurfaceOrientationMark.Dorsal), Vector(value.variant.request))
        .left.map(_.message)
