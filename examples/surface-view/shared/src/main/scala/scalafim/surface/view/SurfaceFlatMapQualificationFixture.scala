package scalafim.surface.view

import intaglio.Rgba32
import scalafim.surface.VertexId

/** Frozen C11 source and interaction policy.  This adapter deliberately sits
  * beside, rather than inside, [[SurfaceQualificationFixture]]: flat-map
  * interaction is a host concern and C11 has no surface-scene JSON schema.
  */
object SurfaceFlatMapQualificationFixture:
  final case class Style(background: String, fill: String, stroke: String,
      hover: String, selected: String, roi: String, pointRadius: Double)

  final case class Prepared(source: SurfaceQualificationFixture.Prepared,
      model: SurfaceViewerModel, initial: SurfaceViewerState,
      map: SurfaceFlatMap, viewport: SurfaceFlatMapViewport, style: Style):
    val surface = map.surface

    def selected: Either[String, SurfaceViewerState] =
      SurfaceViewer.reduce(model, initial,
        SurfaceViewerAction.Select(surface, VertexId(0))).left.map(_.message)

    def atTimeOne: Either[String, SurfaceViewerState] =
      selected.flatMap(state => SurfaceViewer.reduce(model, state,
        SurfaceViewerAction.SetTimepoint(1)).left.map(_.message))

    def linkedMarker: Either[String, Vector[SurfaceDecoration]] =
      for
        id <- SurfaceDecorationId.make("c11-linked-marker").left.map(_.toString)
        size <- SurfaceDecorationSize.world(.12).left.map(_.toString)
      yield Vector(SurfaceMarkerDecoration(id,
        SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(surface, VertexId(0))),
        SurfaceMarkerShape.Sphere, size, Rgba32.unsafe(255, 209, 102), SurfaceDecorationDepthPolicy.DepthTest))

  val SourceJson: String =
    """{"width":600,"height":450,"camera":{"projection":"perspective","eye":[0.6,0.375,4.65],"target":[0.6,0.375,0.65],"up":[0,1,0],"near":0.1,"far":2000,"aspect":1.3333333333333333,"fovDegrees":35},"profile":{"name":"controlled-unlit","ambientIntensity":3.141592653589793,"directionalIntensity":0},"timeSeconds":0,"surfaces":[{"id":"flatmap-square2tri","hemisphere":"left","vertices":[[-1,-1,0],[1,-1,0],[-1,1,0],[1,1,0]],"faces":[[0,1,2],[1,3,2]],"layers":[{"id":"temporal","kind":"temporal","frames":[[0,0,0,0],[1,1,1,1]],"times":[0,1],"range":[0,1],"palette":[[0.85,0.85,0.85],[0.85,0.85,0.85]]}]}]}"""

  /** Reference-defined defaults plus the C11 frozen fill and stroke. */
  val FrozenStyle = Style("#050505", "#d9d9d9", "#888888", "#58c4ff", "#ffd166", "#ffd166", 4.0)

  val Polygon = Vector(
    SurfaceFlatMapPoint(-1, -1), SurfaceFlatMapPoint(.1, -1),
    SurfaceFlatMapPoint(.1, .1), SurfaceFlatMapPoint(-1, .1))
  val Lasso = Vector(
    SurfaceFlatMapPoint(-1.1, -1.1), SurfaceFlatMapPoint(.1, -1.1),
    SurfaceFlatMapPoint(.1, .1), SurfaceFlatMapPoint(-1.1, .1), SurfaceFlatMapPoint(-1.1, -1.1))
  val RoiName = "ROI"

  def decode: Either[String, Prepared] =
    for
      source <- SurfaceQualificationFixture.decode(SourceJson)
      id <- source.sourceSummaries.headOption.map(_.surface).toRight("C11 source surface is absent")
      asset <- source.model.surface(id).toRight("C11 source asset is absent")
      layerId <- source.model.layers.headOption.map(_.id).toRight("C11 source layer is absent")
      temporal <- SurfaceLayer.packedRgba(layerId, id, asset.geometry,
        Vector.fill(8)(Rgba32.unsafe(217, 217, 217)), frameCount = 2).left.map(_.message)
      model <- SurfaceViewerModel.make(Vector(asset), Vector(temporal)).left.map(_.message)
      map <- SurfaceFlatMap.make(id,
        Vector(SurfaceFlatMapPoint(-1, -1), SurfaceFlatMapPoint(1, -1),
          SurfaceFlatMapPoint(-1, 1), SurfaceFlatMapPoint(1, 1)),
        Vector((0, 1, 2), (1, 3, 2))).left.map(_.toString)
      viewport <- SurfaceFlatMapViewport.make(600, 450, 20, SurfaceFlatMapFit.Contain).left.map(_.toString)
    yield Prepared(source, model, source.state, map, viewport, FrozenStyle)
