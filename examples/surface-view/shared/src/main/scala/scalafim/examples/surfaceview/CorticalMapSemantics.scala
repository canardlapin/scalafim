package scalafim.examples.surfaceview

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

enum CorticalMapMode:
  case Scalar, Layered, CurvatureLayered, Nearest, Face

/** Rendering fixture, not an estimated brain map. The overlay is
  * f(y,z) = 4 sin(y/25) cos(z/30) on the original pial coordinates (millimetres).
  * Categories are synthetic anterior/posterior bands, not anatomical parcels.
  * The observed folding underlay is supplied separately with its mesh identity.
  */
object CorticalMapSemantics:
  val Surface: SurfaceId = SurfaceId.unsafe("cortical-semantics-left")
  val Overlay: SurfaceLayerId = SurfaceLayerId.unsafe("synthetic-overlay")
  val Underlay: SurfaceLayerId = SurfaceLayerId.unsafe("sulcal-depth")
  val Curvature: SurfaceLayerId = SurfaceLayerId.unsafe("umbrella-curvature")
  val Palette: Map[Int, Rgba32] = Map(1 -> Rgba32.unsafe(38, 139, 210),
    2 -> Rgba32.unsafe(220, 80, 64), 3 -> Rgba32.unsafe(92, 184, 92))
  val Mapping: ScalarMapping = ScalarMapping(ScalarScale.split(DisplayWindow.unsafe(-4, 4), 0, -1, 1,
    ScalarRamp.linear(Rgba32.unsafe(25, 60, 180), Rgba32.unsafe(170, 205, 245)),
    ScalarRamp.linear(Rgba32.unsafe(250, 200, 140), Rgba32.unsafe(175, 25, 20))).toOption.get)
  val FoldingMapping: ScalarMapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1.5, 1.5),
    ScalarRamp.linear(Rgba32.unsafe(210, 210, 210), Rgba32.unsafe(80, 80, 80))))

  def scalar(y: Double, z: Double): Double = 4 * math.sin(y / 25) * math.cos(z / 30)
  def category(y: Double): Int = if y < -40 then 1 else if y < 0 then 2 else 3

  def build(surfaces: SurfaceSet, folding: SurfaceField[Double], mode: CorticalMapMode): SurfaceViewerExample =
    val pial = surfaces.default
    require(pial.kind == SurfaceKind.Pial && pial.hemisphere == Hemisphere.Left)
    require(folding.geometry.hasSameMeshDomain(pial), "folding field must use the same ordered mesh")
    require(folding.indices.sameElements(Array.tabulate(pial.vertexCount)(identity)), "folding field must be dense and ordered")
    require(folding.data.forall(_.isFinite), "folding field must be finite")
    val values = Array.tabulate(pial.vertexCount): i =>
      scalar(pial.mesh.coordinates(i * 3 + 1), pial.mesh.coordinates(i * 3 + 2))
    val overlay = mode match
      case CorticalMapMode.Nearest =>
        val labels = Array.tabulate(pial.vertexCount)(i => category(pial.mesh.coordinates(i * 3 + 1)))
        SurfaceLayer.labels(Overlay, Surface, pial, labels ++ labels.map(label => label % 3 + 1), LabelColorizer(Palette),
          frameCount = 2, interpolation = SurfaceVertexInterpolation.NearestSample).toOption.get
      case CorticalMapMode.Face =>
        val labels = Array.tabulate(pial.faceCount): face =>
          val y = (0 until 3).map(c => pial.mesh.coordinates(pial.mesh.faceIndices(face * 3 + c) * 3 + 1)).sum / 3
          category(y)
        SurfaceLayer.faceLabels(Overlay, Surface, SurfaceFaceField.make(pial, labels ++ labels.map(label => label % 3 + 1), frameCount = 2).toOption.get, LabelColorizer(Palette))
      case _ => SurfaceLayer.interpolatedScalar(Overlay, Surface, pial, values ++ values.map(_ * 0.8), Mapping,
        frameCount = 2, opacity = DisplayOpacity.unsafe(0.8)).toOption.get
    val layers = mode match
      case CorticalMapMode.Layered =>
        val underlay = SurfaceLayer.interpolatedScalar(Underlay, Surface, pial, folding.data, FoldingMapping).toOption.get
        Vector(underlay, overlay)
      case CorticalMapMode.CurvatureLayered =>
        val values = SurfaceThresholdParityFixture.curvatureValues(MeshTopology.from(pial))
        val mapping = FoldingMapping.resolve(window = Some(DisplayWindow.unsafe(-1, 1))).toOption.get
        val underlay = SurfaceLayer.interpolatedScalar(Curvature, Surface, pial, values.toArray, mapping).toOption.get
        Vector(underlay, overlay)
      case _ => Vector(overlay)
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(Surface, surfaces).toOption.get), layers).toOption.get
    val actions = Vector(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Lateral(CorticalHemisphere.Left)),
      SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit),
      SurfaceViewerAction.SetProjection(CameraProjection.Orthographic(OrthographicScale.unsafe(1.25))),
      SurfaceViewerAction.FitCamera)
    val state = actions.foldLeft(SurfaceViewerState.initial(model))((s, a) => SurfaceViewer.reduce(model, s, a).toOption.get)
    SurfaceViewerExample(model, state, SurfaceCompiler.compile(model, state).toOption.get)
