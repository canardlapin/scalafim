package scalafim.surface.view

import image4s.geometry.{D3, Frame, Point}
import intaglio.*
import scalafim.surface.*

/** Small independent color/visibility specimens, separate from the frozen
  * cortex gallery. Camera and source values are exported for the JS reference.
  */
object SurfaceVisibilityFixture:
  val Cases = Vector("gray", "gray-half-light", "black-white-gradient", "mixed-gradient",
    "encoded-layer-opacity", "plus-lighter", "curvature-negative", "curvature-zero", "curvature-positive",
    "world-clip", "world-light", "world-light-orbit", "threshold-edge", "threshold-edge-opacity", "threshold-equality", "bivariate-confidence")
  val Id = SurfaceId.unsafe("visibility-plane")
  val Faces = Vector((0, 1, 2), (0, 2, 3))
  def vertices(name: String): Vector[Vector[Double]] =
    Vector(Vector(-1.0, -1.0, 0.0), Vector(1.0, -1.0, 0.0),
      Vector(1.0, 1.0, 0.0), Vector(-1.0, 1.0, 0.0)).map: point =>
      if name.startsWith("world-light") then point.updated(2, -point(0) * .5) else point
  def eye(name: String): Vector[Double] = if name.endsWith("orbit") then Vector(2.0, 0.0, 4.0) else Vector(0.0, 0.0, 4.0)
  def ambient(name: String): Double = if name == "gray-half-light" then .5 else if name.startsWith("world-light") then .2 else 1.0
  def diffuse(name: String): Double = if name.startsWith("world-light") then .8 else 0.0
  def colors(name: String): Vector[Rgba32] =
    if name.contains("gradient") then Vector(0, 255, 255, 0).map(value => Rgba32.unsafe(value, value, value))
    else if name == "encoded-layer-opacity" then Vector.fill(4)(Rgba32.unsafe(255, 255, 255))
    else if name == "plus-lighter" then Vector.fill(4)(Rgba32.unsafe(128, 0, 0))
    else Vector.fill(4)(Rgba32.unsafe(128, 128, 128))
  def curvature(name: String): Option[Double] = name match
    case "curvature-negative" => Some(-.5)
    case "curvature-zero" => Some(0.0)
    case "curvature-positive" => Some(.5)
    case _ => None
  def layerOpacity(name: String): Double = if name == "encoded-layer-opacity" || name == "threshold-edge-opacity" then .5 else 1.0
  def base(name: String): Rgba32 = if name == "encoded-layer-opacity" || name == "plus-lighter" then Rgba32.unsafe(0, 0, 0) else Rgba32.unsafe(184, 184, 184)

  def thresholdValues(name: String): Vector[Double] =
    if name == "threshold-equality" then Vector(-.5, .5, .50001, Double.NaN)
    else Vector(-.25, .75, .75, -.25)
  val ThresholdMapping: ScalarMapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1, 1),
    ScalarRamp.linear(Rgba32.unsafe(255, 0, 0), Rgba32.unsafe(255, 0, 0))),
    visibility = ScalarVisibility.Outside(ScalarInterval.make(-.5, .5, ScalarEndpointInclusion.Both).toOption.get))
  val Effects = Vector(-1.0, -.25, .25, 1.0)
  val Confidence = Vector(0.0, .25, .75, 1.0)
  val BivariateMapping = BivariateEffectConfidenceMapping(ThresholdMapping.copy(visibility = ScalarVisibility.All))

  def plan(name: String): SurfaceRenderPlan =
    require(Cases.contains(name))
    val geometry = SurfaceGeometry(TriangleMesh.fromRows(vertices(name), Faces), Hemisphere.Left, SurfaceKind.Pial)
    val layer = curvature(name) match
      case Some(value) => SurfaceLayer.curvatureUnderlayWithStyle(SurfaceLayerId.unsafe("color"), Id,
        SurfaceField.full(geometry, Vector.fill(4)(value), "analytic curvature"), geometry, SurfaceCurvatureDisplay.Default).toOption.get
      case None if name.startsWith("threshold-") => SurfaceLayer.scalarWithThresholdEdges(SurfaceLayerId.unsafe("color"), Id,
        geometry, thresholdValues(name).toArray, ThresholdMapping, DisplayOpacity.unsafe(layerOpacity(name))).toOption.get
      case None if name == "bivariate-confidence" =>
        val ids = Vector.tabulate(4)(VertexId(_))
        SurfaceLayer.bivariateEffectConfidence(SurfaceLayerId.unsafe("color"), Id,
          SurfaceMatrix.fromRows(geometry, ids, Effects.map(Vector(_))),
          SurfaceMatrix.fromRows(geometry, ids, Confidence.map(Vector(_))), BivariateMapping).toOption.get
      case None => SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("color"), Id, geometry, colors(name),
        opacity = DisplayOpacity.unsafe(layerOpacity(name))).toOption.get
    val second = name match
      case "mixed-gradient" => Vector(SurfaceLayer.facePackedRgba(SurfaceLayerId.unsafe("zero-opacity-face"), Id,
        SurfaceFaceField.make(geometry, Array.fill(2)(Rgba32.unsafe(255, 0, 255))).toOption.get, opacity = DisplayOpacity.Transparent))
      case "plus-lighter" => Vector(SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("green-plus"), Id, geometry,
        Vector.fill(4)(Rgba32.unsafe(0, 128, 0)), blendMode = DisplayBlendMode.PlusLighter).toOption.get)
      case _ => Vector.empty
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(Id, geometry).toOption.get), Vector(layer) ++ second).toOption.get
    val state = SurfaceViewerState.initial(model).copy(
      camera = SurfaceCamera.unsafe(SurfaceViewpoint.Dorsal, CameraProjection.Perspective(FieldOfViewDegrees.unsafe(35))),
      lighting = SurfaceLighting.directional(ambient(name), diffuse(name), 0, 0, 1).toOption.get,
      clipping = if name == "world-clip" then SurfaceClipping.WorldPlanes(Vector(WorldClipPlane.unsafe(1, 0, 0, 0)))
        else SurfaceClipping.nearFar(.01, 1000).toOption.get,
      colorPipeline = SurfaceColorPipeline.DisplaySrgbLinearLight, surfaceBase = base(name))
    val frame = Frame.named[D3]("visibility specimen").toOption.get
    val display = SurfaceDisplayFrame.declare(model, Id, frame).toOption.get
    val cameraEye = eye(name)
    val pose = SurfaceCameraPose.make(Point.in(frame)(0, 0, 0).toOption.get,
      Point.in(frame)(cameraEye(0), cameraEye(1), cameraEye(2)).toOption.get).toOption.get
    SurfaceCompiler.compile(model, state, display, pose).toOption.get

  /** Distinct planes isolate encoded framebuffer source-over from layer opacity. */
  def alphaComposition(frontOpacity: Double, reverse: Boolean): SurfaceCompositionPlan =
    def plane(name: String, z: Double, color: Rgba32): SurfaceRenderPlan =
      val id = SurfaceId.unsafe(name)
      val geometry = SurfaceGeometry(TriangleMesh.fromRows(vertices("gray").map(_.updated(2, z)), Faces),
        Hemisphere.Left, SurfaceKind.Pial)
      val layer = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe(s"$name-color"), id, geometry,
        Vector.fill(4)(color)).toOption.get
      val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(id, geometry).toOption.get), Vector(layer)).toOption.get
      val frame = Frame.named[D3](s"$name fixture").toOption.get
      val display = SurfaceDisplayFrame.declare(model, id, frame).toOption.get
      val pose = SurfaceCameraPose.make(Point.in(frame)(0, 0, 0).toOption.get,
        Point.in(frame)(0, 0, 4).toOption.get).toOption.get
      SurfaceCompiler.compile(model, SurfaceViewerState.initial(model).copy(
        camera = SurfaceCamera.unsafe(SurfaceViewpoint.Dorsal, CameraProjection.Perspective(FieldOfViewDegrees.unsafe(35))),
        lighting = SurfaceLighting.Unlit, clipping = SurfaceClipping.nearFar(.01, 1000).toOption.get,
        colorPipeline = SurfaceColorPipeline.DisplaySrgbLinearLight), display, pose).toOption.get
    val front = plane("front-red", .2, Rgba32.unsafe(255, 0, 0))
    val back = plane("back-blue", -.2, Rgba32.unsafe(0, 0, 255))
    def instance(p: SurfaceRenderPlan, opacity: Double): SurfaceCompositionInstance =
      SurfaceCompositionInstance(SurfaceInstanceId.make(p.meshes.head.surface.value).toOption.get, p.meshes.head.surface,
        p.meshes.head.resourceKey, p.layers.map(_.resourceKey), opacity = DisplayOpacity.unsafe(opacity))
    val instances = Vector(instance(front, frontOpacity), instance(back, 1.0))
    SurfaceCompositionPlan.make(Vector(SurfaceCompositionView(SurfaceViewId.make("alpha").toOption.get,
      SurfaceViewport(0, 0, 1, 1), front.camera, if reverse then instances.reverse else instances,
      SurfaceLighting.Unlit, front.clipping, colorPipeline = SurfaceColorPipeline.DisplaySrgbLinearLight)),
      SurfaceCompositionResources((front.meshes ++ back.meshes).map(m => m.resourceKey -> m).toMap,
        (front.layers ++ back.layers).map(l => l.resourceKey -> l).toMap)).toOption.get
