package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.Uint8Array
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

/** Inert until explicitly called by the native paired-camera acceptance harness. */
object ThreePairedNativeProbe:
  private val left = SurfaceId.unsafe("left")
  private val right = SurfaceId.unsafe("right")
  private val leftLayer = SurfaceLayerId.unsafe("left-colors")
  private val rightLayer = SurfaceLayerId.unsafe("right-colors")
  private val lateral = Map(left -> SurfaceViewpoint.Lateral(CorticalHemisphere.Left), right -> SurfaceViewpoint.Lateral(CorticalHemisphere.Right))
  private val medial = Map(left -> SurfaceViewpoint.Medial(CorticalHemisphere.Left), right -> SurfaceViewpoint.Medial(CorticalHemisphere.Right))
  private def asset(id: SurfaceId, hemisphere: Hemisphere, sign: Double): SurfaceAsset =
    val positions = Vector(3.0, 1.0).flatMap(x =>
      Vector(Vector(sign * x, -2.0, -1.0), Vector(sign * x, 3.0, -1.0), Vector(sign * x, -1.0, 4.0)))
    val faces = if sign < 0 then Vector((0, 2, 1), (3, 4, 5)) else Vector((0, 1, 2), (3, 5, 4))
    val inflated = SurfaceGeometry(TriangleMesh.fromRows(positions, faces), hemisphere, SurfaceKind.Inflated)
    val white = SurfaceGeometry(TriangleMesh.fromRows(positions.map(p => Vector(p(0), p(1) * 0.85 + 0.4, p(2) * 0.9 - 0.2)), faces), hemisphere, SurfaceKind.White)
    SurfaceAsset.make(id, SurfaceSet.of(SurfaceKind.Inflated, inflated, SurfaceKind.White -> white)).toOption.get
  private val assets = Vector(asset(left, Hemisphere.Left, -1), asset(right, Hemisphere.Right, 1))
  private val layers = assets.zipWithIndex.map: (asset, index) =>
    val outer = if index == 0 then Rgba32.unsafe(230, 60, 40) else Rgba32.unsafe(40, 190, 90)
    val inner = if index == 0 then Rgba32.unsafe(40, 80, 230) else Rgba32.unsafe(220, 180, 40)
    val colors = Vector.fill(3)(outer) ++ Vector.fill(3)(inner)
    val inverted = colors.map(c => Rgba32.unsafe(255 - c.red, 255 - c.green, 255 - c.blue))
    SurfaceLayer.packedRgba(if index == 0 then leftLayer else rightLayer, asset.id, asset.geometry,
      colors ++ inverted, frameCount = 2).toOption.get
  private val model = SurfaceViewerModel.make(assets, layers).toOption.get
  private def initial(perspective: Boolean): SurfaceViewerState =
    val value = SurfaceViewerState.initial(model)
    value.copy(layout = SurfaceLayout.Bilateral(left, right), lighting = SurfaceLighting.Unlit,
      surfaceViewpoints = lateral, camera = value.camera.copy(projection = if perspective then
        CameraProjection.Perspective(FieldOfViewDegrees.unsafe(45)) else CameraProjection.Orthographic(OrthographicScale.unsafe(4.5))))
  private val stages: Vector[(String, Int, Int, Vector[SurfaceViewerAction])] = Vector(
    ("lateral", 600, 320, Vector.empty),
    ("medial", 600, 320, Vector(SurfaceViewerAction.SetSurfaceViewpoints(medial))),
    ("orbit", 600, 320, Vector(SurfaceViewerAction.OrbitBy(17, 11))),
    ("tall", 320, 600, Vector.empty),
    ("reorder", 320, 600, Vector(SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(left, right, BilateralOrder.values.last)))),
    ("white", 600, 320, Vector(SurfaceViewerAction.SetGeometryState(left, SurfaceKind.White), SurfaceViewerAction.SetGeometryState(right, SurfaceKind.White))),
    ("morph", 600, 320, Vector(SurfaceViewerAction.BeginGeometryMorph(left, SurfaceKind.Inflated), SurfaceViewerAction.BeginGeometryMorph(right, SurfaceKind.Inflated),
      SurfaceViewerAction.SetGeometryMorphFraction(left, SurfaceMorphFraction.unsafe(0.5)), SurfaceViewerAction.SetGeometryMorphFraction(right, SurfaceMorphFraction.unsafe(0.5)))),
    ("time", 600, 320, Vector(SurfaceViewerAction.SetTimepoint(1))),
    ("opacity", 600, 320, Vector(SurfaceViewerAction.SetLayerOpacity(leftLayer, DisplayOpacity.unsafe(0.5)), SurfaceViewerAction.SetLayerOpacity(rightLayer, DisplayOpacity.unsafe(0.7)))),
    ("fit", 800, 400, Vector(SurfaceViewerAction.FitCamera)),
    ("focus", 400, 400, Vector(SurfaceViewerAction.SetLayout(SurfaceLayout.Single(right)))),
    ("paired", 800, 400, Vector(SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(left, right)))),
    ("reset", 800, 400, Vector(SurfaceViewerAction.ResetCamera)),
    ("global", 800, 400, Vector(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Lateral(CorticalHemisphere.Left)))),
    ("restored", 800, 400, Vector(SurfaceViewerAction.SetSurfaceViewpoints(lateral))),
    ("clipped", 800, 400, Vector(SurfaceViewerAction.SetClipping(SurfaceClipping.NearFar(0.1, 2.0)))),
    ("unclipped", 800, 400, Vector(SurfaceViewerAction.SetClipping(SurfaceClipping.Disabled)))
  )

  @JSExportTopLevel("runScalafimThreePairedNativeProbe")
  def run(three: js.Dynamic, canvas: js.Dynamic, perspective: Boolean, ratio: Int,
    referenceFrames: js.Array[js.Dynamic]): js.Array[js.Dynamic] =
    require(ratio == 1 || ratio == 2)
    val runtime = ThreeJsRuntime.create(three, canvas).fold(e => throw new AssertionError(e.message), identity)
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    val rows = new js.Array[js.Dynamic]()
    var state = initial(perspective)
    try
      for (name, width, height, actions) <- stages do
        actions.foreach(action => state = SurfaceViewer.reduce(model, state, action).toOption.get)
        val plan = SurfaceCompiler.compile(model, state).toOption.get
        val size = ThreeCanvasSize.unsafe(width / ratio, height / ratio, ratio.toDouble)
        val rendered = backend.render(plan, size, forceDraw = true).fold(e => throw new AssertionError(e.message), identity)
        val gl = canvas.applyDynamic("getContext")("webgl2")
        val pixels = new Uint8Array(width * height * 4)
        gl.applyDynamic("readPixels")(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels)
        require(gl.applyDynamic("getError")().asInstanceOf[Int] == 0, s"$name WebGL error")
        val image = canvas.applyDynamic("toDataURL")("image/png").asInstanceOf[String]
        val reference = referenceFrames.find(_.stage.asInstanceOf[String] == name).get
        require(reference.width.asInstanceOf[Int] == width && reference.height.asInstanceOf[Int] == height)
        var hits = 0
        var misses = 0
        var maxColor = 0
        var maxBary = 0.0
        reference.points.asInstanceOf[js.Array[js.Dynamic]].foreach: expected =>
          val x = expected.x.asInstanceOf[Double]
          val y = expected.y.asInstanceOf[Double]
          val actual = backend.pick(x / ratio, y / ratio).fold(e => throw new AssertionError(e.message), identity)
          if expected.surface == null then
            require(actual.isEmpty, s"$name unexpected WebGL pick at $x,$y")
            misses += 1
          else
            val pick = actual.getOrElse(throw new AssertionError(s"$name WebGL miss at $x,$y"))
            require(pick.surface.value == expected.surface.asInstanceOf[String] && pick.face == expected.face.asInstanceOf[Int] && pick.vertex == expected.vertex.asInstanceOf[Int],
              s"$name WebGL pick IDs at $x,$y: $pick")
            val weights = expected.weights.asInstanceOf[js.Array[Double]]
            val bary = pick.barycentric.get
            val error = Vector(math.abs(bary._1 - weights(0)), math.abs(bary._2 - weights(1)), math.abs(bary._3 - weights(2))).max
            maxBary = math.max(maxBary, error)
            require(error <= 1e-4, s"$name WebGL barycentric error $error")
            val offset = ((height - 1 - y.toInt) * width + x.toInt) * 4
            val rgb = expected.rgb.asInstanceOf[js.Array[Int]]
            val colorError = (0 until 3).map(channel => math.abs(pixels(offset + channel).toInt - rgb(channel))).max
            maxColor = math.max(maxColor, colorError)
            require(colorError <= 3, s"$name WebGL color error $colorError at $x,$y")
            hits += 1
        rows.push(js.Dynamic.literal(stage = name, width = width, height = height, ratio = ratio,
          hits = hits, misses = misses, maximumColorError = maxColor, maximumBarycentricError = maxBary,
          geometryUploads = rendered.geometryUploads, geometryUpdates = rendered.geometryUpdates,
          colorUploads = rendered.colorUploads, image = image))
      rows
    finally backend.dispose()
