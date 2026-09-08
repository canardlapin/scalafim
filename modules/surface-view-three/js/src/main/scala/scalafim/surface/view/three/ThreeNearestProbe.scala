package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.Uint8Array
import intaglio.*
import scalafim.surface.view.*

/** Synchronous draw/readback keeps the default framebuffer valid without
  * asking the production renderer to preserve its drawing buffer.
  */
object ThreeNearestProbe:
  @JSExportTopLevel("runScalafimThreeNearestProbe")
  def run(three: js.Dynamic, canvas: js.Dynamic): js.Dynamic =
    val model = SurfaceNearestFixture.model
    val state = SurfaceNearestFixture.state(model)
    val plan = SurfaceCompiler.compile(model, state).toOption.get
    val runtime = ThreeJsRuntime.create(three, canvas).toOption.get
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    val size = ThreeCanvasSize.unsafe(256, 256)
    try
      backend.render(plan, size).toOption.get
      val gl = canvas.applyDynamic("getContext")("webgl2")
      val pixels = new Uint8Array(256 * 256 * 4)
      gl.applyDynamic("readPixels")(0, 0, 256, 256, gl.RGBA, gl.UNSIGNED_BYTE, pixels)
      val image = canvas.applyDynamic("toDataURL")("image/png").asInstanceOf[String]
      val first = backend.pick(180.0, 160.0).toOption.flatten.get
      val second = backend.pick(60.0, 80.0).toOption.flatten.get
      require(first.face != second.face, "probe did not sample both faces")
      var maximumError = 0
      var checked = 0
      for
        y <- 50 until 205 by 10
        x <- 50 until 205 by 10
        if math.abs(x + y - 256) > 16
      do
        val pick = backend.pick(x.toDouble + 0.5, y.toDouble + 0.5)
          .fold(error => throw new IllegalStateException(error.message), identity)
          .getOrElse(throw new IllegalStateException(s"nearest probe missed ($x,$y)"))
        validateProjection(pick, x + 0.5, y + 0.5, plan)
        val (a, b, c) = pick.barycentric.get
        val expectedWeights = if pick.face == 0 then
          Vector((1.0 - pick.worldX) / 2.0, (pick.worldX - pick.worldY) / 2.0, (pick.worldY + 1.0) / 2.0)
        else Vector((1.0 - pick.worldY) / 2.0, (pick.worldX + 1.0) / 2.0, (pick.worldY - pick.worldX) / 2.0)
        val weights = Vector(a, b, c)
        require(weights.zip(expectedWeights).forall((actual, expected) => math.abs(actual - expected) < 1e-6),
          s"native original barycentrics differ: $pick expected=$expectedWeights")
        val ranked = weights.sorted.reverse
        val vertices = if pick.face == 0 then Vector(0, 1, 2) else Vector(0, 2, 3)
        val expectedVertex = vertices(weights.indices.maxBy(weights(_)))
        require(pick.vertex == expectedVertex, s"nearest pick mismatch: $pick")
        val expected = SurfaceNearestFixture.Palette(expectedVertex)
        val ids = if pick.face == 0 then Vector(0, 1, 2) else Vector(0, 2, 3)
        require(ids.contains(pick.vertex), s"render vertex escaped as scientific id: ${pick.vertex}")
        val offset = ((255 - y) * 256 + x) * 4
        val error = math.max(math.abs(pixels(offset).toInt - expected.red),
          math.max(math.abs(pixels(offset + 1).toInt - expected.green), math.abs(pixels(offset + 2).toInt - expected.blue)))
        if ranked.last > 0.04 && ranked(0) - ranked(1) > 0.04 then
          maximumError = math.max(maximumError, error)
          checked += 1
      require(checked > 80, s"insufficient nearest coverage: $checked first=$first second=$second")
      require(maximumError <= 1, s"nearest channel error: $maximumError")
      val lighting = verifyLighting(backend, plan, size, canvas)
      // Restore original geometry before measuring a data-only update.
      backend.render(plan, size).toOption.get
      val next = SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetTimepoint(1)).toOption.get
      val update = backend.render(SurfaceCompiler.compile(model, next).toOption.get, size).toOption.get
      require(update.geometryUploads == 0 && update.geometryUpdates == 0, "timepoint uploaded geometry")
      require(update.colorUploads == 1, "timepoint did not update colors")
      val perspective = SurfaceViewer.reduce(model, next,
        SurfaceViewerAction.SetProjection(CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50.0)))).toOption.get
      val oblique = SurfaceViewer.reduce(model, perspective, SurfaceViewerAction.OrbitBy(25.0, 15.0)).toOption.get
      val perspectivePlan = SurfaceCompiler.compile(model, oblique).toOption.get
      val cameraUpdate = backend.render(perspectivePlan, size).toOption.get
      require(cameraUpdate.geometryUploads == 0 && cameraUpdate.geometryUpdates == 0 && cameraUpdate.colorUploads == 0,
        "camera-only update uploaded surface resources")
      var perspectivePicks = 0
      for y <- 60 until 196 by 10; x <- 60 until 196 by 10 do
        backend.pick(x + 0.5, y + 0.5).toOption.flatten.foreach: pick =>
          validateProjection(pick, x + 0.5, y + 0.5, perspectivePlan)
          perspectivePicks += 1
      require(perspectivePicks > 30, s"insufficient perspective pick coverage: $perspectivePicks")
      js.Dynamic.literal(status = "pass", checked = checked, maximumChannelError = maximumError,
        firstFace = first.face, firstVertex = first.vertex, secondFace = second.face, secondVertex = second.vertex,
        geometryUploadsOnTimepoint = update.geometryUploads,
        geometryUpdatesOnTimepoint = update.geometryUpdates, colorUploadsOnTimepoint = update.colorUploads,
        cameraOnlyResourceUploads = cameraUpdate.geometryUploads + cameraUpdate.geometryUpdates + cameraUpdate.colorUploads,
        perspectivePicks = perspectivePicks, lighting = lighting, image = image)
    finally backend.dispose()


  /** Orthogonal unit source normals give shade = 0.2 + 0.8 * original wc.
    * Normalizing the interpolated display-cell normals violates this oracle.
    */
  private def verifyLighting(backend: ThreeSurfaceBackend, plan: SurfaceRenderPlan,
      size: ThreeCanvasSize, canvas: js.Dynamic): js.Dynamic =
    val mesh = plan.meshes.head
    val partition = mesh.nearestPartition.get
    val normals = Array.tabulate(mesh.normals.length): index =>
      val (a, b, c) = partition.weights(index / 3)
      (index % 3 match
        case 0 => a
        case 1 => b
        case _ => c).toFloat
    val lit = plan.copy(meshes = Vector(mesh.copy(normals = new FloatBufferView(normals),
      geometryRevision = Some(SurfaceResourceKey("nearest-lighting-oracle")))),
      lighting = SurfaceLighting.directional(0.2, 0.8, 0, 0, 1).toOption.get)
    backend.render(lit, size, forceDraw = true).fold(e => throw new IllegalArgumentException(e.message), identity)
    val gl = canvas.applyDynamic("getContext")("webgl2")
    val pixels = new Uint8Array(size.width * size.height * 4)
    gl.applyDynamic("readPixels")(0, 0, size.width, size.height, gl.RGBA, gl.UNSIGNED_BYTE, pixels)
    var checked = 0
    var maximumError = 0
    for y <- 50 until 205 by 10; x <- 50 until 205 by 10 do
      val pick = backend.pick(x + 0.5, y + 0.5).toOption.flatten.get
      val (a, b, c) = pick.barycentric.get
      val weights = Vector(a, b, c).sorted
      if weights.head > 0.08 && weights(2) - weights(1) > 0.08 then
        val color = SurfaceNearestFixture.Palette(pick.vertex)
        val shade = 0.2 + 0.8 * c
        val offset = ((size.height - 1 - y) * size.width + x) * 4
        val channels = Vector(color.red, color.green, color.blue)
        for channel <- 0 until 3 do
          val expected = math.round(channels(channel) * shade).toInt
          maximumError = math.max(maximumError, math.abs(pixels(offset + channel).toInt - expected))
        checked += 1
    require(checked > 80 && maximumError <= 1,
      s"nearest analytic lighting failed: checked=$checked error=$maximumError")
    js.Dynamic.literal(checked = checked, maximumChannelError = maximumError, passed = true)

  private def validateProjection(pick: ThreePick, x: Double, y: Double, plan: SurfaceRenderPlan): Unit =
    val world = Array(pick.worldX, pick.worldY, pick.worldZ, 1.0)
    def transform(matrix: FloatBufferView, input: Array[Double]): Array[Double] =
      Array.tabulate(4)(row => (0 until 4).map(column => matrix(row * 4 + column) * input(column)).sum)
    val clip = transform(plan.camera.projectionMatrix, transform(plan.camera.viewMatrix, world))
    val screenX = (clip(0) / clip(3) + 1.0) * 128.0
    val screenY = (1.0 - clip(1) / clip(3)) * 128.0
    require(math.abs(screenX - x) < 1e-5 && math.abs(screenY - y) < 1e-5,
      s"native pick does not reproject to requested pixel: ($x,$y) -> ($screenX,$screenY) $pick")
