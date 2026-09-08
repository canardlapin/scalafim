package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.Uint8Array
import intaglio.*
import scalafim.surface.view.*

/** Synchronous draw/readback keeps the default framebuffer valid without
  * asking the production renderer to preserve its drawing buffer.
  */
object ThreeFaceProbe:
  @JSExportTopLevel("runScalafimThreeFaceProbe")
  def run(three: js.Dynamic, canvas: js.Dynamic): js.Dynamic =
    val model = SurfaceFaceFixture.model
    val state = SurfaceFaceFixture.state(model)
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
        val pick = backend.pick(x.toDouble + 0.5, y.toDouble + 0.5).toOption.flatten.get
        val expected = if pick.face == 0 then SurfaceFaceFixture.Red else SurfaceFaceFixture.Blue
        val ids = if pick.face == 0 then Vector(0, 1, 2) else Vector(0, 2, 3)
        require(ids.contains(pick.vertex), s"render vertex escaped as scientific id: ${pick.vertex}")
        val offset = ((255 - y) * 256 + x) * 4
        val error = math.max(math.abs(pixels(offset).toInt - expected.red),
          math.max(math.abs(pixels(offset + 1).toInt - expected.green), math.abs(pixels(offset + 2).toInt - expected.blue)))
        maximumError = math.max(maximumError, error)
        checked += 1
      require(maximumError <= 1, s"facewise channel error: $maximumError")
      val next = SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetTimepoint(1)).toOption.get
      val update = backend.render(SurfaceCompiler.compile(model, next).toOption.get, size).toOption.get
      require(update.geometryUploads == 0 && update.geometryUpdates == 0, "timepoint uploaded geometry")
      require(update.colorUploads == 1, "timepoint did not update colors")
      js.Dynamic.literal(status = "pass", checked = checked, maximumChannelError = maximumError,
        firstFace = first.face, firstVertex = first.vertex, secondFace = second.face, secondVertex = second.vertex,
        geometryUploadsOnTimepoint = update.geometryUploads, image = image)
    finally backend.dispose()
