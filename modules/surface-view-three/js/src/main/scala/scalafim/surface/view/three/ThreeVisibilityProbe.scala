package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel
import intaglio.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

object ThreeVisibilityProbe:
  private def ok[A](value: Either[?, A]): A = value.fold(error => throw new IllegalStateException(s"$error"), identity)
  @JSExportTopLevel("runScalafimVisibilityProbe")
  def run(three: js.Dynamic, canvas: js.Dynamic, rasterCanvas: js.Dynamic): js.Dynamic =
    val backend = ok(ThreeSurfaceBackend.create(ok(ThreeJsRuntime.create(three, canvas))))
    val host = ok(CanvasRasterSurfaceHost.attach(rasterCanvas))
    val captures = new js.Array[js.Any]()
    try
      for name <- SurfaceVisibilityFixture.Cases do
        val plan = SurfaceVisibilityFixture.plan(name)
        val size = ThreeCanvasSize.unsafe(256, 256)
        ok(backend.render(plan, size, forceDraw = true))
        val native = canvas.applyDynamic("toDataURL")("image/png")
        val composition = ok(SurfaceCompositionPlan.fromLegacy(plan))
        val receipt = ok(host.submit(composition, RasterDimensions.unsafe(256, 256),
          SurfaceRasterStyle(culling = TriangleCulling.None)))
        val picks = new js.Array[js.Any]()
        for y <- 48 until 208 by 8; x <- 48 until 208 by 8 do
          val nativePick = ok(backend.pick(x + .5, y + .5))
          val rasterPick = ok(host.pickDisplay(x + .5, y + .5))
          picks.push(js.Dynamic.literal(x = x, y = y, nativePresent = nativePick.nonEmpty,
            rasterPresent = rasterPick.nonEmpty))
        val p = plan.camera.projectionMatrix
        captures.push(js.Dynamic.literal(name = name, native = native,
          raster = rasterCanvas.applyDynamic("toDataURL")("image/png"), picks = picks,
          renderNanos = receipt.renderNanos.toDouble,
          vertices = SurfaceVisibilityFixture.vertices(name).map(_.toJSArray).toJSArray,
          faces = SurfaceVisibilityFixture.Faces.map(t => js.Array(t._1, t._2, t._3)).toJSArray,
          rgba = SurfaceVisibilityFixture.colors(name).map(c => js.Array(c.red / 255.0, c.green / 255.0, c.blue / 255.0, c.alpha / 255.0)).toJSArray,
          eye = SurfaceVisibilityFixture.eye(name).toJSArray,
          ambient = SurfaceVisibilityFixture.ambient(name), diffuse = SurfaceVisibilityFixture.diffuse(name),
          opacity = SurfaceVisibilityFixture.layerOpacity(name),
          curvature = SurfaceVisibilityFixture.curvature(name).map(_.asInstanceOf[js.Any]).getOrElse(null),
          base = (SurfaceVisibilityFixture.base(name).red << 16) | (SurfaceVisibilityFixture.base(name).green << 8) | SurfaceVisibilityFixture.base(name).blue,
          near = p(11).toDouble / (p(10).toDouble - 1), far = p(11).toDouble / (p(10).toDouble + 1)))
      val alphaCaptures = new js.Array[js.Any]()
      for opacity <- Vector(.5, 0.0); reverse <- Vector(false, true) do
        val scene = SurfaceVisibilityFixture.alphaComposition(opacity, reverse)
        val native = backend.render(scene, ThreeCanvasSize.unsafe(256, 256), forceDraw = true)
        val receipt = ok(host.submit(scene, RasterDimensions.unsafe(256, 256), SurfaceRasterStyle(culling = TriangleCulling.None)))
        val picked = ok(host.pickDisplay(128, 128)).map:
          case SurfaceDisplayHit.Scientific(value) => value.surface.value
          case SurfaceDisplayHit.Decoration(_) => "decoration"
        alphaCaptures.push(js.Dynamic.literal(name = s"surface-alpha-$opacity-$reverse", opacity = opacity, reverse = reverse,
          nativeRefused = native.isLeft, raster = rasterCanvas.applyDynamic("toDataURL")("image/png"),
          picked = picked.getOrElse("none"), renderNanos = receipt.renderNanos.toDouble))
      js.Dynamic.literal(captures = captures, alphaCaptures = alphaCaptures)
    finally
      host.dispose()
      backend.dispose()
