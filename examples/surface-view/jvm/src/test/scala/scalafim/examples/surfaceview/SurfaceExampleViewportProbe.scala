package scalafim.examples.surfaceview

import intaglio.*
import intaglio.svg.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*
import java.nio.file.{Files, Paths}

/** Isolate viewport fitting from every mesh, scalar, color and camera input. */
object SurfaceExampleViewportProbe:
  def main(args: Array[String]): Unit =
    val output = Paths.get(args.headOption.getOrElse("target/surface-example-viewport"))
    Files.createDirectories(output)
    val example = SurfaceViewerExample.portable
    for (name, plan) <- Vector("contained" -> example.plan, "legacy-fill" -> example.plan.copy(viewportFit = SurfaceViewportFit.Fill)) do
      val current = example.copy(plan = plan)
      val receipt = SurfaceViewerExample.semanticReceipt(current)
      val image = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(320, 180), SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get.image
      val scene = Scene(Vector(Grob.imageUnsafe(image, Point.npcUnsafe(0, 0), Size.npcUnsafe(1, 1),
        anchor = Anchor.BottomLeft, interpolation = RasterInterpolation.Nearest)))
      val svg = SvgRenderer.render(scene, SvgOptions.unsafe(320, 180, Some(name))).toOption.get
      Files.writeString(output.resolve(s"$name.svg"), svg.value)
      Files.writeString(output.resolve(s"$name.json"), receipt.canonicalJson + "\n")
      println(s"$name ${receipt.canonicalJson}")
