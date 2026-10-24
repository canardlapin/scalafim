package scalafim.fmri.design

import intaglio.svg.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/** Reproducible ScalaFIM-specific design-plot integration gallery.
  *
  * Renderer-neutral and backend conformance galleries live in Intaglio. This
  * runner proves only the domain boundary: `DesignGraphics` emits an Intaglio
  * scene that the separately developed SVG backend can render.
  */
object DesignGalleryRender:
  def main(args: Array[String]): Unit =
    val outDir = Paths.get(args.headOption.getOrElse("target/design-gallery"))
    Files.createDirectories(outDir)

    val task = Vector.tabulate(32): index =>
      val time = index.toDouble
      TracePoint(time, math.exp(-math.pow(time - 10.0, 2.0) / 18.0), "task", 0, "task")
    val drift = Vector.tabulate(32): index =>
      val time = index.toDouble
      TracePoint(time, -0.25 + time / 62.0, "drift", 0, "drift")
    val data = EventPlotData(
      points = task ++ drift,
      regressors = Vector("task", "drift"),
      boundaries = Vector.empty,
      useFacets = false,
      facetByBlock = false,
      suppressLabels = false,
      blockAxis = PlotBlockAxis.Global,
      rendererNote = "ScalaFIM design gallery fixture"
    )

    val scene = DesignGraphics.eventScene(data).fold(error => fail(error.message), identity)
    val svg = SvgRenderer
      .render(scene, SvgOptions.unsafe(width = 640, height = 480, title = Some("Design event plot")))
      .fold(error => fail(error.message), _.value)

    Files.writeString(outDir.resolve("design-event-plot.svg"), svg, StandardCharsets.UTF_8)
    Files.writeString(
      outDir.resolve("manifest.tsv"),
      "case\tfile\nDesignGraphics\tdesign-event-plot.svg\n",
      StandardCharsets.UTF_8
    )
    Files.writeString(
      outDir.resolve("index.html"),
      """<!doctype html>
        |<html lang="en">
        |  <head>
        |    <meta charset="utf-8">
        |    <meta name="viewport" content="width=device-width, initial-scale=1">
        |    <title>ScalaFIM design gallery</title>
        |  </head>
        |  <body>
        |    <h1>ScalaFIM design gallery</h1>
        |    <figure>
        |      <figcaption>DesignGraphics event plot rendered by Intaglio SVG</figcaption>
        |      <img src="design-event-plot.svg" alt="Design event plot">
        |    </figure>
        |  </body>
        |</html>
        |""".stripMargin,
      StandardCharsets.UTF_8
    )

    println(s"wrote ScalaFIM design gallery to $outDir")

  private def fail(message: String): Nothing =
    throw new IllegalStateException(message)
