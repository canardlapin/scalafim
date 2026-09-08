package scalafim.examples.surfaceview

import intaglio.*
import intaglio.svg.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*
import java.nio.file.{Files, Paths}

/** Publication layout acceptance on an analytic two-triangle surface. Real
  * cortical geometry and native 3D rendering have separate acceptance gates.
  */
object SurfaceLegendPublicationGallery:
  def main(args: Array[String]): Unit =
    val output = Paths.get(args.headOption.getOrElse("target/surface-publication-legends"))
    Files.createDirectories(output)
    val blue = Rgba32.unsafe(30, 70, 170)
    val white = Rgba32.unsafe(245, 245, 245)
    val red = Rgba32.unsafe(180, 35, 35)
    val window = DisplayWindow.unsafe(-4, 8)
    val lower = ScalarRamp.linear(blue, white)
    val upper = ScalarRamp.linear(white, red)
    val sequential = ScalarMapping(ScalarScale.sequential(window, ScalarRamp.linear(blue, red)))
    val asymmetric = ScalarMapping(ScalarScale.diverging(window, 0, lower, upper).toOption.get)
    val split = ScalarMapping(ScalarScale.split(window, 0, -1, 2, lower, upper).toOption.get)
    val missing = asymmetric.copy(visibility = ScalarVisibility.Outside(ScalarInterval.make(-0.25, 0.25,
      ScalarEndpointInclusion.Neither).toOption.get), invalid = Rgba32.unsafe(190, 60, 175))
    val id = SurfaceLayerId.unsafe("analytic-map")
    val surface = SurfaceFaceFixture.Surface
    val geometry = SurfaceFaceFixture.geometry
    val title = LegendTitle.make("Estimated response relative to the comparison condition with a long quantity name",
      Some("percent signal change")).toOption.get
    val cases = Vector.newBuilder[String]
    for
      (name, mapping) <- Vector("continuous" -> sequential, "asymmetric" -> asymmetric, "split" -> split,
        "missing" -> missing, "categorical" -> sequential, "manual" -> sequential)
      (format, preset) <- Vector("manuscript" -> SurfacePublicationPreset.ManuscriptSingleColumn, "poster" -> SurfacePublicationPreset.Poster)
    do
      val category = name == "categorical"
      val layer = if category then
        SurfaceLayer.faceLabels(id, surface, SurfaceFaceField.make(geometry, Array(1, 2)).toOption.get,
          LabelColorizer(Map(1 -> red, 2 -> blue)))
      else
        val values = if name == "missing" then Array(-4.0, Double.NaN, 8.0, -4.0) else Array(-4.0, 8.0, 8.0, -4.0)
        SurfaceLayer.interpolatedScalar(id, surface, geometry, values, mapping).toOption.get
      val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), Vector(layer)).toOption.get
      val state = SurfaceViewer.reduce(model, SurfaceFaceFixture.state(model), SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit)).toOption.get
      val request = if category then
        SurfaceLegendRequest.Categorical(SurfaceLegendLayers.one(id), title,
          Map(1 -> "Motor and premotor association cortex with an extended anatomical description", 2 -> "Visual association cortex"))
      else if name == "manual" then SurfaceLegendRequest.Manual(title,
        Vector(SurfaceLegendItem.unsafe("Selected analysis region with an extended explanatory label", red), SurfaceLegendItem.unsafe("Reference region", blue)))
      else if name == "split" then SurfaceLegendRequest.Split(SurfaceLegendLayers.one(id), title)
      else SurfaceLegendRequest.Continuous(SurfaceLegendLayers.one(id), title)
      val spec = SurfacePublicationSpec(preset, Some(s"Analytic surface: $name mapping"), SurfaceOrientationMark.Anterior)
      val publication = SurfaceLegendPublication.prepare(model, state, spec, Vector(request)).fold(error => throw new IllegalStateException(error.message), identity)
      val artifact = publication.renderWith: input =>
        SurfaceRasterizer.render(input.plan, input.dimensions, SurfaceRasterStyle(background = input.background, culling = TriangleCulling.None)).map(_.image)
      .fold(error => throw new IllegalStateException(error.toString), identity)
      val svg = SvgRenderer.render(artifact.scene, SvgOptions.unsafe(preset.width, preset.height, Some(s"$name-$format"))).toOption.get
      val file = s"$name-$format.svg"
      Files.writeString(output.resolve(file), svg.value)
      val frame = artifact.receipt.surfaceFrame
      val legend = artifact.receipt.legends.head.frame
      def rect(value: SurfacePublicationFrame): String =
        Vector(value.leftPt, value.topPt, value.widthPt, value.heightPt).mkString("[", ",", "]")
      cases += s"{\"file\":\"$file\",\"width\":${preset.width},\"height\":${preset.height},\"surfaceFramePt\":${rect(frame)},\"legendFramePt\":${rect(legend)}}"
      println(s"publication=$file surface=${publication.input.dimensions.width}x${publication.input.dimensions.height} legend=${legend.widthPt}x${legend.heightPt}pt")
    Files.writeString(output.resolve("gallery.json"), cases.result().mkString("[", ",", "]"))
