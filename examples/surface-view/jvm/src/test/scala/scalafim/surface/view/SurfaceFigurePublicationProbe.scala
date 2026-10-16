package scalafim.surface.view

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import intaglio.*
import intaglio.java2d.*
import scalafim.surface.*
import scalafim.surface.view.raster.*

/** Writes real linked-view figure specimens for visual review.
  *
  * Run with `surfaceViewExamplesJVM/Test/runMain
  * scalafim.surface.view.SurfaceFigurePublicationProbe [output-directory]`.
  */
object SurfaceFigurePublicationProbe:
  private def orThrow[A](value: Either[?, A]): A = value.fold(error => throw new IllegalStateException(s"$error"), identity)

  def main(args: Array[String]): Unit =
    val root = args.headOption.fold(Path.of(System.getProperty("java.io.tmpdir"), "scalafim-surface-figure-probe"))(Path.of(_))
    Files.createDirectories(root)
    val model = SurfaceLinkedFixture.model(SurfaceLinkedFixture.playback(.5))
    val state = SurfaceLinkedFixture.state(model, selected = 2, morph = .5)
    val navigation = SurfaceLinkedFixture.navigation(model, state)
    val labels = navigation.views.map(_.id).zip(Vector("Face-on", "Oblique left", "Oblique right", "Lower oblique")).toMap
    Vector(SurfaceFigureOutputScale.One, SurfaceFigureOutputScale.Two).foreach: scale =>
      val spec = orThrow(SurfaceFigureSpec.make(Some("Linked temporal surface"), scale, labels))
      write(root, s"linked-surface-${scale.factor}x", model, state, navigation, spec, SurfaceLinkedFixture.Legend)
    val ids = Vector.tabulate(SurfaceLinkedFixture.Geometry.vertexCount)(VertexId(_))
    val bivariate = orThrow(SurfaceLayer.bivariateEffectConfidence(SurfaceLinkedFixture.Layer, SurfaceLinkedFixture.Surface,
      SurfaceMatrix.fromRows(SurfaceLinkedFixture.Geometry, ids, SurfaceVisibilityFixture.Effects.map(Vector(_))),
      SurfaceMatrix.fromRows(SurfaceLinkedFixture.Geometry, ids, SurfaceVisibilityFixture.Confidence.map(Vector(_))),
      SurfaceVisibilityFixture.BivariateMapping))
    val bivariateModel = orThrow(SurfaceViewerModel.make(Vector(SurfaceLinkedFixture.Asset), Vector(bivariate)))
    val bivariateState = SurfaceLinkedFixture.state(bivariateModel, selected = 2, morph = .5)
    val bivariateNavigation = SurfaceLinkedFixture.navigation(bivariateModel, bivariateState)
    val bivariateLabels = bivariateNavigation.views.map(_.id).zip(Vector("Face-on", "Oblique left", "Oblique right", "Lower oblique")).toMap
    val bivariateSpec = orThrow(SurfaceFigureSpec.make(Some("Effect and confidence"), SurfaceFigureOutputScale.One, bivariateLabels))
    val bivariateLegend = SurfaceLegendRequest.Bivariate(SurfaceLegendLayers.one(SurfaceLinkedFixture.Layer),
      orThrow(LegendTitle.make("Effect")), orThrow(LegendTitle.make("Confidence")),
      Vector(AxisTick.unsafe(-1, "-1"), AxisTick.unsafe(0, "0"), AxisTick.unsafe(1, "1")),
      Vector(AxisTick.unsafe(0, "0"), AxisTick.unsafe(.5, "0.5"), AxisTick.unsafe(1, "1")))
    write(root, "linked-bivariate-1x", bivariateModel, bivariateState, bivariateNavigation, bivariateSpec, bivariateLegend)

  private def write[F <: image4s.geometry.Frame[image4s.geometry.D3]](root: Path, stem: String,
      model: SurfaceViewerModel, state: SurfaceViewerState, navigation: SurfaceCompositionState[F],
      spec: SurfaceFigureSpec, legend: SurfaceLegendRequest): Unit =
    val figure = orThrow(SurfaceFigurePublication.prepare(model, state, navigation, spec, Vector(legend)))
    val artifact = orThrow(figure.renderWith[String]: input =>
      val packets = orThrow(SurfaceDecorationCompiler.compile(SurfaceLinkedFixture.marker(state, input.pixelScale), input.plan))
      val geometry = orThrow(SurfaceDecorationGeometry.lower(packets, input.plan, input.dimensions.width, input.dimensions.height))
      SurfaceDecorationRaster.render(input.plan, geometry, input.dimensions,
        SurfaceRasterStyle(background = input.background)).left.map(_.message).map(_.image)
    )
    val receipt = artifact.receipt
    require(receipt.dimensions == figure.input.dimensions, "figure renderer returned an unexpected raster size")
    val png = orThrow(Java2DRenderer.renderPng(artifact.scene,
      orThrow(Java2DOptions(receipt.dimensions.width, receipt.dimensions.height, receipt.pixelsPerInch.toDouble))))
    Files.write(root.resolve(s"$stem.png"), png)
    Files.writeString(root.resolve(s"$stem.json"), receiptJson(receipt), StandardCharsets.UTF_8)
    println(s"wrote ${root.resolve(s"$stem.png")} ${receipt.dimensions.width}x${receipt.dimensions.height} ${receipt.pixelsPerInch}ppi")

  private def receiptJson(receipt: SurfaceFigurePublicationReceipt): String =
    val frame = receipt.surfaceFrame
    val legends = receipt.legends.map(value => s"\"${escape(value.canonicalKey)}\"").mkString(",")
    s"""{"logicalWidth":${receipt.logicalWidth},"logicalHeight":${receipt.logicalHeight},"width":${receipt.dimensions.width},"height":${receipt.dimensions.height},"pixelsPerInch":${receipt.pixelsPerInch},"surfaceFrame":{"leftPt":${frame.leftPt},"topPt":${frame.topPt},"widthPt":${frame.widthPt},"heightPt":${frame.heightPt}},"legendKeys":[$legends]}"""

  private def escape(value: String): String =
    value.replace("\\", "\\\\").replace("\"", "\\\"")
