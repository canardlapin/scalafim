package scalafim.surface.view.javafx

import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.CountDownLatch
import javafx.application.Platform
import javafx.scene.{Group, Scene, SubScene}
import javafx.scene.image.WritableImage
import javafx.stage.Stage

import scalafim.examples.surfaceview.*
import scalafim.graphics.*
import scalafim.surface.*
import scalafim.surface.io.GiftiSurfaceReader
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** Live JavaFX differential gate for the complete real cortical geometry
  * family. This is intentionally opt-in because it requires Scene3D and a
  * display.
  */
object JavaFxCorticalMorphProbe:
  def main(args: Array[String]): Unit =
    val corpusRoot = args.headOption.map(Path.of(_)).getOrElse(Path.of(
      System.getProperty("user.home"),
      "code",
      "jscode",
      "FROIAtlas",
      "app",
      "public",
      "data",
      "surfaces"
    ))
    val loaded = CorticalSurfaceMorphAcceptance.Corpus.map: entry =>
      (entry.hemisphere, entry.kind) -> read(corpusRoot, entry)
    .toMap
    val example = CorticalSurfaceMorphAcceptance.build(
      loaded(Hemisphere.Left -> SurfaceKind.White),
      loaded(Hemisphere.Left -> SurfaceKind.Pial),
      loaded(Hemisphere.Left -> SurfaceKind.Inflated),
      loaded(Hemisphere.Right -> SurfaceKind.White),
      loaded(Hemisphere.Right -> SurfaceKind.Pial),
      loaded(Hemisphere.Right -> SurfaceKind.Inflated)
    ).fold(error => throw new IllegalStateException(error.message), identity)
    val cases = CorticalSurfaceMorphAcceptance.cases(example)
      .fold(error => throw new IllegalStateException(error.message), identity)

    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup: () =>
      var stage: Stage | Null = null
      var subScene: SubScene | Null = null
      var backend: JavaFxSurfaceBackend | Null = null
      try
        backend = JavaFxSurfaceBackend.create().toOption.get
        val config = JavaFxSnapshotConfig.make(
          CorticalSurfaceAcceptance.Dimensions.width,
          CorticalSurfaceAcceptance.Dimensions.height
        ).toOption.get
        var previousGeometryKeys = Vector.empty[SurfaceResourceKey]
        val rows = Vector.newBuilder[String]
        var index = 0
        while index < cases.length do
          val current = cases(index)
          val reference = SurfaceRasterizer.render(
            current.plan,
            CorticalSurfaceAcceptance.Dimensions,
            SurfaceRasterStyle(culling = TriangleCulling.None)
          ).fold(error => throw new IllegalStateException(error.message), identity)
          val render = backend.nn.render(current.plan).toOption.get
          if stage == null then
            subScene = backend.nn.newSubScene(config).toOption.get
            stage = new Stage()
            stage.nn.setScene(new Scene(new Group(subScene.nn), config.width.toDouble, config.height.toDouble))
            stage.nn.show()
          val snapshot = new WritableImage(config.width, config.height)
          subScene.nn.snapshot(null, snapshot)
          val observed = rasterImage(snapshot, CorticalSurfaceAcceptance.Dimensions)
          val visual = SurfaceVisualQa.compare(reference.image, observed).toOption.get
          val violations = visual.violations(CorticalSurfaceAcceptance.Policy)
          require(violations.isEmpty, s"${current.label}: ${violations.mkString("; ")}")
          require(render.atlasUpdates == 0, s"${current.label}: unexpectedly updated ${render.atlasUpdates} atlases")

          val geometryKeys = current.plan.meshes.map(_.geometryKey)
          if index == 0 then
            require(render.geometryUpdates == 0, s"${current.label}: cold load performed an in-place update")
          else if geometryKeys == previousGeometryKeys then
            require(!render.dirty.geometry && render.geometryUpdates == 0,
              s"${current.label}: an exact endpoint should be a no-op")
          else
            require(render.geometryUpdates == 1, s"${current.label}: expected one batched geometry update")
            require(render.geometryBytesUpdated == current.plan.profile.verticesPacked.toLong * 6L * 4L,
              s"${current.label}: geometry byte accounting drifted")
          previousGeometryKeys = geometryKeys

          rows += row(current, render, visual)
          index += 1
        println(s"{" +
          s"\"schema\":\"scalafim.surface-cortical-morph-acceptance.v1\"," +
          s"\"backend\":\"javafx-scene3d\"," +
          s"\"cases\":[${rows.result().mkString(",")}]}")
      catch case error: Throwable => failure = error
      finally
        if backend != null then backend.nn.dispose()
        if stage != null then stage.nn.close()
        done.countDown()
    done.await()
    Platform.exit()
    if failure != null then throw failure.nn

  private def read(root: Path, entry: CorticalMorphCorpusEntry): SurfaceGeometry =
    val path = root.resolve(entry.fileName)
    require(Files.isRegularFile(path), s"missing cortical morph corpus file: $path")
    val actual = sha256(Files.readAllBytes(path))
    require(actual == entry.sha256,
      s"SHA-256 mismatch for $path: expected ${entry.sha256}, got $actual")
    GiftiSurfaceReader.read(path, entry.hemisphere, entry.kind)

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(byte => f"${byte & 0xff}%02x").mkString

  private def rasterImage(actual: WritableImage, dimensions: RasterDimensions): RasterImage =
    val reader = actual.getPixelReader
    RasterImage.tabulate(dimensions): (x, y) =>
      val argb = reader.getArgb(x, y)
      Rgba32.unsafe(
        (argb >>> 16) & 0xff,
        (argb >>> 8) & 0xff,
        argb & 0xff,
        (argb >>> 24) & 0xff
      )

  private def row(
    current: CorticalMorphCase,
    render: JavaFxInterpretReceipt,
    visual: SurfaceVisualQaReceipt
  ): String =
    s"{" +
      s"\"label\":\"${current.label}\"," +
      s"\"geometryUpdates\":${render.geometryUpdates}," +
      s"\"geometryBytesUpdated\":${render.geometryBytesUpdated}," +
      f"\"elapsedMillis\":${render.elapsedNanos.toDouble / 1e6}%.6f," +
      f"\"maskIntersectionOverUnion\":${visual.maskIntersectionOverUnion}%.9f," +
      f"\"centroidDistancePixels\":${visual.centroidDistancePixels}%.9f," +
      f"\"meanInteriorChannelError\":${visual.meanInteriorChannelError}%.9f}"
