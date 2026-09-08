package scalafim.surface.view.javafx

import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import javafx.application.Platform
import javafx.scene.{Group, ParallelCamera, Scene, SceneAntialiasing, SubScene}
import javafx.scene.image.WritableImage
import javafx.scene.input.PickResult
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{CullFace, MeshView, TriangleMesh}
import javafx.stage.Stage

/** Two plain native triangles isolate the cortical occlusion-edge discrepancy.
  * No surface compiler, approximation, atlas, or reference rasterizer is used.
  */
object JavaFxSubpixelCoverageProbe:
  private val far = Vector((235.1332718239765, 309.8404360206807),
    (233.8105492808274, 308.99204440924774), (233.96824569006287, 310.0272434905887))
  private val near = Vector((234.7253242989343, 309.4272616646574),
    (233.68347095764375, 309.766623615658), (233.96568799205988, 309.73329925163625))

  def main(args: Array[String]): Unit =
    val output = Path.of(args.headOption.getOrElse("/private/tmp/scalafim-javafx-subpixel.json"))
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup(() => ())
    Platform.runLater: () =>
      val stage = new Stage()
      try
        val mesh = new TriangleMesh()
        mesh.getTexCoords.setAll(0.25f, 0.5f, 0.75f, 0.5f)
        mesh.getFaces.setAll(0, 0, 1, 0, 2, 0, 3, 1, 4, 1, 5, 1)
        val texture = new WritableImage(2, 1)
        texture.getPixelWriter.setColor(0, 0, Color.rgb(120, 120, 120))
        texture.getPixelWriter.setColor(1, 0, Color.rgb(85, 85, 85))
        val material = new PhongMaterial(Color.BLACK)
        material.setSelfIlluminationMap(texture)
        val view = new MeshView(mesh)
        view.setCullFace(CullFace.NONE)
        view.setMaterial(material)
        val scene = new SubScene(new Group(view), 1024, 1024, true, SceneAntialiasing.DISABLED)
        scene.setCamera(new ParallelCamera())
        scene.setFill(Color.WHITE)
        stage.setScene(new Scene(new Group(scene), 1024, 1024))
        stage.show()
        val method = classOf[SubScene].getDeclaredMethod("pickRootSG", java.lang.Double.TYPE, java.lang.Double.TYPE)
        method.setAccessible(true)
        val rows = Vector(-0.01, 0.0, 0.01).map: shift =>
          val nearPoints = near.map((x, y) => (x.toFloat.toDouble, (y + shift).toFloat.toDouble))
          val points = far.flatMap((x, y) => Vector(x.toFloat, y.toFloat, 300.0f)) ++
            nearPoints.flatMap((x, y) => Vector(x.toFloat, y.toFloat, 200.0f))
          mesh.getPoints.setAll(points.toArray*)
          val exactInside = inside(nearPoints)
          require(exactInside == (shift < 0), "the analytic fixture must straddle the occlusion edge")
          val quantizedInside = inside(nearPoints.map((x, y) => (math.rint(x * 256) / 256, math.rint(y * 256) / 256)))
          val image = scene.snapshot(null, new WritableImage(1024, 1024))
          val gray = image.getPixelReader.getArgb(234, 309) & 255
          val picked = method.invoke(scene, Double.box(234.5), Double.box(309.5)).asInstanceOf[PickResult]
          require(picked.getIntersectedFace == (if exactInside then 1 else 0), "native CPU ray disagrees with analytic coverage")
          require(gray == (if quantizedInside then 85 else 120), "native pixel differs from the observed 1/256-pixel coverage model")
          val coverage = NativeSubpixelCoverage.certify(
            nearPoints.map((x, y) => NativeSubpixelCoverage.Point(x, y, 0.2)), 234.5, 309.5, 0.7,
            picked.getIntersectedFace, 1, intaglio.Rgba32.unsafe(85, 85, 85), intaglio.Rgba32.unsafe(gray, gray, gray))
          require(coverage.nonEmpty == (shift == 0), "only the native subpixel visibility difference should be classified")
          val row = s"{\"classifiedCoverage\":${coverage.nonEmpty},\"shiftY\":$shift,\"exactNearCoverage\":$exactInside,\"quantizedNearCoverage\":$quantizedInside,\"nativeGray\":$gray,\"nativeRayFace\":${picked.getIntersectedFace}}"
          println(s"subpixel_coverage=$row")
          row
        Files.writeString(output, rows.mkString("[\n", ",\n", "\n]\n"))
      catch case error: Throwable => failure = error
      finally
        stage.close()
        done.countDown()
    try
      require(done.await(60, TimeUnit.SECONDS), "subpixel probe timed out")
      if failure != null then throw failure.nn
    finally Platform.exit()

  private def inside(points: Vector[(Double, Double)]): Boolean =
    val edges = points.indices.map: i =>
      val (ax, ay) = points(i)
      val (bx, by) = points((i + 1) % 3)
      (bx - ax) * (309.5 - ay) - (by - ay) * (234.5 - ax)
    edges.forall(_ >= 0) || edges.forall(_ <= 0)
