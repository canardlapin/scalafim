package scalafim.surface.view.javafx

import java.util.concurrent.CountDownLatch
import javafx.application.{ConditionalFeature, Platform}
import javafx.scene.{Group, PerspectiveCamera, SceneAntialiasing, SubScene}
import javafx.scene.image.WritableImage
import javafx.scene.paint.Color
import javafx.scene.shape.CullFace

import intaglio.*
import scalafim.surface.view.*

object JavaFxSurfaceBenchmark:
  private[javafx] final case class Grid(vertices: Int, width: Int, height: Int)

  def main(args: Array[String]): Unit =
    val iterations = args.headOption.flatMap(_.toIntOption).getOrElse(8).max(3)
    val grids = Vector(Grid(32000, 256, 125), Grid(164000, 400, 410))
    val latch = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup: () =>
      try run(grids, Vector(1, 4, 8), iterations)
      catch case error: Throwable => failure = error
      finally latch.countDown()
    latch.await()
    Platform.exit()
    if failure != null then throw failure.nn

  private def run(grids: Vector[Grid], layerCounts: Vector[Int], iterations: Int): Unit =
    println(s"javafx_scene3d_supported=${Platform.isSupported(ConditionalFeature.SCENE3D)}")
    println("vertices,layers,faces,chunks,mesh_ms,atlas_ms,color_update_ms,morph_update_ms,morph_mib,snapshot_p50_ms,snapshot_p95_ms,atlas_mib")
    grids.foreach: grid =>
      layerCounts.foreach: layers =>
        val plan = syntheticPlan(grid, layers, phase = 0)
        val result = JavaFxSurfaceProbe.compile(plan).toOption.get
        result.chunks.foreach(_.view.setCullFace(CullFace.NONE))
        val camera = new PerspectiveCamera(true)
        camera.setNearClip(0.01)
        camera.setFarClip(100.0)
        camera.setFieldOfView(35.0)
        camera.setTranslateZ(-4.0)
        val subScene = new SubScene(result.root, 640.0, 640.0, true, SceneAntialiasing.BALANCED)
        subScene.setFill(Color.WHITE)
        subScene.setCamera(camera)
        val host = new Group(subScene)
        host.applyCss()
        val image = new WritableImage(640, 640)
        var warmup = 0
        while warmup < 8 do
          result.root.setRotate(warmup.toDouble * 0.5)
          subScene.snapshot(null, image)
          warmup += 1
        val morphed = syntheticPlan(grid, layers, phase = 0, morphPhase = 1)
        val morphUpdate = result.updateGeometry(morphed).toOption.get
        val updated = syntheticPlan(grid, layers, phase = 1, morphPhase = 1)
        val update = result.updateColors(updated, commit = true).toOption.get
        val timings = new Array[Double](iterations)
        var index = 0
        while index < iterations do
          result.root.setRotate(index.toDouble * 0.5)
          val started = System.nanoTime()
          subScene.snapshot(null, image)
          timings(index) = (System.nanoTime() - started).toDouble / 1e6
          index += 1
        scala.util.Sorting.quickSort(timings)
        val p50 = percentile(timings, 0.50)
        val p95 = percentile(timings, 0.95)
        val receipt = result.receipt
        val atlasMib = receipt.directAtlasBytes.toDouble / (1024.0 * 1024.0)
        val morphMib = morphUpdate.bytesUpdated.toDouble / (1024.0 * 1024.0)
        println(f"${grid.vertices},$layers,${receipt.facesUploaded},${receipt.chunks},${receipt.meshBuildNanos / 1e6}%.3f,${receipt.atlasBuildNanos / 1e6}%.3f,${update.updateNanos / 1e6}%.3f,${morphUpdate.updateNanos / 1e6}%.3f,$morphMib%.3f,$p50%.3f,$p95%.3f,$atlasMib%.3f")

  private[javafx] def percentile(sorted: Array[Double], probability: Double): Double =
    sorted(math.min(sorted.length - 1, math.ceil(probability * sorted.length).toInt - 1))

  private[javafx] def syntheticPlan(
    grid: Grid,
    layerCount: Int,
    phase: Int,
    morphPhase: Int = 0
  ): SurfaceRenderPlan =
    val surface = SurfaceId.unsafe("benchmark")
    val positions = new Array[Float](grid.vertices * 3)
    val normals = new Array[Float](grid.vertices * 3)
    var row = 0
    while row < grid.height do
      var column = 0
      while column < grid.width do
        val vertex = row * grid.width + column
        val offset = vertex * 3
        positions(offset) = (column.toDouble / (grid.width - 1) * 2.0 - 1.0).toFloat
        positions(offset + 1) = (row.toDouble / (grid.height - 1) * 2.0 - 1.0).toFloat
        positions(offset + 2) = (
          (0.08 + morphPhase * 0.04) * math.sin(column * 0.08) * math.cos(row * 0.08)
        ).toFloat
        normals(offset + 2) = -1.0f
        column += 1
      row += 1
    val faces = new Array[Int]((grid.width - 1) * (grid.height - 1) * 6)
    var faceOffset = 0
    row = 0
    while row + 1 < grid.height do
      var column = 0
      while column + 1 < grid.width do
        val a = row * grid.width + column
        val b = a + 1
        val c = a + grid.width
        val d = c + 1
        faces(faceOffset) = a; faces(faceOffset + 1) = c; faces(faceOffset + 2) = b
        faces(faceOffset + 3) = b; faces(faceOffset + 4) = c; faces(faceOffset + 5) = d
        faceOffset += 6
        column += 1
      row += 1
    val meshKey = SurfaceResourceKey("benchmark-mesh")
    val mesh = SurfaceMeshPacket(
      surface,
      meshKey,
      new FloatBufferView(positions),
      new FloatBufferView(normals),
      new IntBufferView(faces),
      Some(SurfaceResourceKey(s"benchmark-geometry-$morphPhase"))
    )
    val layers = Vector.tabulate(layerCount): layerIndex =>
      val colors = new Array[Int](grid.vertices)
      var vertex = 0
      while vertex < colors.length do
        val red = (vertex + phase * 17 + layerIndex * 31) & 0xff
        val green = (vertex / grid.width * 3 + layerIndex * 19) & 0xff
        val blue = (255 - red + layerIndex * 7) & 0xff
        colors(vertex) = (red << 24) | (green << 16) | (blue << 8) | (if layerIndex == 0 then 255 else 96)
        vertex += 1
      SurfaceLayerPacket(
        SurfaceLayerId.unsafe(s"layer-$layerIndex"),
        surface,
        SurfaceResourceKey(s"benchmark-layer-$layerIndex-$phase"),
        new IntBufferView(colors),
        DisplayOpacity.Opaque,
        DisplayBlendMode.Normal
      )
    val cameraMatrix = new FloatBufferView(Array(
      1f, 0f, 0f, 0f,
      0f, 1f, 0f, 0f,
      0f, 0f, 1f, 0f,
      0f, 0f, 0f, 1f
    ))
    val slot = SurfaceViewSlot(surface, SurfaceViewport(0.0, 0.0, 1.0, 1.0))
    val passes = layers.map(layer => SurfaceDrawPass(0, meshKey, layer.resourceKey, layer.blendMode))
    SurfaceRenderPlan(
      Vector(slot),
      Vector(mesh),
      layers,
      SurfaceCameraPacket(cameraMatrix, cameraMatrix, 0.0, 0.0, -1.0),
      SurfaceLighting.Unlit,
      SurfaceClipping.Disabled,
      passes,
      Scene.empty,
      Vector.empty,
      SurfaceProfile(1, grid.vertices, faces.length / 3, layerCount, grid.vertices * layerCount, 0L),
      SurfaceRenderReceipt(Vector(meshKey), layers.map(_.resourceKey), "benchmark", passes.length, 0)
    )
