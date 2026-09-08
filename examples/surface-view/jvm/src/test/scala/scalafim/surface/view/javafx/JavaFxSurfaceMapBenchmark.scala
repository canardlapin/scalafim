package scalafim.surface.view.javafx

import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import javafx.application.Platform
import javafx.scene.{Group, Scene, SceneAntialiasing}
import javafx.stage.Stage
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.examples.surfaceview.*

/** Repeated native admission, including pixels and real scene-traversal picks.
  * CLI: output repetitions vertex-counts modes [ortho|perspective] [unlit|lit].
  * Timings exclude model construction and reference verification. Snapshots
  * measure deferred native paint separately. No forced GC or warmup is hidden.
  */
object JavaFxSurfaceMapBenchmark:
  private val ByteBudget = 1024L * 1024 * 1024
  private val TriangleBudget = 3000000
  private val DepthBudget = 24
  private val CutsPerFaceBudget = 64

  def main(args: Array[String]): Unit =
    val output = Path.of(args.headOption.getOrElse("/private/tmp/scalafim-map-benchmark"))
    val repetitions = args.lift(1).fold(3)(_.toInt)
    val sizes = args.lift(2).fold(Vector(32768, 163842))(_.split(",").toVector.map(_.toInt))
    val modes = args.lift(3).fold(CorticalMapMode.values.toVector)(_.split(",").toVector.map(CorticalMapMode.valueOf))
    val projections = Vector(false, true).filter(p => args.lift(4).forall(_ == (if p then "perspective" else "ortho")))
    val lights = Vector(false, true).filter(l => args.lift(5).forall(_ == (if l then "lit" else "unlit")))
    require(repetitions > 0 && sizes.nonEmpty && modes.nonEmpty && projections.nonEmpty && lights.nonEmpty)
    Files.createDirectories(output)
    val metadata = s"\"fixture\":\"synthetic ellipsoid grid, millimetres; synthetic folding\",\"repetitions\":$repetitions,\"vertices\":${sizes.mkString("[", ",", "]")},\"modes\":${modes.map(m => json(m.toString)).mkString("[", ",", "]")},\"projections\":${projections.mkString("[", ",", "]")},\"lighting\":${lights.mkString("[", ",", "]")},\"channelErrorBudget\":4,\"depthBudget\":$DepthBudget,\"cutsPerFaceBudget\":$CutsPerFaceBudget,\"triangleBudget\":$TriangleBudget,\"byteBudget\":$ByteBudget,\"javaVersion\":${json(System.getProperty("java.version"))},\"os\":${json(System.getProperty("os.name") + " " + System.getProperty("os.version") + " " + System.getProperty("os.arch"))},\"maxHeapBytes\":${Runtime.getRuntime.maxMemory()},\"resourceExclusions\":\"objects, driver allocations, color and depth render targets\",\"timingPolicy\":\"no hidden warmup or forced GC; dispatch excludes verification; snapshot includes native paint; pick batch uses actual scene traversal\""
    Files.writeString(output.resolve("run.json"), s"{$metadata,\"status\":\"running\"}\n")
    val rows = scala.collection.mutable.ArrayBuffer.empty[String]
    def record(row: String): Unit =
      rows += row
      Files.writeString(output.resolve("samples.json"), rows.mkString("[\n", ",\n", "\n]\n"))
      println(s"map_benchmark=$row")
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup(() => ())
    Platform.runLater: () =>
      Platform.setImplicitExit(false)
      val stage = new Stage()
      try
        for vertices <- sizes do
          val fixture = SurfaceMapBenchmarkFixture(vertices)
          for mode <- modes do
            val example = fixture.example(mode)
            for perspective <- projections; lit <- lights do
              val lighting = if lit then SurfaceLighting.directional(0.4, 0.6, -1, -0.2, 0.3).toOption.get else SurfaceLighting.Unlit
              val projection = if perspective then CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50))
                else CameraProjection.Orthographic(OrthographicScale.unsafe(100))
              val state = Vector(SurfaceViewerAction.SetProjection(projection), SurfaceViewerAction.FitCamera,
                SurfaceViewerAction.SetLighting(lighting)).foldLeft(example.state): (s, a) =>
                  SurfaceViewer.reduce(example.model, s, a).toOption.get
              val plan = SurfaceCompiler.compile(example.model, state).toOption.get
              for repetition <- 0 until repetitions do
                val prefix = s"\"vertices\":$vertices,\"sourceFaces\":${fixture.surfaces.default.faceCount},\"mode\":\"$mode\",\"perspective\":$perspective,\"lit\":$lit,\"repetition\":$repetition,\"triangleBudget\":$TriangleBudget,\"byteBudget\":$ByteBudget"
                val backend = JavaFxSurfaceBackend.createApproximate(JavaFxApproximationConfig.make(
                  maxChannelError = 4, maxTriangles = TriangleBudget, maxGeneratedBytes = ByteBudget,
                  maxDepth = DepthBudget, maxCutsPerFace = CutsPerFaceBudget).toOption.get).toOption.get
                try
                  println(s"map_preparing=$vertices/$mode/$perspective/$lit/$repetition")
                  val start = System.nanoTime()
                  val rendered = backend.render(plan)
                  val coldNanos = System.nanoTime() - start
                  rendered match
                    case Left(error) =>
                      requireBudget(error)
                      require(backend.chunks.isEmpty, "rejected cold render retained partial resources")
                      record(s"{$prefix,\"action\":\"cold\",\"status\":\"rejected\",\"dispatchNanos\":$coldNanos,\"reason\":${json(error.message)}}")
                    case Right(receipt) =>
                      val scene = backend.newSubScene(JavaFxSnapshotConfig.make(768, 768, SceneAntialiasing.DISABLED).toOption.get).toOption.get
                      stage.setScene(new Scene(new Group(scene), 768, 768))
                      stage.show()
                      val controller = JavaFxSurfaceController.attach(example.model, state, backend, scene).toOption.get
                      def nativeBytes: Long = backend.chunks.map(c => (c.mesh.getPoints.size().toLong + c.mesh.getNormals.size() + c.mesh.getFaces.size() + c.mesh.getTexCoords.size()) * 4).sum
                      def atlasBytes: Long = backend.chunks.map(c => c.atlas.width.toLong * c.atlas.height * 4).sum
                      def accepted(action: String, nanos: Long, receipts: Vector[JavaFxInterpretReceipt], mesh: Long, atlas: Long, uv: Long): Unit =
                        val label = s"$mode-${if perspective then "perspective" else "ortho"}-${if lit then "lit" else "unlit"}-$vertices-$repetition-$action"
                        val metrics = JavaFxCorticalMapProbe.verify(label, controller, backend, scene, output, writeImages = repetition == 0)
                        val generated = receipts.last.approximation.fold("null")(_.generatedBytes.toString)
                        record(s"{$prefix,\"action\":\"$action\",\"status\":\"rendered\",\"dispatchNanos\":$nanos,\"uploadedMeshBytes\":$mesh,\"uploadedAtlasBytes\":$atlas,\"uploadedUvBytes\":$uv,\"nativeMeshBytes\":$nativeBytes,\"atlasBytes\":$atlasBytes,\"derivedFaces\":${backend.chunks.map(_.faceCount).sum},\"generatedBytes\":$generated,$metrics}")
                      var latestReceipt = receipt
                      try
                        accepted("cold", coldNanos, Vector(receipt), nativeBytes, atlasBytes, 0)
                        val surface = CorticalMapSemantics.Surface
                        val layer = CorticalMapSemantics.Overlay
                        val scalar = mode != CorticalMapMode.Nearest && mode != CorticalMapMode.Face
                        val actions = Vector(
                          "unchanged" -> Vector(SurfaceViewerAction.SetLighting(lighting)),
                          "camera" -> Vector(SurfaceViewerAction.OrbitBy(5, 2)),
                          "timepoint" -> Vector(SurfaceViewerAction.SetTimepoint(1)),
                          "opacity" -> Vector(SurfaceViewerAction.SetLayerOpacity(layer, DisplayOpacity.unsafe(0.6)))) ++
                          (if scalar then Vector("threshold" -> Vector(SurfaceViewerAction.SetLayerThreshold(layer,
                            DisplayThreshold.transparentBand(-0.75, 0.75).toOption.get))) else Vector.empty) ++
                          Vector("morph-half" -> Vector(SurfaceViewerAction.BeginGeometryMorph(surface, SurfaceKind.Inflated),
                            SurfaceViewerAction.SetGeometryMorphFraction(surface, SurfaceMorphFraction.unsafe(0.5)), SurfaceViewerAction.FitCamera),
                            "morph-end" -> Vector(SurfaceViewerAction.SetGeometryMorphFraction(surface, SurfaceMorphFraction.unsafe(1)), SurfaceViewerAction.FitCamera))
                        var sequenceAdmitted = true
                        for (action, changes) <- actions if sequenceAdmitted do
                          var meshBytes = 0L
                          var textureBytes = 0L
                          var uvBytes = 0L
                          val receipts = scala.collection.mutable.ArrayBuffer.empty[JavaFxInterpretReceipt]
                          var elapsed = 0L
                          for change <- changes if sequenceAdmitted do
                            val before = controller.plan
                            val previous = backend.chunks.map(_.mesh)
                            val previousAtlases = backend.chunks.map(_.atlas.image)
                            val started = System.nanoTime()
                            val result = controller.dispatch(change)
                            elapsed += System.nanoTime() - started
                            result match
                              case Left(JavaFxInteractionError.Backend(error)) =>
                                requireBudget(error)
                                require(controller.plan eq before, "rejection changed controller plan")
                                require(previous == backend.chunks.map(_.mesh) && previousAtlases == backend.chunks.map(_.atlas.image), "rejection replaced native resources")
                                val preserved = JavaFxCorticalMapProbe.verify(s"$mode-rejection-preserved", controller, backend, scene, output, writeImages = false)
                                record(s"{$prefix,\"action\":\"$action\",\"status\":\"rejected\",\"dispatchNanos\":$elapsed,\"reason\":${json(error.message)},\"remainingSequenceSkipped\":true,\"preservedState\":{$preserved}}")
                                sequenceAdmitted = false
                              case Left(error) => throw new AssertionError(error.message)
                              case Right(r) =>
                                receipts += r
                                latestReceipt = r
                                if previous != backend.chunks.map(_.mesh) then
                                  meshBytes += nativeBytes
                                  textureBytes += atlasBytes
                                else
                                  meshBytes += r.geometryBytesUpdated
                                  if r.atlasUpdates > 0 then textureBytes += atlasBytes
                                uvBytes += r.textureCoordinateBytesUpdated
                          if sequenceAdmitted then
                            if action == "camera" || action == "unchanged" then
                              require(meshBytes == 0 && textureBytes == 0 && uvBytes == 0, "camera/no-op uploaded data")
                            accepted(action, elapsed, receipts.toVector, meshBytes, textureBytes, uvBytes)
                        if sequenceAdmitted then
                          val started = System.nanoTime()
                          scene.setWidth(800)
                          scene.setHeight(600)
                          val resizeNanos = System.nanoTime() - started
                          accepted("resize", resizeNanos, Vector(latestReceipt), 0, 0, 0)
                      finally controller.dispose()
                finally
                  backend.dispose()
                  stage.setScene(null)
      catch case error: Throwable => failure = error
      finally
        stage.close()
        done.countDown()
    try
      require(done.await(2, TimeUnit.HOURS), "map benchmark timed out")
      if failure != null then
        Files.writeString(output.resolve("run.json"), s"{$metadata,\"status\":\"failed\",\"reason\":${json(failure.nn.toString)},\"recordedSamples\":${rows.size}}\n")
        throw failure.nn
      Files.writeString(output.resolve("run.json"), s"{$metadata,\"status\":\"completed\",\"recordedSamples\":${rows.size}}\n")
    finally Platform.exit()

  private def requireBudget(error: JavaFxSurfaceError): Unit = error match
    case JavaFxSurfaceError.IncompatiblePlan(reason) if reason == "approximation exceeds its generated byte or triangle budget" ||
        reason == "approximation exceeds its generated byte budget" ||
        reason.matches("(color approximation|mapping partition) exceeds [0-9]+ triangles") || reason.contains("exceeds subdivision depth") => ()
    case _ => throw new AssertionError(s"unexpected admission failure: ${error.message}")

  private def json(value: String): String =
    "\"" + value.flatMap:
      case '"' => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case c => c.toString
    + "\""
