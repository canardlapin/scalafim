package scalafim.surface.view.javafx

import java.util.concurrent.CountDownLatch
import javafx.animation.AnimationTimer
import javafx.application.{ConditionalFeature, Platform}
import javafx.scene.{Group, PerspectiveCamera, Scene, SceneAntialiasing, SubScene}
import javafx.scene.paint.Color
import javafx.scene.shape.CullFace
import javafx.stage.Stage

object JavaFxSurfacePulseBenchmark:
  private final case class Scenario(grid: JavaFxSurfaceBenchmark.Grid, layers: Int)

  def main(args: Array[String]): Unit =
    val samples = args.headOption.flatMap(_.toIntOption).getOrElse(60).max(20)
    val done = new CountDownLatch(1)
    val scenarios =
      Vector(JavaFxSurfaceBenchmark.Grid(32000, 256, 125), JavaFxSurfaceBenchmark.Grid(164000, 400, 410))
        .flatMap(grid => Vector(1, 4, 8).map(Scenario(grid, _)))
    Platform.startup(() => start(scenarios, samples, done))
    done.await()

  private def start(scenarios: Vector[Scenario], samples: Int, done: CountDownLatch): Unit =
    println(s"javafx_scene3d_supported=${Platform.isSupported(ConditionalFeature.SCENE3D)}")
    println("vertices,layers,frame_interval_p50_ms,frame_interval_p95_ms,samples")
    runScenario(scenarios, 0, samples, done)

  private def runScenario(
    scenarios: Vector[Scenario],
    index: Int,
    samples: Int,
    done: CountDownLatch
  ): Unit =
    if index >= scenarios.length then
      done.countDown()
      Platform.exit()
    else
      val scenario = scenarios(index)
      val plan = JavaFxSurfaceBenchmark.syntheticPlan(scenario.grid, scenario.layers, phase = 0)
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
      val stage = new Stage()
      stage.setScene(new Scene(new Group(subScene), 640.0, 640.0))
      stage.setTitle(s"ScalaFIM JavaFX pulse ${scenario.grid.vertices}/${scenario.layers}")
      stage.show()

      val warmupFrames = 20
      val timings = new Array[Double](samples)
      val timer = new AnimationTimer:
        private var previous = 0L
        private var frame = 0
        private var measured = 0

        def handle(now: Long): Unit =
          if previous != 0L && frame >= warmupFrames then
            timings(measured) = (now - previous).toDouble / 1e6
            measured += 1
          result.root.setRotate(frame.toDouble * 0.35)
          previous = now
          frame += 1
          if measured == samples then
            stop()
            scala.util.Sorting.quickSort(timings)
            val p50 = JavaFxSurfaceBenchmark.percentile(timings, 0.50)
            val p95 = JavaFxSurfaceBenchmark.percentile(timings, 0.95)
            println(f"${scenario.grid.vertices},${scenario.layers},$p50%.3f,$p95%.3f,$samples")
            stage.close()
            Platform.runLater(() => runScenario(scenarios, index + 1, samples, done))
      timer.start()
