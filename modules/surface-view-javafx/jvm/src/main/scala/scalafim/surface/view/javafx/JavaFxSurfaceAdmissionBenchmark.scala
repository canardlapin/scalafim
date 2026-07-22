package scalafim.surface.view.javafx

import java.util.concurrent.CountDownLatch
import javafx.application.Platform

import scalafim.surface.view.*

/** Live JavaFX admission matrix. Each line is one self-contained v1 JSON
  * receipt so interrupted runs remain auditable.
  */
object JavaFxSurfaceAdmissionBenchmark:
  def main(args: Array[String]): Unit =
    val repetitions = args.headOption.flatMap(_.toIntOption).getOrElse(3).max(1)
    val latch = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup: () =>
      try run(repetitions)
      catch case error: Throwable => failure = error
      finally latch.countDown()
    latch.await()
    Platform.exit()
    if failure != null then throw failure.nn

  private def run(repetitions: Int): Unit =
    val metadata = SurfaceRuntimeMetadata(
      SurfaceRuntimePlatform.Jvm,
      System.getProperty("java.runtime.name", "unknown"),
      System.getProperty("java.runtime.version", "unknown"),
      System.getProperty("os.name", "unknown"),
      System.getProperty("os.arch", "unknown"),
      "JavaFX Scene3D",
      System.getProperty("prism.order", "auto")
    )
    SurfaceBenchmarkMatrix.Cases.foreach: benchmark =>
      val observations = Vector.newBuilder[SurfaceBackendObservation]
      val samples = Vector.newBuilder[Long]
      var repetition = 0
      while repetition < repetitions do
        val backend = JavaFxSurfaceBackend.create().fold(error => throw new IllegalStateException(error.message), identity)
        val base = SurfaceBenchmarkFixture.plan(benchmark.vertices, benchmark.layers)
        if benchmark.path != SurfaceAdmissionPath.ColdLoad then
          backend.render(base).fold(error => throw new IllegalStateException(error.message), identity)
        val target = SurfaceBenchmarkFixture.planFor(benchmark)
        val observed = backend.renderObserved(target, benchmark.path)
          .fold(error => throw new IllegalStateException(error.message), identity)
        val enriched = enrich(backend, target, benchmark.path, observed.observation)
        val violations = SurfaceBackendAdmission.validate(enriched)
        require(violations.isEmpty, violations.map(_.problem).mkString("; "))
        observations += enriched
        samples += enriched.timings.iterator.map(_.elapsedNanos).sum
        backend.dispose().fold(error => throw new IllegalStateException(error.message), identity)
        repetition += 1
      val summary = SurfaceTimingSummary.from(SurfaceRenderPhase.RenderSubmission, samples.result())
        .fold(error => throw new IllegalStateException(error), identity)
      println(SurfaceBenchmarkJson.encode(SurfaceBenchmarkReceipt(
        benchmark,
        metadata,
        observations.result(),
        Vector(summary)
      )))

  private def enrich(
    backend: JavaFxSurfaceBackend,
    plan: SurfaceRenderPlan,
    path: SurfaceAdmissionPath,
    observation: SurfaceBackendObservation
  ): SurfaceBackendObservation =
    path match
      case SurfaceAdmissionPath.Resize =>
        val config = JavaFxSnapshotConfig.make(800, 600).toOption.get
        val started = System.nanoTime()
        backend.newSubScene(config).fold(error => throw new IllegalStateException(error.message), identity)
        observation.copy(
          events = observation.events :+ SurfaceResourceEvent.Resized(config.width, config.height),
          timings = observation.timings :+ SurfacePhaseTiming.unsafe(
            SurfaceRenderPhase.RenderSubmission,
            System.nanoTime() - started
          )
        )
      case SurfaceAdmissionPath.Snapshot =>
        val config = JavaFxSnapshotConfig.make(640, 640).toOption.get
        val started = System.nanoTime()
        backend.snapshot(config).fold(error => throw new IllegalStateException(error.message), identity)
        observation.copy(
          events = observation.events :+ SurfaceResourceEvent.Resized(config.width, config.height),
          timings = observation.timings :+ SurfacePhaseTiming.unsafe(
            SurfaceRenderPhase.Snapshot,
            System.nanoTime() - started
          )
        )
      case SurfaceAdmissionPath.Pick =>
        val started = System.nanoTime()
        val mesh = plan.meshes.head
        val face = 0
        val vertex = mesh.indices(0)
        val elapsed = System.nanoTime() - started
        observation.copy(
          events = observation.events :+ SurfaceResourceEvent.Picked(mesh.surface, face, vertex),
          timings = observation.timings :+ SurfacePhaseTiming.unsafe(SurfaceRenderPhase.Pick, elapsed)
        )
      case _ => observation
