package scalafim.surface.view.raster

import intaglio.RasterDimensions
import scalafim.surface.view.*

/** Deterministic CPU baseline for the pinned admission matrix. Each output
  * line is an independently parseable v1 JSON receipt.
  */
object SurfaceRasterAdmissionBenchmark:
  def main(args: Array[String]): Unit =
    val repetitions = args.headOption.flatMap(_.toIntOption).getOrElse(3).max(1)
    val dimensions = RasterDimensions.unsafe(128, 128)
    val metadata = SurfaceRuntimeMetadata(
      SurfaceRuntimePlatform.Jvm,
      System.getProperty("java.runtime.name", "unknown"),
      System.getProperty("java.runtime.version", "unknown"),
      System.getProperty("os.name", "unknown"),
      System.getProperty("os.arch", "unknown"),
      "scalar reference raster",
      "CPU"
    )
    SurfaceBenchmarkMatrix.Cases.foreach: benchmark =>
      val observations = Vector.newBuilder[SurfaceBackendObservation]
      val samples = Vector.newBuilder[Long]
      var repetition = 0
      while repetition < repetitions do
        val plan = SurfaceBenchmarkFixture.planFor(benchmark)
        val observed = SurfaceRasterizer.renderObserved(plan, dimensions, path = benchmark.path)
          .fold(error => throw new IllegalStateException(error.message), identity)
        val enriched =
          if benchmark.path == SurfaceAdmissionPath.Pick then
            val started = System.nanoTime()
            val pick = observed.result.pick(dimensions.width / 2, dimensions.height / 2)
              .fold(error => throw new IllegalStateException(error.message), identity)
            val event = pick.map(value => SurfaceResourceEvent.Picked(value.surface, value.face, value.vertex)).toVector
            observed.observation.copy(
              events = observed.observation.events ++ event,
              timings = observed.observation.timings :+ SurfacePhaseTiming.unsafe(
                SurfaceRenderPhase.Pick,
                System.nanoTime() - started
              )
            )
          else observed.observation
        val violations = SurfaceBackendAdmission.validate(enriched)
        require(violations.isEmpty, violations.map(_.problem).mkString("; "))
        observations += enriched
        samples += enriched.timings.iterator.map(_.elapsedNanos).sum
        repetition += 1
      val summary = SurfaceTimingSummary.from(SurfaceRenderPhase.Snapshot, samples.result())
        .fold(error => throw new IllegalStateException(error), identity)
      println(SurfaceBenchmarkJson.encode(SurfaceBenchmarkReceipt(
        benchmark,
        metadata,
        observations.result(),
        Vector(summary)
      )))
