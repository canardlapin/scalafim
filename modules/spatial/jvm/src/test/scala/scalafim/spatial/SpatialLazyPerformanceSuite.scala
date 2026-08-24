package scalafim.spatial

import com.sun.management.ThreadMXBean
import scalafim.image.io.Nifti
import scalafim.image.{SampleSpaces, Axis, DMat, PrimitiveBuffers, SomeSampleSpace, SomeScalarSeries}
import scalafim.image.SampleSpaces.*
import scalafim.spatial.io.{NiftiFieldSource, NiftiFieldSourceStats}

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path}

final case class SpatialLazyBenchmarkReceipt(
  chainIterations: Int,
  chainConstructionNanos: Long,
  chainConstructionAllocatedBytes: Long,
  firstEvaluationNanos: Long,
  firstEvaluationAllocatedBytes: Long,
  repeatedEvaluations: Int,
  repeatedEvaluationNanos: Long,
  repeatedEvaluationAllocatedBytes: Long,
  sourceBytesRead: Long,
  fullSourceBytes: Long,
  planCacheEntries: Int,
  resultCacheEntries: Int,
  runtimeStats: LazyRuntimeStats,
  checksum: Double
):
  def render: String =
    Vector(
      "spatial-lazy-benchmark-v1",
      s"chain.iterations=$chainIterations",
      s"chain.elapsed_nanos=$chainConstructionNanos",
      s"chain.allocated_bytes=$chainConstructionAllocatedBytes",
      s"first.elapsed_nanos=$firstEvaluationNanos",
      s"first.allocated_bytes=$firstEvaluationAllocatedBytes",
      s"repeat.iterations=$repeatedEvaluations",
      s"repeat.elapsed_nanos=$repeatedEvaluationNanos",
      s"repeat.allocated_bytes=$repeatedEvaluationAllocatedBytes",
      s"source.bytes_read=$sourceBytesRead",
      s"source.full_bytes=$fullSourceBytes",
      s"cache.plan_entries=$planCacheEntries",
      s"cache.result_entries=$resultCacheEntries",
      s"runtime.compilations=${runtimeStats.compilations}",
      s"runtime.executions=${runtimeStats.executions}",
      s"runtime.result_cache_hits=${runtimeStats.resultCacheHits}",
      s"checksum=$checksum"
    ).mkString("\n")

class SpatialLazyPerformanceSuite extends munit.FunSuite:

  private final case class Measurement[A](value: A, elapsedNanos: Long, allocatedBytes: Long)

  private val rows = 128
  private val observations = 20
  private val selectedRows = Vector(0, 16, 32, 48, 64, 80, 96, 112)
  private val selectedObservations = 5

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def apiValue[A](result: Either[FieldApiError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def volumeDomain(name: String, space: SomeSampleSpace): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-benchmark"))
    val modality = spatialValue(Modality(name))
    val geometry = spatialValue(SamplingGeometry.volume(space))
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def affine(name: String, source: Domain, target: Domain, x: Double): Morphism =
    val matrix =
      DMat.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0, x),
          Vector(0.0, 1.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    spatialValue(
      Morphism.between(
        spatialValue(MorphismId(name)),
        source,
        target,
        MorphismKind.Affine3D,
        RouteTag.Anatomical,
        inverse = Inverse.Exact("analytic"),
        coordinateMap = spatialValue(CoordinateMap.affine3D(matrix))
      )
    )

  private def withNiftiPath[A](body: Path => A): A =
    val directory = Files.createTempDirectory("scalafim-spatial-benchmark-")
    val path = directory.resolve("bold.nii")
    try body(path)
    finally
      Files.deleteIfExists(path)
      Files.deleteIfExists(directory)

  private def allocationBean(): ThreadMXBean =
    ManagementFactory.getThreadMXBean match
      case bean: ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
        if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
        bean
      case _ =>
        fail("JVM thread-allocation accounting is required for the spatial benchmark receipt")

  private def measure[A](bean: ThreadMXBean)(body: => A): Measurement[A] =
    val allocatedBefore = bean.getCurrentThreadAllocatedBytes
    val started = System.nanoTime()
    val value = body
    val elapsed = System.nanoTime() - started
    val allocated = bean.getCurrentThreadAllocatedBytes - allocatedBefore
    Measurement(value, elapsed, allocated)

  test("instrumented benchmark records view cost, allocation, IO narrowing, and cache reuse"):
    val receipt = withNiftiPath(runBenchmark)
    println(receipt.render)

    assert(receipt.chainConstructionNanos > 0L)
    assert(receipt.chainConstructionAllocatedBytes > 0L)
    assert(receipt.firstEvaluationNanos > 0L)
    assert(receipt.firstEvaluationAllocatedBytes > 0L)
    assert(receipt.repeatedEvaluationNanos > 0L)
    assert(receipt.repeatedEvaluationAllocatedBytes > 0L)
    assertEquals(receipt.sourceBytesRead, selectedRows.length.toLong * selectedObservations.toLong * 8L)
    assert(receipt.sourceBytesRead < receipt.fullSourceBytes)
    assertEquals(receipt.planCacheEntries, 1)
    assertEquals(receipt.resultCacheEntries, 1)
    assertEquals(receipt.runtimeStats.compilations, 1L)
    assertEquals(receipt.runtimeStats.executions, 1L)
    assertEquals(receipt.runtimeStats.resultCacheHits, receipt.repeatedEvaluations.toLong)
    assert(receipt.checksum.isFinite)

  private def runBenchmark(path: Path): SpatialLazyBenchmarkReceipt =
    val space = SampleSpaces(Vector(rows, 1, 1), trans = Some(DMat.eye(4)))
    val root = volumeDomain("native", space)
    val mid = volumeDomain("mid", space)
    val target = volumeDomain("target", space)
    val source = spatialValue(NiftiFieldSource.prepare(path, root, observations, "benchmark-bold"))
    val field = spatialValue(Field.fromSource(root, source))
    given SpatialGraph = spatialValue(
      SpatialGraph.build(
        Vector(root, mid, target),
        Vector(affine("native-mid", root, mid, 0.5), affine("mid-target", mid, target, 0.5))
      )
    )
    val fileValues =
      PrimitiveBuffers.tabulate[Double](rows * observations) { index =>
        val frame = index % observations
        val row = index / observations
        frame.toDouble * 1000.0 + row.toDouble
      }
    Nifti
      .writeSeries(path, SomeScalarSeries.unsafeCopyFromCanonicalArray(fileValues, space.addDim(observations, Some(Axis.Time)), "benchmark"))
      .fold(error => fail(error.message), _ => ())

    val bean = allocationBean()
    val chainIterations = 500
    val constructed = measure(bean) {
      var index = 0
      var current = Option.empty[Field]
      while index < chainIterations do
        current = Some(
          apiValue(
            field
              .to(mid)
              .flatMap(_.to(target))
              .flatMap(_.rows(selectedRows*))
              .flatMap(_.timeBlock(start = 5, length = selectedObservations))
          )
        )
        index += 1
      current.getOrElse(fail("benchmark did not construct a view"))
    }
    assertEquals(source.stats, NiftiFieldSourceStats(0L, 0L, 0L, 0L, 0L, 0L))

    val planCache = InMemoryEvaluationPlanCache.empty
    val resultCache = InMemoryFieldResultCache.empty
    val runtime = LazyFieldRuntime(
      summon[SpatialGraph],
      backend = PullbackProgramCompiler.volumeAffine,
      planCache = planCache,
      resultCache = resultCache
    )
    given FieldRuntime = runtime
    val first = measure(bean)(apiValue(constructed.value.value))
    val repeatedEvaluations = 50
    val repeated = measure(bean) {
      var index = 0
      var checksum = 0.0
      while index < repeatedEvaluations do
        val values = apiValue(constructed.value.value)
        checksum += values(0, 0)
        index += 1
      checksum
    }
    val firstChecksum = first.value.copyData.foldLeft(0.0)(_ + _)
    val fullSourceBytes = rows.toLong * observations.toLong * 8L

    SpatialLazyBenchmarkReceipt(
      chainIterations = chainIterations,
      chainConstructionNanos = constructed.elapsedNanos,
      chainConstructionAllocatedBytes = constructed.allocatedBytes,
      firstEvaluationNanos = first.elapsedNanos,
      firstEvaluationAllocatedBytes = first.allocatedBytes,
      repeatedEvaluations = repeatedEvaluations,
      repeatedEvaluationNanos = repeated.elapsedNanos,
      repeatedEvaluationAllocatedBytes = repeated.allocatedBytes,
      sourceBytesRead = source.stats.bytesRead,
      fullSourceBytes = fullSourceBytes,
      planCacheEntries = planCache.size,
      resultCacheEntries = resultCache.size,
      runtimeStats = runtime.stats,
      checksum = firstChecksum + repeated.value
    )
