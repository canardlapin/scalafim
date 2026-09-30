package scalafim.fmri.mvpa.spatial

import locus4s.{DomainRegistry, Region}
import scalafim.fmri.mvpa.*
import scalafim.image.{ExactVolumeSearchlight, GridDomain, SampleSpaces, SearchlightRadius}

class SearchlightPerformanceReceiptSuite extends munit.FunSuite:
  private val dims = Vector(20, 20, 12)
  private val samples = 8
  private val radius = SearchlightRadius.make(3.1).toOption.get
  private val response = Response.continuous(Vector.tabulate(samples)(_.toDouble)).toOption.get
  private val patterns =
    PatternMatrix.fromRows:
      Vector.tabulate(samples): sample =>
        Vector.tabulate(dims.product): feature =>
          ((sample + 1) * 17 + (feature % 101) * 3).toDouble / 29.0

  private val meanAnalysis: DenseRoiAnalysis =
    new DenseRoiAnalysis:
      override val name: String = "performance-mean"
      override val minFeatures: Int = 1

      override def evaluate(
          roi: PatternMatrix,
          context: RoiContext
      ): Either[MvpaError, RoiAnalysisResult] =
        var sum = 0.0
        var row = 0
        while row < roi.value.rows do
          var column = 0
          while column < roi.value.cols do
            sum += roi.value(row, column)
            column += 1
          row += 1
        Right(RoiAnalysisResult(MetricVector("Mean" -> (sum / (roi.value.rows * roi.value.cols)))))

  test("report neighborhood setup separately from a prepared local-analysis sweep"):
    val grid = SampleSpaces.requireVolumeD3(SampleSpaces(dims)).toOption.get.grid
    val packed =
      GridDomain
        .register(grid, "searchlight performance receipt", DomainRegistry.empty)
        .toOption
        .get
    type Voxel = packed.S
    val domain = packed.value
    val centers = Region.whole(domain.space)

    def prepare(): FeatureSetPlan =
      val neighborhoods =
        ExactVolumeSearchlight
          .metricBalls(domain, radius, centers)
          .toOption
          .get
      SpatialFeatureSetPlans
        .volumeSearchlight("performance receipt", neighborhoods)
        .toOption
        .get
        .plan

    def run(plan: FeatureSetPlan): (Int, Double) =
      val result =
        MvpaEngine.run(patterns, plan, response, meanAnalysis).toOption.get
      val checksum = result.successes.iterator.map(_.metrics("Mean").get).sum
      (result.successes.length, checksum)

    val warmedPlan = prepare()
    val warmed = run(warmedPlan)
    assertEquals(warmed._1, dims.product)

    val setupMillis = Vector.fill(3):
      val started = System.nanoTime()
      val plan = prepare()
      val elapsed = (System.nanoTime() - started).toDouble / 1e6
      assertEquals(plan.featureSets.length, dims.product)
      elapsed

    val kernelMillis = Vector.fill(3):
      val started = System.nanoTime()
      val result = run(warmedPlan)
      val elapsed = (System.nanoTime() - started).toDouble / 1e6
      assertEquals(result._1, dims.product)
      assertEqualsDouble(result._2, warmed._2, 1e-9)
      elapsed

    val neighborhoodMembers = warmedPlan.featureSets.iterator.map(_.size.toLong).sum
    val setupMedian = median(setupMillis)
    val kernelMedian = median(kernelMillis)
    println(
      f"SEARCHLIGHT_PERFORMANCE_RECEIPT {\"fixture\":\"20x20x12-r3.1\",\"centers\":${dims.product},\"neighborhood_members\":$neighborhoodMembers,\"setup_median_ms\":$setupMedian%.3f,\"kernel_median_ms\":$kernelMedian%.3f,\"checksum\":${warmed._2}%.12f}"
    )

  private def median(values: Vector[Double]): Double =
    values.sorted.apply(values.length / 2)
