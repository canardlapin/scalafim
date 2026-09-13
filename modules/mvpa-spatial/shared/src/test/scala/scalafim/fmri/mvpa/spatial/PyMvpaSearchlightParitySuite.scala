package scalafim.fmri.mvpa.spatial

import image4s.geometry.{Affine, D3}
import locus4s.{DomainRegistry, Region as VolumeRegion}
import scalafim.fmri.mvpa.*
import scalafim.image.{ExactVolumeSearchlight, GridDomain, SampleSpaces, SearchlightRadius}
import scalafim.locus.{Region as SurfaceRegion, SpaceKey}
import scalafim.surface.{
  DistanceMetric,
  Hemisphere,
  MeshTopology,
  SurfaceGeometry,
  SurfaceKind,
  SurfaceLocusDomain,
  SurfaceSearchlight,
  TriangleMesh
}

class PyMvpaSearchlightParitySuite extends munit.FunSuite:
  private val Tolerance = PyMvpaSearchlightParityFixtures.tolerance
  private val response =
    Response.categorical(PyMvpaSearchlightParityFixtures.labels).toOption.get

  private final case class MeanContrastAnalysis(
      failAt: Option[Int] = None
  ) extends DenseRoiAnalysis:
    override val name: String = "fixture-mean-contrast"
    override val minFeatures: Int = 1

    override def evaluate(
        roi: PatternMatrix,
        context: RoiContext
    ): Either[MvpaError, RoiAnalysisResult] =
      if failAt.contains(context.featureSet.id.value) then
        Left(MvpaError.AnalysisFailed(context.featureSet.id, "forced fixture failure"))
      else
        context.response match
          case Response.Categorical(labels) =>
            var zeroSum = 0.0
            var zeroCount = 0
            var oneSum = 0.0
            var oneCount = 0
            var row = 0
            while row < roi.value.rows do
              var column = 0
              while column < roi.value.cols do
                labels(row).value match
                  case "zero" =>
                    zeroSum += roi.value(row, column)
                    zeroCount += 1
                  case "one" =>
                    oneSum += roi.value(row, column)
                    oneCount += 1
                  case other =>
                    return Left(
                      MvpaError.AnalysisFailed(
                        context.featureSet.id,
                        s"unexpected fixture label $other"
                      )
                    )
                column += 1
              row += 1
            Right(
              RoiAnalysisResult(
                MetricVector("MeanContrast" -> (oneSum / oneCount - zeroSum / zeroCount))
              )
            )
          case _ =>
            Left(
              MvpaError.AnalysisFailed(
                context.featureSet.id,
                "fixture requires categorical response"
              )
            )

  test("physical-mm volume neighborhoods match native PyMVPA under a complete affine"):
    val fixture = PyMvpaSearchlightParityFixtures.Volume
    val affine = Affine.fromRowMajor[D3](fixture.affineRowMajor).toOption.get
    val sampleSpace = SampleSpaces(fixture.dims, affine = Some(affine))
    val grid = SampleSpaces.requireVolumeD3(sampleSpace).toOption.get.grid
    val packed =
      GridDomain
        .register(grid, "PyMVPA physical searchlight parity", DomainRegistry.empty)
        .toOption
        .get
    type Voxel = packed.S
    val domain = packed.value
    val centers =
      VolumeRegion.fromOrdinals(domain.space, fixture.centerOrdinals).toOption.get
    val support =
      VolumeRegion.fromOrdinals(domain.space, fixture.supportOrdinals).toOption.get
    val unrestricted =
      ExactVolumeSearchlight
        .metricBalls(
          domain,
          SearchlightRadius.make(fixture.radiusMillimeters).toOption.get,
          centers
        )
        .toOption
        .get
    val neighborhoods =
      ExactVolumeSearchlight.restrictTargets(unrestricted, support).toOption.get
    val plan =
      SpatialFeatureSetPlans
        .volumeSearchlight("PyMVPA physical searchlight", neighborhoods)
        .toOption
        .get
        .plan

    assertEquals(plan.featureSets.map(_.id.value), fixture.centerOrdinals)
    assertEquals(plan.featureSets.map(_.center.map(_.value)), fixture.centerOrdinals.map(Some(_)))
    assertEquals(
      plan.featureSets.map(set => set.id.value -> set.featureIndices.map(_.value)),
      fixture.neighborhoods
    )
    assertEquals(
      directPhysicalNeighborhoods(
        fixture.dims,
        fixture.affineRowMajor,
        fixture.supportOrdinals,
        fixture.centerOrdinals,
        fixture.radiusMillimeters * fixture.radiusMillimeters
      ),
      fixture.neighborhoods
    )

    val boundaryMembers = plan.featureSets.find(_.id.value == 8).get.featureIndices.map(_.value)
    assertEquals(boundaryMembers, Vector(2, 4, 8, 10, 12, 14))
    assertEqualsDouble(physicalSquaredDistance(fixture.dims, fixture.affineRowMajor, 8, 4), 10.0, 0.0)
    assertEqualsDouble(physicalSquaredDistance(fixture.dims, fixture.affineRowMajor, 8, 10), 10.0, 0.0)

    val indexCounterfactual =
      directIndexNeighborhoods(
        fixture.dims,
        fixture.supportOrdinals,
        fixture.centerOrdinals,
        fixture.radiusMillimeters * fixture.radiusMillimeters
      )
    assertNotEquals(indexCounterfactual, fixture.neighborhoods)
    assert(indexCounterfactual.head._2.contains(1))
    assert(!fixture.neighborhoods.head._2.contains(1))

    assertLocalResults(
      fixture.patternMatrix,
      plan,
      fixture.localMeanContrast
    )

  test("surface neighborhoods match independent Dijkstra geodesics, not chord spheres"):
    val fixture = PyMvpaSearchlightParityFixtures.Surface
    val geometry =
      SurfaceGeometry(
        TriangleMesh.fromRows(fixture.vertices, fixture.faces),
        Hemisphere.Left,
        SurfaceKind.Pial
      )
    val topology = MeshTopology.from(geometry)
    val packed =
      SurfaceLocusDomain
        .semantic(SpaceKey.unsafe("umvpa:pymvpa-parity:left"), geometry)
        .toOption
        .get
    type Vertex = packed.S
    val domain = packed.value
    val centers =
      SurfaceRegion.fromOrdinals(domain.finiteSpace, fixture.centerOrdinals).toOption.get
    val searchlight =
      SurfaceSearchlight
        .metricBalls(
          domain,
          topology,
          fixture.radius,
          centers,
          DistanceMetric.Geodesic
        )
        .toOption
        .get
    val plan =
      LocusFeatureSetPlans
        .fromSearchlight("independent Dijkstra surface parity", searchlight)
        .toOption
        .get

    assertEquals(plan.featureSets.map(_.id.value), fixture.centerOrdinals)
    assertEquals(
      plan.featureSets.map(set => set.id.value -> set.featureIndices.map(_.value)),
      fixture.geodesicNeighborhoods
    )
    assertNotEquals(fixture.geodesicNeighborhoods, fixture.chordNeighborhoods)
    assert(fixture.chordNeighborhoods.head._2.contains(3))
    assert(!fixture.geodesicNeighborhoods.head._2.contains(3))
    assertLocalResults(
      fixture.patternMatrix,
      plan,
      fixture.localMeanContrast
    )

  test("invalid geometry is typed and one local failure does not abort later centers"):
    assert(
      SearchlightRadius
        .make(-0.1)
        .left
        .toOption
        .exists(_.message.contains("non-negative"))
    )

    val fixture = PyMvpaSearchlightParityFixtures.Volume
    val plan = FeatureSetPlan.fromNeighborhoods("local-failure", fixture.neighborhoods).toOption.get
    val result =
      MvpaEngine
        .run(
          fixture.patternMatrix,
          plan,
          response,
          MeanContrastAnalysis(failAt = Some(13))
        )
        .toOption
        .get

    assertEquals(result.failures.map(_.roiId.value), Vector(13))
    assertEquals(
      result.failures.head.error,
      MvpaError.AnalysisFailed(RoiId(13), "forced fixture failure")
    )
    assertEquals(result.successes.map(_.roiId.value), Vector(0, 5, 8, 18, 23))
    assert(result.successes.exists(_.roiId.value == 23))

  private def assertLocalResults(
      patterns: PatternMatrix,
      plan: FeatureSetPlan,
      expected: Vector[Double]
  ): Unit =
    val result =
      MvpaEngine
        .run(patterns, plan, response, MeanContrastAnalysis())
        .toOption
        .get
    assertEquals(result.failures, Vector.empty)
    assertEquals(result.successes.map(_.roiId.value), plan.featureSets.map(_.id.value))
    result.successes
      .zip(expected)
      .foreach: (outcome, reference) =>
        val actual = outcome.metrics("MeanContrast").get
        assertEqualsDouble(actual, reference, Tolerance + Tolerance * math.abs(reference))

  private def directPhysicalNeighborhoods(
      dims: Vector[Int],
      affine: Vector[Double],
      support: Vector[Int],
      centers: Vector[Int],
      radiusSquared: Double
  ): Vector[(Int, Vector[Int])] =
    centers.map: center =>
      center -> support.filter: candidate =>
        physicalSquaredDistance(dims, affine, center, candidate) <= radiusSquared

  private def physicalSquaredDistance(
      dims: Vector[Int],
      affine: Vector[Double],
      left: Int,
      right: Int
  ): Double =
    val a = voxelCoordinate(dims, left)
    val b = voxelCoordinate(dims, right)
    val dx = b(0) - a(0)
    val dy = b(1) - a(1)
    val dz = b(2) - a(2)
    val wx = affine(0) * dx + affine(1) * dy + affine(2) * dz
    val wy = affine(4) * dx + affine(5) * dy + affine(6) * dz
    val wz = affine(8) * dx + affine(9) * dy + affine(10) * dz
    wx * wx + wy * wy + wz * wz

  private def directIndexNeighborhoods(
      dims: Vector[Int],
      support: Vector[Int],
      centers: Vector[Int],
      radiusSquared: Double
  ): Vector[(Int, Vector[Int])] =
    centers.map: center =>
      val origin = voxelCoordinate(dims, center)
      center -> support.filter: candidate =>
        val point = voxelCoordinate(dims, candidate)
        val dx = point(0) - origin(0)
        val dy = point(1) - origin(1)
        val dz = point(2) - origin(2)
        dx * dx + dy * dy + dz * dz <= radiusSquared

  private def voxelCoordinate(dims: Vector[Int], ordinal: Int): Vector[Int] =
    val z = ordinal % dims(2)
    val xy = ordinal / dims(2)
    val y = xy % dims(1)
    val x = xy / dims(1)
    Vector(x, y, z)
