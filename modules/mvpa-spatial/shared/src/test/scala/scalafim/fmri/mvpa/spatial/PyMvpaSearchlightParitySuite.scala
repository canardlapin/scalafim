package scalafim.fmri.mvpa.spatial

import mvpa4s.locus.*

import image4s.locus.GridDomain

import gale.linalg.DMat
import locus4s.Region
import multivar.core.{SemanticSpace, SpaceRole, ValueId, ValueIdentity}
import image4s.geometry.{Affine, D3}
import locus4s.{DomainRegistry, Region as VolumeRegion}
import neuroim4s.image.{ExactVolumeSearchlight, SampleSpaces, SearchlightRadius}
import locus4s.{DomainId, Region as SurfaceRegion}
import neurosurf4s.surface.{DistanceMetric, Hemisphere, MeshTopology, SurfaceGeometry, SurfaceKind, SurfaceLocusDomain, SurfaceSearchlight, TriangleMesh}
import mvpa4s.{AxisRef, EvidenceSource, Observations}
import mvpa4s.measurement.{MeasurementFrameDeclaration, MeasurementId, MeasurementResource, MeasurementVisitor}
import locus4s.provenance.{Provenance, ProvenanceId, SourceId}

class PyMvpaSearchlightParitySuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)

  test("frozen PyMVPA volume neighborhoods execute as real spatial measurements with local numeric parity"):
    val fixture = PyMvpaSearchlightParityFixtures.Volume
    val affine = right(Affine.fromRowMajor[D3](fixture.affineRowMajor))
    val grid = right(SampleSpaces.requireVolumeD3(SampleSpaces(fixture.dims, affine = Some(affine)))).grid
    val packed = right(GridDomain.register(grid, "pymvpa-physical", DomainRegistry.empty))
    val centers = right(VolumeRegion.fromOrdinals(packed.value.space, fixture.centerOrdinals))
    val support = right(VolumeRegion.fromOrdinals(packed.value.space, fixture.supportOrdinals))
    val balls = right(ExactVolumeSearchlight.metricBalls(packed.value, right(SearchlightRadius.make(fixture.radiusMillimeters)), centers))
    val physical = right(ExactVolumeSearchlight.restrictTargets(balls, support))
    val rowsByVoxel = physical.centers.indices.map: center =>
      physical.center(center).ordinal -> physical.neighborhood(center).ordinalsInDomainOrder.toVector
    val byVoxel = rowsByVoxel.toMap
    val physicalRows = fixture.centerOrdinals.map(center => center -> byVoxel(center))
    assertEquals(physicalRows, fixture.neighborhoods)
    assertEquals(directPhysicalNeighborhoods(fixture.dims, fixture.affineRowMajor, fixture.supportOrdinals, fixture.centerOrdinals, fixture.radiusMillimeters * fixture.radiusMillimeters), fixture.neighborhoods)
    assertEqualsDouble(physicalSquaredDistance(fixture.dims, fixture.affineRowMajor, 8, 4), 10.0, 0.0)
    val indexCounterfactual = directIndexNeighborhoods(fixture.dims, fixture.supportOrdinals, fixture.centerOrdinals, fixture.radiusMillimeters * fixture.radiusMillimeters)
    assertNotEquals(indexCounterfactual, fixture.neighborhoods)
    assert(indexCounterfactual.head._2.contains(1))
    assertLocalMeasurements("volume", fixture.patternMatrix.value, fixture.dims.product, physicalRows, fixture.localMeanContrast)

  private def directPhysicalNeighborhoods(dims: Vector[Int], affine: Vector[Double], support: Vector[Int], centers: Vector[Int], radiusSquared: Double): Vector[(Int, Vector[Int])] =
    centers.map(center => center -> support.filter(candidate => physicalSquaredDistance(dims, affine, center, candidate) <= radiusSquared))

  private def physicalSquaredDistance(dims: Vector[Int], affine: Vector[Double], left: Int, right: Int): Double =
    val a = voxelCoordinate(dims, left)
    val b = voxelCoordinate(dims, right)
    val dx = b(0) - a(0); val dy = b(1) - a(1); val dz = b(2) - a(2)
    val wx = affine(0) * dx + affine(1) * dy + affine(2) * dz
    val wy = affine(4) * dx + affine(5) * dy + affine(6) * dz
    val wz = affine(8) * dx + affine(9) * dy + affine(10) * dz
    wx * wx + wy * wy + wz * wz

  private def directIndexNeighborhoods(dims: Vector[Int], support: Vector[Int], centers: Vector[Int], radiusSquared: Double): Vector[(Int, Vector[Int])] =
    centers.map: center =>
      val origin = voxelCoordinate(dims, center)
      center -> support.filter: candidate =>
        val point = voxelCoordinate(dims, candidate)
        val dx = point(0) - origin(0); val dy = point(1) - origin(1); val dz = point(2) - origin(2)
        dx * dx + dy * dy + dz * dz <= radiusSquared

  test("surface searchlights use actual geodesic construction rather than chord neighborhoods"):
    val fixture = PyMvpaSearchlightParityFixtures.Surface
    val geometry = SurfaceGeometry(TriangleMesh.fromRows(fixture.vertices, fixture.faces), Hemisphere.Left, SurfaceKind.Pial)
    val domain = right(SurfaceLocusDomain.semantic(DomainId.parse("pymvpa-surface").toOption.get, geometry)).value
    val centers = right(SurfaceRegion.fromOrdinals(domain.finiteSpace, fixture.centerOrdinals))
    val searchlight = right(SurfaceSearchlight.metricBalls(domain, MeshTopology.from(geometry), fixture.radius, centers, DistanceMetric.Geodesic))
    val neighborhoods = searchlight.value
    val supports = neighborhoods.centers.indices.map: compact =>
      neighborhoods.center(compact).ordinal -> neighborhoods.neighborhood(compact).ordinalsInDomainOrder.toVector
    .toMap
    val actual = fixture.centerOrdinals.map(center => center -> supports(center))
    assertEquals(actual, fixture.geodesicNeighborhoods)
    assertNotEquals(actual, fixture.chordNeighborhoods)
    assert(fixture.chordNeighborhoods.head._2.contains(3))
    assert(!actual.head._2.contains(3))
    assertLocalMeasurements("surface", fixture.patternMatrix.value, fixture.vertices.size, actual, fixture.localMeanContrast)

  test("invalid radius is typed and an entry failure leaves later frame entries present"):
    assert(SearchlightRadius.make(-0.1).isLeft)
    val fixture = PyMvpaSearchlightParityFixtures.Volume
    assertLocalMeasurements("failure", fixture.patternMatrix.value, fixture.dims.product, fixture.neighborhoods, fixture.localMeanContrast, Some(13))

  private def assertLocalMeasurements(
      name: String, values: DMat, featureCount: Int,
      neighborhoods: Vector[(Int, Vector[Int])], expected: Vector[Double],
      failAt: Option[Int] = None
  ): Unit =
    val samples = right(AxisRef.fromStableKeys(name + "-samples", SpaceRole.Samples, Vector.tabulate(values.rows)(_.toString), "fixture", "unit", "raw"))
    val neural = right(AxisRef.fromStableKeys(name + "-neural", SpaceRole.Observed, Vector.tabulate(featureCount)(_.toString), "fixture", "unit", "raw"))
    val sourceId = SourceId.unsafe("pymvpa-" + name)
    val source = right(EvidenceSource(sourceId, Provenance.source(ProvenanceId.unsafe(name + "-root"), sourceId)))
    val observations = right(Observations.fromDense(samples, neural, values, ValueIdentity.source(ValueId.unsafe(name + "-values")), source))
    val sites = neighborhoods.map: (center, members) =>
      SpatialMeasurementSite(
        MeasurementId.unsafe(name + "-" + center),
        SpatialMeasurementSupport.Regional(right(Region.fromOrdinals(neural.locus, members))),
        Some(neural.locus.indexAtValidatedOrdinal(center)), Some(center.toString)
      )
    val frame = right(SpatialMeasurementFrames.regions(neural, MeasurementFrameDeclaration(name, PyMvpaSearchlightParityFixtures.sourceRevision, Vector.empty), sites))
    val visitor = new MeasurementVisitor[samples.Id, neural.Id, String, SpatialMeasurementRendition[neural.Locus], Double]:
      def visit[L <: SemanticSpace](entry: mvpa4s.measurement.PackedMeasurementEntry[neural.Id, String, SpatialMeasurementRendition[neural.Locus]] { type Local = L }, measured: mvpa4s.measurement.MeasuredObservations[samples.Id, L]) =
        val center = entry.measurement.descriptor.id.value.stripPrefix(name + "-").toInt
        if failAt.contains(center) then Left(mvpa4s.measurement.MeasurementFailure.Task("forced fixture failure"))
        else measured.patterns(DMat.eye(measured.patterns.cols)).left.map(error => mvpa4s.measurement.MeasurementFailure.Task(error.message)).map: local =>
          var zero = 0.0
          var one = 0.0
          var zeroCount = 0
          var oneCount = 0
          var row = 0
          while row < local.rows do
            var column = 0
            while column < local.cols do
              PyMvpaSearchlightParityFixtures.labels(row) match
                case "zero" =>
                  zero += local(row, column)
                  zeroCount += 1
                case "one" =>
                  one += local(row, column)
                  oneCount += 1
                case other => fail("unexpected fixture label " + other)
              column += 1
            row += 1
          one / oneCount - zero / zeroCount
    val result = frame.traverse(1)(Right(MeasurementResource(observations)(())))(visitor)
    assertEquals(result.error, None)
    assertEquals(result.value.map(_.descriptor.id.value), neighborhoods.map((center, _) => name + "-" + center).sorted)
    val expectedByCenter = neighborhoods.map(_._1).zip(expected).toMap
    result.value.foreach: outcome =>
      val center = outcome.descriptor.id.value.stripPrefix(name + "-").toInt
      outcome.value match
        case Right(actual) =>
          assert(!failAt.contains(center))
          assertEqualsDouble(actual, expectedByCenter(center), PyMvpaSearchlightParityFixtures.tolerance)
        case Left(error) =>
          assert(failAt.contains(center), error.toString)
          assertEquals(error, mvpa4s.measurement.MeasurementFailure.Task("forced fixture failure"))
    failAt.foreach: _ =>
      assertEquals(result.value.count(_.value.isLeft), 1)
      assert(result.value.exists(outcome => outcome.descriptor.id.value == name + "-23" && outcome.value.isRight))

  private def voxelCoordinate(dims: Vector[Int], ordinal: Int): Vector[Int] =
    val z = ordinal % dims(2)
    val xy = ordinal / dims(2)
    Vector(xy / dims(1), xy % dims(1), z)
