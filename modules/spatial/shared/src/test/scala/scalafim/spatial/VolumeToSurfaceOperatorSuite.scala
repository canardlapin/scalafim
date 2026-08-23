package scalafim.spatial

import scalafim.image.{DMat, Mask, NeuroSpace, NeuroVol, PrimitiveBuffers}
import scalafim.linalg.{CsrMatrix, DoubleMatrix, LinearMapError, SparseTriplets}
import scalafim.surface.*

class VolumeToSurfaceOperatorSuite extends munit.FunSuite:

  private val space = NeuroSpace(Vector(3, 3, 3))
  private val volume =
    NeuroVol.copyFromCanonicalArray(
      PrimitiveBuffers.tabulate[Double](27) { idx =>
        val g = space.indexToGrid3D(idx)
        g(0).toDouble + 10.0 * g(1).toDouble + 100.0 * g(2).toDouble
      },
      space,
      "synthetic"
    )

  private val pair =
    SurfaceGeometryPair(
      surfaceAtZ(0.0, SurfaceKind.White),
      surfaceAtZ(2.0, SurfaceKind.Pial)
    )

  private def value[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def linValue[A](result: Either[LinearMapError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def volumeDomain(mask: Option[NeuroVol[Boolean]] = None): Domain =
    val id = value(DomainId("volume"))
    val subject = value(SubjectId("sub-01"))
    val modality = value(Modality("bold"))
    val geometry = value(SamplingGeometry.volume(space, mask))
    value(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def surfaceDomain(mask: Option[SurfaceRoi[Boolean]] = None): Domain =
    val id = value(DomainId("surface"))
    val subject = value(SubjectId("sub-01"))
    val geometry = value(SamplingGeometry.surface(pair.white, mask))
    value(Domain.build(id, SpaceRef.Surface(subject, Hemisphere.Left, SurfaceKind.White), geometry))

  private def volumeToSurface(source: Domain, target: Domain): Morphism =
    value(
      Morphism.build(
        id = value(MorphismId("volume-to-surface")),
        source = source.id,
        target = target.id,
        kind = MorphismKind.VolumeToSurface,
        routeTag = RouteTag.Anatomical,
        cost = 1.0,
        inverse = Inverse.AdjointOnly
      )
    )

  private def graph(source: Domain, target: Domain): SpatialGraph =
    value(SpatialGraph.build(Vector(source, target), Vector(volumeToSurface(source, target))))

  private def sourceMatrix: DoubleMatrix =
    DoubleMatrix.fromRows(Vector.tabulate(space.spatialDims.product)(i => Vector(volume.valueAtCanonicalOrdinal(i))))

  private def compile(source: Domain, target: Domain, request: VolumeToSurfaceRequest): SpatialOperator =
    value(VolumeToSurfaceOperatorCompiler.compile(graph(source, target), request))

  private def triplets(operator: SpatialOperator): SparseTriplets =
    operator.map match
      case csr: CsrMatrix => csr.toTriplets
      case other => fail(s"expected CsrMatrix, got ${other.getClass.getName}")

  test("midpoint operator matches eager surface sampling and exposes an adjoint"):
    val source = volumeDomain()
    val target = surfaceDomain()
    val request = VolumeToSurfaceRequest.midpoint(source.id, target.id, pair)
    val operator = compile(source, target, request)
    val sampled = linValue(operator.forward(sourceMatrix))
    val eager = VolumeSurfaceSampler.sample(volume, pair, path = SurfaceSamplingPath.Midpoint)

    assertEquals(sampled.rows, pair.white.vertexCount)
    assertEqualsDouble(sampled(0, 0), eager.values.valueAt(VertexId(0)).get, 1e-12)
    assertEqualsDouble(sampled(1, 0), eager.values.valueAt(VertexId(1)).get, 1e-12)
    assertEqualsDouble(sampled(2, 0), eager.values.valueAt(VertexId(2)).get, 1e-12)
    assertEquals(operator.qc.coverage.rowCoverage, Vector(1.0, 1.0, 1.0))

    val back = linValue(operator.map.adjoint.forward(DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0)))))
    assertEqualsDouble(back(1, 0), 1.0, 1e-12)
    assertEqualsDouble(back(10, 0), 2.0, 1e-12)
    assertEqualsDouble(back(4, 0), 3.0, 1e-12)

  test("ribbon operator averages normalized white-to-pial sample weights"):
    val source = volumeDomain()
    val target = surfaceDomain()
    val request = VolumeToSurfaceRequest.ribbon(source.id, target.id, pair, fractions = Vector(0.0, 1.0))
    val operator = compile(source, target, request)
    val sampled = linValue(operator.forward(sourceMatrix))
    val t = triplets(operator)

    assertEqualsDouble(sampled(0, 0), 100.0, 1e-12)
    assertEqualsDouble(sampled(1, 0), 101.0, 1e-12)
    assertEquals(operator.qc.coverage.rowCoverage, Vector(1.0, 1.0, 1.0))

    val row0 = t.rowIndices.zip(t.colIndices).zip(t.values).collect {
      case ((row, col), value) if row == 0 => col -> value
    }.toVector
    assertEquals(row0, Vector(0 -> 0.5, 2 -> 0.5))

  test("source masks keep valid ribbon samples normalized and report partial coverage"):
    val pialVertex0 = space.gridToIndex3D(0, 0, 2)
    val mask = Mask.fromIndices(space, PrimitiveBuffers.fromArray(Array(pialVertex0)), "pial-only")
    val source = volumeDomain(Some(mask))
    val target = surfaceDomain()
    val request = VolumeToSurfaceRequest.ribbon(source.id, target.id, pair, fractions = Vector(0.0, 1.0))
    val operator = compile(source, target, request)
    val sampled = linValue(operator.forward(sourceMatrix))

    assertEqualsDouble(sampled(0, 0), 200.0, 1e-12)
    assertEqualsDouble(sampled(1, 0), 0.0, 1e-12)
    assertEquals(operator.qc.coverage.rowCoverage, Vector(0.5, 0.0, 0.0))
    assertEquals(operator.qc.coverage.uncoveredTargetRows, Vector(1, 2))

  test("surface masks produce empty operator rows for masked vertices"):
    val field = SurfaceField.full(pair.white, Vector(false, true, false), "surface-mask")
    val mask =
      SurfaceRoi.fromField(field, Vector(VertexId(0), VertexId(1), VertexId(2)), "surface-mask")
    val source = volumeDomain()
    val target = surfaceDomain(Some(mask))
    val request = VolumeToSurfaceRequest.midpoint(source.id, target.id, pair)
    val operator = compile(source, target, request)
    val sampled = linValue(operator.forward(sourceMatrix))

    assertEqualsDouble(sampled(0, 0), 0.0, 1e-12)
    assertEqualsDouble(sampled(1, 0), 101.0, 1e-12)
    assertEqualsDouble(sampled(2, 0), 0.0, 1e-12)
    assertEquals(operator.qc.coverage.rowCoverage, Vector(0.0, 1.0, 0.0))

  test("ROI rows preserve requested vertex order"):
    val source = volumeDomain()
    val target = surfaceDomain()
    val request =
      VolumeToSurfaceRequest.midpoint(source.id, target.id, pair, roi = Some(Vector(2, 0)))
    val operator = compile(source, target, request)
    val sampled = linValue(operator.forward(sourceMatrix))

    assertEquals(operator.rows, 2)
    assertEquals(operator.qc.coverage.targetRows, Vector(2, 0))
    assertEqualsDouble(sampled(0, 0), 110.0, 1e-12)
    assertEqualsDouble(sampled(1, 0), 100.0, 1e-12)

  test("compiler rejects paths without a volume-to-surface morphism"):
    val source = volumeDomain()
    val target = surfaceDomain()
    val bad =
      value(
        Morphism.build(
          id = value(MorphismId("bad")),
          source = source.id,
          target = target.id,
          kind = MorphismKind.Functional,
          routeTag = RouteTag.Anatomical,
          cost = 1.0
        )
      )
    val badGraph = value(SpatialGraph.build(Vector(source, target), Vector(bad)))
    val result =
      VolumeToSurfaceOperatorCompiler.compile(
        badGraph,
        VolumeToSurfaceRequest.midpoint(source.id, target.id, pair)
      )

    assertEquals(result.left.toOption, Some(SpatialError.UnsupportedMorphismForCompilation(bad.id, bad.kind)))

  private def surfaceAtZ(z: Double, kind: SurfaceKind): SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(0.0, 0.0, z),
          Vector(1.0, 0.0, z),
          Vector(0.0, 1.0, z)
        ),
        Vector((0, 1, 2))
      ),
      Hemisphere.Left,
      kind
    )
