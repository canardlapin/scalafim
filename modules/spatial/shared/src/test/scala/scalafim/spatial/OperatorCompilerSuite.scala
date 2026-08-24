package scalafim.spatial

import image4s.geometry.{Affine, D3}
import scalafim.image.{SampleSpaces, SomeSampleSpace, SpatialPoint}
import scalafim.image.SampleSpaces.*

class OperatorCompilerSuite extends munit.FunSuite:

  private def value[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def linValue[A](result: Either[LinearMapError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.getMessage)

  private def domain(name: String, dims: Vector[Int]): Domain =
    val id = value(DomainId(name))
    val subject = value(SubjectId("sub-01"))
    val modality = value(Modality(name))
    val geometry = value(SamplingGeometry.volume(SampleSpaces(dims, affine = Some(ProviderAffines.identity))))
    value(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def translation(x: Double, y: Double, z: Double): Affine[D3] =
    ProviderAffines.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, x),
        Vector(0.0, 1.0, 0.0, y),
        Vector(0.0, 0.0, 1.0, z),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def affine(
    idValue: String,
    source: Domain,
    target: Domain,
    matrix: Affine[D3]
  ): Morphism =
    value(
      Morphism.build(
        id = value(MorphismId(idValue)),
        source = source.id,
        target = target.id,
        kind = MorphismKind.Affine3D,
        routeTag = RouteTag.Anatomical,
        cost = 1.0,
        inverse = Inverse.Exact("analytic"),
        coordinateMap = value(CoordinateMap.affine(source, target, matrix))
      )
    )

  private def graph(domains: Vector[Domain], morphisms: Vector[Morphism]): SpatialGraph =
    value(SpatialGraph.build(domains, morphisms))

  private def triplets(operator: SpatialOperator): SparseTriplets =
    operator.map match
      case csr: CsrMatrix => csr.toTriplets
      case other => fail(s"expected CsrMatrix, got ${other.getClass.getName}")

  test("identity volume compilation emits zero-based target-by-source rows"):
    val epi = domain("epi", Vector(2, 2, 1))
    val request = CompileRequest(epi.id, epi.id, sampling = SamplingPolicy.Nearest)
    val operator = value(OperatorCompiler.compile(graph(Vector(epi), Vector.empty), request))
    val t = triplets(operator)

    assertEquals(operator.rows, 4)
    assertEquals(operator.cols, 4)
    assertEquals(t.rowIndices.toVector, Vector(0, 1, 2, 3))
    assertEquals(t.colIndices.toVector, Vector(0, 1, 2, 3))
    assertEquals(t.values.toVector, Vector(1.0, 1.0, 1.0, 1.0))
    assertEquals(operator.qc.coverage.rowCoverage, Vector(1.0, 1.0, 1.0, 1.0))
    assertEquals(operator.provenance.path.map(_.value), Vector("identity:epi"))
    assertEquals(operator.provenance.sampling, SamplingPolicy.Nearest)

  test("trilinear affine compilation normalizes row weights"):
    val source = domain("source", Vector(3, 1, 1))
    val target = domain("target", Vector(1, 1, 1))
    val morphism = affine("source-to-target", source, target, translation(0.25, 0.0, 0.0))
    val operator =
      value(OperatorCompiler.compile(graph(Vector(source, target), Vector(morphism)), CompileRequest(source.id, target.id)))
    val t = triplets(operator)

    assertEquals(t.rowIndices.toVector, Vector(0, 0))
    assertEquals(t.colIndices.toVector, Vector(0, 1))
    assertEqualsDouble(t.values.toVector.sum, 1.0, 1e-12)
    assertEqualsDouble(t.values.toVector(0), 0.75, 1e-12)
    assertEqualsDouble(t.values.toVector(1), 0.25, 1e-12)
    assertEqualsDouble(operator.qc.coverage.rowCoverage.head, 1.0, 1e-12)

    val sourceValues = DoubleMatrix.fromRows(Vector(Vector(10.0), Vector(20.0), Vector(30.0)))
    val sampled = linValue(operator.forward(sourceValues))
    assertEqualsDouble(sampled(0, 0), 12.5, 1e-12)

  test("ROI rows preserve order and report out-of-bounds coverage"):
    val source = domain("source", Vector(2, 1, 1))
    val target = domain("target", Vector(3, 1, 1))
    val morphism = affine("source-to-target", source, target, ProviderAffines.identity)
    val request =
      CompileRequest(
        source = source.id,
        target = target.id,
        sampling = SamplingPolicy.Nearest,
        roi = Some(Vector(2, 1))
      )
    val operator = value(OperatorCompiler.compile(graph(Vector(source, target), Vector(morphism)), request))
    val sampled = linValue(operator.forward(DoubleMatrix.fromRows(Vector(Vector(5.0), Vector(7.0)))))

    assertEquals(operator.rows, 2)
    assertEquals(operator.qc.coverage.targetRows, Vector(2, 1))
    assertEquals(operator.qc.coverage.rowCoverage, Vector(0.0, 1.0))
    assertEquals(operator.qc.coverage.uncoveredTargetRows, Vector(2))
    assertEqualsDouble(sampled(0, 0), 0.0, 1e-12)
    assertEqualsDouble(sampled(1, 0), 7.0, 1e-12)
    assertEquals(operator.provenance.roi, Some(Vector(2, 1)))
    assertEquals(operator.provenance.rowSelection, RowSelection.Rows(Vector(2, 1)))
    assertEquals(operator.signature.shape, OperatorShape.unsafe(2, 2))
    assertEquals(operator.signature.recipe, operator.provenance.recipe)

  test("typed row selections produce the same cache signature as ROI adapters"):
    val source = domain("source", Vector(2, 1, 1))
    val target = domain("target", Vector(3, 1, 1))
    val morphism = affine("source-to-target", source, target, ProviderAffines.identity)
    val g = graph(Vector(source, target), Vector(morphism))
    val rows = value(RowSelection.rows(Vector(2, 1)))
    val typed =
      value(
        OperatorCompiler.compile(
          g,
          CompileRequest.forRows(source.id, target.id, rows, sampling = SamplingPolicy.Nearest)
        )
      )
    val adapter =
      value(
        OperatorCompiler.compile(
          g,
          CompileRequest(source.id, target.id, sampling = SamplingPolicy.Nearest, roi = Some(Vector(2, 1)))
        )
      )

    assertEquals(typed.signature, adapter.signature)
    assertEquals(OperatorCacheKey.from(typed), OperatorCacheKey.from(adapter))

  test("compiler rejects duplicate and out-of-bounds ROI rows"):
    val source = domain("source", Vector(2, 1, 1))
    val target = domain("target", Vector(2, 1, 1))
    val morphism = affine("source-to-target", source, target, ProviderAffines.identity)
    val g = graph(Vector(source, target), Vector(morphism))

    val duplicate = OperatorCompiler.compile(g, CompileRequest(source.id, target.id, roi = Some(Vector(0, 0))))
    assertEquals(duplicate.left.toOption, Some(SpatialError.DuplicateRoiRow(0)))

    val outOfBounds = OperatorCompiler.compile(g, CompileRequest(source.id, target.id, roi = Some(Vector(2))))
    assertEquals(outOfBounds.left.toOption, Some(SpatialError.InvalidRoiRow(2, 2)))

  test("compiler requires executable coordinate maps"):
    val source = domain("source", Vector(2, 1, 1))
    val target = domain("target", Vector(2, 1, 1))
    val morphism =
      value(
        Morphism.build(
          id = value(MorphismId("source-to-target")),
          source = source.id,
          target = target.id,
          kind = MorphismKind.Affine3D,
          routeTag = RouteTag.Anatomical,
          cost = 1.0
        )
      )
    val result = OperatorCompiler.compile(graph(Vector(source, target), Vector(morphism)), CompileRequest(source.id, target.id))

    assertEquals(result.left.toOption, Some(SpatialError.MissingCoordinateMap(morphism.id)))

  test("spatial affine paths compile without a second image transform hierarchy"):
    val source = domain("source", Vector(3, 1, 1))
    val mid = domain("mid", Vector(3, 1, 1))
    val target = domain("target", Vector(3, 1, 1))
    val first = affine("source-to-mid", source, mid, translation(1.0, 0.0, 0.0))
    val second = affine("mid-to-target", mid, target, translation(0.0, 2.0, 0.0))
    val path = value(graph(Vector(source, mid, target), Vector(first, second)).path(source.id, target.id))
    val executable = value(ExecutableAffinePath.from(path))

    assertEquals(executable.source, source.id)
    assertEquals(executable.target, target.id)
    assertEquals(
      value(executable.coordinateMap.transform(Vector(0.0, 0.0, 0.0))),
      Vector(1.0, 2.0, 0.0)
    )
    assertEquals(
      value(executable.coordinateMap.transform(SpatialPoint.Origin)),
      SpatialPoint(1.0, 2.0, 0.0)
    )
