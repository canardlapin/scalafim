package scalafim.spatial

import scalafim.image.{DMat, NeuroSpace}

class SpatialQcSuite extends munit.FunSuite:

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
    val geometry = value(SamplingGeometry.volume(NeuroSpace(dims, trans = Some(DMat.eye(4)))))
    value(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def translation(x: Double, y: Double, z: Double): DMat =
    DMat.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, x),
        Vector(0.0, 1.0, 0.0, y),
        Vector(0.0, 0.0, 1.0, z),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def affine(idValue: String, source: Domain, target: Domain, matrix: DMat): Morphism =
    value(
      Morphism.build(
        id = value(MorphismId(idValue)),
        source = source.id,
        target = target.id,
        kind = MorphismKind.Affine3D,
        routeTag = RouteTag.Anatomical,
        cost = 1.0,
        inverse = Inverse.Exact("analytic"),
        coordinateMap = value(CoordinateMap.affine3D(matrix))
      )
    )

  private def graph(domains: Vector[Domain], morphisms: Vector[Morphism]): SpatialGraph =
    value(SpatialGraph.build(domains, morphisms))

  private def compile(graph: SpatialGraph, request: CompileRequest): SpatialOperator =
    value(OperatorCompiler.compile(graph, request))

  test("identity law reports identity operator behavior"):
    val epi = domain("epi", Vector(3, 1, 1))
    val operator = compile(graph(Vector(epi), Vector.empty), CompileRequest(epi.id, epi.id, sampling = SamplingPolicy.Nearest))
    val probe = DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0), Vector(5.0, 6.0)))

    val check = value(SpatialQc.identityLaw(operator, probe))
    assert(check.passed)
    assertEqualsDouble(check.maxAbsError, 0.0, 1e-12)

  test("composition law matches direct and composed affine paths"):
    val source = domain("source", Vector(4, 1, 1))
    val mid = domain("mid", Vector(4, 1, 1))
    val target = domain("target", Vector(2, 1, 1))

    val directGraph = graph(Vector(source, target), Vector(affine("source-to-target", source, target, translation(0.75, 0.0, 0.0))))
    val composedGraph =
      graph(
        Vector(source, mid, target),
        Vector(
          affine("source-to-mid", source, mid, translation(0.25, 0.0, 0.0)),
          affine("mid-to-target", mid, target, translation(0.5, 0.0, 0.0))
        )
      )

    val direct = compile(directGraph, CompileRequest(source.id, target.id))
    val composed = compile(composedGraph, CompileRequest(source.id, target.id))
    val probe = DoubleMatrix.fromRows(Vector(Vector(2.0), Vector(4.0), Vector(8.0), Vector(16.0)))

    val check = value(SpatialQc.compositionLaw(direct, composed, probe))
    assert(check.passed)
    assertEqualsDouble(check.maxAbsError, 0.0, 1e-12)

  test("adjoint law checks dot(Px, y) equals dot(x, P^T y)"):
    val source = domain("source", Vector(3, 1, 1))
    val target = domain("target", Vector(2, 1, 1))
    val morphism = affine("source-to-target", source, target, translation(0.25, 0.0, 0.0))
    val operator = compile(graph(Vector(source, target), Vector(morphism)), CompileRequest(source.id, target.id))
    val sourceProbe = DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(3.0, 5.0), Vector(7.0, 11.0)))
    val targetProbe = DoubleMatrix.fromRows(Vector(Vector(13.0, 17.0), Vector(19.0, 23.0)))

    val check = value(SpatialQc.adjointLaw(operator, sourceProbe, targetProbe))
    assert(check.passed)
    assertEqualsDouble(check.maxAbsError, 0.0, 1e-12)

  test("ROI restriction law preserves selected target rows"):
    val source = domain("source", Vector(3, 1, 1))
    val target = domain("target", Vector(3, 1, 1))
    val morphism = affine("source-to-target", source, target, DMat.eye(4))
    val g = graph(Vector(source, target), Vector(morphism))
    val full = compile(g, CompileRequest(source.id, target.id, sampling = SamplingPolicy.Nearest))
    val restricted = compile(g, CompileRequest(source.id, target.id, sampling = SamplingPolicy.Nearest, roi = Some(Vector(2, 0))))
    val probe = DoubleMatrix.fromRows(Vector(Vector(1.0, 10.0), Vector(2.0, 20.0), Vector(3.0, 30.0)))

    val check = value(SpatialQc.roiRestrictionLaw(full, restricted, Vector(2, 0), probe))
    assert(check.passed)
    assertEquals(linValue(restricted.forward(probe)).toRows, Vector(Vector(3.0, 30.0), Vector(1.0, 10.0)))

  test("coverage law reports invalid target samples explicitly"):
    val source = domain("source", Vector(2, 1, 1))
    val target = domain("target", Vector(3, 1, 1))
    val morphism = affine("source-to-target", source, target, DMat.eye(4))
    val operator = compile(graph(Vector(source, target), Vector(morphism)), CompileRequest(source.id, target.id, sampling = SamplingPolicy.Nearest))

    val check = SpatialQc.coverageLaw(operator, Vector(1.0, 1.0, 0.0))
    assert(check.passed)
    assertEquals(operator.qc.coverage.uncoveredTargetRows, Vector(2))

  test("one-based neurofunctor-style triplet fixtures compare against zero-based operators"):
    val epi = domain("epi", Vector(2, 1, 1))
    val operator = compile(graph(Vector(epi), Vector.empty), CompileRequest(epi.id, epi.id, sampling = SamplingPolicy.Nearest))
    val fixture =
      value(
        TripletFixture.oneBased(
          rows = 2,
          cols = 2,
          rowIndices = Vector(1, 2),
          colIndices = Vector(1, 2),
          values = Vector(1.0, 1.0)
        )
      )

    val check = value(SpatialQc.tripletFixtureLaw(operator, fixture))
    assert(check.passed)
    assertEquals(fixture.rowIndices, Vector(0, 1))
