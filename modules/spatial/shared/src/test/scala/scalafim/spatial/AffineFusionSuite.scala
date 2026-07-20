package scalafim.spatial

import scalafim.image.{DMat, NeuroSpace}
import scalafim.linalg.{CsrMatrix, DoubleMatrix, LinearMapError}

class AffineFusionSuite extends munit.FunSuite:

  private def value[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def linearValue[A](result: Either[LinearMapError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def domain(name: String): Domain =
    val id = value(DomainId(name))
    val subject = value(SubjectId("sub-01"))
    val modality = value(Modality(name))
    val geometry =
      value(SamplingGeometry.volume(NeuroSpace(Vector(4, 1, 1), trans = Some(DMat.eye(4)))))
    value(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def translation(x: Double): DMat =
    DMat.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, x),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def affine(
    name: String,
    source: Domain,
    target: Domain,
    x: Double
  ): Morphism =
    value(
      Morphism.build(
        id = value(MorphismId(name)),
        source = source.id,
        target = target.id,
        kind = MorphismKind.Affine3D,
        routeTag = RouteTag.Anatomical,
        inverse = Inverse.Exact("analytic"),
        coordinateMap = value(CoordinateMap.affine3D(translation(x)))
      )
    )

  private def graph(domains: Vector[Domain], morphisms: Vector[Morphism]): SpatialGraph =
    value(SpatialGraph.build(domains, morphisms))

  private def sampled(operator: SpatialOperator, input: DoubleMatrix): DoubleMatrix =
    linearValue(operator.forward(input))

  test("an affine route samples the root once and agrees with an independent direct composite"):
    val root = domain("root")
    val mid = domain("mid")
    val target = domain("target")
    val first = affine("root-mid-half", root, mid, 0.5)
    val second = affine("mid-target-half", mid, target, 0.5)
    val direct = affine("root-target-full", root, target, 1.0)
    val chainedGraph = graph(Vector(root, mid, target), Vector(first, second))
    val directGraph = graph(Vector(root, target), Vector(direct))
    val rootValues = DoubleMatrix.fromRows(Vector(Vector(0.0), Vector(8.0), Vector(0.0), Vector(4.0)))

    val fused =
      value(OperatorCompiler.compile(chainedGraph, CompileRequest(root.id, target.id)))
    val directComposite =
      value(OperatorCompiler.compile(directGraph, CompileRequest(root.id, target.id)))
    val fusedValues = sampled(fused, rootValues)
    val directValues = sampled(directComposite, rootValues)

    assertEquals(fused.rows, 4)
    assertEquals(fused.cols, 4)
    assertEquals(fused.provenance.path.map(_.value), Vector("root-mid-half", "mid-target-half"))
    assertEquals(fused.provenance.compiler, "affine-pullback-fused-v1")
    assertEquals(fusedValues.copyData.toVector, Vector(8.0, 0.0, 4.0, 0.0))
    assertEquals(fusedValues.copyData.toVector, directValues.copyData.toVector)

    fused.map match
      case csr: CsrMatrix =>
        val triplets = csr.toTriplets
        assertEquals(triplets.rowIndices.toVector, Vector(0, 1, 2))
        assertEquals(triplets.colIndices.toVector, Vector(1, 2, 3))
        assertEquals(triplets.values.toVector, Vector(1.0, 1.0, 1.0))
      case other =>
        fail(s"expected fused CSR operator, got ${other.getClass.getName}")

  test("fused root sampling is observably different from sequential double interpolation"):
    val root = domain("root")
    val mid = domain("mid")
    val target = domain("target")
    val first = affine("root-mid-half", root, mid, 0.5)
    val second = affine("mid-target-half", mid, target, 0.5)
    val g = graph(Vector(root, mid, target), Vector(first, second))
    val rootValues = DoubleMatrix.fromRows(Vector(Vector(0.0), Vector(8.0), Vector(0.0), Vector(4.0)))

    val fused = value(OperatorCompiler.compile(g, CompileRequest(root.id, target.id)))
    val firstOperator = value(OperatorCompiler.compile(g, CompileRequest(root.id, mid.id)))
    val secondOperator = value(OperatorCompiler.compile(g, CompileRequest(mid.id, target.id)))
    val fusedValues = sampled(fused, rootValues).copyData.toVector
    val sequentialValues = sampled(secondOperator, sampled(firstOperator, rootValues)).copyData.toVector

    assertEquals(fusedValues, Vector(8.0, 0.0, 4.0, 0.0))
    assertEquals(sequentialValues, Vector(4.0, 3.0, 3.0, 4.0))
    assertNotEquals(fusedValues, sequentialValues)

  test("ROI order, boundary coverage, policies, and original route survive fusion"):
    val root = domain("root")
    val mid = domain("mid")
    val target = domain("target")
    val first = affine("root-mid-half", root, mid, 0.5)
    val second = affine("mid-target-half", mid, target, 0.5)
    val g = graph(Vector(root, mid, target), Vector(first, second))
    val request =
      CompileRequest(
        source = root.id,
        target = target.id,
        routing = RoutingPolicy.Anatomical,
        sampling = SamplingPolicy.Trilinear,
        roi = Some(Vector(3, 0))
      )
    val operator = value(OperatorCompiler.compile(g, request))
    val values =
      sampled(
        operator,
        DoubleMatrix.fromRows(Vector(Vector(0.0), Vector(8.0), Vector(0.0), Vector(4.0)))
      )

    assertEquals(operator.qc.coverage.targetRows, Vector(3, 0))
    assertEquals(operator.qc.coverage.rowCoverage, Vector(0.0, 1.0))
    assertEquals(values.copyData.toVector, Vector(0.0, 8.0))
    assertEquals(operator.provenance.routing, RoutingPolicy.Anatomical)
    assertEquals(operator.provenance.sampling, SamplingPolicy.Trilinear)
    assertEquals(operator.provenance.rowSelection, RowSelection.Rows(Vector(3, 0)))
    assertEquals(operator.provenance.path.map(_.value), Vector("root-mid-half", "mid-target-half"))
