package scalafim.spatial

import image4s.geometry.{Affine, D3}
import scalafim.image.{SampleSpaces, SpatialPoint}
import scalafim.image.world.{SubjectId, TemplateName}

/** neurofunctor QC and operator parity: round trips, commutativity, projection metrics, field backprojection and
  * hybrid assembly (`compile_to_hybrid`, `compile_from_hybrid`, block diagonal).
  */
class OperatorParitySuite extends munit.FunSuite:

  private def value[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def apiValue[A](result: Either[FieldApiError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def linValue[A](result: Either[LinearMapError, A]): A =
    result.fold(error => fail(error.getMessage), identity)

  private def domain(name: String, dims: Vector[Int] = Vector(4, 1, 1)): Domain =
    val subject = value(SubjectId("sub-01").asSpatial)
    val geometry = value(SamplingGeometry.volume(SampleSpaces(dims, affine = Some(ProviderAffines.identity))))
    value(Domain.build(value(DomainId(name)), SpaceRef.Volume(subject, None, value(Modality(name))), geometry))

  private def translation(x: Double, y: Double = 0.0, z: Double = 0.0): Affine[D3] =
    ProviderAffines.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, x),
        Vector(0.0, 1.0, 0.0, y),
        Vector(0.0, 0.0, 1.0, z),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  /** An affine edge whose pullback (target world to source world) is `pullback`. */
  private def affine(name: String, source: Domain, target: Domain, pullback: Affine[D3], cost: Double = 1.0): Morphism =
    value(
      Morphism.build(
        value(MorphismId(name)),
        source.id,
        target.id,
        MorphismKind.Affine3D,
        RouteTag.Anatomical,
        cost,
        Inverse.Exact("analytic"),
        value(CoordinateMap.affine(source, target, pullback))
      )
    )

  private val probes = Vector(SpatialPoint(0.0, 0.0, 0.0), SpatialPoint(1.5, -2.0, 3.0), SpatialPoint(10.0, 4.0, -1.0))

  private def dense(matrix: DoubleMatrix): Vector[Vector[Double]] =
    Vector.tabulate(matrix.rows)(row => Vector.tabulate(matrix.cols)(col => matrix(row, col)))

  private def column(values: Double*): DoubleMatrix =
    DoubleMatrix.fromRows(values.map(Vector(_)))

  test("round trips through exact inverses return every probe"):
    val a = domain("rt-a")
    val b = domain("rt-b")
    val graph = value(SpatialGraph.build(Vector(a, b), Vector(affine("a-b", a, b, translation(1.0, 2.0, -3.0)))))
    val trip = value(SpatialQc.roundTrip(graph, a.id, b.id, probes))
    assertEquals(trip.probes, 3)
    assertEquals(trip.first.map(_.value), Vector("a-b", "a-b:inverse"))
    assert(trip.usedInverses)
    assertEqualsDouble(trip.maxDistance, 0.0, 1e-12)
    assert(trip.check().passed)

  test("round trips expose an explicit reverse edge that does not invert the forward one"):
    val a = domain("rt2-a")
    val b = domain("rt2-b")
    val graph =
      value(
        SpatialGraph.build(
          Vector(a, b),
          Vector(affine("a-b", a, b, translation(1.0)), affine("b-a", b, a, translation(0.5)))
        )
      )
    val trip = value(SpatialQc.roundTrip(graph, a.id, b.id, probes))
    assert(!trip.usedInverses, clue = "the explicit reverse edge is preferred over the inverse")
    assertEqualsDouble(trip.maxDistance, 1.5, 1e-12)
    assertEqualsDouble(trip.rmsDistance, 1.5, 1e-12)
    assert(!trip.check().passed)
    assertEquals(SpatialQc.roundTrip(graph, a.id, b.id, Vector.empty).left.toOption, Some(SpatialError.EmptyQcProbes))

  test("commutes compares two routes between the same domains"):
    val a = domain("cm-a")
    val b = domain("cm-b")
    val c = domain("cm-c")
    val ab = affine("a-b", a, b, translation(1.0))
    val bc = affine("b-c", b, c, translation(0.0, 2.0))
    val direct = affine("a-c", a, c, translation(1.0, 2.0), cost = 5.0)
    val skewed = affine("a-c-skewed", a, c, translation(1.0, 2.25), cost = 6.0)
    val graph = value(SpatialGraph.build(Vector(a, b, c), Vector(ab, bc, direct, skewed)))

    val composed = value(graph.path(a.id, c.id))
    assertEquals(composed.ids.map(_.value), Vector("a-b", "b-c"))
    val directPath = value(MorphismPath.build(Vector(direct)))
    val agreement = value(SpatialQc.commutes(composed, directPath, probes))
    assertEqualsDouble(agreement.maxDistance, 0.0, 1e-12)
    assertEquals(agreement.second.map(_.value), Vector("a-c"))

    val all = value(SpatialQc.commutativity(graph, a.id, c.id, probes))
    assertEquals(all.map(_.second.map(_.value)), Vector(Vector("a-c"), Vector("a-c-skewed")))
    assertEqualsDouble(all(0).maxDistance, 0.0, 1e-12)
    assertEqualsDouble(all(1).maxDistance, 0.25, 1e-12)
    assertEquals(all.map(_.check().passed), Vector(true, false))

    val elsewhere = value(MorphismPath.build(Vector(ab)))
    assertEquals(
      SpatialQc.commutes(composed, elsewhere, probes).left.toOption,
      Some(SpatialError.PathEndpointsMismatch(a.id, c.id, a.id, b.id))
    )

  test("projection metrics report row sums and weight sanity"):
    val a = domain("pm-a")
    val b = domain("pm-b")
    // pullback shifts target points by +2.5 voxels: two target rows fall off the source grid
    val graph = value(SpatialGraph.build(Vector(a, b), Vector(affine("a-b", a, b, translation(2.5)))))
    val operator = value(OperatorCompiler.compile(graph, CompileRequest(a.id, b.id)))
    val metrics = value(SpatialQc.projectionMetrics(operator))

    assertEquals(metrics.targetRows, 4)
    assertEquals(metrics.sourceColumns, 4)
    assertEquals(metrics.rowSums.map(sum => math.round(sum * 1e9) / 1e9), Vector(1.0, 1.0, 0.0, 0.0))
    assertEqualsDouble(metrics.rowSumSummary.mean, 0.5, 1e-12)
    assertEqualsDouble(metrics.rowSumSummary.fractionNearOne, 0.5, 1e-12)
    assertEqualsDouble(metrics.coverage, 0.5, 1e-12)
    metrics.weights match
      case WeightInspection.Stored(summary) =>
        assertEquals(summary.entries, 3)
        assert(!summary.anyNegative)
        assert(!summary.anyNonFinite)
        assertEqualsDouble(summary.max.get, 1.0, 1e-12)
      case other => fail(s"expected stored weights, got $other")
    assertEqualsDouble(metrics.nnzPerRow.get, 0.75, 1e-12)
    assert(metrics.rowSumCheck().passed)
    assert(!metrics.rowSumCheck(coveredOnly = false).passed)
    assert(metrics.weightChecks.forall(_.passed))

  test("backproject applies the adjoint of the view's operator and keeps its provenance"):
    val a = domain("bp-a")
    val b = domain("bp-b")
    given graph: SpatialGraph = value(SpatialGraph.build(Vector(a, b), Vector(affine("a-b", a, b, translation(0.5)))))
    val root = Field.fromMatrix(a.id, DoubleMatrix.zeros(a.nElements, 2), "bold")
    val view = apiValue(root.to(b))
    val data = DoubleMatrix.fromRows(Vector(Vector(1.0, 0.0), Vector(2.0, 1.0), Vector(3.0, 0.0), Vector(4.0, 1.0)))

    val back = apiValue(view.backproject(data))
    val operator = value(OperatorCompiler.compile(graph, CompileRequest(a.id, b.id)))
    val expected = linValue(operator.map.transposeApplyTo(data))
    assertEquals(back.field.domain, a.id)
    assertEquals(back.field.sampleCount, a.nElements)
    assertEquals(back.operator.provenance.path.map(_.value), Vector("a-b"))
    assert(!back.operator.provenance.usedInverses)
    val actualRows = dense(back.data)
    val expectedRows = dense(expected)
    actualRows.flatten.zip(expectedRows.flatten).foreach((actual, wanted) => assertEqualsDouble(actual, wanted, 1e-12))

    // adjoint law against the forward view: <P x, y> == <x, P^T y>
    val x = DoubleMatrix.fromRows(Vector(Vector(1.0, -1.0), Vector(0.5, 2.0), Vector(-3.0, 0.0), Vector(2.0, 1.0)))
    val px = linValue(operator.map.applyTo(x))
    val lhs = dense(px).flatten.zip(dense(data).flatten).map(_ * _).sum
    val rhs = dense(x).flatten.zip(actualRows.flatten).map(_ * _).sum
    assertEqualsDouble(lhs, rhs, 1e-10)

  test("backproject rejects root fields and data of the wrong height"):
    val a = domain("bpr-a")
    val b = domain("bpr-b")
    given SpatialGraph = value(SpatialGraph.build(Vector(a, b), Vector(affine("a-b", a, b, translation(0.0)))))
    val root = Field.fromMatrix(a.id, DoubleMatrix.zeros(a.nElements, 1), "bold")
    assertEquals(root.backproject(column(1.0, 1.0, 1.0, 1.0)).left.toOption, Some(FieldApiError.Spatial(SpatialError.FieldIsRoot(a.id))))
    val view = apiValue(root.to(b))
    assertEquals(view.backproject(column(1.0)).left.toOption, Some(FieldApiError.Spatial(SpatialError.FieldShapeMismatch(4, 1))))

  test("backproject follows the inverse route a view was planned with"):
    val a = domain("bpi-a")
    val b = domain("bpi-b")
    given SpatialGraph = value(SpatialGraph.build(Vector(a, b), Vector(affine("a-b", a, b, translation(1.0)))))
    val root = Field.fromMatrix(b.id, DoubleMatrix.zeros(b.nElements, 1), "bold")
    val view = apiValue(root.to(a, allowInverses = true))
    val back = apiValue(view.backproject(column(1.0, 1.0, 1.0, 1.0)))
    assert(back.operator.provenance.usedInverses)
    assertEquals(back.operator.path.ids.map(_.value), Vector("a-b:inverse"))

  private def hybrid(name: String, parts: Vector[(String, Domain)]): Domain =
    val geometry = value(SamplingGeometry.hybrid(parts.map((part, domain) => value(PartName(part)) -> domain)))
    value(Domain.build(value(DomainId(name)), SpaceRef.Template(value(TemplateName(name).asSpatial), None, TemplateKind.Hybrid), geometry))

  private final case class HybridFixture(
    graph: SpatialGraph,
    source: Domain,
    left: Domain,
    right: Domain,
    leftSource: Domain,
    rightSource: Domain
  )

  private def hybridFixture(): HybridFixture =
    val source = domain("hy-source", Vector(3, 1, 1))
    val left = domain("hy-left", Vector(2, 1, 1))
    val right = domain("hy-right", Vector(3, 1, 1))
    val leftSource = domain("hy-left-source", Vector(2, 1, 1))
    val rightSource = domain("hy-right-source", Vector(3, 1, 1))
    val graph =
      value(
        SpatialGraph.build(
          Vector(source, left, right, leftSource, rightSource),
          Vector(
            affine("source-left", source, left, translation(0.5)),
            affine("source-right", source, right, translation(1.0)),
            affine("left-source-left", leftSource, left, translation(0.25)),
            affine("right-source-right", rightSource, right, translation(0.0)),
            affine("right-source-left", rightSource, left, translation(0.0))
          )
        )
      )
    HybridFixture(graph, source, left, right, leftSource, rightSource)

  test("compile to a hybrid target stacks the per-part rows in part order"):
    val f = hybridFixture()
    val target = hybrid("hy-target", Vector("left" -> f.left, "right" -> f.right))
    val op = value(HybridOperator.toHybrid(f.graph, f.source.id, target))
    assertEquals(op.layout, HybridLayout.ToHybrid)
    assertEquals((op.rows, op.cols), (5, 3))
    assertEquals(op.parts.map(part => (part.name.value, part.offset)), Vector("left" -> 0, "right" -> 2))
    assertEquals(op.path.map(_.value), Vector("source-left", "source-right"))

    val x = column(1.0, 2.0, 4.0)
    val stacked = dense(linValue(op.forward(x))).flatten
    val leftPart = dense(linValue(op.parts(0).operator.forward(x))).flatten
    val rightPart = dense(linValue(op.parts(1).operator.forward(x))).flatten
    stacked.zip(leftPart ++ rightPart).foreach((actual, expected) => assertEqualsDouble(actual, expected, 1e-12))

    val y = column(1.0, -1.0, 2.0, 0.5, 3.0)
    val lhs = stacked.zip(dense(y).flatten).map(_ * _).sum
    val rhs = dense(x).flatten.zip(dense(linValue(op.adjoint(y))).flatten).map(_ * _).sum
    assertEqualsDouble(lhs, rhs, 1e-10)

    val metrics = value(SpatialQc.projectionMetrics(op))
    assertEquals(metrics.targetRows, 5)
    metrics.weights match
      case WeightInspection.Stored(summary) => assert(!summary.anyNegative)
      case other => fail(s"expected stored part weights, got $other")

  test("compile from a hybrid source concatenates per-part columns"):
    val f = hybridFixture()
    val source = hybrid("hy-source-hybrid", Vector("left" -> f.leftSource, "right" -> f.rightSource))
    val op = value(HybridOperator.fromHybrid(f.graph, source, f.left.id))
    assertEquals(op.layout, HybridLayout.FromHybrid)
    assertEquals((op.rows, op.cols), (2, 5))
    assertEquals(op.parts.map(part => (part.name.value, part.offset)), Vector("left" -> 0, "right" -> 2))

    val out = dense(linValue(op.forward(column(1.0, 2.0, 3.0, 4.0, 5.0)))).flatten
    val leftPart = dense(linValue(op.parts(0).operator.forward(column(1.0, 2.0)))).flatten
    val rightPart = dense(linValue(op.parts(1).operator.forward(column(3.0, 4.0, 5.0)))).flatten
    out.zip(leftPart.zip(rightPart).map(_ + _)).foreach((actual, expected) => assertEqualsDouble(actual, expected, 1e-12))
    assertEqualsDouble(op.meanPartCoverage, op.parts.map(_.operator.qc.coverage.fraction).sum / 2.0, 1e-12)

    val unreachable = hybrid("hy-unreachable", Vector("left" -> f.leftSource))
    assertEquals(
      HybridOperator.fromHybrid(f.graph, unreachable, f.right.id).left.toOption,
      Some(SpatialError.NoPath(f.leftSource.id, f.right.id))
    )

  test("block-diagonal hybrid compile maps part i to part i"):
    val f = hybridFixture()
    val source = hybrid("hy-bd-source", Vector("left" -> f.leftSource, "right" -> f.rightSource))
    val target = hybrid("hy-bd-target", Vector("left" -> f.left, "right" -> f.right))
    val op = value(HybridOperator.blockDiagonal(f.graph, source, target))
    assertEquals(op.layout, HybridLayout.BlockDiagonal)
    assertEquals((op.rows, op.cols), (5, 5))

    val x = column(1.0, 2.0, 3.0, 4.0, 5.0)
    val out = dense(linValue(op.forward(x))).flatten
    val leftPart = dense(linValue(op.parts(0).operator.forward(column(1.0, 2.0)))).flatten
    val rightPart = dense(linValue(op.parts(1).operator.forward(column(3.0, 4.0, 5.0)))).flatten
    out.zip(leftPart ++ rightPart).foreach((actual, expected) => assertEqualsDouble(actual, expected, 1e-12))

    val swapped = hybrid("hy-bd-swapped", Vector("right" -> f.right, "left" -> f.left))
    assert(
      HybridOperator.blockDiagonal(f.graph, source, swapped).left.exists {
        case SpatialError.HybridLayoutMismatch(_) => true
        case _ => false
      }
    )
    assert(HybridOperator.toHybrid(f.graph, f.source.id, f.left).left.exists {
      case SpatialError.HybridLayoutMismatch(_) => true
      case _ => false
    })

  test("commutativity evaluates non-commuting steps in route order"):
    val a = domain("nc-a")
    val b = domain("nc-b")
    val c = domain("nc-c")
    val scale =
      ProviderAffines.fromRows(
        Vector(
          Vector(2.0, 0.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    // pullbacks: c -> b translates by +1, b -> a scales x by 2; the route pullback at p is scale(translate(p)) = 2(x+1)
    val ab = affine("a-b", a, b, scale)
    val bc = affine("b-c", b, c, translation(1.0))
    val right = affine("a-c-right", a, c, ProviderAffines.fromRows(Vector(Vector(2.0, 0.0, 0.0, 2.0), Vector(0.0, 1.0, 0.0, 0.0), Vector(0.0, 0.0, 1.0, 0.0), Vector(0.0, 0.0, 0.0, 1.0))), cost = 5.0)
    val swapped = affine("a-c-swapped", a, c, ProviderAffines.fromRows(Vector(Vector(2.0, 0.0, 0.0, 1.0), Vector(0.0, 1.0, 0.0, 0.0), Vector(0.0, 0.0, 1.0, 0.0), Vector(0.0, 0.0, 0.0, 1.0))), cost = 6.0)
    val graph = value(SpatialGraph.build(Vector(a, b, c), Vector(ab, bc, right, swapped)))

    val composed = value(graph.path(a.id, c.id))
    assertEquals(value(composed.pullback(SpatialPoint(3.0, 0.0, 0.0))), SpatialPoint(8.0, 0.0, 0.0))
    val differences = value(SpatialQc.commutativity(graph, a.id, c.id, probes))
    assertEquals(differences.map(_.second.map(_.value)), Vector(Vector("a-c-right"), Vector("a-c-swapped")))
    assertEqualsDouble(differences(0).maxDistance, 0.0, 1e-12)
    assertEqualsDouble(differences(1).maxDistance, 1.0, 1e-12)
