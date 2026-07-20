package scalafim.connectivity

import gale.linalg.{DMat, Matrix}

class ConnectivityMatrixSuite extends munit.FunSuite:

  private val axis =
    NodeAxis.generated(3).toOption.get

  test("ConnectivityMatrix enforces square and symmetric undirected inputs") {
    val undirected = EdgeSpace.undirected(axis).toOption.get
    val symmetric = GaleTestMatrix.fromRows(Vector(
      Vector(1.0, 0.2, 0.3),
      Vector(0.2, 1.0, 0.4),
      Vector(0.3, 0.4, 1.0)
    ))
    val asymmetric = symmetric.updated(0, 2, 0.9)

    assert(ConnectivityMatrix.from(symmetric, undirected).isRight)
    assert(ConnectivityMatrix.from(symmetric, undirected, measure = ConnectivityMeasure.correlation, diagonalPolicy = DiagonalPolicy.Unit).isRight)
    assert(ConnectivityMatrix.from(symmetric.updated(0, 0, 0.0), undirected, measure = ConnectivityMeasure.correlation, diagonalPolicy = DiagonalPolicy.Unit).isLeft)
    assert(ConnectivityMatrix.from(asymmetric, undirected).swap.toOption.exists(_.message.contains("not symmetric")))
    assert(ConnectivityMatrix.from(asymmetric, undirected, symmetryTolerance = Double.NaN).swap.toOption.exists(_.message.contains("symmetry tolerance")))
    assert(ConnectivityMatrix.from(asymmetric, undirected, symmetryTolerance = -1.0).isLeft)
    assert(ConnectivityMatrix.from(Matrix.zeros(3, 2), undirected).isLeft)
  }

  test("directed and rectangular matrices accept non-symmetric shapes appropriate to their spaces") {
    val directed = EdgeSpace.directed(axis).toOption.get
    val rect = EdgeSpace.rectangular(NodeAxis.generated(2, "seed").toOption.get, axis).toOption.get
    val directedMatrix = GaleTestMatrix.fromRows(Vector(
      Vector(0.0, 1.0, 2.0),
      Vector(3.0, 0.0, 4.0),
      Vector(5.0, 6.0, 0.0)
    ))
    val rectMatrix = GaleTestMatrix.fromRows(Vector(
      Vector(1.0, 2.0, 3.0),
      Vector(4.0, 5.0, 6.0)
    ))

    assert(ConnectivityMatrix.from(directedMatrix, directed).isRight)
    assert(ConnectivityMatrix.from(rectMatrix, rect).isRight)
    assert(ConnectivityMatrix.from(rectMatrix, rect).toOption.exists(_.diagonalPolicy == DiagonalPolicy.NotApplicable))
    assert(ConnectivityMatrix.from(rectMatrix, rect, measure = ConnectivityMeasure.correlation).isRight)
    assert(ConnectivityMatrix.from(rectMatrix, rect, measure = ConnectivityMeasure.precision).isLeft)
  }

  test("ConnectivityMatrix copies validated inputs instead of borrowing source storage") {
    val undirected = EdgeSpace.undirected(axis).toOption.get
    val source = GaleTestMatrix.fromRows(Vector(
      Vector(1.0, 0.2, 0.3),
      Vector(0.2, 1.0, 0.4),
      Vector(0.3, 0.4, 1.0)
    ))
    val matrix = ConnectivityMatrix.from(source, undirected).toOption.get

    assert(!(matrix.values eq source))
    assertEqualsDouble(matrix.values(0, 1), 0.2, 1e-12)
  }

  test("DynamicConnectivity requires every slice to share windows and edge ordering") {
    val scalaSpace = EdgeSpace.undirected(axis).toOption.get
    val ariadneSpace = EdgeSpace.undirected(axis, VectorizationOrder.AriadneCompatible).toOption.get
    val matrix = ConnectivityMatrix.fromEdgeVector(EdgeVector.from(scalaSpace, Vector(1.0, 2.0, 3.0)).toOption.get).toOption.get
    val same = StaticConnectivity(matrix)
    val other = StaticConnectivity(ConnectivityMatrix.fromEdgeVector(EdgeVector.from(ariadneSpace, Vector(1.0, 2.0, 3.0)).toOption.get).toOption.get)
    val dynamic = DynamicConnectivity.from(Vector(DynamicSlice.unsafe(0, same, length = 4), DynamicSlice.unsafe(2, same, length = 4))).toOption.get

    assertEquals(dynamic.windowAxis.windows.map(_.description), Vector("0:4", "2:6"))
    assert(DynamicConnectivity.from(Vector(DynamicSlice.unsafe(0, same), DynamicSlice.unsafe(0, same))).isLeft)
    assert(DynamicConnectivity.from(Vector(DynamicSlice.unsafe(1, same), DynamicSlice.unsafe(0, same))).isLeft)
    assert(DynamicConnectivity.from(Vector(DynamicSlice.unsafe(0, same), DynamicSlice.unsafe(1, other))).isLeft)
  }

  test("edge ordering includes scientific node-axis provenance") {
    val firstAxis = NodeAxis.from(axis.nodes, NodeAxisProvenance.unsafeDeclared("atlas", Some("1"))).toOption.get
    val revisedAxis = NodeAxis.from(axis.nodes, NodeAxisProvenance.unsafeDeclared("atlas", Some("2"))).toOption.get
    val first = EdgeSpace.undirected(firstAxis).toOption.get
    val revised = EdgeSpace.undirected(revisedAxis).toOption.get

    assert(firstAxis.sameIdentityAs(revisedAxis))
    assert(!firstAxis.sameScientificBasisAs(revisedAxis))
    assert(!first.sameOrderingAs(revised))
  }

  test("ConnectivitySet aligns subjects, typed covariates, and edge spaces") {
    val space = EdgeSpace.undirected(axis).toOption.get
    val otherSpace = EdgeSpace.directed(axis).toOption.get
    val static = StaticConnectivity(ConnectivityMatrix.fromEdgeVector(EdgeVector.from(space, Vector(1.0, 2.0, 3.0)).toOption.get).toOption.get)
    val otherStatic = StaticConnectivity(ConnectivityMatrix.fromEdgeVector(EdgeVector.from(otherSpace, Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)).toOption.get).toOption.get)
    val s1 = SubjectConnectivity(SubjectId.unsafe("sub-1"), static)
    val s2 = SubjectConnectivity(SubjectId.unsafe("sub-2"), static)
    val bad = SubjectConnectivity(SubjectId.unsafe("sub-3"), otherStatic)
    val group = CovariateName.unsafe("group")
    val age = CovariateName.unsafe("age")
    val cov1 = SubjectCovariates.unsafe(s1.subjectId, Map(
      group -> CovariateValue.categorical("a").toOption.get,
      age -> CovariateValue.continuous(30.0).toOption.get
    ))
    val cov2 = SubjectCovariates.unsafe(s2.subjectId, Map(
      group -> CovariateValue.categorical("b").toOption.get,
      age -> CovariateValue.continuous(31.0).toOption.get
    ))
    val unknown = SubjectCovariates.unsafe(SubjectId.unsafe("sub-99"), Map(group -> CovariateValue.categorical("z").toOption.get))

    assertEquals(ConnectivitySet.from(Vector(s1, s2), Vector(cov1, cov2)).toOption.map(_.subjectCount), Some(2))
    assert(ConnectivitySet.from(Vector(s1, s2), Vector(cov1)).isLeft)
    assert(ConnectivitySet.from(Vector(s1, s2), Vector(cov1, cov1)).isLeft)
    assert(ConnectivitySet.from(Vector(s1, s2), Vector(cov1, unknown)).isLeft)
    assert(ConnectivitySet.from(Vector(s1, s1), Vector(cov1, cov2)).isLeft)
    assert(ConnectivitySet.from(Vector(s1, bad), Vector.empty).isLeft)
  }
