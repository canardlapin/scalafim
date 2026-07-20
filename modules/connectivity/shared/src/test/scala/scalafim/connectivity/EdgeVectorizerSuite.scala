package scalafim.connectivity

import gale.linalg.{DMat, Matrix}

class EdgeVectorizerSuite extends munit.FunSuite:

  private val axis4 =
    NodeAxis.fromIdsAndLabels(
      Vector("n1", "n2", "n3", "n4").map(NodeId.unsafe),
      Vector("n1", "n2", "n3", "n4")
    ).toOption.get

  test("ScalaNative undirected order is zero-based row-major lexicographic") {
    val space = EdgeSpace.undirected(axis4).toOption.get

    val pairs = space.edges.map(edge => (edge.sourceIndex, edge.targetIndex))

    assertEquals(pairs, Vector((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)))
  }

  test("AriadneCompatible undirected order is explicit and distinct") {
    val space = EdgeSpace.undirected(axis4, VectorizationOrder.AriadneCompatible).toOption.get

    val pairs = space.edges.map(edge => (edge.sourceIndex, edge.targetIndex))

    assertEquals(pairs, Vector((0, 1), (0, 2), (1, 2), (0, 3), (1, 3), (2, 3)))
  }

  test("directed order defaults to source-major and has Ariadne compatibility mode") {
    val axis3 = NodeAxis.generated(3).toOption.get
    val scalaSpace = EdgeSpace.directed(axis3).toOption.get
    val ariadneSpace = EdgeSpace.directed(axis3, VectorizationOrder.AriadneCompatible).toOption.get

    assertEquals(scalaSpace.edges.map(e => (e.sourceIndex, e.targetIndex)), Vector((0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1)))
    assertEquals(ariadneSpace.edges.map(e => (e.sourceIndex, e.targetIndex)), Vector((1, 0), (2, 0), (0, 1), (2, 1), (0, 2), (1, 2)))
  }

  test("rectangular order defaults to row-major and can match Ariadne column-major") {
    val seeds = NodeAxis.generated(2, "seed").toOption.get
    val rois = NodeAxis.generated(3, "roi").toOption.get
    val scalaSpace = EdgeSpace.rectangular(seeds, rois).toOption.get
    val ariadneSpace = EdgeSpace.rectangular(seeds, rois, VectorizationOrder.AriadneCompatible).toOption.get

    assertEquals(scalaSpace.edges.map(e => (e.sourceIndex, e.targetIndex)), Vector((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2)))
    assertEquals(ariadneSpace.edges.map(e => (e.sourceIndex, e.targetIndex)), Vector((0, 0), (1, 0), (0, 1), (1, 1), (0, 2), (1, 2)))
  }

  test("edge vectors validate length finite values and round-trip through matrices") {
    val space = EdgeSpace.undirected(axis4).toOption.get
    val vector = EdgeVector.from(space, Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)).toOption.get
    val matrix = EdgeVectorizer.toMatrix(vector)

    assertEquals(matrix.toRows, Vector(
      Vector(0.0, 1.0, 2.0, 3.0),
      Vector(1.0, 0.0, 4.0, 5.0),
      Vector(2.0, 4.0, 0.0, 6.0),
      Vector(3.0, 5.0, 6.0, 0.0)
    ))
    assert(EdgeVector.from(space, Vector(1.0)).isLeft)
    assert(EdgeVector.from(space, Vector(1.0, 2.0, Double.PositiveInfinity, 4.0, 5.0, 6.0)).isLeft)
    assertEquals(EdgeVectorizer.fromMatrix(matrix, space).toOption.get.toVector, vector.toVector)
  }

  test("edge vector materialization makes diagonal policy explicit") {
    val space = EdgeSpace.undirected(NodeAxis.generated(3).toOption.get).toOption.get
    val vector = EdgeVector.from(space, Vector(0.1, 0.2, 0.3)).toOption.get
    val zero = EdgeVectorizer.toMatrix(vector)
    val unit = EdgeVectorizer.toMatrix(vector, DiagonalPolicy.Unit)

    assertEqualsDouble(zero.row(0).toVector.sum, 0.3, 1e-12)
    assertEqualsDouble(zero.row(1).toVector.sum, 0.4, 1e-12)
    assertEqualsDouble(zero.row(2).toVector.sum, 0.5, 1e-12)
    assertEquals(unit(0, 0), 1.0)
    assertEquals(unit(1, 1), 1.0)
    assertEquals(unit(2, 2), 1.0)
    assertEquals(ConnectivityMatrix.fromEdgeVector(vector, ConnectivityMeasure.correlation, DiagonalPolicy.Unit).toOption.get.diagonalPolicy, DiagonalPolicy.Unit)
  }

  test("fromMatrix rejects shape mismatches") {
    val space = EdgeSpace.directed(NodeAxis.generated(3).toOption.get).toOption.get
    val bad = Matrix.zeros(3, 2)

    assert(EdgeVectorizer.fromMatrix(bad, space).isLeft)
  }
