package scalafim.connectivity

import scalafim.graph.Direction
import scalafim.graph.VertexIx
import scalafim.linalg.DoubleMatrix

class ProjectionSuite extends munit.FunSuite:
  private val ids = Vector("a", "b", "c", "d").map(NodeId.unsafe)
  private val provenance = NodeAxisProvenance.unsafeDeclared("atlas", Some("v1"))
  private val axis = NodeAxis.unsafe(ids.map(id => NodeSpec(id, id.value.toUpperCase)), provenance)

  test("absolute-density selection can preserve signed correlations"):
    val matrix = correlationMatrix(Vector(-0.9, 0.2, 0.7, -0.4, 0.1, -0.8))
    val projection = ConnectivityGraphProjection.undirected(
      EdgeEligibility.All,
      SelectionScore.Absolute,
      EdgeSelection.Density(0.5),
      WeightTransform.preserve,
      zeroPolicy = ZeroEdgePolicy.PreserveSelected
    )
    val projected = projection.project(matrix).toOption.get

    assertEquals(projected.graph.basis.keys, ids)
    assertEquals(
      projected.graph.edges.map(edge => endpointValues(projected, edge.endpoints.first, edge.endpoints.second, edge.value)),
      Vector(("a", "b", -0.9), ("a", "d", 0.7), ("c", "d", -0.8))
    )
    assertEquals(projected.sourceCoordinateByGraphEdge.map(_.value), Vector(0, 2, 5))
    assertEquals(projected.receipt.selection.requestedDensity, Some(0.5))
    assertEquals(projected.receipt.selection.realizedDensity, Some(0.5))
    assertEquals(projected.receipt.weightTransform, "preserve")

  test("density ties are deterministic and the receipt distinguishes exact from inclusive ties"):
    val matrix = correlationMatrix(Vector(0.9, 0.8, 0.8, 0.1, 0.0, -0.1))
    val exact = ConnectivityGraphProjection.undirected(
      EdgeEligibility.All,
      SelectionScore.Absolute,
      EdgeSelection.Density(2.0 / 6.0, TiePolicy.ExactByEdgeSpaceOrder),
      WeightTransform.preserve
    ).project(matrix).toOption.get
    val inclusive = ConnectivityGraphProjection.undirected(
      EdgeEligibility.All,
      SelectionScore.Absolute,
      EdgeSelection.Density(2.0 / 6.0, TiePolicy.IncludeBoundaryTies),
      WeightTransform.preserve
    ).project(matrix).toOption.get

    assertEquals(exact.sourceCoordinateByGraphEdge.map(_.value), Vector(0, 1))
    assertEquals(inclusive.sourceCoordinateByGraphEdge.map(_.value), Vector(0, 1, 2))
    assertEquals(exact.receipt.selection.realizedThreshold, Some(0.8))
    assertEquals(exact.receipt.selection.realizedDensity, Some(2.0 / 6.0))
    assertEquals(inclusive.receipt.selection.realizedDensity, Some(3.0 / 6.0))
    assertEquals(inclusive.receipt.selection.tiePolicy, Some(TiePolicy.IncludeBoundaryTies))

  test("canonical graph order carries an exact Ariadne source-coordinate correspondence"):
    val edgeSpace = EdgeSpace.undirected(axis, VectorizationOrder.AriadneCompatible).toOption.get
    val matrix = matrixFromUndirectedEdgeValues(
      edgeSpace,
      Vector(0.1, 0.2, 0.9, 0.8, 0.3, 0.4),
      ConnectivityMeasure.correlation,
      DiagonalPolicy.Unit
    )
    val mask = EdgeMask.fromIndices(
      edgeSpace,
      Vector(EdgeSpaceIx.from(2, edgeSpace).toOption.get, EdgeSpaceIx.from(3, edgeSpace).toOption.get)
    ).toOption.get
    val projected = ConnectivityGraphProjection.undirected(
      EdgeEligibility.All,
      SelectionScore.Raw,
      EdgeSelection.Mask(mask),
      WeightTransform.preserve,
      zeroPolicy = ZeroEdgePolicy.PreserveSelected
    ).project(matrix).toOption.get

    assertEquals(
      projected.graph.edges.map(edge =>
        projected.graph.basis.keyAt(edge.endpoints.first).value -> projected.graph.basis.keyAt(edge.endpoints.second).value
      ),
      Vector("a" -> "d", "b" -> "c")
    )
    assertEquals(projected.sourceCoordinateByGraphEdge.map(_.value), Vector(3, 2))
    assertEquals(projected.sourceEdgeOfGraphEdge(0).source.value -> projected.sourceEdgeOfGraphEdge(0).target.value, "a" -> "d")
    assertEquals(projected.sourceMask.selectedIndices.map(_.value), Vector(2, 3))
    assertEquals(projected.receipt.sourceVectorizationOrder, VectorizationOrder.AriadneCompatible)
    assertEquals(projected.receipt.selection.requestedMask.map(_.value), Vector(2, 3))

  test("post-transform zero policy controls topology without changing selection policy"):
    val matrix = correlationMatrix(Vector(-0.7, 0.4, -0.2, 0.0, 0.3, -0.1))
    val exclude = ConnectivityGraphProjection.undirected(
      EdgeEligibility.All,
      SelectionScore.Absolute,
      EdgeSelection.AllEligible,
      WeightTransform.positiveMagnitude,
      zeroPolicy = ZeroEdgePolicy.ExcludeAfterTransform
    ).project(matrix).toOption.get
    val preserve = ConnectivityGraphProjection.undirected(
      EdgeEligibility.All,
      SelectionScore.Absolute,
      EdgeSelection.AllEligible,
      WeightTransform.positiveMagnitude,
      zeroPolicy = ZeroEdgePolicy.PreserveSelected
    ).project(matrix).toOption.get

    assertEquals(exclude.graph.size, 2)
    assertEquals(preserve.graph.size, 6)
    assertEquals(preserve.graph.edges.map(_.value), Vector(0.0, 0.4, 0.0, 0.0, 0.3, 0.0))
    assertEquals(exclude.receipt.zeroPolicy, ZeroEdgePolicy.ExcludeAfterTransform)
    assertEquals(exclude.receipt.selectedEdgeCount, 2)

  test("custom transformations produce typed edge payloads"):
    final case class SignedEdge(value: Double)
    val typedTransform = new WeightTransform[SignedEdge]:
      def description: String = "signed-edge"
      def transform(value: Double, measure: ConnectivityMeasure): Either[ConnectivityError, SignedEdge] =
        Right(SignedEdge(value))
      def numericalValue(weight: SignedEdge): Double = weight.value

    val matrix = correlationMatrix(Vector(-0.9, 0.2, 0.7, -0.4, 0.1, -0.8))
    val projected = ConnectivityGraphProjection.undirected(
      EdgeEligibility.NegativeOnly,
      SelectionScore.Absolute,
      EdgeSelection.Threshold(0.5),
      typedTransform,
      zeroPolicy = ZeroEdgePolicy.PreserveSelected
    ).project(matrix).toOption.get

    assertEquals(projected.graph.edges.map(_.value), Vector(SignedEdge(-0.9), SignedEdge(-0.8)))
    assertEquals(projected.receipt.weightTransform, "signed-edge")

  test("zero-weighted-degree receipt vertices use transformed nonzero support"):
    val matrix = correlationMatrix(Vector(0.5, 0.0, 0.0, 0.0, 0.0, 0.0))
    val projected = ConnectivityGraphProjection.undirected(
      EdgeEligibility.All,
      SelectionScore.Raw,
      EdgeSelection.AllEligible,
      WeightTransform.preserve,
      zeroPolicy = ZeroEdgePolicy.PreserveSelected
    ).project(matrix).toOption.get

    assertEquals(projected.receipt.zeroWeightedDegreeVertices.map(_.value), Vector("c", "d"))

  test("distance ordering and RBF output require explicit distance semantics"):
    val edgeSpace = EdgeSpace.undirected(axis).toOption.get
    val distances = matrixFromUndirectedEdgeValues(
      edgeSpace,
      Vector(1.0, 4.0, 2.0, 3.0, 5.0, 6.0),
      ConnectivityMeasure.distance,
      DiagonalPolicy.StructuralZero
    )
    val rbf = WeightTransform.rbfDistance(2.0).toOption.get
    val projected = ConnectivityGraphProjection.undirected(
      EdgeEligibility.All,
      SelectionScore.DistanceAscending,
      EdgeSelection.Threshold(2.0),
      rbf,
      diagonal = DiagonalTreatment.Reject,
      zeroPolicy = ZeroEdgePolicy.PreserveSelected
    ).project(distances).toOption.get

    assertEquals(projected.sourceCoordinateByGraphEdge.map(_.value), Vector(0, 2))
    assertEqualsDouble(projected.graph.edges(0).value, Math.exp(-1.0 / 8.0), 1e-12)
    assertEqualsDouble(projected.graph.edges(1).value, Math.exp(-4.0 / 8.0), 1e-12)

    val correlations = correlationMatrix(Vector.fill(6)(0.2))
    val invalid = ConnectivityGraphProjection.undirected(
      EdgeEligibility.All,
      SelectionScore.DistanceAscending,
      EdgeSelection.AllEligible,
      WeightTransform.preserve
    ).project(correlations)
    assert(invalid.left.toOption.exists(_.message.contains("distance connectivity measure")))

  test("directed projection preserves arc orientation and canonical source indices"):
    val measure = ConnectivityMeasure.external(
      "directed-score",
      ConnectivityValueScale.Similarity,
      symmetric = false
    ).toOption.get
    val edgeSpace = EdgeSpace.directed(axis).toOption.get
    val values = DoubleMatrix.fromRows(
      Vector(
        Vector(0.0, 0.8, 0.0, 0.0),
        Vector(0.1, 0.0, 0.0, 0.0),
        Vector(0.0, 0.7, 0.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0)
      )
    )
    val matrix = ConnectivityMatrix.from(
      values,
      edgeSpace,
      measure = measure,
      diagonalPolicy = DiagonalPolicy.StructuralZero
    ).toOption.get
    val projected = ConnectivityGraphProjection.directed(
      EdgeEligibility.All,
      SelectionScore.Raw,
      EdgeSelection.Threshold(0.5),
      WeightTransform.preserve,
      diagonal = DiagonalTreatment.Reject
    ).project(matrix).toOption.get

    assertEquals(projected.graph.direction, Direction.Directed)
    assert(projected.graph.containsEdge(ids(0), ids(1)))
    assert(projected.graph.containsEdge(ids(2), ids(1)))
    assert(!projected.graph.containsEdge(ids(1), ids(0)))
    assertEquals(projected.sourceCoordinateByGraphEdge.map(_.value), Vector(0, 7))

  test("rectangular matrices and incompatible graph directions are rejected"):
    val target = NodeAxis.unsafe(Vector(NodeSpec(NodeId.unsafe("x"), "X"), NodeSpec(NodeId.unsafe("y"), "Y")))
    val rectangularSpace = EdgeSpace.rectangular(axis, target).toOption.get
    val rectangular = ConnectivityMatrix.from(
      DoubleMatrix.fromRows(Vector.fill(4)(Vector(0.1, 0.2))),
      rectangularSpace,
      measure = ConnectivityMeasure.edgeWeight
    ).toOption.get
    val projection = ConnectivityGraphProjection.undirected(
      EdgeEligibility.All,
      SelectionScore.Raw,
      EdgeSelection.AllEligible,
      WeightTransform.preserve
    )

    assert(projection.project(rectangular).left.toOption.exists(_.message.contains("rectangular connectivity")))
    val directed = directedZeroMatrix()
    assert(projection.project(directed).left.toOption.exists(_.message.contains("directed connectivity")))

  test("diagonal treatment is explicit and replay is deterministic"):
    val matrix = correlationMatrix(Vector(-0.9, 0.2, 0.7, -0.4, 0.1, -0.8))
    val rejecting = ConnectivityGraphProjection.undirected(
      EdgeEligibility.All,
      SelectionScore.Absolute,
      EdgeSelection.Density(0.5),
      WeightTransform.preserve,
      diagonal = DiagonalTreatment.Reject
    )
    assert(rejecting.project(matrix).left.toOption.exists(_.message.contains("structural-zero")))

    val replayable = ConnectivityGraphProjection.undirected(
      EdgeEligibility.All,
      SelectionScore.Absolute,
      EdgeSelection.Density(0.5, TiePolicy.ExactByEdgeSpaceOrder),
      WeightTransform.preserve,
      diagonal = DiagonalTreatment.ValidateAndIgnore,
      zeroPolicy = ZeroEdgePolicy.PreserveSelected
    )
    val first = replayable.project(matrix).toOption.get
    val second = replayable.project(matrix).toOption.get

    assertEquals(first.graph, second.graph)
    assertEquals(first.receipt, second.receipt)
    assertEquals(first.sourceCoordinateByGraphEdge, second.sourceCoordinateByGraphEdge)
    assertEquals(first.receipt.sourceBasisKeys, ids)
    assertEquals(first.receipt.sourceBasisProvenance, "atlas@v1")
    assertEquals(first.receipt.projectionVersion, ConnectivityGraphProjection.version)

  test("mask compatibility is checked against scientific EdgeSpace ordering"):
    val native = EdgeSpace.undirected(axis, VectorizationOrder.ScalaNative).toOption.get
    val ariadne = EdgeSpace.undirected(axis, VectorizationOrder.AriadneCompatible).toOption.get
    val mask = EdgeMask.from(ariadne, Vector.fill(ariadne.size)(true)).toOption.get
    val matrix = matrixFromUndirectedEdgeValues(
      native,
      Vector.fill(native.size)(0.2),
      ConnectivityMeasure.correlation,
      DiagonalPolicy.Unit
    )
    val result = ConnectivityGraphProjection.undirected(
      EdgeEligibility.All,
      SelectionScore.Raw,
      EdgeSelection.Mask(mask),
      WeightTransform.preserve
    ).project(matrix)

    assert(result.left.toOption.exists(_.message.contains("mask edge space")))

  private def correlationMatrix(edgeValues: Vector[Double]): ConnectivityMatrix =
    val edgeSpace = EdgeSpace.undirected(axis).toOption.get
    matrixFromUndirectedEdgeValues(edgeSpace, edgeValues, ConnectivityMeasure.correlation, DiagonalPolicy.Unit)

  private def matrixFromUndirectedEdgeValues(
      edgeSpace: EdgeSpace,
      edgeValues: Vector[Double],
      measure: ConnectivityMeasure,
      diagonalPolicy: DiagonalPolicy
  ): ConnectivityMatrix =
    val vector = EdgeVector.from(edgeSpace, edgeValues).toOption.get
    ConnectivityMatrix.fromEdgeVector(vector, measure, diagonalPolicy).toOption.get

  private def directedZeroMatrix(): ConnectivityMatrix =
    val edgeSpace = EdgeSpace.directed(axis).toOption.get
    val measure = ConnectivityMeasure.external(
      "directed-zero",
      ConnectivityValueScale.EdgeWeight,
      symmetric = false
    ).toOption.get
    ConnectivityMatrix.from(
      DoubleMatrix.zeros(axis.size, axis.size),
      edgeSpace,
      measure = measure,
      diagonalPolicy = DiagonalPolicy.StructuralZero
    ).toOption.get

  private def endpointValues(
      projected: ProjectedConnectivityGraph[Direction.Undirected.type, Double],
      first: VertexIx,
      second: VertexIx,
      value: Double
  ): (String, String, Double) =
    (
      projected.graph.basis.keyAt(first).value,
      projected.graph.basis.keyAt(second).value,
      value
    )
