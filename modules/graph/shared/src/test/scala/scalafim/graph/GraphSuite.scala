package scalafim.graph

class GraphSuite extends munit.FunSuite:
  private val basis =
    VertexBasis.from(Vector("a" -> "A", "b" -> "B", "c" -> "C", "d" -> "D")).toOption.get

  test("undirected construction canonicalizes endpoints and input order"):
    val first = Graph.undirected(basis, Vector(
      ("d", "b", 4),
      ("c", "a", 2),
      ("a", "b", 1)
    )).toOption.get
    val second = Graph.undirected(basis, Vector(
      ("b", "a", 1),
      ("a", "c", 2),
      ("b", "d", 4)
    )).toOption.get

    assertEquals(first, second)
    assertEquals(first.hashCode, second.hashCode)
    assertEquals(
      first.edges.map(_.endpoints.vertices).map { case (a, b) => (a.toInt, b.toInt) },
      Vector((0, 1), (0, 2), (1, 3))
    )

  test("all input permutations and endpoint orientations produce one undirected value"):
    val canonical = Vector(
      ("a", "b", 1),
      ("a", "d", 2),
      ("b", "c", 3),
      ("c", "d", 4)
    )
    val expected = Graph.undirected(basis, canonical).toOption.get

    canonical.permutations.foreach: permutation =>
      val oriented = permutation.zipWithIndex.map { case ((from, to, value), index) =>
        if index % 2 == 0 then (to, from, value) else (from, to, value)
      }
      assertEquals(Graph.undirected(basis, oriented).toOption.get, expected)

  test("edge payloads participate in graph value equality"):
    val left = Graph.directed(basis, Vector(("a", "b", 1))).toOption.get
    val right = Graph.directed(basis, Vector(("a", "b", 2))).toOption.get

    assertNotEquals(left, right)

  test("directed construction preserves reverse arcs as distinct edges"):
    val graph = Graph.directed(basis, Vector(
      ("b", "a", "ba"),
      ("a", "b", "ab")
    )).toOption.get

    assertEquals(graph.size, 2)
    assertEquals(graph.edgeBetween("a", "b").map(_.value), Some("ab"))
    assertEquals(graph.edgeBetween("b", "a").map(_.value), Some("ba"))
    assertEquals(graph.outgoingEdges(VertexIx.unsafe(0)).map(_.value), Vector("ab"))
    assertEquals(graph.incomingEdges(VertexIx.unsafe(0)).map(_.value), Vector("ba"))

  test("simple loopless construction accumulates unknown loop and duplicate errors"):
    val result = Graph.undirected(basis, Vector(
      ("missing", "a", 0),
      ("b", "b", 1),
      ("a", "c", 2),
      ("c", "a", 3)
    ))

    val errors = result.left.toOption.get.errors
    assert(errors.exists {
      case GraphBuildError.UnknownVertex(0, EndpointRole.Source, "missing") => true
      case _                                                                => false
    })
    assert(errors.exists {
      case GraphBuildError.SelfEdge(1, "b") => true
      case _                                  => false
    })
    assert(errors.exists {
      case GraphBuildError.DuplicateEdge(3, "a", "c") => true
      case _                                            => false
    })

  test("undirected incidence caches expose every edge at both endpoints"):
    val graph = Graph.undirected(basis, Vector(
      ("a", "b", 1),
      ("a", "c", 2),
      ("b", "d", 3)
    )).toOption.get

    assertEquals(graph.outgoingEdges(VertexIx.unsafe(0)).map(_.value), Vector(1, 2))
    assertEquals(graph.incomingEdges(VertexIx.unsafe(0)).map(_.value), Vector(1, 2))
    assertEquals(graph.outgoingEdges(VertexIx.unsafe(3)).map(_.value), Vector(3))
    assert(graph.containsEdge("c", "a"))
    assert(!graph.containsEdge("c", "d"))

  test("identity metadata and edge mappings preserve graph values"):
    val graph = Graph.undirected(basis, Vector(("a", "b", 1), ("c", "d", 2))).toOption.get

    assertEquals(graph.mapVertexMetadata((_, value) => value), graph)
    assertEquals(graph.mapEdgeValues(identity), graph)

  test("metadata mapping preserves keys and topology but changes value equality"):
    val graph = Graph.directed(basis, Vector(("a", "b", 1), ("b", "c", 2))).toOption.get
    val mapped = graph.mapVertexMetadata((key, value) => s"$key:$value")

    assert(graph.basis.sameKeyOrderAs(mapped.basis))
    assertEquals(mapped.edges, graph.edges)
    assertNotEquals(mapped, graph)

  test("induced graphs preserve source order and obey nested intersection"):
    val graph = Graph.undirected(basis, Vector(
      ("a", "b", 1),
      ("a", "c", 2),
      ("b", "c", 3),
      ("c", "d", 4)
    )).toOption.get
    val all = graph.inducedByKeys(basis.keys).toOption.get.graph
    val first = graph.inducedByKeys(Vector("d", "b", "c")).toOption.get.graph
    val nested = first.inducedByKeys(Vector("c", "b")).toOption.get.graph
    val direct = graph.inducedByKeys(Vector("b", "c")).toOption.get.graph

    assertEquals(all, graph)
    assertEquals(first.basis.keys, Vector("b", "c", "d"))
    assertEquals(nested, direct)
    assertEquals(direct.edges.map(_.value), Vector(3))

  test("reindexing by a permutation and its inverse restores the graph"):
    val graph = Graph.directed(basis, Vector(
      ("a", "b", "ab"),
      ("d", "a", "da"),
      ("c", "b", "cb")
    )).toOption.get
    val reordered = graph.reindex(Vector("d", "b", "a", "c")).toOption.get.graph
    val restored = reordered.reindex(basis.keys).toOption.get.graph

    assertEquals(reordered.basis.keys, Vector("d", "b", "a", "c"))
    assertNotEquals(reordered, graph)
    assertEquals(restored, graph)

  test("empty graphs preserve an empty or nonempty vertex basis"):
    val edgeless = Graph.undirected(basis, Vector.empty[(String, String, Int)]).toOption.get
    val emptyBasis = VertexBasis.from(Vector.empty[(String, Unit)]).toOption.get
    val empty = Graph.directed(emptyBasis, Vector.empty[(String, String, Int)]).toOption.get

    assertEquals(edgeless.order, 4)
    assertEquals(edgeless.size, 0)
    assertEquals(empty.order, 0)
    assertEquals(empty.size, 0)
