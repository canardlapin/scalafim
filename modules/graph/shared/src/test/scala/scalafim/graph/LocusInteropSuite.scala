package scalafim.graph

import scalafim.locus.*

class LocusInteropSuite extends munit.FunSuite:
  private val basis =
    VertexBasis.from(Vector("a" -> "A", "b" -> "B", "c" -> "C")).toOption.get
  private val locus =
    VertexLocus.make(SpaceKey.unsafe("graph:test-basis"), basis)

  test("a vertex locus reuses basis order, keys, and metadata"):
    assertEquals(locus.space.size, basis.size)
    assert(locus.basis eq basis)
    assertEquals(locus.pointOf(basis.indexOf("b").get).ordinal, 1)
    assertEquals(locus.vertexOf(locus.space.point(2).get).toInt, 2)

  test("directed graph conversion makes edge-value filtering explicit"):
    val graph =
      Graph.directed(
        basis,
        Vector(("a", "b", 1), ("b", "c", 0), ("c", "a", 2))
      ).toOption.get
    val relation = GraphRelation.fromDirected(locus, graph)(_ > 0).toOption.get

    assertEquals(relation.ordinalRows.map(_.toVector).toVector, Vector(Vector(1), Vector(), Vector(0)))

    val restored =
      GraphRelation.toDirected(locus)(relation, RelationLoopPolicy.Reject): (from, to) =>
        s"$from->$to"
    assertEquals(restored.toOption.get.edgeBetween("a", "b").map(_.value), Some("a->b"))
    assertEquals(restored.toOption.get.edgeBetween("c", "a").map(_.value), Some("c->a"))
    assert(!restored.toOption.get.containsEdge("b", "c"))

  test("undirected graph conversion produces validated symmetric evidence"):
    val graph =
      Graph.undirected(basis, Vector(("a", "b", 1), ("b", "c", 2))).toOption.get
    val symmetric = GraphRelation.fromUndirected(locus, graph)(_ => true).toOption.get

    assertEquals(
      symmetric.relation.ordinalRows.map(_.toVector).toVector,
      Vector(Vector(1), Vector(0, 2), Vector(1))
    )

    val restored =
      GraphRelation.toUndirected(locus)(symmetric, RelationLoopPolicy.Reject)((_, _) => ())
        .toOption
        .get
    assert(restored.containsEdge("a", "b"))
    assert(restored.containsEdge("b", "c"))
    assertEquals(restored.size, 2)

  test("identity relation loops require an explicit reject or drop policy"):
    val identity = Relation.identity(locus.space)
    val rejected =
      GraphRelation.toDirected(locus)(identity, RelationLoopPolicy.Reject)((_, _) => ())

    rejected match
      case Left(RelationGraphError.SelfLoops(keys)) =>
        assertEquals(keys, Vector("a", "b", "c"))
      case other =>
        fail(s"expected self-loop rejection, found $other")

    val dropped =
      GraphRelation.toDirected(locus)(identity, RelationLoopPolicy.Drop)((_, _) => ())
        .toOption
        .get
    assertEquals(dropped.order, 3)
    assertEquals(dropped.size, 0)

  test("relation-to-graph conversion checks runtime space identity"):
    val wrongSpace =
      FiniteSpace.make[locus.S](SpaceKey.unsafe("graph:wrong-space"), basis.size).toOption.get
    val wrongRelation = Relation.identity(wrongSpace)

    val result =
      GraphRelation.toDirected(locus)(wrongRelation, RelationLoopPolicy.Drop)((_, _) => ())
    assert(result.isLeft)

  test("graph-to-relation conversion checks the exact keyed basis"):
    val otherBasis =
      VertexBasis.from(Vector("a" -> "A", "c" -> "C", "b" -> "B")).toOption.get
    val graph =
      Graph.directed(otherBasis, Vector(("a", "b", 1))).toOption.get

    assert(GraphRelation.fromDirected(locus, graph)(_ => true).isLeft)
