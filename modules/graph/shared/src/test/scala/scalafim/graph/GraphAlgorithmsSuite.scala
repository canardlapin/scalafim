package scalafim.graph

import scala.util.Random

class GraphAlgorithmsSuite extends munit.FunSuite:
  private def intBasis(size: Int): VertexBasis[Int, Unit] =
    VertexBasis.from((0 until size).map(_ -> ())).toOption.get

  private given EdgeCost[Double] with
    def cost(edge: Double): Double = edge

  test("undirected components partition every vertex in basis order"):
    val graph = Graph.undirected(intBasis(7), Vector(
      (0, 2, ()),
      (2, 4, ()),
      (1, 3, ())
    )).toOption.get

    assertEquals(graph.connectedComponents.map(_.keys), Vector(
      Vector(0, 2, 4),
      Vector(1, 3),
      Vector(5),
      Vector(6)
    ))
    assert(!graph.isConnected)
    assert(Graph.undirected(intBasis(1), Vector.empty[(Int, Int, Unit)]).toOption.get.isConnected)
    assert(!Graph.undirected(intBasis(0), Vector.empty[(Int, Int, Unit)]).toOption.get.isConnected)

  test("randomized undirected components match an independent union-find oracle"):
    var seed = 0
    while seed < 40 do
      val random = new Random(seed)
      val size = 1 + random.nextInt(10)
      val parent = Array.tabulate(size)(identity)

      def find(start: Int): Int =
        var root = start
        while parent(root) != root do root = parent(root)
        var current = start
        while parent(current) != root do
          val next = parent(current)
          parent(current) = root
          current = next
        root

      def union(left: Int, right: Int): Unit =
        val a = find(left)
        val b = find(right)
        if a != b then parent(Math.max(a, b)) = Math.min(a, b)

      val edges = Vector.newBuilder[(Int, Int, Unit)]
      var left = 0
      while left < size do
        var right = left + 1
        while right < size do
          if random.nextDouble() < 0.3 then
            edges += ((left, right, ()))
            union(left, right)
          right += 1
        left += 1

      val expected = (0 until size).groupBy(find).values.map(_.toVector.sorted).toVector.sortBy(_.head)
      val graph = Graph.undirected(intBasis(size), edges.result()).toOption.get
      assertEquals(graph.connectedComponents.map(_.keys), expected, s"seed=$seed")
      seed += 1

  test("directed weak and strong components have distinct semantics"):
    val graph = Graph.directed(intBasis(6), Vector(
      (0, 1, ()),
      (1, 0, ()),
      (1, 2, ()),
      (2, 3, ()),
      (3, 2, ()),
      (3, 4, ())
    )).toOption.get

    assertEquals(graph.weaklyConnectedComponents.map(_.keys), Vector(Vector(0, 1, 2, 3, 4), Vector(5)))
    assertEquals(graph.stronglyConnectedComponents.map(_.keys), Vector(Vector(0, 1), Vector(2, 3), Vector(4), Vector(5)))

  test("randomized strong components match mutual transitive reachability"):
    var seed = 0
    while seed < 30 do
      val random = new Random(2000L + seed)
      val size = 1 + random.nextInt(8)
      val reach = Array.fill(size * size)(false)
      val edges = Vector.newBuilder[(Int, Int, Unit)]
      var from = 0
      while from < size do
        reach(from * size + from) = true
        var to = 0
        while to < size do
          if from != to && random.nextDouble() < 0.25 then
            edges += ((from, to, ()))
            reach(from * size + to) = true
          to += 1
        from += 1

      var through = 0
      while through < size do
        from = 0
        while from < size do
          var to = 0
          while to < size do
            if reach(from * size + through) && reach(through * size + to) then reach(from * size + to) = true
            to += 1
          from += 1
        through += 1

      val assigned = Array.fill(size)(false)
      val expected = Vector.newBuilder[Vector[Int]]
      var start = 0
      while start < size do
        if !assigned(start) then
          val members = (0 until size).filter: vertex =>
            reach(start * size + vertex) && reach(vertex * size + start)
          members.foreach(vertex => assigned(vertex) = true)
          expected += members.toVector
        start += 1

      val graph = Graph.directed(intBasis(size), edges.result()).toOption.get
      assertEquals(graph.stronglyConnectedComponents.map(_.keys), expected.result(), s"seed=$seed")
      seed += 1

  test("shortest paths use edge costs and reconstruct vertices and payloads"):
    val basis = VertexBasis.from(Vector("a" -> (), "b" -> (), "c" -> (), "d" -> ())).toOption.get
    val graph = Graph.undirected(basis, Vector(
      ("a", "b", 1.0),
      ("b", "c", 2.0),
      ("a", "c", 10.0),
      ("c", "d", 0.5)
    )).toOption.get
    val forward = graph.shortestPath("a", "d").toOption.get
    val reverse = graph.shortestPath("d", "a").toOption.get

    assertEquals(forward.vertices, Vector("a", "b", "c", "d"))
    assertEquals(forward.edges.map(_.value), Vector(1.0, 2.0, 0.5))
    assertEqualsDouble(forward.totalCost, 3.5, 1e-12)
    assertEquals(reverse.vertices, forward.vertices.reverse)
    assertEqualsDouble(reverse.totalCost, 3.5, 1e-12)
    val identityPath = graph.shortestPath("b", "b").toOption.get
    assertEquals(identityPath.vertices, Vector("b"))
    assertEquals(identityPath.edges, Vector.empty)
    assertEqualsDouble(identityPath.totalCost, 0.0, 1e-12)

  test("directed shortest paths respect arc orientation"):
    val graph = Graph.directed(intBasis(3), Vector((0, 1, 1.0), (1, 2, 1.0))).toOption.get

    assertEquals(graph.shortestPath(0, 2).toOption.get.vertices, Vector(0, 1, 2))
    assertEquals(graph.shortestPath(2, 0).left.toOption, Some(PathError.NoPath(2, 0)))

  test("shortest paths reject unknown vertices and invalid graph-wide costs"):
    val graph = Graph.undirected(intBasis(3), Vector((0, 1, 1.0), (1, 2, Double.NaN))).toOption.get

    assertEquals(graph.shortestPath(9, 0).left.toOption, Some(PathError.UnknownVertex(9)))
    graph.shortestPath(0, 1) match
      case Left(PathError.InvalidEdgeCost(1, 2, value)) => assert(value.isNaN)
      case other                                         => fail(s"unexpected result: $other")

    val negative = Graph.undirected(intBasis(2), Vector((0, 1, -1.0))).toOption.get
    negative.shortestPath(0, 1) match
      case Left(PathError.InvalidEdgeCost(0, 1, -1.0)) => ()
      case other                                        => fail(s"unexpected result: $other")

  test("validated Distance supplies a nonnegative EdgeCost capability"):
    assertEquals(Distance.from(-0.1).left.toOption, Some(DistanceError.InvalidValue(-0.1)))
    assertEquals(
      Distance.from(Double.PositiveInfinity).left.toOption,
      Some(DistanceError.InvalidValue(Double.PositiveInfinity))
    )
    val graph = Graph.undirected(intBasis(2), Vector((0, 1, Distance.unsafe(2.5)))).toOption.get

    assertEqualsDouble(graph.shortestPath(0, 1).toOption.get.totalCost, 2.5, 1e-12)

  test("Dijkstra matches Floyd-Warshall on deterministic random graphs"):
    var seed = 0
    while seed < 30 do
      val random = new Random(1000L + seed)
      val size = 2 + random.nextInt(6)
      val distances = Array.fill(size * size)(Double.PositiveInfinity)
      var i = 0
      while i < size do
        distances(i * size + i) = 0.0
        i += 1

      val edges = Vector.newBuilder[(Int, Int, Double)]
      var left = 0
      while left < size do
        var right = left + 1
        while right < size do
          if random.nextDouble() < 0.45 then
            val cost = 0.25 + random.nextInt(20).toDouble / 4.0
            edges += ((left, right, cost))
            distances(left * size + right) = cost
            distances(right * size + left) = cost
          right += 1
        left += 1

      var through = 0
      while through < size do
        var from = 0
        while from < size do
          var to = 0
          while to < size do
            val candidate = distances(from * size + through) + distances(through * size + to)
            if candidate < distances(from * size + to) then distances(from * size + to) = candidate
            to += 1
          from += 1
        through += 1

      val graph = Graph.undirected(intBasis(size), edges.result()).toOption.get
      var from = 0
      while from < size do
        var to = 0
        while to < size do
          val expected = distances(from * size + to)
          graph.shortestPath(from, to) match
            case Right(path) => assertEqualsDouble(path.totalCost, expected, 1e-10, s"seed=$seed from=$from to=$to")
            case Left(PathError.NoPath(_, _)) => assert(expected.isPosInfinity, s"seed=$seed from=$from to=$to")
            case other => fail(s"seed=$seed from=$from to=$to unexpected result: $other")
          to += 1
        from += 1
      seed += 1

  test("cycle witnesses consist entirely of present directed edges"):
    val basis = VertexBasis.fromKeys(Vector("a", "b", "c", "d")).toOption.get
    val graph = Graph.directed(basis, Vector(
      ("a", "b", ()),
      ("b", "c", ()),
      ("c", "a", ()),
      ("c", "d", ())
    )).toOption.get
    val cycle = graph.findCycle.get

    assertEquals(cycle.keys.head, cycle.keys.last)
    cycle.keys.sliding(2).foreach:
      case Vector(from, to) => assert(graph.containsEdge(from, to), s"missing witness edge $from -> $to")
      case other            => fail(s"invalid witness window: $other")
    assert(Dag.from(graph).isLeft)
    assert(graph.topologicalOrder.isLeft)

  test("DAG layers are deterministic in basis order and respect every edge"):
    val basis = VertexBasis.fromKeys(Vector("a", "b", "c", "d", "e")).toOption.get
    val graph = Graph.directed(basis, Vector(
      ("a", "c", ()),
      ("b", "c", ()),
      ("c", "d", ()),
      ("b", "e", ())
    )).toOption.get
    val dag = Dag.from(graph).toOption.get
    val layers = dag.topologicalLayers
    val position = dag.topologicalOrder.zipWithIndex.toMap

    assertEquals(layers, Vector(Vector("a", "b"), Vector("c", "e"), Vector("d")))
    graph.edges.foreach: edge =>
      val from = graph.basis.keyAt(edge.endpoints.first)
      val to = graph.basis.keyAt(edge.endpoints.second)
      assert(position(from) < position(to), s"topological order violated by $from -> $to")

  test("empty directed graphs are valid DAGs with no layers"):
    val graph = Graph.directed(intBasis(0), Vector.empty[(Int, Int, Unit)]).toOption.get
    val dag = Dag.from(graph).toOption.get

    assertEquals(dag.topologicalOrder, Vector.empty)
    assertEquals(dag.topologicalLayers, Vector.empty)
    assertEquals(graph.findCycle, None)
    assertEquals(graph.stronglyConnectedComponents, Vector.empty)
