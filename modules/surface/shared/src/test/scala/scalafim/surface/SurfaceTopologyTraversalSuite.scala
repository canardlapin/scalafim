package scalafim.surface

class SurfaceTopologyTraversalSuite extends munit.FunSuite:

  test("active components agree with edge unions for every mask on a fragmented mesh"):
    val mesh = TriangleMesh.fromRows(
      Vector(
        Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0), Vector(1.0, 1.0, 0.0),
        Vector(3.0, 0.0, 0.0), Vector(4.0, 0.0, 0.0), Vector(3.0, 1.0, 0.0)
      ),
      Vector((2, 0, 1), (2, 1, 3), (6, 4, 5))
    )
    val topology = MeshTopology.from(mesh)
    val edges = Vector(0 -> 1, 0 -> 2, 1 -> 2, 1 -> 3, 2 -> 3, 4 -> 5, 4 -> 6, 5 -> 6)

    for mask <- 0 until (1 << mesh.vertexCount) do
      val active = (0 until mesh.vertexCount).filter(i => (mask & (1 << i)) != 0).toSet
      val parents = Array.tabulate(mesh.vertexCount)(identity)
      def root(vertex: Int): Int =
        var current = vertex
        while parents(current) != current do current = parents(current)
        current

      edges.foreach: (a, b) =>
        if active(a) && active(b) then parents(root(b)) = root(a)
      val expected = active.toVector.groupBy(root).values.map(_.sorted).toVector.sortBy(_.head)

      assertEquals(SurfaceTopologyTraversal.connectedComponents(topology, active), expected, s"mask=$mask")
