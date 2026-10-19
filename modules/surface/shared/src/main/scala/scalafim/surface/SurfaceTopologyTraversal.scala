package scalafim.surface

private[surface] object SurfaceTopologyTraversal:
  def connectedComponents(topology: MeshTopology, active: Set[Int]): Vector[Vector[Int]] =
    if active.isEmpty then Vector.empty
    else
      val owner = topology.mesh.topology
      val visited = scala.collection.mutable.Set.empty[Int]
      val components = Vector.newBuilder[Vector[Int]]

      active.toVector.sorted.foreach: start =>
        if !visited(start) then
          val queue = scala.collection.mutable.Queue.empty[Int]
          val component = Vector.newBuilder[Int]
          visited += start
          queue.enqueue(start)
          val visit: owner.VertexIndex => Unit = neighbor =>
            val index = neighbor.ordinal
            if active(index) && !visited(index) then
              visited += index
              queue.enqueue(index)

          while queue.nonEmpty do
            val vertex = queue.dequeue()
            component += vertex
            owner.foreachNeighbor(owner.vertices.indexAtValidatedOrdinal(vertex))(visit)

          components += component.result().sorted

      components.result()
