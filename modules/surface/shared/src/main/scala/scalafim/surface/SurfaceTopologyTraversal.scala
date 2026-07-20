package scalafim.surface

private[surface] object SurfaceTopologyTraversal:
  def connectedComponents(topology: MeshTopology, active: Set[Int]): Vector[Vector[Int]] =
    if active.isEmpty then Vector.empty
    else
      val visited = scala.collection.mutable.Set.empty[Int]
      val components = Vector.newBuilder[Vector[Int]]

      active.toVector.sorted.foreach: start =>
        if !visited(start) then
          val queue = scala.collection.mutable.Queue.empty[Int]
          val component = Vector.newBuilder[Int]
          visited += start
          queue.enqueue(start)

          while queue.nonEmpty do
            val vertex = queue.dequeue()
            component += vertex
            topology.neighborsOf(VertexId.unsafe(vertex)).foreach: neighbor =>
              val index = neighbor.index
              if active(index) && !visited(index) then
                visited += index
                queue.enqueue(index)

          components += component.result().sorted

      components.result()
