package scalafim.surface

import narr.nArray2NArr
import scalafim.image.NArrayUtil

final case class SurfaceThreshold(low: Double, high: Double):
  require(low <= high, "threshold low must be <= high")

  def keep(value: Double): Boolean =
    value <= low || value >= high

final case class SurfaceComponentResult(
  index: SurfaceField[Int],
  size: SurfaceField[Int]
)

object SurfaceComponents:

  def connectedComponents(
    field: SurfaceField[Double],
    threshold: SurfaceThreshold
  ): SurfaceComponentResult =
    val topology = MeshTopology.from(field.geometry.mesh)
    val active = scala.collection.mutable.Set.empty[Int]

    var i = 0
    while i < field.size do
      if threshold.keep(field.data(i)) then active += field.indices(i)
      i += 1

    val components = collectComponents(topology, active.toSet)
      .sortBy(vertices => (-vertices.length, vertices.min))

    val componentIndex = scala.collection.mutable.Map.empty[Int, Int]
    val componentSize = scala.collection.mutable.Map.empty[Int, Int]

    components.zipWithIndex.foreach { case (vertices, idx) =>
      val id = idx + 1
      val size = vertices.length
      vertices.foreach { vertex =>
        componentIndex(vertex) = id
        componentSize(vertex) = size
      }
    }

    val indices = copyIndices(field)
    val indexData = Array.ofDim[Int](field.size)
    val sizeData = Array.ofDim[Int](field.size)

    i = 0
    while i < field.size do
      val vertex = field.indices(i)
      indexData(i) = componentIndex.getOrElse(vertex, 0)
      sizeData(i) = componentSize.getOrElse(vertex, 0)
      i += 1

    SurfaceComponentResult(
      index = SurfaceField(field.geometry, NArrayUtil.fromArray(indices), NArrayUtil.fromArray(indexData), field.label),
      size = SurfaceField(field.geometry, NArrayUtil.fromArray(indices), NArrayUtil.fromArray(sizeData), field.label)
    )

  def clusterThreshold(
    field: SurfaceField[Double],
    threshold: SurfaceThreshold,
    minSize: Int,
    fill: Double = 0.0
  ): SurfaceField[Double] =
    require(minSize > 0, "minSize must be positive")
    val components = connectedComponents(field, threshold)
    val indices = copyIndices(field)
    val out = Array.ofDim[Double](field.size)

    var i = 0
    while i < field.size do
      out(i) =
        if components.size.data(i) >= minSize then field.data(i) else fill
      i += 1

    SurfaceField(field.geometry, NArrayUtil.fromArray(indices), NArrayUtil.fromArray(out), field.label)

  private def collectComponents(topology: MeshTopology, active: Set[Int]): Vector[Vector[Int]] =
    if active.isEmpty then Vector.empty
    else
      val visited = scala.collection.mutable.Set.empty[Int]
      val components = Vector.newBuilder[Vector[Int]]

      active.toVector.sorted.foreach { start =>
        if !visited(start) then
          val queue = scala.collection.mutable.Queue.empty[Int]
          val component = Vector.newBuilder[Int]
          visited += start
          queue.enqueue(start)

          while queue.nonEmpty do
            val vertex = queue.dequeue()
            component += vertex
            topology.neighborsOf(VertexId.unsafe(vertex)).foreach { neighbor =>
              val n = neighbor.index
              if active(n) && !visited(n) then
                visited += n
                queue.enqueue(n)
            }

          components += component.result().sorted
      }

      components.result()

  private def copyIndices[A](field: SurfaceField[A]): Array[Int] =
    Array.tabulate(field.size)(i => field.indices(i))
