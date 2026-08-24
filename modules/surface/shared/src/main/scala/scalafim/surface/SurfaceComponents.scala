package scalafim.surface

import scalafim.image.PrimitiveBuffers

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
    val fieldIndices = field.unsafeIndices
    val fieldData = field.unsafeData

    var i = 0
    while i < field.size do
      if threshold.keep(fieldData(i)) then active += fieldIndices(i)
      i += 1

    val components = SurfaceTopologyTraversal.connectedComponents(topology, active.toSet)
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
      val vertex = fieldIndices(i)
      indexData(i) = componentIndex.getOrElse(vertex, 0)
      sizeData(i) = componentSize.getOrElse(vertex, 0)
      i += 1

    SurfaceComponentResult(
      index = SurfaceField(field.geometry, PrimitiveBuffers.fromArray(indices), PrimitiveBuffers.fromArray(indexData), field.label),
      size = SurfaceField(field.geometry, PrimitiveBuffers.fromArray(indices), PrimitiveBuffers.fromArray(sizeData), field.label)
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
    val componentSizes = components.size.unsafeData
    val fieldData = field.unsafeData

    var i = 0
    while i < field.size do
      out(i) =
        if componentSizes(i) >= minSize then fieldData(i) else fill
      i += 1

    SurfaceField(field.geometry, PrimitiveBuffers.fromArray(indices), PrimitiveBuffers.fromArray(out), field.label)

  private def copyIndices[A](field: SurfaceField[A]): Array[Int] =
    field.unsafeIndices.clone()
