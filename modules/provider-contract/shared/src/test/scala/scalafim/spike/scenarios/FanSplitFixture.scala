package scalafim.spike.scenarios

import neurosurf4s.surface.TriangleMesh

/** Explicit fixture-only cut of disconnected vertex fans in a retained face
  * subset. Every oriented face, corner coordinate and original face row stays
  * unchanged. Copies have distinct local ordinals and an explicit lineage;
  * original source IDs are not unique identities in this derivative domain.
  *
  * This preserves pointwise sampling and forward interpolation on fields
  * constant over copies. It does not preserve geodesic, smoothing, area,
  * mass-normalization, inverse or adjoint semantics.
  */
private[scenarios] final class FanSplitFixture private (
  val mesh: TriangleMesh,
  val sourceRows: Vector[Int],
  private val originalFaces: Vector[Vector[Int]]
):
  def expand(rows: Array[Array[Double]]): Array[Array[Double]] =
    sourceRows.map(row => rows(row).clone()).toArray

  /** Winning oriented triples in the independent oracle name a unique
    * original face. Retaining its row and corner order remaps each coefficient
    * without changing it: P_split E = P_original for every original field.
    */
  def remapPlan(rows: Array[Array[Double]]): Array[Array[Double]] =
    val faceRows = originalFaces.zipWithIndex.toMap
    require(faceRows.size == originalFaces.size, "oracle faces must be unique")
    rows.map: original =>
      val face = faceRows(original.take(3).map(_.toInt).toVector)
      val result = original.clone()
      var corner = 0
      while corner < 3 do
        val mapped = mesh.faceIndices(3 * face + corner)
        require(sourceRows(mapped) == original(corner).toInt, "fan split changed an oracle coefficient's source")
        result(corner) = mapped.toDouble
        corner += 1
      result

private[scenarios] object FanSplitFixture:
  def apply(coordinates: Array[Double], faceIndices: Array[Int]): FanSplitFixture =
    val vertexCount = coordinates.length / 3
    require(coordinates.length % 3 == 0 && faceIndices.length % 3 == 0)
    val faces = faceIndices.grouped(3).map(_.toVector).toVector
    val incident = Array.fill(vertexCount)(scala.collection.mutable.ArrayBuffer.empty[Int])
    faces.zipWithIndex.foreach: (face, index) =>
      require(face.distinct.size == 3 && face.forall(v => v >= 0 && v < vertexCount))
      face.foreach(vertex => incident(vertex) += index)
    val sources = scala.collection.mutable.ArrayBuffer.from(0 until vertexCount)
    val split = faceIndices.clone()
    var vertex = 0
    while vertex < vertexCount do
      val remaining = scala.collection.mutable.Set.from(incident(vertex))
      var firstFan = true
      while remaining.nonEmpty do
        val seed = remaining.min
        val queue = scala.collection.mutable.Queue(seed)
        val fan = scala.collection.mutable.ArrayBuffer.empty[Int]
        remaining -= seed
        while queue.nonEmpty do
          val current = queue.dequeue()
          fan += current
          val adjacent = remaining.toVector.sorted.filter: other =>
            faces(current).exists(v => v != vertex && faces(other).contains(v))
          adjacent.foreach: other =>
            remaining -= other
            queue.enqueue(other)
        val mapped =
          if firstFan then vertex
          else
            sources += vertex
            sources.size - 1
        fan.foreach: face =>
          val corner = faces(face).indexOf(vertex)
          split(3 * face + corner) = mapped
        firstFan = false
      vertex += 1
    val lineage = sources.toVector
    val values = lineage.flatMap(row => coordinates.slice(3 * row, 3 * row + 3)).toArray
    new FanSplitFixture(TriangleMesh.fromArrays(values, split), lineage, faces)
