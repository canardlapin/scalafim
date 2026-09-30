package scalafim.surface.fixtures

import scalafim.surface.TriangleMesh

/** Closed icospheres for analytic surface tests. */
object SphereMeshes:
  /** Icosahedron subdivided `levels` times, projected onto the sphere of `radius` about `center`. Outward-wound. */
  def icosphere(levels: Int, radius: Double, center: (Double, Double, Double) = (0.0, 0.0, 0.0)): TriangleMesh =
    val t = (1.0 + math.sqrt(5.0)) / 2.0
    var vertices = Vector(
      (-1.0, t, 0.0), (1.0, t, 0.0), (-1.0, -t, 0.0), (1.0, -t, 0.0), (0.0, -1.0, t), (0.0, 1.0, t),
      (0.0, -1.0, -t), (0.0, 1.0, -t), (t, 0.0, -1.0), (t, 0.0, 1.0), (-t, 0.0, -1.0), (-t, 0.0, 1.0)
    )
    var faces = Vector(
      (0, 11, 5), (0, 5, 1), (0, 1, 7), (0, 7, 10), (0, 10, 11), (1, 5, 9), (5, 11, 4), (11, 10, 2), (10, 7, 6), (7, 1, 8),
      (3, 9, 4), (3, 4, 2), (3, 2, 6), (3, 6, 8), (3, 8, 9), (4, 9, 5), (2, 4, 11), (6, 2, 10), (8, 6, 7), (9, 8, 1)
    )
    (0 until levels).foreach: _ =>
      val midpoints = scala.collection.mutable.Map.empty[(Int, Int), Int]
      val buffer = scala.collection.mutable.ArrayBuffer.from(vertices)
      def mid(a: Int, b: Int): Int =
        midpoints.getOrElseUpdate((math.min(a, b), math.max(a, b)), {
          val (p, q) = (buffer(a), buffer(b))
          buffer += (((p._1 + q._1) / 2, (p._2 + q._2) / 2, (p._3 + q._3) / 2))
          buffer.size - 1
        })
      faces = faces.flatMap: (a, b, c) =>
        val (ab, bc, ca) = (mid(a, b), mid(b, c), mid(c, a))
        Vector((a, ab, ca), (b, bc, ab), (c, ca, bc), (ab, bc, ca))
      vertices = buffer.toVector
    val placed = vertices.map: (x, y, z) =>
      val n = math.sqrt(x * x + y * y + z * z)
      Vector(center._1 + x / n * radius, center._2 + y / n * radius, center._3 + z / n * radius)
    TriangleMesh.fromRows(placed, faces)
