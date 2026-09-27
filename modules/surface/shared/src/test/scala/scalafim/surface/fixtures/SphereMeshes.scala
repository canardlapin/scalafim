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

  /** A closed latitude-longitude sphere about the origin: two poles and `rings` rings of `segments` vertices, so
    * `rings * segments + 2` vertices and `2 * rings * segments` faces (171 x 190 gives fsLR 32k's 32492 and 64980).
    * `tilt` rotates it about the x axis, so its poles need not sit on another test mesh's vertices.
    */
  def uvSphere(rings: Int, segments: Int, radius: Double, tilt: Double = 0.0): TriangleMesh =
    require(rings >= 1 && segments >= 3, "a uv sphere needs at least one ring of three segments")
    val (ct, st) = (math.cos(tilt), math.sin(tilt))
    def place(x: Double, y: Double, z: Double): Vector[Double] = Vector(radius * x, radius * (ct * y - st * z), radius * (st * y + ct * z))
    val ringVertices =
      for
        r <- 1 to rings
        s <- 0 until segments
      yield
        val (theta, phi) = (math.Pi * r / (rings + 1), 2.0 * math.Pi * (s + 0.5 * (r % 2)) / segments)
        place(math.sin(theta) * math.cos(phi), math.sin(theta) * math.sin(phi), math.cos(theta))
    val south = rings * segments + 1
    val vertices = (place(0.0, 0.0, 1.0) +: ringVertices.toVector) :+ place(0.0, 0.0, -1.0)
    def v(r: Int, s: Int): Int = 1 + (r - 1) * segments + Math.floorMod(s, segments)
    val cap = (0 until segments).map(s => (0, v(1, s), v(1, s + 1)))
    val bands =
      for
        r <- 1 until rings
        s <- 0 until segments
        face <- Vector((v(r, s), v(r + 1, s), v(r, s + 1)), (v(r, s + 1), v(r + 1, s), v(r + 1, s + 1)))
      yield face
    val bottom = (0 until segments).map(s => (south, v(rings, s + 1), v(rings, s)))
    TriangleMesh.fromRows(vertices, (cap ++ bands ++ bottom).toVector)
