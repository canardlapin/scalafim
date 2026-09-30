package scalafim.surface

class SurfaceResamplingSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  /** Icosahedron subdivided `levels` times, projected to `radius`, optionally rotated about z then x. */
  private def icosphere(levels: Int, radius: Double, rotation: Double = 0.0): TriangleMesh =
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
    val (cz, sz, cx, sx) = (math.cos(rotation), math.sin(rotation), math.cos(0.7 * rotation), math.sin(0.7 * rotation))
    val placed = vertices.map: (x, y, z) =>
      val n = math.sqrt(x * x + y * y + z * z)
      val (x1, y1, z1) = (x / n * radius, y / n * radius, z / n * radius)
      val (x2, y2) = (cz * x1 - sz * y1, sz * x1 + cz * y1)
      Vector(x2, cx * y2 - sx * z1, sx * y2 + cx * z1)
    TriangleMesh.fromRows(placed, faces)

  private val fine = icosphere(3, 87.0)
  private val coarse = icosphere(2, 101.0, rotation = 0.37)

  test("sphere helpers recognise spheres and normalise their radius"):
    assert(SphereMesh.isSphere(fine))
    val scaled = ok(SphereMesh.withRadius(fine, 100.0))
    (0 until scaled.vertexCount).foreach(i => assertEqualsDouble(SphereMesh.radius(scaled.coordinates, i), 100.0, 1e-9))
    val squashed = TriangleMesh.fromArrays(fine.coordinates.zipWithIndex.map((v, i) => if i % 3 == 2 then v * 0.5 else v), fine.faceIndices)
    assert(!SphereMesh.isSphere(squashed))
    assert(SphereMesh.withRadius(squashed).isLeft)

  test("barycentric weights reconstruct a point on the query's own radial ray (never the antipodal face)"):
    val plan = ok(SurfaceResampling.plan(coarse, fine))
    val moving = ok(SphereMesh.withRadius(fine))
    val reference = ok(SphereMesh.withRadius(coarse))
    (0 until reference.vertexCount).foreach: i =>
      val q = Vector(reference.coordinates(3 * i), reference.coordinates(3 * i + 1), reference.coordinates(3 * i + 2))
      val entries = plan.rows.indices.filter(plan.rows(_) == i)
      val hit = Vector.tabulate(3)(k => entries.map(e => plan.vals(e) * moving.coordinates(3 * plan.cols(e) + k)).sum)
      val cross = Vector(q(1) * hit(2) - q(2) * hit(1), q(2) * hit(0) - q(0) * hit(2), q(0) * hit(1) - q(1) * hit(0))
      assert(cross.map(math.abs).max < 1e-6 * 100 * 100, s"vertex $i: hit $hit not on the ray through $q")
      assert(q.zip(hit).map(_ * _).sum > 0.0, s"vertex $i: hit on the far side of the sphere")
      assertEqualsDouble(entries.map(plan.vals(_)).sum, 1.0, 1e-12)

  test("nearest-vertex resampling of a mesh onto itself is the identity"):
    val plan = ok(SurfaceResampling.plan(fine, fine, SurfaceResampling.Method.Nearest))
    assertEquals(plan.cols.toVector, (0 until fine.vertexCount).toVector)

  test("normalisations: element rows total one, sum conserves mass, and the inverse is the exact adjoint"):
    val plan = ok(SurfaceResampling.plan(coarse, fine))
    val ones = Array.fill(fine.vertexCount)(1.0)
    ok(SurfaceResampling.apply(plan, ones)).foreach(v => assertEqualsDouble(v, 1.0, 1e-12))
    // Sum normalises each input element's weights to one, so total mass is conserved in either direction.
    val mass = Array.tabulate(coarse.vertexCount)(i => 1.0 + math.sin(i.toDouble))
    val spread = ok(SurfaceResampling.apply(plan, mass, inverse = true, normalization = SurfaceResampling.Normalization.Sum))
    assertEqualsDouble(spread.sum, mass.sum, 1e-9)
    val x = Array.tabulate(fine.vertexCount)(i => math.sin(i * 0.37))
    val y = Array.tabulate(coarse.vertexCount)(i => math.cos(i * 0.11))
    val ax = ok(SurfaceResampling.apply(plan, x, normalization = SurfaceResampling.Normalization.None))
    val aty = ok(SurfaceResampling.apply(plan, y, inverse = true, normalization = SurfaceResampling.Normalization.None))
    assertEqualsDouble(ax.zip(y).map(_ * _).sum, x.zip(aty).map(_ * _).sum, 1e-9)

  test("a linear field on the sphere is reproduced to within the triangle-sagitta error"):
    val plan = ok(SurfaceResampling.plan(coarse, fine))
    val moving = ok(SphereMesh.withRadius(fine))
    val reference = ok(SphereMesh.withRadius(coarse))
    def f(c: Array[Double], i: Int) = 0.3 * c(3 * i) - 0.5 * c(3 * i + 1) + 0.8 * c(3 * i + 2)
    val out = ok(SurfaceResampling.apply(plan, Array.tabulate(moving.vertexCount)(f(moving.coordinates, _))))
    val worst = out.indices.map(i => math.abs(out(i) - f(reference.coordinates, i))).max
    assert(worst < 2.0, s"max error $worst exceeds the chord sagitta bound for a level-3 icosphere of radius 100")

  test("input of the wrong length is a typed failure"):
    val plan = ok(SurfaceResampling.plan(coarse, fine))
    assert(SurfaceResampling.apply(plan, Array(1.0, 2.0)).isLeft)
