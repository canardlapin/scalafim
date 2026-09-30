package scalafim.surface

class TemplatePlanTopologySuite extends munit.FunSuite:
  private def ok[A](result: Either[SurfaceError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def icosphere(levels: Int, radius: Double, rotation: Double = 0.0): TriangleMesh =
    val t = (1.0 + math.sqrt(5.0)) / 2.0
    val vertices = scala.collection.mutable.ArrayBuffer(
      (-1.0, t, 0.0), (1.0, t, 0.0), (-1.0, -t, 0.0), (1.0, -t, 0.0),
      (0.0, -1.0, t), (0.0, 1.0, t), (0.0, -1.0, -t), (0.0, 1.0, -t),
      (t, 0.0, -1.0), (t, 0.0, 1.0), (-t, 0.0, -1.0), (-t, 0.0, 1.0)
    )
    var faces = Vector(
      (0, 11, 5), (0, 5, 1), (0, 1, 7), (0, 7, 10), (0, 10, 11),
      (1, 5, 9), (5, 11, 4), (11, 10, 2), (10, 7, 6), (7, 1, 8),
      (3, 9, 4), (3, 4, 2), (3, 2, 6), (3, 6, 8), (3, 8, 9),
      (4, 9, 5), (2, 4, 11), (6, 2, 10), (8, 6, 7), (9, 8, 1)
    )
    (0 until levels).foreach: _ =>
      val midpoints = scala.collection.mutable.HashMap.empty[Long, Int]
      def midpoint(a: Int, b: Int): Int =
        val key = math.min(a, b).toLong * 1000000L + math.max(a, b)
        midpoints.get(key) match
          case Some(index) => index
          case None =>
            val (p, q) = (vertices(a), vertices(b))
            vertices.addOne(((p._1 + q._1) / 2, (p._2 + q._2) / 2, (p._3 + q._3) / 2))
            val index = vertices.size - 1
            midpoints(key) = index
            index
      faces = faces.flatMap: (a, b, c) =>
        val (ab, bc, ca) = (midpoint(a, b), midpoint(b, c), midpoint(c, a))
        Vector((a, ab, ca), (b, bc, ab), (c, ca, bc), (ab, bc, ca))
    val (cosine, sine) = (math.cos(rotation), math.sin(rotation))
    val coordinates = new Array[Double](3 * vertices.size)
    vertices.indices.foreach: i =>
      val (x, y, z) = vertices(i)
      val norm = math.sqrt(x * x + y * y + z * z)
      coordinates(3 * i) = radius * (cosine * x - sine * y) / norm
      coordinates(3 * i + 1) = radius * (sine * x + cosine * y) / norm
      coordinates(3 * i + 2) = radius * z / norm
    TriangleMesh.fromArrays(coordinates, faces.flatMap((a, b, c) => Vector(a, b, c)).toArray)

  test("fsaverage5 plan freezes endpoint topology and rejects a later face-order mutation"):
    val surface = TemplateSurface(TemplateMesh.FsAverage5, CorticalHemisphere.Left)
    val sourceMesh = icosphere(5, 97.0)
    val targetMesh = icosphere(5, 103.0, 0.013)
    val originalSourceGeometry = SurfaceGeometry(sourceMesh, Hemisphere.Left, SurfaceKind.White)
    val source = ok(TemplateSphere.admit(surface, SphereRegistration.FsAverage, sourceMesh, "test:source"))
    val target = ok(TemplateSphere.admit(surface, SphereRegistration.FsAverage, targetMesh, "test:target"))
    val plan = ok(TemplateResampling.plan(source, target))
    assertEquals(plan.plan.referenceVertices, 10242)
    assertEquals(ok(plan.resample(Array.tabulate(10242)(i => math.sin(i * 0.013)))).length, 10242)
    assertEquals(plan.validateSourceGeometry(originalSourceGeometry), Right(()))
    val sourceFaces = sourceMesh.faceIndices
    val swap = sourceFaces(1)
    sourceFaces(1) = sourceFaces(2)
    sourceFaces(2) = swap
    plan.validateSourceGeometry(originalSourceGeometry) match
      case Left(SurfaceError.InvalidTopology(reason)) => assert(reason.contains("ordered template topology"), reason)
      case other => fail(s"expected frozen topology refusal, got $other")
    val wrongHemisphere = SurfaceGeometry(targetMesh, Hemisphere.Right, SurfaceKind.Inflated)
    plan.validateTargetGeometry(wrongHemisphere) match
      case Left(SurfaceError.InvalidGeometry(reason)) => assert(reason.contains("hemisphere"), reason)
      case other => fail(s"expected target hemisphere refusal, got $other")
