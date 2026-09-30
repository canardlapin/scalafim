package scalafim.surface

/** Typed template sphere domains and resampling plans on synthetic icospheres (the TemplateFlow spheres themselves are
  * exercised against Workbench in the JVM `TemplateSphereFilesSuite`).
  */
class TemplateSurfacesSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  /** Icosahedron subdivided `levels` times (5 levels: 10242 vertices, fsaverage5's counts), rotated about z. */
  private def icosphere(levels: Int, radius: Double, rotation: Double = 0.0): TriangleMesh =
    val t = (1.0 + math.sqrt(5.0)) / 2.0
    val vertices = scala.collection.mutable.ArrayBuffer(
      (-1.0, t, 0.0), (1.0, t, 0.0), (-1.0, -t, 0.0), (1.0, -t, 0.0), (0.0, -1.0, t), (0.0, 1.0, t),
      (0.0, -1.0, -t), (0.0, 1.0, -t), (t, 0.0, -1.0), (t, 0.0, 1.0), (-t, 0.0, -1.0), (-t, 0.0, 1.0)
    )
    var faces = Vector(
      (0, 11, 5), (0, 5, 1), (0, 1, 7), (0, 7, 10), (0, 10, 11), (1, 5, 9), (5, 11, 4), (11, 10, 2), (10, 7, 6), (7, 1, 8),
      (3, 9, 4), (3, 4, 2), (3, 2, 6), (3, 6, 8), (3, 8, 9), (4, 9, 5), (2, 4, 11), (6, 2, 10), (8, 6, 7), (9, 8, 1)
    )
    (0 until levels).foreach: _ =>
      val midpoints = scala.collection.mutable.HashMap.empty[Long, Int]
      def mid(a: Int, b: Int): Int =
        midpoints.getOrElseUpdate(math.min(a, b).toLong * 1000000L + math.max(a, b), {
          val (p, q) = (vertices(a), vertices(b))
          vertices += (((p._1 + q._1) / 2, (p._2 + q._2) / 2, (p._3 + q._3) / 2))
          vertices.size - 1
        })
      faces = faces.flatMap: (a, b, c) =>
        val (ab, bc, ca) = (mid(a, b), mid(b, c), mid(c, a))
        Vector((a, ab, ca), (b, bc, ab), (c, ca, bc), (ab, bc, ca))
    val (cz, sz) = (math.cos(rotation), math.sin(rotation))
    val coordinates = new Array[Double](3 * vertices.size)
    vertices.indices.foreach: i =>
      val (x, y, z) = vertices(i)
      val n = math.sqrt(x * x + y * y + z * z)
      coordinates(3 * i) = radius * (cz * x - sz * y) / n
      coordinates(3 * i + 1) = radius * (sz * x + cz * y) / n
      coordinates(3 * i + 2) = radius * z / n
    TriangleMesh.fromArrays(coordinates, faces.flatMap((a, b, c) => Vector(a, b, c)).toArray)

  private lazy val ico5 = icosphere(5, 97.0)
  private lazy val ico5Turned = icosphere(5, 103.0, rotation = 0.013)
  private val left = TemplateSurface(TemplateMesh.FsAverage5, CorticalHemisphere.Left)

  private lazy val onFsAverage = ok(TemplateSphere.admit(left, SphereRegistration.FsAverage, ico5, "test:ico5"))
  private lazy val turned = ok(TemplateSphere.admit(left, SphereRegistration.FsAverage, ico5Turned, "test:ico5-turned"))

  test("stock template meshes carry their TemplateFlow counts and native registration"):
    assertEquals(
      TemplateMesh.values.toVector.map(mesh => (mesh.label, mesh.vertices, mesh.faces)),
      Vector(
        ("fsaverage5", 10242, 20480),
        ("fsaverage6", 40962, 81920),
        ("fsaverage", 163842, 327680),
        ("fsLR_32k", 32492, 64980),
        ("fsLR_59k", 59292, 118580),
        ("fsLR_164k", 163842, 327680)
      )
    )
    // every template mesh is a closed triangulated sphere: F = 2V - 4
    TemplateMesh.values.foreach(mesh => assertEquals(mesh.faces, 2 * mesh.vertices - 4, mesh.label))
    assertEquals(TemplateMesh.FsLR59k.nativeRegistration, SphereRegistration.FsLR)
    assertEquals(TemplateMesh.FsAverage6.nativeRegistration, SphereRegistration.FsAverage)

  test("a template sphere is admitted only with the template's counts, on a sphere, and on a registration it has"):
    assertEquals(onFsAverage.sphere.vertexCount, 10242)
    (0 until onFsAverage.sphere.vertexCount by 97).foreach(i => assertEqualsDouble(SphereMesh.radius(onFsAverage.sphere.coordinates, i), 100.0, 1e-9))
    assert(TemplateSphere.admit(TemplateSurface(TemplateMesh.FsAverage6, CorticalHemisphere.Left), SphereRegistration.FsAverage, ico5, "x").isLeft)
    TemplateSphere.admit(left, SphereRegistration.FsLR, ico5, "x") match
      case Left(SurfaceError.InvalidGeometry(reason)) => assert(reason.contains("fsaverage sphere"), reason)
      case other                                      => fail(s"fsaverage5 on the fsLR sphere must be refused, got $other")
    val squashed = TriangleMesh.fromArrays(ico5.coordinates.zipWithIndex.map((v, i) => if i % 3 == 2 then 0.5 * v else v), ico5.faceIndices)
    assert(TemplateSphere.admit(left, SphereRegistration.FsAverage, squashed, "x").isLeft)
    assert(TemplateSphere.admit(left, SphereRegistration.FsAverage, ico5, " ").isLeft)

  test("meshes on different registration spheres cannot be planned against each other"):
    val errors = compileErrors(
      "val a: TemplateSphere[SphereRegistration.FsLR.type] = ???; TemplateResampling.plan(a, onFsAverage)"
    )
    assert(errors.contains("Found:") && errors.contains("FsLR") && errors.contains("FsAverage"), errors)
  test("planning refuses hemispheres that differ"):
    val right = ok(TemplateSphere.admit(TemplateSurface(TemplateMesh.FsAverage5, CorticalHemisphere.Right), SphereRegistration.FsAverage, ico5, "test:ico5-rh"))
    TemplateResampling.plan(onFsAverage, right) match
      case Left(SurfaceError.InvalidGeometry(reason)) => assert(reason.contains("hemispheres differ"), reason)
      case other                                      => fail(s"expected a hemisphere refusal, got $other")

  test("a plan interpolates with unit rows, its adjoint is the exact transpose, and Sum conserves mass"):
    val plan = ok(TemplateResampling.plan(onFsAverage, turned))
    assertEquals(plan.source, left)
    assertEquals(plan.registration, SphereRegistration.FsAverage)
    assert(plan.identity.contains("test:ico5 -> test:ico5-turned"), plan.identity)
    val ones = ok(plan.resample(Array.fill(10242)(1.0)))
    ones.foreach(v => assertEqualsDouble(v, 1.0, 1e-12))
    // a linear field on the sphere is reproduced up to the chord sagitta of a ~2 mm triangle
    val x = Array.tabulate(10242)(i => onFsAverage.sphere.coordinates(3 * i))
    val resampled = ok(plan.resample(x))
    (0 until 10242 by 13).foreach(i => assertEqualsDouble(resampled(i), turned.sphere.coordinates(3 * i), 0.05, s"vertex $i"))
    val u = Array.tabulate(10242)(i => math.sin(0.37 * i))
    val v = Array.tabulate(10242)(i => math.cos(0.11 * i))
    val au = ok(plan.resample(u, SurfaceResampling.Normalization.None))
    val atv = ok(plan.adjoint(v))
    assertEqualsDouble(au.zip(v).map(_ * _).sum, u.zip(atv).map(_ * _).sum, 1e-8)
    val mass = ok(plan.resample(u.map(math.abs), SurfaceResampling.Normalization.Sum))
    assertEqualsDouble(mass.sum, u.map(math.abs).sum, 1e-8)
    assert(plan.resample(Array.fill(10)(1.0)).isLeft)

  test("the pinned TemplateFlow spheres cover every mesh, hemisphere and published registration"):
    assertEquals(TemplateSphereAssets.all.size, 18)
    assertEquals(TemplateSphereAssets.all.map(_.sha256).distinct.size, 18)
    assertEquals(
      TemplateSphereAssets.find(TemplateSurface(TemplateMesh.FsLR32k, CorticalHemisphere.Left), SphereRegistration.FsAverage).map(_.relativePath),
      Some("tpl-fsLR/tpl-fsLR_space-fsaverage_hemi-L_den-32k_sphere.surf.gii")
    )
    assertEquals(
      TemplateSphereAssets.find(TemplateSurface(TemplateMesh.FsAverage7, CorticalHemisphere.Right), SphereRegistration.FsAverage).map(_.relativePath),
      Some("tpl-fsaverage/tpl-fsaverage_hemi-R_den-164k_desc-std_sphere.surf.gii")
    )
    assertEquals(TemplateSphereAssets.find(left, SphereRegistration.FsLR), None)
