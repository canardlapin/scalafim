package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.view.*

class JavaFxSurfaceApproximationSuite extends munit.FunSuite:
  private val settings = JavaFxApproximationConfig.make(maxChannelError = 8, maxTriangles = 200000).toOption.get

  test("preparation preserves original-face coordinates and reports generated allocation"):
    val source = SurfaceScalarFixture.plan(SurfaceScalarFixture.thresholded)
    val prepared = JavaFxSurfaceApproximation.prepare(source, settings, JavaFxAtlasConfig.Default).toOption.get
    val receipt = prepared.approximation.get
    assertEquals(receipt.sourceFaces, 2)
    assert(receipt.derivedFaces > receipt.partitionFaces)
    assert(receipt.maximumChannelError <= 8)
    assert(receipt.generatedBytes <= settings.maxGeneratedBytes)
    assertEquals(prepared.plan.lighting, SurfaceLighting.Unlit)
    assert(prepared.plan.fragmentSurfaces.isEmpty)
    assert(prepared.plan.layers.forall(_.scalarField.isEmpty))
    val mesh = prepared.plan.meshes.head
    mesh.constantPartition.get.zipWithIndex.foreach: (t, face) =>
      assertEquals(mesh.sourceFace(face), t.sourceFace)
      assertEquals(mesh.sourceFaceVertices(face), source.meshes.head.sourceFaceVertices(t.sourceFace))
      val expected = t.weights(0.2, 0.3, 0.5)
      val actual = mesh.sourceBarycentric(face, 0.2, 0.3, 0.5)
      assertEqualsDouble(actual._1, expected.a, 1e-14)
      assertEqualsDouble(actual._2, expected.b, 1e-14)
      assertEqualsDouble(actual._3, expected.c, 1e-14)
      assertEquals(prepared.plan.layers.head.colors(face * 3), prepared.plan.layers.head.colors(face * 3 + 2))

  test("mixed nearest/scalar preparation uses original positions after subdivision"):
    val scalar = SurfaceScalarFixture.model()
    val nearest = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("nearest"), SurfaceScalarFixture.Surface,
      SurfaceFaceFixture.geometry, Vector.fill(4)(Rgba32.unsafe(0, 0, 0, 0)), interpolation = SurfaceVertexInterpolation.NearestSample).toOption.get
    val model = SurfaceViewerModel.make(scalar.surfaces, scalar.layers :+ nearest).toOption.get
    val source = SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get
    val original = source.meshes.head.samplePositions.get
    assertEquals(original.length, 12)
    val prepared = JavaFxSurfaceApproximation.prepare(source, settings, JavaFxAtlasConfig.Default).toOption.get.plan
    val mesh = prepared.meshes.head
    mesh.constantPartition.get.zipWithIndex.foreach: (t, face) =>
      val (a, b, c) = t.sourceVertices
      for (w, corner) <- Vector(t.a, t.b, t.c).zipWithIndex; axis <- 0 until 3 do
        val expected = (w.a * original(a * 3 + axis) + w.b * original(b * 3 + axis) + w.c * original(c * 3 + axis)).toFloat
        assertEquals(mesh.positions((face * 3 + corner) * 3 + axis), expected)

  test("small budgets fail before native allocation and plain plans retain the legacy path"):
    val source = SurfaceScalarFixture.plan()
    val tiny = JavaFxApproximationConfig.make(maxGeneratedBytes = 100).toOption.get
    assert(JavaFxSurfaceApproximation.prepare(source, tiny, JavaFxAtlasConfig.Default).isLeft)
    val plain = source.copy(fragmentSurfaces = Set.empty, layers = Vector.empty)
    val prepared = JavaFxSurfaceApproximation.prepare(plain, settings, JavaFxAtlasConfig.Default).toOption.get
    assertEquals(prepared.plan, plain)
    assertEquals(prepared.approximation, None)

  test("constant display normals remain perpendicular to vertical faces for native tangent frames"):
    val base = SurfaceScalarFixture.plan()
    // Rotate the original XY plane onto YZ: the former dummy +Z normal lies
    // in this plane and can collapse Prism's artificial tangent frame.
    val source = base.copy(meshes = base.meshes.map: mesh =>
      val positions = mesh.positions.unsafeArray.grouped(3).flatMap(p => Array(p(2), p(1), -p(0))).toArray
      mesh.copy(positions = new FloatBufferView(positions)))
    val mesh = JavaFxSurfaceApproximation.prepare(source, settings, JavaFxAtlasConfig.Default).toOption.get.plan.meshes.head
    for vertex <- 0 until mesh.positions.length / 3 do
      val offset = vertex * 3
      assertEqualsDouble(math.abs(mesh.normals(offset)), 1, 1e-6)
      assertEqualsDouble(mesh.normals(offset + 1), 0, 1e-6)
      assertEqualsDouble(mesh.normals(offset + 2), 0, 1e-6)

  test("normal changes have distinct derived identities even when positions are unchanged"):
    val source = SurfaceScalarFixture.plan()
    val normalOnly = source.copy(meshes = source.meshes.map(_.copy(normals = new FloatBufferView(Array.fill(12)(0.0f)))))
    val first = JavaFxSurfaceApproximation.prepare(source, settings, JavaFxAtlasConfig.Default).toOption.get
    val second = JavaFxSurfaceApproximation.prepare(normalOnly, settings, JavaFxAtlasConfig.Default).toOption.get
    assertNotEquals(first.plan.receipt.meshKeys, second.plan.receipt.meshKeys)
    assert(first.approximation.get.preparationNanos > 0)

  test("native indexing and texture arithmetic reject oversized configurations before allocation"):
    assert(JavaFxApproximationConfig.make(maxTriangles = Int.MaxValue).isLeft)
    val unsafeAtlas = JavaFxAtlasConfig.make(maxTextureSize = Int.MaxValue).toOption.get
    assert(JavaFxSurfaceApproximation.prepare(SurfaceScalarFixture.plan(), settings, unsafeAtlas).isLeft)

  test("lit nearest cells approximate legacy corner shading and retain original provenance"):
    val base = SurfaceNearestFixture.plan
    val mesh = base.meshes.head
    // A varying, non-unit normal field distinguishes legacy corner Lambert
    // factors from normalized fragment lighting. Only red varies here.
    val normals = Array.tabulate(mesh.positions.length): index =>
      if index % 3 == 2 then (mesh.positions(index - 2) + 1) * 0.25f else 0.0f
    val source = base.copy(meshes = Vector(mesh.copy(normals = new FloatBufferView(normals))),
      lighting = SurfaceLighting.directional(0.4, 0.6, 0, 0, 1).toOption.get)
    val prepared = JavaFxSurfaceApproximation.prepare(source, settings, JavaFxAtlasConfig.Default).toOption.get
    val output = prepared.plan.meshes.head
    assert(prepared.approximation.nonEmpty)
    assertEquals(prepared.approximation.get.sourceFaces, 2)
    assert(prepared.approximation.get.maximumChannelError <= settings.quality.maxChannelError)
    val tooStrict = JavaFxApproximationConfig.make(maxChannelError = 1).toOption.get
    assert(JavaFxSurfaceApproximation.prepare(source, tooStrict, JavaFxAtlasConfig.Default).isLeft)
    assertEquals(prepared.plan.lighting, SurfaceLighting.Unlit)
    assertEquals(output.constantPartition.get.map(_.sourceFace).toSet, Set(0, 1))
    output.constantPartition.get.zipWithIndex.foreach: (t, face) =>
      val center = t.centroid
      val (a, b, c) = t.sourceVertices
      val x = center.a * SurfaceFaceFixture.geometry.mesh.coordinates(a * 3) +
        center.b * SurfaceFaceFixture.geometry.mesh.coordinates(b * 3) + center.c * SurfaceFaceFixture.geometry.mesh.coordinates(c * 3)
      val owner = SurfaceNearestPartition.nearestVertex(a, b, c, center.a, center.b, center.c)
      val baseColor = SurfaceNearestFixture.Palette(owner)
      val factor = 0.4 + 0.6 * (x + 1) * 0.25
      val actual = Rgba32.fromPackedInt(prepared.plan.layers.head.colors(face * 3))
      assert(math.abs(actual.red - math.round(baseColor.red * factor)) <= settings.quality.maxChannelError)
      val positionX = (output.positions(face * 9) + output.positions(face * 9 + 3) + output.positions(face * 9 + 6)) / 3.0
      assertEqualsDouble(positionX, x, 1e-6)

  test("varying legacy colors obey byte budgets and constant unlit categories retain their mesh"):
    val nearest = SurfaceNearestFixture.plan
    val unchanged = JavaFxSurfaceApproximation.prepare(nearest, settings, JavaFxAtlasConfig.Default).toOption.get
    assertEquals(unchanged.approximation, None)
    assert(unchanged.plan eq nearest)
    val varying = nearest.copy(layers = nearest.layers.map(l => l.copy(colors = new IntBufferView(
      Array.tabulate(l.colors.length)(i => Rgba32.unsafe(i % 256, 0, 0).toPackedInt)))))
    val tiny = JavaFxApproximationConfig.make(maxGeneratedBytes = 100).toOption.get
    assert(JavaFxSurfaceApproximation.prepare(varying, tiny, JavaFxAtlasConfig.Default).isLeft)
