package scalafim.surface.view

import intaglio.*

class SurfaceMappingPartitionSuite extends munit.FunSuite:
  private val budget = SurfacePartitionBudget.make(10000, 64).toOption.get

  private def partition(plan: SurfaceRenderPlan): Vector[SurfaceMappingTriangle] =
    SurfaceMappingPartition.build(plan.meshes.head, plan.layers, budget).toOption.get

  test("cuts cover each original face once and retain the analytic hidden-band area"):
    val plan = SurfaceScalarFixture.plan(SurfaceScalarFixture.thresholded)
    val triangles = partition(plan)
    assert(triangles.length > 2)
    triangles.groupBy(_.sourceFace).foreach: (_, parts) =>
      assertEqualsDouble(parts.map(_.areaFraction).sum, 1.0, 2e-14)
      assert(parts.forall(_.areaFraction > 0))
    def value(t: SurfaceMappingTriangle, w: SurfaceFaceWeights): Double =
      val (a, b, c) = t.sourceVertices
      w.a * SurfaceScalarFixture.Values(a) + w.b * SurfaceScalarFixture.Values(b) + w.c * SurfaceScalarFixture.Values(c)
    val hidden = triangles.filter(t => math.abs(value(t, t.centroid)) < 0.25)
    // f = 1 + 3*x; the band has width 1/6 of a world unit over a square of width 2.
    assertEqualsDouble(hidden.map(_.areaFraction).sum / 2.0, 1.0 / 12.0, 1e-14)
    triangles.foreach: triangle =>
      val values = Vector(triangle.a, triangle.b, triangle.c).map(value(triangle, _))
      Vector(-1.0, -0.5, -0.25, 0.25, 1.0).foreach: cut =>
        assert(!(values.min < cut - 2e-14 && values.max > cut + 2e-14), s"triangle crosses $cut: $values")

  test("two independent coordinate fields partition every mapping boundary"):
    val initial = SurfaceScalarFixture.plan(SurfaceScalarFixture.thresholded)
    val first = initial.layers.head
    val second = first.copy(layer = SurfaceLayerId.unsafe("orthogonal"),
      scalarField = Some(new SurfaceScalarPacket(new DoubleBufferView(Array(-2.0, -2.0, 4.0, 4.0)), SurfaceScalarFixture.thresholded)))
    val triangles = partition(initial.copy(layers = initial.layers :+ second))
    assertEqualsDouble(triangles.map(_.areaFraction).sum, 2.0, 5e-14)
    var overlap = 0.0
    triangles.foreach: t =>
      val w = t.centroid
      val (a, b, c) = t.sourceVertices
      val x = w.a * first.scalarField.get.samples(a) + w.b * first.scalarField.get.samples(b) + w.c * first.scalarField.get.samples(c)
      val y = w.a * second.scalarField.get.samples(a) + w.b * second.scalarField.get.samples(b) + w.c * second.scalarField.get.samples(c)
      if math.abs(x) < 0.25 && math.abs(y) < 0.25 then overlap += t.areaFraction
    assertEqualsDouble(overlap / 2.0, 1.0 / 144.0, 1e-14)

  test("nearest subdivisions retain original provenance after scalar cuts"):
    val plan = SurfaceScalarFixture.plan()
    val original = plan.meshes.head
    val lowered = SurfaceNearestPartition.lower(original).copy(sampleNormals = Some(original.normals))
    val triangles = SurfaceMappingPartition.build(lowered, plan.layers, budget).toOption.get
    assertEqualsDouble(triangles.map(_.areaFraction).sum, 2.0, 2e-14)
    triangles.foreach: t =>
      assertEquals(t.sourceVertices, original.sourceFaceVertices(t.sourceFace))
      val center = t.centroid
      val owner = Vector(center.a, center.b, center.c).zipWithIndex.maxBy(_._1)._2
      Vector(t.a, t.b, t.c).foreach: w =>
        val coordinates = Vector(w.a, w.b, w.c)
        assert(coordinates(owner) >= coordinates.max - 2e-14)

  test("finite extreme domains do not overflow and invalid interiors remain whole"):
    val plan = SurfaceScalarFixture.plan()
    val mapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-Double.MaxValue, Double.MaxValue),
      ScalarRamp.make(Vector(0.0 -> Rgba32.unsafe(0, 0, 0), 0.5 -> Rgba32.unsafe(100, 100, 100),
        1.0 -> Rgba32.unsafe(200, 200, 200))).toOption.get))
    assertEquals(SurfaceMappingPartition.boundaries(mapping), Vector(-Double.MaxValue, 0.0, Double.MaxValue))
    val samples = new DoubleBufferView(Array(-Double.MaxValue, Double.MaxValue, Double.MaxValue, -Double.MaxValue))
    val layers = Vector(plan.layers.head.copy(scalarField = Some(new SurfaceScalarPacket(samples, mapping))))
    val triangles = SurfaceMappingPartition.build(plan.meshes.head, layers, budget).toOption.get
    assertEquals(triangles.length, 6)
    assertEqualsDouble(triangles.map(_.areaFraction).sum, 2.0, 1e-14)
    val invalid = layers.head.copy(scalarField = Some(new SurfaceScalarPacket(new DoubleBufferView(Array(Double.NaN, 4, 4, -2)), mapping)))
    assertEquals(SurfaceMappingPartition.build(plan.meshes.head, Vector(invalid), budget).toOption.get.length, 2)

  test("budgets reject before unbounded geometry growth"):
    val plan = SurfaceScalarFixture.plan(SurfaceScalarFixture.thresholded)
    val triangles = SurfacePartitionBudget.make(2, 64).toOption.get
    assertEquals(SurfaceMappingPartition.build(plan.meshes.head, plan.layers, triangles),
      Left(SurfacePartitionError.TriangleBudgetExceeded(2)))
    val cuts = SurfacePartitionBudget.make(1000, 0).toOption.get
    assertEquals(SurfaceMappingPartition.build(plan.meshes.head, plan.layers, cuts),
      Left(SurfacePartitionError.CutBudgetExceeded(0, 1, 0)))
    assert(SurfacePartitionBudget.make(0, 4).isLeft)
