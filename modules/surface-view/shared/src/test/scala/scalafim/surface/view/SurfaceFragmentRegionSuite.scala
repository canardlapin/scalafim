package scalafim.surface.view

import intaglio.*

class SurfaceFragmentRegionSuite extends munit.FunSuite:
  test("cell evaluation matches the reference inside and uses the correct side at a hidden seam"):
    val mapping = SurfaceScalarFixture.thresholded
    val plan = SurfaceScalarFixture.plan(mapping)
    val mesh = plan.meshes.head
    val evaluator = new SurfaceFragmentEvaluator(mesh, plan.layers, SurfaceLighting.Unlit, Rgba32.unsafe(184, 184, 184))
    val triangles = SurfaceMappingPartition.build(mesh, plan.layers, SurfacePartitionBudget.make(1000, 64).toOption.get).toOption.get
    var seamChecks = 0
    triangles.foreach: t =>
      val region = evaluator.within(t)
      val (a, b, c) = t.sourceVertices
      for i <- 1 until 10; j <- 1 until 10 - i do
        val w = t.weights(i / 10.0, j / 10.0, 1.0 - (i + j) / 10.0)
        assertEquals(region.color(t.sourceFace, a, b, c, w.a, w.b, w.c), evaluator.color(t.sourceFace, a, b, c, w.a, w.b, w.c))
      def scalar(w: SurfaceFaceWeights): Double =
        w.a * SurfaceScalarFixture.Values(a) + w.b * SurfaceScalarFixture.Values(b) + w.c * SurfaceScalarFixture.Values(c)
      if math.abs(scalar(t.centroid)) < 0.25 then
        Vector(t.a, t.b, t.c).foreach: w =>
          if math.abs(math.abs(scalar(w)) - 0.25) < 1e-14 then
            assertEquals(region.color(t.sourceFace, a, b, c, w.a, w.b, w.c), Rgba32.unsafe(184, 184, 184))
            seamChecks += 1
    assert(seamChecks > 0)

  test("invalid cell interiors stay invalid at finite corners instead of bleeding a rescued value"):
    val model = SurfaceScalarFixture.model(values = Array(Double.NaN, 4, 4, -2))
    val plan = SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get
    val mesh = plan.meshes.head
    val evaluator = new SurfaceFragmentEvaluator(mesh, plan.layers, SurfaceLighting.Unlit, Rgba32.unsafe(184, 184, 184))
    val triangle = SurfaceMappingPartition.build(mesh, plan.layers, SurfacePartitionBudget.make(100, 64).toOption.get).toOption.get.head
    val (a, b, c) = triangle.sourceVertices
    assertEquals(evaluator.within(triangle).color(triangle.sourceFace, a, b, c, 0, 1, 0), Rgba32.unsafe(184, 184, 184))
    assertNotEquals(evaluator.color(triangle.sourceFace, a, b, c, 0, 1, 0), Rgba32.unsafe(184, 184, 184))

  test("texture padding extrapolates the affine field instead of flattening it at a triangle edge"):
    val ramp = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-2, 4),
      ScalarRamp.linear(Rgba32.unsafe(0, 0, 0), Rgba32.unsafe(240, 240, 240))))
    val plan = SurfaceScalarFixture.plan(ramp)
    val mesh = plan.meshes.head
    val evaluator = new SurfaceFragmentEvaluator(mesh, plan.layers, SurfaceLighting.Unlit, Rgba32.unsafe(184, 184, 184))
    val triangle = SurfaceMappingPartition.build(mesh, plan.layers, SurfacePartitionBudget.make(100, 64).toOption.get).toOption.get.head
    val (a, b, c) = triangle.sourceVertices
    // (x,y) = (0,-1.2) is below the square; f=1+3*x=1 and the ramp value is 120.
    assertEquals(evaluator.within(triangle).extrapolatedColor(0, a, b, c, 0.5, 0.6, -0.1), Rgba32.unsafe(120, 120, 120))
    intercept[IllegalArgumentException](evaluator.extrapolatedColor(0, a, b, c, 0.5, 0.6, -0.1))

  test("a saturated region remains constant in its texture padding"):
    val plan = SurfaceScalarFixture.plan()
    val mesh = plan.meshes.head
    val evaluator = new SurfaceFragmentEvaluator(mesh, plan.layers, SurfaceLighting.Unlit, Rgba32.unsafe(184, 184, 184))
    val triangle = SurfaceMappingPartition.build(mesh, plan.layers, SurfacePartitionBudget.make(100, 64).toOption.get).toOption.get.find: t =>
      val w = t.centroid
      val (a, b, c) = t.sourceVertices
      w.a * SurfaceScalarFixture.Values(a) + w.b * SurfaceScalarFixture.Values(b) + w.c * SurfaceScalarFixture.Values(c) < -1
    val t = triangle.get
    val (a, b, c) = t.sourceVertices
    assertEquals(evaluator.within(t).extrapolatedColor(t.sourceFace, a, b, c, 0.5, 0.5, 0), Rgba32.unsafe(0, 0, 200))
