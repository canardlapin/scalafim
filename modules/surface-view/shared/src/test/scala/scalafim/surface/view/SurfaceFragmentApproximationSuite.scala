package scalafim.surface.view

import intaglio.*

class SurfaceFragmentApproximationSuite extends munit.FunSuite:
  private def error(a: Rgba32, b: Rgba32): Int = Vector(math.abs(a.red - b.red), math.abs(a.green - b.green),
    math.abs(a.blue - b.blue), math.abs(a.alpha - b.alpha)).max
  private val config = SurfaceFragmentApproximationConfig.make(4, 500000).toOption.get
  private val base = Rgba32.unsafe(184, 184, 184)

  private def verify(plan: SurfaceRenderPlan): SurfaceFragmentApproximation =
    val mesh = plan.meshes.head
    val result = SurfaceFragmentApproximation.build(mesh, plan.layers, plan.lighting, config).fold(e => fail(e.message), identity)
    assert(result.maximumChannelError <= 4)
    val evaluator = new SurfaceFragmentEvaluator(mesh, plan.layers, plan.lighting, base)
    val random = new scala.util.Random(902108L)
    result.cells.foreach: cell =>
      val t = cell.triangle
      val (a, b, c) = t.sourceVertices
      assert(t.areaFraction > 0)
      for _ <- 0 until 5 do
        val x = random.nextDouble() + 0.001
        val y = random.nextDouble() + 0.001
        val z = random.nextDouble() + 0.001
        val sum = x + y + z
        val w = t.weights(x / sum, y / sum, z / sum)
        val actual = evaluator.color(t.sourceFace, a, b, c, w.a, w.b, w.c)
        assert(cell.bounds.contains(actual), s"$actual outside ${cell.bounds}, triangle=$t")
        assert(error(cell.color, actual) <= 4)
    result.cells.groupBy(_.triangle.sourceFace).foreach: (_, cells) =>
      assertEqualsDouble(cells.map(_.triangle.areaFraction).sum, 1.0, 2e-12)
    result

  test("scalar thresholds and ramp knots have a bounded constant-color approximation"):
    val result = verify(SurfaceScalarFixture.plan(SurfaceScalarFixture.thresholded))
    assert(result.cells.length > result.initialTriangles)
    assert(result.maximumDepth > 0)

  test("color intervals enclose every blend mode with varying source alpha"):
    val mapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(0, 1),
      ScalarRamp.linear(Rgba32.unsafe(20, 80, 140, 30), Rgba32.unsafe(200, 160, 80, 220))))
    val model = SurfaceScalarFixture.model()
    val under = SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("under"), SurfaceScalarFixture.Surface,
      SurfaceFaceFixture.geometry, Array(0.0, 0.0, 1.0, 1.0), mapping).toOption.get
    for mode <- DisplayBlendMode.values do
      val over = SurfaceLayer.interpolatedScalar(SurfaceScalarFixture.Layer, SurfaceScalarFixture.Surface,
        SurfaceFaceFixture.geometry, SurfaceScalarFixture.Values.toArray, SurfaceScalarFixture.thresholded,
        opacity = DisplayOpacity.unsafe(0.7), blendMode = mode).toOption.get
      val viewer = SurfaceViewerModel.make(model.surfaces, Vector(under, over)).toOption.get
      verify(SurfaceCompiler.compile(viewer, SurfaceFaceFixture.state(viewer)).toOption.get)

  test("normal-cone bounds enclose normalized world lighting, including a terminator"):
    val plan = SurfaceScalarFixture.plan(SurfaceScalarFixture.thresholded)
    val mesh = plan.meshes.head.copy(normals = new FloatBufferView(Array[Float](
      -0.6f, 0, 0.8f, 0.6f, 0, 0.8f, 0, 0.6f, 0.8f, 0, -0.6f, 0.8f)))
    verify(plan.copy(meshes = Vector(mesh), lighting = SurfaceLighting.directional(0.2, 0.8, 1, 0, 0).toOption.get))

  test("resource limits reject instead of returning an approximation outside its promised error"):
    val plan = SurfaceScalarFixture.plan()
    val tiny = SurfaceFragmentApproximationConfig.make(1, 20).toOption.get
    assert(SurfaceFragmentApproximation.build(plan.meshes.head, plan.layers, plan.lighting, tiny).isLeft)
    val shallow = SurfaceFragmentApproximationConfig.make(1, 100000, maxDepth = 0).toOption.get
    assert(SurfaceFragmentApproximation.build(plan.meshes.head, plan.layers, plan.lighting, shallow).left.toOption.get match
      case SurfaceApproximationError.DepthBudgetExceeded(_, 0, error) => error > 1
      case _ => false)
    assert(SurfaceFragmentApproximationConfig.make(0, 100).isLeft)

  test("adaptive leaves meet along matching edges without hanging vertices"):
    val plan = SurfaceScalarFixture.plan(SurfaceScalarFixture.thresholded)
    val result = SurfaceFragmentApproximation.build(plan.meshes.head, plan.layers, plan.lighting, config).toOption.get
    type Edge = (Int, SurfaceFaceWeights, SurfaceFaceWeights)
    val edges = scala.collection.mutable.Map.empty[Edge, Int]
    result.cells.foreach: cell =>
      val t = cell.triangle
      for (a, b) <- Vector((t.a, t.b), (t.b, t.c), (t.c, t.a)) do
        val ordered = a.a < b.a || (a.a == b.a && (a.b < b.b || (a.b == b.b && a.c <= b.c)))
        val key = if ordered then (t.sourceFace, a, b) else (t.sourceFace, b, a)
        edges(key) = edges.getOrElse(key, 0) + 1
    edges.foreach: (key, count) =>
      val (_, a, b) = key
      val outer = (a.a == 0 && b.a == 0) || (a.b == 0 && b.b == 0) || (a.c == 0 && b.c == 0)
      assertEquals(count, if outer then 1 else 2, s"unmatched internal edge: $key")

  test("incident scientific faces share the same adaptive edge vertices"):
    val model = SurfaceScalarFixture.model(SurfaceScalarFixture.thresholded, Array(-2.0, 4.0, 1.0, -0.5))
    val plan = SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get
    val result = SurfaceFragmentApproximation.build(plan.meshes.head, plan.layers, plan.lighting, config).toOption.get
    type Point = Vector[(Int, Double)]
    val edges = scala.collection.mutable.Map.empty[(Int, Point, Point), Int]
    val outerEdges = Vector(Set(0, 1), Set(1, 2), Set(2, 3), Set(0, 3))
    result.cells.foreach: cell =>
      val t = cell.triangle
      def key(w: SurfaceFaceWeights): Point = Vector(t.sourceVertices._1 -> w.a, t.sourceVertices._2 -> w.b,
        t.sourceVertices._3 -> w.c).filter(_._2 != 0).sortBy(_._1)
      for (a, b) <- Vector((t.a, t.b), (t.b, t.c), (t.c, t.a)) do
        val x = key(a)
        val y = key(b)
        val support = (x.map(_._1) ++ y.map(_._1)).toSet
        val scope = if support.size <= 2 then -1 else t.sourceFace
        val edge = if x.toString < y.toString then (scope, x, y) else (scope, y, x)
        edges(edge) = edges.getOrElse(edge, 0) + 1
    edges.foreach: (edge, count) =>
      val support = (edge._2.map(_._1) ++ edge._3.map(_._1)).toSet
      val outer = outerEdges.exists(support.subsetOf)
      assertEquals(count, if outer then 1 else 2, s"nonconforming scientific edge: $edge")

  test("geometric aliases conform duplicated edges without merging discontinuous colors"):
    val plan = SurfaceFaceFixture.plan
    val mesh = plan.meshes.head.copy(sourceVertices = None)
    val colors = new IntBufferView(Array(Rgba32.unsafe(0, 0, 0).toPackedInt,
      Rgba32.unsafe(100, 0, 0).toPackedInt, Rgba32.unsafe(255, 0, 0).toPackedInt,
      SurfaceFaceFixture.Blue.toPackedInt, SurfaceFaceFixture.Blue.toPackedInt, SurfaceFaceFixture.Blue.toPackedInt))
    val layer = plan.layers.head.copy(colors = colors, sampleColors = Some(colors),
      association = SurfaceSampleAssociation.Vertex, interpolation = SurfaceMapInterpolation.VertexColor)
    val aliases = new IntBufferView(Array(0, 1, 2, 0, 2, 3))
    val result = SurfaceFragmentApproximation.build(mesh, Vector(layer), SurfaceLighting.Unlit, config, Some(aliases)).toOption.get
    def sharedSegments(face: Int): Set[(Double, Double)] =
      result.cells.filter(_.triangle.sourceFace == face).flatMap: cell =>
        val t = cell.triangle
        def point(w: SurfaceFaceWeights): Option[Double] =
          val weights = Vector(t.sourceVertices._1 -> w.a, t.sourceVertices._2 -> w.b, t.sourceVertices._3 -> w.c)
          if weights.exists((v, weight) => weight != 0 && aliases(v) != 0 && aliases(v) != 2) then None
          else Some(weights.filter((v, _) => aliases(v) == 2).map(_._2).sum)
        Vector((t.a, t.b), (t.b, t.c), (t.c, t.a)).flatMap: (a, b) =>
          for x <- point(a); y <- point(b) if x != y yield (math.min(x, y), math.max(x, y))
      .toSet
    val first = sharedSegments(0)
    assert(first.size > 1, "the varying face must refine the shared diagonal")
    assertEquals(sharedSegments(1), first)
    result.cells.filter(_.triangle.sourceFace == 1).foreach(cell => assertEquals(cell.color, SurfaceFaceFixture.Blue))
    assertEqualsDouble(result.cells.filter(_.triangle.sourceFace == 1).map(_.triangle.areaFraction).sum, 1, 1e-12)
    assertEquals(SurfaceFragmentApproximation.build(mesh, Vector(layer), SurfaceLighting.Unlit, config,
      Some(new IntBufferView(Array(0)))).left.toOption, Some(SurfaceApproximationError.InvalidEdgeAliases))
    assertEquals(SurfaceFragmentApproximation.build(mesh, Vector(layer), SurfaceLighting.Unlit, config,
      Some(new IntBufferView(Array.fill(6)(0)))).left.toOption, Some(SurfaceApproximationError.InvalidEdgeAliases))
