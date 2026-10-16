package scalafim.surface.view.javafx

import scalafim.surface.view.*

class JavaFxSurfaceCompositionSuite extends munit.FunSuite:
  private def ok[E, A](value: Either[E, A]): A = value.fold(error => fail(s"$error"), identity)

  private def base: SurfaceCompositionPlan =
    ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))

  test("ordered depth testing without depth writes explicitly requires the raster fallback"):
    val plan = base
    val marker = SurfaceMarkerDecoration(ok(SurfaceDecorationId.make("ordered")),
      SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(SurfaceFaceFixture.Surface, scalafim.surface.VertexId(0))),
      SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.pixels(4)), intaglio.Rgba32.unsafe(0, 0, 0),
      SurfaceDecorationDepthPolicy.OrderedDepthTest)
    val geometry = ok(SurfaceDecorationGeometry.lower(ok(SurfaceDecorationCompiler.compile(Vector(marker), plan)), plan, 100, 100))
    assert(JavaFxSurfaceComposition.validateDepthPolicy(geometry).isLeft)
    assert(JavaFxSurfaceComposition.validateDepthPolicy(geometry.copy(meshes = geometry.meshes.map(
      _.copy(depthPolicy = SurfaceDecorationDepthPolicy.DepthTest)))).isRight)

  test("toolkit-free preflight rejects unsupported fragment and clipping policies before native mutation"):
    val plan = base
    val fragment = ok(SurfaceCompositionPlan.fromLegacy(SurfaceScalarFixture.plan()))
    assert(JavaFxSurfaceComposition.resourcePlan(fragment).isLeft)
    val clipped = plan.views.head.copy(clipping = SurfaceClipping.WorldPlanes(Vector(WorldClipPlane.unsafe(1, 0, 0, 0))))
    assert(JavaFxSurfaceComposition.resourcePlan(ok(SurfaceCompositionPlan.make(Vector(clipped), plan.resources))).isLeft)

  test("resource preflight retains declared resource identities for repeated instances"):
    val plan = base
    val view = plan.views.head
    val first = view.instances.head
    val repeated = first.copy(id = SurfaceInstanceId.make("repeat").toOption.get)
    val composed = ok(SurfaceCompositionPlan.make(Vector(
      view.copy(id = SurfaceViewId.make("left").toOption.get, viewport = SurfaceViewport(0, 0, .5, 1), instances = Vector(first)),
      view.copy(id = SurfaceViewId.make("right").toOption.get, viewport = SurfaceViewport(.5, 0, .5, 1), instances = Vector(repeated))
    ), plan.resources))
    val proxy = ok(JavaFxSurfaceComposition.resourcePlan(composed))
    assertEquals(proxy.meshes.map(_.resourceKey).distinct.length, 1)
    assertEquals(proxy.layers.map(_.resourceKey).distinct.length, 1)

  test("explicit asymmetric orthographic packets require the raster host before native mutation"):
    val plan = base
    val view = plan.views.head
    val camera = view.camera.copy(projectionMatrix = new FloatBufferView(Array(
      .5f, 0f, 0f, .25f, 0f, .5f, 0f, -.25f,
      0f, 0f, -.2f, -1.02f, 0f, 0f, 0f, 1f)))
    val composed = ok(SurfaceCompositionPlan.make(Vector(view.copy(camera = camera)), plan.resources))
    assert(JavaFxSurfaceComposition.resourcePlan(composed).isLeft)
    assert(JavaFxSurfaceBackend.validateCapabilities(SurfaceFaceFixture.plan.copy(camera = camera)).isLeft)


  test("shared atlas refuses per-view lighting disagreement"):
    val plan = base
    val view = plan.views.head
    val views = Vector(view.copy(viewport = SurfaceViewport(0, 0, .5, 1)),
      view.copy(id = ok(SurfaceViewId.make("lit")), viewport = SurfaceViewport(.5, 0, .5, 1),
        lighting = SurfaceLighting.Default,
        instances = Vector(view.instances.head.copy(id = ok(SurfaceInstanceId.make("lit-copy"))))))
    assert(JavaFxSurfaceComposition.resourcePlan(ok(SurfaceCompositionPlan.make(views, plan.resources))).isLeft)
