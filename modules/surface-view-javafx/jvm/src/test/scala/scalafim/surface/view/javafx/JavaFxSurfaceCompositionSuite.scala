package scalafim.surface.view.javafx

import scalafim.surface.view.*

class JavaFxSurfaceCompositionSuite extends munit.FunSuite:
  private def ok[E, A](value: Either[E, A]): A = value.fold(error => fail(s"$error"), identity)

  private def base: SurfaceCompositionPlan =
    ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))

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


  test("shared atlas refuses per-view lighting disagreement"):
    val plan = base
    val view = plan.views.head
    val views = Vector(view.copy(viewport = SurfaceViewport(0, 0, .5, 1)),
      view.copy(id = ok(SurfaceViewId.make("lit")), viewport = SurfaceViewport(.5, 0, .5, 1),
        lighting = SurfaceLighting.Default,
        instances = Vector(view.instances.head.copy(id = ok(SurfaceInstanceId.make("lit-copy"))))))
    assert(JavaFxSurfaceComposition.resourcePlan(ok(SurfaceCompositionPlan.make(views, plan.resources))).isLeft)
