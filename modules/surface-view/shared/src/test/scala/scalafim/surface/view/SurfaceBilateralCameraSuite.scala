package scalafim.surface.view

import scalafim.surface.*

class SurfaceBilateralCameraSuite extends munit.FunSuite:
  private def plan(offset: Double, order: BilateralOrder): SurfaceRenderPlan =
    def asset(id: String, hemisphere: Hemisphere, x: Double): SurfaceAsset =
      val mesh = TriangleMesh.fromRows(
        Vector(Vector(x, 0.0, 0.0), Vector(x + 8, 0.0, 0.0), Vector(x, 6.0, 0.0)),
        Vector((0, 1, 2)))
      SurfaceAsset.make(SurfaceId.unsafe(id), SurfaceGeometry(mesh, hemisphere, SurfaceKind.Inflated)).toOption.get
    val left = asset("left", Hemisphere.Left, 0)
    val right = asset("right", Hemisphere.Right, offset)
    val model = SurfaceViewerModel.make(Vector(left, right), Vector.empty).toOption.get
    val state = SurfaceViewer.reduce(model, SurfaceViewerState.initial(model),
      SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(left.id, right.id, order))).toOption.get
    SurfaceCompiler.compile(model, state).toOption.get

  test("bilateral camera distance follows centered extents, independently of inter-mesh translation"):
    for order <- BilateralOrder.values do
      val baseline = plan(0, order)
      val translated = plan(100, order)
      // Each 8x6 triangle has bounding radius 5. A sphere of radius 5
      // subtends the 17.5-degree half-field at distance 16.6275476,
      // irrespective of the original inter-mesh gap.
      def centerDepth(value: SurfaceRenderPlan, centerX: Double): Double =
        val view = value.camera.viewMatrix
        view(8) * centerX + view(9) * 3.0 + view(11)
      assertEqualsDouble(centerDepth(baseline, 4), -16.6275476, 1e-5)
      assertEqualsDouble(centerDepth(translated, 54), -16.6275476, 1e-5)
