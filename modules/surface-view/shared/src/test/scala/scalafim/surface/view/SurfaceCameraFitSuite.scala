package scalafim.surface.view

import munit.FunSuite
import scalafim.surface.*

/** Projects world vertices independently of backend fitting and rasterization. */
class SurfaceCameraFitSuite extends FunSuite:
  test("initial perspective camera contains anatomical bounds across field of view and layout"):
    for
      fov <- Vector(10.0, 35.0, 90.0)
      bilateral <- Vector(false, true)
      viewpoint <- Vector(SurfaceViewpoint.Anterior, SurfaceViewpoint.Dorsal,
        SurfaceViewpoint.Lateral(CorticalHemisphere.Left))
    do
      def asset(name: String, centerX: Double, hemisphere: Hemisphere): SurfaceAsset =
        val points = for x <- Vector(-60.0, 60.0); y <- Vector(-90.0, 90.0); z <- Vector(-65.0, 65.0)
          yield Seq(x + centerX, y - 20.0, z + 20.0)
        val faces = Seq((0,1,2),(1,3,2),(4,6,5),(5,6,7),(0,4,1),(1,4,5),
          (2,3,6),(3,7,6),(0,2,4),(2,6,4),(1,5,3),(3,5,7))
        val geometry = SurfaceGeometry(TriangleMesh.fromRows(points, faces), hemisphere, SurfaceKind.Pial)
        SurfaceAsset.make(SurfaceId.unsafe(name), geometry).toOption.get
      val left = asset("left", -80.0, Hemisphere.Left)
      val right = asset("right", 80.0, Hemisphere.Right)
      val assets = if bilateral then Vector(left,right) else Vector(left)
      val model = SurfaceViewerModel.make(assets, Vector.empty).toOption.get
      val start = SurfaceViewerState.initial(model)
      val camera = SurfaceCamera.unsafe(viewpoint, CameraProjection.Perspective(FieldOfViewDegrees.unsafe(fov)))
      val state = start.copy(camera = camera, layout = if bilateral then SurfaceLayout.Bilateral(left.id,right.id) else start.layout)
      val plan = SurfaceCompiler.compile(model,state).toOption.get
      val view = plan.camera.viewMatrix
      val projection = plan.camera.projectionMatrix
      for slot <- plan.slots do
        val positions = plan.meshes.find(_.surface == slot.surface).get.positions
        var i = 0
        while i < positions.length do
          val world = Vector(positions(i).toDouble + slot.worldOffsetX,
            positions(i+1).toDouble + slot.worldOffsetY, positions(i+2).toDouble + slot.worldOffsetZ, 1.0)
          val eye = Vector.tabulate(4)(row => (0 until 4).map(col => view(row*4+col).toDouble * world(col)).sum)
          val clip = Vector.tabulate(4)(row => (0 until 4).map(col => projection(row*4+col).toDouble * eye(col)).sum)
          assert(clip(3) > 0.0)
          assert(math.abs(clip(2)/clip(3)) < 1.0, s"vertex outside depth planes: $clip")
          assert(math.abs(clip(0)/clip(3)) < 1.0 && math.abs(clip(1)/clip(3)) < 1.0,
            s"clipped vertex: fov=$fov bilateral=$bilateral viewpoint=$viewpoint vertex=${i/3} clip=$clip")
          i += 3
