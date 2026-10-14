package scalafim.surface.view.three

import scalafim.surface.view.*

class ThreeSurfaceFaceSuite extends munit.FunSuite:
  test("face colors remain constant per triangle in native upload buffers"):
    val plan = SurfaceFaceFixture.plan
    val program = ThreeSurfaceProgram.compile(None, plan, ThreeCanvasSize.unsafe(256, 256)).toOption.get
    val colors = program.commands.collectFirst:
      case ThreeSurfaceCommand.UploadColors(colors) => colors.head.rgb
    .get
    assertEquals(colors.length, 18)
    for vertex <- 0 until 6 do
      assertEqualsDouble(colors(vertex * 3).toDouble, if vertex < 3 then 1.0 else 0.0, 0.0)
      assertEqualsDouble(colors(vertex * 3 + 2).toDouble, if vertex < 3 then 0.0 else 1.0, 0.0)
    assertEquals(plan.meshes.head.sourceVertex(5), 3)
