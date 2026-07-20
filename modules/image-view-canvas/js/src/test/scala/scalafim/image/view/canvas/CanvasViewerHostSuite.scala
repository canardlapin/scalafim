package scalafim.image.view.canvas

import scalafim.graphics.*
import scalafim.graphics.canvas.CanvasProgram
import scalafim.image.*
import scalafim.image.view.*

class CanvasViewerHostSuite extends munit.FunSuite:

  private val space = VolumeSpace(NeuroSpace(Vector(3, 3, 3)))
  private val volume = NeuroVol.fromLinear(
    NArrayUtil.tabulate[Double](space.nVoxels)(_.toDouble),
    space.toNeuroSpace,
    "canvas"
  )
  private val layer = SliceLayer(
    LayerId.unsafe("anatomy"),
    volume,
    SliceSampling.Linear(),
    ScalarColorizer(DisplayWindow.unsafe(0.0, space.nVoxels.toDouble - 1.0))
  )
  private val model = ViewerModel.unsafe(space, Vector(layer))
  private val session = ViewerSession(
    ViewerState.centered(space),
    DeviceContext.unsafe(640.0, 480.0)
  )

  test("Canvas host compiles viewer scenes through the existing backend") {
    val compiled = CanvasViewerHost.compile(model, session).toOption.get

    assertEquals(CanvasProgram.validate(compiled.program), None)
    assertEquals(compiled.frame.device, session.device)
  }

  test("Canvas-relative pointer and wheel input become pure viewer actions") {
    val compiled = CanvasViewerHost.compile(model, session).toOption.get
    val panel = compiled.frame.panels.axial
    val rootX = panel.rect.left + panel.rect.width / 2.0
    val rootY = panel.rect.bottom + panel.rect.height / 2.0
    val deviceX = rootX * session.device.width
    val deviceY = (1.0 - rootY) * session.device.height

    val pick = CanvasViewerHost.pickAction(compiled, deviceX, deviceY).toOption.get
    val scroll = CanvasViewerHost.scrollAction(compiled, deviceX, deviceY, -2).toOption.get
    assert(pick.isInstanceOf[ViewerAction.Pick])
    assertEquals(scroll, ViewerAction.Scroll(AnatomicalPlane.Axial, -2))
    assert(CanvasViewerHost.pickAction(compiled, 639.0, 479.0).isLeft)
  }
