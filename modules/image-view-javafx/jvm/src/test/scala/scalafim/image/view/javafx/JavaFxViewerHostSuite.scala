package scalafim.image.view.javafx

import intaglio.*
import intaglio.javafx.JavaFxProgram
import scalafim.image.*
import scalafim.image.view.*

class JavaFxViewerHostSuite extends munit.FunSuite:

  private val space = VolumeSpace(NeuroSpace(Vector(3, 3, 3)))
  private val volume = NeuroVol.copyFromCanonicalArray(
    PrimitiveBuffers.fillConst[Double](space.nVoxels, 1.0),
    space.toNeuroSpace,
    "javafx"
  )
  private val layer = SliceLayer(
    LayerId.unsafe("anatomy"),
    volume,
    SliceSampling.Linear(),
    ScalarColorizer(DisplayWindow.unsafe(0.0, 2.0))
  )
  private val model = ViewerModel.unsafe(space, Vector(layer))
  private val session = ViewerSession(
    ViewerState.centered(space),
    DeviceContext.unsafe(500.0, 350.0)
  )

  test("JavaFX host compiles viewer scenes without starting the toolkit") {
    val compiled = JavaFxViewerHost.compile(model, session).toOption.get

    assertEquals(JavaFxProgram.validate(compiled.program), None)
    assertEquals(compiled.frame.device, session.device)
  }

  test("JavaFX device events translate through shared viewer receipts") {
    val compiled = JavaFxViewerHost.compile(model, session).toOption.get
    val panel = compiled.frame.panels.coronal.rect
    val x = (panel.left + panel.width / 2.0) * session.device.width
    val y = (1.0 - panel.bottom - panel.height / 2.0) * session.device.height

    val action = JavaFxViewerHost.pickAction(compiled, x, y).toOption.get
    action match
      case ViewerAction.Pick(plane, _) => assertEquals(plane, AnatomicalPlane.Coronal)
      case other => fail(s"expected pick action, got $other")
  }
