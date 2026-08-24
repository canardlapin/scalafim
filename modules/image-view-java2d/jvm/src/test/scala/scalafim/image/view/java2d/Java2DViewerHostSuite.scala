package scalafim.image.view.java2d

import intaglio.*
import intaglio.java2d.Java2DProgram
import scalafim.image.*
import scalafim.image.view.*

class Java2DViewerHostSuite extends munit.FunSuite:

  private val space = VolumeSpace(SampleSpaces(Vector(3, 3, 3)))
  private val volume = SomeScalarVolume.unsafeCopyFromCanonicalArray(
    PrimitiveBuffers.fillConst[Double](space.nVoxels, 1.0),
    space.toSampleSpace,
    "java2d"
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
    DeviceContext.unsafe(400.0, 300.0)
  )

  test("Java2D host compiles and renders a real ARGB image") {
    val (compiled, image) = Java2DViewerHost.renderImage(model, session).toOption.get

    assertEquals(Java2DProgram.validate(compiled.program), None)
    assertEquals(image.getWidth, 400)
    assertEquals(image.getHeight, 300)
    val panel = compiled.frame.panels.axial.rect
    val x = math.round((panel.left + panel.width / 2.0) * session.device.width).toInt
    val y = math.round((1.0 - panel.bottom - panel.height / 2.0) * session.device.height).toInt
    assert(((image.getRGB(x, y) >>> 24) & 0xff) > 0)
  }

  test("Java2D device events identify their anatomical panel") {
    val compiled = Java2DViewerHost.compile(model, session).toOption.get
    val panel = compiled.frame.panels.sagittal.rect
    val x = (panel.left + panel.width / 2.0) * session.device.width
    val y = (1.0 - panel.bottom - panel.height / 2.0) * session.device.height

    val action = Java2DViewerHost.scrollAction(compiled, x, y, 1).toOption.get
    assertEquals(action, ViewerAction.Scroll(AnatomicalPlane.Sagittal, 1))
  }
