package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

class JavaFxSurfaceLayerTransactionSuite extends munit.FunSuite:
  private val leftId = SurfaceId.unsafe("left")
  private val rightId = SurfaceId.unsafe("right")
  private val leftLayer = SurfaceLayerId.unsafe("left-color")
  private val rightLayer = SurfaceLayerId.unsafe("right-color")

  private def geometry(hemisphere: Hemisphere, offset: Double): SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Seq(
          Seq(offset, 0.0, 0.0),
          Seq(offset + 1.0, 0.0, 0.0),
          Seq(offset, 1.0, 0.0)
        ),
        Seq((0, 1, 2))
      ),
      hemisphere,
      SurfaceKind.Inflated
    )

  private val left = geometry(Hemisphere.Left, -2.0)
  private val right = geometry(Hemisphere.Right, 2.0)

  private def plan(leftColor: Rgba32, rightColor: Rgba32): SurfaceRenderPlan =
    val model = SurfaceViewerModel.make(
      Vector(
        SurfaceAsset.make(leftId, left).toOption.get,
        SurfaceAsset.make(rightId, right).toOption.get
      ),
      Vector(
        SurfaceLayer.packedRgba(leftLayer, leftId, left, Vector.fill(3)(leftColor)).toOption.get,
        SurfaceLayer.packedRgba(rightLayer, rightId, right, Vector.fill(3)(rightColor)).toOption.get
      )
    ).toOption.get
    val bilateral = SurfaceViewer.reduce(
      model,
      SurfaceViewerState.initial(model),
      SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(leftId, rightId))
    ).toOption.get
    SurfaceCompiler.compile(model, bilateral).toOption.get

  test("layer dirt is conditional and stable warm updates do not upload geometry"):
    val red = Rgba32.unsafe(255, 0, 0)
    val blue = Rgba32.unsafe(0, 0, 255)
    val before = plan(red, red)
    val identical = JavaFxSurfaceProgram.compile(Some(before), plan(red, red))
    assert(identical.dirty.isClean)

    val changed = JavaFxSurfaceProgram.compile(Some(before), plan(blue, blue))
    assert(changed.dirty.layerData)
    assert(!changed.dirty.geometry)
    assertEquals(changed.commands.collect { case JavaFxSurfaceCommand.UpdateAtlases(_) => 1 }.sum, 1)

  test("a second-hemisphere preflight failure leaves both atlases intact and permits recovery"):
    val red = Rgba32.unsafe(255, 0, 0)
    val blue = Rgba32.unsafe(0, 0, 255)
    val before = plan(red, red)
    val next = plan(blue, blue)
    val result = JavaFxSurfaceProbe.compile(before).toOption.get
    val leftPixel = result.chunks.find(_.surface == leftId).get.atlas.image.getPixelReader.getArgb(1, 1)
    val rightPixel = result.chunks.find(_.surface == rightId).get.atlas.image.getPixelReader.getArgb(1, 1)
    val brokenRight = next.meshes.find(_.surface == rightId).get.copy(indices = new IntBufferView(Array.emptyIntArray))
    val broken = next.copy(meshes = next.meshes.map(packet => if packet.surface == rightId then brokenRight else packet))

    assert(result.updateColors(broken).left.toOption.exists(_.message.contains("right")))
    assertEquals(result.chunks.find(_.surface == leftId).get.atlas.image.getPixelReader.getArgb(1, 1), leftPixel)
    assertEquals(result.chunks.find(_.surface == rightId).get.atlas.image.getPixelReader.getArgb(1, 1), rightPixel)

    val recovered = result.updateColors(next).toOption.get
    assertEquals(recovered.atlasesUpdated, 2)
    assert(result.chunks.find(_.surface == leftId).get.atlas.image.getPixelReader.getArgb(1, 1) != leftPixel)
    assert(result.chunks.find(_.surface == rightId).get.atlas.image.getPixelReader.getArgb(1, 1) != rightPixel)
