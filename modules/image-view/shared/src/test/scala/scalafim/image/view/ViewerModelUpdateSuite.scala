package scalafim.image.view

import intaglio.*
import scalafim.image.*
import scalafim.image.SampleSpaces.*

class ViewerModelUpdateSuite extends munit.FunSuite:
  private val support = SampleSpaces.requireVolumeD3(SampleSpaces(Vector(3, 3, 3))).toOption.get
  private val volume = SomeMaskVolume.unsafeCopyFromCanonicalArray(PrimitiveBuffers.fillConst[Boolean](27, true), support, "mask")
  private val red = Rgba32.unsafe(255, 0, 0, 255)
  private val blue = Rgba32.unsafe(0, 0, 255, 255)
  private val green = Rgba32.unsafe(0, 255, 0, 255)
  private def layer(name: String, color: Rgba32): SliceLayer =
    SliceLayer(LayerId.unsafe(name), volume, SliceSampling.Nearest(false), MaskColorizer(color))
  private val a = layer("a", red)
  private val b = layer("b", blue)
  private val c = layer("c", green)
  private val model = ViewerModel.unsafe(support.grid, Vector(a, b, c))
  private val session = ViewerSession(ViewerState.centered(support.grid).copy(showCrosshair = false), DeviceContext.unsafe(300, 300))
  private def pixels(frame: ViewerFrame): Vector[Rgba32] =
    frame.scene.grobs.collect { case group: Grob.Group => group }.flatMap(_.children.collect {
      case image: Grob.Image => image.image.pixelUnsafe(1, 1)
    })

  test("partial reorder preserves unmentioned layers and rejects unknown/duplicate ids"):
    val next = model.updated(ViewerModelUpdate.ReorderLayers(Vector(c.id))).toOption.get
    assertEquals(next.layers, Vector(c, a, b))
    assertEquals(model.updated(ViewerModelUpdate.ReorderLayers(Vector.empty)).toOption.get, model)
    assertEquals(model.updated(ViewerModelUpdate.ReorderLayers(Vector(a.id, a.id))), Left(ImageViewError.DuplicateLayerId(a.id)))
    val unknown = LayerId.unsafe("unknown")
    assertEquals(model.updated(ViewerModelUpdate.ReorderLayers(Vector(unknown))), Left(ImageViewError.UnknownLayer(unknown)))
    assertEquals(model.updated(ViewerModelUpdate.ReplaceLayer(layer("unknown", red))), Left(ImageViewError.UnknownLayer(unknown)))

  test("reorder changes compositing sequence while every unchanged raster stays cached"):
    val first = session.compileCached(model, ViewerCache.empty(24)).toOption.get
    val next = model.updated(ViewerModelUpdate.ReorderLayers(Vector(c.id))).toOption.get
    val reordered = session.compileCached(next, first.cache).toOption.get
    assertEquals(pixels(first.frame), Vector.fill(3)(Vector(red, blue, green)).flatten)
    assertEquals(pixels(reordered.frame), Vector.fill(3)(Vector(green, red, blue)).flatten)
    assertEquals(reordered.profile.cacheHits, 9)
    assertEquals(reordered.profile.sourceReads, 0)
    assertEquals(reordered.profile.colorizedPixels, 0L)

  test("typed replacement changes pixels without stale color-bound sample reuse"):
    val first = session.compileCached(model, ViewerCache.empty(24)).toOption.get
    val changed = model.updated(ViewerModelUpdate.ReplaceLayer(layer("b", red))).toOption.get
    val second = session.compileCached(changed, first.cache).toOption.get
    assertEquals(pixels(second.frame), Vector.fill(3)(Vector(red, red, green)).flatten)
    assertEquals(second.profile.cacheHits, 6)
    assertEquals(second.profile.cacheMisses, 3)
    assertEquals(second.profile.sourceReads, 1)
    assert(second.profile.colorizedPixels > 0)
    assertEquals(second.frame.readouts, first.frame.readouts)

  test("session validation refuses unsupported presentation rather than silently dropping it"):
    val incompatible = session.copy(state = session.state.copy(layerPresentation = Map(a.id -> LayerPresentation(window = Some(DisplayWindow.unsafe(0, 1))))))
    assertEquals(incompatible.validateModel(model), Left(ImageViewError.WindowUnsupported(a.id)))
    val threshold = DisplayThreshold.transparentBand(0.0, 0.5).toOption.get
    val thresholded = session.copy(state = session.state.copy(layerPresentation = Map(b.id -> LayerPresentation(threshold = Some(threshold)))))
    assertEquals(thresholded.validateModel(model), Left(ImageViewError.ThresholdUnsupported(b.id)))
    assert(session.validateModel(model).isRight)
    assertEquals(
      session.copy(state = session.state.copy(timepoint = 1)).validateModel(model),
      Left(ImageViewError.TimepointOutOfBounds(1, 1))
    )
