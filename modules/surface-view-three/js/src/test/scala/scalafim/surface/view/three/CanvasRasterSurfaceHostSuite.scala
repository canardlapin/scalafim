package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8ClampedArray
import intaglio.*
import scalafim.surface.view.*

class CanvasRasterSurfaceHostSuite extends munit.FunSuite:
  private val plan = SurfaceCompositionPlan.fromLegacy(SurfaceVisibilityFixture.plan("gray")).toOption.get
  private val size = RasterDimensions.unsafe(32, 32)
  private final class Fixture:
    var failCreate = false
    var failPut = false
    var puts = 0
    val context = js.Dynamic.literal(
      createImageData = ((width: Int, height: Int) =>
        if failCreate then throw new IllegalStateException("injected staging failure")
        js.Dynamic.literal(data = new Uint8ClampedArray(width * height * 4))): js.Function2[Int, Int, js.Dynamic],
      putImageData = ((data: js.Dynamic, x: Int, y: Int) =>
        if failPut then throw new IllegalStateException("injected publication failure")
        puts += 1): js.Function3[js.Dynamic, Int, Int, Unit])
    val canvas = js.Dynamic.literal(
      getContext = ((name: String) => context): js.Function1[String, js.Dynamic],
      getBoundingClientRect = (() => js.Dynamic.literal(left = 10, top = 20, width = 32, height = 32)): js.Function0[js.Dynamic])
    val host = CanvasRasterSurfaceHost.attach(canvas).toOption.get

  test("staging failure preserves the old frame; publication failure invalidates stale picks"):
    val f = new Fixture
    assert(f.host.submit(plan, size).isRight)
    val previous = f.host.presentedFrame
    f.failCreate = true
    assert(f.host.submit(plan, size).isLeft)
    assertEquals(f.host.presentedFrame, previous)
    assert(f.host.pickDisplay(16, 16).toOption.get.nonEmpty)
    f.failCreate = false
    f.failPut = true
    assert(f.host.submit(plan, size).isLeft)
    assertEquals(f.host.presentedFrame, None)
    assertEquals(f.host.pickDisplay(16, 16), Left(CanvasRasterSurfaceHostError.NoFrame))

  test("CSS borders and padding are excluded before mapping content coordinates"):
    val f = new Fixture
    assert(f.host.submit(plan, size).isRight)
    f.canvas.updateDynamic("offsetWidth")(56)
    f.canvas.updateDynamic("offsetHeight")(56)
    f.canvas.updateDynamic("getBoundingClientRect")((() => js.Dynamic.literal(left = 10, top = 20, width = 112, height = 112)): js.Function0[js.Dynamic])
    val style = js.Dynamic.literal(transform = "matrix(2, 0, 0, 2, 0, 0)",
      borderLeftWidth = "10px", borderRightWidth = "10px", borderTopWidth = "10px", borderBottomWidth = "10px",
      paddingLeft = "2px", paddingRight = "2px", paddingTop = "2px", paddingBottom = "2px")
    f.canvas.updateDynamic("ownerDocument")(js.Dynamic.literal(defaultView = js.Dynamic.literal(
      getComputedStyle = ((canvas: js.Dynamic) => style): js.Function1[js.Dynamic, js.Dynamic])))
    assertEquals(f.host.pickDisplay(12, 56), Right(None))
    assertEquals(f.host.pickDisplay(56, 12), Right(None))
    val center = f.host.pickDisplay(56, 56).toOption.get
    assert(center.nonEmpty)
    assertEquals(f.host.pickClient(66, 76), Right(center))
    style.updateDynamic("transform")("matrix(1, 1, 0, 1, 0, 0)")
    assert(f.host.pickDisplay(56, 56).isLeft)

  private def lower(p: SurfaceCompositionPlan, d: RasterDimensions): Either[String, SurfaceDecorationGeometry] =
    SurfaceDecorationCompiler.compile(Vector.empty, p).flatMap(packets =>
      SurfaceDecorationGeometry.lower(packets, p, d.width, d.height)).left.map(_.toString)

  test("reentrant lowering cannot resurrect a disposed host or overwrite a newer frame"):
    val disposed = new Fixture
    val result = disposed.host.submitDecorated(plan, size, (p, d) =>
      disposed.host.dispose()
      lower(p, d))
    assertEquals(result, Left(CanvasRasterSurfaceHostError.Disposed))
    assertEquals(disposed.puts, 0)
    assertEquals(disposed.host.presentedFrame, None)
    val newer = new Fixture
    val superseded = newer.host.submitDecorated(plan, size, (p, d) =>
      assert(newer.host.submit(plan, RasterDimensions.unsafe(24, 24)).isRight)
      lower(p, d))
    assertEquals(superseded, Left(CanvasRasterSurfaceHostError.Superseded))
    assertEquals(newer.puts, 1)
    assertEquals(newer.host.presentedFrame.get.dimensions, RasterDimensions.unsafe(24, 24))
