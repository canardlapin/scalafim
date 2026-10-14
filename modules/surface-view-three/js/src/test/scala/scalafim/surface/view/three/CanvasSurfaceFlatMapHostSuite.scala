package scalafim.surface.view.three

import scala.scalajs.js

import scalafim.surface.view.*

class CanvasSurfaceFlatMapHostSuite extends munit.FunSuite:
  private val surface = SurfaceId.unsafe("flat-map-host")
  private val map = SurfaceFlatMap.make(surface,
    Vector(SurfaceFlatMapPoint(0, 0), SurfaceFlatMapPoint(2, 0), SurfaceFlatMapPoint(2, 1), SurfaceFlatMapPoint(0, 1)),
    Vector((0, 1, 2), (0, 2, 3))).toOption.get
  private val viewport = SurfaceFlatMapViewport.make(100, 100, 10).toOption.get

  private final class Fixture:
    var failFill = false
    private val context = js.Dynamic.literal(
      clearRect = ((_: Double, _: Double, _: Double, _: Double) => ()): js.Function4[Double, Double, Double, Double, Unit],
      fillRect = ((_: Double, _: Double, _: Double, _: Double) => ()): js.Function4[Double, Double, Double, Double, Unit],
      beginPath = (() => ()): js.Function0[Unit],
      moveTo = ((_: Double, _: Double) => ()): js.Function2[Double, Double, Unit],
      lineTo = ((_: Double, _: Double) => ()): js.Function2[Double, Double, Unit],
      closePath = (() => ()): js.Function0[Unit],
      fill = (() => if failFill then throw new IllegalStateException("injected flat-map draw failure")): js.Function0[Unit],
      stroke = (() => ()): js.Function0[Unit],
      fillText = ((_: String, _: Double, _: Double) => ()): js.Function3[String, Double, Double, Unit],
      arc = ((_: Double, _: Double, _: Double, _: Double, _: Double) => ()): js.Function5[Double, Double, Double, Double, Double, Unit]
    )
    val canvas = js.Dynamic.literal(getContext = ((_: String) => context): js.Function1[String, js.Dynamic])
    val host = CanvasSurfaceFlatMapHost.attach(canvas).toOption.get

  test("failed publication clears stale picks and disposed hosts cannot resurrect"):
    val f = new Fixture
    assertEquals(f.host.hoverAt(10, 70), Left(CanvasSurfaceFlatMapHostError.NoFrame))
    assert(f.host.render(map, viewport).isRight)
    assertEquals(f.host.selectAt(Double.NaN, 70).left.toOption, Some(CanvasSurfaceFlatMapHostError.Pointer("coordinates and radius must be finite; radius must be positive")))
    f.failFill = true
    assert(f.host.resize(viewport).isLeft)
    assertEquals(f.host.hoverAt(10, 70), Left(CanvasSurfaceFlatMapHostError.NoFrame))
    f.host.dispose()
    assertEquals(f.host.render(map, viewport), Left(CanvasSurfaceFlatMapHostError.Disposed))
    assertEquals(f.host.selectAt(10, 70), Left(CanvasSurfaceFlatMapHostError.Disposed))

  test("resize preserves map-space ROI drafts and replacing geometry clears them"):
    val f = new Fixture
    assert(f.host.render(map, viewport).isRight)
    assert(f.host.beginRoi(SurfaceFlatMapRoiMode.Polygon, 5, 5).isRight)
    assert(f.host.appendRoiPoint(95, 5).isRight)
    assert(f.host.appendRoiPoint(95, 95).isRight)
    assert(f.host.appendRoiPoint(5, 95).isRight)
    assert(f.host.resize(SurfaceFlatMapViewport.make(200, 150, 10).toOption.get).isRight)
    assert(f.host.finishRoi("region").isRight)
    assertEquals(f.host.renderedRois.head.vertices.map(_.index), Vector(0, 1, 2, 3))
    assert(f.host.resize(viewport).isRight)
    assertEquals(f.host.renderedRois.size, 1)
    assert(f.host.beginRoi(SurfaceFlatMapRoiMode.Polygon, 5, 5).isRight)
    val replacement = SurfaceFlatMap.make(surface,
      Vector(SurfaceFlatMapPoint(0, 0), SurfaceFlatMapPoint(4, 0), SurfaceFlatMapPoint(4, 2), SurfaceFlatMapPoint(0, 2)),
      Vector((0, 1, 2), (0, 2, 3))).toOption.get
    assert(f.host.render(replacement, viewport).isRight)
    assertEquals(f.host.renderedRois, Vector.empty)
    assert(f.host.finishRoi("stale").isLeft)
