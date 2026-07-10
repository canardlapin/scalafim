package scalafim.graphics.canvas

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import scalafim.graphics.*

class CanvasRendererSuite extends munit.FunSuite:

  test("program compilation is deterministic and preserves draw order") {
    val first = Grob.circleUnsafe(
      Point.npcUnsafe(0.25, 0.5),
      ExtentExpr.pointsUnsafe(4.0),
      name = Some(GraphicsName.unsafe("first"))
    )
    val second = Grob.rectUnsafe(
      Point.npcUnsafe(0.75, 0.5),
      Size.npcUnsafe(0.2, 0.3),
      name = Some(GraphicsName.unsafe("second"))
    )
    val scene = Scene(Vector(first, second))
    val options = CanvasOptions.unsafe(width = 200, height = 100)

    val left = CanvasRenderer.compile(scene, options).fold(e => fail(e.message), identity)
    val right = CanvasRenderer.compile(scene, options).fold(e => fail(e.message), identity)

    assertEquals(left, right)
    assert(left.commands(0).isInstanceOf[CanvasCommand.Disc])
    assert(left.commands(1).isInstanceOf[CanvasCommand.Rectangle])
  }

  test("draw interprets the deterministic program against a Canvas 2D context") {
    val calls = ArrayBuffer.empty[String]
    def noArgs(label: String): js.Function0[Unit] =
      () =>
        calls += label
        ()
    val context = js.Dynamic
      .literal(
        save = noArgs("save"),
        restore = noArgs("restore"),
        beginPath = noArgs("beginPath"),
        closePath = noArgs("closePath"),
        fill = noArgs("fill"),
        stroke = noArgs("stroke"),
        clip = noArgs("clip"),
        moveTo = ((_: Double, _: Double) => calls += "moveTo"): js.Function2[Double, Double, Unit],
        lineTo = ((_: Double, _: Double) => calls += "lineTo"): js.Function2[Double, Double, Unit],
        rect = ((_: Double, _: Double, _: Double, _: Double) => calls += "rect"): js.Function4[Double, Double, Double, Double, Unit],
        arc = ((_: Double, _: Double, _: Double, _: Double, _: Double, _: Boolean) => calls += "arc"): js.Function6[Double, Double, Double, Double, Double, Boolean, Unit],
        translate = ((_: Double, _: Double) => calls += "translate"): js.Function2[Double, Double, Unit],
        rotate = ((_: Double) => calls += "rotate"): js.Function1[Double, Unit],
        setLineDash = ((_: js.Array[Double]) => calls += "dash"): js.Function1[js.Array[Double], Unit],
        fillText = ((_: String, _: Double, _: Double) => calls += "fillText"): js.Function3[String, Double, Double, Unit],
        strokeStyle = "",
        fillStyle = "",
        globalAlpha = 1.0,
        lineWidth = 1.0,
        font = "",
        textAlign = "start",
        textBaseline = "alphabetic"
      )
      .asInstanceOf[CanvasRenderingContext2D]
    val line = Grob
      .lines(
        Vector(Point.npcUnsafe(0.1, 0.2), Point.npcUnsafe(0.9, 0.8)),
        gp = GraphicParams.unsafe(stroke = Some(Rgba.unsafe(10, 20, 30)), lineType = LineType.Dashed)
      )
      .toOption
      .get
    val program = CanvasRenderer
      .compile(Scene(Vector(line)), CanvasOptions.unsafe(width = 100, height = 80))
      .fold(e => fail(e.message), identity)

    CanvasRenderer.draw(program, context)

    assertEquals(calls.toVector, Vector("save", "beginPath", "moveTo", "lineTo", "dash", "stroke", "restore"))
  }

  test("invalid canvas dimensions return typed errors") {
    assertEquals(
      CanvasOptions(width = 0, height = 20).left.toOption,
      Some(CanvasRenderError.InvalidCanvasSize(0, 20))
    )
  }
