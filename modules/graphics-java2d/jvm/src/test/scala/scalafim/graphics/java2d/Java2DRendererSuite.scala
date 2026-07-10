package scalafim.graphics.java2d

import java.awt.Color
import java.awt.image.BufferedImage
import scalafim.graphics.*

class Java2DRendererSuite extends munit.FunSuite:

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
    val options = Java2DOptions.unsafe(width = 200, height = 100)

    val left = Java2DRenderer.compile(scene, options).fold(e => fail(e.message), identity)
    val right = Java2DRenderer.compile(scene, options).fold(e => fail(e.message), identity)

    assertEquals(left, right)
    assert(left.commands(0).isInstanceOf[Java2DCommand.Disc])
    assert(left.commands(1).isInstanceOf[Java2DCommand.Rectangle])
  }

  test("real BufferedImage rendering preserves fill color and combined alpha") {
    val rect = Grob.rectUnsafe(
      Point.npcUnsafe(0.5, 0.5),
      Size.npcUnsafe(0.5, 0.5),
      gp = GraphicParams.unsafe(
        stroke = None,
        fill = Some(Rgba.unsafe(200, 40, 20, 0.5)),
        alpha = 0.5
      )
    )
    val options = Java2DOptions.unsafe(width = 80, height = 60)
    val program = Java2DRenderer.compile(Scene(Vector(rect)), options).fold(e => fail(e.message), identity)
    val image = new BufferedImage(options.width, options.height, BufferedImage.TYPE_INT_ARGB)
    val graphics = image.createGraphics()
    try Java2DRenderer.draw(program, graphics)
    finally graphics.dispose()

    val pixel = new Color(image.getRGB(40, 30), true)
    assert(math.abs(pixel.getRed - 200) <= 1)
    assert(math.abs(pixel.getGreen - 40) <= 1)
    assert(math.abs(pixel.getBlue - 20) <= 1)
    assertEquals(pixel.getAlpha, 64)
    assertEquals(new Color(image.getRGB(0, 0), true).getAlpha, 0)
  }

  test("invalid image dimensions return typed errors") {
    assertEquals(
      Java2DOptions(width = 20, height = 0).left.toOption,
      Some(Java2DRenderError.InvalidImageSize(20, 0))
    )
  }
