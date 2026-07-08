package scalafim.graphics.svg

import scalafim.graphics.*

class SvgRendererSuite extends munit.FunSuite:

  private def occurrences(value: String, needle: String): Int =
    var count = 0
    var from = 0
    var next = value.indexOf(needle, from)
    while next >= 0 do
      count += 1
      from = next + needle.length
      next = value.indexOf(needle, from)
    count

  private def assertSvgEquals(scene: Scene, expected: String, options: SvgOptions = SvgOptions.default): Unit =
    assertEquals(SvgRenderer.render(scene, options).toOption.get.value, expected)

  test("renders a deterministic SVG document for basic grobs") {
    val point =
      Grob.points(
        Vector(Point.npcUnsafe(0.5, 0.25)),
        gp = GraphicParams.unsafe(fill = Some(Rgba.unsafe(40, 80, 120, 0.5)))
      ).toOption.get
    val line =
      Grob.segments(
        Vector((Point.nativeUnsafe(0.0, 0.0), Point.nativeUnsafe(10.0, 20.0))),
        gp = GraphicParams.unsafe(stroke = Some(Rgba.unsafe(200, 10, 5)), lineWidth = 2.0, lineType = LineType.Dashed)
      ).toOption.get
    val label =
      Grob.text(
        "A&B <test>",
        Point.nativeUnsafe(10.0, 20.0),
        anchor = Anchor(HJust.Left, VJust.Top),
        gp = GraphicParams.unsafe(fontFamily = Some("Inter"))
      )
    val scene =
      Scene(Vector(point, line, label))

    val svg =
      SvgRenderer
        .render(scene, SvgOptions.unsafe(width = 120, height = 80, title = Some("Smoke & SVG")))
        .toOption
        .get
        .value

    assert(svg.contains("""<svg xmlns="http://www.w3.org/2000/svg" width="120" height="80" viewBox="0 0 120 80">"""))
    assert(svg.contains("<title>Smoke &amp; SVG</title>"))
    assert(svg.contains("""<circle stroke="#000000" fill="#285078" fill-opacity="0.5" stroke-width="1" cx="50%" cy="25%" r="4pt" />"""))
    assert(svg.contains("""<line stroke="#c80a05" fill="none" stroke-width="2" stroke-dasharray="6 4" x1="0" y1="0" x2="10" y2="20" />"""))
    assert(svg.contains("""<text fill="#000000" stroke="none" font-family="Inter" font-size="12pt" x="10" y="20" text-anchor="start" dominant-baseline="text-before-edge">A&amp;B &lt;test&gt;</text>"""))
    assert(svg.endsWith("</svg>\n"))
  }

  test("renders an x axis from baseline, tickmark segments, and labels") {
    val range = Interval.unsafe(0.0, 10.0)
    val ticks = Axis.ticks(range, Breaks.countUnsafe(3)).toOption.get
    val viewport =
      Viewport(
        origin = Point.npcUnsafe(0.1, 0.85),
        size = Size.npcUnsafe(0.8, 0.1),
        xScale = Interval.unsafe(0.0, 10.0),
        yScale = Interval.unsafe(-1.0, 1.0),
        clip = Clip.Off
      )
    val axis =
      Axis
        .bottom(
          range,
          ticks,
          tickLength = 0.4,
          labelOffset = 0.8,
          axisGp = GraphicParams.unsafe(lineWidth = 0.5),
          tickGp = GraphicParams.unsafe(lineWidth = 0.5),
          labelGp = GraphicParams.unsafe(fontSize = Length.pointsUnsafe(8.0)),
          name = Some(GraphicsName.unsafe("x-axis"))
        )
        .flatMap(_.toGrob(Some(viewport)))
        .toOption
        .get

    val svg = SvgRenderer.render(Scene(Vector(axis))).toOption.get.value

    assert(svg.contains("""<svg data-name="x-axis" x="10%" y="85%" width="80%" height="10%" viewBox="0 -1 10 2" overflow="visible">"""))
    assert(svg.contains("""<g data-name="x-axis" stroke="none" fill="none" stroke-width="1">"""))
    assertEquals(occurrences(svg, """data-name="x-axis-baseline""""), 1)
    assertEquals(occurrences(svg, """data-name="x-axis-ticks""""), 3)
    assertEquals(occurrences(svg, """data-name="x-axis-label""""), 3)
    assert(svg.contains("""<line data-name="x-axis-baseline" stroke="#000000" fill="none" stroke-width="0.5" x1="0" y1="0" x2="10" y2="0" />"""))
    assert(svg.contains("""<line data-name="x-axis-ticks" stroke="#000000" fill="none" stroke-width="0.5" x1="5" y1="0" x2="5" y2="0.4" />"""))
    assert(svg.contains("""<text data-name="x-axis-label" fill="#000000" stroke="none" font-size="8pt" x="5" y="0.8" text-anchor="middle" dominant-baseline="text-before-edge">5</text>"""))
  }

  test("renders axis-only SVG golden exactly") {
    val range = Interval.unsafe(0.0, 10.0)
    val ticks = Axis.ticks(range, Breaks.countUnsafe(3)).toOption.get
    val viewport =
      Viewport(
        origin = Point.npcUnsafe(0.1, 0.8),
        size = Size.npcUnsafe(0.8, 0.15),
        xScale = range,
        yScale = Interval.unsafe(-1.0, 1.0),
        clip = Clip.Off
      )
    val axis =
      Axis
        .bottom(
          range,
          ticks,
          tickLength = 0.4,
          labelOffset = 0.8,
          axisGp = GraphicParams.unsafe(lineWidth = 0.5),
          tickGp = GraphicParams.unsafe(lineWidth = 0.5),
          labelGp = GraphicParams.unsafe(fontSize = Length.pointsUnsafe(8.0)),
          name = Some(GraphicsName.unsafe("x-axis"))
        )
        .flatMap(_.toGrob(Some(viewport)))
        .toOption
        .get

    val expected =
      """<svg xmlns="http://www.w3.org/2000/svg" width="200" height="120" viewBox="0 0 200 120">
        |  <svg data-name="x-axis" x="10%" y="80%" width="80%" height="15%" viewBox="0 -1 10 2" overflow="visible">
        |    <g data-name="x-axis" stroke="none" fill="none" stroke-width="1">
        |      <line data-name="x-axis-baseline" stroke="#000000" fill="none" stroke-width="0.5" x1="0" y1="0" x2="10" y2="0" />
        |      <line data-name="x-axis-ticks" stroke="#000000" fill="none" stroke-width="0.5" x1="0" y1="0" x2="0" y2="0.4" />
        |      <line data-name="x-axis-ticks" stroke="#000000" fill="none" stroke-width="0.5" x1="5" y1="0" x2="5" y2="0.4" />
        |      <line data-name="x-axis-ticks" stroke="#000000" fill="none" stroke-width="0.5" x1="10" y1="0" x2="10" y2="0.4" />
        |      <text data-name="x-axis-label" fill="#000000" stroke="none" font-size="8pt" x="0" y="0.8" text-anchor="middle" dominant-baseline="text-before-edge">0</text>
        |      <text data-name="x-axis-label" fill="#000000" stroke="none" font-size="8pt" x="5" y="0.8" text-anchor="middle" dominant-baseline="text-before-edge">5</text>
        |      <text data-name="x-axis-label" fill="#000000" stroke="none" font-size="8pt" x="10" y="0.8" text-anchor="middle" dominant-baseline="text-before-edge">10</text>
        |    </g>
        |  </svg>
        |</svg>
        |""".stripMargin

    assertSvgEquals(Scene(Vector(axis)), expected, SvgOptions.unsafe(width = 200, height = 120))
  }

  test("renders multi-point line grobs as adjacent SVG line segments") {
    val polyline =
      Grob.lines(
        Vector(
          Point.nativeUnsafe(0.0, 0.0),
          Point.nativeUnsafe(1.0, 2.0),
          Point.nativeUnsafe(3.0, 1.0)
        ),
        gp = GraphicParams.unsafe(stroke = Some(Rgba.unsafe(20, 30, 40)), lineType = LineType.Dotted),
        name = Some(GraphicsName.unsafe("trajectory"))
      ).toOption.get

    val svg = SvgRenderer.render(Scene(Vector(polyline))).toOption.get.value

    assertEquals(occurrences(svg, """data-name="trajectory""""), 2)
    assert(svg.contains("""<line data-name="trajectory" stroke="#141e28" fill="none" stroke-width="1" stroke-dasharray="1 3" x1="0" y1="0" x2="1" y2="2" />"""))
    assert(svg.contains("""<line data-name="trajectory" stroke="#141e28" fill="none" stroke-width="1" stroke-dasharray="1 3" x1="1" y1="2" x2="3" y2="1" />"""))
  }

  test("renders point shapes, rectangles, and groups") {
    val points =
      Vector(PointShape.Square, PointShape.Triangle, PointShape.Cross).zipWithIndex.map { case (shape, idx) =>
        Grob.points(
          Vector(Point.nativeUnsafe(idx.toDouble, idx.toDouble)),
          size = LengthExpr(Length.pointsUnsafe(6.0)),
          shape = shape,
          gp = GraphicParams.unsafe(fill = Some(Rgba.White)),
          name = Some(GraphicsName.unsafe(s"shape-${idx}"))
        ).toOption.get
      }
    val rect =
      Grob.rect(
        Point.npcUnsafe(0.5, 0.5),
        Size.npcUnsafe(0.2, 0.4),
        anchor = Anchor.Center,
        gp = GraphicParams.unsafe(fill = Some(Rgba.unsafe(10, 20, 30)), alpha = 0.75),
        name = Some(GraphicsName.unsafe("centered-rect"))
      )
    val group =
      Grob.group(points :+ rect, name = Some(GraphicsName.unsafe("shape-group")))

    val svg = SvgRenderer.render(Scene(Vector(group))).toOption.get.value

    assert(svg.contains("""<g data-name="shape-group" stroke="#000000" fill="none" stroke-width="1">"""))
    assert(svg.contains("""<rect data-name="shape-0" stroke="#000000" fill="#ffffff" stroke-width="1" x="0" y="0" width="6pt" height="6pt" />"""))
    assert(svg.contains("""<path data-name="shape-1" stroke="#000000" fill="#ffffff" stroke-width="1" d="M 1 1 l 6pt 0 l 0 6pt z" />"""))
    assert(svg.contains("""<text data-name="shape-2" fill="#ffffff" stroke="none" font-size="12pt" x="2" y="2">+</text>"""))
    assert(svg.contains("""<rect data-name="centered-rect" stroke="#000000" fill="#0a141e" stroke-width="1" opacity="0.75" x="calc(50% - 20% / 2)" y="calc(50% - 40% / 2)" width="20%" height="40%" />"""))
  }

  test("renders mixed primitive SVG golden exactly") {
    val scene =
      Scene(
        Vector(
          Grob.group(
            Vector(
              Grob.rect(
                Point.npcUnsafe(0.5, 0.5),
                Size.npcUnsafe(0.25, 0.5),
                gp = GraphicParams.unsafe(fill = Some(Rgba.unsafe(10, 20, 30))),
                name = Some(GraphicsName.unsafe("panel"))
              ),
              Grob.circle(
                Point.nativeUnsafe(10.0, 20.0),
                LengthExpr(Length.pointsUnsafe(3.0)),
                gp = GraphicParams.unsafe(stroke = Some(Rgba.unsafe(200, 0, 0)), alpha = 0.8),
                name = Some(GraphicsName.unsafe("marker"))
              ),
              Grob.text(
                "A&B <label>",
                Point.nativeUnsafe(4.0, 5.0),
                rotationDegrees = 45.0,
                gp = GraphicParams.unsafe(stroke = None, fill = Some(Rgba.Black), fontSize = Length.pointsUnsafe(9.0)),
                name = Some(GraphicsName.unsafe("caption"))
              )
            ),
            name = Some(GraphicsName.unsafe("mixed"))
          )
        )
      )
    val expected =
      """<svg xmlns="http://www.w3.org/2000/svg" width="80" height="60" viewBox="0 0 80 60">
        |  <title>Mixed primitives</title>
        |  <g data-name="mixed" stroke="#000000" fill="none" stroke-width="1">
        |    <rect data-name="panel" stroke="#000000" fill="#0a141e" stroke-width="1" x="calc(50% - 25% / 2)" y="calc(50% - 50% / 2)" width="25%" height="50%" />
        |    <circle data-name="marker" stroke="#c80000" fill="none" stroke-width="1" opacity="0.8" cx="10" cy="20" r="3pt" />
        |    <text data-name="caption" fill="#000000" stroke="none" font-size="9pt" x="4" y="5" text-anchor="middle" dominant-baseline="middle" transform="rotate(45 4 5)">A&amp;B &lt;label&gt;</text>
        |  </g>
        |</svg>
        |""".stripMargin

    assertSvgEquals(scene, expected, SvgOptions.unsafe(width = 80, height = 60, title = Some("Mixed primitives")))
  }

  test("renders viewport wrappers with raw native viewBox coordinates") {
    val viewport =
      Viewport(
        origin = Point.npcUnsafe(0.1, 0.2),
        size = Size.npcUnsafe(0.5, 0.4),
        xScale = Interval.unsafe(-1.0, 1.0),
        yScale = Interval.unsafe(0.0, 10.0),
        clip = Clip.Off,
        angleDegrees = 15.0
      )
    val grob =
      Grob.lines(
        Vector(Point.nativeUnsafe(-1.0, 0.0), Point.nativeUnsafe(1.0, 10.0)),
        viewport = Some(viewport),
        name = Some(GraphicsName.unsafe("native-line"))
      ).toOption.get

    val svg = SvgRenderer.render(Scene(Vector(grob))).toOption.get.value

    assert(svg.contains("""<svg data-name="native-line" x="10%" y="20%" width="50%" height="40%" viewBox="-1 0 2 10" overflow="visible" transform="rotate(15 10% 20%)">"""))
    assert(svg.contains("""<line data-name="native-line" stroke="#000000" fill="none" stroke-width="1" x1="-1" y1="0" x2="1" y2="10" />"""))
  }

  test("returns typed errors for unsupported SVG unit semantics") {
    val grob =
      Grob.circle(
        Point.npcUnsafe(0.5, 0.5),
        LengthExpr(Length.unsafe(1.0, LengthUnit.Line))
      )

    assertEquals(
      SvgRenderer.render(Scene(Vector(grob))).left.toOption,
      Some(SvgRenderError.UnsupportedLengthUnit(LengthUnit.Line))
    )
  }

  test("returns typed errors for unsupported nested unit multiplication") {
    val expr =
      (LengthExpr.npcUnsafe(0.5) + LengthExpr.nativeUnsafe(1.0)).times(2.0).toOption.get
    val grob =
      Grob.circle(Point.npcUnsafe(0.5, 0.5), expr)

    assertEquals(
      SvgRenderer.render(Scene(Vector(grob))).left.toOption,
      Some(SvgRenderError.UnsupportedLengthExpression("non-scalar multiplication"))
    )
  }

  test("rejects invalid document sizes through smart options constructor") {
    assertEquals(
      SvgOptions(width = 0, height = 20).left.toOption,
      Some(SvgRenderError.InvalidDocumentSize(0, 20))
    )
  }
