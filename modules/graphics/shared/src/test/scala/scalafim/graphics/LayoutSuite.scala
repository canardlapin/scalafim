package scalafim.graphics

class LayoutSuite extends munit.FunSuite:

  private val frame =
    PanelFrame.npcUnsafe(0.1, 0.2, 0.8, 0.6)

  private val layout =
    PanelLayout(
      frame,
      xScale = Interval.unsafe(0.0, 10.0),
      yScale = Interval.unsafe(-1.0, 1.0),
      margins = PanelMargins.npcUnsafe(0.08, 0.04, 0.12, 0.1),
      clip = Clip.On
    )

  test("panel layout exposes a viewport and maps data coordinates into panel space") {
    val viewport = layout.viewport
    val midpoint = layout.dataToPanel(5.0, 0.0).toOption.get

    assertEquals(viewport.origin, frame.origin)
    assertEquals(viewport.size, frame.size)
    assertEquals(viewport.xScale, Interval.unsafe(0.0, 10.0))
    assertEquals(viewport.yScale, Interval.unsafe(-1.0, 1.0))
    assertEquals(viewport.clip, Clip.On)
    assertEquals(
      midpoint,
      Point(
        frame.origin.x + LengthExpr.Mul(0.5, frame.size.width.expr),
        frame.origin.y + LengthExpr.Mul(0.5, frame.size.height.expr)
      )
    )
    assert(layout.contains(5.0, 0.0))
    assert(!layout.contains(11.0, 0.0))
  }

  test("panel coordinate transforms reject non-finite inputs") {
    assert(layout.dataToPanel(Double.NaN, 0.0).left.toOption.exists {
      case GraphicsError.InvalidLayoutCoordinate("x", value) => value.isNaN
      case _                                                 => false
    })
    assertEquals(
      layout.dataToPanel(0.0, Double.PositiveInfinity).left.toOption,
      Some(GraphicsError.InvalidLayoutCoordinate("y", Double.PositiveInfinity))
    )
  }

  test("axis guides lower through existing axis grobs with unclipped guide viewport") {
    val guide =
      GuideSpec
        .lower(
          GuideSpec.Axis(
            AxisSide.Bottom,
            breaks = Breaks.countUnsafe(3),
            tickLength = Some(0.2),
            labelOffset = Some(0.4),
            name = Some(GraphicsName.unsafe("x-guide"))
          ),
          layout
        )
        .toOption
        .get
    val group = guide.grob.asInstanceOf[Grob.Group]
    val baseline = group.children.head.asInstanceOf[Grob.Segments]
    val ticks = group.children(1).asInstanceOf[Grob.Segments]

    assertEquals(group.name.map(_.value), Some("x-guide"))
    assertEquals(group.viewport.map(_.clip), Some(Clip.Off))
    assertEquals(group.children.length, 5)
    assertEquals(
      baseline.segments,
      Vector((Point.nativeUnsafe(0.0, -1.0), Point.nativeUnsafe(10.0, -1.0)))
    )
    assertEquals(ticks.segments(1), (Point.nativeUnsafe(5.0, -1.0), Point.nativeUnsafe(5.0, -1.2)))
  }

  test("legend guides lower to stable marker and label grobs") {
    val guide =
      GuideSpec
        .lower(
          GuideSpec.Legend(
            title = Some("condition"),
            entries = Vector(
              LegendEntry.colorUnsafe("A", Rgba.Black),
              LegendEntry.colorUnsafe("B", Rgba.White)
            ),
            origin = Point.npcUnsafe(0.82, 0.2),
            rowGap = ExtentExpr.npcUnsafe(0.05),
            labelOffset = LengthExpr.npcUnsafe(0.03),
            name = Some(GraphicsName.unsafe("condition-legend"))
          ),
          layout
        )
        .toOption
        .get
    val group = guide.grob.asInstanceOf[Grob.Group]
    val title = group.children.head.asInstanceOf[Grob.Text]
    val secondKey = group.children(3).asInstanceOf[Grob.Points]
    val secondLabel = group.children(4).asInstanceOf[Grob.Text]

    assertEquals(group.name.map(_.value), Some("condition-legend"))
    assertEquals(group.children.length, 5)
    assertEquals(title.name.map(_.value), Some("condition-legend-title"))
    assertEquals(title.label, "condition")
    assertEquals(secondKey.name.map(_.value), Some("condition-legend-entry-1-key"))
    assertEquals(secondKey.gp.fill, Some(Rgba.White))
    assertEquals(secondLabel.name.map(_.value), Some("condition-legend-entry-1-label"))
    assertEquals(secondLabel.label, "B")
  }

  test("empty legends fail before backend rendering") {
    val guide =
      GuideSpec.Legend(
        title = None,
        entries = Vector.empty
      )

    assertEquals(GuideSpec.lower(guide, layout).left.toOption, Some(GraphicsError.EmptyGeometry("legend")))
  }
