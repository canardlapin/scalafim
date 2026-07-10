package scalafim.graphics

class PlotLayoutSuite extends munit.FunSuite:
  private val tol = 1e-9

  private val policy = LayoutPolicy()

  // Reference device 640x480 at 96ppi: 1pt = 4/3 px.
  private def npcX(pt: Double): Double = pt * (4.0 / 3.0) / 640.0
  private def npcY(pt: Double): Double = pt * (4.0 / 3.0) / 480.0

  private def originX(frame: PanelFrame): Double =
    frame.origin.x match
      case LengthExpr.Const(length) => length.value
      case other                    => fail(s"expected npc constant, got $other")

  private def originY(frame: PanelFrame): Double =
    frame.origin.y match
      case LengthExpr.Const(length) => length.value
      case other                    => fail(s"expected npc constant, got $other")

  private def width(frame: PanelFrame): Double =
    frame.size.width.expr match
      case LengthExpr.Const(length) => length.value
      case other                    => fail(s"expected npc constant, got $other")

  private def height(frame: PanelFrame): Double =
    frame.size.height.expr match
      case LengthExpr.Const(length) => length.value
      case other                    => fail(s"expected npc constant, got $other")

  test("panel-only requests allocate margins and nothing else") {
    val frames = PlotLayoutSolver.solve(policy, PlotLayoutRequest()).fold(e => fail(e.message), identity)
    assertEqualsDouble(originX(frames.panel), npcX(10.0), tol)
    assertEqualsDouble(originY(frames.panel), npcY(10.0), tol)
    assertEqualsDouble(width(frames.panel), 1.0 - 2.0 * npcX(10.0), tol)
    assertEqualsDouble(height(frames.panel), 1.0 - 2.0 * npcY(10.0), tol)
    assertEquals(frames.axes, Map.empty[AxisSide, PanelFrame])
    assertEquals(frames.legend, None)
  }

  test("axis requests allocate strips sized from tick labels via text metrics") {
    val request = PlotLayoutRequest(
      axes = Map(
        AxisSide.Bottom -> Vector("0", "5", "10"),
        AxisSide.Left -> Vector("-1", "1")
      )
    )
    val frames = PlotLayoutSolver.solve(policy, request).fold(e => fail(e.message), identity)

    // Bottom strip: tick 4 + gap 4 + line height 12.5 = 20.5pt.
    val bottomStrip = npcY(20.5)
    // Left strip: tick 4 + gap 4 + max label width ("-1": 2 chars * 10pt * 0.62 = 12.4) = 20.4pt.
    val leftStrip = npcX(20.4)

    assertEqualsDouble(originY(frames.panel), npcY(10.0) + bottomStrip, tol)
    assertEqualsDouble(originX(frames.panel), npcX(10.0) + leftStrip, tol)

    val bottom = frames.axes(AxisSide.Bottom)
    assertEqualsDouble(originY(bottom), npcY(10.0), tol)
    assertEqualsDouble(height(bottom), bottomStrip, tol)
    assertEqualsDouble(width(bottom), width(frames.panel), tol)

    val left = frames.axes(AxisSide.Left)
    assertEqualsDouble(originX(left), npcX(10.0), tol)
    assertEqualsDouble(width(left), leftStrip, tol)
    assertEqualsDouble(height(left), height(frames.panel), tol)
  }

  test("legend requests allocate a right-hand column beside the panel") {
    val request = PlotLayoutRequest(
      legend = Some(LegendRequest(title = Some("condition"), labels = Vector("A", "B")))
    )
    val frames = PlotLayoutSolver.solve(policy, request).fold(e => fail(e.message), identity)
    val legend = frames.legend.getOrElse(fail("expected a legend frame"))

    // Title width 9 chars * 10pt * 0.62 = 55.8pt dominates entries; plus 12pt padding.
    val legendWidth = npcX(2.0 * 6.0 + 55.8)
    assertEqualsDouble(width(legend), legendWidth, tol)
    assertEqualsDouble(originX(legend), originX(frames.panel) + width(frames.panel) + npcX(10.0), tol)
    assertEqualsDouble(originY(legend), originY(frames.panel), tol)
    assertEqualsDouble(height(legend), height(frames.panel), tol)
    assertEqualsDouble(
      originX(frames.panel) + width(frames.panel) + npcX(10.0) + legendWidth + npcX(10.0),
      1.0,
      tol
    )
  }

  test("legend columns clear a right-axis strip instead of overlapping it") {
    val request = PlotLayoutRequest(
      axes = Map(AxisSide.Right -> Vector("-1", "1")),
      legend = Some(LegendRequest(title = None, labels = Vector("A", "B")))
    )
    val frames = PlotLayoutSolver.solve(policy, request).fold(e => fail(e.message), identity)
    val right = frames.axes(AxisSide.Right)
    val legend = frames.legend.getOrElse(fail("expected a legend frame"))

    assertEqualsDouble(originX(right), originX(frames.panel) + width(frames.panel), tol)
    assertEqualsDouble(originX(legend), originX(right) + width(right) + npcX(10.0), tol)
  }

  test("impossible layouts are typed errors") {
    val hugeLegend = PlotLayoutRequest(
      legend = Some(LegendRequest(title = None, labels = Vector("x" * 200)))
    )
    assertEquals(
      PlotLayoutSolver.solve(policy, hugeLegend).left.toOption,
      Some(GraphicsError.LayoutOverflow("panel width"))
    )
  }

  test("solver-driven compilation places the legend in its own named viewport") {
    final case class Obs(x: Double, y: Double, condition: String)
    val data = Vector(Obs(0.0, 1.0, "A"), Obs(1.0, 2.0, "B"), Obs(2.0, 3.0, "A"))
    val colorScale = DiscreteScale(
      "condition",
      DiscreteDomain.ordered(Vector("A", "B")).fold(e => fail(e.message), identity),
      DiscretePalette.valuesUnsafe(Vector(Rgba.unsafe(40, 80, 120), Rgba.unsafe(210, 120, 40)))
    ).fold(e => fail(e.message), identity)
    val plot = Plot(data)
      .withScale(ScaleBinding[Obs, String, Rgba](Aesthetic.Color, _.condition, colorScale))
      .flatMap(_.addLayer(Layer.point[Obs](_.x, _.y)))
      .fold(e => fail(e.message), identity)

    val trained = PlotCompiler
      .resolve(plot, PlotCompilerOptions(policy = Some(policy), guides = GuidePolicy.Derived()))
      .fold(e => fail(e.message), identity)

    val layout = trained.layout.getOrElse(fail("expected a solved layout"))
    assertEquals(layout.xScale, Interval.unsafe(-0.1, 2.1))

    val legendGuide = trained.guides.collectFirst {
      case guide @ ResolvedGuide(_: GuideSpec.Legend, _) => guide
    }.getOrElse(fail("expected a derived legend"))
    val legendGroup = legendGuide.grob.asInstanceOf[Grob.Group]
    assertEquals(legendGroup.name.map(_.value), Some("condition-legend"))
    assert(legendGroup.viewport.nonEmpty, "legend must live in its allocated viewport")

    val scene = trained.scene
    assert(scene.grobs.head.name.map(_.value).contains("plot-panel"))
  }

  test("solved scenes lower to identical device scenes across runs") {
    final case class Obs(x: Double, y: Double)
    val data = Vector(Obs(0.0, 1.0), Obs(1.0, 2.0), Obs(2.0, 3.0))
    val plot = Plot(data)
      .addLayer(Layer.point[Obs](_.x, _.y))
      .fold(e => fail(e.message), identity)
    val trained = PlotCompiler
      .resolve(plot, PlotCompilerOptions(policy = Some(policy), guides = GuidePolicy.Derived()))
      .fold(e => fail(e.message), identity)

    val device = DeviceContext.unsafe(640.0, 480.0)
    val first = DeviceScene.fromScene(trained.scene, device).fold(e => fail(e.message), identity)
    val second = DeviceScene.fromScene(trained.scene, device).fold(e => fail(e.message), identity)
    assertEquals(first, second)
  }

  test("default derived ranges keep scaled and unscaled corner discs inside the panel clip") {
    final case class Obs(x: Double, y: Double)
    val data = Vector(Obs(0.0, 0.0), Obs(0.0, 1.0), Obs(1.0, 0.0), Obs(1.0, 1.0))

    def plot(scaled: Boolean): Plot[Obs] =
      val base =
        if scaled then
          val xScale = ContinuousScale.train("x", data.map(_.x), Palette.numeric).fold(e => fail(e.message), identity)
          val yScale = ContinuousScale.train("y", data.map(_.y), Palette.numeric).fold(e => fail(e.message), identity)
          Plot(data)
            .withScale(ScaleBinding[Obs, Double, Double](Aesthetic.X, _.x, xScale))
            .flatMap(_.withScale(ScaleBinding[Obs, Double, Double](Aesthetic.Y, _.y, yScale)))
            .fold(e => fail(e.message), identity)
        else Plot(data)
      base.addLayer(Layer.point[Obs](_.x, _.y)).fold(e => fail(e.message), identity)

    def panel(scaled: Boolean, expansion: RangeExpansion): (DeviceClip, Vector[DevicePrimitive.Disc]) =
      val trained = PlotCompiler
        .resolve(
          plot(scaled),
          PlotCompilerOptions(
            policy = Some(policy),
            expansion = expansion,
            guides = GuidePolicy.Derived()
          )
        )
        .fold(e => fail(e.message), identity)
      val scene = DeviceScene
        .fromScene(trained.scene, policy.referenceDevice)
        .fold(e => fail(e.message), identity)
      scene.elements.collectFirst {
        case DeviceElement.Group(name, Some(clip), _, children)
            if name.exists(_.value == "plot-panel") =>
          val discs = children.collect {
            case DeviceElement.Mark(disc: DevicePrimitive.Disc) => disc
          }
          (clip, discs)
      }.getOrElse(fail(s"missing clipped panel for scaled=$scaled"))

    Vector(false, true).foreach { scaled =>
      val (clip, discs) = panel(scaled, RangeExpansion.default)
      assertEquals(discs.length, 4)
      discs.foreach { disc =>
        assert(disc.centerX - disc.radius >= clip.x - tol)
        assert(disc.centerX + disc.radius <= clip.x + clip.width + tol)
        assert(disc.centerY - disc.radius >= clip.y - tol)
        assert(disc.centerY + disc.radius <= clip.y + clip.height + tol)
      }
    }

    val (exactClip, exactDiscs) = panel(scaled = false, RangeExpansion.none)
    assert(
      exactDiscs.exists(disc => disc.centerX - disc.radius < exactClip.x),
      "the explicit no-expansion policy must retain exact edge-centered framing"
    )
  }
