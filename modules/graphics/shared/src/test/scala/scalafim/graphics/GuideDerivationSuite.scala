package scalafim.graphics

class GuideDerivationSuite extends munit.FunSuite:
  private final case class Obs(x: Double, y: Double, condition: String)

  private val data =
    Vector(
      Obs(0.0, 1.0, "A"),
      Obs(1.0, 2.0, "B"),
      Obs(2.0, 3.0, "A")
    )

  private val frame =
    PanelFrame.npcUnsafe(0.1, 0.1, 0.8, 0.8)

  private def colorScale: DiscreteScale[Rgba] =
    DiscreteScale(
      "condition-color",
      DiscreteDomain.ordered(Vector("A", "B")).fold(e => fail(e.message), identity),
      DiscretePalette.valuesUnsafe(Vector(Rgba.unsafe(40, 80, 120), Rgba.unsafe(210, 120, 40)))
    ).fold(e => fail(e.message), identity)

  private def directPlot: Plot[Obs] =
    Plot(data)
      .addLayer(Layer.point[Obs](_.x, _.y))
      .fold(e => fail(e.message), identity)

  private def coloredPlot: Plot[Obs] =
    Plot(data)
      .withScale(ScaleBinding[Obs, String, Rgba](Aesthetic.Color, _.condition, colorScale))
      .flatMap(_.addLayer(Layer.point[Obs](_.x, _.y)))
      .fold(e => fail(e.message), identity)

  test("frame-based layouts derive panel ranges from resolved rows") {
    val trained = PlotCompiler
      .resolve(directPlot, PlotCompilerOptions(frame = Some(frame), guides = GuidePolicy.Derived()))
      .fold(e => fail(e.message), identity)
    val layout = trained.layout.getOrElse(fail("expected a derived layout"))

    assertEquals(layout.frame, frame)
    assertEquals(layout.xScale, Interval.unsafe(0.0, 2.0))
    assertEquals(layout.yScale, Interval.unsafe(1.0, 3.0))
  }

  test("derived guides produce x/y axes and a legend from the discrete color scale") {
    val trained = PlotCompiler
      .resolve(coloredPlot, PlotCompilerOptions(frame = Some(frame), guides = GuidePolicy.Derived()))
      .fold(e => fail(e.message), identity)

    val names = trained.guides.flatMap(_.spec.name).map(_.value)
    assertEquals(names, Vector("x-axis", "y-axis", "condition-color-legend"))

    val legend = trained.guides.collectFirst {
      case ResolvedGuide(spec: GuideSpec.Legend, _) => spec
    }.getOrElse(fail("expected a derived legend"))
    assertEquals(legend.title, Some("condition-color"))
    assertEquals(legend.entries.map(_.label), Vector("A", "B"))
    assertEquals(legend.entries.head.gp.fill, Some(Rgba.unsafe(40, 80, 120)))
  }

  test("scaled positions derive unit panel ranges and transform-aware ticks") {
    val xScale = ContinuousScale
      .train("x-log", Vector(1.0, 10.0, 100.0), Palette.numeric, transform = Transform.log10)
      .fold(e => fail(e.message), identity)
    val logData = Vector(Obs(1.0, 1.0, "A"), Obs(10.0, 2.0, "B"), Obs(100.0, 3.0, "A"))
    val plot = Plot(logData)
      .withScale(ScaleBinding[Obs, Double, Double](Aesthetic.X, _.x, xScale))
      .flatMap(_.addLayer(Layer.point[Obs](_.x, _.y)))
      .fold(e => fail(e.message), identity)

    val trained = PlotCompiler
      .resolve(plot, PlotCompilerOptions(frame = Some(frame), guides = GuidePolicy.Derived()))
      .fold(e => fail(e.message), identity)
    val layout = trained.layout.getOrElse(fail("expected a derived layout"))
    assertEquals(layout.xScale, Interval.unsafe(0.0, 1.0))

    val axis = trained.guides.collectFirst {
      case ResolvedGuide(spec: GuideSpec.Axis, _) if spec.side == AxisSide.Bottom => spec
    }.getOrElse(fail("expected a derived bottom axis"))
    val ticks = axis.ticks.getOrElse(fail("expected transform-derived ticks"))
    assertEquals(ticks.map(_.label), Vector("1", "10", "100"))
    assertEqualsDouble(ticks(0).value, 0.0, 1e-12)
    assertEqualsDouble(ticks(1).value, 0.5, 1e-12)
    assertEqualsDouble(ticks(2).value, 1.0, 1e-12)
  }

  test("explicit overrides suppress matching derived guides and are included") {
    val customAxis = GuideSpec.Axis(
      AxisSide.Bottom,
      breaks = Breaks.countUnsafe(2),
      name = Some(GraphicsName.unsafe("time-axis"))
    )
    val customLegend = GuideSpec.Legend(
      title = Some("condition"),
      entries = Vector(LegendEntry.colorUnsafe("A", Rgba.Black)),
      name = Some(GraphicsName.unsafe("custom-legend"))
    )
    val trained = PlotCompiler
      .resolve(
        coloredPlot,
        PlotCompilerOptions(
          frame = Some(frame),
          guides = GuidePolicy.Derived(overrides = Vector(customAxis, customLegend))
        )
      )
      .fold(e => fail(e.message), identity)

    val names = trained.guides.flatMap(_.spec.name).map(_.value)
    assertEquals(names, Vector("y-axis", "time-axis", "custom-legend"))
    val bottomAxes = trained.guides.map(_.spec).collect {
      case axis: GuideSpec.Axis if axis.side == AxisSide.Bottom => axis
    }
    assertEquals(bottomAxes.length, 1)
    assertEquals(bottomAxes.head.name.map(_.value), Some("time-axis"))
  }

  test("mixing scaled and unscaled position bindings across layers is a typed error") {
    val xScale = ContinuousScale
      .train("x-position", data.map(_.x), Palette.numeric)
      .fold(e => fail(e.message), identity)
    val scaledLayer = Layer.fromMapping(
      Geom.Point,
      AesSpec
        .empty[Obs]
        .withPosition(_.x, _.y)
        .bindScale(ScaleBinding[Obs, Double, Double](Aesthetic.X, _.x, xScale))
        .fold(e => fail(e.message), identity),
      inheritMapping = false
    ).fold(e => fail(e.message), identity)
    val plot = Plot(data)
      .addLayer(Layer.point[Obs](_.x, _.y))
      .flatMap(_.addLayer(scaledLayer))
      .fold(e => fail(e.message), identity)

    assertEquals(
      PlotCompiler
        .resolve(plot, PlotCompilerOptions(frame = Some(frame), guides = GuidePolicy.Derived()))
        .left
        .toOption,
      Some(GraphicsError.MixedPositionScaling("x"))
    )
  }

  test("multiple derived legends stack instead of overprinting") {
    val fillScale = DiscreteScale(
      "condition-fill",
      DiscreteDomain.ordered(Vector("A", "B")).fold(e => fail(e.message), identity),
      DiscretePalette.valuesUnsafe(Vector(Rgba.unsafe(70, 145, 85), Rgba.unsafe(140, 90, 170)))
    ).fold(e => fail(e.message), identity)
    val plot = Plot(data)
      .withScale(ScaleBinding[Obs, String, Rgba](Aesthetic.Color, _.condition, colorScale))
      .flatMap(_.withScale(ScaleBinding[Obs, String, Rgba](Aesthetic.Fill, _.condition, fillScale)))
      .flatMap(_.addLayer(Layer.point[Obs](_.x, _.y)))
      .fold(e => fail(e.message), identity)

    val trained = PlotCompiler
      .resolve(plot, PlotCompilerOptions(frame = Some(frame), guides = GuidePolicy.Derived()))
      .fold(e => fail(e.message), identity)
    val legends = trained.guides.map(_.spec).collect { case legend: GuideSpec.Legend => legend }

    assertEquals(legends.map(_.name.map(_.value)), Vector(Some("condition-color-legend"), Some("condition-fill-legend")))
    val origins = legends.map(_.origin.y)
    assert(origins(0) != origins(1), "stacked legends must not share an origin")
  }

  test("derived guides without a frame or layout remain a typed error") {
    assertEquals(
      PlotCompiler.resolve(directPlot, PlotCompilerOptions(guides = GuidePolicy.Derived())).left.toOption,
      Some(GraphicsError.MissingLayout("guides"))
    )
  }

  test("explicit empty guides still compile without a layout") {
    val trained = PlotCompiler
      .resolve(directPlot, PlotCompilerOptions(guides = GuidePolicy.NoGuides))
      .fold(e => fail(e.message), identity)
    assertEquals(trained.guides, Vector.empty)
    assertEquals(trained.layout, None)
  }
