package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.graphics.*

class DesignGraphicsSuite extends munit.FunSuite:

  private def eventModel() =
    val sf = SamplingFrame(blockLens = Seq(20, 20), tr = Seq(1.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 8.0, 3.0, 11.0)),
      "cond" -> Column.Strings(Vector("A", "B", "A", "B"))
    )
    EventModelBuilder.build(
      formula = "onset ~ hrf(cond, id = task)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 1, 1)
    )

  test("event plot exports build renderer-neutral graphics plots") {
    val model = eventModel()
    val exported = DesignExports.eventPlotData(model, termName = Some("task"))
    val plot = DesignGraphics.eventPlot(exported).toOption.get
    val resolved = PlotCompiler.resolve(plot).toOption.get

    assertEquals(plot.layers.length, exported.regressors.length)
    assertEquals(resolved.layers.length, exported.regressors.length)
    assertEquals(resolved.droppedRows, Vector.empty)
    assert(resolved.layers.forall(_.geom == Geom.Line))
    assert(resolved.scaleDeclarations.forall(_.aesthetic == "color"))
  }

  test("event plot exports compile to panel scene with axes and legend guides") {
    val model = eventModel()
    val exported = DesignExports.eventPlotData(model, termName = Some("task"))
    val scene = DesignGraphics.eventScene(exported).toOption.get
    val panel = scene.grobs.head.asInstanceOf[Grob.Group]
    val timeAxis = scene.grobs(1).asInstanceOf[Grob.Group]
    val responseAxis = scene.grobs(2).asInstanceOf[Grob.Group]
    val legend = scene.grobs(3).asInstanceOf[Grob.Group]

    assertEquals(scene.size, 4)
    assertEquals(panel.name.map(_.value), Some("plot-panel"))
    assertEquals(panel.viewport.map(_.clip), Some(Clip.On))
    assertEquals(panel.children.length, exported.regressors.length)
    assert(panel.children.forall(_.isInstanceOf[Grob.Lines]))
    assertEquals(timeAxis.name.map(_.value), Some("time-axis"))
    assertEquals(responseAxis.name.map(_.value), Some("response-axis"))
    assertEquals(legend.name.map(_.value), Some("regressor-legend"))
    assertEquals(legend.children.length, 1 + exported.regressors.length * 2)
    assertEquals(
      timeAxis.children.collect { case text: Grob.Text => text.label },
      Vector("10", "20", "30")
    )
  }

  test("the DSL migration preserves the previous event scene exactly") {
    val exported = DesignExports.eventPlotData(eventModel(), termName = Some("task"))
    val domain =
      DiscreteDomain
        .ordered(exported.regressors)
        .flatMap(_.train(exported.points.map(_.regressor)))
        .fold(error => fail(error.message), identity)
    val scale =
      DiscreteScale(
        "regressor",
        domain,
        DiscretePalette.valuesUnsafe(DesignGraphics.defaultColors)
      ).fold(error => fail(error.message), identity)
    val base =
      Plot(exported.points)
        .withCoord(Coord.Cartesian())
        .withScale(ScaleBinding[TracePoint, String, Rgba](Aesthetic.Color, _.regressor, scale))
        .fold(error => fail(error.message), identity)
    val legacyPlot = exported.regressors.foldLeft(Right(base): Either[GraphicsError, Plot[TracePoint]]) {
      (current, regressor) =>
        val trace = exported.points.filter(_.regressor == regressor)
        val layer =
          if trace.length < 2 then Layer.point[TracePoint](_.time, _.response, data = Some(trace))
          else Layer.line[TracePoint](_.time, _.response, data = Some(trace))
        current.flatMap(_.addLayer(layer))
    }.fold(error => fail(error.message), identity)
    val compilerOptions = PlotCompilerOptions(
      policy = Some(LayoutPolicy()),
      guides = GuidePolicy.Derived(
        overrides = Vector(
          GuideSpec.Axis(AxisSide.Bottom, name = Some(GraphicsName.unsafe("time-axis"))),
          GuideSpec.Axis(AxisSide.Left, name = Some(GraphicsName.unsafe("response-axis")))
        )
      )
    )
    val legacy = PlotCompiler.compile(legacyPlot, compilerOptions).fold(error => fail(error.message), identity)
    val concise = DesignGraphics.eventScene(exported).fold(error => fail(error.message), identity)

    assertEquals(concise, legacy)
  }

  test("event model helper composes design export errors with graphics errors") {
    val model = eventModel()
    val scene = DesignGraphics.eventModelScene(model, termName = Some("task"))
    val missing = DesignGraphics.eventModelScene(model, termName = Some("missing")).left.toOption
    val empty =
      DesignGraphics
        .eventScene(
          DesignExports.eventPlotData(model, termName = Some("task")).copy(points = Vector.empty)
        )
        .left
        .toOption

    assert(scene.toOption.exists(!_.isEmpty))
    assert(missing.exists {
      case DesignGraphicsError.Export(DesignExportError.MissingEventTerm("missing", known)) => known.contains("task")
      case _                                                                                => false
    })
    assertEquals(empty, Some(GraphicsError.EmptyGeometry("event plot")))
  }
