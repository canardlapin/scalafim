package scalafim.graphics

import scala.compiletime.testing.typeCheckErrors

class PlotDslSuite extends munit.FunSuite:
  private final case class Observation(x: Double, y: Double, group: String)

  private val rows =
    Vector(
      Observation(0.0, 1.0, "control"),
      Observation(1.0, 2.0, "control"),
      Observation(0.0, 1.5, "task"),
      Observation(1.0, 2.5, "task")
    )

  test("point and line programs compose mappings, scales, labels, and theme concisely") {
    val program =
      plot(rows)
        .aes(_.x, _.y)
        .group(_.group)
        .scaleColorDiscrete(_.group, levels = Vector("control", "task"), name = "condition")
        .geomPoint()
        .geomLine()
        .title("Response")
        .axisTitles("Time", "Signal")
        .theme(Theme.minimal)
        .build
        .fold(error => fail(error.message), identity)

    val trained = program.resolve.fold(error => fail(error.message), identity)
    assertEquals(program.plot.layers.map(_.geom), Vector(Geom.Point, Geom.Line))
    assertEquals(program.plot.labels.title, Some("Response"))
    assertEquals(program.compilerOptions.theme, Theme.minimal)
    assertEquals(trained.droppedRows, Vector.empty)
    assertEquals(trained.trainedScales.map(_.aesthetic), Vector("color"))
    assert(trained.guides.nonEmpty)
  }

  test("canonical histogram, summary, and density programs expose trained plots") {
    val histogram =
      plot(rows)
        .aes(_.x)
        .geomHistogram(HistogramBins.countUnsafe(2))
        .resolve
        .fold(error => fail(error.message), identity)
    assertEquals(histogram.layers.map(_.stat.label), Vector("bin"))
    assertEquals(histogram.layers.map(_.geom), Vector(Geom.Bar))

    val summary =
      plot(rows)
        .aes(_.x, _.y)
        .geomSummary(SummaryInterval.Range)
        .resolve
        .fold(error => fail(error.message), identity)
    assertEquals(summary.layers.map(_.stat.label), Vector("summary"))
    assertEquals(summary.droppedRows, Vector.empty)

    val density =
      plot(rows)
        .aes(_.y)
        .geomDensity()
        .resolve
        .fold(error => fail(error.message), identity)
    assertEquals(density.layers.map(_.stat.label), Vector("density"))
  }

  test("the DSL and core grammar compile canonical points to the same scene") {
    val x: Observation => Double = _.x
    val y: Observation => Double = _.y
    val options = PlotCompilerOptions(
      policy = Some(LayoutPolicy()),
      expansion = RangeExpansion.none
    )
    val manual =
      Plot(rows)
        .addLayer(Layer.point(x, y))
        .flatMap(PlotCompiler.compile(_, options))
        .fold(error => fail(error.message), identity)
    val concise =
      plot(rows)
        .aes(x, y)
        .geomPoint()
        .compilerOptions(options)
        .scene
        .fold(error => fail(error.message), identity)

    assertEquals(concise, manual)
  }

  test("checked builder operations retain typed errors until build") {
    val emptyPalette =
      plot(rows)
        .aes(_.x, _.y)
        .scaleColorDiscrete(_.group, colors = Vector.empty)
        .geomPoint()
        .build
    assertEquals(emptyPalette.left.toOption, Some(GraphicsError.EmptyPalette))

    val invalidCoord =
      plot(rows)
        .aes(_.x, _.y)
        .geomPoint()
        .coordFixed(0.0)
        .build
    assertEquals(invalidCoord.left.toOption, Some(GraphicsError.InvalidCoordinateRatio(0.0)))
  }

  test("geom prerequisites are compile-time constraints") {
    val pointErrors = typeCheckErrors("""
      import scalafim.graphics.*
      final case class Row(x: Double, y: Double)
      plot(Vector(Row(1.0, 2.0))).geomPoint()
    """)
    val summaryErrors = typeCheckErrors("""
      import scalafim.graphics.*
      final case class Row(x: Double, y: Double)
      plot(Vector(Row(1.0, 2.0))).aes(_.x).geomSummary()
    """)

    assert(pointErrors.nonEmpty)
    assert(summaryErrors.nonEmpty)
    assert(pointErrors.exists(_.message.contains("requires x and y")))
    assert(summaryErrors.exists(_.message.contains("requires x and y")))
  }
