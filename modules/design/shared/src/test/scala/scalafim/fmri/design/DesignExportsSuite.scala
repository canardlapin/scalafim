package scalafim.fmri.design

import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel}
import scalafim.fmri.design.contrast.ContrastSpec
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.*
import scalafim.fmri.design.DesignExports
import scalafim.fmri.design.DesignExports.DesignSource.given
import scalafim.fmri.design.DesignExports.*
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.design.SamplingFrame

class DesignExportsSuite extends munit.FunSuite:

  private def eventModel(withContrasts: Boolean = false) =
    val sf = SamplingFrame(blockLens = Seq(20, 20), tr = Seq(1.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 8.0, 3.0, 11.0)),
      "cond" -> Column.Strings(Vector("A", "B", "A", "B"))
    )
    val cset =
      ContrastSpec.ContrastSet(
        ContrastSpec.Pair(
          name = "A_vs_B",
          A = cell => cell("cond") == "A",
          B = cell => cell("cond") == "B"
        )
      )
    EventModelBuilder.build(
      formula = if withContrasts then "onset ~ hrf(cond, id = task, contrasts = myset)" else "onset ~ hrf(cond, id = task)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 1, 1),
      contrastSets = if withContrasts then Map("myset" -> cset) else Map.empty
    )

  test("designTable and designMap expose matrix rows with metadata") {
    val model = eventModel()

    val table = model.designTable
    assertEquals(table.columnNames, model.columnNames)
    assertEquals(table.rows.length, model.designMatrix.rows)
    assertEquals(table.rows.head.values.length, model.designMatrix.cols)
    assertEquals(table.metadata.length, model.designMatrix.cols)

    val map = model.designMap()
    assertEquals(map.cells.length, model.designMatrix.rows * model.designMatrix.cols)
    assertEquals(map.blockSeparators, Vector(20))
    assertEquals(map.regressors, model.columnNames)
    assert(map.rendererNote.contains("adapter"))
  }

  test("correlationMap exports full and half matrix cells") {
    val model = eventModel()

    val full = model.correlationMap()
    assertEquals(full.cells.length, model.designMatrix.cols * model.designMatrix.cols)
    assertEquals(full.limits, Some((-1.0, 1.0)))
    assert(full.cells.exists(c => c.var1 == model.columnNames.head && c.var2 == model.columnNames.head && c.correlation.exists(v => math.abs(v - 1.0) < 1e-12)))

    val half = model.correlationMap(halfMatrix = true, absoluteLimits = false)
    assertEquals(half.halfMatrix, true)
    assert(half.cells.exists(c => c.col > c.row && c.correlation.isEmpty))
    assert(half.limits.nonEmpty)
  }

  test("eventPlotData groups traces within block and exports block boundaries") {
    val model = eventModel()
    val plot = DesignExports.eventPlotData(
      model,
      termName = Some("task"),
      facetThreshold = 1,
      blockAxis = PlotBlockAxis.Global,
      facetByBlock = true
    )

    assertEquals(plot.regressors, model.columnNames)
    assertEquals(plot.points.length, model.designMatrix.rows * model.designMatrix.cols)
    assert(plot.points.map(_.group).distinct.forall(_.contains("#")))
    assertEquals(plot.useFacets, true)
    assertEquals(plot.facetByBlock, true)
    assertEquals(plot.boundaries.map(_.time).distinct.sorted, Vector(0.0, 20.0))
    assertEquals(plot.boundaries.flatMap(_.block).distinct.sorted, Vector(1, 2))
  }

  test("baselinePlotData selects first varying term and suppresses structural zero traces") {
    val sf = SamplingFrame(blockLens = Seq(12, 12), tr = Seq(1.0))
    val model = BaselineModel.build(samplingFrame = sf, basis = BaselineBasis.Poly, degree = 2)

    val plot = DesignExports.baselinePlotData(model)

    assertEquals(plot.termName, "drift")
    assert(plot.points.nonEmpty)
    assertEquals(plot.points.map(_.block).distinct.sorted, Vector(1, 2))
    assert(plot.points.forall(p => p.group.contains("#")))
  }

  test("contrastPlotData exports attached contrast weights in design-column space") {
    val model = eventModel(withContrasts = true)
    val plot = DesignExports.contrastPlotData(model, scaleMode = ContrastScaleMode.Auto)

    assertEquals(plot.contrastNames, Vector("task#A_vs_B"))
    assertEquals(plot.regressors, model.columnNames)
    assertEquals(plot.cells.length, model.designMatrix.cols)
    assertEquals(plot.scaleMode, ContrastScaleMode.Diverging)
    assert(plot.limits.nonEmpty)
    assert(plot.cells.exists(_.weight < 0.0))
    assert(plot.cells.exists(_.weight > 0.0))
  }
