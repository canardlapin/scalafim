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
    assertEquals(model.designTableEither.map(_.columnNames), Right(model.columnNames))
    assertEquals(table.columnNames, model.columnNames)
    assertEquals(table.rows.length, model.designMatrix.rows)
    assertEquals(table.rows.head.values.length, model.designMatrix.cols)
    assertEquals(table.metadata.length, model.designMatrix.cols)
    assertEquals(table.descriptors.length, model.designMatrix.cols)
    assertEquals(table.descriptors.map(_.toMeta), table.metadata)
    assertEquals(model.designDescriptors.map(_.toMeta), model.designMeta)
    assertEquals(table.rows.head.scanIndex.oneBased, 1)
    assertEquals(table.rows.head.scanIndex.zeroBased, 0)
    assertEquals(table.descriptors.head.index.oneBased, 1)
    assertEquals(table.descriptors.head.index.zeroBased, 0)
    assert(DesignColumnIndex.fromOneBased(0).isLeft)

    val map = model.designMap()
    assertEquals(model.designMapEither.map(_.regressors), Right(model.columnNames))
    assertEquals(map.cells.length, model.designMatrix.rows * model.designMatrix.cols)
    assertEquals(map.cells.head.scanIndex.oneBased, 1)
    assertEquals(map.cells.head.columnIndex.zeroBased, 0)
    assertEquals(map.blockSeparators, Vector(20))
    assertEquals(map.regressors, model.columnNames)
    assert(map.rendererNote.contains("adapter"))
  }

  test("correlationMap exports full and half matrix cells") {
    val model = eventModel()

    val full = model.correlationMap()
    assertEquals(model.correlationMapEither().map(_.regressors), Right(model.columnNames))
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
    assertEquals(
      DesignExports.eventPlotDataEither(model, termName = Some("task")).map(_.regressors),
      Right(model.columnNames)
    )

    assertEquals(plot.regressors, model.columnNames)
    assertEquals(plot.points.length, model.designMatrix.rows * model.designMatrix.cols)
    assert(plot.points.map(_.group).distinct.forall(_.contains("#")))
    assertEquals(plot.useFacets, true)
    assertEquals(plot.facetByBlock, true)
    assertEquals(plot.boundaries.map(_.time).distinct.sorted, Vector(0.0, 20.0))
    assertEquals(plot.boundaries.flatMap(_.block).distinct.sorted, Vector(1, 2))
  }

  test("semantic selectors filter exports without relying on rendered column names") {
    val model0 = eventModel()
    val model = model0.copy(columnNames = model0.columnNames.map(name => s"renamed_$name"))

    val condition = model.designDescriptors.flatMap(_.condition).head
    val conditionSelector = DesignColumnSelector.condition(condition)
    val conditionColumns = model.designDescriptors.filter(_.condition.contains(condition)).map(_.name)
    val conditionTable = model.designTable(conditionSelector)
    assertEquals(model.designTableEither(conditionSelector).map(_.columnNames), Right(conditionColumns))
    assertEquals(conditionTable.columnNames, conditionColumns)
    assert(conditionTable.columnNames.forall(_.startsWith("renamed_")))

    val taskBasisSelector =
      DesignColumnSelector.role(ColumnRole.Task) && DesignColumnSelector.unsafeBasisIndexOneBased(1)
    val taskBasisTable = model.designTable(taskBasisSelector)
    assertEquals(taskBasisTable.columnNames, model.columnNames)

    val plot = DesignExports.eventPlotData(
      model,
      termName = Some("task"),
      columnSelector = conditionSelector
    )
    assertEquals(plot.regressors, conditionColumns)
    assertEquals(plot.points.length, model.designMatrix.rows * conditionColumns.length)
  }

  test("semantic selectors can select baseline exports by run") {
    val sf = SamplingFrame(blockLens = Seq(12, 12), tr = Seq(1.0))
    val model = BaselineModel.build(samplingFrame = sf, basis = BaselineBasis.Poly, degree = 2)
    val selector = DesignColumnSelector.unsafeRunOneBased(1)

    val table = model.designTable(selector)
    assert(table.descriptors.nonEmpty)
    assert(table.descriptors.forall(_.run.exists(_.oneBased == 1)))

    val map = model.designMap(selector)
    assertEquals(model.designMapEither(selector).map(_.regressors), Right(table.columnNames))
    assertEquals(map.regressors, table.columnNames)
    assertEquals(map.metadata, table.metadata)
  }

  test("baselinePlotData selects first varying term and suppresses structural zero traces") {
    val sf = SamplingFrame(blockLens = Seq(12, 12), tr = Seq(1.0))
    val model = BaselineModel.build(samplingFrame = sf, basis = BaselineBasis.Poly, degree = 2)

    val plot = DesignExports.baselinePlotData(model)
    assertEquals(DesignExports.baselinePlotDataEither(model).map(_.termName), Right("drift"))

    assertEquals(plot.termName, "drift")
    assert(plot.points.nonEmpty)
    assertEquals(plot.points.map(_.block).distinct.sorted, Vector(1, 2))
    assert(plot.points.forall(p => p.group.contains("#")))
  }

  test("contrastPlotData exports attached contrast weights in design-column space") {
    val model = eventModel(withContrasts = true)
    val plot = DesignExports.contrastPlotData(model, scaleMode = ContrastScaleMode.Auto)
    assertEquals(
      DesignExports.contrastPlotDataEither(model, scaleMode = ContrastScaleMode.Auto).map(_.contrastNames),
      Right(Vector("task#A_vs_B"))
    )

    assertEquals(plot.contrastNames, Vector("task#A_vs_B"))
    assertEquals(plot.regressors, model.columnNames)
    assertEquals(plot.cells.length, model.designMatrix.cols)
    assertEquals(plot.scaleMode, ContrastScaleMode.Diverging)
    assert(plot.limits.nonEmpty)
    assert(plot.cells.exists(_.weight < 0.0))
    assert(plot.cells.exists(_.weight > 0.0))
  }

  test("Either export APIs report structured user errors") {
    val model = eventModel()

    assertEquals(
      model.copy(columnNames = model.columnNames.drop(1)).designTableEither.left.toOption,
      Some(DesignExportError.ColumnCountMismatch("designTable", model.designMatrix.cols, model.columnNames.length - 1))
    )

    assertEquals(
      DesignExports.eventPlotDataEither(model, termName = Some("missing")).left.toOption,
      Some(DesignExportError.MissingEventTerm("missing", model.termKeys))
    )

    val badSampling = model.copy(samplingFrame = SamplingFrame(blockLens = Seq(1), tr = Seq(1.0)))
    assertEquals(
      DesignExports.eventPlotDataEither(badSampling).left.toOption,
      Some(DesignExportError.SamplingRowMismatch("eventPlotData", model.designMatrix.rows, 1, 1))
    )

    assertEquals(
      DesignExports.contrastPlotDataEither(model).left.toOption,
      Some(DesignExportError.EmptyContrasts)
    )

    val sf = SamplingFrame(blockLens = Seq(12, 12), tr = Seq(1.0))
    val baseline = BaselineModel.build(samplingFrame = sf, basis = BaselineBasis.Poly, degree = 2)
    assertEquals(
      DesignExports.baselinePlotDataEither(baseline, termName = Some("missing")).left.toOption,
      Some(DesignExportError.MissingBaselineTerm("missing", baseline.termKeys))
    )

    val noColumns = model.designTableEither(DesignColumnSelector.condition("missing")).left.toOption
    assert(noColumns.exists {
      case DesignExportError.EmptySelection(target) => target.contains("Condition")
      case _                                       => false
    })
  }
