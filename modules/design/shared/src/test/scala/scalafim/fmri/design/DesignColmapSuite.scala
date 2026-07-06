package scalafim.fmri.design

import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.{ColumnRole, ModulationType, ModelSource}
import scalafim.fmri.design.DesignColmap.*
import scalafim.fmri.design.event.*
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.design.SamplingFrame

class DesignColmapSuite extends munit.FunSuite:

  test("designColmap(event) includes HRF basis metadata and covariate modulation") {
    val sf = SamplingFrame(blockLens = Seq(10), tr = Seq(1.0))

    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 2.0, 3.0, 4.0)),
      "cond" -> Column.Strings(Vector("A", "B", "A", "B"))
    )

    val motion = DataTable.fromColumns(
      "x" -> Column.Doubles(Vector.tabulate(10)(_.toDouble)),
      "y" -> Column.Doubles(Vector.fill(10)(0.0))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(cond, basis=\"spmg3\", id=\"task\") + covariate(x, y, data=motion, id=\"motion\", prefix=\"motion\")",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0, 0),
      tables = Map("motion" -> motion)
    )

    val colmap = model.designColmap
    assertEquals(colmap.length, model.designMatrix.cols)

    val first = colmap.head
    assertEquals(first.modelSource, ModelSource.Event)
    assertEquals(first.role, ColumnRole.Task)
    assertEquals(first.termTag, Some("task"))
    assertEquals(first.basisName, Some("SPMG3"))
    assertEquals(first.basisTotal, Some(3))
    assertEquals(first.basisIx, Some(1))
    assertEquals(first.basisLabel, Some("canonical"))

    val covCols = colmap.filter(_.termTag.contains("motion"))
    assertEquals(covCols.length, 2)
    assert(covCols.forall(_.role == ColumnRole.Covariate))
    assert(covCols.forall(_.basisName.isEmpty))
    assert(covCols.forall(_.modulationType.contains(ModulationType.Covariate)))
  }

  test("EventModelBuilder supports hrf(Scale(rt)) and yields parametric metadata") {
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0))

    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 3.0, 5.0, 7.0)),
      "rt" -> Column.Doubles(Vector(500.0, 600.0, 450.0, 550.0))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(Scale(rt))",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0, 0)
    )

    assertEquals(model.termKeys, Vector("z_rt"))

    val colmap = model.designColmap
    assertEquals(colmap.length, model.designMatrix.cols)
    assertEquals(colmap.head.modulationType, Some(ModulationType.Parametric))
    assertEquals(colmap.head.modulationId, Some("rt"))
    assertEquals(colmap.head.prettyName, "rt")
  }

  test("EventModelBuilder supports trialwise() with optional mean column") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0))

    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(2.0, 8.0, 14.0, 20.0, 26.0))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ trialwise(basis = \"spmg1\", add_sum = TRUE, label = \"trial\")",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0, 0, 0)
    )

    // One column per trial + the mean column.
    assertEquals(model.termKeys, Vector("trial"))
    assert(model.terms.head._2.isTrialwise)
    assertEquals(model.designMatrix.cols, 6)
    assertEquals(model.columnNames.last, "trial_mean")

    val term = model.terms.head._2
    assertEquals(term.resolvedColumnRoles.take(5), Vector.fill(5)(EventTermColumnRole.Trial))
    assertEquals(term.resolvedColumnRoles.lastOption, Some(EventTermColumnRole.TrialAggregate))

    val colmap = model.designColmap
    assert(colmap.take(5).forall(_.role == ColumnRole.Trial))
    assertEquals(colmap.last.termTag, Some("trial"))
    assertEquals(colmap.last.role, ColumnRole.TrialAggregate)
    assertEquals(colmap.last.condition, Some("mean"))
  }

  test("designColmap(baseline) includes drift/intercept roles and run ids") {
    val sf = SamplingFrame(blockLens = Seq(6, 7), tr = Seq(1.0))
    val bmod = BaselineModel.build(samplingFrame = sf, basis = BaselineBasis.Bs, degree = 5, intercept = Intercept.Runwise)

    val colmap = bmod.designColmap
    assertEquals(colmap.length, bmod.designMatrix.cols)
    assert(colmap.forall(_.modelSource == ModelSource.Baseline))
    assert(colmap.exists(_.role == ColumnRole.Drift))
    assert(colmap.exists(_.role == ColumnRole.Intercept))

    val driftRuns = colmap.filter(_.role == ColumnRole.Drift).flatMap(_.run).distinct.sorted
    assertEquals(driftRuns, Vector(1, 2))
  }
