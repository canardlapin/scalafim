package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class CovariateSuite extends munit.FunSuite:

  test("CovariateSpec.construct validates sampling frame row count") {
    val sf = SamplingFrame(blockLens = Seq(5), tr = Seq(1.0))
    val bad = DataTable.fromColumns("x" -> Column.Doubles(Vector(1.0, 2.0)))
    val spec = CovariateSpec(vars = Vector(ColumnId.unsafe("x")), data = bad)

    assert(spec.construct(sf).isLeft)
  }

  test("EventModel.buildTerms can mix convolved and covariate terms") {
    val sf = SamplingFrame(blockLens = Seq(10), tr = Seq(1.0))

    // Event term (convolved)
    val eventData = Vector("A", "B", "A", "B")
    val onsets = Vector(1.0, 2.0, 3.0, 4.0).map(Seconds(_))
    val blockIds = Vector(0, 0, 0, 0)
    val et = EventTerm(
      events = Vector(Event.factor(eventData, "cond")),
      onsets = onsets,
      blockIds = blockIds,
      termTag = Some("task")
    )
    val conv = et.convolve(Hrfs.SPMG1, sf)

    // Covariate term (not convolved)
    val covData = DataTable.fromColumns(
      "x" -> Column.Doubles(Vector.tabulate(10)(_.toDouble)),
      "y" -> Column.Doubles(Vector.fill(10)(1.0))
    )
    val cov = CovariateSpec(vars = Vector("x", "y").map(ColumnId.unsafe), data = covData, id = Some("motion"), prefix = Some("motion"))
      .construct(sf)
      .fold(err => fail(err.message), identity)

    val model = EventModel.buildTerms(Seq(conv, cov), sf)

    assertEquals(model.designMatrix.rows, 10)
    assertEquals(model.termKeys, Vector("task", "motion"))
    assertEquals(model.colIndices("motion").length, 2)
    assertEquals(model.columnNames.takeRight(2), Vector("motion_x", "motion_y"))
  }
