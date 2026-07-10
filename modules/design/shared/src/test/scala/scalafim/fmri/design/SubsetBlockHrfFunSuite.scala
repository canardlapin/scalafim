package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.ConvolvedTerm
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.design.hrf.HrfSelection
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class SubsetBlockHrfFunSuite extends munit.FunSuite:

  test("EventModelBuilder parses block=~run and canonicalizes to 0-based ids") {
    val sf = SamplingFrame(blockLens = Seq(10, 10), tr = Seq(1.0), startTime = Seq(0.0))

    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 2.0, 1.0, 2.0)),
      "run" -> Column.Ints(Vector(1, 1, 2, 2)),
      "cond" -> Column.Strings(Vector("A", "B", "A", "B"))
    )

    val model = EventModelBuilder.buildWithBlockFormula(
      formula = "onset ~ hrf(cond)",
      data = events,
      samplingFrame = sf,
      block = "~run"
    )

    val ct = model.terms.head._2 match
      case c: ConvolvedTerm => c
      case other            => fail(s"expected ConvolvedTerm, found $other")

    assertEquals(ct.term.blockIds0, Vector(0, 0, 1, 1))
  }

  test("EventModelBuilder rejects decreasing block ids") {
    val sf = SamplingFrame(blockLens = Seq(10, 10), tr = Seq(1.0), startTime = Seq(0.0))

    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 2.0, 1.0, 2.0)),
      "run" -> Column.Ints(Vector(2, 2, 1, 1)),
      "cond" -> Column.Strings(Vector("A", "B", "A", "B"))
    )

    intercept[IllegalArgumentException] {
      EventModelBuilder.buildWithBlockFormula(
        formula = "onset ~ hrf(cond)",
        data = events,
        samplingFrame = sf,
        block = "~run"
      )
    }
  }

  test("EventModelBuilder applies subset= mask to realized EventTerm") {
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0), startTime = Seq(0.0))

    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 2.0, 3.0, 4.0)),
      "cond" -> Column.Strings(Vector("A", "B", "A", "B")),
      "keep" -> Column.Bools(Vector(true, false, true, false))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(cond, subset = keep)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0, 0)
    )

    val ct = model.terms.head._2 match
      case c: ConvolvedTerm => c
      case other            => fail(s"expected ConvolvedTerm, found $other")

    assertEquals(ct.term.onsets.map(_.value), Vector(1.0, 3.0))
    assertEquals(ct.term.blockIds0, Vector(0, 0))
  }

  test("EventModelBuilder supports per-onset HRF via hrf_fun generator") {
    val sf = SamplingFrame(blockLens = Seq(6), tr = Seq(1.0), startTime = Seq(0.0))

    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 3.0)),
      "x" -> Column.Doubles(Vector(1.0, 1.0))
    )

    val h1 = Hrfs.boxcar(1.0.s)
    val h2 = Hrfs.boxcar(2.0.s)

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(x, hrf_fun = myHrfFun)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0),
      hrfFuns = Map("myHrfFun" -> (_ => HrfSelection.perEvent(Seq(h1, h2))))
    )

    assertEquals(model.designMatrix.rows, 6)
    assertEquals(model.designMatrix.cols, 1)
    assertEquals(model.designMatrix.col(0).data.toVector, Vector(1.0, 0.0, 0.0, 1.0, 1.0, 0.0))
  }

  test("EventModelBuilder supports per-onset HRF via HRF list column") {
    val sf = SamplingFrame(blockLens = Seq(6), tr = Seq(1.0), startTime = Seq(0.0))

    val h1 = Hrfs.boxcar(1.0.s)
    val h2 = Hrfs.boxcar(2.0.s)

    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 3.0)),
      "x" -> Column.Doubles(Vector(1.0, 1.0)),
      "hrfs" -> Column.Hrfs(Vector(h1, h2))
    )

    val model = EventModelBuilder.build(
      formula = """onset ~ hrf(x, hrf_fun = "hrfs")""",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0)
    )

    assertEquals(model.designMatrix.col(0).data.toVector, Vector(1.0, 0.0, 0.0, 1.0, 1.0, 0.0))
  }
