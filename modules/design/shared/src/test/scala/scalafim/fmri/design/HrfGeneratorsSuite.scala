package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.design.hrf.HrfGenerators
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class HrfGeneratorsSuite extends munit.FunSuite:

  test("duration HRF generator preserves zero-duration events and blocks positive durations") {
    val data = DataTable.fromColumns(
      "duration" -> Column.Doubles(Vector(0.0, 2.0))
    )

    val result = HrfGenerators.duration()(data)
    val hrfs =
      result match
        case h: Hrf     => Vector(h)
        case hs: Seq[?] => hs.asInstanceOf[Seq[Hrf]].toVector

    assertEquals(hrfs.length, 2)
    assertEquals(hrfs.head.name, Hrfs.SPMG1.name)
    assert(hrfs(1).name.contains("_block"))

    val peak = (0 to 260).iterator.map(i => math.abs(hrfs(1)((i.toDouble / 10.0).s).data(0))).max
    assert(math.abs(peak - 1.0) < 1e-8)
  }

  test("weighted HRF generator works with list columns") {
    val sf = SamplingFrame(blockLens = Seq(50), tr = Seq(2.0), startTime = Seq(0.0))

    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 20.0)),
      "condition" -> Column.Strings(Vector("A", "B")),
      "sub_times" -> Column.DoubleLists(Vector(Vector(0.0, 1.0, 2.0), Vector(0.0, 3.0, 6.0))),
      "sub_weights" -> Column.DoubleLists(Vector(Vector(0.2, 0.5, 0.3), Vector(0.1, 0.6, 0.3)))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(condition, hrf_fun = weighted)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0),
      hrfFuns = Map("weighted" -> HrfGenerators.weighted(relative = true))
    )

    assertEquals(model.designMatrix.rows, 50)
    assertEquals(model.designMatrix.cols, 2)
  }

  test("weighted HRF generator sees onset/duration/blockid and original list columns") {
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0), startTime = Seq(0.0))

    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 10.0)),
      "condition" -> Column.Strings(Vector("A", "B")),
      "sub_times" -> Column.DoubleLists(Vector(Vector(0.0, 1.0), Vector(0.0, 2.0))),
      "sub_weights" -> Column.DoubleLists(Vector(Vector(0.5, 0.5), Vector(0.2, 0.8)))
    )

    var received: Vector[String] = Vector.empty
    val gen: scalafim.fmri.design.hrf.HrfFun = d =>
      received = d.names
      Vector.fill(d.nrows)(Hrfs.SPMG1)

    EventModelBuilder.build(
      formula = "onset ~ hrf(condition, hrf_fun = gen)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0),
      durations = Seq(0.0),
      hrfFuns = Map("gen" -> gen)
    )

    assert(received.contains("onset"))
    assert(received.contains("duration"))
    assert(received.contains("blockid"))
    assert(received.contains("condition"))
    assert(received.contains("sub_times"))
    assert(received.contains("sub_weights"))
  }

  test("hrf_fun generator respects subset= and is not called for empty terms") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(2.0), startTime = Seq(0.0))

    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 10.0, 20.0)),
      "condition" -> Column.Strings(Vector("A", "B", "A"))
    )

    var nReceived = -1
    val tracking: scalafim.fmri.design.hrf.HrfFun = d =>
      nReceived = d.nrows
      Vector.fill(d.nrows)(Hrfs.SPMG1)

    EventModelBuilder.build(
      formula = """onset ~ hrf(condition, hrf_fun = tracking, subset = condition == "A")""",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0),
      hrfFuns = Map("tracking" -> tracking)
    )
    assertEquals(nReceived, 2)

    var called = false
    val never: scalafim.fmri.design.hrf.HrfFun = d =>
      called = true
      Vector.fill(d.nrows)(Hrfs.SPMG1)

    EventModelBuilder.build(
      formula = """onset ~ hrf(condition, hrf_fun = never, subset = condition == "C")""",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0),
      hrfFuns = Map("never" -> never)
    )
    assert(!called)
  }

  test("hrf_fun validation catches length mismatch and inconsistent nbasis") {
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0), startTime = Seq(0.0))

    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 5.0, 10.0)),
      "condition" -> Column.Strings(Vector("A", "B", "C"))
    )

    val wrongLen: scalafim.fmri.design.hrf.HrfFun = _ => Seq(Hrfs.SPMG1, Hrfs.SPMG1)
    intercept[IllegalArgumentException] {
      EventModelBuilder.build(
        formula = "onset ~ hrf(condition, hrf_fun = wrongLen)",
        data = events,
        samplingFrame = sf,
        blockIds = Vector(0, 0, 0),
        hrfFuns = Map("wrongLen" -> wrongLen)
      )
    }

    val mixedNbasis: scalafim.fmri.design.hrf.HrfFun = _ => Seq(Hrfs.SPMG1, Hrfs.SPMG2, Hrfs.SPMG1)
    intercept[IllegalArgumentException] {
      EventModelBuilder.build(
        formula = "onset ~ hrf(condition, hrf_fun = mixedNbasis)",
        data = events,
        samplingFrame = sf,
        blockIds = Vector(0, 0, 0),
        hrfFuns = Map("mixedNbasis" -> mixedNbasis)
      )
    }
  }
