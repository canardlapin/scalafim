package scalafim.fmri.design

import scalafim.fmri.design.contrast.{ContrastRegistry, ContrastSpec}
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.{ConvolvedTerm, EventModel, EventModelDiagnosticKind}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.design.SamplingFrame

class EventModelBuilderParitySuite extends munit.FunSuite:

  private def convolved(model: EventModel, ix: Int = 0): ConvolvedTerm =
    model.terms(ix)._2 match
      case c: ConvolvedTerm => c
      case other            => fail(s"expected ConvolvedTerm, found $other")

  private def maxAbs(xs: Array[Double]): Double =
    xs.iterator.map(math.abs).max

  test("hrf supports per-term onsets, durations, and prefix") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 0.0, 0.0)),
      "stim_onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "dur" -> Column.Doubles(Vector(0.5, 1.0, 1.5)),
      "cond" -> Column.Strings(Vector("A", "B", "A")),
      "rt" -> Column.Doubles(Vector(2.0, 4.0, 6.0))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(cond, rt, onsets = stim_onset, durations = dur, prefix = pre)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0)
    )

    val ct = convolved(model)
    assertEquals(model.termKeys, Vector("pre"))
    assertEquals(ct.term.onsets.map(_.value), Vector(1.0, 5.0, 9.0))
    assertEquals(ct.term.durations0.map(_.value), Vector(0.5, 1.0, 1.5))
    assertEquals(ct.columnNames, Vector("pre_cond.A_rt", "pre_cond.B_rt"))
  }

  test("hrf normalize scales convolved columns to unit maximum absolute value") {
    val sf = SamplingFrame(blockLens = Seq(40), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 8.0, 15.0)),
      "x" -> Column.Doubles(Vector(1.0, 2.0, 4.0))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(x, normalize = TRUE)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0)
    )

    assert(math.abs(maxAbs(model.designMatrix.data) - 1.0) < 1e-8)
  }

  test("fourier HRF basis is available through formula basis") {
    val sf = SamplingFrame(blockLens = Seq(40), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 8.0)),
      "cond" -> Column.Strings(Vector("A", "B"))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(cond, basis = fourier, nbasis = 3)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0)
    )

    assertEquals(model.designMatrix.cols, 6)
    assert(model.columnNames.contains("cond_cond.A_b03"))
    assert(model.columnNames.contains("cond_cond.B_b03"))
  }

  test("builder records degenerate continuous modulator diagnostics") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "x" -> Column.Doubles(Vector(1.0, 1.0, 1.0))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(x)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0)
    )

    assert(model.diagnostics.exists(_.kind == EventModelDiagnosticKind.DegenerateModulator))
    assert(model.diagnostics.exists(_.message.contains("zero variance")))
  }

  test("builder records all-zero and non-finite continuous modulator diagnostics") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val zeroEvents = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "z" -> Column.Doubles(Vector(0.0, 0.0, 0.0))
    )

    val zeroModel = EventModelBuilder.build(
      formula = "onset ~ hrf(z)",
      data = zeroEvents,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0)
    )
    assert(zeroModel.diagnostics.exists(_.message.contains("all zero")))

    val naEvents = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "x" -> Column.Doubles(Vector(1.0, Double.NaN, 3.0))
    )

    val naModel = EventModelBuilder.build(
      formula = "onset ~ hrf(x)",
      data = naEvents,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0)
    )
    assert(naModel.diagnostics.exists(_.kind == EventModelDiagnosticKind.NonFiniteModulator))
    assertEquals(naModel.designMatrix.cols, 1)
    assert(naModel.designMatrix.data.forall(_.isFinite))
  }

  test("builder records onset-bound diagnostics and strict mode promotes them to errors") {
    val sf = SamplingFrame(blockLens = Seq(10), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(-1.0, 12.0)),
      "cond" -> Column.Strings(Vector("A", "B"))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(cond)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0)
    )
    assert(model.diagnostics.exists(_.kind == EventModelDiagnosticKind.OnsetOutOfBounds))
    assert(model.diagnostics.exists(_.message.contains("negative onset")))
    assert(model.diagnostics.exists(_.message.contains("outside block")))

    intercept[IllegalArgumentException] {
      EventModelBuilder.build(
        formula = "onset ~ hrf(cond)",
        data = events,
        samplingFrame = sf,
        blockIds = Vector(0, 0),
        strict = true
      )
    }
  }

  test("onset-bound diagnostics respect per-block TR, length, and event duration") {
    val sf = SamplingFrame(blockLens = Seq(5, 4), tr = Seq(2.0, 1.0), startTime = Seq(0.0, 0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(9.0, 3.0, 4.0)),
      "dur" -> Column.Doubles(Vector(2.0, 0.5, 0.0)),
      "cond" -> Column.Strings(Vector("A", "B", "A"))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(cond, durations = dur)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 1, 1)
    )

    val messages = model.diagnostics.map(_.message).mkString("\n")
    assert(messages.contains("ends at 11"))
    assert(messages.contains("starts at 4"))
    assert(messages.contains("block index 1 ending at 4"))

    intercept[IllegalArgumentException] {
      EventModelBuilder.build(
        formula = "onset ~ hrf(cond, durations = dur)",
        data = events,
        samplingFrame = sf,
        blockIds = Vector(0, 1, 1),
        strict = true
      )
    }
  }

  test("trialwise supports duration overrides") {
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "dur" -> Column.Doubles(Vector(0.1, 0.2, 0.3))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ trialwise(durations = dur)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0)
    )

    assertEquals(convolved(model).term.durations0.map(_.value), Vector(0.1, 0.2, 0.3))
  }

  test("attached and F contrast keys use canonical interaction term key") {
    val sf = SamplingFrame(blockLens = Seq(40), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0, 13.0)),
      "cond" -> Column.Strings(Vector("A", "B", "A", "B")),
      "cue" -> Column.Strings(Vector("X", "X", "Y", "Y"))
    )
    val cset = ContrastSpec.ContrastSet(ContrastSpec.UnitContrast(name = "unit"))

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(cond, cue, contrasts = myset)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0, 0),
      contrastSets = Map("myset" -> cset)
    )

    import ContrastRegistry.*

    assertEquals(model.termKeys, Vector("cond_cue"))
    assert(model.colIndices.contains("cond_cue"))
    assertEquals(model.contrastWeights.keySet, Set("cond_cue#unit"))

    val fKeys = model.fContrastWeights().keySet
    assert(fKeys.contains("cond_cue#cond"))
    assert(fKeys.contains("cond_cue#cue"))
    assert(fKeys.contains("cond_cue#cond:cue"))
    assert(!fKeys.exists(_.startsWith("cond:cue#")))
  }
