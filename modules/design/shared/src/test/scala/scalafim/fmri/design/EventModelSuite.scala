package scalafim.fmri.design

import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class EventModelSuite extends munit.FunSuite:

  test("basic event model creation works") {
    val eventData = Vector("A", "B", "A", "B")
    val onsets = Vector(1.0, 10.0, 20.0, 30.0).map(Seconds(_))
    val blockIds = Vector(0, 0, 0, 0)

    val sf = SamplingFrame(blockLens = Seq(40), tr = Seq(2.0))

    val term = EventTerm(
      events = Vector(Event.factor(eventData, "condition")),
      onsets = onsets,
      blockIds = blockIds,
      termTag = Some("condition")
    )

    val conv = term.convolve(Hrfs.SPMG1, sf)
    val model = EventModel.build(Seq(conv), sf)

    assertEquals(model.designMatrix.rows, 40)
    assertEquals(model.columnNames.length, model.designMatrix.cols)
    assertEquals(model.designMatrix.cols, 2)
  }

  test("feature and basis suffixes combine in convolve") {
    val mat = Mat.unsafe(3, 2, Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))
    val onsets = Vector(1.0, 2.0, 3.0).map(Seconds(_))
    val sf = SamplingFrame(blockLens = Seq(10), tr = Seq(1.0))

    val term = EventTerm(
      events = Vector(Event.matrix(mat, name = "m")),
      onsets = onsets,
      blockIds = Vector(0, 0, 0),
      termTag = Some("term")
    )

    val conv = term.convolve(Hrfs.SPMG3, sf)
    assertEquals(
      conv.columnNames,
      Vector(
        "term_f01_b01",
        "term_f02_b01",
        "term_f01_b02",
        "term_f02_b02",
        "term_f01_b03",
        "term_f02_b03"
      )
    )
    assertEquals(conv.data.rows, 10)
    assertEquals(conv.data.cols, 6)
  }

  test("EventModel rejects term metadata that does not match matrix columns") {
    val sf = SamplingFrame(blockLens = Seq(2), tr = Seq(1.0))
    val badTerm = new EventModelTerm:
      def data: Mat = Mat.unsafe(2, 2, Array(1.0, 0.0, 0.0, 1.0))
      def columnNames: Vector[String] = Vector("one")
      def columnRoles: Vector[EventTermColumnRole] = Vector.empty
      def keyHint: Option[String] = Some("bad")
      def hrfOpt: Option[Hrf] = None
      def role: EventTermRole = EventTermRole.Task

    val error = intercept[IllegalArgumentException] {
      EventModel.buildTerms(Seq(badTerm), sf)
    }
    assert(error.getMessage.contains("columnNames"))
  }

  test("ConvolvedTerm rejects explicit column roles that do not match matrix columns") {
    val mat = Mat.unsafe(2, 2, Array(1.0, 0.0, 0.0, 1.0))
    val term = EventTerm(
      events = Vector(Event.factor(Vector("A", "B"), "condition")),
      onsets = Vector(1.0, 2.0).map(Seconds(_)),
      blockIds = Vector(0, 0),
      termTag = Some("condition")
    )

    val error = intercept[IllegalArgumentException] {
      ConvolvedTerm(
        term = term,
        hrf = Hrfs.SPMG1,
        data = mat,
        columnNames = Vector("condition_A", "condition_B"),
        columnRoles = Vector(EventTermColumnRole.Task)
      )
    }
    assert(error.getMessage.contains("columnRoles"))
  }
