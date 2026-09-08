package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class EventSourceRowsSuite extends munit.FunSuite:
  test("retained signed events are counted as used and never falsely recorded as excluded"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(-1.0)),
      "condition" -> Column.Strings(Vector("A")))
    val model = EventModelBuilder.build("onset ~ hrf(condition)", data,
      SamplingFrame(blockLens = Seq(8), tr = Seq(1.0)), blockIds = Vector(0),
      defaultHrf = Hrfs.fir(nBasis = 2, span = 4.s), precision = 0.25.s)
    assert(model.designMatrix.data.exists(_ > 0.0))
    assertEquals(model.designSchema.audit.eventsSeen, 1)
    assertEquals(model.designSchema.audit.eventsUsed, 1)
    assertEquals(model.designSchema.audit.excludedEvents, Vector.empty)

  private val frame = SamplingFrame(blockLens = Seq(12, 12), tr = Seq(1.0))

  test("subset and missing-value masks preserve original rows without inventing trial identities"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 2.0, 3.0, 4.0, 5.0)),
      "x" -> Column.Doubles(Vector(10.0, Double.NaN, 30.0, 40.0, 50.0)),
      "keep" -> Column.Bools(Vector(false, true, true, false, true)))
    val model = EventModelBuilder.build("onset ~ hrf(center(x), subset = keep, id = response)", data,
      frame, blockIds = Vector(0, 0, 0, 1, 1), missingValuePolicy = MissingValuePolicy.DropFromTerm)
    val term = model.terms.head._2.asInstanceOf[ConvolvedTerm].term
    assertEquals(term.sourceRowIndices, Vector(2, 4))
    assertEquals(term.eventProvenance, Vector.empty)
    val rows = model.designSchema.audit.sourceEvents
    assertEquals(rows.map(_.eventIndex), Vector(0, 1))
    assertEquals(rows.map(_.sourceRow), Vector(2, 4))
    assertEquals(rows.map(_.blockId), Vector(0, 1))
    assertEquals(rows.map(_.onset.value), Vector(3.0, 5.0))
    assert(rows.forall(_.term.value == model.termKeys.head))

  test("trialwise and multiple ordinary terms carry term-scoped original rows"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 3.0)),
      "condition" -> Column.Strings(Vector("A", "B")))
    val model = EventModelBuilder.build("onset ~ hrf(condition, id = task) + trialwise(label = trials)",
      data, frame, blockIds = Vector(0, 1))
    val grouped = model.designSchema.audit.sourceEvents.groupBy(_.term)
    assertEquals(grouped.size, 2)
    grouped.values.foreach(rows => assertEquals(rows.map(_.sourceRow), Vector(0, 1)))

  test("phase source rows retain their explicit parent trials and reject conflicting row annotations"):
    val phase = EventPhase.fromParts(PhaseId.unsafe("sample"), Vector(1.s, 2.s), Vector(0.s, 0.s),
      Vector(0, 1), Vector(TrialId.unsafe("t1"), TrialId.unsafe("t2")), Vector(5, 9)).toOption.get
    val term = phase.term(Vector(Event.factor(Vector("A", "B"), "condition")), Some("sample")).toOption.get
    assertEquals(term.sourceRowIndices, Vector(5, 9))
    val model = EventModel.build(Vector(term.convolve(Hrfs.SPMG1, frame)), frame)
    assertEquals(model.designSchema.audit.sourceEvents.map(_.sourceRow), Vector(5, 9))
    assertEquals(model.designSchema.audit.eventProvenance.map(_.parent.value), Vector("t1", "t2"))
    intercept[IllegalArgumentException](term.copy(sourceRows = Vector(0, 1)))

  test("source identity changes the design fingerprint and survives schema combination"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 3.0)),
      "condition" -> Column.Strings(Vector("A", "B")))
    val schema = EventModelBuilder.build("onset ~ hrf(condition)", data, frame,
      blockIds = Vector(0, 1)).designSchema
    val changedRows = schema.audit.sourceEvents.map(row => row.copy(sourceRow = row.sourceRow + 10))
    val changed = DesignSchema.validated(schema.matrix, schema.rows, schema.columns,
      schema.audit.copy(sourceEvents = changedRows)).toOption.get
    assertNotEquals(schema.fingerprint.value, changed.fingerprint.value)
    val empty = DesignSchema.validated(Mat.zeros(schema.matrix.rows, 0), schema.rows, Vector.empty).toOption.get
    val combined = DesignSchema.combine(empty, schema).toOption.get
    assertEquals(combined.audit.sourceEvents, schema.audit.sourceEvents)
    assert(combined.fingerprint.canonicalEncoding.contains(schema.audit.sourceEvents.head.canonical))

  test("hand-built terms keep unknown source identity explicit"):
    val term = EventTerm(Vector(Event.factor(Vector("A"), "condition")), Vector(1.s))
    assertEquals(term.sourceRowIndices, Vector.empty)
    intercept[IllegalArgumentException](term.copy(sourceRows = Vector(-1)))
    intercept[IllegalArgumentException](term.copy(sourceRows = Vector(0, 1)))
