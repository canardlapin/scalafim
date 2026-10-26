package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.design.event.*
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class SourceEventProvenanceSuite extends munit.FunSuite:
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

  test("term-specific missing rows retain independent task and slope source occurrences"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 2.0, 3.0)),
      "condition" -> Column.Strings(Vector("A", "A", "B")),
      "x" -> Column.Doubles(Vector(1.0, Double.NaN, 3.0)))
    val model = EventModelBuilder.build("onset ~ hrf(condition, id = task) + hrf(x, id = slope)", data,
      frame, blockIds = Vector(0, 0, 1), missingValuePolicy = MissingValuePolicy.DropFromTerm)
    val rows = model.designSchema.audit.sourceEvents.groupBy(_.term.value)
    assertEquals(rows("task").map(_.sourceRow), Vector(0, 1, 2))
    assertEquals(rows("slope").map(_.sourceRow), Vector(0, 2))
    assertEquals(rows("slope").map(_.eventIndex), Vector(0, 1))

  test("derived missing-row removal maps ordinary terms and audit to original caller rows"):
    import scalafim.fmri.design.formula.{DerivedEventPlan, DerivedMissingRows}
    import EventModelBuilder.*
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 2.0, 3.0, 1.0, 2.0, 3.0)),
      "gain" -> Column.Doubles(Vector(1.0, Double.NaN, 3.0, 4.0, 5.0, 6.0)))
    val derived = DerivedEventPlan.parse(Vector("reward: number = gain * 2"), DerivedMissingRows.Drop).toOption.get
    val request = EventDesignRequest.fromText("onset ~ hrf(reward, id = slope)", data, frame,
      BlockPlan.explicit(Vector(0, 0, 0, 1, 1, 1)), derived = derived).toOption.get
    val model = buildEither(request).fold(error => fail(error.message), identity)
    val expected = Vector(0, 2, 3, 4, 5)
    val term = model.terms.head._2.asInstanceOf[ConvolvedTerm].term
    assertEquals(term.sourceRowIndices, expected)
    assertEquals(model.designSchema.audit.sourceEvents.map(_.sourceRow), expected)
    assertEquals(model.designSchema.audit.sourceEvents.map(_.blockId), Vector(0, 0, 1, 1, 1))
    assertEquals(model.designSchema.audit.sourceEvents.map(_.eventIndex), expected.indices.toVector)
    assert(model.withDerivedRowsEither(model.designSchema.audit.derivedRows.head).isLeft,
      "caller rows outside the materialized table must not be remapped a second time")

  test("adding available source evidence does not change matrix bits or legacy audit semantics"):
    val unknown = EventTerm(Vector(Event.factor(Vector("A", "B"), "condition")), Vector(1.s, 3.s), termTag = Some("task"))
    val known = unknown.copy(sourceRows = Vector(2, 7))
    val first = EventModel.build(Vector(unknown.convolve(Hrfs.SPMG1, frame)), frame).designSchema
    val second = EventModel.build(Vector(known.convolve(Hrfs.SPMG1, frame)), frame).designSchema
    assertEquals(first.matrix.data.map(java.lang.Double.doubleToLongBits).toVector,
      second.matrix.data.map(java.lang.Double.doubleToLongBits).toVector)
    assertEquals(first.columnIds, second.columnIds)
    assertEquals(first.audit.legacyV2Canonical, second.audit.legacyV2Canonical)
    assertNotEquals(first.fingerprint, second.fingerprint)

  test("typed construction rejects invalid ordinary source rows before creating a term"):
    val events = Vector(Event.factor(Vector("A"), "condition"))
    assert(EventTerm.validated(events, Vector(1.s), sourceRows = Vector(-1)).isLeft)
    assert(EventTerm.validated(events, Vector(1.s), sourceRows = Vector(0, 1)).isLeft)

  test("an empty formula subset records no source rows rather than restoring its input"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 2.0)),
      "condition" -> Column.Strings(Vector("A", "B")), "keep" -> Column.Bools(Vector(false, false)))
    val model = EventModelBuilder.build("onset ~ hrf(condition, subset = keep, id = task)", data,
      frame, blockIds = Vector(0, 1))
    assertEquals(model.designSchema.audit.sourceEvents, Vector.empty)
    assert(model.terms.collect { case (_, term: ConvolvedTerm) => term.term.sourceRowIndices }.forall(_.isEmpty))
