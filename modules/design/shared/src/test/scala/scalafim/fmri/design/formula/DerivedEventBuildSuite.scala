package scalafim.fmri.design.formula

import scalafim.fmri.design.{ColumnId, DesignError, FactorPartitionAudit, RunIndex}
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame

class DerivedEventBuildSuite extends munit.FunSuite:
  import EventModelBuilder.*

  private def id(value: String): ColumnId = ColumnId.unsafe(value)
  private val frame = SamplingFrame(blockLens = Seq(24, 24), tr = Seq(1.0))
  private val data = DataTable.fromColumns(
    "onset" -> Column.Doubles(Vector(2.0, 8.0, 14.0, 2.0, 8.0, 14.0)),
    "gain" -> Column.Doubles(Vector(1.0, Double.NaN, 5.0, 4.0, 2.0, 6.0))
  )
  private val blocks = Vector(0, 0, 0, 1, 1, 1)
  private val formula = "onset ~ hrf(level, id = bins) + hrf(reward, id = slope)"
  private val declarations = Vector(
    "reward: number = gain * 2",
    "level: text = cut(gain, c(-Inf, 3, Inf), c(\"small\", \"large\"))",
    "unobserved: logical = is.na(gain)"
  )

  private def plan(policy: DerivedMissingRows): DerivedEventPlan =
    DerivedEventPlan.parse(declarations, policy).fold(error => fail(error.message), identity)

  private def request(table: DataTable, blockIds: Vector[Int], derived: DerivedEventPlan, formula: String = formula): EventDesignRequest =
    EventDesignRequest.fromText(formula, table, frame, BlockPlan.explicit(blockIds), DurationPlan(Seq(0.5)), derived = derived)
      .fold(error => fail(error.message), identity)

  private def built(request: EventDesignRequest): EventModel =
    buildEither(request).fold(error => fail(error.message), identity)

  test("a build from declarations equals the build from manual materialization"):
    val declared = built(request(data, blocks, plan(DerivedMissingRows.Drop)))
    val manual = plan(DerivedMissingRows.Drop).materialize(data).fold(error => fail(error.message), identity)
    assertEquals(manual.retainedRows, Vector(0, 2, 3, 4, 5))
    val reference = built(request(manual.table, manual.retainedRows.map(blocks), DerivedEventPlan.empty))
    assertEquals(declared.columnNames, reference.columnNames)
    assertEquals(declared.designMatrix.data.toVector, reference.designMatrix.data.toVector)
    assertEquals(declared.designSchema.audit.copy(derivedRows = Vector.empty), reference.designSchema.audit)
    assertEquivalent(declared, reference, declared.designSchema.audit.derivedRows.head)
    // The policy is part of scientific identity.
    assertNotEquals(declared.designSchema.fingerprint.value, reference.designSchema.fingerprint.value)

  /** The declared build is the manual build with its compiled rows renumbered
    * to the caller's rows and the receipt attached.
    */
  private def assertEquivalent(declared: EventModel, manual: EventModel, receipt: DerivedRowsReceipt)(using munit.Location): Unit =
    assertEquals(declared.columnNames, manual.columnNames)
    assertEquals(declared.designMatrix.data.toVector, manual.designMatrix.data.toVector)
    val renumbered = manual.withDerivedRowsEither(receipt).fold(error => fail(error.message), identity)
    assertEquals(declared.designSchema.audit, renumbered.designSchema.audit)
    assertEquals(declared.designSchema.fingerprint, renumbered.designSchema.fingerprint)

  private def table(gain: Vector[Double], extra: (String, Column)*): DataTable =
    DataTable.fromColumns((Vector("onset" -> Column.Doubles(Vector(2.0, 8.0, 14.0, 2.0, 8.0, 14.0)), "gain" -> Column.Doubles(gain)) ++ extra)*)

  test("a dropped row in a later run keeps per-run centering equal to manual materialization"):
    val values = table(Vector(1.0, 2.0, 5.0, 4.0, Double.NaN, 6.0))
    val centered = "onset ~ hrf(level, modulators(modulator(reward, center = run, scale = raw, missing = zero)), include_main = TRUE, id = task)"
    val declared = built(request(values, blocks, plan(DerivedMissingRows.Drop), centered))
    val manual = plan(DerivedMissingRows.Drop).materialize(values).toOption.get
    assertEquals(manual.droppedRows, Vector(4))
    val reference = built(request(manual.table, manual.retainedRows.map(blocks), DerivedEventPlan.empty, centered))
    val uncentered = built(request(values, blocks, plan(DerivedMissingRows.Drop), centered.replace("center = run, scale = raw, missing = zero", "center = none, scale = raw, missing = reject")))
    assertNotEquals(declared.designMatrix.data.toVector, uncentered.designMatrix.data.toVector)
    assertEquivalent(declared, reference, declared.designSchema.audit.derivedRows.head)
    assertEquals(declared.designSchema.audit.derivedRows.head.dropped.map(_.run), Vector(RunIndex.unsafeOneBased(2)))

  test("a run whose rows are all dropped is still reported by per-run factor audits"):
    val values = table(Vector(1.0, 5.0, 2.0, Double.NaN, Double.NaN, Double.NaN))
    val declared = built(request(values, blocks, plan(DerivedMissingRows.Drop)))
    val manual = plan(DerivedMissingRows.Drop).materialize(values).toOption.get
    val reference = built(request(manual.table, manual.retainedRows.map(blocks), DerivedEventPlan.empty))
    assertEquals(declared.columnNames, reference.columnNames)
    assertEquals(declared.designMatrix.data.toVector, reference.designMatrix.data.toVector)
    val partitions = declared.designSchema.audit.factorLevels.flatMap(_.partitions)
    assertEquals(partitions.map(_.partition), Vector("run-1", "run-2"))
    assertEquals(partitions.last, FactorPartitionAudit("run-2", Vector.empty))
    assertEquals(declared.designSchema.audit.derivedRows.head.dropped.map(_.sourceRow), Vector(3, 4, 5))

  test("a policy that drops every row fails explicitly"):
    val values = table(Vector.fill(6)(Double.NaN))
    assertEquals(buildEither(request(values, blocks, plan(DerivedMissingRows.Drop))),
      Left(DesignError.DerivedColumns(DerivedEventError.NoRetainedRows(DerivedMissingRows.Drop, 6))))

  test("block formulas and per-row durations are restricted to the retained rows"):
    val values = table(Vector(1.0, 5.0, Double.NaN, 4.0, 2.0, 6.0), "run" -> Column.Ints(Vector(1, 1, 1, 2, 2, 2)))
    val durations = DurationPlan(Seq(0.5, 1.0, 1.5, 2.0, 2.5, 3.0))
    def formulaRequest(data: DataTable, durationPlan: DurationPlan, derived: DerivedEventPlan): EventDesignRequest =
      EventDesignRequest.fromText(formula, data, frame, BlockPlan.Formula("~run"), durationPlan, derived = derived).fold(error => fail(error.message), identity)
    val declared = built(formulaRequest(values, durations, plan(DerivedMissingRows.Drop)))
    val manual = plan(DerivedMissingRows.Drop).materialize(values).toOption.get
    val reference = built(formulaRequest(manual.table, DurationPlan(manual.retainedRows.map(Vector(0.5, 1.0, 1.5, 2.0, 2.5, 3.0))), DerivedEventPlan.empty))
    assertEquivalent(declared, reference, declared.designSchema.audit.derivedRows.head)
    val shifted = built(formulaRequest(manual.table, DurationPlan(Seq(0.5, 1.0, 2.0, 2.5, 3.0).reverse), DerivedEventPlan.empty))
    assertNotEquals(declared.designMatrix.data.toVector, shifted.designMatrix.data.toVector)

  test("every caller-row index in the audit refers to the caller's table after an earlier row is dropped"):
    val values = table(Vector(1.0, Double.NaN, 5.0, 4.0, 2.0, 6.0), "trial" -> Column.Strings(Vector("t1", "t2", "t3", "t4", "t5", "t6")))
    val declarations = DerivedEventPlan.parse(Vector(
      "level: text = cut(gain, c(-Inf, 3, Inf), c(\"small\", \"large\"))",
      "flat: number = ifelse(is.na(gain), missing(), 1)"
    ), DerivedMissingRows.Drop).toOption.get
    val phased = "onset ~ hrf(level, phase = cue, parent = trial, id = cue) + hrf(flat, id = flat)"
    val declared = built(request(values, blocks, declarations, phased))
    val audit = declared.designSchema.audit
    assertEquals(audit.eventProvenance.map(_.sourceRow), Vector(0, 2, 3, 4, 5))
    assertEquals(audit.eventProvenance.map(_.parent.value), Vector("t1", "t3", "t4", "t5", "t6"))
    assertEquals(audit.eventProvenance.map(p => values.get[String](ColumnId.unsafe("trial")).toOption.get(p.sourceRow)), audit.eventProvenance.map(_.parent.value))
    assertEquals(audit.degenerateModulatorReceipts.flatMap(_.sourceRows), Vector(0, 2, 3, 4, 5))
    assertEquals(declared.degenerateModulatorReceipts, audit.degenerateModulatorReceipts)
    val term = declared.terms.collectFirst { case (_, ct: scalafim.fmri.design.event.ConvolvedTerm) if ct.term.eventProvenance.nonEmpty => ct }.get
    assertEquals(term.term.eventProvenance.map(_.sourceRow), Vector(0, 2, 3, 4, 5))
    val manual = declarations.materialize(values).toOption.get
    val reference = built(request(manual.table, manual.retainedRows.map(blocks), DerivedEventPlan.empty, phased))
    assertEquals(reference.designSchema.audit.eventProvenance.map(_.sourceRow), Vector(0, 1, 2, 3, 4))
    assertEquivalent(declared, reference, audit.derivedRows.head)

  test("dropped rows are recorded in the build audit with provenance"):
    val model = built(request(data, blocks, plan(DerivedMissingRows.Drop)))
    val receipt = model.designSchema.audit.derivedRows match
      case Vector(value) => value
      case other => fail(s"expected one derived-row receipt, got $other")
    assertEquals(receipt.policy, DerivedMissingRows.Drop)
    assertEquals(receipt.columns, Vector(id("reward"), id("level"), id("unobserved")))
    assertEquals(receipt.retainedRows, Vector(0, 2, 3, 4, 5))
    assertEquals(receipt.dropped, Vector(DerivedRowDrop(1, RunIndex.unsafeOneBased(1), Vector(id("reward"), id("level")))))
    assert(model.designSchema.audit.canonical.contains(";derived-rows=policy=drop:"), model.designSchema.audit.canonical)

  test("PreserveNumeric keeps numeric gaps for the modulator policy and rejects categorical gaps"):
    val numeric = DerivedEventPlan.parse(Vector("reward: number = gain * 2"), DerivedMissingRows.PreserveNumeric).toOption.get
    val model = built(request(data, blocks, numeric, "onset ~ hrf(reward, id = slope)"))
    val receipt = model.designSchema.audit.derivedRows.head
    assertEquals((receipt.retainedRows, receipt.dropped), (Vector.range(0, 6), Vector.empty))
    assertEquals(model.designSchema.audit.missingValues.map(_.eventIndex), Vector(1))
    assert(buildEither(request(data, blocks, plan(DerivedMissingRows.PreserveNumeric))).left.exists:
      case DesignError.DerivedColumns(DerivedEventError.MissingValues(_, rows)) => rows == Vector(1)
      case _ => false
    )

  test("the reject policy fails with the offending rows, and evaluation errors stay typed"):
    buildEither(request(data, blocks, plan(DerivedMissingRows.Reject))) match
      case Left(DesignError.DerivedColumns(DerivedEventError.MissingValues(columns, rows))) =>
        assertEquals(rows, Vector(1))
        assertEquals(columns, Vector(id("reward"), id("level"), id("unobserved")))
      case other => fail(s"expected a missing-row rejection, got $other")
    val overwrite = DerivedEventPlan.parse(Vector("gain: number = 1"), DerivedMissingRows.Drop).toOption.get
    assert(buildEither(request(data, blocks, overwrite)).left.exists:
      case DesignError.DerivedColumns(DerivedEventError.InvalidDefinition(_)) => true
      case _ => false
    )
    assert(DerivedEventPlan.parse(Vector("x: number = gain", "x: number = gain * 2"), DerivedMissingRows.Drop).isLeft)

  test("an empty plan leaves the build and its identity untouched"):
    val manual = plan(DerivedMissingRows.Drop).materialize(data).toOption.get
    val plain = built(request(manual.table, manual.retainedRows.map(blocks), DerivedEventPlan.empty))
    assertEquals(plain.designSchema.audit.derivedRows, Vector.empty)
    assert(!plain.designSchema.audit.canonical.contains("derived-rows"))

  test("incremental designs carry the same derived-row evidence"):
    val incremental = prepareIncremental(request(data, blocks, plan(DerivedMissingRows.Drop))).fold(error => fail(error.message), identity)
    val direct = built(request(data, blocks, plan(DerivedMissingRows.Drop)))
    assertEquals(incremental.model.designSchema.fingerprint, direct.designSchema.fingerprint)

  test("the derived plan and its missing-row policy round trip through JSON"):
    DerivedMissingRows.values.foreach: policy =>
      val json = ModelJsonCodec.encodeDerivedEvents(plan(policy)).fold(error => fail(error.message), identity)
      val decoded = ModelJsonCodec.decodeDerivedEvents(json).fold(error => fail(error.message), identity)
      assertEquals(decoded, plan(policy))
      assertEquals(ModelJsonCodec.encodeDerivedEvents(decoded), Right(json))
    val json = ModelJsonCodec.encodeDerivedEvents(plan(DerivedMissingRows.Drop)).toOption.get
    assert(json.contains("\"missingRows\":\"drop\""), json)
    assertEquals(ModelJsonCodec.decodeDerivedEvents(json.replace("\"drop\"", "\"Drop\"")).swap.toOption.map(_.path), Some("$.value.missingRows"))
    assertEquals(ModelJsonCodec.decodeDerivedEvents(json.replace("\"missingRows\":\"drop\",", "")).swap.toOption.map(_.path), Some("$.value"))
    assertEquals(ModelJsonCodec.decodeDerivedEvents(json.replace("gain * 2", "gain *")).swap.toOption.map(_.path), Some("$.value.columns[0]"))
