package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.{ConvolvedTerm, ContinuousEvent, EventModel}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

class EventResponseSupportSuite extends munit.FunSuite:
  private val frame = SamplingFrame(blockLens = Seq(12), tr = Seq(1.0), startTime = Seq(0.0))
  private val scans = (1 to 12).map(ScanIndex.unsafeOneBased).toVector
  private def checked[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def build(formula: String, data: DataTable, request: Option[ResponseSupportRequest],
      basis: Hrf = Hrfs.fir(nBasis = 1, span = 2.s)): Either[DesignError, EventModel] =
    for
      spec <- EventModelBuilder.EventDesignRequest.fromText(formula, data, frame,
        blockPlan = EventModelBuilder.BlockPlan.SingleBlock,
        options = EventModelBuilder.BuildOptions(defaultHrf = basis, precision = 0.25.s,
          dropEmpty = false, responseSupport = request))
      model <- EventModelBuilder.buildEither(spec)
    yield model

  test("excluding a late event precedes centering and retains original source rows"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 4.0, 100.0)),
      "x" -> Column.Doubles(Vector(1.0, 3.0, 100.0)))
    val request = ResponseSupportRequest(scans, EventSupportPolicy.ExcludeUnobservedAllowPartial)
    val model = checked(build("onset ~ hrf(center(x), id = response)", data, Some(request)))
    val term = model.terms.head._2.asInstanceOf[ConvolvedTerm].term
    val values = term.events.collectFirst { case continuous: ContinuousEvent => continuous.value.data.toVector }.get
    assertEquals(values, Vector(-1.0, 1.0))
    assertEquals(term.sourceRowIndices, Vector(0, 1))
    val audit = model.designSchema.audit
    assertEquals(audit.responseSupport.head.decisions.map(_.disposition),
      Vector(EventSupportDisposition.Retained, EventSupportDisposition.Retained, EventSupportDisposition.Excluded))
    assertEquals(audit.excludedEvents.map(_.eventIndex), Vector(2))
    assertEquals(audit.eventsSeen, 3)
    assertEquals(audit.eventsUsed, 2)
    assert(model.validateResponseSupportSelection(scans).isRight)
    assert(model.validateResponseSupportSelection(scans.drop(1)).isLeft)
    assertEquals(audit.centeringReceipts.head.groups.head.center, Some(2.0))
    val legacy = checked(build("onset ~ hrf(center(x), id = response)", data, None))
    assertNotEquals(model.designMatrix.data.toVector, legacy.designMatrix.data.toVector)
    assert(model.designSchema.fingerprint.canonicalEncoding.contains("response-support="))
    assertEquals(data.get[Double](ColumnId.unsafe("x")).toOption.get, Vector(1.0, 3.0, 100.0))

  test("rejection returns an inspectable source-addressable receipt"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 100.0)),
      "condition" -> Column.Strings(Vector("A", "A")))
    build("onset ~ hrf(condition)", data, Some(ResponseSupportRequest(scans,
      EventSupportPolicy.RejectUnobservedAllowPartial))) match
      case Left(DesignError.ResponseSupportRejected(receipt)) =>
        assertEquals(receipt.decisions.filter(_.disposition == EventSupportDisposition.Rejected).map(_.sourceRow), Vector(1))
      case other => fail(s"expected typed support rejection, got $other")

  test("excluded-only source rows must be covered by a derived-row remapping"):
    import scalafim.fmri.design.formula.{DerivedMissingRows, DerivedRowsReceipt}
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 100.0)),
      "condition" -> Column.Strings(Vector("A", "A")))
    val model = checked(build("onset ~ hrf(condition, id = task)", data,
      Some(ResponseSupportRequest(scans, EventSupportPolicy.ExcludeUnobservedAllowPartial))))
    val schema = model.designSchema
    val exclusionsOnly = model.copy(compiledSchema = Some(checked(DesignSchema.validated(
      schema.matrix, schema.rows, schema.columns, schema.audit.copy(responseSupport = Vector.empty)))))
    val short = DerivedRowsReceipt(DerivedMissingRows.Drop, Vector(ColumnId.unsafe("gain")), Vector(10), Vector.empty)
    for value <- Vector(model, exclusionsOnly) do
      assertEquals(value.withDerivedRowsEither(short),
        Left(DesignError.InvalidSchema("compiled source row 1 has no retained caller row")))
    val complete = checked(model.withDerivedRowsEither(short.copy(retainedRows = Vector(10, 20))))
    assertEquals(complete.designSchema.audit.responseSupport.head.decisions.map(_.sourceRow), Vector(10, 20))
    assertEquals(complete.designSchema.audit.excludedEvents.map(_.eventIndex), Vector(20))

  test("different terms excluding the same source row retain distinct term attribution"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 100.0)),
      "condition" -> Column.Strings(Vector("A", "A")))
    val model = checked(build("onset ~ hrf(condition, id = first) + hrf(condition, id = second)", data,
      Some(ResponseSupportRequest(scans, EventSupportPolicy.ExcludeUnobservedAllowPartial))))
    assertEquals(model.designSchema.audit.excludedEvents.map(row => (row.eventIndex, row.term.map(_.value))),
      Vector((1, Some("first")), (1, Some("second"))))

  test("partial FIR windows require an explicit accepting policy"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(-1.0, 10.0)),
      "condition" -> Column.Strings(Vector("A", "A")))
    val basis = Hrfs.fir(nBasis = 2, span = 4.s)
    val allowed = checked(build("onset ~ hrf(condition)", data,
      Some(ResponseSupportRequest(scans, EventSupportPolicy.RejectUnobservedAllowPartial)), basis))
    assertEquals(allowed.designSchema.audit.responseSupport.head.decisions.map(_.status),
      Vector(EventSupportStatus.PartialWindow, EventSupportStatus.PartialWindow))
    assert(build("onset ~ hrf(condition)", data,
      Some(ResponseSupportRequest(scans, EventSupportPolicy.RequireFullWindow)), basis).isLeft)

  test("zero modulators remain eligible in their actual factor cell"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 100.0)),
      "condition" -> Column.Strings(Vector("A", "B")), "x" -> Column.Doubles(Vector(0.0, 0.0)))
    val model = checked(build("onset ~ hrf(condition, x)", data,
      Some(ResponseSupportRequest(scans, EventSupportPolicy.ExcludeUnobservedAllowPartial))))
    val receipt = model.designSchema.audit.responseSupport.head
    assertEquals(receipt.decisions.map(_.disposition), Vector(EventSupportDisposition.Retained, EventSupportDisposition.Excluded))
    assertEquals(receipt.decisions.map(_.columns.length), Vector(1, 1))
    assertNotEquals(receipt.decisions(0).columns.head.name, receipt.decisions(1).columns.head.name)
    assertEquals(model.designSchema.audit.sourceEvents.map(_.sourceRow), Vector(0))

  test("formula subsets and policy exclusions retain distinct original row identities"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(2.0, 3.0, 100.0, 5.0)),
      "x" -> Column.Doubles(Vector(999.0, 1.0, 100.0, 3.0)),
      "keep" -> Column.Bools(Vector(false, true, true, true)))
    val model = checked(build("onset ~ hrf(center(x), subset = keep)", data,
      Some(ResponseSupportRequest(scans, EventSupportPolicy.ExcludeUnobservedAllowPartial))))
    assertEquals(model.designSchema.audit.responseSupport.head.decisions.map(_.sourceRow), Vector(1, 2, 3))
    assertEquals(model.designSchema.audit.sourceEvents.map(_.sourceRow), Vector(1, 3))
    assertEquals(model.designSchema.audit.centeringReceipts.head.groups.head.center, Some(2.0))

  test("retained scan selection controls support and trialwise terms obey the same policy"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 100.0)),
      "condition" -> Column.Strings(Vector("A", "A")))
    assert(build("onset ~ hrf(condition)", data,
      Some(ResponseSupportRequest(Vector.empty, EventSupportPolicy.RejectUnobservedAllowPartial))).isLeft)
    val trial = checked(build("onset ~ trialwise()", data,
      Some(ResponseSupportRequest(scans, EventSupportPolicy.ExcludeUnobservedAllowPartial))))
    assertEquals(trial.designSchema.audit.sourceEvents.map(_.sourceRow), Vector(0))
    assertEquals(trial.designSchema.audit.excludedEvents.map(_.eventIndex), Vector(1))

  test("data-fitted scaling is refitted on retained source rows"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 4.0, 100.0)),
      "x" -> Column.Doubles(Vector(1.0, 3.0, 100.0)))
    val retained = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 4.0)),
      "x" -> Column.Doubles(Vector(1.0, 3.0)))
    val model = checked(build("onset ~ hrf(scale(x))", data,
      Some(ResponseSupportRequest(scans, EventSupportPolicy.ExcludeUnobservedAllowPartial))))
    val oracle = checked(build("onset ~ hrf(scale(x))", retained, None))
    assertEquals(model.designMatrix.data.toVector, oracle.designMatrix.data.toVector)

  test("derived-row exclusions and refusals refer to original input rows"):
    import scalafim.fmri.design.formula.{DerivedEventPlan, DerivedMissingRows}
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(0.0, 1.0, 100.0, 4.0)),
      "x" -> Column.Doubles(Vector(Double.NaN, 1.0, 2.0, 3.0)))
    val derived = checked(DerivedEventPlan.parse(Vector("gain: number = x * 2"), DerivedMissingRows.Drop))
    def request(policy: EventSupportPolicy) = checked(EventModelBuilder.EventDesignRequest.fromText(
      "onset ~ hrf(gain)", data, frame, blockPlan = EventModelBuilder.BlockPlan.SingleBlock,
      options = EventModelBuilder.BuildOptions(defaultHrf = Hrfs.fir(nBasis = 1, span = 2.s), precision = 0.25.s,
        responseSupport = Some(ResponseSupportRequest(scans, policy))), derived = derived))
    val model = checked(EventModelBuilder.buildEither(request(EventSupportPolicy.ExcludeUnobservedAllowPartial)))
    assertEquals(model.designSchema.audit.responseSupport.head.decisions.map(_.sourceRow), Vector(1, 2, 3))
    assertEquals(model.designSchema.audit.excludedEvents.map(_.eventIndex), Vector(2))
    assertEquals(model.designSchema.audit.sourceEvents.map(_.sourceRow), Vector(1, 3))
    val convolved = model.terms.head._2.asInstanceOf[ConvolvedTerm]
    assertEquals(checked(convolved.sampledSupport(scans)).contributions.map(_.sourceRow), Vector(Some(1), Some(3)))
    EventModelBuilder.buildEither(request(EventSupportPolicy.RejectUnobservedAllowPartial)) match
      case Left(DesignError.ResponseSupportRejected(receipt)) =>
        assertEquals(receipt.decisions.filter(_.disposition == EventSupportDisposition.Rejected).map(_.sourceRow), Vector(2))
      case other => fail(s"expected source-addressable refusal, got $other")
    val incremental = checked(EventModelBuilder.prepareIncremental(request(EventSupportPolicy.ExcludeUnobservedAllowPartial)))
    val repeated = checked(incremental.updateFormula(incremental.formula))
    assertEquals(repeated.model.designSchema.fingerprint, model.designSchema.fingerprint)
    assertEquals(checked(repeated.model.terms.head._2.asInstanceOf[ConvolvedTerm].sampledSupport(scans)).contributions.map(_.sourceRow),
      Vector(Some(1), Some(3)))

  test("support exclusion precedes observed centering and products with original component rows"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(0.0, 1.0, 4.0, 7.0, 100.0)),
      "condition" -> Column.Strings(Vector.fill(5)("A")),
      "gain" -> Column.Doubles(Vector(999.0, 1.0, 3.0, 5.0, 500.0)),
      "loss" -> Column.Doubles(Vector(999.0, 5.0, 2.0, 7.0, 100.0)),
      "keep" -> Column.Bools(Vector(false, true, true, true, true)))
    val manual = data.filterRows(Vector(false, true, true, true, false))
    val formulas = Vector(
      "onset ~ hrf(condition, modulators(product(center_within_run(gain), center_within_run(loss))), subset = keep, id = task)",
      "onset ~ hrf(condition, modulators(modulator(gain, center = run, scale = raw, missing = zero)), subset = keep, id = task)")
    for formula <- formulas do
      val model = checked(build(formula, data, Some(ResponseSupportRequest(scans, EventSupportPolicy.ExcludeUnobservedAllowPartial))))
      val reference = checked(build(formula, manual, None))
      assertEquals(model.designMatrix.data.toVector, reference.designMatrix.data.toVector)
      assertEquals(model.designSchema.audit.sourceEvents.map(_.sourceRow), Vector(1, 2, 3))
      assertEquals(model.designSchema.audit.excludedEvents.map(_.eventIndex), Vector(4))

  test("post-convolution basis transforms keep coverage policy but refuse stale per-column support"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 4.0)),
      "condition" -> Column.Strings(Vector("A", "A")))
    val formula = "onset ~ hrf(condition, basis = spmg2, orthogonalize_basis = TRUE, id = task)"
    val request = checked(EventModelBuilder.EventDesignRequest.fromText(formula, data, frame,
      blockPlan = EventModelBuilder.BlockPlan.SingleBlock,
      options = EventModelBuilder.BuildOptions(responseSupport = Some(ResponseSupportRequest(scans,
        EventSupportPolicy.RejectUnobservedAllowPartial)))))
    val model = checked(EventModelBuilder.buildEither(request))
    assertEquals(model.designSchema.audit.responseSupport.head.decisions.length, 2)
    assert(model.terms.head._2.asInstanceOf[ConvolvedTerm].sampledSupport(scans).isLeft)
    val incremental = checked(EventModelBuilder.prepareIncremental(request))
    assertEquals(incremental.model.designSchema.fingerprint, model.designSchema.fingerprint)
    assert(incremental.model.terms.head._2.asInstanceOf[ConvolvedTerm].sampledSupport(scans).isLeft)

  test("shared slopes preserve distinct main and slope column contributions under coverage policy"):
    val data = DataTable.fromColumns("onset" -> Column.Doubles(Vector(1.0, 4.0, 100.0)),
      "condition" -> Column.Strings(Vector("A", "B", "B")), "gain" -> Column.Doubles(Vector(1.0, 3.0, 100.0)))
    val formula = "onset ~ hrf(condition, modulators(gain), include_main = TRUE, shared_slopes = TRUE, id = task)"
    val model = checked(build(formula, data, Some(ResponseSupportRequest(scans, EventSupportPolicy.ExcludeUnobservedAllowPartial))))
    val reference = checked(build(formula, data.filterRows(Vector(true, true, false)), None))
    assertEquals(model.designMatrix.data.toVector, reference.designMatrix.data.toVector)
    val term = model.terms.head._2.asInstanceOf[ConvolvedTerm]
    val support = checked(term.sampledSupport(scans)).contributions.filter(_.actual.sampleCount > 0)
    assertEquals(support.map(_.columnIndex).distinct.sorted, term.columnNames.indices.toVector)
    assertEquals(model.designSchema.audit.excludedEvents.map(_.eventIndex), Vector(2))
