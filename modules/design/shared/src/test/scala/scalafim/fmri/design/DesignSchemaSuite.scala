package scalafim.fmri.design

import scalafim.fmri.design.event.{Event, EventModel, EventTerm}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.hrf.{BasisRole, Hrfs, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class DesignSchemaSuite extends munit.FunSuite:

  test("built event models retain structural cell and basis provenance") {
    val sampling = SamplingFrame(blockLens = Seq(16), tr = Seq(1.0))
    val term = EventTerm(
      events = Vector(
        Event.factor(Vector("encode", "recall", "encode", "recall"), "task"),
        Event.factor(Vector("low", "high", "low", "high"), "load")
      ),
      onsets = Vector(1.0, 3.0, 7.0, 9.0).map(Seconds(_)),
      blockIds = Vector.fill(4)(0),
      termTag = Some("task")
    )
    val model = EventModel.build(Vector(term.convolve(Hrfs.SPMG3, sampling)), sampling)
    val schema = model.compiledSchema.getOrElse(fail("EventModel.build must attach a compiled schema"))

    assertEquals(schema.matrix.rows, schema.rows.rows)
    assertEquals(schema.matrix.cols, schema.columns.length)
    assertEquals(schema.columnNames, model.columnNames)
    assertEquals(schema.columnIds.distinct.length, schema.columnIds.length)
    assert(schema.columns.forall(_.origin.isInstanceOf[StructuralColumnOrigin.Event]))
    assert(schema.columns.exists { column =>
      column.origin match
        case StructuralColumnOrigin.Event(_, _, cell, _, Some(basis), _, _) =>
          cell.get(FactorId.unsafe("task")).exists(_.value == "encode") &&
            cell.get(FactorId.unsafe("load")).exists(_.value == "low") &&
            basis.role.contains(BasisRole.Canonical) &&
            basis.elementId.nonEmpty
        case _ => false
    })
    assert(schema.columns.exists {
      case column => column.origin match
        case StructuralColumnOrigin.Event(_, _, _, _, Some(basis), _, _) => basis.role.contains(BasisRole.TemporalDerivative)
        case _ => false
    })
    assert(schema.columns.exists {
      case column => column.origin match
        case StructuralColumnOrigin.Event(_, _, _, _, Some(basis), _, _) => basis.role.contains(BasisRole.DispersionDerivative)
        case _ => false
    })
    assert(schema.audit.eventsSeen == 4)
    assert(schema.audit.eventsUsed == 4)
    assertSchemaValidation(model)
  }

  test("event provenance records the observed run and multi-run terms remain global") {
    val sampling = SamplingFrame(blockLens = Seq(8, 9), tr = Seq(1.0, 1.5))
    val singleRun = EventTerm(
      events = Vector(Event.factor(Vector("A", "A"), "condition")),
      onsets = Vector(1.0, 4.0).map(Seconds(_)),
      blockIds = Vector(0, 0),
      termTag = Some("single")
    )
    val multiRun = EventTerm(
      events = Vector(Event.factor(Vector("A", "A"), "condition")),
      onsets = Vector(1.0, 2.0).map(Seconds(_)),
      blockIds = Vector(0, 1),
      termTag = Some("multi")
    )
    val model = EventModel.build(
      Vector(
        singleRun.convolve(Hrfs.SPMG1, sampling),
        multiRun.convolve(Hrfs.SPMG1, sampling)
      ),
      sampling
    )

    val scopes = model.designSchema.columns.map(_.origin).collect {
      case StructuralColumnOrigin.Event(term, _, _, _, _, _, scope) => term.value -> scope
    }
    assert(scopes.exists { case (term, scope) => term == "single" && scope == RunScope.Run(RunIndex.unsafeOneBased(1)) })
    assert(scopes.exists { case (term, scope) => term == "multi" && scope == RunScope.Global })
  }

  test("combining event and baseline schemas retains event provenance") {
    val rows = RowLayout(
      blockIds = Vector(RunIndex.unsafeOneBased(1), RunIndex.unsafeOneBased(1)),
      acquisitionTimes = Vector(Seconds(0.5), Seconds(1.5)),
      selectedRows = Vector(ScanIndex.unsafeOneBased(1), ScanIndex.unsafeOneBased(2))
    )
    val provenance = EventRowProvenance(
      parent = TrialId.unsafe("trial-1"),
      phase = Some(PhaseId.unsafe("sample")),
      sourceRow = 0,
      blockId = 0,
      onset = Seconds(0.5),
      duration = Seconds(0.0)
    )
    val eventColumn = StructuralColumn.fromOrigin(
      1,
      StructuralColumnOrigin.Event(
        TermId.unsafe("sample"),
        Some(PhaseId.unsafe("sample")),
        CellKey.empty,
        None,
        None,
        ColumnRole.Task,
        RunScope.Global
      ),
      "sample"
    ).toOption.get
    val baselineColumn = StructuralColumn.fromOrigin(
      1,
      StructuralColumnOrigin.Intercept(RunScope.Global),
      "intercept"
    ).toOption.get
    val eventSchema = DesignSchema.validated(
      Mat.unsafe(2, 1, Array(0.0, 1.0)),
      rows,
      Vector(eventColumn),
      DesignAudit(eventsSeen = 1, eventsUsed = 1, eventProvenance = Vector(provenance))
    ).toOption.get
    val baselineSchema = DesignSchema.validated(
      Mat.unsafe(2, 1, Array(1.0, 1.0)),
      rows,
      Vector(baselineColumn)
    ).toOption.get

    val combined = DesignSchema.combine(eventSchema, baselineSchema).toOption.getOrElse(fail("event and baseline schemas should combine"))
    assertEquals(combined.audit.eventProvenance, Vector(provenance))
    assertEquals(combined.audit.eventsSeen, 1)
    assertEquals(combined.audit.eventsUsed, 1)
    assert(combined.fingerprint.canonicalEncoding.contains(provenance.canonical))
  }

  test("fingerprints are deterministic and respond to semantic, row, and matrix changes") {
    val rows = RowLayout(
      blockIds = Vector(RunIndex.unsafeOneBased(1), RunIndex.unsafeOneBased(1)),
      acquisitionTimes = Vector(Seconds(0.5), Seconds(1.5)),
      selectedRows = Vector(ScanIndex.unsafeOneBased(1), ScanIndex.unsafeOneBased(2))
    )
    val origin = StructuralColumnOrigin.Intercept(RunScope.Global)
    val column = StructuralColumn.fromOrigin(1, origin, "intercept").toOption.get
    val matrix = Mat.unsafe(2, 1, Array(1.0, 1.0))
    val first = DesignSchema.validated(matrix, rows, Vector(column)).toOption.get
    val second = DesignSchema.validated(matrix, rows, Vector(column)).toOption.get
    assertEquals(first.fingerprint.value, second.fingerprint.value)
    assertEquals(first.fingerprint.canonicalEncoding, second.fingerprint.canonicalEncoding)

    val changedMatrix = DesignSchema.validated(Mat.unsafe(2, 1, Array(1.0, 2.0)), rows, Vector(column)).toOption.get
    assertNotEquals(first.fingerprint.value, changedMatrix.fingerprint.value)

    val changedRows = DesignSchema.validated(
      matrix,
      rows.copy(acquisitionTimes = Vector(Seconds(0.5), Seconds(2.5))),
      Vector(column)
    ).toOption.get
    assertNotEquals(first.fingerprint.value, changedRows.fingerprint.value)

    val changedOrigin = StructuralColumn.fromOrigin(
      1,
      StructuralColumnOrigin.Intercept(RunScope.PerRun),
      "intercept"
    ).toOption.get
    val changedSemantics = DesignSchema.validated(matrix, rows, Vector(changedOrigin)).toOption.get
    assertNotEquals(first.fingerprint.value, changedSemantics.fingerprint.value)
  }

  test("validated schemas compute scale-aware structural rank previews") {
    val rows = RowLayout(
      blockIds = Vector.fill(5)(RunIndex.unsafeOneBased(1)),
      acquisitionTimes = (0 until 5).toVector.map(index => Seconds(index.toDouble)),
      selectedRows = (1 to 5).toVector.map(ScanIndex.unsafeOneBased)
    )
    val columns = Vector(
      StructuralColumn.fromOrigin(1, StructuralColumnOrigin.Intercept(RunScope.Global), "intercept").toOption.get,
      StructuralColumn.fromOrigin(
        2,
        StructuralColumnOrigin.Nuisance(TermId.unsafe("trend"), ModulatorId.unsafe("trend"), RunScope.Global),
        "trend"
      ).toOption.get,
      StructuralColumn.fromOrigin(
        3,
        StructuralColumnOrigin.Nuisance(TermId.unsafe("duplicate"), ModulatorId.unsafe("duplicate"), RunScope.Global),
        "duplicate"
      ).toOption.get
    )
    val duplicate = DesignSchema.validated(
      Mat.unsafe(
        5,
        3,
        Array(
          1.0, -2.0, -2.0,
          1.0, -1.0, -1.0,
          1.0, 0.0, 0.0,
          1.0, 1.0, 1.0,
          1.0, 2.0, 2.0
        )
      ),
      rows,
      columns
    ).toOption.getOrElse(fail("duplicate structural schema should compile with a deficient preview"))
    val preview = duplicate.rankPreview.evidence.getOrElse(fail("finite duplicate design should have rank evidence"))

    assertEquals(preview.toleranceConvention, RankToleranceConvention.ScaleAware)
    assertEquals(preview.numericalRank, 2)
    assertEquals(preview.pivotOrder.toSet, columns.map(_.id).toSet)
    assertEquals(preview.independentColumns.length, 2)
    assertEquals(preview.aliasedColumns.length, 1)
    assertEquals(duplicate.audit.rankPreview, Some(duplicate.rankPreview))
    assert(preview.conditionEstimate.exists(_ >= 1.0))

    val scaledColumns = columns.take(2)
    val scaled = DesignSchema.validated(
      Mat.unsafe(
        5,
        2,
        Array(
          1.0e-12, -2.0e-12,
          1.0e-12, -1.0e-12,
          1.0e-12, 0.0,
          1.0e-12, 1.0e-12,
          1.0e-12, 2.0e-12
        )
      ),
      rows,
      scaledColumns
    ).toOption.getOrElse(fail("uniformly scaled full-rank schema should compile"))

    val scaledPreview = scaled.rankPreview.evidence.getOrElse(fail("finite scaled design should have rank evidence"))
    assertEquals(scaledPreview.numericalRank, 2)
    assert(!scaledPreview.deficient)
    assert(scaledPreview.tolerance < 1.0e-20)
  }

  test("schema validation returns typed errors for shape and identity violations") {
    val rows = RowLayout(
      blockIds = Vector(RunIndex.unsafeOneBased(1)),
      acquisitionTimes = Vector(Seconds(0.5)),
      selectedRows = Vector(ScanIndex.unsafeOneBased(1))
    )
    val origin = StructuralColumnOrigin.Intercept(RunScope.Global)
    val column = StructuralColumn.fromOrigin(1, origin, "intercept").toOption.get
    val result = DesignSchema.validated(Mat.unsafe(2, 1, Array(1.0, 1.0)), rows, Vector(column))
    assert(result.left.toOption.exists(_.isInstanceOf[DesignError.InvalidSchema]))
  }

  test("coefficient axes select structural columns without changing design identity") {
    val rows = RowLayout(
      blockIds = Vector(RunIndex.unsafeOneBased(1), RunIndex.unsafeOneBased(1)),
      acquisitionTimes = Vector(Seconds(0.5), Seconds(1.5)),
      selectedRows = Vector(ScanIndex.unsafeOneBased(1), ScanIndex.unsafeOneBased(2))
    )
    val columns = Vector(
      StructuralColumn.fromOrigin(1, StructuralColumnOrigin.Event(TermId.unsafe("task"), None, CellKey.empty, None, None, ColumnRole.Task, RunScope.Global), "task").toOption.get,
      StructuralColumn.fromOrigin(2, StructuralColumnOrigin.Intercept(RunScope.Global), "intercept").toOption.get
    )
    val schema = DesignSchema.validated(Mat.unsafe(2, 2, Array(1.0, 1.0, 2.0, 1.0)), rows, columns).toOption.get
    val axis = schema.coefficientAxis
    val selected = axis.select(Vector(1)).toOption.get

    assertEquals(selected.designFingerprint, axis.designFingerprint)
    assertEquals(selected.columnIds, Vector(columns(1).id))
    assertEquals(selected.columnNames, Vector("intercept"))
    assert(selected.select(Vector(1)).isLeft)
    assert(selected.select(Vector(0, 0)).isLeft)
  }

  test("runwise coefficient axes lower global families to an inspectable run identity") {
    val rows = RowLayout(
      blockIds = Vector(RunIndex.unsafeOneBased(1), RunIndex.unsafeOneBased(2)),
      acquisitionTimes = Vector(Seconds(0.5), Seconds(1.5)),
      selectedRows = Vector(ScanIndex.unsafeOneBased(1), ScanIndex.unsafeOneBased(2))
    )
    val columns = Vector(
      StructuralColumn.fromOrigin(
        1,
        StructuralColumnOrigin.Event(TermId.unsafe("task"), None, CellKey.empty, None, None, ColumnRole.Task, RunScope.Global),
        "task"
      ).toOption.get,
      StructuralColumn.fromOrigin(2, StructuralColumnOrigin.Intercept(RunScope.Global), "intercept").toOption.get,
      StructuralColumn.fromOrigin(3, StructuralColumnOrigin.Intercept(RunScope.Run(RunIndex.unsafeOneBased(1))), "run1").toOption.get
    )
    val schema = DesignSchema.validated(
      Mat.unsafe(2, 3, Array(1.0, 1.0, 1.0, 2.0, 1.0, 0.0)),
      rows,
      columns
    ).toOption.get

    val run2 = schema.coefficientAxis.forRunwiseCoefficient(RunIndex.unsafeOneBased(2)).toOption.get
    assertEquals(run2.designFingerprint, schema.fingerprint)
    assertEquals(run2.columns(0).origin match
      case StructuralColumnOrigin.Event(_, _, _, _, _, _, scope) => scope
      case other => fail(s"expected event origin, got $other"), RunScope.Run(RunIndex.unsafeOneBased(2)))
    assertEquals(run2.columns(1).origin, StructuralColumnOrigin.Intercept(RunScope.Run(RunIndex.unsafeOneBased(2))))
    assertEquals(
      run2.columns(2).origin,
      StructuralColumnOrigin.Intercept(RunScope.Run(RunIndex.unsafeOneBased(1)))
    )
    assert(!run2.structurallyCompatible(schema.coefficientAxis))
    assertEquals(
      schema.runwiseSlice(RunIndex.unsafeOneBased(1)).toOption.map(_.sourceRowIndices),
      Some(Vector(0))
    )
    assertEquals(
      schema.runwiseSlice(RunIndex.unsafeOneBased(2)).toOption.map(_.sourceRowIndices),
      Some(Vector(1))
    )
  }

  test("legacy axes do not claim a runwise structural identity") {
    val sampling = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))
    val legacy = EventModel(
      terms = Vector.empty,
      samplingFrame = sampling,
      designMatrix = Mat.unsafe(4, 1, Array(1.0, 2.0, 3.0, 4.0)),
      columnNames = Vector("task"),
      termSpans = Vector(0 -> 1),
      colIndices = Map("task" -> Vector(0))
    )
    assert(legacy.designSchema.coefficientAxis.forRunwiseCoefficient(RunIndex.unsafeOneBased(1)).isLeft)
  }

  test("runwise slices retain an explicit censored-row selection") {
    val run = RunIndex.unsafeOneBased(1)
    val rows = RowLayout(
      blockIds = Vector.fill(5)(run),
      acquisitionTimes = Vector(0, 1, 2, 3, 4).map(Seconds(_)),
      selectedRows = (1 to 5).toVector.map(ScanIndex.unsafeOneBased)
    )
    val column = StructuralColumn.fromOrigin(
      1,
      StructuralColumnOrigin.Event(
        TermId.unsafe("task"),
        None,
        CellKey.empty,
        None,
        None,
        ColumnRole.Task,
        RunScope.Global
      ),
      "task"
    ).toOption.get
    val schema = DesignSchema.validated(
      Mat.unsafe(5, 1, Array(10.0, 20.0, 30.0, 40.0, 50.0)),
      rows,
      Vector(column)
    ).toOption.get

    val slice = schema.runwiseSlice(
      run,
      Vector(ScanIndex.unsafeOneBased(1), ScanIndex.unsafeOneBased(3), ScanIndex.unsafeOneBased(5))
    ).toOption.getOrElse(fail("explicit runwise row selection should compile"))

    assertEquals(slice.sourceRowIndices, Vector(0, 2, 4))
    assertEquals(slice.matrix.data.toVector, Vector(10.0, 30.0, 50.0))
    assertEquals(slice.axis.columns.head.origin match
      case StructuralColumnOrigin.Event(_, _, _, _, _, _, scope) => scope
      case other => fail(s"expected event origin, got $other"), RunScope.Run(run))
    assert(schema.runwiseSlice(run, Vector(ScanIndex.unsafeOneBased(2), ScanIndex.unsafeOneBased(2))).isLeft)
  }

  test("runwise projections exclude zero-support nuisance and empty shared columns") {
    val runOne = RunIndex.unsafeOneBased(1)
    val runTwo = RunIndex.unsafeOneBased(2)
    val rows = RowLayout(
      blockIds = Vector(runOne, runOne, runTwo, runTwo),
      acquisitionTimes = Vector(0, 1, 0, 1).map(Seconds(_)),
      selectedRows = (1 to 4).toVector.map(ScanIndex.unsafeOneBased)
    )
    val columns = Vector(
      StructuralColumn.fromOrigin(
        1,
        StructuralColumnOrigin.Event(
          TermId.unsafe("task"), None, CellKey.empty, None, None, ColumnRole.Task, RunScope.Global
        ),
        "task"
      ).toOption.get,
      StructuralColumn.fromOrigin(2, StructuralColumnOrigin.Intercept(RunScope.Global), "global").toOption.get,
      StructuralColumn.fromOrigin(3, StructuralColumnOrigin.Intercept(RunScope.Run(runOne)), "run1").toOption.get,
      StructuralColumn.fromOrigin(4, StructuralColumnOrigin.Intercept(RunScope.Run(runTwo)), "run2").toOption.get,
      StructuralColumn.fromOrigin(5, StructuralColumnOrigin.Event(
        TermId.unsafe("empty"), None, CellKey.empty, None, None, ColumnRole.Task, RunScope.Global
      ), "empty").toOption.get
    )
    val matrix = Mat.unsafe(
      4,
      5,
      Array(
        1.0, 1.0, 1.0, 0.0, 0.0,
        2.0, 1.0, 1.0, 0.0, 0.0,
        1.0, 1.0, 0.0, 1.0, 0.0,
        2.0, 1.0, 0.0, 1.0, 0.0
      )
    )
    val schema = DesignSchema.validated(matrix, rows, columns).toOption.get

    val first = schema.runwiseProjection(runOne).toOption.getOrElse(fail("run one projection should compile"))
    val second = schema.runwiseProjection(runTwo).toOption.getOrElse(fail("run two projection should compile"))

    assertEquals(first.sourceColumnIndices, Vector(0, 1, 2))
    assertEquals(second.sourceColumnIndices, Vector(0, 1, 3))
    assertEquals(first.axis.predictors, 3)
    assertEquals(second.axis.predictors, 3)
    assert(first.decisions(0).disposition == RunColumnDisposition.SharedEstimand)
    assert(first.decisions(1).disposition == RunColumnDisposition.SharedEstimand)
    assert(first.decisions(2).disposition == RunColumnDisposition.RunLocalNuisance)
    assert(first.decisions(3).disposition.isInstanceOf[RunColumnDisposition.Omitted])
    assert(first.decisions(4).disposition.isInstanceOf[RunColumnDisposition.NonEstimable])
    assert(second.decisions(3).disposition == RunColumnDisposition.RunLocalNuisance)
    assert(second.nonEstimable.map(_.sourceColumnIndex) == Vector(4))

    val slice = schema.runwiseSlice(runOne).toOption.getOrElse(fail("run one slice should compile"))
    assertEquals(slice.sourceColumnIndices, Vector(0, 1, 2))
    assertEquals(slice.matrix.cols, 3)
  }

  test("built baseline models retain run-scoped structural origins") {
    val sampling = SamplingFrame(blockLens = Seq(6, 7), tr = Seq(1.0), startTime = Seq(0.5, 0.5))
    val model = BaselineModel.build(
      samplingFrame = sampling,
      basis = BaselineBasis.Bs,
      degree = 4,
      intercept = Intercept.Runwise
    )
    val schema = model.compiledSchema.getOrElse(fail("BaselineModel.build must attach a compiled schema"))
    assertEquals(schema.matrix.cols, model.designMatrix.cols)
    assertEquals(schema.rows.blockIds.map(_.oneBased).distinct, Vector(1, 2))
    assert(schema.columns.exists(_.origin.isInstanceOf[StructuralColumnOrigin.Drift]))
    assert(schema.columns.exists {
      case column => column.origin match
        case StructuralColumnOrigin.Intercept(RunScope.Run(index)) => index.oneBased == 1
        case _                                                     => false
    })
    assertEquals(model.designSchemaValidation, Right(()))
  }

  private def assertSchemaValidation(model: EventModel): Unit =
    assertEquals(model.designSchemaValidation, Right(()))
