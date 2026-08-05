package scalafim.fmri.model

import scalafim.dataset.{DatasetEventRow, DatasetEvents, DatasetFieldId, DatasetId, DatasetValue, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineError, Intercept, NuisanceCheck}
import scalafim.fmri.design.{CellAssignment, CellKey, ColumnId, DegenerateModulatorPolicy, DesignError, EmptyCellDisposition, EmptyCellPolicy, FactorId, FactorLevelRegistry, FactorSchemaBinding, HrfAssignment, HrfByCell, HrfByPhase, ModulatorId, ModulatorOrthogonalization, ModulatorOrthogonalizationPlan, OrthogonalizationOutcome, TermId}
import scalafim.fmri.design.contrast.LevelId
import scalafim.fmri.design.data.Column
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.hrf.Hrfs
import scalafim.image.{DMat, NeuroSpace}

class ModelBuilderSuite extends munit.FunSuite:

  private def col(name: String): ColumnId =
    ColumnId(name).fold(error => fail(error.message), identity)

  private def samplingFrame: SamplingFrame =
    SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))

  private def dataset(events: DatasetEvents): FmriDataset =
    val data = DMat.fromRows(
      Vector(
        Vector(1.0),
        Vector(3.0),
        Vector(5.0),
        Vector(7.0)
      )
    )
    FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(DatasetId("builder-demo"), data, NeuroSpace(Vector(1, 1, 1))),
      samplingFrame = samplingFrame,
      events = events
    ).dataset

  test("eventsTable infers typed columns from dataset event rows") {
    val table = FmriModelBuilder.eventsTable(
      DatasetEvents(
        Vector(
          Map("onset" -> "0.0", "run" -> "1", "condition" -> "face", "keep" -> "true"),
          Map("onset" -> "1.5", "run" -> "1", "condition" -> "house", "keep" -> "false")
        )
      )
    )

    assertEquals(table.nrows, 2)
    assertEquals(table.column(col("run")), Right(Column.Ints(Vector(1, 1))))
    assertEquals(table.get[Double](col("onset")), Right(Vector(0.0, 1.5)))
    assertEquals(table.get[String](col("condition")), Right(Vector("face", "house")))
    assertEquals(table.get[Boolean](col("keep")), Right(Vector(true, false)))
  }

  test("eventsTable preserves explicitly typed text values") {
    val rows =
      Vector(
        DatasetEventRow.unsafe(
          Map(
            DatasetFieldId("onset") -> DatasetValue.Number(0.0),
            DatasetFieldId("code") -> DatasetValue.Text("1")
          )
        ),
        DatasetEventRow.unsafe(
          Map(
            DatasetFieldId("onset") -> DatasetValue.Number(1.0),
            DatasetFieldId("code") -> DatasetValue.Text("2")
          )
        )
      )
    val events = DatasetEvents.fromTypedRows(rows).fold(error => fail(error.message), identity)
    val table = FmriModelBuilder.eventsTable(events)

    assertEquals(table.get[Double](col("onset")), Right(Vector(0.0, 1.0)))
    assertEquals(table.column(col("code")), Right(Column.Strings(Vector("1", "2"))))
  }

  test("buildModel constructs an inspectable model from dataset-resident events") {
    val events = DatasetEvents(
      Vector.tabulate(4)(i => Map("onset" -> i.toString, "task" -> i.toString))
    )

    val model = FmriModelBuilder.buildModel(
      dataset(events),
      ModelBuildSpec("onset ~ covariate(task)", baselineIntercept = Intercept.Global)
    )

    assertEquals(model.nTimepoints, 4)
    assertEquals(model.columnNames, Vector("task", "base_constant"))
    assertEquals(model.eventModel.designMatrix.rows, 4)
    assertEquals(model.eventModel.designMatrix.cols, 1)
    assertEquals(model.eventModel.designMatrix.col(0).data.toVector, Vector(0.0, 1.0, 2.0, 3.0))
  }

  test("buildModel propagates declared factor levels through the public model path") {
    val events = DatasetEvents(
      Vector(
        Map("onset" -> "0.0", "condition" -> "2"),
        Map("onset" -> "1.0", "condition" -> "1"),
        Map("onset" -> "2.0", "condition" -> "2"),
        Map("onset" -> "3.0", "condition" -> "1")
      )
    )
    val levels = FactorLevelRegistry.of("condition" -> Seq("1", "2", "3")).toOption.get
    val schemaBinding = FactorSchemaBinding.perSubject("subject-1" -> levels).toOption.get

    val model = FmriModelBuilder.buildModel(
      dataset(events),
      ModelBuildSpec(
        formula = "onset ~ hrf(condition)",
        factorLevels = levels,
        emptyCellPolicy = EmptyCellPolicy.RetainZero,
        factorSchemaBinding = Some(schemaBinding)
      )
    )

    val event = model.eventModel
    assertEquals(event.designMatrix.cols, 3)
    assertEquals(event.columnNames, Vector("condition_condition.1", "condition_condition.2", "condition_condition.3"))
    val audit = event.designSchema.audit
    assertEquals(audit.factorLevels.map(_.factor.value), Vector("condition"))
    assertEquals(audit.factorLevels.head.declared.map(_.map(_.value)), Some(Vector("1", "2", "3")))
    assertEquals(audit.factorLevels.head.observed.map(_.value), Vector("1", "2"))
    val emptyCell = audit.emptyCells.find(_.canonical == "cell:{condition=3}")
    assert(emptyCell.nonEmpty)
    val emptyAudit = audit.emptyCellAudits.find(_.cell.canonical == "cell:{condition=3}").get
    assertEquals(emptyAudit.disposition, EmptyCellDisposition.RetainedZero)
    assert(event.policyReceipts.exists(r => r.name == "empty-cells" && r.detail.contains("retain-zero")))
    assert(event.policyReceipts.exists(r => r.name == "factor-schema-binding" && r.detail.contains("subject-1")))
  }

  test("buildModel carries heterogeneous HRF assignments through the public model path") {
    val events = DatasetEvents(
      Vector(
        Map("onset" -> "0.0", "condition" -> "A"),
        Map("onset" -> "1.0", "condition" -> "B"),
        Map("onset" -> "2.0", "condition" -> "A"),
        Map("onset" -> "3.0", "condition" -> "B")
      )
    )
    val levels = FactorLevelRegistry.of("condition" -> Seq("A", "B")).toOption.get
    val condition = FactorId.unsafe("condition")
    def cell(level: String): CellKey =
      CellKey.unsafe(Seq(CellAssignment(condition, LevelId.unsafe(level))))
    val assignments = HrfByCell.of(
      cell("A") -> scalafim.fmri.hrf.Hrfs.SPMG1,
      cell("B") -> scalafim.fmri.hrf.Hrfs.SPMG2
    ).toOption.get

    val model = FmriModelBuilder.buildModel(
      dataset(events),
      ModelBuildSpec(
        formula = "onset ~ hrf(condition, id=\"task\")",
        factorLevels = levels,
        emptyCellPolicy = EmptyCellPolicy.RetainZero,
        hrfByCell = Some(assignments)
      )
    )

    assertEquals(model.eventModel.designMatrix.cols, 3)
    assertEquals(model.eventModel.designSchema.columns.count(_.origin match
      case scalafim.fmri.design.StructuralColumnOrigin.Event(_, _, _, _, Some(basis), _, _) => basis.index.oneBased == 1
      case _ => false
    ), 2)
    assert(model.eventModel.policyReceipts.exists(receipt => receipt.name == "hrf-by-cell"))
  }

  test("buildModel carries exhaustive phase and cell HRF assignments through the public model path") {
    val events = DatasetEvents(
      Vector(
        Map("sample_onset" -> "0.0", "probe_onset" -> "1.0", "condition" -> "A", "trial" -> "trial-1"),
        Map("sample_onset" -> "1.0", "probe_onset" -> "2.0", "condition" -> "B", "trial" -> "trial-2"),
        Map("sample_onset" -> "2.0", "probe_onset" -> "3.0", "condition" -> "A", "trial" -> "trial-3"),
        Map("sample_onset" -> "3.0", "probe_onset" -> "3.0", "condition" -> "B", "trial" -> "trial-4")
      )
    )
    val levels = FactorLevelRegistry.of("condition" -> Seq("A", "B"))
      .fold(error => fail(error.message), identity)
    val probe = HrfByCell.oneFactor(
      "condition",
      "A" -> Hrfs.SPMG1,
      "B" -> Hrfs.SPMG2
    ).fold(error => fail(error.message), identity)
    val assignments = HrfByPhase.named(
      "sample" -> HrfAssignment.Shared(Hrfs.SPMG1),
      "probe" -> HrfAssignment.ByCell(probe)
    ).fold(error => fail(error.message), identity)

    val model = FmriModelBuilder.buildModelEither(
      dataset(events),
      ModelBuildSpec(
        formula =
          "sample_onset ~ " +
            "hrf(condition, onsets = sample_onset, phase = sample, parent = trial, id = sample) + " +
            "hrf(condition, onsets = probe_onset, phase = probe, parent = trial, id = probe)",
        baselineIntercept = Intercept.None,
        factorLevels = levels,
        hrfByPhase = Some(assignments)
      )
    ).fold(error => fail(error.message), identity)

    val phaseWidths = model.eventModel.designSchema.columns.flatMap(_.origin match
      case event: scalafim.fmri.design.StructuralColumnOrigin.Event => event.phase.map(_.value)
      case _                                                        => None
    ).groupBy(identity).view.mapValues(_.length).toMap
    assertEquals(phaseWidths, Map("sample" -> 2, "probe" -> 3))
    assert(model.eventModel.policyReceipts.exists(_.name == "hrf-by-phase"))
  }

  test("eventsTable rejects ragged event rows before model construction") {
    val err = intercept[IllegalArgumentException] {
      FmriModelBuilder.eventsTable(
        DatasetEvents(
          Vector(
            Map("onset" -> "0.0", "task" -> "1.0"),
            Map("onset" -> "1.0")
          )
        )
      )
    }

    assert(err.getMessage.contains("dataset event row 1"))
  }

  test("buildModel rejects missing block and duration columns explicitly") {
    val events = DatasetEvents(
      Vector.tabulate(4)(i => Map("onset" -> i.toString, "task" -> i.toString))
    )

    intercept[IllegalArgumentException] {
      FmriModelBuilder.buildModel(
        dataset(events),
        ModelBuildSpec("onset ~ covariate(task)", blockColumn = Some("run"))
      )
    }

    intercept[IllegalArgumentException] {
      FmriModelBuilder.buildModel(
        dataset(events),
        ModelBuildSpec("onset ~ covariate(task)", durationColumn = Some("duration"))
      )
    }
  }

  test("buildModelEither preserves structured design failures") {
    val events = DatasetEvents(
      Vector.tabulate(4)(i => Map("onset" -> i.toString, "condition" -> (if i % 2 == 0 then "A" else "B")))
    )

    FmriModelBuilder.buildModelEither(
      dataset(events),
      ModelBuildSpec("onset ~ hrf(condition, phase = probe, parent = trial_id)")
    ) match
      case Left(ModelError.DesignFailure(DesignError.MissingColumn(column))) =>
        assertEquals(column, "trial_id")
      case other =>
        fail(s"expected a structured missing parent column, found $other")
  }

  test("public model specs retain ordered modulator policy and typed degeneracy failures") {
    val events = DatasetEvents(
      Vector(
        Map("onset" -> "0.0", "x" -> "1.0", "y" -> "2.0"),
        Map("onset" -> "1.0", "x" -> "2.0", "y" -> "4.0"),
        Map("onset" -> "2.0", "x" -> "3.0", "y" -> "6.0"),
        Map("onset" -> "3.0", "x" -> "4.0", "y" -> "8.0")
      )
    )
    val retained = ModulatorOrthogonalization
      .ordered("modulators", "x", "y")
      .fold(error => fail(error.message), identity)
    val retainedSpec = ModelBuildSpec(
      formula = "onset ~ hrf(modulators(x, y), id = modulators)",
      baselineIntercept = Intercept.None,
      orthogonalization = ModulatorOrthogonalizationPlan.one(retained)
    )
    val model = FmriModelBuilder.buildModelEither(dataset(events), retainedSpec)
      .fold(error => fail(error.message), identity)
    assertEquals(
      model.designSchema.getOrElse(fail("model should retain its design schema")).audit.orthogonalizationReceipts.head.steps.head.groups.head.outcome,
      OrthogonalizationOutcome.DegenerateRetained
    )

    val rejected = ModulatorOrthogonalization.ordered(
      term = TermId.unsafe("modulators"),
      order = Vector(ModulatorId.unsafe("x"), ModulatorId.unsafe("y")),
      degenerate = DegenerateModulatorPolicy.Reject
    ).fold(error => fail(error.message), identity)
    FmriModelBuilder.buildModelEither(
      dataset(events),
      retainedSpec.copy(orthogonalization = ModulatorOrthogonalizationPlan.one(rejected))
    ) match
      case Left(ModelError.DesignFailure(DesignError.DegenerateModulator(term, modulator, _, DegenerateModulatorPolicy.Reject))) =>
        assertEquals(term, "modulators")
        assertEquals(modulator, ModulatorId.unsafe("y"))
      case other => fail(s"expected a typed model-level degeneracy failure, found $other")
  }

  test("buildModelEither preserves structured baseline failures") {
    val events = DatasetEvents(
      Vector.tabulate(4)(i => Map("onset" -> i.toString, "task" -> i.toString))
    )

    FmriModelBuilder.buildModelEither(
      dataset(events),
      ModelBuildSpec(
        formula = "onset ~ covariate(task)",
        baselineBasis = BaselineBasis.Bs,
        baselineDegree = 1
      )
    ) match
      case Left(ModelError.BaselineFailure(BaselineError.InvalidInput(detail))) =>
        assert(detail.contains("degree >= 3"))
      case other =>
        fail(s"expected a structured baseline failure, found $other")
  }

  test("buildModel attaches nuisance regressors and exposes diagnostics") {
    val events = DatasetEvents(
      Vector.tabulate(4)(i => Map("onset" -> i.toString, "task" -> i.toString))
    )
    val nuisance = Mat.fromRows(
      Vector(
        Vector(0.0, 0.0, 1.0),
        Vector(1.0, 1.0, 1.0),
        Vector(2.0, 2.0, 1.0),
        Vector(3.0, 3.0, 1.0)
      )
    )

    val model = FmriModelBuilder.buildModel(
      dataset(events),
      ModelBuildSpec(
        "onset ~ covariate(task)",
        baselineIntercept = Intercept.Global,
        nuisance = Some(
          NuisanceRegressors(
            matrices = Vector(nuisance),
            names = Some(Vector(Vector("motion_x", "motion_x_dup", "constant_conf"))),
            check = NuisanceCheck.Drop
          )
        )
      )
    )

    val report = model.baselineModel.nuisanceReport.get
    assertEquals(report.retainedByBlock, Vector(Vector("motion_x")))
    assertEquals(report.droppedByBlock, Vector(Vector("motion_x_dup", "constant_conf")))
    assertEquals(model.baselineModel.termKeys, Vector("drift", "nuisance"))
    assertEquals(model.baselineModel.colIndices("nuisance").length, 1)
    assert(model.designSchema.toVector.flatMap(_.columns).exists {
      _.origin match
        case scalafim.fmri.design.StructuralColumnOrigin.Nuisance(_, regressor, _) =>
          regressor.value == "motion_x"
        case _ => false
    })
  }

  test("sampled nuisance columns validate shape and retain semantic identities") {
    val run = SampledRegressorRun
      .fromColumns(
        "motion_x" -> Vector(0.0, 1.0, 2.0, 3.0),
        "a_comp_cor_00" -> Vector(0.1, -0.1, 0.1, -0.1)
      )
      .fold(error => fail(error.message), identity)
    val nuisance = NuisanceRegressors
      .fromRuns(Vector(run), check = NuisanceCheck.Drop)
      .fold(error => fail(error.message), identity)

    assertEquals(nuisance.matrices.map(_.rows), Vector(4))
    assertEquals(nuisance.names, Some(Vector(Vector("motion_x", "a_comp_cor_00"))))
    assert(SampledRegressorRun.fromColumns(
      "motion_x" -> Vector(0.0, 1.0),
      "a_comp_cor_00" -> Vector(0.1)
    ).left.exists(_.message.contains("has 1 scans; expected 2")))

    val misaligned = FmriModelBuilder.buildModelEither(
      dataset(DatasetEvents(Vector.tabulate(4)(i => Map("onset" -> i.toString, "task" -> i.toString)))),
      ModelBuildSpec(
        formula = "onset ~ covariate(task)",
        nuisance = Some(
          NuisanceRegressors.fromRuns(
            Vector(
              SampledRegressorRun
                .fromColumns("motion_x" -> Vector(0.0, 1.0, 2.0))
                .fold(error => fail(error.message), identity)
            )
          ).fold(error => fail(error.message), identity)
        )
      )
    )
    assert(misaligned.left.exists(_.message.contains("row mismatch for block 0")))
  }
