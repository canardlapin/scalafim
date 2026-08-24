package scalafim.fmri.design

import scalafim.fmri.design.contrast.*
import scalafim.fmri.design.contrast.LevelId
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.{ContinuousEvent, ConvolvedTerm, EventModel, EventModelDiagnosticKind}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.Hrfs
import scalafim.fmri.hrf.design.SamplingFrame

class EventModelBuilderParitySuite extends munit.FunSuite:

  private enum TrialCondition:
    case Match
    case Mismatch
    case Catch

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

  test("phase and parent arguments retain source provenance in one formula") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 0.0, 0.0)),
      "phase_onset" -> Column.Doubles(Vector(1.25, 5.5, 10.25)),
      "phase_dur" -> Column.Doubles(Vector(0.5, 0.5, 0.5)),
      "cond" -> Column.Strings(Vector("A", "B", "A")),
      "trial_id" -> Column.Strings(Vector("trial-1", "trial-2", "trial-3"))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(cond, onsets = phase_onset, durations = phase_dur, phase = probe, parent = trial_id, id = probe)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0)
    )

    val term = convolved(model).term
    assertEquals(term.phaseId, Some(PhaseId.unsafe("probe")))
    assertEquals(term.eventProvenance.map(_.parent.value), Vector("trial-1", "trial-2", "trial-3"))
    assertEquals(term.eventProvenance.map(_.phase), Vector.fill(3)(Some(PhaseId.unsafe("probe"))))
    assertEquals(term.eventProvenance.map(_.sourceRow), Vector(0, 1, 2))
    assert(model.designSchema.columns.forall {
      case StructuralColumn(_, _, StructuralColumnOrigin.Event(_, Some(phase), _, _, _, _, _), _, _, _) => phase == PhaseId.unsafe("probe")
      case _ => false
    })
  }

  test("phase provenance retains structured parent-column errors") {
    val sf = SamplingFrame(blockLens = Seq(10), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0)),
      "cond" -> Column.Strings(Vector("A"))
    )

    val result = EventModelBuilder.buildEither(
      formula = "onset ~ hrf(cond, phase = probe, parent = trial_id)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0)
    )

    assertEquals(result, Left(DesignError.MissingColumn("trial_id")))
  }

  test("phase policy receipts retain the exact source trial identity") {
    val sf = SamplingFrame(blockLens = Seq(24), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 0.0, 0.0)),
      "probe_onset" -> Column.Doubles(Vector(2.0, 7.0, 12.0)),
      "probe_duration" -> Column.Doubles(Vector(0.25, 0.25, 0.25)),
      "condition" -> Column.Strings(Vector("match", "mismatch", "match")),
      "rt" -> Column.Doubles(Vector(0.5, Double.NaN, 0.7)),
      "trial_id" -> Column.Strings(Vector("trial-1", "trial-2", "trial-3"))
    )

    val model = EventModelBuilder.build(
      formula =
        "onset ~ hrf(condition, center_within(rt, condition), onsets = probe_onset, durations = probe_duration, phase = probe, parent = trial_id, id = probe_rt)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0),
      missingValuePolicy = MissingValuePolicy.ZeroContribution
    )

    assertEquals(model.designSchema.audit.missingValues.length, 1)
    val resolution = model.designSchema.audit.missingValues.head
    assertEquals(resolution.eventIndex, 1)
    assertEquals(resolution.source.map(_.parent), Some(TrialId.unsafe("trial-2")))
    assertEquals(resolution.source.flatMap(_.phase), Some(PhaseId.unsafe("probe")))
    assertEquals(resolution.source.map(_.sourceRow), Some(1))
    assertEquals(resolution.source.map(_.blockId), Some(0))
    assertEquals(resolution.source.map(_.onset.value), Some(7.0))
  }

  test("multiphase formula lowering is permutation stable and permits overlapping responses") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val trialIds = Vector("trial-1", "trial-2", "trial-3", "trial-4")
    val sampleOnsets = Vector(1.0, 6.0, 11.0, 16.0)
    val probeOnsets = Vector(2.0, 7.0, 12.0, 17.0)
    val conditions = Vector("A", "B", "A", "B")
    val rt = Vector(0.5, 0.8, 0.6, 0.9)

    def table(order: Vector[Int]): DataTable =
      DataTable.fromColumns(
        "onset" -> Column.Doubles(order.map(_ => 0.0)),
        "sample_onset" -> Column.Doubles(order.map(sampleOnsets)),
        "sample_duration" -> Column.Doubles(order.map(_ => 0.5)),
        "probe_onset" -> Column.Doubles(order.map(probeOnsets)),
        "probe_duration" -> Column.Doubles(order.map(_ => 0.25)),
        "condition" -> Column.Strings(order.map(conditions)),
        "rt" -> Column.Doubles(order.map(rt)),
        "trial_id" -> Column.Strings(order.map(trialIds))
      )

    val formula =
      """onset ~
        |  hrf(condition, onsets = sample_onset, durations = sample_duration, basis = spmg1, phase = sample, parent = trial_id, id = sample) +
        |  hrf(condition, center_within(rt, condition), onsets = probe_onset, durations = probe_duration, basis = spmg2, phase = probe, parent = trial_id, id = probe)""".stripMargin

    def build(order: Vector[Int]): EventModel =
      EventModelBuilder.build(
        formula = formula,
        data = table(order),
        samplingFrame = sf,
        blockIds = Vector.fill(order.length)(0),
        precision = scalafim.fmri.hrf.Seconds(0.2)
      )

    val reference = build(Vector(0, 1, 2, 3))
    val permuted = build(Vector(2, 0, 3, 1))
    assertEquals(reference.designSchema.columns.map(_.origin), permuted.designSchema.columns.map(_.origin))
    reference.designMatrix.data.zip(permuted.designMatrix.data).foreach { case (left, right) =>
      assertEqualsDouble(left, right, 1e-12)
    }
    assertEquals(convolved(reference, 0).hrf.nbasis, 1)
    assertEquals(convolved(reference, 1).hrf.nbasis, 2)
    assertEquals(reference.centeringReceipts.map(_.modulator.value), Vector("rt"))
    assert((0 until reference.designMatrix.rows).exists { row =>
      (0 until 2).exists(column => math.abs(reference.designMatrix(row, column)) > 1e-8) &&
      (2 until reference.designMatrix.cols).exists(column => math.abs(reference.designMatrix(row, column)) > 1e-8)
    })

    val alignedParents = permuted.designSchema.audit.eventProvenance
      .map(value => (value.phase.map(_.value), value.parent.value, value.onset.value, value.duration.value))
      .sortBy(value => (value._1, value._2))
    val referenceParents = reference.designSchema.audit.eventProvenance
      .map(value => (value.phase.map(_.value), value.parent.value, value.onset.value, value.duration.value))
      .sortBy(value => (value._1, value._2))
    assertEquals(alignedParents, referenceParents)
  }

  test("phase construction rejects duplicate parents and strict invalid timing") {
    val sf = SamplingFrame(blockLens = Seq(10), tr = Seq(1.0), startTime = Seq(0.0))
    val duplicates = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 0.0)),
      "phase_onset" -> Column.Doubles(Vector(1.0, 4.0)),
      "duration" -> Column.Doubles(Vector(0.25, 0.25)),
      "condition" -> Column.Strings(Vector("A", "B")),
      "trial_id" -> Column.Strings(Vector("trial-1", "trial-1"))
    )
    val duplicateResult = EventModelBuilder.buildEither(
      formula = "onset ~ hrf(condition, onsets = phase_onset, durations = duration, phase = probe, parent = trial_id)",
      data = duplicates,
      samplingFrame = sf,
      blockIds = Vector(0, 0)
    )
    assert(duplicateResult.left.exists(_.message.contains("parent trial ids must be unique")))

    val invalid = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(0.0, 0.0)),
      "phase_onset" -> Column.Doubles(Vector(-0.5, 12.0)),
      "duration" -> Column.Doubles(Vector(0.25, 0.25)),
      "condition" -> Column.Strings(Vector("A", "B")),
      "trial_id" -> Column.Strings(Vector("trial-1", "trial-2"))
    )
    val timingResult = EventModelBuilder.buildEither(
      formula = "onset ~ hrf(condition, onsets = phase_onset, durations = duration, phase = probe, parent = trial_id)",
      data = invalid,
      samplingFrame = sf,
      blockIds = Vector(0, 2),
      strict = true
    )
    assert(timingResult.left.exists {
      case DesignError.InvalidSchedule(message) =>
        message.contains("negative onset") && message.contains("references block index 2")
      case _ => false
    })
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

  test("structural HRF-by-cell assignments lower heterogeneous basis widths") {
    val sf = SamplingFrame(blockLens = Seq(40), tr = Seq(1.0), startTime = Seq(0.0))
    val registry = FactorLevelRegistry.of("condition" -> Seq("A", "B")).toOption.get
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 8.0, 15.0, 22.0)),
      "condition" -> Column.Strings(Vector("A", "B", "A", "B"))
    )
    val cellA = CellKey.unsafe(Vector(CellAssignment(FactorId.unsafe("condition"), LevelId.unsafe("A"))))
    val cellB = CellKey.unsafe(Vector(CellAssignment(FactorId.unsafe("condition"), LevelId.unsafe("B"))))
    val assignments = HrfByCell.of(cellA -> Hrfs.SPMG1, cellB -> Hrfs.SPMG2).toOption.get
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(condition, id = task)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(
        factorLevels = registry,
        emptyCellPolicy = EmptyCellPolicy.RetainZero,
        hrfByCell = Some(assignments)
      )
    ).toOption.get

    val model = EventModelBuilder.build(request)
    val term = convolved(model)
    assertEquals(term.columnHrfs.map(_.nbasis), Vector(1, 2, 2))
    assertEquals(term.columnBasisIx, Vector(Some(1), Some(1), Some(2)))
    assertEquals(term.columnCells, Vector(Some(cellA), Some(cellB), Some(cellB)))
    assertEquals(model.designMatrix.cols, 3)
    assert(model.policyReceipts.exists(r => r.name == "hrf-by-cell" && r.detail.contains("hrf-by-cell")))

    val origins = model.designSchema.columns.map(_.origin).collect {
      case StructuralColumnOrigin.Event(_, _, cell, _, Some(basis), _, _) => (cell, basis.basisId, basis.index.oneBased)
    }
    assertEquals(origins, Vector((cellA, Hrfs.SPMG1.name, 1), (cellB, Hrfs.SPMG2.name, 1), (cellB, Hrfs.SPMG2.name, 2)))
    assertEquals(DesignColmap.forEventModel(model).map(_.basisTotal), Vector(Some(1), Some(2), Some(2)))

    val canonicalA = term.term.convolve(Hrfs.SPMG1, sf, dropEmpty = false)
    val canonicalB = term.term.convolve(Hrfs.SPMG2, sf, dropEmpty = false)
    val actualA = model.designMatrix.data.grouped(model.designMatrix.cols).map(_(0)).toVector
    val expectedA = canonicalA.data.data.grouped(canonicalA.data.cols).map(_(0)).toVector
    actualA.zip(expectedA).foreach { case (actual, expected) => assertEqualsDouble(actual, expected, 1e-12) }
    val actualB = model.designMatrix.data.grouped(model.designMatrix.cols).flatMap(row => Vector(row(1), row(2))).toVector
    val expectedB = canonicalB.data.data.grouped(canonicalB.data.cols).flatMap(row => Vector(row(1), row(3))).toVector
    actualB.zip(expectedB).foreach { case (actual, expected) => assertEqualsDouble(actual, expected, 1e-12) }
  }

  test("heterogeneous HRF terms reject homogeneous legacy contrast expansion") {
    val sf = SamplingFrame(blockLens = Seq(40), tr = Seq(1.0), startTime = Seq(0.0))
    val registry = FactorLevelRegistry.of("condition" -> Seq("A", "B")).toOption.get
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 8.0, 15.0, 22.0)),
      "condition" -> Column.Strings(Vector("A", "B", "A", "B"))
    )
    val cellA = CellKey.unsafe(Vector(CellAssignment(FactorId.unsafe("condition"), LevelId.unsafe("A"))))
    val cellB = CellKey.unsafe(Vector(CellAssignment(FactorId.unsafe("condition"), LevelId.unsafe("B"))))
    val assignments = HrfByCell.of(cellA -> Hrfs.SPMG1, cellB -> Hrfs.SPMG2).toOption.get
    val model = EventModelBuilder.build(
      EventModelBuilder.EventDesignRequest.fromText(
        formula = "onset ~ hrf(condition, id = task)",
        data = events,
        samplingFrame = sf,
        options = EventModelBuilder.BuildOptions(
          factorLevels = registry,
          emptyCellPolicy = EmptyCellPolicy.RetainZero,
          hrfByCell = Some(assignments)
        )
      ).toOption.get
    )
    val term = convolved(model)
    assertEquals(term.basisWidths, Vector(1, 2))
    assert(term.hasHeterogeneousBasis)

    val condition = CellSelector.factorUnsafe("condition")
    val expression = ContrastExpr
      .pair("mixed-pair", condition === "A", condition === "B")
      .fold(error => fail(error.message), identity)
    ContrastCompiler.compile(term, expression) match
      case Left(ContrastError.HeterogeneousBasisUnsupported("mixed-pair", Vector(1, 2))) => ()
      case other => fail(s"expected typed heterogeneous-basis rejection, found $other")

    FContrasts.forConvolvedTermEither(term).left.toOption match
      case Some(ContrastError.HeterogeneousBasisUnsupported("task", Vector(1, 2))) => ()
      case other => fail(s"expected typed F-contrast rejection, found $other")

    val directFailure = intercept[IllegalArgumentException] {
      ConvolvedContrastWeights.pair(
        term = term,
        name = "direct-mixed-pair",
        A = cell => cell("condition") == "A",
        B = cell => cell("condition") == "B"
      )
    }
    assert(directFailure.getMessage.contains("requires one shared HRF basis width"))
  }

  test("HRF-by-cell assignments reject missing and extra structural cells") {
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0), startTime = Seq(0.0))
    val registry = FactorLevelRegistry.of("condition" -> Seq("A", "B")).toOption.get
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 8.0)),
      "condition" -> Column.Strings(Vector("A", "B"))
    )
    val cellA = CellKey.unsafe(Vector(CellAssignment(FactorId.unsafe("condition"), LevelId.unsafe("A"))))
    val cellC = CellKey.unsafe(Vector(CellAssignment(FactorId.unsafe("condition"), LevelId.unsafe("C"))))
    val assignments = HrfByCell.of(cellA -> Hrfs.SPMG1, cellC -> Hrfs.SPMG2).toOption.get
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(condition)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(
        factorLevels = registry,
        hrfByCell = Some(assignments)
      )
    ).toOption.get

    EventModelBuilder.buildEither(request).left.toOption.get match
      case DesignError.InvalidHrfAssignment(term, missing, extra) =>
        assertEquals(term, "condition")
        assertEquals(missing, Vector("cell:{condition=B}"))
        assertEquals(extra, Vector("cell:{condition=C}"))
      case other => fail(s"expected InvalidHrfAssignment, found $other")
  }

  test("HRF-by-cell assignments cover declared empty cells without offsets") {
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0), startTime = Seq(0.0))
    val registry = FactorLevelRegistry.of("condition" -> Seq("A", "B", "C")).toOption.get
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 8.0)),
      "condition" -> Column.Strings(Vector("A", "B"))
    )
    def cell(level: String): CellKey =
      CellKey.unsafe(Vector(CellAssignment(FactorId.unsafe("condition"), LevelId.unsafe(level))))
    val assignments = HrfByCell.of(
      cell("A") -> Hrfs.SPMG1,
      cell("B") -> Hrfs.SPMG2,
      cell("C") -> Hrfs.fir(nBasis = 2)
    ).toOption.get
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(condition, id = task)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(
        factorLevels = registry,
        emptyCellPolicy = EmptyCellPolicy.RetainZero,
        hrfByCell = Some(assignments)
      )
    ).toOption.get

    val model = EventModelBuilder.build(request)
    assertEquals(model.designMatrix.cols, 5)
    assertEquals(convolved(model).columnHrfs.map(_.nbasis), Vector(1, 2, 2, 2, 2))
    assertEquals(convolved(model).columnCells.map(_.map(_.canonical)), Vector(
      Some(cell("A").canonical),
      Some(cell("B").canonical),
      Some(cell("B").canonical),
      Some(cell("C").canonical),
      Some(cell("C").canonical)
    ))
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

  test("ordered orthogonalization is explicit, ordered, and receipt-backed") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val x = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    val expectedResidual = Vector(-1.0, 1.0, 0.0, 1.0, -1.0, 0.0)
    val y = x.zip(expectedResidual).map { case (value, residual) => 2.0 * value + residual }
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0, 13.0, 17.0, 21.0)),
      "x" -> Column.Doubles(x),
      "y" -> Column.Doubles(y)
    )
    val policy = ModulatorOrthogonalization
      .ordered("modulators", "x", "y")
      .fold(error => fail(error.message), identity)
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(modulators(x, y), id = modulators)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(
        orthogonalization = ModulatorOrthogonalizationPlan.one(policy)
      )
    ).fold(error => fail(error.message), identity)

    val model = EventModelBuilder.buildEither(request).fold(error => fail(error.message), identity)
    val continuous = convolved(model).term.events.collectFirst { case event: ContinuousEvent => event }
      .getOrElse(fail("modulator family should lower to one continuous event"))
    val first = Vector.tabulate(continuous.value.rows)(row => continuous.value(row, 0))
    val second = Vector.tabulate(continuous.value.rows)(row => continuous.value(row, 1))
    assertEquals(first, x)
    second.zip(expectedResidual).foreach { case (actual, expected) =>
      assertEqualsDouble(actual, expected, 1e-12)
    }
    assertEqualsDouble(
      first.zip(second).map(_ * _).sum,
      0.0,
      1e-12
    )
    val receipt = model.designSchema.audit.orthogonalizationReceipts.head
    assertEquals(receipt.policy.order.map(_.value), Vector("x", "y"))
    assertEquals(receipt.steps.head.against.map(_.value), Vector("x"))
    assertEquals(receipt.steps.head.groups.head.sourceRows, Vector(0, 1, 2, 3, 4, 5))
    assertEquals(receipt.steps.head.groups.head.referenceRank, 1)
    assertEquals(receipt.steps.head.groups.head.outcome, OrthogonalizationOutcome.Applied)
    assert(model.designSchema.fingerprint.canonicalEncoding.contains("orthogonalization="))

    val noPolicy = EventModelBuilder.build(
      formula = "onset ~ hrf(modulators(x, y), id = modulators)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector.fill(6)(0)
    )
    val untouched = convolved(noPolicy).term.events.collectFirst { case event: ContinuousEvent => event }
      .getOrElse(fail("modulator family should lower to one continuous event"))
    assertEquals(Vector.tabulate(untouched.value.rows)(row => untouched.value(row, 1)), y)
    assertEquals(noPolicy.designSchema.audit.orthogonalizationReceipts, Vector.empty)
  }

  test("ordered orthogonalization respects run scopes") {
    val sf = SamplingFrame(blockLens = Seq(20, 20), tr = Seq(1.0, 1.0), startTime = Seq(0.0, 0.0))
    val blocks = Vector(0, 0, 0, 1, 1, 1)
    val x = Vector(1.0, 2.0, 3.0, 2.0, 4.0, 6.0)
    val y = Vector(1.0, 5.0, 6.0, 3.0, 9.0, 12.0)
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0, 1.0, 5.0, 9.0)),
      "x" -> Column.Doubles(x),
      "y" -> Column.Doubles(y)
    )
    val policy = ModulatorOrthogonalization.ordered(
      term = TermId.unsafe("modulators"),
      order = Vector(ModulatorId.unsafe("x"), ModulatorId.unsafe("y")),
      scope = OrthogonalizationScope.WithinRun
    ).fold(error => fail(error.message), identity)
    val model = EventModelBuilder.build(
      EventModelBuilder.EventDesignRequest.fromText(
        formula = "onset ~ hrf(modulators(x, y), id = modulators)",
        data = events,
        samplingFrame = sf,
        blockPlan = EventModelBuilder.BlockPlan.explicit(blocks),
        options = EventModelBuilder.BuildOptions(
          orthogonalization = ModulatorOrthogonalizationPlan.one(policy)
        )
      ).fold(error => fail(error.message), identity)
    )
    val values = convolved(model).term.events.collectFirst { case event: ContinuousEvent => event }
      .getOrElse(fail("modulator family should lower to one continuous event"))
    blocks.distinct.foreach { run =>
      val rows = blocks.indices.filter(blocks(_) == run)
      val crossproduct = rows.map(row => values.value(row, 0) * values.value(row, 1)).sum
      assertEqualsDouble(crossproduct, 0.0, 1e-12)
    }
    assertEquals(
      model.designSchema.audit.orthogonalizationReceipts.head.steps.head.groups.map(_.key),
      Vector("run-1", "run-2")
    )
  }

  test("degenerate modulator policies retain evidence or reject with a typed error") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "x" -> Column.Doubles(Vector(1.0, 2.0, 3.0)),
      "y" -> Column.Doubles(Vector(2.0, 4.0, 6.0))
    )
    val retainedPolicy = ModulatorOrthogonalization
      .ordered("modulators", "x", "y")
      .fold(error => fail(error.message), identity)
    val retainedRequest = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(modulators(x, y), id = modulators)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(
        orthogonalization = ModulatorOrthogonalizationPlan.one(retainedPolicy)
      )
    ).fold(error => fail(error.message), identity)
    val retained = EventModelBuilder.buildEither(retainedRequest).fold(error => fail(error.message), identity)
    assertEquals(
      retained.designSchema.audit.orthogonalizationReceipts.head.steps.head.groups.head.outcome,
      OrthogonalizationOutcome.DegenerateRetained
    )

    val rejectPolicy = ModulatorOrthogonalization.ordered(
      term = TermId.unsafe("modulators"),
      order = Vector(ModulatorId.unsafe("x"), ModulatorId.unsafe("y")),
      degenerate = DegenerateModulatorPolicy.Reject
    ).fold(error => fail(error.message), identity)
    val rejectedRequest = retainedRequest.copy(
      options = retainedRequest.options.copy(
        orthogonalization = ModulatorOrthogonalizationPlan.one(rejectPolicy)
      )
    )
    EventModelBuilder.buildEither(rejectedRequest) match
      case Left(DesignError.DegenerateModulator(term, modulator, scope, DegenerateModulatorPolicy.Reject)) =>
        assertEquals(term, "modulators")
        assertEquals(modulator, ModulatorId.unsafe("y"))
        assertEquals(scope, "whole-term")
      case other => fail(s"expected typed degenerate-modulator failure, found $other")
  }

  test("centering records an explicit empty-scope outcome") {
    val sf = SamplingFrame(blockLens = Seq(10), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0)),
      "x" -> Column.Doubles(Vector(1.0, 3.0)),
      "keep" -> Column.Bools(Vector(false, false))
    )
    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(center(x), subset = keep)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0)
    )
    val receipt = model.designSchema.audit.centeringReceipts.head
    assertEquals(receipt.groups.map(_.outcome), Vector(CenteringOutcome.EmptyScope))
    assertEquals(receipt.groups.head.eventIndices, Vector.empty)
  }

  test("builder records the default zero-contribution missing-value policy") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "condition" -> Column.Strings(Vector("A", "B", "A")),
      "x" -> Column.Doubles(Vector(1.0, Double.NaN, 3.0))
    )
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(condition, x, id = task)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(
        missingValuePolicy = MissingValuePolicy.ZeroContribution
      )
    ).toOption.get

    val model = EventModelBuilder.build(request)
    assertEquals(model.missingValueResolutions.map(_.eventIndex), Vector(1))
    assertEquals(model.missingValueResolutions.map(_.modulator.value), Vector("x"))
    assertEquals(model.missingValueResolutions.map(_.action), Vector("zero-contribution"))
    assertEquals(model.missingValueResolutions.map(_.column), Vector(Some("x")))
    assert(model.policyReceipts.exists(r => r.name == "modulator-missing-values" && r.detail.contains("zero-contribution")))
    assertEquals(model.designSchema.audit.missingValues, model.missingValueResolutions)
    val term = convolved(model).term
    val modulator = term.events.collectFirst { case event: ContinuousEvent => event }
      .getOrElse(fail("task term should retain its continuous modulator"))
    assertEquals(term.onsets.length, 3)
    assertEquals(term.events.find(_.varName == "condition").map(_.nEvents), Some(3))
    assertEqualsDouble(modulator.value(1, 0), 0.0, 0.0)
    assert(model.designMatrix.data.forall(_.isFinite))
  }

  test("explicit centering policies retain scope and receipt evidence") {
    val sf = SamplingFrame(blockLens = Seq(20, 20), tr = Seq(1.0, 1.0), startTime = Seq(0.0, 0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 3.0, 5.0, 1.0, 3.0, 5.0)),
      "x" -> Column.Doubles(Vector(1.0, 3.0, 5.0, 10.0, 12.0, 14.0)),
      "group" -> Column.Strings(Vector("A", "A", "B", "A", "A", "B")),
      "cell" -> Column.Strings(Vector("X", "X", "Y", "X", "X", "Y"))
    )
    val blocks = Vector(0, 0, 0, 1, 1, 1)

    def centeredValues(formula: String): Vector[Double] =
      val model = EventModelBuilder.build(
        formula = formula,
        data = events,
        samplingFrame = sf,
        blockIds = blocks
      )
      convolved(model).term.events.collectFirst { case e: ContinuousEvent => e }.get.value.data.toVector

    val grand = EventModelBuilder.build(
      formula = "onset ~ hrf(center(x))",
      data = events,
      samplingFrame = sf,
      blockIds = blocks
    )
    val withinGroup = EventModelBuilder.build(
      formula = "onset ~ hrf(center_within(x, group))",
      data = events,
      samplingFrame = sf,
      blockIds = blocks
    )
    val withinRun = EventModelBuilder.build(
      formula = "onset ~ hrf(center_within_run(x))",
      data = events,
      samplingFrame = sf,
      blockIds = blocks
    )
    val withinCell = EventModelBuilder.build(
      formula = "onset ~ hrf(center_within(x, group, cell))",
      data = events,
      samplingFrame = sf,
      blockIds = blocks
    )
    val at = EventModelBuilder.build(
      formula = "onset ~ hrf(center_at(x, 2.0))",
      data = events,
      samplingFrame = sf,
      blockIds = blocks
    )

    centeredValues("onset ~ hrf(center(x))").zip(Vector(-6.5, -4.5, -2.5, 2.5, 4.5, 6.5)).foreach {
      case (actual, expected) => assertEqualsDouble(actual, expected, 1e-12)
    }
    centeredValues("onset ~ hrf(center_within(x, group))").zip(Vector(-5.5, -3.5, -4.5, 3.5, 5.5, 4.5)).foreach {
      case (actual, expected) => assertEqualsDouble(actual, expected, 1e-12)
    }
    centeredValues("onset ~ hrf(center_within_run(x))").zip(Vector(-2.0, 0.0, 2.0, -2.0, 0.0, 2.0)).foreach {
      case (actual, expected) => assertEqualsDouble(actual, expected, 1e-12)
    }
    centeredValues("onset ~ hrf(center_at(x, 2.0))").zip(Vector(-1.0, 1.0, 3.0, 8.0, 10.0, 12.0)).foreach {
      case (actual, expected) => assertEqualsDouble(actual, expected, 1e-12)
    }

    val grandReceipt = grand.designSchema.audit.centeringReceipts.head
    assertEquals(grandReceipt.policy, CenteringPolicy.GrandMean)
    assertEquals(grandReceipt.groups.map(_.center), Vector(Some(7.5)))
    assertEquals(grandReceipt.affectedEvents, Vector(0, 1, 2, 3, 4, 5))
    assertEquals(grandReceipt.groups.map(_.outcome), Vector(CenteringOutcome.Applied))

    val groupReceipt = withinGroup.designSchema.audit.centeringReceipts.head
    assertEquals(groupReceipt.policy, CenteringPolicy.WithinFactor(FactorId.unsafe("group")))
    assertEquals(groupReceipt.groups.map(_.center), Vector(Some(6.5), Some(9.5)))

    val runReceipt = withinRun.designSchema.audit.centeringReceipts.head
    assertEquals(runReceipt.policy, CenteringPolicy.WithinRun)
    assertEquals(runReceipt.groups.map(_.center), Vector(Some(3.0), Some(12.0)))

    val cellReceipt = withinCell.designSchema.audit.centeringReceipts.head
    assertEquals(
      cellReceipt.policy,
      CenteringPolicy.WithinCells(Vector(FactorId.unsafe("group"), FactorId.unsafe("cell")))
    )
    assertEquals(cellReceipt.groups.map(_.center), Vector(Some(6.5), Some(9.5)))

    val atReceipt = at.designSchema.audit.centeringReceipts.head
    assertEquals(atReceipt.policy, CenteringPolicy.At(2.0))
    assert(at.designSchema.fingerprint.canonicalEncoding.contains("centering="))
  }

  test("centering excludes non-finite values from the reference before missing repair") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "x" -> Column.Doubles(Vector(1.0, Double.NaN, 3.0))
    )
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(center(x))",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(
        missingValuePolicy = MissingValuePolicy.ZeroContribution
      )
    ).toOption.get

    val model = EventModelBuilder.build(request)
    val continuous = convolved(model).term.events.collectFirst { case e: ContinuousEvent => e }.get
    assertEquals(continuous.value.data.toVector, Vector(-1.0, 0.0, 1.0))
    val receipt = model.designSchema.audit.centeringReceipts.head
    assertEquals(receipt.groups.head.center, Some(2.0))
    assertEquals(receipt.groups.head.finiteEventIndices, Vector(0, 2))
    assertEquals(model.missingValueResolutions.map(_.eventIndex), Vector(1))
    assertEquals(model.missingValueResolutions.map(_.action), Vector("zero-contribution"))
  }

  test("centering is scoped after term subsetting and invariant to event-row permutation") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))

    def build(onsets: Vector[Double], values: Vector[Double], keep: Vector[Boolean]): EventModel =
      val data = DataTable.fromColumns(
        "onset" -> Column.Doubles(onsets),
        "x" -> Column.Doubles(values),
        "keep" -> Column.Bools(keep)
      )
      EventModelBuilder.build(
        formula = "onset ~ hrf(center(x), subset = keep)",
        data = data,
        samplingFrame = sf,
        blockIds = Vector.fill(onsets.length)(0)
      )

    val first = build(Vector(1.0, 3.0, 5.0), Vector(1.0, 3.0, 100.0), Vector(true, true, false))
    val permuted = build(Vector(3.0, 1.0, 5.0), Vector(3.0, 1.0, 100.0), Vector(true, true, false))
    val firstEvent = convolved(first).term.events.collectFirst { case e: ContinuousEvent => e }.get
    assertEquals(firstEvent.value.data.toVector, Vector(-1.0, 1.0))
    assertEquals(first.designMatrix.data.toVector, permuted.designMatrix.data.toVector)
    assertEquals(first.designSchema.fingerprint, permuted.designSchema.fingerprint)
    assertEquals(first.designSchema.audit.centeringReceipts.head.groups.head.center, Some(2.0))
    assertEquals(permuted.designSchema.audit.centeringReceipts.head.groups.head.center, Some(2.0))
  }

  test("builder rejects non-finite modulators when policy is Reject") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "x" -> Column.Doubles(Vector(1.0, Double.NaN, 3.0))
    )
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(x)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(
        missingValuePolicy = MissingValuePolicy.Reject
      )
    ).toOption.get

    val error = EventModelBuilder.buildEither(request).left.toOption.get
    error match
      case DesignError.MissingModulatorValue(term, column, eventIndex, policy) =>
        assertEquals(term, "x")
        assertEquals(column, "x")
        assertEquals(eventIndex, 1)
        assertEquals(policy, "reject")
      case other => fail(s"expected MissingModulatorValue, found $other")
  }

  test("builder applies an explicit imputation constant before convolution") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "x" -> Column.Doubles(Vector(1.0, Double.NaN, 3.0))
    )
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(x)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(
        missingValuePolicy = MissingValuePolicy.ImputeConstant(2.0)
      )
    ).toOption.get

    val model = EventModelBuilder.build(request)
    val continuous = convolved(model).term.events.collectFirst { case e: ContinuousEvent => e }.get
    assertEquals(continuous.value.data.toVector, Vector(1.0, 2.0, 3.0))
    assertEquals(model.missingValueResolutions.map(_.action), Vector("imputed-constant"))
    assert(model.policyReceipts.exists(_.detail.contains("impute-constant")))
  }

  test("builder reports a non-finite imputation constant as a typed error") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "x" -> Column.Doubles(Vector(1.0, Double.NaN, 3.0))
    )
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(x)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(
        missingValuePolicy = MissingValuePolicy.ImputeConstant(Double.NaN)
      )
    ).toOption.get

    val error = EventModelBuilder.buildEither(request).left.toOption.get
    error match
      case DesignError.InvalidSchema(detail) =>
        assert(detail.contains("impute constant must be finite"))
      case other => fail(s"expected InvalidSchema, found $other")
  }

  test("builder drops missing events only from the selected term") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "x" -> Column.Doubles(Vector(1.0, Double.NaN, 3.0))
    )
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(x)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(
        missingValuePolicy = MissingValuePolicy.DropFromTerm
      )
    ).toOption.get

    val model = EventModelBuilder.build(request)
    val ct = convolved(model)
    assertEquals(ct.term.onsets.map(_.value), Vector(1.0, 9.0))
    assertEquals(ct.term.events.map(_.nEvents), Vector(2))
    assertEquals(model.missingValueResolutions.map(_.action), Vector("dropped-event"))
    assertEquals(model.designSchema.rows.rows, sf.blockLens.sum)
    assert(model.designMatrix.data.forall(_.isFinite))
  }

  test("declared factor levels give stable order and retain an explicit zero cell") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val registry = FactorLevelRegistry.of("condition" -> Seq("A", "B", "C")).toOption.get
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "condition" -> Column.Strings(Vector("B", "A", "B"))
    )
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(condition)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(
        factorLevels = registry,
        emptyCellPolicy = EmptyCellPolicy.RetainZero
      )
    ).toOption.get

    val model = EventModelBuilder.build(request)
    val categorical = convolved(model).term.events.collectFirst { case e: scalafim.fmri.design.event.CategoricalEvent => e }.get
    assertEquals(categorical.levels, Vector("A", "B", "C"))
    assertEquals(model.designMatrix.cols, 3)
    val cColumn = convolved(model).term.designMatrix(dropEmpty = false).data
    assert(cColumn.data.grouped(3).forall(row => row(2) == 0.0))
    val factorAudit = model.designSchema.audit.factorLevels.find(_.factor.value == "condition").get
    assertEquals(factorAudit.declared.map(_.map(_.value)), Some(Vector("A", "B", "C")))
    assertEquals(factorAudit.observed.map(_.value), Vector("A", "B"))
    val empty = CellKey.unsafe(Vector(CellAssignment(FactorId.unsafe("condition"), LevelId.unsafe("C"))))
    assert(model.designSchema.audit.emptyCells.contains(empty))
    val retainedAudit = model.designSchema.audit.emptyCellAudits.find(_.cell == empty).get
    assertEquals(retainedAudit.policy, EmptyCellPolicy.RetainZero)
    assertEquals(retainedAudit.disposition, EmptyCellDisposition.RetainedZero)
    assert(retainedAudit.term.exists(_.value.nonEmpty))
    assert(model.policyReceipts.exists(r => r.name == "empty-cells" && r.detail.contains("retain-zero")))

    val omittedRequest = request.copy(
      options = request.options.copy(emptyCellPolicy = EmptyCellPolicy.Omit)
    )
    val omitted = EventModelBuilder.build(omittedRequest)
    val omittedAudit = omitted.designSchema.audit.emptyCellAudits.find(_.cell == empty).get
    assertEquals(omittedAudit.policy, EmptyCellPolicy.Omit)
    assertEquals(omittedAudit.disposition, EmptyCellDisposition.Omitted)
    assertEquals(omitted.designMatrix.cols, 2)
  }

  test("declared factor levels make row permutation semantically invariant") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val registry = FactorLevelRegistry.of("condition" -> Seq("A", "B", "C")).toOption.get

    def build(onsets: Vector[Double], levels: Vector[String]): EventModel =
      val data = DataTable.fromColumns(
        "onset" -> Column.Doubles(onsets),
        "condition" -> Column.Strings(levels)
      )
      val request = EventModelBuilder.EventDesignRequest.fromText(
        formula = "onset ~ hrf(condition)",
        data = data,
        samplingFrame = sf,
        options = EventModelBuilder.BuildOptions(
          factorLevels = registry,
          emptyCellPolicy = EmptyCellPolicy.RetainZero
        )
      ).toOption.get
      EventModelBuilder.build(request)

    val first = build(Vector(1.0, 5.0, 9.0), Vector("A", "B", "B"))
    val permuted = build(Vector(5.0, 9.0, 1.0), Vector("B", "B", "A"))
    assertEquals(first.columnNames, permuted.columnNames)
    first.designMatrix.data.zip(permuted.designMatrix.data).foreach { case (actual, expected) =>
      assertEqualsDouble(actual, expected, 1e-12)
    }
  }

  test("declared factor levels reject unknown observations before convolution") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val registry = FactorLevelRegistry.of("condition" -> Seq("A", "B")).toOption.get
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0)),
      "condition" -> Column.Strings(Vector("A", "C"))
    )
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(condition)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(factorLevels = registry)
    ).toOption.get

    EventModelBuilder.buildEither(request).left.toOption.get match
      case DesignError.UnknownFactorLevel(factor, observed, declared) =>
        assertEquals(factor, "condition")
        assertEquals(observed, "C")
        assertEquals(declared, Vector("A", "B"))
      case other => fail(s"expected UnknownFactorLevel, found $other")
  }

  test("Reject makes declared empty cells a typed compilation error") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val registry = FactorLevelRegistry.of("condition" -> Seq("A", "B", "C")).toOption.get
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0)),
      "condition" -> Column.Strings(Vector("A", "B"))
    )
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(condition)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(
        factorLevels = registry,
        emptyCellPolicy = EmptyCellPolicy.Reject
      )
    ).toOption.get

    EventModelBuilder.buildEither(request).left.toOption.get match
      case DesignError.EmptyFactorCell(term, cell, policy) =>
        assertEquals(term, "condition")
        assertEquals(cell, "cell:{condition=C}")
        assertEquals(policy, "reject")
      case other => fail(s"expected EmptyFactorCell, found $other")
  }

  test("explicit empty-cell policies apply within each run partition") {
    val runOne = RunIndex.unsafeOneBased(1)
    val runTwo = RunIndex.unsafeOneBased(2)
    val sf = SamplingFrame(blockLens = Seq(12, 12), tr = Seq(1.0, 1.0), startTime = Seq(0.0, 0.0))
    val registry = FactorLevelRegistry.of("condition" -> Seq("A", "B")).toOption.get
    val emptyB = CellKey.unsafe(Vector(CellAssignment(FactorId.unsafe("condition"), LevelId.unsafe("B"))))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 1.0)),
      "condition" -> Column.Strings(Vector("A", "B", "A"))
    )

    def request(policy: EmptyCellPolicy): EventModelBuilder.EventDesignRequest =
      EventModelBuilder.EventDesignRequest.fromText(
        formula = "onset ~ hrf(condition)",
        data = events,
        samplingFrame = sf,
        blockPlan = EventModelBuilder.BlockPlan.explicit(Seq(0, 0, 1)),
        options = EventModelBuilder.BuildOptions(
          factorLevels = registry,
          emptyCellPolicy = policy
        )
      ).toOption.get

    val retained = EventModelBuilder.build(request(EmptyCellPolicy.RetainZero))
    val retainedAudit = retained.designSchema.audit.emptyCellAudits.find { audit =>
      audit.run.contains(runTwo) && audit.cell == emptyB
    }.getOrElse(fail("run-two retained empty-cell audit should be present"))
    assertEquals(retainedAudit.disposition, EmptyCellDisposition.RetainedZero)
    assert(
      retained.designSchema.runwiseProjection(runTwo).toOption.get
        .decisions.exists(decision => decision.sourceColumn.origin match
          case StructuralColumnOrigin.Event(_, _, cell, _, _, _, _) =>
            cell == emptyB && decision.disposition.isInstanceOf[RunColumnDisposition.NonEstimable]
          case _ => false
        )
    )

    val omitted = EventModelBuilder.build(request(EmptyCellPolicy.Omit))
    val omittedAudit = omitted.designSchema.audit.emptyCellAudits.find { audit =>
      audit.run.contains(runTwo) && audit.cell == emptyB
    }.getOrElse(fail("run-two omitted empty-cell audit should be present"))
    assertEquals(omittedAudit.disposition, EmptyCellDisposition.Omitted)
    assert(
      omitted.designSchema.runwiseProjection(runTwo).toOption.get
        .decisions.exists(decision => decision.sourceColumn.origin match
          case StructuralColumnOrigin.Event(_, _, cell, _, _, _, _) =>
            cell == emptyB && decision.disposition.isInstanceOf[RunColumnDisposition.Omitted]
          case _ => false
        )
    )
    assertEquals(omitted.designSchema.runwiseProjection(runOne).toOption.get.sharedSourceColumnIndices.length, 2)

    EventModelBuilder.buildEither(request(EmptyCellPolicy.Reject)).left.toOption.get match
      case DesignError.EmptyFactorCellInRun(term, cell, run, policy) =>
        assertEquals(term, "condition")
        assertEquals(cell, emptyB)
        assertEquals(run, runTwo)
        assertEquals(policy, EmptyCellPolicy.Reject)
      case other => fail(s"expected EmptyFactorCellInRun, found $other")
  }

  test("factor registries reject duplicate declared levels") {
    FactorLevelRegistry.of("condition" -> Seq("A", "A")).left.toOption.get match
      case DesignError.InvalidSchema(detail) =>
        assert(detail.contains("duplicate"))
      case other => fail(s"expected InvalidSchema, found $other")
  }

  test("Scala enums derive stable declared level order through the validated constructor") {
    val factor = FactorId.unsafe("condition")
    val derived = FactorLevelSet.fromEnum[TrialCondition](factor)
    val explicit = FactorLevelSet.from(factor, Seq("Match", "Mismatch", "Catch"))

    assertEquals(derived, explicit)
    assertEquals(derived.toOption.map(_.values), Some(Vector("Match", "Mismatch", "Catch")))
  }

  test("run factor schema bindings reject incompatible level declarations") {
    val run1 = FactorLevelRegistry.of("condition" -> Seq("A", "B")).toOption.get
    val run2 = FactorLevelRegistry.of("condition" -> Seq("A", "C")).toOption.get

    FactorSchemaBinding.perRun("run-1" -> run1, "run-2" -> run2).left.toOption.get match
      case DesignError.IncompatibleFactorSchema(scope, source, factor, expected, observed) =>
        assertEquals(scope, "run")
        assertEquals(source, "run-2")
        assertEquals(factor, "condition")
        assertEquals(expected, Vector("A", "B"))
        assertEquals(observed, Vector("A", "C"))
      case other => fail(s"expected IncompatibleFactorSchema, found $other")
  }

  test("bound run schemas are retained in multi-run factor audit evidence") {
    val sf = SamplingFrame(blockLens = Seq(20, 20), tr = Seq(1.0, 1.0), startTime = Seq(0.0, 0.0))
    val registry = FactorLevelRegistry.of("condition" -> Seq("A", "B")).toOption.get
    val binding = FactorSchemaBinding
      .perRun("run-1" -> registry, "run-2" -> registry)
      .toOption
      .get
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 1.0, 5.0)),
      "condition" -> Column.Strings(Vector("A", "A", "B", "B"))
    )
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(condition)",
      data = events,
      samplingFrame = sf,
      blockPlan = EventModelBuilder.BlockPlan.explicit(Vector(0, 0, 1, 1)),
      options = EventModelBuilder.BuildOptions(
        factorLevels = registry,
        factorSchemaBinding = Some(binding),
        emptyCellPolicy = EmptyCellPolicy.RetainZero
      )
    ).toOption.get

    val model = EventModelBuilder.build(request)
    val audit = model.designSchema.audit.factorLevels.find(_.factor.value == "condition").get
    assertEquals(audit.partitions.map(_.partition), Vector("run-1", "run-2"))
    assertEquals(audit.partitions.map(_.observed.map(_.value)), Vector(Vector("A"), Vector("B")))
    assert(model.policyReceipts.exists(r => r.name == "factor-schema-binding" && r.detail.contains("sources=run-1,run-2")))
    assert(model.designSchema.fingerprint.canonicalEncoding.contains("run-1"))
  }

  test("declared registries can explicitly treat integer columns as factors") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val registry = FactorLevelRegistry.of("load" -> Seq("1", "2", "3")).toOption.get
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "load" -> Column.Ints(Vector(2, 1, 2))
    )
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(load)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(
        factorLevels = registry,
        emptyCellPolicy = EmptyCellPolicy.RetainZero
      )
    ).toOption.get

    val model = EventModelBuilder.build(request)
    val categorical = convolved(model).term.events.collectFirst { case e: scalafim.fmri.design.event.CategoricalEvent => e }.get
    assertEquals(categorical.levels, Vector("1", "2", "3"))
    assertEquals(model.designMatrix.cols, 3)
  }

  test("declared registries use stable text for double-coded factor values") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val registry = FactorLevelRegistry.of("load" -> Seq("1", "2")).toOption.get
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0)),
      "load" -> Column.Doubles(Vector(2.0, 1.0))
    )
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = "onset ~ hrf(load)",
      data = events,
      samplingFrame = sf,
      options = EventModelBuilder.BuildOptions(factorLevels = registry)
    ).toOption.get

    val model = EventModelBuilder.build(request)
    val categorical = convolved(model).term.events.collectFirst { case e: scalafim.fmri.design.event.CategoricalEvent => e }.get
    assertEquals(categorical.levels, Vector("1", "2"))
  }

  test("builder records parametric basis repairs and strict mode promotes them to errors") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "x" -> Column.Doubles(Vector(2.0, 2.0, 2.0))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(scale(x))",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0)
    )
    assert(model.diagnostics.exists(_.kind == EventModelDiagnosticKind.BasisDegeneracy))
    assert(model.diagnostics.exists(_.message.contains("zero variance")))
    assert(model.designMatrix.data.forall(_.isFinite))

    val strictError = intercept[IllegalArgumentException] {
      EventModelBuilder.build(
        formula = "onset ~ hrf(scale(x))",
        data = events,
        samplingFrame = sf,
        blockIds = Vector(0, 0, 0),
        strict = true
      )
    }
    assert(strictError.getMessage.contains("zero variance"))
  }

  test("builder reports all non-finite parametric basis inputs") {
    val sf = SamplingFrame(blockLens = Seq(30), tr = Seq(1.0), startTime = Seq(0.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 5.0, 9.0)),
      "x" -> Column.Doubles(Vector(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity))
    )

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(standardized(x))",
      data = events,
      samplingFrame = sf,
      blockIds = Vector(0, 0, 0)
    )

    assert(model.diagnostics.exists(_.kind == EventModelDiagnosticKind.BasisDegeneracy))
    assert(model.diagnostics.exists(_.message.contains("all non-finite")))
    assert(model.designMatrix.data.forall(_.isFinite))
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
