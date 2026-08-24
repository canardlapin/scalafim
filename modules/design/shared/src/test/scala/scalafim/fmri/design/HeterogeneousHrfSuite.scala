package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.contrast.LevelId
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.design.event.{Event, EventModel, EventTerm}
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame

/** Structural contracts for phase- and cell-specific mixed-width HRFs. */
class HeterogeneousHrfSuite extends munit.FunSuite:

  private val sampling = SamplingFrame(blockLens = Seq(28), tr = Seq(1.0), startTime = Seq(0.0))
  private val levels = FactorLevelRegistry.of("condition" -> Seq("A", "B", "C"))
    .fold(error => fail(error.message), identity)

  test("phase assignments are exhaustive and derive cardinality from assigned bases") {
    val probe = HrfByCell.oneFactor(
      "condition",
      "A" -> Hrfs.SPMG2,
      "B" -> Hrfs.fir(nBasis = 3)
    ).fold(error => fail(error.message), identity)
    val assignments = HrfByPhase.named(
      "sample" -> HrfAssignment.Shared(Hrfs.SPMG1),
      "probe" -> HrfAssignment.ByCell(probe)
    ).fold(error => fail(error.message), identity)
    val model = build(
      twoPhaseFormula,
      twoConditionData(Vector(0, 1, 2, 3)),
      EventModelBuilder.BuildOptions(
        factorLevels = FactorLevelRegistry.of("condition" -> Seq("A", "B"))
          .fold(error => fail(error.message), identity),
        hrfByPhase = Some(assignments)
      )
    )
    val origins = eventOrigins(model)
    val sample = origins.filter(_.phase.exists(_.value == "sample"))
    val probeColumns = origins.filter(_.phase.exists(_.value == "probe"))

    assertEquals(sample.length, 2)
    assertEquals(probeColumns.length, 5)
    assertEquals(model.designMatrix.cols, 7)
    assert(
      origins.forall(_.basis.exists(ref => ref.elementId.nonEmpty && ref.role.nonEmpty))
    )
    assertEquals(
      probeColumns.groupBy(_.cell.canonical).view.mapValues(_.length).toMap,
      Map(cell("condition", "A").canonical -> 2, cell("condition", "B").canonical -> 3)
    )
    assert(model.policyReceipts.exists(_.name == "hrf-by-phase"))
  }

  test("phase assignment coverage reports missing and extra phases together") {
    val assignments = HrfByPhase.named(
      "sample" -> HrfAssignment.Shared(Hrfs.SPMG1),
      "delay" -> HrfAssignment.Shared(Hrfs.SPMG2)
    ).fold(error => fail(error.message), identity)
    val request = requestFor(
      twoPhaseFormula,
      twoConditionData(Vector(0, 1, 2, 3)),
      EventModelBuilder.BuildOptions(hrfByPhase = Some(assignments))
    )

    EventModelBuilder.buildEither(request) match
      case Left(DesignError.InvalidPhaseHrfAssignment(missing, extra)) =>
        assertEquals(missing.map(_.value), Vector("probe"))
        assertEquals(extra.map(_.value), Vector("delay"))
      case other => fail(s"expected exact phase-assignment coverage error, found $other")
  }

  test("declared empty cells retain their assigned widths") {
    val probe = HrfByCell.oneFactor(
      "condition",
      "A" -> Hrfs.SPMG1,
      "B" -> Hrfs.SPMG2,
      "C" -> Hrfs.fir(nBasis = 3)
    ).fold(error => fail(error.message), identity)
    val assignments = HrfByPhase.named(
      "probe" -> HrfAssignment.ByCell(probe)
    ).fold(error => fail(error.message), identity)
    val model = build(
      onePhaseFormula,
      twoConditionData(Vector(0, 1, 2, 3)),
      EventModelBuilder.BuildOptions(
        factorLevels = levels,
        emptyCellPolicy = EmptyCellPolicy.RetainZero,
        hrfByPhase = Some(assignments)
      )
    )
    val emptyCell = cell("condition", "C")
    val emptyColumns = model.designSchema.columns.zipWithIndex.collect {
      case (column, index)
          if (column.origin match
            case event: StructuralColumnOrigin.Event => event.cell == emptyCell
            case _                                   => false) =>
        index
    }

    assertEquals(model.designMatrix.cols, 6)
    assertEquals(emptyColumns.length, 3)
    assert(emptyColumns.forall(index => matrixColumn(model, index).forall(_ == 0.0)))
    assert(model.designSchema.audit.emptyCells.contains(emptyCell))
  }

  test("cell assignment coverage reports missing and extra declared cells together") {
    val probe = HrfByCell.oneFactor(
      "condition",
      "A" -> Hrfs.SPMG1,
      "B" -> Hrfs.SPMG2,
      "D" -> Hrfs.fir(nBasis = 3)
    ).fold(error => fail(error.message), identity)
    val assignments = HrfByPhase.named(
      "probe" -> HrfAssignment.ByCell(probe)
    ).fold(error => fail(error.message), identity)
    val request = requestFor(
      onePhaseFormula,
      twoConditionData(Vector(0, 1, 2, 3)),
      EventModelBuilder.BuildOptions(
        factorLevels = levels,
        emptyCellPolicy = EmptyCellPolicy.RetainZero,
        hrfByPhase = Some(assignments)
      )
    )

    EventModelBuilder.buildEither(request) match
      case Left(DesignError.InvalidHrfAssignment("probe", missing, extra)) =>
        assertEquals(missing, Vector(cell("condition", "C").canonical))
        assertEquals(extra, Vector(cell("condition", "D").canonical))
      case other => fail(s"expected exact cell-assignment coverage error, found $other")
  }

  test("unscoped cell assignments cannot silently govern several HRF terms") {
    val assignments = HrfByCell.oneFactor(
      "condition",
      "A" -> Hrfs.SPMG1,
      "B" -> Hrfs.SPMG2
    ).fold(error => fail(error.message), identity)
    val request = requestFor(
      twoPhaseFormula,
      twoConditionData(Vector(0, 1, 2, 3)),
      EventModelBuilder.BuildOptions(
        factorLevels = FactorLevelRegistry.of("condition" -> Seq("A", "B"))
          .fold(error => fail(error.message), identity),
        hrfByCell = Some(assignments)
      )
    )

    EventModelBuilder.buildEither(request) match
      case Left(DesignError.InvalidSchema(detail)) =>
        assert(detail.contains("use hrfByPhase for multiphase formulas"))
      case other => fail(s"expected an explicit assignment-scope error, found $other")
  }

  test("canonical informed FIR spline Fourier and custom bases coexist deterministically") {
    val delay = HrfByCell.oneFactor(
      "condition",
      "A" -> Hrfs.SPMG2,
      "B" -> Hrfs.fir(nBasis = 2),
      "C" -> Hrfs.bspline(nBasis = 3)
    ).fold(error => fail(error.message), identity)
    val probe = HrfByCell.oneFactor(
      "condition",
      "A" -> Hrfs.fourier(nBasis = 3),
      "B" -> customPair,
      "C" -> Hrfs.SPMG1
    ).fold(error => fail(error.message), identity)
    val assignments = HrfByPhase.named(
      "sample" -> HrfAssignment.Shared(Hrfs.SPMG1),
      "delay" -> HrfAssignment.ByCell(delay),
      "probe" -> HrfAssignment.ByCell(probe)
    ).fold(error => fail(error.message), identity)
    val options = EventModelBuilder.BuildOptions(factorLevels = levels, hrfByPhase = Some(assignments))
    val originalData = threePhaseData(Vector(0, 1, 2, 3, 4, 5))
    val permutedData = threePhaseData(Vector(4, 0, 5, 2, 1, 3))
    val original = build(threePhaseFormula, originalData, options)
    val eventPermuted = build(threePhaseFormula, permutedData, options)
    val termPermuted = build(threePhaseFormulaPermuted, originalData, options)

    val represented = eventOrigins(original).flatMap(_.basis.map(_.basisId)).toSet
    assert(
      Set(Hrfs.SPMG1.name, Hrfs.SPMG2.name, Hrfs.fir(2).name, Hrfs.bspline(3).name, Hrfs.fourier(3).name, customPair.name)
        .subsetOf(represented)
    )
    assertAlignedColumns(original, eventPermuted)
    assertAlignedColumns(original, termPermuted)
  }

  test("scan-space scaling is named, exact, and preserved in structural identity") {
    val data = twoConditionData(Vector(0, 1, 2, 3))
    val registry = FactorLevelRegistry.of("condition" -> Seq("A", "B"))
      .fold(error => fail(error.message), identity)
    val options = EventModelBuilder.BuildOptions(factorLevels = registry)
    val raw = build(
      "sample_onset ~ hrf(condition, basis = spmg2, id = task, scaling = as_convolved)",
      data,
      options
    )
    val scaled = build(
      "sample_onset ~ hrf(condition, basis = spmg2, id = task, scaling = unit_maximum_absolute)",
      data,
      options
    )
    val compatibility = build(
      "sample_onset ~ hrf(condition, basis = spmg2, id = task, normalize = true)",
      data,
      options
    )

    assertEquals(raw.designMatrix.cols, scaled.designMatrix.cols)
    var column = 0
    while column < raw.designMatrix.cols do
      val rawValues = matrixColumn(raw, column)
      val scaledValues = matrixColumn(scaled, column)
      val divisor = rawValues.map(math.abs).max
      assertEqualsDouble(scaled.designSchema.columns(column).hrfScale.divisor, divisor, 1e-12)
      assertEquals(scaled.designSchema.columns(column).hrfScale.policy, HrfColumnScaling.UnitMaximumAbsolute)
      scaledValues.zip(rawValues).foreach { case (actual, expected) =>
        assertEqualsDouble(actual * divisor, expected, 1e-12)
      }
      column += 1
    assertEquals(compatibility.designMatrix.data.toVector, scaled.designMatrix.data.toVector)
    assertEquals(compatibility.designSchema.columns.map(_.hrfScale), scaled.designSchema.columns.map(_.hrfScale))
    assertNotEquals(raw.designSchema.fingerprint, scaled.designSchema.fingerprint)
    val relabeled = scaled.designSchema.withRenderedLabels(Vector.tabulate(scaled.designMatrix.cols)(i => s"column-${i + 1}"))
      .fold(error => fail(error.message), identity)
    assertEquals(relabeled.columns.map(_.hrfScale), scaled.designSchema.columns.map(_.hrfScale))
  }

  test("unit-maximum scaling remains exact for small nonzero columns") {
    val tiny = Hrf.scalar(
      name = "tiny-canonical",
      span = Seconds(4.0),
      support = Support.Compact(Seconds(4.0))
    )(lag => 1e-12 * math.exp(-lag.value))
    val term = EventTerm(
      events = Vector(Event.factor(Vector("A"), "condition")),
      onsets = Vector(Seconds(1.0)),
      termTag = Some("tiny")
    )
    val convolved = term.convolve(
      hrf = tiny,
      samplingFrame = sampling,
      precision = Seconds(1.0),
      scaling = HrfColumnScaling.UnitMaximumAbsolute
    )
    val values = Vector.tabulate(convolved.data.rows)(row => convolved.data(row, 0))

    convolved.columnScales match
      case Vector(scale) => assertEqualsDouble(scale.divisor, 1e-12, 1e-24)
      case other         => fail(s"expected one exact column scale, found $other")
    assertEqualsDouble(values.map(math.abs).max, 1.0, 1e-12)
  }

  private val twoPhaseFormula =
    "sample_onset ~ " +
      "hrf(condition, onsets = sample_onset, phase = sample, parent = trial, id = sample) + " +
      "hrf(condition, onsets = probe_onset, phase = probe, parent = trial, id = probe)"

  private val onePhaseFormula =
    "probe_onset ~ hrf(condition, onsets = probe_onset, phase = probe, parent = trial, id = probe)"

  private val threePhaseFormula =
    "sample_onset ~ " +
      "hrf(condition, onsets = sample_onset, phase = sample, parent = trial, id = sample) + " +
      "hrf(condition, onsets = delay_onset, phase = delay, parent = trial, id = delay) + " +
      "hrf(condition, onsets = probe_onset, phase = probe, parent = trial, id = probe)"

  private val threePhaseFormulaPermuted =
    "sample_onset ~ " +
      "hrf(condition, onsets = probe_onset, phase = probe, parent = trial, id = probe) + " +
      "hrf(condition, onsets = sample_onset, phase = sample, parent = trial, id = sample) + " +
      "hrf(condition, onsets = delay_onset, phase = delay, parent = trial, id = delay)"

  private def twoConditionData(order: Vector[Int]): DataTable =
    val sample = Vector(1.0, 7.0, 13.0, 19.0)
    val probe = Vector(3.0, 9.0, 15.0, 21.0)
    val conditions = Vector("A", "B", "A", "B")
    DataTable.fromColumns(
      "sample_onset" -> Column.Doubles(order.map(sample)),
      "probe_onset" -> Column.Doubles(order.map(probe)),
      "condition" -> Column.Strings(order.map(conditions)),
      "trial" -> Column.Strings(order.map(index => s"trial-${index + 1}"))
    )

  private def threePhaseData(order: Vector[Int]): DataTable =
    val sample = Vector(1.0, 5.0, 9.0, 13.0, 17.0, 21.0)
    val delay = sample.map(_ + 1.0)
    val probe = sample.map(_ + 2.0)
    val conditions = Vector("A", "B", "C", "A", "B", "C")
    DataTable.fromColumns(
      "sample_onset" -> Column.Doubles(order.map(sample)),
      "delay_onset" -> Column.Doubles(order.map(delay)),
      "probe_onset" -> Column.Doubles(order.map(probe)),
      "condition" -> Column.Strings(order.map(conditions)),
      "trial" -> Column.Strings(order.map(index => s"trial-${index + 1}"))
    )

  private def customPair: Hrf =
    val elements = Vector(
      basisElement("custom-canonical", 1, BasisRole.Canonical, "custom canonical"),
      basisElement("custom-temporal", 2, BasisRole.TemporalDerivative, "custom temporal")
    )
    Hrf.multiWithBasisElements(
      name = "custom-pair",
      elements = elements,
      span = Seconds(4.0),
      support = Support.Compact(Seconds(4.0))
    ) { lag =>
      val decay = math.exp(-lag.value)
      Array(decay, lag.value * decay)
    }.fold(error => fail(error.message), identity)

  private def basisElement(id: String, index: Int, role: BasisRole, label: String): BasisElement =
    val parsed = BasisElementId.from(id).fold(error => fail(error.message), identity)
    BasisElement(parsed, index, role, label)

  private def build(
      formula: String,
      data: DataTable,
      options: EventModelBuilder.BuildOptions
  ): EventModel =
    EventModelBuilder.buildEither(requestFor(formula, data, options))
      .fold(error => fail(error.message), identity)

  private def requestFor(
      formula: String,
      data: DataTable,
      options: EventModelBuilder.BuildOptions
  ): EventModelBuilder.EventDesignRequest =
    EventModelBuilder.EventDesignRequest.fromText(
      formula = formula,
      data = data,
      samplingFrame = sampling,
      options = options
    ).fold(error => fail(error.message), identity)

  private def eventOrigins(model: EventModel): Vector[StructuralColumnOrigin.Event] =
    model.designSchema.columns.flatMap(_.origin match
      case event: StructuralColumnOrigin.Event => Some(event)
      case _                                   => None
    )

  private def cell(factor: String, level: String): CellKey =
    val factorId = FactorId(factor).fold(error => fail(error.message), identity)
    val levelId = LevelId(level).fold(error => fail(error.message), identity)
    CellKey.from(Vector(CellAssignment(factorId, levelId)))
      .fold(error => fail(error.message), identity)

  private def matrixColumn(model: EventModel, column: Int): Vector[Double] =
    Vector.tabulate(model.designMatrix.rows)(row => model.designMatrix(row, column))

  private def alignedColumns(model: EventModel): Map[String, Vector[Double]] =
    model.designSchema.columns.zipWithIndex.map { case (column, index) =>
      column.origin.canonical -> matrixColumn(model, index)
    }.toMap

  private def assertAlignedColumns(left: EventModel, right: EventModel): Unit =
    val expected = alignedColumns(left)
    val actual = alignedColumns(right)
    assertEquals(actual.keySet, expected.keySet)
    expected.foreach { case (key, values) =>
      actual(key).zip(values).foreach { case (observed, reference) =>
        assertEqualsDouble(observed, reference, 1e-12)
      }
    }
