package scalafim.fmri.fit

import scalafim.fmri.design.*
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

import scala.compiletime.testing.typeCheckErrors

/** The consumer surface stays semantic while compilation remains structural. */
class StructuralHypothesisDslSuite extends munit.FunSuite:

  private val probe = StructuralHypothesisDsl.term("probe").inPhase("probe")
  private val matchStatus = StructuralHypothesisDsl.factor("match")
  private val load = StructuralHypothesisDsl.factor("load")
  private val mismatchHigh = probe.cell(matchStatus === "mismatch", load === "high")
  private val matchHigh = probe.cell(matchStatus === "match", load === "high")

  test("cell and basis references compile without column bookkeeping") {
    val hypothesis =
      (mismatchHigh.coefficient(Hrfs.SPMG2, BasisRole.Canonical) -
        matchHigh.coefficient(Hrfs.SPMG2, BasisRole.Canonical))
        .named("probe-mismatch", "probe mismatch minus match")

    val compiled = hypothesis.compile(schema).toOption.getOrElse(fail("semantic hypothesis should compile"))

    assertEquals(compiled.selectedColumnIds.toSet, Set(schema.columns(0).id, schema.columns(2).id))
    assertEquals(compiled.estimability.decision, StructuralEstimabilityDecision.Estimable)
    assertEquals(compiled.source, HypothesisSource.Structural)
  }

  test("weighted semantic rows compose T trends and F omnibus hypotheses") {
    val canonicalMismatch = mismatchHigh.coefficient(Hrfs.SPMG2, BasisRole.Canonical)
    val canonicalMatch = matchHigh.coefficient(Hrfs.SPMG2, BasisRole.Canonical)
    val linear = canonicalMismatch * 2.5 - canonicalMatch * 0.75
    val quadratic = canonicalMismatch * 1.5 + canonicalMatch * 1.5
    val compiledT = linear
      .named("probe-weighted", "weighted semantic trend")
      .compile(schema)
      .fold(error => fail(error.message), identity)
    val compiledF = (linear.asOmnibusRow + quadratic.asOmnibusRow)
      .named("probe-weighted-omnibus", "joint weighted semantic trends")
      .compile(schema)
      .fold(error => fail(error.message), identity)

    assertEquals(compiledT.weights(0, 0), 2.5)
    assertEquals(compiledT.weights(0, 2), -0.75)
    assertEquals(compiledF.numeratorRank, 2)
    assertEquals(compiledF.selectedColumnIds.toSet, Set(schema.columns(0).id, schema.columns(2).id))
  }

  test("response functionals and modulators remain semantic") {
    val response =
      (mismatchHigh.modulatedBy("rt").response(Hrfs.SPMG2, ResponseFunctional.At(Seconds(6.0))) -
        matchHigh.response(Hrfs.SPMG2, ResponseFunctional.At(Seconds(6.0))))
        .named("probe-at-six", "RT-modulated mismatch response at six seconds")

    val compiled = response.compile(schema) match
      case Right(value) => value
      case Left(error)  => fail(s"response hypothesis should compile: ${error.message}")

    assertEquals(compiled.selectedColumnIds.toSet, Set(schema.columns(2).id, schema.columns(3).id, schema.columns(4).id, schema.columns(5).id))
    assertEquals(compiled.metadata.responseFunctionals.length, 2)
    assertEquals(compiled.estimability.decision, StructuralEstimabilityDecision.Estimable)
  }

  test("response functionals transport through explicit scan-space scaling") {
    val rawSchema = responseSchema(HrfColumnScaling.AsConvolved)
    val scaledSchema = responseSchema(HrfColumnScaling.UnitMaximumAbsolute)
    val probeMismatch = StructuralHypothesisDsl
      .term("probe")
      .inPhase("probe")
      .cell(StructuralHypothesisDsl.factor("match") === "mismatch")
    val functional = probeMismatch
      .response(Hrfs.SPMG2, ResponseFunctional.At(Seconds(6.0)))
      .named("probe-at-six-scaled", "probe response at six seconds")
    val raw = functional.compile(rawSchema).fold(error => fail(error.message), identity)
    val scaled = functional.compile(scaledSchema).fold(error => fail(error.message), identity)

    assertEquals(raw.weights.cols, scaled.weights.cols)
    val rawCoefficients = Vector(0.75, -0.25)
    val scaledCoefficients = scaledSchema.columns.zip(rawCoefficients).map { case (column, coefficient) =>
      column.hrfScale.divisor * coefficient
    }
    var rawResponse = 0.0
    var scaledResponse = 0.0
    var column = 0
    while column < raw.weights.cols do
      val divisor = scaledSchema.columns(column).hrfScale.divisor
      assertEqualsDouble(scaled.weights(0, column), raw.weights(0, column) / divisor, 1e-12)
      rawResponse += raw.weights(0, column) * rawCoefficients(column)
      scaledResponse += scaled.weights(0, column) * scaledCoefficients(column)
      column += 1
    assertEqualsDouble(scaledResponse, rawResponse, 1e-12)
    assert(scaledSchema.columns.exists(_.hrfScale.divisor != 1.0))

    val coefficient = probeMismatch
      .coefficient(Hrfs.SPMG2, BasisRole.TemporalDerivative)
      .named("probe-temporal-coordinate", "probe temporal coordinate")
    val rawCoefficient = coefficient.compile(rawSchema).fold(error => fail(error.message), identity)
    val scaledCoefficient = coefficient.compile(scaledSchema).fold(error => fail(error.message), identity)
    assertEquals(
      Vector.tabulate(scaledCoefficient.weights.cols)(column => scaledCoefficient.weights(0, column)),
      Vector.tabulate(rawCoefficient.weights.cols)(column => rawCoefficient.weights(0, column))
    )
  }

  test("basis omnibus supports an explicit non-canonical scope") {
    val shape =
      mismatchHigh
        .omnibus(Hrfs.SPMG2, StructuralHypothesisDsl.BasisScope.NonCanonical)
        .named("probe-shape", "probe non-canonical basis dimensions")

    val compiled = shape.compile(schema).toOption.getOrElse(fail("basis omnibus should compile"))

    assertEquals(compiled.numeratorRank, 1)
    assertEquals(compiled.selectedColumnIds.toSet, Set(schema.columns(1).id))
  }

  test("semantic omnibus blocks compose without exposing column rows") {
    val joint =
      (mismatchHigh.omnibus(Hrfs.SPMG2) + matchHigh.omnibus(Hrfs.SPMG2))
        .named("probe-joint", "joint mismatch and match basis effects")

    val compiled = joint.compile(schema).toOption.getOrElse(fail("joint semantic omnibus should compile"))

    assertEquals(compiled.numeratorRank, 4)
    assertEquals(compiled.selectedColumnIds.toSet, schema.columns.take(4).map(_.id).toSet)
  }

  test("an empty basis scope reports a typed selection failure") {
    val empty =
      mismatchHigh
        .omnibus(Hrfs.SPMG1, StructuralHypothesisDsl.BasisScope.NonCanonical)
        .named("empty-shape", "canonical basis has no shape dimensions")

    empty.compile(schema) match
      case Left(FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.EmptySelection, _)) => ()
      case other => fail(s"expected typed empty-selection failure, got $other")
  }

  test("a basis role absent from one heterogeneous cell is a typed basis failure") {
    val canonicalOnly = StructuralHypothesisDsl
      .term("probe")
      .inPhase("probe")
      .cell(StructuralHypothesisDsl.factor("condition") === "A")
    val temporal = canonicalOnly
      .coefficient(Hrfs.SPMG2, BasisRole.TemporalDerivative)
      .named("probe-a-temporal", "probe A temporal coordinate")

    temporal.compile(heterogeneousSchema) match
      case Left(FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.UnknownBasis, detail)) =>
        assert(detail.contains("cell:{condition=A}"))
      case other => fail(s"expected a typed cell-scoped basis failure, got $other")
  }

  test("only a named hypothesis exposes compile") {
    val errors = typeCheckErrors("""
      import scalafim.fmri.fit.StructuralHypothesisDsl.*
      import scalafim.fmri.hrf.{BasisRole, Hrfs}

      term("probe").cell().coefficient(Hrfs.SPMG1, BasisRole.Canonical).compile(null)
    """)

    assert(errors.exists(_.message.contains("compile")), errors.map(_.message).mkString("\n"))
  }

  test("validated domain ids remain first-class DSL inputs") {
    val typedTerm = StructuralHypothesisDsl
      .term(TermId.unsafe("probe"))
      .inPhase(PhaseId.unsafe("probe"))
    val typedFactor = StructuralHypothesisDsl.factor(FactorId.unsafe("match"))
    val typedCell = typedTerm
      .cell(typedFactor === scalafim.fmri.design.contrast.LevelId.unsafe("mismatch"))
      .modulatedBy(ModulatorId.unsafe("rt"))
    val typedSampled = StructuralHypothesisDsl.sampled(ModulatorId.unsafe("motion"))
    val typedHypothesis = typedCell
      .coefficient(Hrfs.SPMG2, BasisRole.Canonical)
      .named(ContrastId.unsafe("typed-probe"), "typed probe coefficient")

    assertEquals(typedTerm.id, TermId.unsafe("probe"))
    assertEquals(typedTerm.phaseId, Some(PhaseId.unsafe("probe")))
    assertEquals(typedFactor.id, FactorId.unsafe("match"))
    assertEquals(typedCell.modulator, ModulatorSelection.ById(ModulatorId.unsafe("rt")))
    assertEquals(typedSampled.regressor, ModulatorId.unsafe("motion"))
    assertEquals(typedHypothesis.id, ContrastId.unsafe("typed-probe"))
  }

  test("sampled regressors compose as semantic coefficients") {
    val x = StructuralColumn.fromOrigin(
      1,
      StructuralColumnOrigin.Sampled(ModulatorId.unsafe("x"), ColumnRole.Task, RunScope.Global),
      "x"
    ).fold(error => fail(error.message), identity)
    val z = StructuralColumn.fromOrigin(
      2,
      StructuralColumnOrigin.Sampled(ModulatorId.unsafe("z"), ColumnRole.Task, RunScope.Global),
      "z"
    ).fold(error => fail(error.message), identity)
    val intercept = StructuralColumn.fromOrigin(
      3,
      StructuralColumnOrigin.Intercept(RunScope.Global),
      "intercept"
    ).fold(error => fail(error.message), identity)
    val sampledRows = RowLayout(
      blockIds = Vector.fill(3)(RunIndex.unsafeOneBased(1)),
      acquisitionTimes = Vector(Seconds(0.0), Seconds(1.0), Seconds(2.0)),
      selectedRows = Vector(1, 2, 3).map(ScanIndex.unsafeOneBased)
    )
    val sampledSchema = DesignSchema.validated(
      Mat.unsafe(3, 3, Array(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0)),
      sampledRows,
      Vector(x, z, intercept)
    ).fold(error => fail(error.message), identity)
    val hypothesis =
      (StructuralHypothesisDsl.sampled("x").coefficient -
        StructuralHypothesisDsl.sampled("z").coefficient)
        .named("x-minus-z", "sampled x minus z")
    val compiled = hypothesis.compile(sampledSchema).fold(error => fail(error.message), identity)

    assertEquals(compiled.selectedColumnIds, Vector(x.id, z.id))
    assertEquals(
      Vector.tabulate(compiled.weights.cols)(column => compiled.weights(0, column)),
      Vector(1.0, -1.0, 0.0)
    )
  }

  private lazy val schema: DesignSchema =
    val basis = Hrfs.SPMG2.basisElementsValidated.toOption.get
    val columns =
      Vector(
        eventColumn(1, mismatchHigh.cell, basis(0)),
        eventColumn(2, mismatchHigh.cell, basis(1)),
        eventColumn(3, matchHigh.cell, basis(0)),
        eventColumn(4, matchHigh.cell, basis(1)),
        eventColumn(5, mismatchHigh.cell, basis(0), Some(ModulatorId.unsafe("rt"))),
        eventColumn(6, mismatchHigh.cell, basis(1), Some(ModulatorId.unsafe("rt"))),
        StructuralColumn.fromOrigin(
          7,
          StructuralColumnOrigin.Intercept(RunScope.Global),
          "intercept"
        ).toOption.get
      )
    val identity = Array.fill(49)(0.0)
    var i = 0
    while i < 7 do
      identity(i * 7 + i) = 1.0
      i += 1
    DesignSchema.validated(Mat.unsafe(7, 7, identity), rows, columns).toOption.get

  private def responseSchema(scaling: HrfColumnScaling): DesignSchema =
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 7.0, 13.0, 19.0)),
      "match" -> Column.Strings(Vector.fill(4)("mismatch")),
      "trial" -> Column.Strings(Vector.tabulate(4)(index => s"trial-${index + 1}"))
    )
    val levels = FactorLevelRegistry.of("match" -> Seq("mismatch"))
      .fold(error => fail(error.message), identity)
    val scalingValue = scaling.label.replace('-', '_')
    buildSchema(
      formula =
        "onset ~ hrf(match, basis = spmg2, phase = probe, parent = trial, id = probe, " +
          s"scaling = $scalingValue)",
      data = data,
      options = EventModelBuilder.BuildOptions(
        precision = Seconds(1.0),
        factorLevels = levels
      )
    )

  private lazy val heterogeneousSchema: DesignSchema =
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 7.0, 13.0, 19.0)),
      "condition" -> Column.Strings(Vector("A", "B", "A", "B")),
      "trial" -> Column.Strings(Vector.tabulate(4)(index => s"trial-${index + 1}"))
    )
    val levels = FactorLevelRegistry.of("condition" -> Seq("A", "B"))
      .fold(error => fail(error.message), identity)
    val cells = HrfByCell.oneFactor(
      "condition",
      "A" -> Hrfs.SPMG1,
      "B" -> Hrfs.SPMG2
    ).fold(error => fail(error.message), identity)
    val phases = HrfByPhase.named(
      "probe" -> HrfAssignment.ByCell(cells)
    ).fold(error => fail(error.message), identity)
    buildSchema(
      formula = "onset ~ hrf(condition, phase = probe, parent = trial, id = probe)",
      data = data,
      options = EventModelBuilder.BuildOptions(
        precision = Seconds(1.0),
        factorLevels = levels,
        hrfByPhase = Some(phases)
      )
    )

  private def buildSchema(
      formula: String,
      data: DataTable,
      options: EventModelBuilder.BuildOptions
  ): DesignSchema =
    val request = EventModelBuilder.EventDesignRequest.fromText(
      formula = formula,
      data = data,
      samplingFrame = SamplingFrame(blockLens = Seq(28), tr = Seq(1.0), startTime = Seq(0.0)),
      options = options
    ).fold(error => fail(error.message), identity)
    EventModelBuilder.buildEither(request).fold(error => fail(error.message), _.designSchema)

  private def eventColumn(
      ordinal: Int,
      cell: CellKey,
      element: BasisElement,
      modulator: Option[ModulatorId] = None
  ): StructuralColumn =
    StructuralColumn.fromOrigin(
      ordinal,
      StructuralColumnOrigin.Event(
        term = TermId.unsafe("probe"),
        phase = Some(PhaseId.unsafe("probe")),
        cell = cell,
        modulator = modulator,
        basis = Some(
          BasisElementRef(
            basisId = Hrfs.SPMG2.name,
            index = BasisIndex.unsafeOneBased(element.index),
            role = Some(element.role),
            elementId = Some(element.id)
          )
        ),
        role = ColumnRole.Task,
        runScope = RunScope.Global
      ),
      s"probe_${cell.canonical}_${modulator.fold("main")(_.value)}_${element.label}"
    ).toOption.get

  private val rows =
    RowLayout(
      blockIds = Vector.fill(7)(RunIndex.unsafeOneBased(1)),
      acquisitionTimes = (0 until 7).toVector.map(i => Seconds(i.toDouble)),
      selectedRows = (1 to 7).toVector.map(ScanIndex.unsafeOneBased)
    )
