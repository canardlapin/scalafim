package scalafim.fmri.fit

import scalafim.fmri.design.*
import scalafim.fmri.design.contrast.LevelId
import scalafim.fmri.hrf.{BasisRole, Seconds}
import scalafim.fmri.hrf.{ResponseFunctional, ResponseUnits}
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitEngine, FitSummary}
import gale.linalg.DVec

/** Structural hypotheses are compiled against provenance, not rendered names
  * or a caller-maintained coefficient offset table.  These tests intentionally
  * use hand-built schemas so they exercise the public schema/hypothesis seam
  * independently of any particular design compiler.
  */
class StructuralHypothesisSuite extends munit.FunSuite:

  private val condition = FactorId.unsafe("condition")
  private val term = TermId.unsafe("task")
  private val phase = PhaseId.unsafe("sample")
  private val canonical = BasisElementRef(
    basisId = "spm",
    index = BasisIndex.unsafeOneBased(1),
    role = Some(BasisRole.Canonical)
  )
  private val temporal = BasisElementRef(
    basisId = "spm",
    index = BasisIndex.unsafeOneBased(2),
    role = Some(BasisRole.TemporalDerivative)
  )
  private val cellA = CellKey.unsafe(Seq(CellAssignment(condition, LevelId.unsafe("A"))))
  private val cellB = CellKey.unsafe(Seq(CellAssignment(condition, LevelId.unsafe("B"))))

  test("semantic selectors resolve to structural identities and carry estimability evidence") {
    val schema = fullRankSchema()
    val hypothesis = StructuralTContrast(
      id = ContrastId.unsafe("A-minus-B"),
      description = "condition A minus condition B",
      terms = Vector(
        StructuralWeight(StructuralColumnSelector.event(term = Some(term), cell = Some(cellA), basis = Some(canonical)), 1.0),
        StructuralWeight(StructuralColumnSelector.event(term = Some(term), cell = Some(cellB), basis = Some(canonical)), -1.0)
      )
    )

    val compiled = hypothesis.compile(schema).toOption.getOrElse(fail("semantic contrast should compile"))

    assertEquals(compiled.designFingerprint, schema.fingerprint)
    assertEquals(compiled.selectedColumnIds.toSet, Set(schema.columns(0).id, schema.columns(1).id))
    assertEquals(compiled.estimability.decision, StructuralEstimabilityDecision.Estimable)
    assert(compiled.estimability.rowSpacePreserved)
    assertEquals(compiled.estimability.designRank, 3)
    assertEquals(compiled.estimability.contrastRank, 1)
    assertEquals(compiled.estimability.augmentedRank, 3)
    assertEquals(compiled.source, HypothesisSource.Structural)
  }

  test("hypothesis estimability uses the same scale-aware convention as fitting") {
    val ordinary = fullRankSchema()
    val tiny = DesignSchema.validated(
      Mat.unsafe(ordinary.matrix.rows, ordinary.matrix.cols, ordinary.matrix.data.map(_ * 1.0e-12)),
      ordinary.rows,
      ordinary.columns
    ).toOption.getOrElse(fail("uniformly scaled structural schema should compile"))
    val hypothesis = StructuralTContrast(
      id = ContrastId.unsafe("tiny-A-minus-B"),
      description = "condition A minus condition B on a uniformly tiny design",
      terms = Vector(
        StructuralWeight(StructuralColumnSelector.event(term = Some(term), cell = Some(cellA), basis = Some(canonical)), 1.0),
        StructuralWeight(StructuralColumnSelector.event(term = Some(term), cell = Some(cellB), basis = Some(canonical)), -1.0)
      )
    )

    val compiled = hypothesis.compile(tiny).toOption.getOrElse(fail("scale-aware hypothesis should remain estimable"))
    assertEquals(
      compiled.estimability.designRank,
      tiny.rankPreview.evidence.map(_.numericalRank).getOrElse(fail("finite tiny design should have rank evidence"))
    )
    assertEquals(compiled.estimability.toleranceConvention, RankToleranceConvention.ScaleAware)
    assert(compiled.estimability.designTolerance < 1.0e-20)
    assert(compiled.estimability.augmentedTolerance > compiled.estimability.designTolerance)

    val absolute = hypothesis.compile(tiny, OlsRankTolerance.Absolute(1.0e-7))
    assert(absolute.left.toOption.exists {
      case FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.NonEstimable, _) => true
      case _ => false
    })
  }

  test("semantic resolution is invariant to rendered labels and coefficient order") {
    val original = fullRankSchema()
    val relabelled = original.withRenderedLabels(Vector("human-A", "human-B", "opaque-nuisance")).toOption.get
    val reordered = reorderedSchema()
    val hypothesis = StructuralTContrast(
      id = ContrastId.unsafe("A-minus-B"),
      description = "condition A minus condition B",
      terms = Vector(
        StructuralWeight(StructuralColumnSelector.event(term = Some(term), cell = Some(cellA), basis = Some(canonical)), 1.0),
        StructuralWeight(StructuralColumnSelector.event(term = Some(term), cell = Some(cellB), basis = Some(canonical)), -1.0)
      )
    )

    val first = hypothesis.compile(original).toOption.getOrElse(fail("original contrast should compile"))
    val relabelledAxis = relabelled.coefficientAxis
    assertEquals(StructuralHypothesis.validateResultAxis(first.id, first.coefficientAxis, Some(relabelledAxis)), Right(()))

    val second = hypothesis.compile(reordered).toOption.getOrElse(fail("reordered contrast should compile"))
    assertEquals(second.selectedColumnIds.toSet, first.selectedColumnIds.toSet)
    assertEquals(second.weights.rows, first.weights.rows)
    assertEquals(second.weights.cols, first.weights.cols)
    // The coefficient order changed, so the dense rows change sign positions;
    // the structural selection and semantic values do not.
    assertEquals(second.weights(0, 0), -1.0)
    assertEquals(second.weights(0, 1), 1.0)
    assertEquals(second.weights(0, 2), 0.0)

    val firstResult = semanticResult(original, Vector(2.0, 1.0, 0.5))
    val secondResult = semanticResult(reordered, Vector(1.0, 2.0, 0.5))
    val firstEvaluation = first.evaluate(firstResult).toOption.getOrElse(fail("original semantic evaluation should succeed"))
    val secondEvaluation = second.evaluate(secondResult).toOption.getOrElse(fail("reordered semantic evaluation should succeed"))
    assertEqualsDouble(firstEvaluation.estimates(0), secondEvaluation.estimates(0), 1e-12)
    assertEqualsDouble(firstEvaluation.standardErrors(0), secondEvaluation.standardErrors(0), 1e-12)
    assertEqualsDouble(firstEvaluation.statistics(0), secondEvaluation.statistics(0), 1e-12)
    assert(firstEvaluation.hypothesis.nonEmpty)
    assert(secondEvaluation.hypothesis.nonEmpty)
  }

  test("structural identity permits repeated rendered labels across run-scoped regressors") {
    val schema = fullRankSchema()
      .withRenderedLabels(Vector("task", "task", "nuisance"))
      .toOption
      .getOrElse(fail("duplicate display labels should not invalidate a structural schema"))
    val hypothesis = StructuralTContrast(
      id = ContrastId.unsafe("structural-a-minus-b"),
      description = "condition A minus condition B",
      terms = Vector(
        StructuralWeight(StructuralColumnSelector.event(term = Some(term), cell = Some(cellA), basis = Some(canonical)), 1.0),
        StructuralWeight(StructuralColumnSelector.event(term = Some(term), cell = Some(cellB), basis = Some(canonical)), -1.0)
      )
    )

    val compiled = hypothesis.compile(schema).toOption.getOrElse(fail("structural contrast should compile"))
    val evaluated = compiled.evaluate(semanticResult(schema, Vector(2.0, 1.0, 0.5)))

    assertEqualsDouble(
      evaluated.toOption.map(_.estimates(0)).getOrElse(fail("structural contrast should evaluate")),
      1.0,
      1e-12
    )
  }

  test("a compiled hypothesis rejects a result from a different design fingerprint") {
    val schema = fullRankSchema()
    val changed = DesignSchema.validated(
      Mat.unsafe(6, 3, Array(
        1.0, 0.0, 0.0,
        0.0, 1.0, 1.0,
        1.0, 0.0, 0.0,
        0.0, 1.0, 3.0,
        1.0, 0.0, 4.0,
        0.0, 1.0, 5.0
      )),
      schema.rows,
      schema.columns
    ).toOption.get
    val hypothesis = StructuralTContrast.fromColumn(
      ContrastId.unsafe("condition-A"),
      "condition A",
      schema.columns(0).id
    ).compile(schema).toOption.get

    val mismatch = StructuralHypothesis.validateResultAxis(
      hypothesis.id,
      hypothesis.coefficientAxis,
      Some(changed.coefficientAxis)
    )

    mismatch match
      case Left(FitError.HypothesisDesignMismatch(name, expected, actual)) =>
        assertEquals(name, "condition-A")
        assertEquals(expected, schema.fingerprint.value)
        assertEquals(actual, changed.fingerprint.value)
      case other => fail(s"expected a typed design mismatch, got $other")
  }

  test("unknown structural components produce typed failures") {
    val schema = fullRankSchema()
    val unknownFactor = CellKey.unsafe(Seq(CellAssignment(FactorId.unsafe("missing-factor"), LevelId.unsafe("A"))))
    val hypothesis = StructuralTContrast(
      ContrastId.unsafe("unknown-factor"),
      "unknown factor",
      Vector(StructuralWeight(StructuralColumnSelector.event(term = Some(term), cell = Some(unknownFactor)), 1.0))
    )

    hypothesis.compile(schema) match
      case Left(FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.UnknownFactor, _)) => ()
      case other => fail(s"expected an unknown-factor failure, got $other")

    val unknownBasis = StructuralTContrast(
      ContrastId.unsafe("unknown-basis"),
      "unknown basis",
      Vector(StructuralWeight(
        StructuralColumnSelector.event(term = Some(term), cell = Some(cellA), basis = Some(
          BasisElementRef("spm", BasisIndex.unsafeOneBased(2), Some(BasisRole.TemporalDerivative))
        )),
        1.0
      ))
    )
    unknownBasis.compile(schema) match
      case Left(FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.UnknownBasis, _)) => ()
      case other => fail(s"expected an unknown-basis failure, got $other")
  }

  test("declared-but-empty cells are distinguished from unknown levels") {
    val empty = CellKey.unsafe(Seq(CellAssignment(condition, LevelId.unsafe("C"))))
    val schema = DesignSchema.validated(
      Mat.unsafe(6, 3, Array(
        1.0, 0.0, 0.0,
        0.0, 1.0, 1.0,
        1.0, 0.0, 2.0,
        0.0, 1.0, 3.0,
        1.0, 0.0, 4.0,
        0.0, 1.0, 5.0
      )),
      rows(),
      eventColumns(),
      DesignAudit(
        emptyCells = Vector(empty),
        factorLevels = Vector(
          FactorLevelAudit(
            factor = condition,
            declared = Some(Vector(LevelId.unsafe("A"), LevelId.unsafe("B"), LevelId.unsafe("C"))),
            observed = Vector(LevelId.unsafe("A"), LevelId.unsafe("B"))
          )
        )
      )
    ).toOption.get

    val declaredEmpty = StructuralTContrast(
      ContrastId.unsafe("declared-empty"),
      "declared but absent condition C",
      Vector(StructuralWeight(StructuralColumnSelector.event(term = Some(term), cell = Some(empty)), 1.0))
    )
    declaredEmpty.compile(schema) match
      case Left(FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.DeclaredEmpty, detail)) =>
        assert(detail.contains("condition=C"))
      case other => fail(s"expected a declared-empty failure, got $other")

    val unknown = CellKey.unsafe(Seq(CellAssignment(condition, LevelId.unsafe("D"))))
    val unknownHypothesis = StructuralTContrast(
      ContrastId.unsafe("unknown-level"),
      "unknown condition D",
      Vector(StructuralWeight(StructuralColumnSelector.event(term = Some(term), cell = Some(unknown)), 1.0))
    )
    unknownHypothesis.compile(schema) match
      case Left(FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.UnknownLevel, _)) => ()
      case other => fail(s"expected an unknown-level failure, got $other")
  }

  test("invalid structural weights are reported during compilation") {
    val schema = fullRankSchema()
    val hypothesis = StructuralTContrast(
      ContrastId.unsafe("zero-weight"),
      "zero weight",
      Vector(StructuralWeight(StructuralColumnSelector.Id(schema.columns(0).id), 0.0))
    )
    hypothesis.compile(schema) match
      case Left(FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.InvalidWeight, _)) => ()
      case other => fail(s"expected an invalid-weight failure, got $other")
  }

  test("rendered labels cannot become structural authority") {
    val schema = fullRankSchema()
    val structural = StructuralTContrast(
      ContrastId.unsafe("label-as-identity"),
      "label as identity",
      Vector(StructuralWeight(StructuralColumnSelector.RenderedLabel("rendered-A"), 1.0))
    )
    structural.compile(schema) match
      case Left(FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.IncompatibleDesign, _)) => ()
      case other => fail(s"expected structural label rejection, got $other")

    val compatibility = StructuralTContrast.fromNameKeyed(TContrast("legacy", Map("rendered-A" -> 1.0)))
    assert(compatibility.compile(schema).isRight)
  }

  test("rank-deficient designs reject non-estimable T contrasts and reduce F numerator rank") {
    val reducedF = StructuralFContrast.fromRows(
      ContrastId.unsafe("duplicate-row-omnibus"),
      "duplicate supplied rows",
      Vector(
        Map(fullRankSchema().columns(0).id -> 1.0),
        Map(fullRankSchema().columns(0).id -> 2.0)
      )
    ).compile(fullRankSchema()).toOption.getOrElse(fail("dependent supplied F rows should be reduced"))
    assertEquals(reducedF.numeratorRank, 1)
    assertEquals(reducedF.weights.cols, 1)

    val schema = rankDeficientSchema()
    val estimableSum = StructuralTContrast(
      ContrastId.unsafe("duplicate-cell-sum"),
      "sum of duplicate cell columns",
      Vector(
        StructuralWeight(StructuralColumnSelector.Id(schema.columns(0).id), 1.0),
        StructuralWeight(StructuralColumnSelector.Id(schema.columns(1).id), 1.0)
      )
    ).compile(schema).toOption.getOrElse(fail("the duplicated-column sum is an estimable row-space function"))
    assertEquals(estimableSum.estimability.designRank, 2)
    assertEquals(estimableSum.estimability.augmentedRank, 2)

    val nonEstimable = StructuralTContrast.fromColumn(
      ContrastId.unsafe("duplicate-B"),
      "duplicate B column",
      schema.columns(1).id
    )
    nonEstimable.compile(schema) match
      case Left(FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.NonEstimable, _)) => ()
      case other => fail(s"expected a non-estimable contrast, got $other")

    val f = StructuralFContrast.fromRows(
      ContrastId.unsafe("duplicate-cell-omnibus"),
      "duplicate cell omnibus",
      Vector(
        Map(schema.columns(0).id -> 1.0),
        Map(schema.columns(1).id -> 1.0)
      )
    )
    f.compile(schema) match
      case Left(FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.NonEstimable, detail)) =>
        assert(detail.contains("design rank=2"))
      case other => fail(s"expected the F hypothesis to carry non-estimability evidence, got $other")
  }

  test("response-level functionals expand over semantic basis elements") {
    val schema = responseSchema()
    val selector = StructuralColumnSelector.event(term = Some(term), phase = Some(phase), cell = Some(cellA))
    val hypothesis = StructuralTContrast.fromResponseFunctional(
      id = ContrastId.unsafe("A-response-at-six"),
      description = "condition A reconstructed response at six seconds",
      selector = selector,
      functional = ResponseFunctional.At(Seconds(6.0)),
      units = ResponseUnits.ResponseValue,
      basisWeights = Vector(
        BasisFunctionalWeight(canonical, 0.75),
        BasisFunctionalWeight(temporal, -0.25)
      )
    )

    val compiled = hypothesis.compile(schema).toOption.getOrElse(fail("response functional should compile"))
    assertEquals(compiled.metadata.responseFunctionals.map(_.units), Vector(ResponseUnits.ResponseValue))
    assertEquals(compiled.selectedColumnIds.toSet, Set(schema.columns(0).id, schema.columns(1).id))
    assertEquals(compiled.weights(0, 0), 0.75)
    assertEquals(compiled.weights(0, 1), -0.25)
    assertEquals(compiled.weights(0, 2), 0.0)
    assertEquals(compiled.estimability.decision, StructuralEstimabilityDecision.Estimable)
  }

  test("reduced fixed-effects axes reject missing T, F, and response-functional components") {
    def assertMissing(result: Either[FitError, ?], column: ColumnId): Unit =
      result match
        case Left(FitError.StructuralHypothesisFailure(_, StructuralHypothesisErrorKind.NonEstimable, detail)) =>
          assert(detail.contains(column.value), s"missing-column diagnostic did not name ${column.value}: $detail")
        case other => fail(s"expected a typed missing-component failure, got $other")

    val schema = fullRankSchema()
    val t = StructuralTContrast(
      ContrastId.unsafe("reduced-axis-t"),
      "A minus B requires both shared cells",
      Vector(
        StructuralWeight(StructuralColumnSelector.Id(schema.columns(0).id), 1.0),
        StructuralWeight(StructuralColumnSelector.Id(schema.columns(1).id), -1.0)
      )
    ).compile(schema).toOption.getOrElse(fail("T hypothesis should compile on the full design"))
    val f = StructuralFContrast.fromRows(
      ContrastId.unsafe("reduced-axis-f"),
      "A and B omnibus requires both shared cells",
      Vector(
        Map(schema.columns(0).id -> 1.0),
        Map(schema.columns(1).id -> 1.0)
      )
    ).compile(schema).toOption.getOrElse(fail("F hypothesis should compile on the full design"))
    val reduced = schema.coefficientAxis.select(Vector(0, 2)).toOption.getOrElse(fail("reduced axis should compile"))

    assertMissing(
      StructuralHypothesis.alignTForResult(t.id, t.coefficientAxis, Some(reduced), t.weights),
      schema.columns(1).id
    )
    assertMissing(
      StructuralHypothesis.alignFForResult(f.id, f.coefficientAxis, Some(reduced), f.weights, f.numeratorRank),
      schema.columns(1).id
    )
    val selectedOnly = schema.coefficientAxis.select(Vector(0, 1)).toOption.getOrElse(fail("selected-only axis should compile"))
    assert(StructuralHypothesis.alignTForResult(t.id, t.coefficientAxis, Some(selectedOnly), t.weights).isRight)
    val projectedF = StructuralHypothesis
      .alignFForResult(f.id, f.coefficientAxis, Some(selectedOnly), f.weights, f.numeratorRank)
      .fold(error => fail(error.message), identity)
    assertEquals(projectedF.weights.rows, selectedOnly.predictors)
    assertEquals(projectedF.weights.cols, f.numeratorRank)
    assertEquals(projectedF.weights(0, 0), 1.0)
    assertEquals(projectedF.weights(1, 1), 1.0)

    val responseDesign = responseSchema()
    val response = StructuralTContrast.fromResponseFunctional(
      id = ContrastId.unsafe("reduced-axis-response"),
      description = "condition A response requires canonical and temporal basis elements",
      selector = StructuralColumnSelector.event(term = Some(term), phase = Some(phase), cell = Some(cellA)),
      functional = ResponseFunctional.At(Seconds(6.0)),
      units = ResponseUnits.ResponseValue,
      basisWeights = Vector(
        BasisFunctionalWeight(canonical, 0.75),
        BasisFunctionalWeight(temporal, -0.25)
      )
    ).compile(responseDesign).toOption.getOrElse(fail("response hypothesis should compile on the full design"))
    val responseReduced = responseDesign.coefficientAxis
      .select(Vector(0, 2, 3, 4))
      .toOption
      .getOrElse(fail("response reduced axis should compile"))
    assertMissing(
      StructuralHypothesis.alignTForResult(response.id, response.coefficientAxis, Some(responseReduced), response.weights),
      responseDesign.columns(1).id
    )
  }

  private def fullRankSchema(): DesignSchema =
    val columns = eventColumns()
    DesignSchema.validated(
      Mat.unsafe(6, 3, Array(
        1.0, 0.0, 0.0,
        0.0, 1.0, 1.0,
        1.0, 0.0, 2.0,
        0.0, 1.0, 3.0,
        1.0, 0.0, 4.0,
        0.0, 1.0, 5.0
      )),
      rows(),
      columns
    ).toOption.get

  private def rankDeficientSchema(): DesignSchema =
    DesignSchema.validated(
      Mat.unsafe(6, 3, Array(
        1.0, 1.0, 0.0,
        0.0, 0.0, 1.0,
        1.0, 1.0, 2.0,
        0.0, 0.0, 3.0,
        1.0, 1.0, 4.0,
        0.0, 0.0, 5.0
      )),
      rows(),
      eventColumns()
    ).toOption.get

  private def reorderedSchema(): DesignSchema =
    val columns = eventColumns()
    val reorderedColumns = Vector(columns(1), columns(0), columns(2)).zipWithIndex.map { case (column, index) =>
      column.copy(ordinal = DesignColumnIndex.unsafeOneBased(index + 1))
    }
    DesignSchema.validated(
      Mat.unsafe(6, 3, Array(
        0.0, 1.0, 0.0,
        1.0, 0.0, 1.0,
        0.0, 1.0, 2.0,
        1.0, 0.0, 3.0,
        0.0, 1.0, 4.0,
        1.0, 0.0, 5.0
      )),
      rows(),
      reorderedColumns
    ).toOption.get

  private def eventColumns(): Vector[StructuralColumn] =
    Vector(
      StructuralColumn.fromOrigin(
        1,
        StructuralColumnOrigin.Event(term, Some(phase), cellA, None, Some(canonical), ColumnRole.Task, RunScope.Global),
        "rendered-A"
      ).toOption.get,
      StructuralColumn.fromOrigin(
        2,
        StructuralColumnOrigin.Event(term, Some(phase), cellB, None, Some(canonical), ColumnRole.Task, RunScope.Global),
        "rendered-B"
      ).toOption.get,
      StructuralColumn.fromOrigin(
        3,
        StructuralColumnOrigin.Nuisance(TermId.unsafe("nuisance"), ModulatorId.unsafe("drift"), RunScope.Global),
        "rendered-nuisance"
      ).toOption.get
    )

  private def responseSchema(): DesignSchema =
    val columns = Vector(
      StructuralColumn.fromOrigin(
        1,
        StructuralColumnOrigin.Event(term, Some(phase), cellA, None, Some(canonical), ColumnRole.Task, RunScope.Global),
        "A_canonical"
      ).toOption.get,
      StructuralColumn.fromOrigin(
        2,
        StructuralColumnOrigin.Event(term, Some(phase), cellA, None, Some(temporal), ColumnRole.Task, RunScope.Global),
        "A_temporal"
      ).toOption.get,
      StructuralColumn.fromOrigin(
        3,
        StructuralColumnOrigin.Event(term, Some(phase), cellB, None, Some(canonical), ColumnRole.Task, RunScope.Global),
        "B_canonical"
      ).toOption.get,
      StructuralColumn.fromOrigin(
        4,
        StructuralColumnOrigin.Event(term, Some(phase), cellB, None, Some(temporal), ColumnRole.Task, RunScope.Global),
        "B_temporal"
      ).toOption.get,
      StructuralColumn.fromOrigin(
        5,
        StructuralColumnOrigin.Intercept(RunScope.Global),
        "intercept"
      ).toOption.get
    )
    val identity = Array.fill(25)(0.0)
    var i = 0
    while i < 5 do
      identity(i * 5 + i) = 1.0
      i += 1
    DesignSchema.validated(Mat.unsafe(5, 5, identity), rows().copy(
      blockIds = Vector.fill(5)(RunIndex.unsafeOneBased(1)),
      acquisitionTimes = Vector(0, 1, 2, 3, 4).map(Seconds(_)),
      selectedRows = (1 to 5).toVector.map(ScanIndex.unsafeOneBased)
    ), columns).toOption.get

  private def rows(): RowLayout =
    RowLayout(
      blockIds = Vector.fill(6)(RunIndex.unsafeOneBased(1)),
      acquisitionTimes = Vector(0, 1, 2, 3, 4, 5).map(Seconds(_)),
      selectedRows = (1 to 6).toVector.map(ScanIndex.unsafeOneBased)
    )

  private def semanticResult(schema: DesignSchema, coefficients: Vector[Double]): DenseFmriFitResult =
    val covariance = CoefficientCovariance.unsafeShared(
      GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 1.0)
        )
      )
    )
    val residualVariance = DVec.fromSeq(Vector(1.0))
    val residualDegreesOfFreedom = ResidualDegreesOfFreedom.unsafe(3)
    DenseFmriFitResult(
      coefficients = CoefficientBlock(GaleTestMatrix.fromRows(coefficients.map(value => Vector(value)))),
      inference = CoefficientInference.unsafeFromExisting(
        CoefficientInferenceScope.All,
        StandardErrorBlock(GaleTestMatrix.fromRows(Vector.fill(3)(Vector(1.0)))),
        covariance,
        residualVariance,
        residualDegreesOfFreedom
      ),
      residualVariance = residualVariance,
      residualDegreesOfFreedom = residualDegreesOfFreedom,
      columnNames = schema.columnNames,
      voxelIndices = Vector(0),
      timepoints = (0 until 6).toVector,
      engine = FitEngine.OrdinaryLeastSquares,
      summary = FitSummary(
        engine = FitEngine.OrdinaryLeastSquares,
        timepoints = 6,
        predictors = 3,
        voxels = 1,
        robust = false,
        autocorrelated = false
      ),
      coefficientAxis = Some(schema.coefficientAxis)
    )
