package scalafim.estimates

import scalafim.image.SampleSpaces

class InferenceEvidenceSuite extends munit.FunSuite:
  private val dataset = DatasetId("00000000-0000-4000-8000-000000000111")
  private val observation = Observation(ObservationId("row"), ParticipantId(dataset, "01"), Vector(AcquisitionId("run")))
  private val columns = Vector(ColumnId("task"), ColumnId("nuisance"))
  private val unknown = ScientificFact.Unknown("learned response subspace not retained")
  private val coefficient = CoefficientInferenceEvidence(observation.id, columns, Vector(columns.head), "task only",
    ScientificFact.Known("bootstrap draws=17 seed=23"), unknown)
  private val a = EstimandId("A")
  private val h = EstimandId("H")
  private val effect = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
    Vector(observation.id), ProductTargets.Scalar(Vector(a)), PoolingScope.Run, "signal")
  private val statistic = effect.copy(id = ProductId("t"), kind = ProductKind.Statistic(StatisticKind.T),
    targets = ProductTargets.Scalar(Vector(h)), units = "dimensionless")
  private val planes = Vector(InferenceStatusScope.Fit(observation.id), InferenceStatusScope.Hypothesis(observation.id, h))
  private val evidence = InferenceEvidence(Vector(coefficient), planes)
  private def unit = EstimateUnit(dataset, UnitId("00000000-0000-4000-8000-000000000112"),
    UnitRevisionId("00000000-0000-4000-8000-000000000113"),
    EstimandCatalog(ModelRevisionId("00000000-0000-4000-8000-000000000114"), Vector(
      EstimandDefinition(a, "A", EstimandKind.Coefficient, "signal", "unit", "coefficient"),
      EstimandDefinition(h, "H", EstimandKind.Hypothesis, "dimensionless", "none", "signed hypothesis"))),
    EstimateDomain.make(SampleSpaces(Vector(3, 1, 1)), Vector(0, 2), "scanner").toOption.get,
    Vector(observation), Vector(EstimandBinding(a, columns, 1, Vector(1.0, 0.0))), Vector(effect, statistic),
    Map(effect.id -> ProductOutcome.Available(effect.id), statistic.id -> ProductOutcome.Available(statistic.id)),
    EstimabilityEvidence.Unknown("no machine-checkable subspace"),
    EstimateProvenance("fixture", "1", "run", unknown, unknown, unknown, unknown, Vector.empty, Vector.empty),
    statistics = Vector(StatisticSemantics(statistic.id, ReferenceDistribution.Unknown("no calibrated law"), None, None, None, None)),
    inferenceEvidence = Some(evidence))

  test("restricted coefficient scope preserves explicit order and unavailable conditioning"):
    assertEquals(unit.inferenceEvidence, Some(evidence))
    assertEquals(unit.estimability, EstimabilityEvidence.Unknown("no machine-checkable subspace"))
    assertEquals(coefficient.copy(inferableColumns = columns.reverse).inferableColumns, columns.reverse)
    assertEquals(coefficient.copy(inferableColumns = Vector.empty).inferableColumns, Vector.empty)
    intercept[IllegalArgumentException](coefficient.copy(columns = Vector.empty))
    intercept[IllegalArgumentException](coefficient.copy(columns = Vector(columns.head, columns.head)))
    intercept[IllegalArgumentException](coefficient.copy(inferableColumns = Vector(ColumnId("absent"))))
    intercept[IllegalArgumentException](coefficient.copy(inferableColumns = Vector(columns.head, columns.head)))
    intercept[IllegalArgumentException](coefficient.copy(method = ScientificFact.Unknown("")))
    intercept[IllegalArgumentException](unit.copy(bindings = Vector.empty))
    intercept[IllegalArgumentException](unit.copy(inferenceEvidence = Some(evidence.copy(coefficients = Vector(coefficient.copy(columns = columns.reverse))))))

  test("planes require known observation and catalog hypothesis with statistic semantics"):
    intercept[IllegalArgumentException](evidence.copy(planes = Vector.empty))
    intercept[IllegalArgumentException](evidence.copy(planes = Vector(planes.head, planes.head)))
    intercept[IllegalArgumentException](unit.copy(inferenceEvidence = Some(evidence.copy(planes = Vector(InferenceStatusScope.Fit(ObservationId("absent")))))))
    intercept[IllegalArgumentException](unit.copy(inferenceEvidence = Some(evidence.copy(planes = Vector(InferenceStatusScope.Hypothesis(observation.id, a))))))
    val effectOnly = unit.copy(products = Vector(effect), outcomes = Map(effect.id -> ProductOutcome.Available(effect.id)),
      statistics = Vector.empty, inferenceEvidence = None)
    intercept[IllegalArgumentException](effectOnly.copy(inferenceEvidence = Some(evidence)))
    assertEquals(effectOnly.inferenceEvidence, None)

  test("all ten codes are closed; Unrecorded is distinct from measured success"):
    assertEquals(InferenceStatusCode.values.map(_.code).toVector, (0 to 9).map(_.toByte).toVector)
    InferenceStatusCode.values.foreach(code => assertEquals(InferenceStatusCode.fromCode(code.code), Right(code)))
    assert(InferenceStatusCode.fromCode(10).isLeft)
    assert(InferenceStatusCode.fromCode(-1).isLeft)
    assertNotEquals(InferenceStatusCode.Unrecorded, InferenceStatusCode.Estimable)

  test("selected status validation preserves permutations and refuses missing evidence and capacities"):
    val selection = InferenceStatusSelection(planes.reverse, Vector(2, 0, 1))
    assertEquals(selection.cells, 6L)
    assert(InferenceStatusValidation.check(unit, selection, 6, 6).isRight)
    assert(InferenceStatusValidation.check(unit, selection, 5, 6).isLeft)
    assert(InferenceStatusValidation.check(unit, selection, 6, 5).isLeft)
    assert(InferenceStatusValidation.check(unit, selection.copy(samples = Vector(3)), 6, 6).isLeft)
    assert(InferenceStatusValidation.check(unit.copy(inferenceEvidence = None), selection, 6, 6)
      .left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    intercept[IllegalArgumentException](selection.copy(samples = Vector(0, 0)))
