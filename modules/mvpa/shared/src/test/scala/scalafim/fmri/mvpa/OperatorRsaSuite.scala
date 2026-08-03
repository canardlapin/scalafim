package scalafim.fmri.mvpa

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}

class OperatorRsaSuite extends munit.FunSuite:

  private val patterns = PatternMatrix.fromRows(
    Vector(
      Vector(0.0, 0.2, 0.0, 0.1, -0.1),
      Vector(1.0, 0.1, 0.5, 0.0, 0.2),
      Vector(0.0, 2.0, 0.2, -0.1, 0.1),
      Vector(1.5, 1.0, -0.2, 0.3, 0.0),
      Vector(0.1, 0.0, 0.1, 0.2, -0.2),
      Vector(1.2, 0.2, 0.4, -0.1, 0.1),
      Vector(-0.1, 1.8, 0.3, 0.0, 0.2),
      Vector(1.4, 1.1, -0.1, 0.2, -0.1),
      Vector(-0.1, 0.1, -0.1, 0.0, 0.0),
      Vector(0.9, -0.1, 0.6, 0.1, 0.3),
      Vector(0.2, 2.1, 0.1, -0.2, 0.0),
      Vector(1.6, 0.9, -0.3, 0.4, 0.1)
    )
  )
  private val response = Response
    .categorical(Vector.fill(3)(Vector("a", "b", "c", "d")).flatten)
    .toOption
    .get
  private val folds = FoldPlan.leaveOneBlockOut(Vector.fill(4)(0) ++ Vector.fill(4)(1) ++ Vector.fill(4)(2)).toOption.get
  private val fullFeatureSet = FeatureSet.unsafe(RoiId(1), Vector(0, 1, 2, 3, 4))

  test("operator crossnobis is exactly equivalent to the explicit-pattern oracle") {
    val dense = MvpaTask.evaluate(
      PatternSource.fromMatrix(patterns),
      fullFeatureSet,
      response,
      CrossnobisAnalysis(normalizeByFeatures = true),
      Some(folds)
    )
    val operator = MvpaTask.evaluate(
      PatternSource.fromOperator(PatternOperator.fromMatrix(patterns).toOption.get),
      fullFeatureSet,
      response,
      OperatorCrossnobisAnalysis(normalizeByFeatures = true),
      Some(folds)
    )

    val expected = rdmFrom(dense)
    val actual = rdmFrom(operator)
    assertEquals(actual.labels, expected.labels)
    actual.rdm.values.zip(expected.rdm.values).foreach: (observed, reference) =>
      assertEqualsDouble(observed, reference, 1e-12)
    assertEquals(metricFrom(operator, "TrialPatternMaterializations"), 0.0)
  }

  test("operator geometry uses only adjoint sufficient statistics and has a fold-independent bound") {
    val counter = ApplyCounter()
    val counting = CountingOperator(patterns.value, counter)
    val operator = PatternOperator
      .fromOperator(
        SampleAxis(patterns.samples).toOption.get,
        patterns.featureIndices,
        counting,
        PatternOperatorProvenance.composed
      )
      .toOption
      .get

    val geometry = OperatorCrossvalidatedGeometry.compute(operator, response, folds).toOption.get

    assertEquals(counter.forwardApplications, 0)
    assertEquals(counter.adjointApplications, folds.folds.length * 4)
    assertEquals(geometry.receipt.execution, OperatorRdmExecution.FoldwiseSufficientStatistics)
    assertEquals(geometry.receipt.trialByFeatureMaterializations, 0)
    assertEquals(geometry.receipt.avoidedTrialPatternDoubles, patterns.samples.toLong * patterns.features)
    val expectedBound = patterns.samples.toLong * 4L +
      2L * patterns.features * 4L +
      2L * 4L * 4L
    assertEquals(geometry.receipt.peakOwnedDoublesUpperBound, expectedBound)
  }

  test("samples absent from every held-out partition cannot leak into the geometry") {
    val baseline = PatternMatrix.fromRows(
      Vector(
        Vector(0.0, 0.0), Vector(1.0, 0.0), Vector(0.0, 1.0),
        Vector(0.1, 0.0), Vector(1.1, 0.1), Vector(-0.1, 1.2),
        Vector(2.0, 2.0)
      )
    )
    val perturbed = PatternMatrix.fromRows(
      baseline.value.toRows.updated(6, Vector(1000000.0, -1000000.0))
    )
    val labels = Response.categorical(Vector("a", "b", "c", "a", "b", "c", "a")).toOption.get
    val plan = FoldPlan.unsafe(
      Vector(
        Fold.unsafe("one", Vector(3, 4, 5, 6), Vector(0, 1, 2)),
        Fold.unsafe("two", Vector(0, 1, 2, 6), Vector(3, 4, 5))
      ),
      samples = 7
    )

    val first = geometryFrom(baseline, labels, plan)
    val second = geometryFrom(perturbed, labels, plan)
    first.observed.rdm.values.zip(second.observed.rdm.values).foreach: (left, right) =>
      assertEqualsDouble(left, right, 1e-12)
  }

  test("crossnobis retains negative null geometry") {
    val nullPatterns = PatternMatrix.fromRows(Vector(Vector(1.0), Vector(-1.0), Vector(-1.0), Vector(1.0)))
    val labels = Response.categorical(Vector("a", "b", "a", "b")).toOption.get
    val plan = FoldPlan.leaveOneBlockOut(Vector(0, 0, 1, 1)).toOption.get
    val geometry = geometryFrom(nullPatterns, labels, plan)

    assertEqualsDouble(geometry.observed.rdm.values.head, -4.0, 1e-12)
  }

  test("operator RSA reuses labeled model scoring and partial nuisance controls through the ordinary engine") {
    val items = Vector("a", "b", "c", "d")
    val target = RdmModel.unsafe(
      "target",
      items,
      RdmVector.unsafe(4, Vector(1.0, 3.0, 2.0, 5.0, 4.0, 8.0))
    )
    val control = RdmModel.unsafe(
      "nuisance",
      items.reverse,
      RdmVector.unsafe(4, Vector(2.0, 5.0, 1.0, 4.0, 3.0, 7.0))
    )
    val scorer = RdmScorer.PartialPearson.unsafe(Vector(control))
    val plan = FeatureSetPlan
      .regional(
        "operator-rsa",
        Vector(fullFeatureSet, FeatureSet.unsafe(RoiId(2), Vector(0, 1, 2)))
      )
      .toOption
      .get
    val source = PatternSource.fromOperator(PatternOperator.fromMatrix(patterns).toOption.get)
    val analysis = OperatorCrossnobisRsaAnalysis(
      models = Vector(target),
      scorer = scorer,
      storeObservedRdm = true
    )
    val result = MvpaEngine.runSource(source, plan, response, analysis, Some(folds)).toOption.get

    assertEquals(result.successes.length, 2)
    assertEquals(result.failures, Vector.empty)
    val payload = result.successes.head.payload match
      case Some(RoiPayload.Rsa(Some(observed), scores)) =>
        assertEquals(scores.map(_.modelName), Vector("target"))
        val aligned = target.alignTo(observed.items).toOption.get
        val expected = scorer.score(observed, aligned).toOption.get
        assertEqualsDouble(scores.head.value, expected, 1e-12)
        scores.head.value
      case other => fail(s"unexpected operator RSA payload: $other")
    assertEqualsDouble(result.successes.head.metrics("target.PartialPearson").get, payload, 1e-12)
  }

  private def geometryFrom(
      matrix: PatternMatrix,
      labels: Response,
      plan: FoldPlan
  ): OperatorCrossvalidatedGeometry =
    OperatorCrossvalidatedGeometry
      .compute(PatternOperator.fromMatrix(matrix).toOption.get, labels, plan)
      .toOption
      .get

  private def rdmFrom(outcome: RoiOutcome): LabeledRdm =
    outcome match
      case RoiOutcome.Success(_, _, _, Some(RoiPayload.Rdm(rdm))) => rdm
      case other => fail(s"unexpected RDM outcome: $other")

  private def metricFrom(outcome: RoiOutcome, name: String): Double =
    outcome match
      case RoiOutcome.Success(_, _, metrics, _) => metrics(name).get
      case other => fail(s"unexpected metric outcome: $other")

  private final case class ApplyCounter(
      var forwardApplications: Int = 0,
      var adjointApplications: Int = 0
  )

  private final case class CountingOperator(value: DMat, counter: ApplyCounter) extends DoubleLinearOperator:
    override val rows: Int = value.rows
    override val cols: Int = value.cols

    override def applyTo(input: DVec, output: MutableDVec): Unit =
      counter.forwardApplications += 1
      multiply(value, input, output)

    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      counter.adjointApplications += 1
      multiply(value.t, input, output)

    private def multiply(matrix: DMat, input: DVec, output: MutableDVec): Unit =
      var row = 0
      while row < matrix.rows do
        var total = 0.0
        var col = 0
        while col < matrix.cols do
          total += matrix(row, col) * input(col)
          col += 1
        output(row) = total
        row += 1
