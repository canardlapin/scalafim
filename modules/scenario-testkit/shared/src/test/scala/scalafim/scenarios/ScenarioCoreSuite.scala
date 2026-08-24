package scalafim.scenarios

class ScenarioCoreSuite extends munit.FunSuite:
  test("clean results are the only default CI pass") {
    val result =
      ScenarioHarness.result(
        "scenario.clean",
        Vector(ScenarioHarness.fact("shape", passed = true, detail = "ok"))
      )

    assertEquals(result.status, ScenarioStatus.Pass)
    assert(result.ciPass)
    assert(result.ciPass(ScenarioPolicy.PassOnly))
  }

  test("failed observations and blocking caveats cannot be policy-approved") {
    val failed =
      ScenarioHarness.result(
        "scenario.failed",
        Vector(ScenarioHarness.fact("shape", passed = false, detail = "wrong"))
      )
    assertEquals(failed.status, ScenarioStatus.Fail)
    assert(!failed.ciPass(ScenarioPolicy.allowCaveats("scenario.gap")))

    val caveat =
      ScenarioCaveat(
        id = "scenario.stale",
        kind = CaveatKind.FixtureFreshness,
        severity = CaveatSeverity.Blocking,
        owner = "testkit",
        followUp = Some("regenerate"),
        detail = "fixture is stale"
      )
    val blocked =
      ScenarioHarness.result(
        "scenario.blocked",
        Vector(ScenarioHarness.fact("numeric", passed = true, detail = "ok")),
        caveats = Vector(caveat)
      )
    assertEquals(blocked.status, ScenarioStatus.Fail)
    assert(!blocked.ciPass(ScenarioPolicy.allowCaveats("scenario.stale")))
  }

  test("matrix and vector adapters compare shape before values") {
    val actualMatrix = new ScenarioMatrix:
      def rows: Int = 2
      def cols: Int = 1
      def apply(row: Int, col: Int): Double = row.toDouble + col
    val expectedMatrix = new ScenarioMatrix:
      def rows: Int = 2
      def cols: Int = 1
      def apply(row: Int, col: Int): Double = row.toDouble + col
    val actualVector = new ScenarioVector:
      def length: Int = 2
      def apply(index: Int): Double = index.toDouble
    val expectedVector = new ScenarioVector:
      def length: Int = 2
      def apply(index: Int): Double = index.toDouble

    val observations =
      ScenarioHarness.matrix("matrix", actualMatrix, expectedMatrix, ScenarioTolerance.absolute(0.0)) ++
        ScenarioHarness.vector("vector", actualVector, expectedVector, ScenarioTolerance.absolute(0.0))
    assert(observations.forall(_.passed))
  }

  test("comparison metrics require scale and signed shape agreement together") {
    val tolerance = ScenarioComparisonTolerance.bounded(
      maxAbsoluteL2Error = 1e-12,
      maxRelativeL2Error = 1e-12,
      minimumSignedCorrelation = 0.999999,
      maxNormRatioDeviation = 1e-12
    )
    val expected = Vector(-2.0, -0.5, 1.0, 3.0)
    val exact = ScenarioHarness.comparisonMetrics("exact", expected, expected, tolerance)
    assert(exact.forall(_.passed), exact.map(_.render).mkString("\n"))

    val scaled = ScenarioHarness.comparisonMetrics("scaled", expected.map(_ * 2.0), expected, tolerance)
    val scaledMetrics = scaled.collect { case metric: ScenarioObservation.Metric => metric }
    assert(
      scaledMetrics.exists {
        case ScenarioObservation.Metric(_, ScenarioMetricKind.SignedCorrelation, _, _, _) => true
        case _ => false
      }
    )
    assert(
      scaledMetrics.collectFirst {
        case metric @ ScenarioObservation.Metric(_, ScenarioMetricKind.SignedCorrelation, _, _, _) => metric.passed
      }.contains(true),
      scaled.map(_.render).mkString("\n")
    )
    assert(!scaled.forall(_.passed), "correlation alone admitted a twofold scale error")

    val reversed = ScenarioHarness.comparisonMetrics("reversed", expected.map(-_), expected, tolerance)
    assert(!reversed.forall(_.passed), "a sign-reversed output passed the comparison profile")
  }

  test("comparison metric domain failures are explicit observations") {
    val tolerance = ScenarioComparisonTolerance.bounded(1.0, 1.0, 0.0, 1.0)
    val empty = ScenarioHarness.comparisonMetrics("empty", Vector.empty, Vector.empty, tolerance)
    val nonFinite = ScenarioHarness.comparisonMetrics("non-finite", Vector(Double.NaN, 1.0), Vector(0.0, 1.0), tolerance)
    assert(!empty.forall(_.passed))
    assert(!nonFinite.forall(_.passed))
    assert(empty.exists {
      case ScenarioObservation.Fact("empty.metrics.domain", false, _) => true
      case _ => false
    })
  }
