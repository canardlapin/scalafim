package scalafim.fmri.mvpa

import gale.linalg.DMat

final case class MvpaBenchmarkCase(
    name: String,
    iterations: Int,
    runOnce: () => Either[MvpaError, Double]
)

final case class MvpaBenchmarkResult(
    name: String,
    iterations: Int,
    checksum: Double,
    elapsedNanos: Long
)

object MvpaBenchmarkHarness:
  def run(
      cases: Vector[MvpaBenchmarkCase],
      nowNanos: () => Long
  ): Either[MvpaError, Vector[MvpaBenchmarkResult]] =
    val out = Vector.newBuilder[MvpaBenchmarkResult]
    var caseIndex = 0
    while caseIndex < cases.length do
      val benchmark = cases(caseIndex)
      val trimmedName = benchmark.name.trim
      if trimmedName.isEmpty then
        return Left(MvpaError.InvalidClassifierInput("benchmark case name must be non-empty"))
      if benchmark.iterations <= 0 then
        return Left(MvpaError.InvalidClassifierInput("benchmark iterations must be positive"))

      val start = nowNanos()
      var checksum = 0.0
      var iteration = 0
      while iteration < benchmark.iterations do
        benchmark.runOnce() match
          case Right(value) if value.isFinite =>
            checksum += value
          case Right(_) =>
            return Left(MvpaError.InvalidClassifierInput(s"benchmark '$trimmedName' produced a non-finite checksum"))
          case Left(error) =>
            return Left(error)
        iteration += 1
      val elapsed = nowNanos() - start
      out += MvpaBenchmarkResult(trimmedName, benchmark.iterations, checksum, elapsed)
      caseIndex += 1
    Right(out.result())

class MvpaParitySuite extends munit.FunSuite:
  private val RReferenceTolerance = 1e-10

  private def assertVectorEquals(actual: Vector[Double], expected: Vector[Double], tolerance: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i), expected(i), tolerance)
      i += 1

  private def assertMatrixEquals(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1

  private def assertMetricVectorEquals(actual: MetricVector, expected: MetricVector, tolerance: Double): Unit =
    assertEquals(actual.names, expected.names)
    assertEquals(actual.values.length, expected.values.length)
    var i = 0
    while i < actual.values.length do
      // The generated feature decode fixture contains exact tied RDM distances; keep rank-score tolerance local.
      val metricTolerance =
        if expected.names(i) == "RdmCorrelation" then math.max(tolerance, 5e-3)
        else tolerance
      assertEqualsDouble(actual.values(i), expected.values(i), metricTolerance)
      i += 1

  test("RDM parity fixtures keep squared, normalized, and Euclidean estimands explicit") {
    val squared = Rdm.squaredEuclidean(MvpaParityFixtures.Rdm.patterns).toOption.get
    val normalized = Rdm
      .squaredEuclidean(MvpaParityFixtures.Rdm.patterns, normalizeByFeatures = true)
      .toOption
      .get
    val euclidean = Rdm.euclidean(MvpaParityFixtures.Rdm.patterns).toOption.get
    val correlation = Rdm.correlation(MvpaParityFixtures.Correlation.patterns).toOption.get

    assertVectorEquals(squared.values, MvpaParityFixtures.Rdm.squaredEuclidean, 1e-12)
    assertVectorEquals(normalized.values, MvpaParityFixtures.Rdm.squaredEuclideanNormalized, 1e-12)
    assertVectorEquals(euclidean.values, MvpaParityFixtures.Rdm.euclidean, 1e-12)
    assertVectorEquals(correlation.values, MvpaParityFixtures.Correlation.distances, 1e-12)
  }

  test("crossnobis parity fixture keeps feature normalization explicit") {
    val raw = Rdm.crossnobisDistances(MvpaParityFixtures.Crossnobis.means, normalizeByFeatures = false)
    val normalized = Rdm.crossnobisDistances(MvpaParityFixtures.Crossnobis.means, normalizeByFeatures = true)

    assertEqualsDouble(raw.values.head, MvpaParityFixtures.Crossnobis.rawDistance, 1e-12)
    assertEqualsDouble(normalized.values.head, MvpaParityFixtures.Crossnobis.normalizedDistance, 1e-12)
  }

  test("RSA scorer parity fixture residualizes labeled nuisance RDMs") {
    val scorer = RdmScorer.PartialPearson.unsafe(Vector(MvpaParityFixtures.Rsa.trendControlReversed))
    val score = scorer
      .score(MvpaParityFixtures.Rsa.items, MvpaParityFixtures.Rsa.observed, MvpaParityFixtures.Rsa.target)
      .toOption
      .get

    assertEqualsDouble(score, 1.0, 1e-12)
  }

  test("classifier parity fixtures anchor centroid and ridge probability oracles") {
    val correlationFit = CorrelationCentroidClassifier()
      .fit(MvpaParityFixtures.Classifiers.centroidPatterns.value, Classification.categorical(MvpaParityFixtures.Classifiers.centroidResponse, MvpaParityFixtures.Classifiers.centroidPatterns.samples).toOption.get)
      .toOption
      .get
    val correlationPrediction = correlationFit.predict(MvpaParityFixtures.Classifiers.centroidPatterns.value).toOption.get

    assertEquals(correlationPrediction.classes.map(_.value), Vector("a", "b"))
    assertEqualsDouble(correlationPrediction.probabilities(0, 0), MvpaParityFixtures.Classifiers.centroidHighProbability, 1e-12)
    assertEqualsDouble(correlationPrediction.probabilities(1, 1), MvpaParityFixtures.Classifiers.centroidHighProbability, 1e-12)

    val swiftFit = SwiftCentroidClassifier(FeatureScaling.None)
      .fit(MvpaParityFixtures.Classifiers.swiftPatterns.value, Classification.categorical(MvpaParityFixtures.Classifiers.swiftResponse, MvpaParityFixtures.Classifiers.swiftPatterns.samples).toOption.get)
      .toOption
      .get
    val swiftPrediction = swiftFit.predict(MvpaParityFixtures.Classifiers.swiftPatterns.value).toOption.get

    assertEquals(swiftPrediction.classes.map(_.value), Vector("a", "b"))
    assertEqualsDouble(swiftPrediction.probabilities(0, 0), MvpaParityFixtures.Classifiers.swiftHighProbability, 1e-12)
    assertEqualsDouble(swiftPrediction.probabilities(1, 1), MvpaParityFixtures.Classifiers.swiftHighProbability, 1e-12)

    val ridgeFit = RidgeLdaClassifier(gamma = 1.0)
      .fit(MvpaParityFixtures.Classifiers.ridgePatterns.value, Classification.categorical(MvpaParityFixtures.Classifiers.ridgeResponse, MvpaParityFixtures.Classifiers.ridgePatterns.samples).toOption.get)
      .toOption
      .get
    val ridgePrediction = ridgeFit.predict(MvpaParityFixtures.Classifiers.ridgePatterns.value).toOption.get

    assertEquals(ridgePrediction.classes.map(_.value), Vector("a", "b"))
    assertEqualsDouble(ridgePrediction.probabilities(0, 0), MvpaParityFixtures.Classifiers.ridgeHighProbability, 1e-12)
    assertEqualsDouble(ridgePrediction.probabilities(3, 1), MvpaParityFixtures.Classifiers.ridgeHighProbability, 1e-12)
  }

  test("lightweight benchmark harness runs deterministic workloads with explicit checksums") {
    var tick = 0L
    def clock(): Long =
      val value = tick
      tick += 100L
      value

    val workloads = Vector(
      MvpaBenchmarkCase(
        "rdm_squared",
        iterations = 3,
        () => Rdm.squaredEuclidean(MvpaParityFixtures.Rdm.patterns).map(_.values.sum)
      ),
      MvpaBenchmarkCase(
        "crossnobis_normalized",
        iterations = 2,
        () => Right(Rdm.crossnobisDistances(MvpaParityFixtures.Crossnobis.means).values.sum)
      ),
      MvpaBenchmarkCase(
        "swift_predict",
        iterations = 2,
        () =>
          val fit = SwiftCentroidClassifier(FeatureScaling.None)
            .fit(MvpaParityFixtures.Classifiers.swiftPatterns.value, Classification.categorical(MvpaParityFixtures.Classifiers.swiftResponse, MvpaParityFixtures.Classifiers.swiftPatterns.samples).toOption.get)
          fit.flatMap(_.predict(MvpaParityFixtures.Classifiers.swiftPatterns.value)).map { prediction =>
            prediction.probabilities.valuesRowMajor.sum
          }
      )
    )

    val results = MvpaBenchmarkHarness.run(workloads, clock).toOption.get

    assertEquals(results.map(_.name), Vector("rdm_squared", "crossnobis_normalized", "swift_predict"))
    assertEquals(results.map(_.elapsedNanos), Vector(100L, 100L, 100L))
    assertEqualsDouble(results(0).checksum, 120.0, 1e-12)
    assertEqualsDouble(results(1).checksum, 8.0, 1e-12)
    assertEqualsDouble(results(2).checksum, 8.0, 1e-12)
  }

  test("lightweight benchmark harness validates workload shape") {
    val result = MvpaBenchmarkHarness.run(
      Vector(MvpaBenchmarkCase("bad", iterations = 0, () => Right(0.0))),
      () => 0L
    )

    assert(result.swap.toOption.get.message.contains("iterations"))
  }
