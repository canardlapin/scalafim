package scalafim.multivar

import gale.linalg.CholeskyOptions
import gale.linalg.DMat

class LdaSuite extends munit.FunSuite:

  /** Independent R fixture convention:
    *
    * `P = Z %*% solve(crossprod(Z)) %*% t(Z)`,
    * `S_B = t(X) %*% (P - 11'/n) %*% X`, and
    * `S_W = t(X) %*% (I - P) %*% X`. Roots come from
    * `eigen(solve(S_W, S_B))`; directions are normalized to
    * `t(w) %*% S_W %*% w = 1` and anchored at their largest-magnitude entry.
    */
  private val rFixture = GaleNumerics.matrixFromRows(
    Seq(
      Seq(2.0, 1.0, 0.0),
      Seq(2.5, 1.2, 0.2),
      Seq(1.8, 0.7, -0.1),
      Seq(-1.0, 2.0, 1.0),
      Seq(-1.2, 2.4, 0.8),
      Seq(-0.7, 1.8, 1.3),
      Seq(0.0, -1.0, 2.0),
      Seq(0.2, -1.3, 2.5),
      Seq(-0.2, -0.8, 1.7)
    )
  )
  private val rLabels = Vector(0, 0, 0, 1, 1, 1, 2, 2, 2)

  test("Fisher LDA matches an independent R generalized-eigen fixture"):
    val problem = accepted(
      LdaProblem.fromMatrix(
        rFixture,
        accepted(ClassIncidence.hard(rLabels)),
        WithinScatterPolicy.RequirePositiveDefinite,
        "lda-r"
      )
    )
    val fit = accepted(problem.fit(ComponentCount.unsafe(2)))

    assertVector(fit.criterionValues, Vector(441.3305203932556, 33.93115606228206), 1e-8)
    val expected = GaleNumerics.matrixFromRows(
      Seq(
        Seq(-3.463611196751007, 0.23913538611950075),
        Seq(1.970564806221832, 1.5396986347477177),
        Seq(3.994840499879260, 0.02043684514177281)
      )
    )
    assertMatrix(fit.functionalFrame.weights.toDense.toOption.get, expected, 1e-8)
    val expectedScores = GaleNumerics.multiply(rFixture, expected)
    assertMatrix(fit.scores(problem.value.table).toDense.toOption.get, expectedScores, 1e-8)
    assertEquals(fit.programFit.program.objective.label, "generalized-rayleigh")
    assert(fit.diagnostics.residual <= 1e-8)

  test("class relations pull back to direct between and within scatter oracles"):
    val incidence = accepted(ClassIncidence.hard(rLabels))
    val problem = accepted(
      LdaProblem.fromMatrix(rFixture, incidence, WithinScatterPolicy.RequirePositiveDefinite, "lda-scatter")
    ).value
    val between = problem.between.toDense.toOption.get
    val within = problem.within.toDense.toOption.get
    val expectedBetween = GaleNumerics.matrixFromRows(
      Seq(
        Seq(14.748888888888889, -2.17, -6.337777777777777),
        Seq(-2.17, 14.82, -6.170000000000001),
        Seq(-6.337777777777777, -6.17, 6.202222222222222)
      )
    )
    val expectedWithin = GaleNumerics.matrixFromRows(
      Seq(
        Seq(0.46666666666666845, -0.07666666666666665, 0.3966666666666666),
        Seq(-0.07666666666666672, 0.44000000000000189, -0.2766666666666665),
        Seq(0.39666666666666645, -0.27666666666666678, 0.5000000000000020)
      )
    )

    assertMatrix(between, expectedBetween, 1e-10)
    assertMatrix(within, expectedWithin, 1e-10)
    assertEquals(problem.between.provenance.events.last.isInstanceOf[SemanticProvenanceEvent.Certified], true)

  test("rank-deficient within scatter requires the explicit fixed shrinkage seam"):
    val x = GaleNumerics.matrixFromRows(
      Seq(Seq(-2.0, 0.0), Seq(-1.0, 0.0), Seq(1.0, 0.0), Seq(2.0, 0.0))
    )
    val incidence = accepted(ClassIncidence.hard(Vector(0, 0, 1, 1)))
    val rejected = LdaProblem.fromMatrix(x, incidence, WithinScatterPolicy.RequirePositiveDefinite, "lda-rank-reject")
    assert(rejected.isLeft)

    val regularized = accepted(
      LdaProblem.fromMatrix(
        x,
        incidence,
        WithinScatterPolicy.FixedTraceScaledRidge(TraceRidgeFraction.unsafe(0.1)),
        "lda-rank-ridge"
      )
    )
    val fit = accepted(regularized.fit(ComponentCount.unsafe(1)))
    val direction = fit.functionalFrame.weights.toDense.toOption.get
    assert(Math.abs(direction(0, 0)) > 10.0 * Math.abs(direction(1, 0)))
    assert(fit.shrinkage.ridgeAmount > 0.0)

  test("trace-ratio is a distinct inspectable program with Euclidean normalization"):
    val problem = accepted(
      LdaProblem.fromMatrix(
        rFixture,
        accepted(ClassIncidence.hard(rLabels)),
        WithinScatterPolicy.RequirePositiveDefinite,
        "lda-trace-ratio"
      )
    )
    val fisher = accepted(problem.fit(ComponentCount.unsafe(2), LdaObjective.FisherRayleigh))
    val traceRatio = accepted(problem.fit(ComponentCount.unsafe(2), LdaObjective.TraceRatio))

    assertEquals(traceRatio.programFit.program.objective.label, "trace-ratio")
    assertEquals(fisher.programFit.program.objective.label, "generalized-rayleigh")
    assert(traceRatio.diagnostics.criterionValue > 0.0)
    assert(traceRatio.diagnostics.residual <= 1e-7)
    val weights = traceRatio.functionalFrame.weights.toDense.toOption.get
    assertMatrix(GaleNumerics.multiply(weights.t, weights), DMat.eye(2), 1e-8)

  test("sample permutation, class relabeling, and common feature scale preserve Fisher semantics"):
    val base = fitR(rFixture, rLabels)
    val order = Vector(8, 2, 5, 0, 7, 3, 1, 6, 4)
    val permuted = GaleNumerics.matrixFromRows(order.map(row => (0 until rFixture.cols).map(col => rFixture(row, col))))
    val permutedLabels = order.map(rLabels)
    val relabeled = permutedLabels.map:
      case 0 => 19
      case 1 => -4
      case _ => 7
    val moved = fitR(permuted, relabeled)
    val scaled = fitR(MatrixOps.scale(rFixture, 3.5), rLabels)

    assertVector(moved.criterionValues, vectorValues(base.criterionValues), 1e-9)
    assertVector(scaled.criterionValues, vectorValues(base.criterionValues), 1e-9)
    assertProjector(moved.functionalFrame.weights.toDense.toOption.get, base.functionalFrame.weights.toDense.toOption.get, 1e-8)
    assertProjector(scaled.functionalFrame.weights.toDense.toOption.get, base.functionalFrame.weights.toDense.toOption.get, 1e-8)

  test("repeated discriminant roots report a subspace cluster, not invented axis identity"):
    val root3 = Math.sqrt(3.0)
    val means = Vector((1.0, 0.0), (-0.5, root3 / 2.0), (-0.5, -root3 / 2.0))
    val directions = means
    val rows = Vector.newBuilder[Seq[Double]]
    val labels = Vector.newBuilder[Int]
    var klass = 0
    while klass < 3 do
      val mean = means(klass)
      val direction = directions(klass)
      rows += Seq(mean._1 + 0.2 * direction._1, mean._2 + 0.2 * direction._2)
      rows += Seq(mean._1 - 0.2 * direction._1, mean._2 - 0.2 * direction._2)
      labels += klass
      labels += klass
      klass += 1
    val problem = accepted(
      LdaProblem.fromMatrix(
        GaleNumerics.matrixFromRows(rows.result()),
        accepted(ClassIncidence.hard(labels.result())),
        WithinScatterPolicy.RequirePositiveDefinite,
        "lda-repeated"
      )
    )
    val fit = accepted(problem.fit(ComponentCount.unsafe(2)))

    assertEquals(fit.diagnostics.spectralClusters, Vector(Vector(0, 1)))
    assert(fit.programFit.program.resultSemantics.equivalence.isInstanceOf[ResultEquivalence.SubspaceEquivalent])

  private def fitR(matrix: DMat, labels: Vector[Int]): LdaOperatorFit[?, ?, ?] =
    accepted(
      accepted(
        LdaProblem.fromMatrix(matrix, accepted(ClassIncidence.hard(labels)), WithinScatterPolicy.RequirePositiveDefinite, "lda-metamorphic")
      ).fit(ComponentCount.unsafe(2))
    )

  private def accepted[A](value: Either[MultivarError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def assertVector(actual: gale.linalg.DVec, expected: Vector[Double], tolerance: Double): Unit =
    assertEquals(actual.length, expected.length)
    var index = 0
    while index < actual.length do
      assertEqualsDouble(actual(index), expected(index), tolerance)
      index += 1

  private def assertMatrix(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1

  private def assertProjector(left: DMat, right: DMat, tolerance: Double): Unit =
    val leftProjector = euclideanProjector(left)
    val rightProjector = euclideanProjector(right)
    assertMatrix(leftProjector, rightProjector, tolerance)

  private def euclideanProjector(value: DMat): DMat =
    val gram = GaleNumerics.multiply(value.t, value)
    val inverse = gram.cholesky(CholeskyOptions()).toOption.get.solve(DMat.eye(gram.rows)).toOption.get
    GaleNumerics.multiply(value, GaleNumerics.multiply(inverse, value.t))

  private def vectorValues(value: gale.linalg.DVec): Vector[Double] =
    Vector.tabulate(value.length)(value.apply)
