package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix}

class WhiteningPlanSuite extends munit.FunSuite:

  private def matrix(values: Vector[Double]): DMat =
    Matrix.tabulate(values.length, 1)((row, _) => values(row))

  private def fromRows(values: Vector[Vector[Double]]): DMat =
    require(values.nonEmpty && values.forall(_.length == values.head.length))
    Matrix.tabulate(values.length, values.head.length)((row, col) => values(row)(col))

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double = 1e-12): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).zipWithIndex.foreach { case ((a, e), i) =>
      assert(math.abs(a - e) <= tol, clues(i, a, e))
    }

  private def assertMatrixClose(actual: DMat, expected: DMat, tol: Double = 1e-12): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    actual.valuesRowMajor.zip(expected.valuesRowMajor).zipWithIndex.foreach { case ((a, e), i) =>
      assert(math.abs(a - e) <= tol, clues(i, a, e))
    }

  private def combine(left: DMat, right: DMat, leftScale: Double, rightScale: Double): DMat =
    require(left.rows == right.rows && left.cols == right.cols)
    val out = DMat.newBuilder(left.rows, left.cols)
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        out(row, col) = leftScale * left(row, col) + rightScale * right(row, col)
        col += 1
      row += 1
    out.result()

  private def manualWhiten(
      values: Vector[Double],
      coefficients: ArmaCoefficients,
      segments: Vector[TimeSegment],
      exactFirstAr1: Boolean
  ): Vector[Double] =
    val out = Array.fill(values.length)(0.0)
    segments.foreach { segment =>
      val firstScale =
        if exactFirstAr1 then coefficients.exactAr1FirstScale.toOption.get
        else 1.0
      var row = segment.start
      while row < segment.endExclusive do
        var value = values(row)
        var lag = 0
        while lag < coefficients.phi.length do
          val lagged = row - lag - 1
          if lagged >= segment.start then value -= coefficients.phi(lag) * values(lagged)
          lag += 1
        lag = 0
        while lag < coefficients.theta.length do
          val lagged = row - lag - 1
          if lagged >= segment.start then value -= coefficients.theta(lag) * out(lagged)
          lag += 1
        if row == segment.start && firstScale != 1.0 then value *= firstScale
        out(row) = value
        row += 1
    }
    out.toVector

  test("global AR(1) plan applies exact first-row scaling") {
    val values = Vector(1.0, 2.0, 4.0, 7.0)
    val coefficients = ArmaCoefficients.ar(0.5)
    val segments = TimeSegments.continuous(values.length)
    val plan = WhiteningPlan.global(coefficients, segments, exactFirstAr1 = true)

    assertEquals(plan.coefficientScope, CoefficientScope.Global(coefficients))
    assertEquals(plan.coveredSegments.nTimepoints, values.length)
    assertEquals(plan.initialCondition, InitialConditionPolicy.ExactAr1)

    val whitened = WhiteningTransform.matrix(plan, matrix(values)).toOption.get.col(0).toSeq.toVector
    val expected = manualWhiten(values, coefficients, segments, exactFirstAr1 = true)

    assertClose(whitened, expected)
    assertEqualsDouble(whitened.head, math.sqrt(0.75), 1e-12)
  }

  test("global AR(2) plan matches manual recursion") {
    val values = Vector(1.0, 2.0, 4.0, 7.0, 11.0)
    val coefficients = ArmaCoefficients.ar(0.5, -0.2)
    val segments = TimeSegments.continuous(values.length)
    val plan = WhiteningPlan.global(coefficients, segments, exactFirstAr1 = true)

    val whitened = WhiteningTransform.matrix(plan, matrix(values)).toOption.get.col(0).toSeq.toVector
    val expected = manualWhiten(values, coefficients, segments, exactFirstAr1 = true)

    assertClose(whitened, expected)
    assertClose(whitened, Vector(1.0, 1.5, 3.2, 5.4, 8.3))
  }

  test("global ARMA(1,1) plan uses previous innovations") {
    val values = Vector(1.0, 1.8, 2.9, 4.1, 5.6)
    val coefficients = ArmaCoefficients.arma(phi = Vector(0.4), theta = Vector(-0.25))
    val segments = TimeSegments.continuous(values.length)
    val plan = WhiteningPlan.global(coefficients, segments, exactFirstAr1 = true)

    val whitened = WhiteningTransform.matrix(plan, matrix(values)).toOption.get.col(0).toSeq.toVector
    val expected = manualWhiten(values, coefficients, segments, exactFirstAr1 = true)

    assertClose(whitened, expected)
  }

  test("censor gaps reset whitening recursions without dropping rows") {
    val values = Vector(1.0, 2.0, 4.0, 8.0, 16.0, 32.0)
    val base = TimeSegments.fromRunLengths(Vector(values.length))
    val segments = TimeSegments.withCensorResets(base, Set(2))
    val coefficients = ArmaCoefficients.ar(0.5)
    val plan = WhiteningPlan.global(coefficients, segments, exactFirstAr1 = false)

    assertEquals(segments, Vector(TimeSegment(0, 3, 0), TimeSegment(3, 6, 0)))

    val whitened = WhiteningTransform.matrix(plan, matrix(values)).toOption.get.col(0).toSeq.toVector
    val expected = manualWhiten(values, coefficients, segments, exactFirstAr1 = false)

    assertClose(whitened, expected)
    assertEqualsDouble(whitened(3), 8.0, 1e-12)
  }

  test("run-specific plans apply coefficients by run index") {
    val values = Vector(1.0, 2.0, 4.0, 8.0, 16.0)
    val segments = TimeSegments.fromRunLengths(Vector(2, 3))
    val plan = WhiteningPlan.byRun(
      Vector(ArmaCoefficients.ar(0.5), ArmaCoefficients.ar(-0.25)),
      segments,
      exactFirstAr1 = false
    )

    assertEquals(plan.coefficientScope.kind, CoefficientScopeKind.ByRun)
    assertEquals(plan.pooling, NoisePooling.Run)
    assertEquals(plan.coefficients.length, 2)

    val whitened = WhiteningTransform.matrix(plan, matrix(values)).toOption.get.col(0).toSeq.toVector

    assertClose(whitened, Vector(1.0, 1.5, 4.0, 9.0, 18.0))
  }

  test("segment layouts distinguish raw layout from matrix row coverage") {
    val layout = SegmentLayout.fromSegments(TimeSegments.fromRunLengths(Vector(2, 3))).toOption.get
    val covered = layout.coverRows(5).toOption.get
    val mismatch = layout.coverRows(6)

    assertEquals(layout.nTimepoints, 5)
    assertEquals(covered.runs, Vector(0, 1))
    assert(mismatch.left.toOption.contains(ArError.SegmentCoverageMismatch(5, 6)))
  }

  test("precomputed initial-condition scale is used without inspecting AR(1) stationarity") {
    val values = Vector(2.0, 3.0, 5.0)
    val coefficients = ArmaCoefficients.ar(1.05)
    val segments = TimeSegments.continuous(values.length)
    val plan = WhiteningPlan
      .globalWithInitialCondition(
        coefficients,
        segments,
        initialCondition = InitialConditionPolicy.PrecomputedScale(0.25)
      )
      .toOption
      .get

    val whitened = WhiteningTransform.matrix(plan, matrix(values)).toOption.get.col(0).toSeq.toVector

    assertClose(whitened, Vector(0.5, 0.9, 1.85))
    assertEquals(plan.exactFirstAr1, false)
  }

  test("invalid precomputed initial-condition scales are structured errors") {
    val result = WhiteningPlan.globalWithInitialCondition(
      ArmaCoefficients.ar(0.5),
      TimeSegments.continuous(3),
      initialCondition = InitialConditionPolicy.PrecomputedScale(Double.NaN)
    )

    assert(result.left.toOption.exists {
      case ArError.InvalidInitialScale(scale) => scale.isNaN
      case _                                  => false
    })
  }

  test("whitening transform applies the same plan to design and response") {
    val design = fromRows(Vector(Vector(1.0, 0.0), Vector(1.0, 1.0), Vector(1.0, 2.0)))
    val response = fromRows(Vector(Vector(2.0), Vector(4.0), Vector(8.0)))
    val plan = WhiteningPlan.global(ArmaCoefficients.ar(0.5), TimeSegments.continuous(3), exactFirstAr1 = false)

    val out = WhiteningTransform(plan, design, response).toOption.get

    assertEquals(out.design.rows, 3)
    assertEquals(out.design.cols, 2)
    assertEquals(out.response.rows, 3)
    assertEquals(out.response.cols, 1)
    assertEqualsDouble(out.design(1, 0), 0.5, 1e-12)
    assertEqualsDouble(out.response(2, 0), 6.0, 1e-12)
  }

  test("whitening is linear in the input matrix") {
    val left = fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(3.0, 5.0),
        Vector(8.0, 13.0),
        Vector(21.0, 34.0)
      )
    )
    val right = fromRows(
      Vector(
        Vector(-1.0, 0.5),
        Vector(2.0, -3.0),
        Vector(4.0, 1.0),
        Vector(0.0, 7.0)
      )
    )
    val plan = WhiteningPlan.global(
      ArmaCoefficients.ar(0.4, -0.15),
      TimeSegments.continuous(left.rows),
      exactFirstAr1 = true
    )

    val combined = combine(left, right, leftScale = 2.0, rightScale = -0.5)
    val whitenedCombined = WhiteningTransform.matrix(plan, combined).toOption.get
    val whitenedLeft = WhiteningTransform.matrix(plan, left).toOption.get
    val whitenedRight = WhiteningTransform.matrix(plan, right).toOption.get
    val expected = combine(whitenedLeft, whitenedRight, leftScale = 2.0, rightScale = -0.5)

    assertMatrixClose(whitenedCombined, expected)
  }

  test("invalid exact AR(1) first-row scaling is reported as a typed error") {
    val plan = WhiteningPlan.global(
      ArmaCoefficients.ar(1.05),
      TimeSegments.continuous(3),
      exactFirstAr1 = true
    )
    val result = WhiteningTransform.matrix(plan, matrix(Vector(1.0, 2.0, 3.0)))

    assert(result.left.toOption.exists {
      case ArError.InvalidExactFirstAr1(rho) => rho == 1.05
      case _                                => false
    })
  }
