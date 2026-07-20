package scalafim.inference

import gale.linalg.DMat

class ProtocolSuite extends munit.FunSuite:

  import InferenceRReferenceFixtures as R

  private def accepted[A](value: Either[InferenceError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def matrix(value: R.MatrixData): DMat =
    InferenceNumerics.matrixFromRows(Vector.tabulate(value.rows) { row =>
      Vector.tabulate(value.cols)(col => value(row, col))
    })

  private def negate(value: DMat): DMat =
    InferenceNumerics.matrixFromRows(Vector.tabulate(value.rows) { row =>
      Vector.tabulate(value.cols)(col => -value(row, col))
    })

  private def reverseColumns(value: DMat): DMat =
    InferenceNumerics.matrixFromRows(Vector.tabulate(value.rows) { row =>
      Vector.tabulate(value.cols)(col => value(row, value.cols - 1 - col))
    })

  private val config = LadderExecutionConfig(
    accepted(MonteCarloDraws(39)),
    accepted(MonteCarloDraws(117)),
    accepted(Alpha(0.1)),
    accepted(BatchSize(5)),
    accepted(LadderSteps(3)),
    RootSeed(1729L),
    UnitPolicy.GroupNearTies(accepted(RelativeGap(0.01)))
  )

  test("PCA protocol reproduces observed roots and deflated rung statistics") {
    val fixture = R.ladders.find(_.family == "pca").get
    val protocol = PcaVarianceProtocol()
    val initial = accepted(PcaVarianceState.from(matrix(fixture.x)))
    val roots = accepted(protocol.roots(initial))
    val first = accepted(protocol.observed(initial))
    val secondState = accepted(protocol.remove(initial))
    val second = accepted(protocol.observed(secondState))

    fixture.roots.zip(roots).foreach { case (expected, actual) =>
      assertEqualsDouble(actual, expected, 1e-8)
    }
    assertEqualsDouble(first, fixture.steps(0).observed, 1e-12)
    assertEqualsDouble(second, fixture.steps(1).observed, 1e-12)
  }

  test("PLSC protocol reproduces observed cross-covariance roots and deflation") {
    val fixture = R.ladders.find(_.family == "plsc").get
    val protocol = PlscCovarianceProtocol()
    val initial = accepted(PlscCovarianceState.from(matrix(fixture.x), matrix(fixture.y.get)))
    val roots = accepted(protocol.roots(initial))
    val first = accepted(protocol.observed(initial))
    val secondState = accepted(protocol.remove(initial))
    val second = accepted(protocol.observed(secondState))

    fixture.roots.zip(roots).foreach { case (expected, actual) =>
      assertEqualsDouble(actual, expected, 1e-7)
    }
    assertEqualsDouble(first, fixture.steps(0).observed, 1e-8)
    assertEqualsDouble(second, fixture.steps(1).observed, 1e-8)
  }

  test("PCA and PLSC targets are invariant to sign and orthogonal coordinate changes") {
    val pcaFixture = R.ladders.find(_.family == "pca").get
    val pca = PcaVarianceProtocol()
    val pcaOriginal = accepted(pca.observed(accepted(PcaVarianceState.from(matrix(pcaFixture.x)))))
    val pcaNegated = accepted(pca.observed(accepted(PcaVarianceState.from(negate(matrix(pcaFixture.x))))))
    val pcaReversed = accepted(pca.observed(accepted(PcaVarianceState.from(reverseColumns(matrix(pcaFixture.x))))))
    assertEqualsDouble(pcaNegated, pcaOriginal, 1e-12)
    assertEqualsDouble(pcaReversed, pcaOriginal, 1e-12)

    val plscFixture = R.ladders.find(_.family == "plsc").get
    val plsc = PlscCovarianceProtocol()
    val x = matrix(plscFixture.x)
    val y = matrix(plscFixture.y.get)
    val original = accepted(plsc.observed(accepted(PlscCovarianceState.from(x, y))))
    val signChanged = accepted(plsc.observed(accepted(PlscCovarianceState.from(x, negate(y)))))
    val coordinatesChanged = accepted(plsc.observed(accepted(
      PlscCovarianceState.from(reverseColumns(x), reverseColumns(y))
    )))
    assertEqualsDouble(signChanged, original, 1e-8)
    assertEqualsDouble(coordinatesChanged, original, 1e-8)
  }

  test("a declared repeated-root subspace is invariant to rotation within that subspace") {
    val original = InferenceNumerics.matrixFromRows(Vector(
      Vector(1.0, 0.0, 0.0),
      Vector(-1.0, 0.0, 0.0),
      Vector(0.0, 1.0, 0.0),
      Vector(0.0, -1.0, 0.0),
      Vector(0.0, 0.0, 0.5),
      Vector(0.0, 0.0, -0.5)
    ))
    val c = Math.cos(Math.PI / 5.0)
    val s = Math.sin(Math.PI / 5.0)
    val rotation = InferenceNumerics.matrixFromRows(Vector(
      Vector(c, -s, 0.0),
      Vector(s, c, 0.0),
      Vector(0.0, 0.0, 1.0)
    ))
    val rotated = InferenceNumerics.multiply(original, rotation)
    val protocol = PcaVarianceProtocol()
    val originalRoots = accepted(protocol.roots(accepted(PcaVarianceState.from(original))))
    val rotatedRoots = accepted(protocol.roots(accepted(PcaVarianceState.from(rotated))))
    originalRoots.zip(rotatedRoots).foreach { case (left, right) =>
      assertEqualsDouble(left, right, 1e-12)
    }

    val plane = accepted(ComponentSet.from(Vector(accepted(ComponentIx(0)), accepted(ComponentIx(1)))))
    val axis = ComponentSet.one(accepted(ComponentIx(2)))
    val policy = UnitPolicy.Declared(accepted(DeclaredUnitGroups.from(Vector(plane, axis))))
    val originalUnits = accepted(LatentUnitFormation.form(originalRoots, Vector(true, true, false), policy))
    val rotatedUnits = accepted(LatentUnitFormation.form(rotatedRoots, Vector(true, true, false), policy))
    assertEquals(originalUnits.map(_.unit), rotatedUnits.map(_.unit))
    assertEquals(originalUnits.map(_.selected), rotatedUnits.map(_.selected))
    originalUnits.zip(rotatedUnits).foreach { case (left, right) =>
      left.roots.zip(right.roots).foreach { case (leftRoot, rightRoot) =>
        assertEqualsDouble(leftRoot, rightRoot, 1e-12)
      }
    }
    assertEquals(originalUnits.head.unit.identifiability, Identifiability.UnorientedSubspace)
  }

  test("PCA and PLSC execute through one deterministic ladder interpreter") {
    val pcaFixture = R.ladders.find(_.family == "pca").get
    val pcaState = accepted(PcaVarianceState.from(matrix(pcaFixture.x)))
    val pcaFirst = accepted(InferenceExecutor.runLadder(pcaState, PcaVarianceProtocol(), config))
    val pcaSecond = accepted(InferenceExecutor.runLadder(pcaState, PcaVarianceProtocol(), config))
    assertEquals(pcaFirst.ladder, pcaSecond.ladder)
    assertEquals(pcaFirst.ladder.rejectedThrough, 2)
    assertEquals(pcaFirst.units.map(_.unit.identifiability), Vector(
      Identifiability.OrientableAxis,
      Identifiability.OrientableAxis,
      Identifiability.UnorientedSubspace
    ))
    assertEquals(pcaFirst.units.map(_.selected), Vector(true, true, false))

    val plscFixture = R.ladders.find(_.family == "plsc").get
    val plscState = accepted(PlscCovarianceState.from(
      matrix(plscFixture.x),
      matrix(plscFixture.y.get)
    ))
    val plscConfig = config.copy(seed = RootSeed(plscFixture.seed))
    val plscFirst = accepted(InferenceExecutor.runLadder(plscState, PlscCovarianceProtocol(), plscConfig))
    val plscSecond = accepted(InferenceExecutor.runLadder(plscState, PlscCovarianceProtocol(), plscConfig))
    assertEquals(plscFirst.ladder, plscSecond.ladder)
    assertEquals(plscFirst.ladder.rejectedThrough, 2)
  }
