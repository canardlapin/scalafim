package scalafim.fmri.fit

import gale.linalg.DMat
import scalafim.fmri.design.{ColumnId, TrialId, RunIndex as DesignRunIndex}

class LssTargetDesignSuite extends munit.FunSuite:
  private def mat(rows: Vector[Vector[Double]]): DMat = GaleTestMatrix.fromRows(rows)
  private def id(value: String): ColumnId = ColumnId.unsafe(value)
  private def run(value: Int): DesignRunIndex = DesignRunIndex.unsafeOneBased(value)
  private def trial(runValue: Int, value: String, column: String): LssTrialIdentity =
    LssTrialIdentity(run(runValue), TrialId.unsafe(value), id(column))
  private def key(runValue: Int, value: String): LssTrialKey = LssTrialKey(run(runValue), TrialId.unsafe(value))

  test("selected target retains identities, row provenance, fixed columns, and sums term peers"):
    val trials = LssTrialDesign.unsafe(mat(Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9))), Vector("trial_a", "trial_b", "trial_c"))
    val fixed = LssFixedDesign.unsafe(mat(Vector(Vector(10), Vector(11), Vector(12))), Vector("motion"))
    val term = LssTermDesign.make(trials, fixed, Vector(trial(1, "a", "trial_a"), trial(1, "b", "trial_b"), trial(1, "c", "trial_c")), Vector(20, 22, 25)).toOption.getOrElse(fail("term"))
    val selected = LssTargetDesign.select(term, key(1, "b")).toOption.getOrElse(fail("target"))
    assertEquals(selected.target.sourceColumnId, id("trial_b"))
    assertEquals(selected.rowProvenance, Vector(20, 22, 25))
    assertEquals(selected.sourceColumnIds, Vector(id("trial_a"), id("trial_b"), id("trial_c")))
    val expected = mat(Vector(Vector(2, 4, 10), Vector(5, 10, 11), Vector(8, 16, 12)))
    var row = 0
    while row < expected.rows do
      var col = 0
      while col < expected.cols do
        assertEqualsDouble(selected.matrix(row, col), expected(row, col), 0.0)
        col += 1
      row += 1

  test("target selection fails explicitly for absent or duplicate identities"):
    val trials = LssTrialDesign.unsafe(mat(Vector(Vector(1, 2), Vector(3, 4))), Vector("x", "y"))
    val fixed = LssFixedDesign.empty(2)
    assertEquals(
      LssTermDesign.make(trials, fixed, Vector(trial(1, "a", "x"), trial(1, "a", "y")), Vector(0, 1)).left.toOption,
      Some(LssTargetDesignError.DuplicateTrialIds(Vector(key(1, "a"))))
    )
    val term = LssTermDesign.make(trials, fixed, Vector(trial(1, "a", "x"), trial(1, "b", "y")), Vector(0, 1)).toOption.getOrElse(fail("term"))
    assertEquals(LssTargetDesign.select(term, key(1, "missing")).left.toOption,
      Some(LssTargetDesignError.MissingTarget(key(1, "missing"), Vector(key(1, "a"), key(1, "b")))))
    assertEquals(LssTargetDesign.select(term, key(2, "a")).left.toOption.map(_.getClass), Some(classOf[LssTargetDesignError.MissingTarget]))

  test("identities are bound to trial columns by name and misordered identities are rejected"):
    val trials = LssTrialDesign.unsafe(mat(Vector(Vector(1, 0), Vector(0, 1))), Vector("x", "y"))
    assertEquals(
      LssTermDesign.make(trials, LssFixedDesign.empty(2), Vector(trial(1, "b", "y"), trial(1, "a", "x")), Vector(0, 1)).left.toOption,
      Some(LssTargetDesignError.IdentityBinding(0, id("y"), "x"))
    )
    val fixed = LssFixedDesign.unsafe(mat(Vector(Vector(1), Vector(0))), Vector("x"))
    assertEquals(
      LssTermDesign.make(trials, fixed, Vector(trial(1, "a", "x"), trial(1, "b", "y")), Vector(0, 1)).left.toOption,
      Some(LssTargetDesignError.TrialColumnInFixed(id("x")))
    )

  test("per-run trial ids may repeat across runs and are selected by run"):
    val trials = LssTrialDesign.unsafe(mat(Vector(Vector(1, 0, 0), Vector(0, 1, 0), Vector(0, 0, 1))), Vector("run1_trial_1", "run1_trial_2", "run2_trial_1"))
    val identities = Vector(trial(1, "1", "run1_trial_1"), trial(1, "2", "run1_trial_2"), trial(2, "1", "run2_trial_1"))
    val term = LssTermDesign.make(trials, LssFixedDesign.empty(3), identities, Vector(0, 1, 2)).fold(error => fail(error.message), identity)
    val second = LssTargetDesign.select(term, key(2, "1")).fold(error => fail(error.message), identity)
    assertEquals(second.target.sourceColumnId, id("run2_trial_1"))
    assertEqualsDouble(second.matrix(2, 0), 1.0, 0.0)
    assertEqualsDouble(second.matrix(0, 1), 1.0, 0.0)
    assertEqualsDouble(second.matrix(2, 1), 0.0, 0.0)

  test("a single-trial term has no peer column"):
    val trials = LssTrialDesign.unsafe(mat(Vector(Vector(0.0), Vector(1.0), Vector(0.5))), Vector("only"))
    val fixed = LssFixedDesign.unsafe(mat(Vector(Vector(1.0), Vector(1.0), Vector(1.0))), Vector("intercept"))
    val term = LssTermDesign.make(trials, fixed, Vector(trial(1, "only", "only")), Vector(0, 1, 2)).fold(error => fail(error.message), identity)
    val selected = LssTargetDesign.select(term, key(1, "only")).fold(error => fail(error.message), identity)
    assertEquals(selected.otherTrialsRegressor, None)
    assertEquals(selected.matrix.cols, 2)
    val response = ResponseBlock.unsafe(mat(Vector(Vector(1.0), Vector(4.0), Vector(2.5))))
    val engine = LeastSquaresSeparate.fit(trials, response, fixed).fold(error => fail(error.message), identity)
    val ols = Ols.fit(DesignMatrix.unsafe(selected.matrix), response).fold(error => fail(error.message), identity)
    assertEqualsDouble(ols.coefficients(0, 0), engine.coefficients(0, 0), 1e-12)

  test("non-finite target or fixed values are rejected when the term is made"):
    val identities = Vector(trial(1, "a", "x"), trial(1, "b", "y"))
    val badTrials = LssTrialDesign.unsafe(mat(Vector(Vector(1, 0), Vector(0, Double.NaN))), Vector("x", "y"))
    assertEquals(
      LssTermDesign.make(badTrials, LssFixedDesign.empty(2), identities, Vector(0, 1)).left.toOption,
      Some(LssTargetDesignError.NonFiniteTrialValue(id("y"), 1))
    )
    val trials = LssTrialDesign.unsafe(mat(Vector(Vector(1, 0), Vector(0, 1))), Vector("x", "y"))
    val badFixed = LssFixedDesign.unsafe(mat(Vector(Vector(Double.PositiveInfinity), Vector(1.0))), Vector("drift"))
    assertEquals(
      LssTermDesign.make(trials, badFixed, identities, Vector(0, 1)).left.toOption,
      Some(LssTargetDesignError.NonFiniteFixedValue("drift", 0))
    )

  test("OLS on every target design reproduces the LSS engine and its fmrilss fixture"):
    // Same setup and expected coefficients as LssSuite's fmrilss fixture.
    val trialMatrix = mat(Vector(
      Vector(1.0, 0.0, 0.0), Vector(0.8, 0.2, 0.0), Vector(0.1, 1.0, 0.0), Vector(0.0, 0.6, 0.3),
      Vector(0.0, 0.2, 1.0), Vector(0.4, 0.0, 0.6), Vector(0.0, 0.0, 0.8), Vector(0.2, 0.5, 0.1)
    ))
    val fixedMatrix = mat(Vector.tabulate(8)(row => Vector(1.0, row - 3.5)))
    val response = ResponseBlock.unsafe(mat(Vector(
      Vector(5.2, -1.0), Vector(4.7, 0.5), Vector(6.4, 1.8), Vector(7.3, 1.0),
      Vector(8.1, 2.4), Vector(6.9, 0.7), Vector(8.8, 2.1), Vector(7.9, 1.5)
    )))
    val fmrilssExpected = Vector(
      Vector(-2.00714771389244, 0.18904127763313),
      Vector(1.8141791857713, 4.7225165628497),
      Vector(0.717973602484473, 3.83721532091097)
    )
    val names = Vector("trial_1", "trial_2", "trial_3")
    val trials = LssTrialDesign.unsafe(trialMatrix, names)
    val fixed = LssFixedDesign.unsafe(fixedMatrix, Vector("intercept", "trend"))
    val engine = LeastSquaresSeparate.fit(trials, response, fixed).fold(error => fail(error.message), identity)
    val identities = names.zipWithIndex.map((name, index) => trial(1, s"${index + 1}", name))
    val term = LssTermDesign.make(trials, fixed, identities, Vector.tabulate(8)(identity)).fold(error => fail(error.message), identity)
    identities.zipWithIndex.foreach { (identity, index) =>
      val target = LssTargetDesign.select(term, identity.key).fold(error => fail(error.message), identity => identity)
      assertEquals(target.matrix.cols, 4)
      val ols = Ols.fit(DesignMatrix.unsafe(target.matrix), response).fold(error => fail(error.message), identity => identity)
      var voxel = 0
      while voxel < 2 do
        assertEqualsDouble(ols.coefficients(0, voxel), engine.coefficients(index, voxel), 1e-10)
        assertEqualsDouble(ols.coefficients(0, voxel), fmrilssExpected(index)(voxel), 1e-10)
        voxel += 1
    }
