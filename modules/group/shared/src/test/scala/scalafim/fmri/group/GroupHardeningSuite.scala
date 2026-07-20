package scalafim.fmri.group

import scalafim.dataset.SubjectId
import scalafim.fmri.design.data.{Column, DataTable}
import gale.linalg.{DMat, DVec, Matrix, Vec}

/** Regression tests for totality and numerical-scale robustness (from the
  * adversarial council review): every public constructor stays total, and a
  * well-posed meta-analysis is not falsely singular merely because its weights
  * are tiny.
  */
class GroupHardeningSuite extends munit.FunSuite:

  private def value[A](e: Either[GroupError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def subjects(n: Int): Vector[SubjectId] =
    (1 to n).toVector.map(i => SubjectId(s"s$i"))

  private def column(values: Double*): DMat =
    GroupTestMatrix.fromRows(values.toVector.map(v => Vector(v)))

  test("covariates returns a typed error for a non-numeric column, not an exception") {
    val table = DataTable.fromColumns("site" -> Column.Strings(Vector("a", "b", "c")))
    assertEquals(
      GroupDesign.covariates(table, Vector("site")).left.toOption,
      Some(GroupError.NonNumericColumn("site"))
    )
  }

  test("non-finite design matrices are rejected") {
    val nan = GroupTestMatrix.fromRows(Vector(Vector(1.0), Vector(Double.NaN), Vector(1.0)))
    assertEquals(
      GroupDesign.fromMatrix(nan, Vector("(Intercept)")).left.toOption,
      Some(GroupError.NonFiniteData("group design"))
    )
    // And through the covariate builder.
    val table = DataTable.fromColumns("x" -> Column.Doubles(Vector(1.0, Double.NaN, 3.0)))
    assert(GroupDesign.covariates(table, Vector("x")).isLeft)
  }

  test("duplicate first-level contrast names are rejected, not silently collapsed") {
    val r = value(GroupResponse.fromEffects(column(1.0, 2.0, 3.0)))
    assertEquals(
      GroupData(subjects(3), GroupSpace.SampleAxis(1), Vector("faces" -> r, "faces" -> r)).left.toOption,
      Some(GroupError.DuplicateContrasts(Vector("faces")))
    )
  }

  test("meta-analysis with very large variances is not falsely singular") {
    // weights ~ 1e-13 make X'WX ~ 3e-13, well-posed but below an absolute 1e-12 pivot.
    val effects = column(1.0, 2.0, 3.0)
    val variances = column(1e13, 1e13, 1e13)
    val data = value(GroupData.single(subjects(3), GroupSpace.SampleAxis(1), "c", effects, Some(variances)))
    val fit = value(GroupEngine.fit(value(GroupModel.build(data, GroupDesign.intercept(3), GroupWeighting.InverseVariance)))).fit("c").get

    // Equal weights -> weighted mean is the plain mean, 2.0; SE finite (~sqrt(1/sum w)).
    assertEqualsDouble(fit.coefficients(0, 0), 2.0, 1e-9)
    assert(fit.standardErrors(0, 0).isFinite && fit.standardErrors(0, 0) > 0.0)
    assert(fit.heterogeneity.get.q(0).isFinite)
  }

  test("FDR excludes non-finite p-values and returns them as NaN") {
    val q = Fdr.benjaminiHochberg(Array(0.01, Double.NegativeInfinity, 0.02))
    assertEqualsDouble(q(0), 0.02, 1e-12)
    assert(q(1).isNaN)
    assertEqualsDouble(q(2), 0.02, 1e-12)
  }

  test("GroupFit rejects a covariance that does not match its terms") {
    intercept[IllegalArgumentException] {
      GroupFit(
        contrast = FirstLevelContrastName.unsafe("c"),
        termNames = Vector("a", "b"),
        coefficients = Matrix.zeros(2, 1),
        standardErrors = Matrix.zeros(2, 1),
        covariance = GroupCovariance.PerSample(termCount = 1, packed = Matrix.zeros(1, 1)),
        statistic = GroupStatistic.Normal,
        heterogeneity = None,
        space = GroupSpace.SampleAxis(1)
      )
    }
  }

  test("saturated fits (n == terms) are rejected so heterogeneity df stays defined") {
    // Documented policy: every estimator requires n > p. Two subjects, two design
    // terms -> InsufficientSubjects, for both OLS and meta-analysis.
    val labels = Vector("a", "b")
    val design = value(GroupDesign.twoSample(labels))
    val data = value(GroupData.single(subjects(2), GroupSpace.SampleAxis(1), "c", column(1.0, 2.0), Some(column(1.0, 1.0))))
    assertEquals(GroupModel.build(data, design).left.toOption, Some(GroupError.InsufficientSubjects(2, 2)))
    assertEquals(
      GroupModel.build(data, design, GroupWeighting.InverseVariance).left.toOption,
      Some(GroupError.InsufficientSubjects(2, 2))
    )
  }
