package scalafim.fmri.group

import scalafim.dataset.SubjectId
import scalafim.fmri.design.data.{Column, DataTable}
import gale.linalg.{DMat, DVec, Matrix, Vec}

class GroupInferenceSuite extends munit.FunSuite:

  private def value[A](e: Either[GroupError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def subjects(n: Int): Vector[SubjectId] =
    (1 to n).toVector.map(i => SubjectId(s"s$i"))

  private def column(values: Double*): DMat =
    GroupTestMatrix.fromRows(values.toVector.map(v => Vector(v)))

  test("singular per-sample WLS system propagates NaN, not a bogus finite value") {
    // Rank-deficient design (two identical columns) makes XᵀWX singular.
    val design = value(GroupDesign.fromMatrix(GroupTestMatrix.fromRows(Vector(Vector(1.0, 1.0), Vector(1.0, 1.0), Vector(1.0, 1.0))), Vector("a", "b")))
    val data = value(GroupData.single(subjects(3), GroupSpace.SampleAxis(1), "c", column(1.0, 2.0, 3.0), Some(column(0.1, 0.1, 0.1))))
    val fit = value(GroupEngine.fit(value(GroupModel.build(data, design, GroupWeighting.InverseVariance)))).fit("c").get

    assert(fit.coefficients(0, 0).isNaN)
    assert(fit.heterogeneity.get.q(0).isNaN)
    assert(value(GroupContrast.term("a").evaluate(fit)).statistics(0).isNaN)
  }

  test("inverse-variance meta-regression recovers a covariate slope") {
    // Perfect line y = 1 + 2x with equal variances -> WLS == OLS, exact coefficients.
    val table = DataTable.fromColumns("x" -> Column.Doubles(Vector(0.0, 1.0, 2.0)))
    val design = value(GroupDesign.covariates(table, Vector("x")))
    val data = value(GroupData.single(subjects(3), GroupSpace.SampleAxis(1), "c", column(1.0, 3.0, 5.0), Some(column(0.25, 0.25, 0.25))))
    val fit = value(GroupEngine.fit(value(GroupModel.build(data, design, GroupWeighting.InverseVariance)))).fit("c").get

    assertEquals(fit.statistic, GroupStatistic.Normal)
    assertEqualsDouble(fit.term("(Intercept)").get.estimates(0), 1.0, 1e-9)
    assertEqualsDouble(fit.term("x").get.estimates(0), 2.0, 1e-9)
  }

  test("difference contrast on a cell-means design gives the group difference") {
    // Cell-means design (no intercept): term a = mean(a), term b = mean(b).
    val design = value(
      GroupDesign.fromMatrix(
        GroupTestMatrix.fromRows(
          Vector(Vector(1.0, 0.0), Vector(1.0, 0.0), Vector(1.0, 0.0), Vector(0.0, 1.0), Vector(0.0, 1.0), Vector(0.0, 1.0))
        ),
        Vector("a", "b")
      )
    )
    val effects = column(1.0, 2.0, 3.0, 5.0, 6.0, 7.0)
    val data = value(GroupData.single(subjects(6), GroupSpace.SampleAxis(1), "c", effects))
    val fit = value(GroupEngine.fit(value(GroupModel.build(data, design)))).fit("c").get

    val diff = value(GroupContrast.difference("b-a", positive = "b", negative = "a").evaluate(fit))
    assertEqualsDouble(diff.estimates(0), 4.0, 1e-12)
    assertEqualsDouble(diff.statistics(0), 4.0 / math.sqrt(2.0 / 3.0), 1e-12)
  }

  test("two-sample OLS p-value matches the t distribution") {
    val labels = Vector("a", "a", "a", "b", "b", "b")
    val design = value(GroupDesign.twoSample(labels))
    val effects = column(1.0, 2.0, 3.0, 5.0, 6.0, 7.0)
    val data = value(GroupData.single(subjects(6), GroupSpace.SampleAxis(1), "c", effects))
    val fit = value(GroupEngine.fit(value(GroupModel.build(data, design)))).fit("c").get
    // 2*pt(-4.898979, 4) = 0.008050.
    assertEqualsDouble(fit.term("b").get.pValues(0), 0.008050, 1e-4)
  }

  test("adjustedP applies BH to a contrast result's p-value map") {
    val space = GroupSpace.SampleAxis(3)
    val result = GroupContrastResult(
      name = GroupContrastName.unsafe("c"),
      estimates = Vec.zeros(3),
      standardErrors = Vec.zeros(3),
      statistics = Vec.zeros(3),
      pValues = DVec.fromSeq(Seq(0.005, 0.01, 0.5)),
      statistic = GroupStatistic.Normal,
      space = space
    )
    val q = result.adjustedP(FdrMethod.BenjaminiHochberg)
    assertEqualsDouble(q(0), 0.015, 1e-12)
    assertEqualsDouble(q(1), 0.015, 1e-12)
    assertEqualsDouble(q(2), 0.5, 1e-12)
  }
