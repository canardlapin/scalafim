package scalafim.fmri.group

import scalafim.dataset.SubjectId
import gale.linalg.{DMat, Matrix}

class GroupGlmSuite extends munit.FunSuite:

  private def value[A](e: Either[GroupError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def subjects(n: Int): Vector[SubjectId] =
    (1 to n).toVector.map(i => SubjectId(s"s$i"))

  test("one-sample OLS reproduces the one-sample t-test") {
    // t.test(c(1,2,3,4,5)): mean 3, se sqrt(0.5), t 4.2426..., df 4.
    val effects = GroupTestMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0), Vector(4.0), Vector(5.0)))
    val data = value(GroupData.single(subjects(5), GroupSpace.SampleAxis(1), "c", effects))
    val model = value(GroupModel.build(data, GroupDesign.intercept(5)))
    val result = value(GroupEngine.fit(model))
    val fit = result.fit("c").get

    assertEquals(fit.statistic, GroupStatistic.unsafeStudentT(4))
    val intercept = fit.term("(Intercept)").get
    assertEqualsDouble(intercept.estimates(0), 3.0, 1e-12)
    assertEqualsDouble(intercept.standardErrors(0), math.sqrt(0.5), 1e-12)
    assertEqualsDouble(intercept.statistics(0), 3.0 / math.sqrt(0.5), 1e-12)
    // Two-sided p from t(4) at t = 4.2426 is ~0.0132355 (hand-integrated I_x(2, .5)).
    assertEqualsDouble(intercept.pValues(0), 0.0132355, 1e-4)
  }

  test("two-sample OLS reproduces the pooled two-sample t-test") {
    // groups a = {1,2,3}, b = {5,6,7}: diff 4, pooled se sqrt(2/3), t 4.898979, df 4.
    val labels = Vector("a", "a", "a", "b", "b", "b")
    val effects = GroupTestMatrix.fromRows(
      Vector(Vector(1.0), Vector(2.0), Vector(3.0), Vector(5.0), Vector(6.0), Vector(7.0))
    )
    val design = value(GroupDesign.twoSample(labels))
    val data = value(GroupData.single(subjects(6), GroupSpace.SampleAxis(1), "c", effects))
    val model = value(GroupModel.build(data, design))
    val fit = value(GroupEngine.fit(model)).fit("c").get

    assertEquals(fit.statistic, GroupStatistic.unsafeStudentT(4))
    // Intercept is the reference-group (a) mean.
    assertEqualsDouble(fit.term("(Intercept)").get.estimates(0), 2.0, 1e-12)
    val diff = fit.term("b").get
    assertEqualsDouble(diff.estimates(0), 4.0, 1e-12)
    assertEqualsDouble(diff.standardErrors(0), math.sqrt(2.0 / 3.0), 1e-12)
    assertEqualsDouble(diff.statistics(0), 4.0 / math.sqrt(2.0 / 3.0), 1e-12)
  }

  test("group contrast on the difference term equals the term statistic") {
    val labels = Vector("a", "a", "a", "b", "b", "b")
    val effects = GroupTestMatrix.fromRows(
      Vector(Vector(1.0), Vector(2.0), Vector(3.0), Vector(5.0), Vector(6.0), Vector(7.0))
    )
    val design = value(GroupDesign.twoSample(labels))
    val data = value(GroupData.single(subjects(6), GroupSpace.SampleAxis(1), "c", effects))
    val fit = value(GroupEngine.fit(value(GroupModel.build(data, design)))).fit("c").get

    val contrast = GroupContrast.term("b")
    val evaluated = value(contrast.evaluate(fit))
    assertEqualsDouble(evaluated.statistics(0), fit.term("b").get.statistics(0), 1e-12)
  }

  test("OLS fits every sample independently") {
    // Column 0: intercept mean 3; column 1: constant 10 (zero residual, t = inf).
    val effects = GroupTestMatrix.fromRows(
      Vector(Vector(1.0, 10.0), Vector(2.0, 10.0), Vector(3.0, 10.0), Vector(4.0, 10.0), Vector(5.0, 10.0))
    )
    val data = value(GroupData.single(subjects(5), GroupSpace.SampleAxis(2), "c", effects))
    val fit = value(GroupEngine.fit(value(GroupModel.build(data, GroupDesign.intercept(5))))).fit("c").get
    val intercept = fit.term("(Intercept)").get
    assertEqualsDouble(intercept.estimates(0), 3.0, 1e-12)
    assertEqualsDouble(intercept.estimates(1), 10.0, 1e-12)
    assertEqualsDouble(intercept.standardErrors(1), 0.0, 1e-12)
  }

  test("unknown contrast term is a typed error") {
    val effects = GroupTestMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0)))
    val data = value(GroupData.single(subjects(3), GroupSpace.SampleAxis(1), "c", effects))
    val fit = value(GroupEngine.fit(value(GroupModel.build(data, GroupDesign.intercept(3))))).fit("c").get
    assertEquals(GroupContrast.term("missing").evaluate(fit).left.toOption, Some(GroupError.UnknownContrastTerm("missing")))
  }
