package scalafim.fmri.group

import scalafim.dataset.SubjectId
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.linalg.DoubleMatrix

class GroupModelSuite extends munit.FunSuite:

  private def value[A](e: Either[GroupError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def subjects(n: Int): Vector[SubjectId] =
    (1 to n).toVector.map(i => SubjectId(s"s$i"))

  private def column(values: Double*): DoubleMatrix =
    DoubleMatrix.fromRows(values.toVector.map(v => Vector(v)))

  test("meta-analytic estimator on variance-free data is a typed error") {
    val data = value(GroupData.single(subjects(3), GroupSpace.SampleAxis(1), "c", column(1.0, 2.0, 3.0)))
    assertEquals(
      GroupModel.build(data, GroupDesign.intercept(3), GroupWeighting.InverseVariance).left.toOption,
      Some(GroupError.MissingVariances("meta:fe"))
    )
  }

  test("variance requirement narrows responses to the variance-carrying ADT case") {
    val effects = column(1.0, 2.0, 3.0)
    val variances = column(0.25, 0.5, 0.75)
    val weighted = value(GroupResponse.weighted(effects, variances))
    val narrowed = value((weighted: GroupResponse).requireVariances(GroupWeighting.InverseVariance))

    assertEquals(narrowed, weighted)
    assertEqualsDouble(narrowed.varianceMatrix(0, 0), 0.25, 1e-12)

    val effectsOnly = value(GroupResponse.fromEffects(effects))
    assertEquals(
      (effectsOnly: GroupResponse).requireVariances(GroupWeighting.InverseVariance).left.toOption,
      Some(GroupError.MissingVariances("meta:fe"))
    )
  }

  test("fewer subjects than terms is a typed error") {
    val labels = Vector("a", "b")
    val data = value(GroupData.single(subjects(2), GroupSpace.SampleAxis(1), "c", column(1.0, 2.0)))
    val design = value(GroupDesign.twoSample(labels))
    assertEquals(
      GroupModel.build(data, design).left.toOption,
      Some(GroupError.InsufficientSubjects(2, 2))
    )
  }

  test("design and data subject mismatch is a typed error") {
    val data = value(GroupData.single(subjects(5), GroupSpace.SampleAxis(1), "c", column(1.0, 2.0, 3.0, 4.0, 5.0)))
    assertEquals(
      GroupModel.build(data, GroupDesign.intercept(3)).left.toOption,
      Some(GroupError.SubjectMismatch(3, 5))
    )
  }

  test("non-positive variances are rejected at construction") {
    assertEquals(
      GroupResponse.weighted(column(1.0, 2.0), column(1.0, 0.0)).left.toOption,
      Some(GroupError.NonPositiveVariance)
    )
  }

  test("variance dims must match effect dims") {
    val effects = DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0)))
    val variances = column(1.0, 1.0)
    assert(GroupResponse.weighted(effects, variances).isLeft)
  }

  test("two-sample design requires exactly two levels") {
    assertEquals(
      GroupDesign.twoSample(Vector("a", "b", "c")).left.toOption,
      Some(GroupError.ContrastMismatch(2, 3))
    )
  }

  test("covariate design reads numeric columns and prepends an intercept") {
    val table = DataTable.fromColumns(
      "age" -> Column.Doubles(Vector(20.0, 30.0, 40.0)),
      "score" -> Column.Ints(Vector(1, 2, 3))
    )
    val design = value(GroupDesign.covariates(table, Vector("age", "score")))
    assertEquals(design.termNames, Vector("(Intercept)", "age", "score"))
    assertEquals(design.terms, 3)
    assertEqualsDouble(design.matrix(0, 0), 1.0, 1e-12)
    assertEqualsDouble(design.matrix(2, 1), 40.0, 1e-12)
    assertEqualsDouble(design.matrix(1, 2), 2.0, 1e-12)
  }

  test("covariate design reports an unknown column") {
    val table = DataTable.fromColumns("age" -> Column.Doubles(Vector(20.0, 30.0)))
    assertEquals(
      GroupDesign.covariates(table, Vector("age", "missing")).left.toOption,
      Some(GroupError.UnknownColumn("missing"))
    )
  }

  test("model summary reflects the composed description") {
    val data = value(GroupData.single(subjects(5), GroupSpace.SampleAxis(4), "c", DoubleMatrix.zeros(5, 4)))
    val summary = value(GroupModel.build(data, GroupDesign.intercept(5))).summary
    assertEquals(summary.subjects, 5)
    assertEquals(summary.samples, 4)
    assertEquals(summary.contrasts, 1)
    assertEquals(summary.terms, 1)
    assertEquals(summary.residualDf, 4)
    assertEquals(summary.weighting, GroupWeighting.Unweighted)
  }
