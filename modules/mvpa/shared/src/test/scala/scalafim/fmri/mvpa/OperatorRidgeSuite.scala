package scalafim.fmri.mvpa

import gale.linalg.{CholeskyOptions, DMat, Matrix}
import multivar.core.ValueId
import scalafim.fmri.mvpa.predictive.*

final class OperatorRidgeSuite extends munit.FunSuite:
  private val samples =
    AxisRef
      .create(
        AxisId.unsafe("operator-ridge-kernel-samples"),
        AxisPurpose.Samples,
        Vector.tabulate(12)(index => SampleId.unsafe(s"sample-$index")),
        CoordinateBasis.unsafe("trial-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("operator-ridge-kernel", "v1")
      )
      .toOption
      .get
  private val features =
    AxisRef
      .create(
        AxisId.unsafe("operator-ridge-kernel-features"),
        AxisPurpose.NeuralFeatures,
        Vector.tabulate(4)(index => FeatureId.unsafe(s"voxel-$index")),
        CoordinateBasis.unsafe("voxel-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("operator-ridge-kernel", "v1")
      )
      .toOption
      .get
  private val classes =
    ClassAxis
      .create(
        AxisId.unsafe("operator-ridge-kernel-classes"),
        Vector(ClassId.unsafe("a"), ClassId.unsafe("b"), ClassId.unsafe("c")),
        CoordinateProvenance.unsafe("operator-ridge-kernel", "v1")
      )
      .toOption
      .get
  private val values = fromRows(
    Vector(
      Vector(2.0, 0.8, 0.2, 1.0),
      Vector(-1.8, -0.9, 0.4, -0.5),
      Vector(0.1, 2.1, -1.0, 0.3),
      Vector(2.2, 1.1, 0.0, 0.8),
      Vector(-2.1, -1.2, 0.1, -0.7),
      Vector(-0.2, 1.8, -1.2, 0.5),
      Vector(1.9, 0.9, 0.3, 1.2),
      Vector(-1.9, -1.0, 0.2, -0.4),
      Vector(0.2, 2.2, -0.8, 0.4),
      Vector(2.1, 1.0, 0.1, 0.9),
      Vector(-2.2, -0.8, 0.3, -0.6),
      Vector(0.0, 1.9, -1.1, 0.2)
    )
  )
  private val evidence =
    EvidenceTable
      .dense(samples, features, values, ValueId.unsafe("operator-ridge-kernel-values"))
      .toOption
      .get
  private val target =
    Matrix.tabulate(samples.size, classes.size): (row, column) =>
      if column == row % classes.size then 1.0 else 0.0
  private val penalty = RidgePenalty.unsafe(0.7)
  private val solver = OperatorRidgeSolverSettings.unsafe(1e-12, 2000)

  test("analytic centered ridge has an unpenalized intercept"):
    val localSamples =
      AxisRef
        .create(
          AxisId.unsafe("operator-ridge-analytic-samples"),
          AxisPurpose.Samples,
          Vector.tabulate(4)(index => SampleId.unsafe(s"analytic-$index")),
          CoordinateBasis.unsafe("analytic-order"),
          None,
          AxisScale.nominal,
          CoordinateProvenance.unsafe("operator-ridge-analytic", "v1")
        )
        .toOption
        .get
    val localFeatures =
      AxisRef
        .create(
          AxisId.unsafe("operator-ridge-analytic-features"),
          AxisPurpose.NeuralFeatures,
          Vector(FeatureId.unsafe("signal"), FeatureId.unsafe("constant")),
          CoordinateBasis.unsafe("analytic-features"),
          None,
          AxisScale.nominal,
          CoordinateProvenance.unsafe("operator-ridge-analytic", "v1")
        )
        .toOption
        .get
    val localClasses =
      ClassAxis
        .create(
          AxisId.unsafe("operator-ridge-analytic-classes"),
          Vector(ClassId.unsafe("a"), ClassId.unsafe("b")),
          CoordinateProvenance.unsafe("operator-ridge-analytic", "v1")
        )
        .toOption
        .get
    val x = fromRows(
      Vector(
        Vector(-1.0, 5.0),
        Vector(1.0, 5.0),
        Vector(-1.0, 5.0),
        Vector(1.0, 5.0)
      )
    )
    val y = fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0)
      )
    )
    val table = EvidenceTable
      .dense(localSamples, localFeatures, x, ValueId.unsafe("analytic-values"))
      .toOption
      .get
    val model = OperatorRidgeKernel
      .fit(
        table,
        localClasses,
        y,
        RidgePenalty.unsafe(1.0),
        OperatorRidgeSolverSettings.unsafe(1e-13, 2000),
        "analytic"
      )
      .toOption
      .get
    val scores = model.predict(table).toOption.get

    assertEqualsDouble(model.coefficients(0, 0), -0.4, 1e-10)
    assertEqualsDouble(model.coefficients(0, 1), 0.4, 1e-10)
    assertEqualsDouble(model.coefficients(1, 0), 0.0, 1e-10)
    assertEqualsDouble(model.coefficients(1, 1), 0.0, 1e-10)
    assertEqualsDouble(model.intercepts(0), 0.5, 1e-10)
    assertEqualsDouble(model.intercepts(1), 0.5, 1e-10)
    assertMatrixClose(
      scores,
      fromRows(
        Vector(
          Vector(0.9, 0.1),
          Vector(0.1, 0.9),
          Vector(0.9, 0.1),
          Vector(0.1, 0.9)
        )
      ),
      1e-9
    )
    assertEquals(model.receipt.trainingAxis, localSamples.identity)
    assertEquals(model.receipt.featureAxis, localFeatures.identity)
    assertEquals(model.receipt.classAxis, localClasses.identity)
    assert(model.receipt.operatorApplications > 0L)

  test("operator LSQR matches an independent dense normal-equation oracle"):
    val model = OperatorRidgeKernel
      .fit(evidence, classes, target, penalty, solver, "dense-oracle")
      .toOption
      .get
    val predicted = model.predict(evidence).toOption.get
    val oracle = denseRidge(values, target, penalty.value)
    val expectedScores = denseScores(values, oracle._1, oracle._2)

    assertMatrixClose(model.coefficients, oracle._1, 1e-7)
    assertVectorClose(model.intercepts, oracle._2, 1e-7)
    assertMatrixClose(predicted, expectedScores, 1e-7)

  test("feature translation preserves fitted scores"):
    val baseline = OperatorRidgeKernel
      .fit(evidence, classes, target, penalty, solver, "baseline")
      .toOption
      .get
      .predict(evidence)
      .toOption
      .get
    val translated = Matrix.tabulate(values.rows, values.cols): (row, column) =>
      values(row, column) + Vector(10.0, -3.0, 7.5, 2.0)(column)
    val translatedEvidence = EvidenceTable
      .dense(samples, features, translated, ValueId.unsafe("translated-values"))
      .toOption
      .get
    val translatedScores = OperatorRidgeKernel
      .fit(translatedEvidence, classes, target, penalty, solver, "translated")
      .toOption
      .get
      .predict(translatedEvidence)
      .toOption
      .get

    assertMatrixClose(translatedScores, baseline, 1e-7)

  test("solver decoding and convergence failure are explicit"):
    val decoded = OperatorRidgeSolverSettings
      .from(OperatorRidgeSolverSettings.choice(solver))
      .toOption
      .get
    assertEquals(decoded.identity, solver.identity)
    assertEqualsDouble(decoded.tolerance.value, 1e-12, 0.0)
    assertEquals(decoded.maxIterations.value, 2000)

    val oneStep = OperatorRidgeKernel.fit(
      evidence,
      classes,
      target,
      RidgePenalty.unsafe(1e-4),
      OperatorRidgeSolverSettings.unsafe(1e-30, 1),
      "one-step"
    )
    assert(oneStep.left.exists {
      case OperatorRidgeError.DidNotConverge(
            "one-step",
            _,
            1,
            residual
          ) =>
        residual.isFinite && residual >= 0.0
      case _ => false
    })

  test("prediction requires the fitted feature witness at compile time"):
    val errors = compileErrors("""
      import multivar.core.SemanticSpace
      import scalafim.fmri.mvpa.*
      import scalafim.fmri.mvpa.predictive.*
      def foreign[A <: SemanticSpace, B <: SemanticSpace](
          model: OperatorRidgeKernelModel[A, FeatureId],
          test: EvidenceTable[A, B, SampleId, FeatureId]
      ) = model.predict(test)
    """)
    assert(errors.nonEmpty)

  private def denseRidge(
      x: DMat,
      y: DMat,
      lambda: Double
  ): (DMat, Vector[Double]) =
    val featureMeans = columnMeans(x)
    val targetMeans = columnMeans(y)
    val centeredX = Matrix.tabulate(x.rows, x.cols): (row, column) =>
      x(row, column) - featureMeans(column)
    val centeredY = Matrix.tabulate(y.rows, y.cols): (row, column) =>
      y(row, column) - targetMeans(column)
    val gram = centeredX.t * centeredX
    val system = Matrix.tabulate(x.cols, x.cols): (row, column) =>
      gram(row, column) + (if row == column then lambda else 0.0)
    val rhs = centeredX.t * centeredY
    val coefficients = system
      .cholesky(CholeskyOptions(1e-12))
      .toOption
      .get
      .solve(rhs)
      .toOption
      .get
    val intercepts = Vector.tabulate(y.cols): column =>
      var contribution = 0.0
      var feature = 0
      while feature < x.cols do
        contribution += featureMeans(feature) * coefficients(feature, column)
        feature += 1
      targetMeans(column) - contribution
    (coefficients, intercepts)

  private def denseScores(
      x: DMat,
      coefficients: DMat,
      intercepts: Vector[Double]
  ): DMat =
    val raw = x * coefficients
    Matrix.tabulate(raw.rows, raw.cols): (row, column) =>
      raw(row, column) + intercepts(column)

  private def columnMeans(value: DMat): Vector[Double] =
    Vector.tabulate(value.cols): column =>
      var total = 0.0
      var row = 0
      while row < value.rows do
        total += value(row, column)
        row += 1
      total / value.rows.toDouble

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, column) => rows(row)(column))

  private def assertMatrixClose(
      actual: DMat,
      expected: DMat,
      tolerance: Double
  ): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertEqualsDouble(actual(row, column), expected(row, column), tolerance)
        column += 1
      row += 1

  private def assertVectorClose(
      actual: IArray[Double],
      expected: Vector[Double],
      tolerance: Double
  ): Unit =
    assertEquals(actual.length, expected.length)
    var index = 0
    while index < actual.length do
      assertEqualsDouble(actual(index), expected(index), tolerance)
      index += 1
