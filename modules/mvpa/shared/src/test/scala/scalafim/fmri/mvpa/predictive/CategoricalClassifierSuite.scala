package scalafim.fmri.mvpa.predictive

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.mvpa.*

final class CategoricalClassifierSuite extends munit.FunSuite:
  private val a = ClassId.unsafe("a")
  private val b = ClassId.unsafe("b")
  private val classes = ClassAxis
    .create(
      AxisId.unsafe("classifier-court-classes"),
      Vector(a, b),
      CoordinateProvenance.unsafe("classifier-court", "v1")
    )
    .toOption
    .get
  private val labels = Vector(a, b, a, b, a, b, a, b)
  private val data = rows(
    Vector(
      Vector(2.0, 2.0, 0.0),
      Vector(-2.0, -2.0, 0.0),
      Vector(2.2, 1.8, 0.1),
      Vector(-2.1, -1.9, -0.1),
      Vector(1.9, 2.1, 0.0),
      Vector(-1.8, -2.2, 0.1),
      Vector(2.1, 2.0, -0.1),
      Vector(-2.0, -2.1, 0.0)
    )
  )

  test("correlation centroid retains the template oracle and common-offset law"):
    val method = CorrelationCentroid()
    val ordinary = fit(method, data)
    val shiftedData = Matrix.tabulate(data.rows, data.cols)((row, column) => data(row, column) + 100.0)
    val shifted = fit(method, shiftedData)

    val predicted = Vector.tabulate(data.rows): row =>
      ordinary.predict(rowValues(data, row)).toOption.get.predicted
    assertEquals(predicted, labels)
    Vector
      .range(0, data.rows)
      .foreach: row =>
        val left = ordinary.predict(rowValues(data, row)).toOption.get.scores
        val right = shifted.predict(rowValues(shiftedData, row)).toOption.get.scores
        left
          .zip(right)
          .foreach:
            case ((_, aScore), (_, bScore)) =>
              assertEqualsDouble(aScore.value, bScore.value, 1e-12)

  test("ridge-LDA matches the one-dimensional analytic score oracle"):
    val oracle = rows(Vector(Vector(0.0), Vector(2.0), Vector(4.0), Vector(6.0)))
    val model = fit(
      RidgeLda(ClassifierRidge(1.0).toOption.get),
      oracle,
      Vector(a, a, b, b)
    )
    val low = model.predict(rowValues(oracle, 0)).toOption.get
    val high = model.predict(rowValues(oracle, 3)).toOption.get
    val expected = 1.0 / (1.0 + math.exp(-2.4))

    assertEqualsDouble(softmax(low.scores)(0), expected, 1e-12)
    assertEqualsDouble(softmax(low.scores)(1), 1.0 - expected, 1e-12)
    assertEqualsDouble(softmax(high.scores)(0), 1.0 - expected, 1e-12)
    assertEqualsDouble(softmax(high.scores)(1), expected, 1e-12)

  test("centroid decision scores retain the elementary probability oracles"):
    val centroid = rows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(2.0, 0.0),
        Vector(0.0, 2.0)
      )
    )
    val correlation = fit(
      CorrelationCentroid(),
      centroid,
      Vector(a, b, a, b)
    ).predict(rowValues(centroid, 0)).toOption.get
    val expectedCorrelation = 1.0 / (1.0 + math.exp(-2.0))
    assertEqualsDouble(softmax(correlation.scores)(0), expectedCorrelation, 1e-12)

    val swiftPatterns = rows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0)
      )
    )
    val swift = fit(
      SwiftCentroid(PredictorScaling.None),
      swiftPatterns,
      Vector(a, b, a, b)
    ).predict(rowValues(swiftPatterns, 0)).toOption.get
    val expectedSwift = 1.0 / (1.0 + math.exp(-1.0))
    assertEqualsDouble(softmax(swift.scores)(0), expectedSwift, 1e-12)

  test("SWIFT scaling remains training-scoped and finite with a constant feature"):
    val methods = Vector(
      SwiftCentroid(PredictorScaling.ZScore),
      SwiftCentroid(
        PredictorScaling.DiagonalShrinkage(ScalingShrinkage(0.25).toOption.get)
      )
    )
    methods.foreach: method =>
      val model = fit(method, data)
      Vector
        .range(0, data.rows)
        .foreach: row =>
          val prediction = model.predict(rowValues(data, row)).toOption.get
          assert(prediction.scores.forall(_._2.value.isFinite))

  test("policy constructors and method identities fail closed"):
    assert(ScalingShrinkage(Double.NaN).isLeft)
    assert(ScalingShrinkage(-0.1).isLeft)
    assert(ClassifierRidge(0.0).isLeft)
    assert(ClassifierRidge(Double.PositiveInfinity).isLeft)

    val identities = Vector(
      ClassificationConfiguration(
        StandardizedNearestCentroid(
          StandardizationSpecification.CenterScaleEmitZero
        )
      ).toOption.get.identity,
      ClassificationConfiguration(CorrelationCentroid()).toOption.get.identity,
      ClassificationConfiguration(
        SwiftCentroid(PredictorScaling.ZScore)
      ).toOption.get.identity,
      ClassificationConfiguration(
        RidgeLda(ClassifierRidge(0.01).toOption.get)
      ).toOption.get.identity
    )
    assertEquals(identities.distinct.length, identities.length)

  private final class CourtModel[
      Configuration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ](
      fitted: Fitted,
      compiler: CategoricalLearnerCompiler[
        Configuration,
        Fitted,
        LearnerError,
        Prediction
      ]
  ):
    def predict(input: IArray[Double]): Either[LearnerError, Prediction] =
      compiler.predict(fitted, input)

  private def fit[
      Configuration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ](
      method: Configuration,
      value: DMat,
      trainingLabels: Vector[ClassId] = labels
  )(using
      compiler: CategoricalLearnerCompiler[
        Configuration,
        Fitted,
        LearnerError,
        Prediction
      ]
  ): CourtModel[Configuration, Fitted, LearnerError, Prediction] =
    val fitted = compiler.fit(method, classes, value, trainingLabels).toOption.get
    new CourtModel(fitted, compiler)

  private def rowValues(value: DMat, row: Int): IArray[Double] =
    IArray.tabulate(value.cols)(column => value(row, column))

  private def rows(values: Vector[Vector[Double]]): DMat =
    Matrix.tabulate(values.length, values.head.length)((row, column) => values(row)(column))

  private def softmax(values: Vector[(ClassId, DecisionScore)]): Vector[Double] =
    val maximum = values.map(_._2.value).max
    val weights = values.map(value => math.exp(value._2.value - maximum))
    val total = weights.sum
    weights.map(_ / total)
