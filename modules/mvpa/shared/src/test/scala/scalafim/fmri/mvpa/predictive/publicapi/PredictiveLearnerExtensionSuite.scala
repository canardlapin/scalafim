package downstream.predictive

import alder.tune.PositiveInt
import alder.kernel.{AuditValue, BackendFingerprint, ComponentId}
import gale.linalg.{DMat, Matrix}
import multivar.core.ValueId
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.predictive.*
import scalafim.fmri.mvpa.predictive.PredictiveAnalysis.given

final class ExternalMean private (
    val minimumFeatures: PositiveInt,
    private val learnerDefinition: CategoricalLearnerDefinition
)

final class ExternalMeanFit(
    val classes: AxisRef[ClassId],
    val featureCount: Int,
    val classMeans: DMat
)

enum ExternalMeanError:
  case EmptyTrainingData
  case DimensionMismatch(expected: Int, actual: Int)
  case UnknownClass(label: ClassId)
  case MissingClass(label: ClassId)
  case NonFiniteCoordinate(column: Int, value: Double)
  case InvalidScore(label: ClassId, value: Double)

  def message: String =
    this match
      case EmptyTrainingData                   => "external mean learner requires training rows"
      case DimensionMismatch(expected, actual) =>
        s"external mean learner expected $expected coordinates, obtained $actual"
      case UnknownClass(label)                => s"external mean learner observed '${label.value}'"
      case MissingClass(label)                => s"external mean learner omitted '${label.value}'"
      case NonFiniteCoordinate(column, value) =>
        s"external mean learner coordinate $column is $value"
      case InvalidScore(label, value) =>
        s"external mean learner score for '${label.value}' is $value"

final case class ExternalMeanPrediction(
    predicted: ClassId,
    scores: Vector[(ClassId, DecisionScore)],
    winningMargin: Double
) extends CategoricalLearnerPrediction

object ExternalMean:
  def apply(
      minimumFeatures: PositiveInt
  ): Either[ScientificIdentityError, ExternalMean] =
    for
      id <- CategoricalLearnerId("external-mean")
      definition <- CategoricalLearnerDefinition(
        id,
        ComponentId("downstream.predictive.external-mean"),
        BackendFingerprint(
          "downstream-predictive-court",
          "external-mean-v1",
          AuditValue.record()
        ),
        minimumFeatures,
        s"minimum-features:${minimumFeatures.toInt}",
        "negative-squared-euclidean-distance",
        "none"
      )
    yield new ExternalMean(minimumFeatures, definition)

  given compiler: CategoricalLearnerCompiler[
    ExternalMean,
    ExternalMeanFit,
    ExternalMeanError,
    ExternalMeanPrediction
  ] with
    override def definition(configuration: ExternalMean): CategoricalLearnerDefinition =
      configuration.learnerDefinition

    override def fit(
        configuration: ExternalMean,
        classes: AxisRef[ClassId],
        data: DMat,
        labels: Vector[ClassId]
    ): Either[ExternalMeanError, ExternalMeanFit] =
      if data.rows == 0 then Left(ExternalMeanError.EmptyTrainingData)
      else if labels.length != data.rows then Left(ExternalMeanError.DimensionMismatch(data.rows, labels.length))
      else if data.cols < configuration.minimumFeatures.toInt then
        Left(
          ExternalMeanError.DimensionMismatch(
            configuration.minimumFeatures.toInt,
            data.cols
          )
        )
      else
        val positions = classes.keys.zipWithIndex.toMap
        val counts = Array.fill(classes.size)(0)
        val sums = Array.fill(classes.size * data.cols)(0.0)
        var row = 0
        while row < data.rows do
          val klass = positions.get(labels(row)) match
            case None        => return Left(ExternalMeanError.UnknownClass(labels(row)))
            case Some(value) => value
          counts(klass) += 1
          var column = 0
          while column < data.cols do
            val value = data(row, column)
            if !value.isFinite then return Left(ExternalMeanError.NonFiniteCoordinate(column, value))
            sums(klass * data.cols + column) += value
            column += 1
          row += 1
        var klass = 0
        while klass < classes.size do
          if counts(klass) == 0 then return Left(ExternalMeanError.MissingClass(classes.keys(klass)))
          klass += 1
        val means = Matrix.newBuilder(classes.size, data.cols)
        klass = 0
        while klass < classes.size do
          var column = 0
          while column < data.cols do
            means(klass, column) = sums(klass * data.cols + column) / counts(klass).toDouble
            column += 1
          klass += 1
        Right(new ExternalMeanFit(classes, data.cols, means.result()))

    override def predict(
        fitted: ExternalMeanFit,
        input: IArray[Double]
    ): Either[ExternalMeanError, ExternalMeanPrediction] =
      if input.length != fitted.featureCount then
        Left(
          ExternalMeanError.DimensionMismatch(
            fitted.featureCount,
            input.length
          )
        )
      else
        val scores = Vector.newBuilder[(ClassId, DecisionScore)]
        var best = 0
        var bestScore = Double.NegativeInfinity
        var secondScore = Double.NegativeInfinity
        var klass = 0
        while klass < fitted.classes.size do
          var squared = 0.0
          var column = 0
          while column < fitted.featureCount do
            val value = input(column)
            if !value.isFinite then return Left(ExternalMeanError.NonFiniteCoordinate(column, value))
            val delta = value - fitted.classMeans(klass, column)
            squared += delta * delta
            column += 1
          val raw = -squared
          val label = fitted.classes.keys(klass)
          val score = DecisionScore(raw) match
            case Left(_)      => return Left(ExternalMeanError.InvalidScore(label, raw))
            case Right(value) => value
          scores += label -> score
          if raw > bestScore then
            secondScore = bestScore
            bestScore = raw
            best = klass
          else if raw > secondScore then secondScore = raw
          klass += 1
        Right(
          ExternalMeanPrediction(
            fitted.classes.keys(best),
            scores.result(),
            bestScore - secondScore
          )
        )

    override def failureMessage(error: ExternalMeanError): String =
      error.message

final class PredictiveLearnerExtensionSuite extends munit.FunSuite:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private def admitted[E, A](result: Either[E, A]): A =
    result match
      case Right(value) => value
      case Left(error)  => fail(s"fixture admission failed: $error")

  private val cat = admitted(ClassId("cat"))
  private val dog = admitted(ClassId("dog"))
  private val samples = admitted(
    AxisRef.create(
      admitted(AxisId("external-learner-samples")),
      AxisPurpose.Samples,
      Vector.tabulate(8)(index => admitted(SampleId(s"sample-$index"))),
      admitted(CoordinateBasis("run-trial-table")),
      None,
      AxisScale.nominal,
      admitted(CoordinateProvenance("downstream-court", "v1"))
    )
  )
  private val features = admitted(
    AxisRef.create(
      admitted(AxisId("external-learner-features")),
      AxisPurpose.NeuralFeatures,
      Vector(admitted(FeatureId("x")), admitted(FeatureId("y"))),
      admitted(CoordinateBasis("voxel-table")),
      None,
      AxisScale.nominal,
      admitted(CoordinateProvenance("downstream-court", "v1"))
    )
  )
  private val classes = admitted(
    ClassAxis.create(
      admitted(AxisId("external-learner-classes")),
      Vector(cat, dog),
      admitted(CoordinateProvenance("downstream-court", "v1"))
    )
  )
  private val labels = Vector(cat, dog, cat, dog, cat, dog, cat, dog)
  private val target = admitted(
    CategoricalTarget(
      samples,
      classes,
      admitted(Column(samples, labels))
    )
  )
  private val evidence = admitted(
    EvidenceTable.dense(
      samples,
      features,
      DMat.dense(
        8,
        2,
        Vector(
          -4.0, -1.0, 4.0, 1.0, -3.0, -0.5, 3.0, 0.5, -2.0, -1.5, 2.0, 1.5, -5.0, -0.25, 5.0, 0.25
        )
      ),
      admitted(ValueId("external-learner-patterns"))
    )
  )
  private val measurement = admitted(
    Measurement.hardSelection(
      features,
      admitted(MeasurementId("external-learner-global")),
      admitted(
        Injection.from(
          indices(0, 1),
          admitted(IndexSpace.of(features.size))
        )
      )
    )
  )
  private val frame = admitted(
    MeasurementFrame(features)(
      Vector(MeasurementEntry(measurement, NoRendition))
    )
  )
  private val design =
    val groups = admitted(
      Labels.dense(indices(0, 0, 1, 1, 2, 2, 3, 3), samples.size)
    )
    val ordinal = LeaveOneGroupOut(groups)
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 419L)
    val indexSpace = admitted(IndexSpace.of(samples.size))
    val compiled = admitted(ordinal.compile(indexSpace, authority.seed))
    val schedule = admitted(
      BoundSchedule(
        compiled,
        samples,
        admitted(AxisPopulationFingerprint.fromAxis(samples)),
        admitted(ScheduleLabels.fromDesign(samples, ordinal)),
        authority
      )
    )
    val samplesAxis = admitted(ScientificAxisName("samples"))
    admitted(
      ValidationDesign(
        schedule,
        samplesAxis,
        GeneralizationAxis(samplesAxis, samples.identity)
      )
    )
  private val materialization = MaterializationPolicy.Allow(
    admitted(MaterializationBudget(16L))
  )
  private val strategy = admitted(
    ExecutionStrategy(
      admitted(BackendId("external-learner-court")),
      ExecutionRepresentation.Dense,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      materialization,
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    )
  )

  private def indices(values: Int*): IArray[Int] =
    IArray.unsafeFromArray(values.toArray)

  test("a downstream learner compiles and runs without a ScalaFIM registry edit"):
    val configuration = admitted(
      ClassificationConfiguration(admitted(ExternalMean(PositiveInt.one)))
    )
    val plan = admitted(
      ScientificPlanFingerprint(
        "scalafim-mvpa-plan-v1-" + Vector.fill(64)("e").mkString
      )
    )
    val result = admitted(
      ClassificationCompiler.run(
        plan,
        evidence,
        target,
        design,
        measurement,
        materialization,
        configuration
      )
    )

    assertEquals(result.predictions.predicted.toVector, labels)
    assertEqualsDouble(result.accuracy.value, 1.0, 1e-12)
    val externalResults = result.foldEstimates.flatMap(_._2.observations)
    assert(externalResults.forall(_.prediction.winningMargin > 0.0))
    assertEquals(
      result.foldReceipts.map(_.fitAudit.component.id.render).distinct,
      Vector("downstream.predictive.external-mean")
    )

  test("an incompatible downstream learner is rejected during binding"):
    val configuration = admitted(
      ClassificationConfiguration(admitted(ExternalMean(PositiveInt.const(3))))
    )
    val source = admitted(CategoricalObservationSource(evidence, target))

    Mvpa.run(source)(
      design,
      frame,
      source.classify(configuration),
      strategy
    ) match
      case Left(
            MvpaRunError.Binding(
              BindError.EstimandRejected(
                _,
                ClassificationBindRejection.MeasurementTooSmall(
                  measurementId,
                  3,
                  2
                ),
                _
              )
            )
          ) =>
        assertEquals(measurementId, measurement.identity.id)
      case other => fail(s"expected typed incompatible-learner rejection, obtained $other")
