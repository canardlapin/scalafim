package scalafim.fmri.mvpa.predictive

import alder.tune.PositiveInt
import alder.kernel.{AuditValue, BackendFingerprint, ComponentId}
import gale.linalg.{CholeskyOptions, DMat, Matrix}
import scalafim.fmri.mvpa.*

opaque type ScalingShrinkage = Double

object ScalingShrinkage:
  def apply(value: Double): Either[CategoricalClassifierError, ScalingShrinkage] =
    if value.isFinite && value >= 0.0 && value <= 1.0 then Right(value)
    else Left(CategoricalClassifierError.InvalidScalingShrinkage(value))

  private[mvpa] def unsafe(value: Double): ScalingShrinkage =
    value

  extension (value: ScalingShrinkage) inline def toDouble: Double = value

opaque type ClassifierRidge = Double

object ClassifierRidge:
  def apply(value: Double): Either[CategoricalClassifierError, ClassifierRidge] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(CategoricalClassifierError.InvalidRidge(value))

  private[mvpa] def unsafe(value: Double): ClassifierRidge =
    value

  extension (value: ClassifierRidge) inline def toDouble: Double = value

enum PredictorScaling:
  case None
  case ZScore
  case DiagonalShrinkage(fraction: ScalingShrinkage)

  def identity: String =
    this match
      case None                        => "none"
      case ZScore                      => "z-score"
      case DiagonalShrinkage(fraction) =>
        s"diagonal-shrinkage:${java.lang.Double.toHexString(fraction.toDouble)}"

enum ClassifierAdaptation:
  case InductiveWithinDomain
  case InductiveFrozenSourceDomain

  def identity: String =
    this match
      case InductiveWithinDomain       => "inductive-within-domain"
      case InductiveFrozenSourceDomain => "inductive-frozen-source-domain"

opaque type CategoricalLearnerId = String

object CategoricalLearnerId:
  def apply(value: String): Either[ScientificIdentityError, CategoricalLearnerId] =
    ScientificIdentityText.lowerIdentifier("categorical learner id", value)

  private[mvpa] def unsafe(value: String): CategoricalLearnerId =
    value

  extension (value: CategoricalLearnerId) inline def text: String = value

/** Auditable scientific meaning and input requirement for one open learner. These values identify results; learner
  * selection is performed by the statically supplied [[CategoricalLearnerCompiler]], never by these strings.
  */
final class CategoricalLearnerDefinition private (
    val id: CategoricalLearnerId,
    val componentId: ComponentId,
    val backend: BackendFingerprint,
    val minimumFeatures: PositiveInt,
    val parameterIdentity: String,
    val scoreIdentity: String,
    val preparationIdentity: String
)

object CategoricalLearnerDefinition:
  def apply(
      id: CategoricalLearnerId,
      componentId: ComponentId,
      backend: BackendFingerprint,
      minimumFeatures: PositiveInt,
      parameterIdentity: String,
      scoreIdentity: String,
      preparationIdentity: String
  ): Either[ScientificIdentityError, CategoricalLearnerDefinition] =
    ScientificIdentityComponents
      .fields(
        Vector(
          "classifier-parameter" -> parameterIdentity,
          "decision-score" -> scoreIdentity,
          "standardization" -> preparationIdentity
        )
      )
      .map(_ =>
        new CategoricalLearnerDefinition(
          id,
          componentId,
          backend,
          minimumFeatures,
          parameterIdentity,
          scoreIdentity,
          preparationIdentity
        )
      )

  private[mvpa] def unsafe(
      id: CategoricalLearnerId,
      componentId: ComponentId,
      backend: BackendFingerprint,
      minimumFeatures: PositiveInt,
      parameterIdentity: String,
      scoreIdentity: String,
      preparationIdentity: String
  ): CategoricalLearnerDefinition =
    new CategoricalLearnerDefinition(
      id,
      componentId,
      backend,
      minimumFeatures,
      parameterIdentity,
      scoreIdentity,
      preparationIdentity
    )

/** Minimum result contract required by scientific reconstruction. A learner remains free to return a richer concrete
  * prediction type.
  */
trait CategoricalLearnerPrediction:
  def predicted: ClassId
  def scores: Vector[(ClassId, DecisionScore)]

/** Open compilation capability for a categorical learner configuration. Configuration, fitted artifact, failure, and
  * prediction stay exact through Alder fitting and the public result type.
  */
trait CategoricalLearnerCompiler[
    Configuration,
    Fitted,
    LearnerError,
    Prediction <: CategoricalLearnerPrediction
]:
  def definition(configuration: Configuration): CategoricalLearnerDefinition

  def fit(
      configuration: Configuration,
      classes: AxisRef[ClassId],
      data: DMat,
      labels: Vector[ClassId]
  ): Either[LearnerError, Fitted]

  def predict(
      fitted: Fitted,
      input: IArray[Double]
  ): Either[LearnerError, Prediction]

  def failureMessage(error: LearnerError): String

final case class StandardizedNearestCentroid(
    zeroVariance: StandardizationSpecification
)

final case class CorrelationCentroid()

final case class SwiftCentroid(scaling: PredictorScaling)

final case class RidgeLda(penalty: ClassifierRidge)

final class StandardizedNearestCentroidFit private[predictive] (
    private[predictive] val classes: AxisRef[ClassId],
    private[predictive] val featureCount: Int,
    private[predictive] val means: Array[Double],
    private[predictive] val scales: Array[Double],
    private[predictive] val classMeans: DMat
)

final class CorrelationCentroidFit private[predictive] (
    private[predictive] val classes: AxisRef[ClassId],
    private[predictive] val featureCount: Int,
    private[predictive] val classMeans: DMat
)

final class SwiftCentroidFit private[predictive] (
    private[predictive] val classes: AxisRef[ClassId],
    private[predictive] val featureCount: Int,
    private[predictive] val means: Array[Double],
    private[predictive] val scales: Array[Double],
    private[predictive] val classMeans: DMat,
    private[predictive] val priors: Vector[Double]
)

final class RidgeLdaFit private[predictive] (
    private[predictive] val classes: AxisRef[ClassId],
    private[predictive] val featureCount: Int,
    private[predictive] val coefficients: DMat,
    private[predictive] val constants: Vector[Double]
)

private object CategoricalBuiltInBackend:
  def apply(implementation: String): BackendFingerprint =
    BackendFingerprint(
      "scalafim-gale",
      implementation,
      AuditValue.record()
    )

object StandardizedNearestCentroid:
  given compiler: CategoricalLearnerCompiler[
    StandardizedNearestCentroid,
    StandardizedNearestCentroidFit,
    CategoricalClassifierError,
    AlderClassPrediction
  ] with
    override def definition(
        configuration: StandardizedNearestCentroid
    ): CategoricalLearnerDefinition =
      CategoricalLearnerDefinition.unsafe(
        CategoricalLearnerId.unsafe("standardized-nearest-centroid"),
        ComponentId("scalafim.mvpa.standardized-nearest-centroid"),
        CategoricalBuiltInBackend("standardized-nearest-centroid-v1"),
        PositiveInt.one,
        "none",
        "negative-squared-euclidean-distance",
        configuration.zeroVariance.identity
      )

    override def fit(
        configuration: StandardizedNearestCentroid,
        classes: AxisRef[ClassId],
        data: DMat,
        labels: Vector[ClassId]
    ): Either[CategoricalClassifierError, StandardizedNearestCentroidFit] =
      CategoricalTraining
        .prepare(
          classes,
          data,
          labels,
          PredictorScaling.ZScore,
          Some(configuration.zeroVariance),
          populationVariance = true,
          PositiveInt.one
        )
        .map(value =>
          new StandardizedNearestCentroidFit(
            classes,
            data.cols,
            value.means,
            value.scales,
            value.classMeans
          )
        )

    override def predict(
        fitted: StandardizedNearestCentroidFit,
        input: IArray[Double]
    ): Either[CategoricalClassifierError, AlderClassPrediction] =
      CategoricalPredictionKernel
        .transform(input, fitted.featureCount, fitted.means, fitted.scales)
        .flatMap(row =>
          CategoricalPredictionKernel.nearestCentroid(
            fitted.classes,
            fitted.classMeans,
            row
          )
        )

    override def failureMessage(error: CategoricalClassifierError): String =
      error.message

object CorrelationCentroid:
  given compiler: CategoricalLearnerCompiler[
    CorrelationCentroid,
    CorrelationCentroidFit,
    CategoricalClassifierError,
    AlderClassPrediction
  ] with
    override def definition(
        configuration: CorrelationCentroid
    ): CategoricalLearnerDefinition =
      CategoricalLearnerDefinition.unsafe(
        CategoricalLearnerId.unsafe("correlation-centroid"),
        ComponentId("scalafim.mvpa.correlation-centroid"),
        CategoricalBuiltInBackend("correlation-centroid-v1"),
        PositiveInt.const(2),
        "none",
        "pearson-template-correlation",
        PredictorScaling.None.identity
      )

    override def fit(
        configuration: CorrelationCentroid,
        classes: AxisRef[ClassId],
        data: DMat,
        labels: Vector[ClassId]
    ): Either[CategoricalClassifierError, CorrelationCentroidFit] =
      CategoricalTraining
        .prepare(
          classes,
          data,
          labels,
          PredictorScaling.None,
          None,
          populationVariance = false,
          PositiveInt.const(2)
        )
        .map(value => new CorrelationCentroidFit(classes, data.cols, value.classMeans))

    override def predict(
        fitted: CorrelationCentroidFit,
        input: IArray[Double]
    ): Either[CategoricalClassifierError, AlderClassPrediction] =
      CategoricalPredictionKernel
        .unscaled(input, fitted.featureCount)
        .flatMap(row =>
          CategoricalPredictionKernel.correlation(
            fitted.classes,
            fitted.classMeans,
            row
          )
        )

    override def failureMessage(error: CategoricalClassifierError): String =
      error.message

object SwiftCentroid:
  given compiler: CategoricalLearnerCompiler[
    SwiftCentroid,
    SwiftCentroidFit,
    CategoricalClassifierError,
    AlderClassPrediction
  ] with
    override def definition(
        configuration: SwiftCentroid
    ): CategoricalLearnerDefinition =
      CategoricalLearnerDefinition.unsafe(
        CategoricalLearnerId.unsafe("swift-centroid"),
        ComponentId("scalafim.mvpa.swift-centroid"),
        CategoricalBuiltInBackend("swift-centroid-v1"),
        PositiveInt.one,
        "none",
        "linear-centroid-discriminant",
        configuration.scaling.identity
      )

    override def fit(
        configuration: SwiftCentroid,
        classes: AxisRef[ClassId],
        data: DMat,
        labels: Vector[ClassId]
    ): Either[CategoricalClassifierError, SwiftCentroidFit] =
      CategoricalTraining
        .prepare(
          classes,
          data,
          labels,
          configuration.scaling,
          None,
          populationVariance = false,
          PositiveInt.one
        )
        .map(value =>
          new SwiftCentroidFit(
            classes,
            data.cols,
            value.means,
            value.scales,
            value.classMeans,
            value.priors
          )
        )

    override def predict(
        fitted: SwiftCentroidFit,
        input: IArray[Double]
    ): Either[CategoricalClassifierError, AlderClassPrediction] =
      CategoricalPredictionKernel
        .transform(input, fitted.featureCount, fitted.means, fitted.scales)
        .flatMap(row =>
          CategoricalPredictionKernel.swift(
            fitted.classes,
            fitted.classMeans,
            fitted.priors,
            row
          )
        )

    override def failureMessage(error: CategoricalClassifierError): String =
      error.message

object RidgeLda:
  given compiler: CategoricalLearnerCompiler[
    RidgeLda,
    RidgeLdaFit,
    CategoricalClassifierError,
    AlderClassPrediction
  ] with
    override def definition(configuration: RidgeLda): CategoricalLearnerDefinition =
      CategoricalLearnerDefinition.unsafe(
        CategoricalLearnerId.unsafe("ridge-lda"),
        ComponentId("scalafim.mvpa.ridge-lda"),
        CategoricalBuiltInBackend("ridge-lda-v1"),
        PositiveInt.one,
        java.lang.Double.toHexString(configuration.penalty.toDouble),
        "ridge-linear-discriminant",
        PredictorScaling.None.identity
      )

    override def fit(
        configuration: RidgeLda,
        classes: AxisRef[ClassId],
        data: DMat,
        labels: Vector[ClassId]
    ): Either[CategoricalClassifierError, RidgeLdaFit] =
      CategoricalTraining
        .prepare(
          classes,
          data,
          labels,
          PredictorScaling.None,
          None,
          populationVariance = false,
          PositiveInt.one
        )
        .flatMap(value =>
          CategoricalTraining
            .ridgeFit(
              value.transformed,
              value.labelPositions,
              value.classMeans,
              value.priors,
              configuration.penalty
            )
            .map((coefficients, constants) => new RidgeLdaFit(classes, data.cols, coefficients, constants))
        )

    override def predict(
        fitted: RidgeLdaFit,
        input: IArray[Double]
    ): Either[CategoricalClassifierError, AlderClassPrediction] =
      CategoricalPredictionKernel
        .unscaled(input, fitted.featureCount)
        .flatMap(row =>
          CategoricalPredictionKernel.ridge(
            fitted.classes,
            fitted.coefficients,
            fitted.constants,
            row
          )
        )

    override def failureMessage(error: CategoricalClassifierError): String =
      error.message

enum CategoricalClassifierError:
  case InvalidScalingShrinkage(value: Double)
  case InvalidRidge(value: Double)
  case EmptyTrainingData
  case DimensionMismatch(expected: Int, actual: Int)
  case UnknownClass(label: ClassId)
  case MissingClass(label: ClassId)
  case TooFewFeatures(required: Int, actual: Int)
  case NonFiniteCoordinate(row: Int, column: Int, value: Double)
  case ConstantFeature(column: Int)
  case FeatureRead(detail: String)
  case LinearSolve(detail: String)
  case NonFiniteScore(label: ClassId, value: Double)

  def message: String =
    this match
      case InvalidScalingShrinkage(value) =>
        s"scaling shrinkage must be finite and in [0, 1], obtained $value"
      case InvalidRidge(value) =>
        s"classifier ridge must be positive and finite, obtained $value"
      case EmptyTrainingData                   => "classifier training data must be non-empty"
      case DimensionMismatch(expected, actual) =>
        s"classifier expected $expected features, obtained $actual"
      case UnknownClass(label)              => s"classifier observed unknown class '${label.value}'"
      case MissingClass(label)              => s"classifier training data omitted class '${label.value}'"
      case TooFewFeatures(required, actual) =>
        s"classifier requires at least $required features, obtained $actual"
      case NonFiniteCoordinate(row, column, value) =>
        s"classifier coordinate ($row,$column) must be finite, obtained $value"
      case ConstantFeature(column) =>
        s"classifier standardization rejects constant feature $column"
      case FeatureRead(detail)          => s"classifier could not read a feature row: $detail"
      case LinearSolve(detail)          => s"classifier linear solve failed: $detail"
      case NonFiniteScore(label, value) =>
        s"classifier score for '${label.value}' must be finite, obtained $value"

private final class PreparedCategoricalTraining(
    val means: Array[Double],
    val scales: Array[Double],
    val transformed: DMat,
    val labelPositions: Array[Int],
    val classMeans: DMat,
    val priors: Vector[Double]
)

private object CategoricalTraining:
  private val Epsilon = 1e-12

  def prepare(
      classes: AxisRef[ClassId],
      data: DMat,
      labels: Vector[ClassId],
      scaling: PredictorScaling,
      zeroVariance: Option[StandardizationSpecification],
      populationVariance: Boolean,
      minimumFeatures: PositiveInt
  ): Either[CategoricalClassifierError, PreparedCategoricalTraining] =
    if data.rows == 0 then Left(CategoricalClassifierError.EmptyTrainingData)
    else if labels.length != data.rows then Left(CategoricalClassifierError.DimensionMismatch(data.rows, labels.length))
    else if data.cols < minimumFeatures.toInt then
      Left(
        CategoricalClassifierError.TooFewFeatures(
          minimumFeatures.toInt,
          data.cols
        )
      )
    else
      validateFinite(data).flatMap: _ =>
        labelPositions(classes, labels).flatMap: positions =>
          predictorScaling(
            data,
            scaling,
            zeroVariance,
            populationVariance
          ).flatMap: (means, scales) =>
            val transformed = transform(data, means, scales)
            classSummary(classes, transformed, positions).map: (classMeans, priors) =>
              new PreparedCategoricalTraining(
                means,
                scales,
                transformed,
                positions,
                classMeans,
                priors
              )

  private def labelPositions(
      classes: AxisRef[ClassId],
      labels: Vector[ClassId]
  ): Either[CategoricalClassifierError, Array[Int]] =
    val classPositions = classes.keys.zipWithIndex.toMap
    val positions = new Array[Int](labels.length)
    var row = 0
    while row < labels.length do
      classPositions.get(labels(row)) match
        case None =>
          return Left(CategoricalClassifierError.UnknownClass(labels(row)))
        case Some(position) => positions(row) = position
      row += 1
    Right(positions)

  private def validateFinite(value: DMat): Either[CategoricalClassifierError, Unit] =
    var row = 0
    while row < value.rows do
      var column = 0
      while column < value.cols do
        if !value(row, column).isFinite then
          return Left(
            CategoricalClassifierError.NonFiniteCoordinate(
              row,
              column,
              value(row, column)
            )
          )
        column += 1
      row += 1
    Right(())

  private def predictorScaling(
      data: DMat,
      scaling: PredictorScaling,
      zeroVariance: Option[StandardizationSpecification],
      populationVariance: Boolean
  ): Either[CategoricalClassifierError, (Array[Double], Array[Double])] =
    val means = new Array[Double](data.cols)
    val scales = new Array[Double](data.cols)
    var column = 0
    while column < data.cols do
      var sum = 0.0
      var row = 0
      while row < data.rows do
        sum += data(row, column)
        row += 1
      val columnMean = sum / data.rows.toDouble
      means(column) = scaling match
        case PredictorScaling.None => 0.0
        case _                     => columnMean
      var squared = 0.0
      row = 0
      while row < data.rows do
        val centered = data(row, column) - columnMean
        squared += centered * centered
        row += 1
      val denominator =
        if populationVariance then data.rows.toDouble
        else math.max(1, data.rows - 1).toDouble
      scales(column) = math.sqrt(squared / denominator)
      column += 1

    scaling match
      case PredictorScaling.None =>
        java.util.Arrays.fill(scales, 1.0)
      case PredictorScaling.ZScore =>
        column = 0
        while column < scales.length do
          if !scales(column).isFinite || scales(column) <= Epsilon then
            zeroVariance match
              case Some(StandardizationSpecification.CenterScaleRejectConstant) =>
                return Left(CategoricalClassifierError.ConstantFeature(column))
              case _ => scales(column) = 1.0
          column += 1
      case PredictorScaling.DiagonalShrinkage(fraction) =>
        val positive = scales.filter(value => value.isFinite && value > Epsilon).sorted
        val target = if positive.isEmpty then 1.0 else positive(positive.length / 2)
        column = 0
        while column < scales.length do
          val variance =
            (1.0 - fraction.toDouble) * scales(column) * scales(column) +
              fraction.toDouble * target * target
          scales(column) = math.sqrt(variance)
          if !scales(column).isFinite || scales(column) <= Epsilon then scales(column) = 1.0
          column += 1
    Right(means -> scales)

  private def transform(
      data: DMat,
      means: Array[Double],
      scales: Array[Double]
  ): DMat =
    Matrix.tabulate(data.rows, data.cols): (row, column) =>
      (data(row, column) - means(column)) / scales(column)

  private def classSummary(
      classes: AxisRef[ClassId],
      data: DMat,
      labelPositions: Array[Int]
  ): Either[CategoricalClassifierError, (DMat, Vector[Double])] =
    val counts = Array.fill(classes.size)(0)
    val sums = Array.fill(classes.size * data.cols)(0.0)
    var row = 0
    while row < data.rows do
      val klass = labelPositions(row)
      counts(klass) += 1
      var column = 0
      while column < data.cols do
        sums(klass * data.cols + column) += data(row, column)
        column += 1
      row += 1
    var klass = 0
    while klass < classes.size do
      if counts(klass) == 0 then return Left(CategoricalClassifierError.MissingClass(classes.keys(klass)))
      klass += 1
    val means = Matrix.newBuilder(classes.size, data.cols)
    klass = 0
    while klass < classes.size do
      var column = 0
      while column < data.cols do
        means(klass, column) = sums(klass * data.cols + column) / counts(klass).toDouble
        column += 1
      klass += 1
    val total = data.rows.toDouble
    Right(means.result() -> counts.map(_ / total).toVector)

  def ridgeFit(
      data: DMat,
      labelPositions: Array[Int],
      classMeans: DMat,
      priors: Vector[Double],
      penalty: ClassifierRidge
  ): Either[CategoricalClassifierError, (DMat, Vector[Double])] =
    val covariance = Matrix.newBuilder(data.cols, data.cols)
    var row = 0
    while row < data.rows do
      val klass = labelPositions(row)
      var left = 0
      while left < data.cols do
        val leftResidual = data(row, left) - classMeans(klass, left)
        var right = 0
        while right < data.cols do
          val rightResidual = data(row, right) - classMeans(klass, right)
          covariance(left, right) = covariance(left, right) + leftResidual * rightResidual
          right += 1
        left += 1
      row += 1
    var diagonal = 0
    while diagonal < data.cols do
      covariance(diagonal, diagonal) = covariance(diagonal, diagonal) + penalty.toDouble
      diagonal += 1
    covariance
      .result()
      .cholesky(CholeskyOptions(1e-12))
      .left
      .map(error => CategoricalClassifierError.LinearSolve(error.getMessage))
      .flatMap: factor =>
        factor
          .solve(classMeans.t)
          .left
          .map(error => CategoricalClassifierError.LinearSolve(error.getMessage))
          .map: coefficients =>
            val constants = Vector.tabulate(classMeans.rows): klass =>
              var dot = 0.0
              var feature = 0
              while feature < data.cols do
                dot += classMeans(klass, feature) * coefficients(feature, klass)
                feature += 1
              -0.5 * dot + math.log(math.max(priors(klass), 1e-300))
            coefficients -> constants

private object CategoricalPredictionKernel:
  private val Epsilon = 1e-12

  def transform(
      input: IArray[Double],
      featureCount: Int,
      means: Array[Double],
      scales: Array[Double]
  ): Either[CategoricalClassifierError, Array[Double]] =
    if input.size != featureCount then Left(CategoricalClassifierError.DimensionMismatch(featureCount, input.size))
    else
      val row = new Array[Double](featureCount)
      var feature = 0
      while feature < featureCount do
        val value = input(feature)
        if !value.isFinite then return Left(CategoricalClassifierError.NonFiniteCoordinate(0, feature, value))
        row(feature) = (value - means(feature)) / scales(feature)
        feature += 1
      Right(row)

  def unscaled(
      input: IArray[Double],
      featureCount: Int
  ): Either[CategoricalClassifierError, Array[Double]] =
    if input.size != featureCount then Left(CategoricalClassifierError.DimensionMismatch(featureCount, input.size))
    else
      val row = new Array[Double](featureCount)
      var feature = 0
      while feature < featureCount do
        val value = input(feature)
        if !value.isFinite then return Left(CategoricalClassifierError.NonFiniteCoordinate(0, feature, value))
        row(feature) = value
        feature += 1
      Right(row)

  def nearestCentroid(
      classes: AxisRef[ClassId],
      classMeans: DMat,
      row: Array[Double]
  ): Either[CategoricalClassifierError, AlderClassPrediction] =
    val raw = new Array[Double](classes.size)
    var klass = 0
    while klass < classes.size do
      var squared = 0.0
      var feature = 0
      while feature < row.length do
        val delta = row(feature) - classMeans(klass, feature)
        squared += delta * delta
        feature += 1
      raw(klass) = -squared
      klass += 1
    prediction(classes, raw)

  def correlation(
      classes: AxisRef[ClassId],
      classMeans: DMat,
      row: Array[Double]
  ): Either[CategoricalClassifierError, AlderClassPrediction] =
    val raw = new Array[Double](classes.size)
    val rowMean = mean(row)
    val rowNorm = centeredNorm(row, rowMean)
    var klass = 0
    while klass < classes.size do
      val templateMean = matrixRowMean(classMeans, klass)
      val templateNorm = matrixRowNorm(classMeans, klass, templateMean)
      var dot = 0.0
      var feature = 0
      while feature < row.length do
        dot += (row(feature) - rowMean) * (classMeans(klass, feature) - templateMean)
        feature += 1
      raw(klass) = dot / math.max(Epsilon, rowNorm * templateNorm)
      klass += 1
    prediction(classes, raw)

  def swift(
      classes: AxisRef[ClassId],
      classMeans: DMat,
      priors: Vector[Double],
      row: Array[Double]
  ): Either[CategoricalClassifierError, AlderClassPrediction] =
    val raw = new Array[Double](classes.size)
    var klass = 0
    while klass < classes.size do
      var dot = 0.0
      var squared = 0.0
      var feature = 0
      while feature < row.length do
        val template = classMeans(klass, feature)
        dot += row(feature) * template
        squared += template * template
        feature += 1
      raw(klass) = dot - 0.5 * squared + math.log(math.max(priors(klass), 1e-300))
      klass += 1
    prediction(classes, raw)

  def ridge(
      classes: AxisRef[ClassId],
      coefficients: DMat,
      constants: Vector[Double],
      row: Array[Double]
  ): Either[CategoricalClassifierError, AlderClassPrediction] =
    val raw = new Array[Double](classes.size)
    var klass = 0
    while klass < classes.size do
      var score = constants(klass)
      var feature = 0
      while feature < row.length do
        score += row(feature) * coefficients(feature, klass)
        feature += 1
      raw(klass) = score
      klass += 1
    prediction(classes, raw)

  private def prediction(
      classes: AxisRef[ClassId],
      raw: Array[Double]
  ): Either[CategoricalClassifierError, AlderClassPrediction] =
    val scores = Vector.newBuilder[(ClassId, DecisionScore)]
    var best = 0
    var bestScore = Double.NegativeInfinity
    var klass = 0
    while klass < classes.size do
      val label = classes.keys(klass)
      val value = raw(klass)
      if !value.isFinite then return Left(CategoricalClassifierError.NonFiniteScore(label, value))
      DecisionScore(value) match
        case Left(_) =>
          return Left(CategoricalClassifierError.NonFiniteScore(label, value))
        case Right(score) => scores += label -> score
      if value > bestScore then
        bestScore = value
        best = klass
      klass += 1
    Right(AlderClassPrediction(classes.keys(best), scores.result()))

  private def mean(values: Array[Double]): Double =
    var sum = 0.0
    var index = 0
    while index < values.length do
      sum += values(index)
      index += 1
    sum / values.length.toDouble

  private def centeredNorm(values: Array[Double], center: Double): Double =
    var squared = 0.0
    var index = 0
    while index < values.length do
      val delta = values(index) - center
      squared += delta * delta
      index += 1
    math.sqrt(squared)

  private def matrixRowMean(value: DMat, row: Int): Double =
    var sum = 0.0
    var column = 0
    while column < value.cols do
      sum += value(row, column)
      column += 1
    sum / value.cols.toDouble

  private def matrixRowNorm(value: DMat, row: Int, center: Double): Double =
    var squared = 0.0
    var column = 0
    while column < value.cols do
      val delta = value(row, column) - center
      squared += delta * delta
      column += 1
    math.sqrt(squared)
