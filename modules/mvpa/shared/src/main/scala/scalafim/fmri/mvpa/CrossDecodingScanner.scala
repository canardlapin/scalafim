package scalafim.fmri.mvpa

import scalafim.linalg.DoubleMatrix

final case class NaiveCrossDecodingScanner(storePredictions: Boolean = false):
  def analysisName: String =
    "xdec_correlation_centroid"

  def run(
      source: PatternMatrix,
      target: PatternMatrix,
      featureSetPlan: FeatureSetPlan,
      design: CrossDecodingDesign
  ): Either[MvpaError, MvpaResult] =
    outcomes(source, target, featureSetPlan, design).map { iterator =>
      MvpaResult(analysisName, Some(featureSetPlan), iterator.toVector)
    }

  def outcomes(
      source: PatternMatrix,
      target: PatternMatrix,
      featureSetPlan: FeatureSetPlan,
      design: CrossDecodingDesign
  ): Either[MvpaError, Iterator[RoiOutcome]] =
    design.validateSamples(source.samples, target.samples).map { validDesign =>
      val sourceLookup = source.featureIndices.zipWithIndex.map { case (feature, index) => feature.value -> index }.toMap
      val targetLookup = target.featureIndices.zipWithIndex.map { case (feature, index) => feature.value -> index }.toMap
      val classes = validDesign.sourceClasses
      featureSetPlan.featureSets.iterator.map { featureSet =>
        evaluate(source, target, featureSet, sourceLookup, targetLookup, validDesign, classes)
      }
    }

  private def evaluate(
      source: PatternMatrix,
      target: PatternMatrix,
      featureSet: FeatureSet,
      sourceLookup: Map[Int, Int],
      targetLookup: Map[Int, Int],
      design: CrossDecodingDesign,
      classes: Vector[ClassLabel]
  ): RoiOutcome =
    featurePositions(featureSet, sourceLookup) match
      case Left(error) =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
      case Right(sourcePositions) if sourcePositions.length < CorrelationCentroidClassifier().minFeatures =>
        RoiOutcome.Failure(
          featureSet.id,
          featureSet.featureIndices,
          MvpaError.TooFewFeatures(featureSet.id, sourcePositions.length, CorrelationCentroidClassifier().minFeatures)
        )
      case Right(sourcePositions) =>
        featurePositions(featureSet, targetLookup) match
          case Left(error) =>
            RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
          case Right(targetPositions) =>
            predict(source, target, sourcePositions, targetPositions, design, classes) match
              case Left(error) =>
                RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
              case Right(prediction) =>
                accuracy(prediction, design.targetLabels) match
                  case Left(error) =>
                    RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
                  case Right(value) =>
                    val payload =
                      if storePredictions then Some(RoiPayload.Classification(prediction))
                      else None
                    RoiOutcome.Success(
                      featureSet.id,
                      featureSet.featureIndices,
                      MetricVector(
                        "Accuracy" -> value,
                        "TestedSamples" -> prediction.probabilities.rows.toDouble,
                        "SourceSamples" -> source.samples.toDouble,
                        "Classes" -> classes.length.toDouble
                      ),
                      payload
                    )

  private def featurePositions(
      featureSet: FeatureSet,
      featureLookup: Map[Int, Int]
  ): Either[MvpaError, Array[Int]] =
    val positions = new Array[Int](featureSet.featureIndices.length)
    var i = 0
    var error: MvpaError | Null = null
    while i < featureSet.featureIndices.length && error == null do
      val feature = featureSet.featureIndices(i)
      featureLookup.get(feature.value) match
        case Some(position) =>
          positions(i) = position
        case None =>
          error = MvpaError.MissingFeature(featureSet.id, feature)
      i += 1
    error match
      case null => Right(positions)
      case e => Left(e)

  private final case class PrototypeFit(
      classes: Vector[ClassLabel],
      centroids: Array[Double],
      centroidMeans: Array[Double],
      centroidNorms: Array[Double]
  )

  private def predict(
      source: PatternMatrix,
      target: PatternMatrix,
      sourcePositions: Array[Int],
      targetPositions: Array[Int],
      design: CrossDecodingDesign,
      classes: Vector[ClassLabel]
  ): Either[MvpaError, ClassificationPrediction] =
    for
      fit <- fitPrototypes(source, sourcePositions, design.sourceLabels, classes)
      prediction <- predictTarget(target, targetPositions, fit)
    yield prediction

  private def fitPrototypes(
      source: PatternMatrix,
      positions: Array[Int],
      labels: Vector[ClassLabel],
      classes: Vector[ClassLabel]
  ): Either[MvpaError, PrototypeFit] =
    val classIndex = classes.zipWithIndex.map { case (label, index) => label.value -> index }.toMap
    val counts = Array.fill(classes.length)(0)
    val centroids = new Array[Double](classes.length * positions.length)
    var row = 0
    var error: MvpaError | Null = null
    while row < source.samples && error == null do
      val klass = classIndex(labels(row).value)
      counts(klass) += 1
      var feature = 0
      while feature < positions.length && error == null do
        val value = source.value.dataArray(row * source.features + positions(feature))
        if !value.isFinite then error = MvpaError.InvalidClassifierInput("training data contains non-finite values")
        else centroids(klass * positions.length + feature) += value
        feature += 1
      row += 1

    if error != null then Left(error)
    else if counts.exists(_ == 0) then Left(MvpaError.InvalidClassifierInput("every class must have at least one training sample"))
    else
      var klass = 0
      while klass < classes.length do
        var feature = 0
        while feature < positions.length do
          centroids(klass * positions.length + feature) /= counts(klass)
          feature += 1
        klass += 1

      val means = new Array[Double](classes.length)
      val norms = new Array[Double](classes.length)
      klass = 0
      while klass < classes.length do
        var sum = 0.0
        var feature = 0
        while feature < positions.length do
          sum += centroids(klass * positions.length + feature)
          feature += 1
        val mean = sum / positions.length
        means(klass) = mean

        var ss = 0.0
        feature = 0
        while feature < positions.length do
          val centered = centroids(klass * positions.length + feature) - mean
          ss += centered * centered
          feature += 1
        norms(klass) = math.sqrt(ss)
        klass += 1

      Right(PrototypeFit(classes, centroids, means, norms))

  private def predictTarget(
      target: PatternMatrix,
      positions: Array[Int],
      fit: PrototypeFit
  ): Either[MvpaError, ClassificationPrediction] =
    val probabilities = new Array[Double](target.samples * fit.classes.length)
    val scores = new Array[Double](fit.classes.length)
    var row = 0
    var error: MvpaError | Null = null
    while row < target.samples && error == null do
      scoreTargetRow(target, positions, row, fit, scores) match
        case Left(e) =>
          error = e
        case Right(()) =>
          softmaxInto(scores, probabilities, row * fit.classes.length)
      row += 1

    error match
      case null =>
        val matrix = DoubleMatrix.unsafe(target.samples, fit.classes.length, probabilities)
        Classification.validateFinite(matrix, "classifier probabilities").map { _ =>
          ClassificationPrediction(fit.classes, matrix, target.sampleIndices)
        }
      case e => Left(e)

  private def scoreTargetRow(
      target: PatternMatrix,
      positions: Array[Int],
      row: Int,
      fit: PrototypeFit,
      scores: Array[Double]
  ): Either[MvpaError, Unit] =
    var sum = 0.0
    var feature = 0
    var error: MvpaError | Null = null
    while feature < positions.length && error == null do
      val value = target.value.dataArray(row * target.features + positions(feature))
      if !value.isFinite then error = MvpaError.InvalidClassifierInput("test data contains non-finite values")
      else sum += value
      feature += 1

    if error != null then Left(error)
    else
      val mean = sum / positions.length
      var ss = 0.0
      feature = 0
      while feature < positions.length do
        val centered = target.value.dataArray(row * target.features + positions(feature)) - mean
        ss += centered * centered
        feature += 1
      val norm = math.sqrt(ss)

      var klass = 0
      while klass < fit.classes.length do
        var dot = 0.0
        feature = 0
        while feature < positions.length do
          dot += (target.value.dataArray(row * target.features + positions(feature)) - mean) *
            (fit.centroids(klass * positions.length + feature) - fit.centroidMeans(klass))
          feature += 1
        scores(klass) = dot / math.max(NaiveCrossDecodingScanner.Eps, norm * fit.centroidNorms(klass))
        klass += 1
      Right(())

  private def softmaxInto(scores: Array[Double], out: Array[Double], offset: Int): Unit =
    var maxScore = scores(0)
    var i = 1
    while i < scores.length do
      maxScore = math.max(maxScore, scores(i))
      i += 1
    var sum = 0.0
    i = 0
    while i < scores.length do
      val value = math.exp(scores(i) - maxScore)
      out(offset + i) = value
      sum += value
      i += 1
    i = 0
    while i < scores.length do
      out(offset + i) /= sum
      i += 1

  private def accuracy(
      prediction: ClassificationPrediction,
      targetLabels: Vector[ClassLabel]
  ): Either[MvpaError, Double] =
    if prediction.probabilities.rows == 0 then Left(MvpaError.InvalidClassifierInput("accuracy requires at least one prediction"))
    else if prediction.probabilities.rows != targetLabels.length then
      Left(MvpaError.ResponseLengthMismatch(prediction.probabilities.rows, targetLabels.length))
    else
      val predicted = prediction.predicted
      var correct = 0
      var row = 0
      while row < predicted.length do
        if predicted(row) == targetLabels(row) then correct += 1
        row += 1
      Right(correct.toDouble / predicted.length)

object NaiveCrossDecodingScanner:
  private val Eps = 1e-12
