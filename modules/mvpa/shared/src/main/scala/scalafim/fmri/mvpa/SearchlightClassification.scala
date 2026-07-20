package scalafim.fmri.mvpa

import gale.linalg.{CholeskyOptions, DMat, DMatBuilder, Matrix}

final case class SearchlightClassifierScanner(
    classifier: Classifier,
    storePredictions: Boolean = false
):
  def analysisName: String =
    s"cv_${classifier.name}"

  def run(
      data: PatternMatrix,
      featureSetPlan: FeatureSetPlan,
      response: Response,
      folds: FoldPlan
  ): Either[MvpaError, MvpaResult] =
    outcomes(data, featureSetPlan, response, folds).map { iterator =>
      MvpaResult(analysisName, Some(featureSetPlan), iterator.toVector)
    }

  def outcomes(
      data: PatternMatrix,
      featureSetPlan: FeatureSetPlan,
      response: Response,
      folds: FoldPlan
  ): Either[MvpaError, Iterator[RoiOutcome]] =
    classifier match
      case swift: SwiftCentroidClassifier =>
        SearchlightClassifierScanner.swiftOutcomes(data, featureSetPlan.featureSets, response, folds, swift, storePredictions)
      case ridge: RidgeLdaClassifier =>
        SearchlightClassifierScanner.ridgeOutcomes(data, featureSetPlan.featureSets, response, folds, ridge, storePredictions)
      case _ =>
        val analysis = CrossValidatedClassifierAnalysis(classifier, storePredictions)
        MvpaStream.outcomes(PatternSource.fromMatrix(data), featureSetPlan, response, analysis, Some(folds))

object SearchlightClassifierScanner:
  private val Eps = 1e-12

  private def swiftOutcomes(
      data: PatternMatrix,
      featureSets: Vector[FeatureSet],
      response: Response,
      folds: FoldPlan,
      classifier: SwiftCentroidClassifier,
      storePredictions: Boolean
  ): Either[MvpaError, Iterator[RoiOutcome]] =
    for
      labels <- Classification.categorical(response, data.samples)
      _ <- MvpaTask.validateFolds(Some(folds), data.samples)
      _ <- Classification.validateScaling(classifier.scaling)
      classes <- responseClasses(labels)
    yield
      val featureLookup = data.featureIndices.zipWithIndex.map { case (feature, index) => feature.value -> index }.toMap
      featureSets.iterator.map { featureSet =>
        evaluateSwift(data, featureSet, featureLookup, labels, classes, folds, classifier, storePredictions)
      }

  private def responseClasses(labels: Vector[ClassLabel]): Either[MvpaError, Vector[ClassLabel]] =
    val classes = labels.distinct
    if classes.length < 2 then Left(MvpaError.SingleClassResponse)
    else Right(classes)

  private def ridgeOutcomes(
      data: PatternMatrix,
      featureSets: Vector[FeatureSet],
      response: Response,
      folds: FoldPlan,
      classifier: RidgeLdaClassifier,
      storePredictions: Boolean
  ): Either[MvpaError, Iterator[RoiOutcome]] =
    for
      labels <- Classification.categorical(response, data.samples)
      _ <- MvpaTask.validateFolds(Some(folds), data.samples)
      classes <- responseClasses(labels)
    yield
      val featureLookup = data.featureIndices.zipWithIndex.map { case (feature, index) => feature.value -> index }.toMap
      featureSets.iterator.map { featureSet =>
        evaluateRidge(data, featureSet, featureLookup, labels, classes, folds, classifier, storePredictions)
      }

  private def evaluateSwift(
      data: PatternMatrix,
      featureSet: FeatureSet,
      featureLookup: Map[Int, Int],
      labels: Vector[ClassLabel],
      classes: Vector[ClassLabel],
      folds: FoldPlan,
      classifier: SwiftCentroidClassifier,
      storePredictions: Boolean
  ): RoiOutcome =
    featurePositions(featureSet, featureLookup) match
      case Left(error) =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
      case Right(positions) if positions.length < classifier.minFeatures =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, MvpaError.TooFewFeatures(featureSet.id, positions.length, classifier.minFeatures))
      case Right(positions) =>
        crossValidateSwift(data, featureSet, positions, labels, classes, folds, classifier.scaling) match
          case Left(error) =>
            RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
          case Right(prediction) =>
            Classification.accuracy(prediction, labels) match
              case Left(error) =>
                RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
              case Right(accuracy) =>
                val payload =
                  if storePredictions then Some(RoiPayload.Classification(prediction))
                  else None
                RoiOutcome.Success(
                  featureSet.id,
                  featureSet.featureIndices,
                  MetricVector("Accuracy" -> accuracy, "TestedSamples" -> prediction.probabilities.rows.toDouble),
                  payload
                )

  private def featurePositions(
      featureSet: FeatureSet,
      featureLookup: Map[Int, Int]
  ): Either[MvpaError, Array[Int]] =
    val positions = new Array[Int](featureSet.featureIndices.length)
    var i = 0
    while i < featureSet.featureIndices.length do
      val feature = featureSet.featureIndices(i)
      featureLookup.get(feature.value) match
        case Some(position) =>
          positions(i) = position
        case None =>
          return Left(MvpaError.MissingFeature(featureSet.id, feature))
      i += 1
    Right(positions)

  private def evaluateRidge(
      data: PatternMatrix,
      featureSet: FeatureSet,
      featureLookup: Map[Int, Int],
      labels: Vector[ClassLabel],
      classes: Vector[ClassLabel],
      folds: FoldPlan,
      classifier: RidgeLdaClassifier,
      storePredictions: Boolean
  ): RoiOutcome =
    featurePositions(featureSet, featureLookup) match
      case Left(error) =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
      case Right(positions) if positions.length < classifier.minFeatures =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, MvpaError.TooFewFeatures(featureSet.id, positions.length, classifier.minFeatures))
      case Right(positions) =>
        crossValidateRidge(data, featureSet, positions, labels, classes, folds, classifier) match
          case Left(error) =>
            RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
          case Right(prediction) =>
            Classification.accuracy(prediction, labels) match
              case Left(error) =>
                RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
              case Right(accuracy) =>
                val payload =
                  if storePredictions then Some(RoiPayload.Classification(prediction))
                  else None
                RoiOutcome.Success(
                  featureSet.id,
                  featureSet.featureIndices,
                  MetricVector("Accuracy" -> accuracy, "TestedSamples" -> prediction.probabilities.rows.toDouble),
                  payload
                )

  private def crossValidateSwift(
      data: PatternMatrix,
      featureSet: FeatureSet,
      positions: Array[Int],
      labels: Vector[ClassLabel],
      classes: Vector[ClassLabel],
      folds: FoldPlan,
      scaling: FeatureScaling
  ): Either[MvpaError, ClassificationPrediction] =
    val testRows = folds.folds.flatMap(_.test.map(_.value)).distinct.sorted
    if testRows.isEmpty then Left(MvpaError.InvalidClassifierInput("fold plan produced no test samples"))
    else
      val rowToOutput = testRows.zipWithIndex.toMap
      val probSum = Matrix.newBuilder(testRows.length, classes.length)
      val probN = Array.fill(testRows.length)(0)

      var foldIndex = 0
      var error: MvpaError | Null = null
      while foldIndex < folds.folds.length && error == null do
        val fold = folds.folds(foldIndex)
        processSwiftFold(data, positions, labels, classes, fold, scaling, rowToOutput, probSum, probN) match
          case Left(e) => error = e
          case Right(()) =>
        foldIndex += 1

      if error != null then Left(error)
      else
        val missing = probN.indexWhere(_ == 0)
        if missing >= 0 then Left(MvpaError.InvalidClassifierInput("some test samples were never predicted"))
        else
          var row = 0
          while row < testRows.length do
            var klass = 0
            while klass < classes.length do
              probSum(row, klass) = probSum(row, klass) / probN(row)
              klass += 1
            row += 1
          val probabilities = probSum.result()
          Classification.validateFinite(probabilities, "classifier probabilities").map { _ =>
            ClassificationPrediction(
              classes,
              probabilities,
              testRows.map(SampleIndex.unsafe).toVector
            )
          }

  private def crossValidateRidge(
      data: PatternMatrix,
      featureSet: FeatureSet,
      positions: Array[Int],
      labels: Vector[ClassLabel],
      classes: Vector[ClassLabel],
      folds: FoldPlan,
      classifier: RidgeLdaClassifier
  ): Either[MvpaError, ClassificationPrediction] =
    val testRows = folds.folds.flatMap(_.test.map(_.value)).distinct.sorted
    if testRows.isEmpty then Left(MvpaError.InvalidClassifierInput("fold plan produced no test samples"))
    else
      val rowToOutput = testRows.zipWithIndex.toMap
      val probSum = Matrix.newBuilder(testRows.length, classes.length)
      val probN = Array.fill(testRows.length)(0)

      var foldIndex = 0
      var error: MvpaError | Null = null
      while foldIndex < folds.folds.length && error == null do
        val fold = folds.folds(foldIndex)
        processRidgeFold(data, positions, labels, classes, fold, classifier, rowToOutput, probSum, probN) match
          case Left(e) => error = e
          case Right(()) =>
        foldIndex += 1

      if error != null then Left(error)
      else
        val missing = probN.indexWhere(_ == 0)
        if missing >= 0 then Left(MvpaError.InvalidClassifierInput("some test samples were never predicted"))
        else
          var row = 0
          while row < testRows.length do
            var klass = 0
            while klass < classes.length do
              probSum(row, klass) = probSum(row, klass) / probN(row)
              klass += 1
            row += 1
          val probabilities = probSum.result()
          Classification.validateFinite(probabilities, "classifier probabilities").map { _ =>
            ClassificationPrediction(
              classes,
              probabilities,
              testRows.map(SampleIndex.unsafe).toVector
            )
          }

  private def processSwiftFold(
      data: PatternMatrix,
      positions: Array[Int],
      labels: Vector[ClassLabel],
      classes: Vector[ClassLabel],
      fold: Fold,
      scaling: FeatureScaling,
      rowToOutput: Map[Int, Int],
      probSum: DMatBuilder,
      probN: Array[Int]
  ): Either[MvpaError, Unit] =
    val trainLabels = fold.train.map(index => labels(index.value)).toVector
    if trainLabels.distinct.toSet != classes.toSet then
      Left(MvpaError.InvalidClassifierInput("every training fold must contain every class"))
    else
      fitSwiftFold(data, positions, labels, trainLabels.distinct, fold.train, scaling) match
        case Left(error) =>
          Left(error)
        case Right(fit) =>
          val scores = new Array[Double](fit.classes.length)
          val probs = new Array[Double](fit.classes.length)
          var localRow = 0
          var error: MvpaError | Null = null
          while localRow < fold.test.length && error == null do
            val sample = fold.test(localRow).value
            scoreSwiftRow(data, positions, sample, fit, scores) match
              case Left(e) => error = e
              case Right(()) =>
                softmaxInto(scores, probs)
                val outRow = rowToOutput(sample)
                var klass = 0
                while klass < classes.length do
                  probSum(outRow, klass) = probSum(outRow, klass) + probs(fit.classColumns(klass))
                  klass += 1
                probN(outRow) += 1
            localRow += 1
          error match
            case null => Right(())
            case e => Left(e)

  private def processRidgeFold(
      data: PatternMatrix,
      positions: Array[Int],
      labels: Vector[ClassLabel],
      classes: Vector[ClassLabel],
      fold: Fold,
      classifier: RidgeLdaClassifier,
      rowToOutput: Map[Int, Int],
      probSum: DMatBuilder,
      probN: Array[Int]
  ): Either[MvpaError, Unit] =
    val trainLabels = fold.train.map(index => labels(index.value)).toVector
    if trainLabels.distinct.toSet != classes.toSet then
      Left(MvpaError.InvalidClassifierInput("every training fold must contain every class"))
    else
      fitRidgeFold(data, positions, labels, trainLabels.distinct, fold.train, classifier) match
        case Left(error) =>
          Left(error)
        case Right(fit) =>
          val scores = new Array[Double](fit.classes.length)
          val probs = new Array[Double](fit.classes.length)
          var localRow = 0
          var error: MvpaError | Null = null
          while localRow < fold.test.length && error == null do
            val sample = fold.test(localRow).value
            scoreRidgeRow(data, positions, sample, fit, scores) match
              case Left(e) => error = e
              case Right(()) =>
                softmaxInto(scores, probs)
                val outRow = rowToOutput(sample)
                var klass = 0
                while klass < classes.length do
                  probSum(outRow, klass) = probSum(outRow, klass) + probs(fit.classColumns(klass))
                  klass += 1
                probN(outRow) += 1
            localRow += 1
          error match
            case null => Right(())
            case e => Left(e)

  private final case class SwiftFoldFit(
      classes: Vector[ClassLabel],
      priors: Array[Double],
      means: Array[Double],
      scales: Array[Double],
      centroids: Array[Double],
      centroidNorms: Array[Double],
      classColumns: Array[Int]
  )

  private final case class RidgeFoldFit(
      classes: Vector[ClassLabel],
      invSigmaMeans: DMat,
      linearConstants: Array[Double],
      classColumns: Array[Int]
  )

  private def fitSwiftFold(
      data: PatternMatrix,
      positions: Array[Int],
      labels: Vector[ClassLabel],
      classes: Vector[ClassLabel],
      train: Vector[SampleIndex],
      scaling: FeatureScaling
  ): Either[MvpaError, SwiftFoldFit] =
    val means = new Array[Double](positions.length)
    val scales = new Array[Double](positions.length)
    var feature = 0
    while feature < positions.length do
      var sum = 0.0
      var row = 0
      while row < train.length do
        val value = data.value(train(row).value, positions(feature))
        if !value.isFinite then return Left(MvpaError.InvalidClassifierInput("training data contains non-finite values"))
        sum += value
        row += 1
      val rawMean = sum / train.length
      means(feature) =
        scaling match
          case FeatureScaling.None => 0.0
          case _ => rawMean

      var ss = 0.0
      row = 0
      while row < train.length do
        val centered = data.value(train(row).value, positions(feature)) - rawMean
        ss += centered * centered
        row += 1
      scales(feature) = math.sqrt(ss / math.max(1, train.length - 1))
      feature += 1

    scaling match
      case FeatureScaling.None =>
        java.util.Arrays.fill(scales, 1.0)
      case FeatureScaling.ZScore =>
        floorScales(scales)
      case FeatureScaling.DiagonalShrinkage(alpha) =>
        val positive = scales.filter(_ > Eps)
        val target =
          if positive.isEmpty then 1.0
          else
            val sorted = positive.sorted
            sorted(sorted.length / 2)
        feature = 0
        while feature < scales.length do
          scales(feature) = math.sqrt((1.0 - alpha.value) * scales(feature) * scales(feature) + alpha.value * target * target)
          feature += 1
        floorScales(scales)

    val classIndex = classes.zipWithIndex.map { case (label, index) => label.value -> index }.toMap
    val counts = Array.fill(classes.length)(0)
    val centroids = new Array[Double](classes.length * positions.length)
    var row = 0
    while row < train.length do
      val sample = train(row).value
      val klass = classIndex(labels(sample).value)
      counts(klass) += 1
      feature = 0
      while feature < positions.length do
        val value = data.value(sample, positions(feature))
        centroids(klass * positions.length + feature) += (value - means(feature)) / scales(feature)
        feature += 1
      row += 1

    if counts.exists(_ == 0) then Left(MvpaError.InvalidClassifierInput("every class must have at least one training sample"))
    else
      val priors = counts.map(_ / train.length.toDouble)
      val norms = new Array[Double](classes.length)
      var klass = 0
      while klass < classes.length do
        feature = 0
        while feature < positions.length do
          val offset = klass * positions.length + feature
          centroids(offset) /= counts(klass)
          norms(klass) += centroids(offset) * centroids(offset)
          feature += 1
        klass += 1

      val fullClasses = labels.distinct
      val classColumns = new Array[Int](fullClasses.length)
      val localIndex = classes.zipWithIndex.map { case (label, index) => label.value -> index }.toMap
      klass = 0
      while klass < fullClasses.length do
        classColumns(klass) = localIndex(fullClasses(klass).value)
        klass += 1

      Right(SwiftFoldFit(classes, priors, means, scales, centroids, norms, classColumns))

  private def fitRidgeFold(
      data: PatternMatrix,
      positions: Array[Int],
      labels: Vector[ClassLabel],
      classes: Vector[ClassLabel],
      train: Vector[SampleIndex],
      classifier: RidgeLdaClassifier
  ): Either[MvpaError, RidgeFoldFit] =
    val classIndex = classes.zipWithIndex.map { case (label, index) => label.value -> index }.toMap
    val counts = Array.fill(classes.length)(0)
    val means = new Array[Double](classes.length * positions.length)

    var row = 0
    while row < train.length do
      val sample = train(row).value
      val klass = classIndex(labels(sample).value)
      counts(klass) += 1
      var feature = 0
      while feature < positions.length do
        val value = data.value(sample, positions(feature))
        if !value.isFinite then return Left(MvpaError.InvalidClassifierInput("training data contains non-finite values"))
        means(klass * positions.length + feature) += value
        feature += 1
      row += 1

    if counts.exists(_ == 0) then Left(MvpaError.InvalidClassifierInput("every class must have at least one training sample"))
    else
      var klass = 0
      while klass < classes.length do
        var feature = 0
        while feature < positions.length do
          means(klass * positions.length + feature) /= counts(klass)
          feature += 1
        klass += 1

      val sigma = Matrix.newBuilder(positions.length, positions.length)
      row = 0
      while row < train.length do
        val sample = train(row).value
        val localClass = classIndex(labels(sample).value)
        var left = 0
        while left < positions.length do
          val residualLeft =
            data.value(sample, positions(left)) -
              means(localClass * positions.length + left)
          var right = 0
          while right < positions.length do
            val residualRight =
              data.value(sample, positions(right)) -
                means(localClass * positions.length + right)
            sigma(left, right) = sigma(left, right) + residualLeft * residualRight
            right += 1
          left += 1
        row += 1

      var diag = 0
      while diag < positions.length do
        sigma(diag, diag) = sigma(diag, diag) + classifier.gamma
        diag += 1

      val meansT = Matrix.newBuilder(positions.length, classes.length)
      var feature = 0
      while feature < positions.length do
        klass = 0
        while klass < classes.length do
          meansT(feature, klass) = means(klass * positions.length + feature)
          klass += 1
        feature += 1

      sigma.result()
        .cholesky(CholeskyOptions(1e-12))
        .left
        .map(error => MvpaError.ClassifierFitFailed(classifier.name, error.getMessage))
        .flatMap { factor =>
          factor
            .solve(meansT.result())
            .left
            .map(error => MvpaError.ClassifierFitFailed(classifier.name, error.getMessage))
            .map { invSigmaMeans =>
              val constants = new Array[Double](classes.length)
              klass = 0
              while klass < classes.length do
                var dot = 0.0
                feature = 0
                while feature < positions.length do
                  dot += means(klass * positions.length + feature) * invSigmaMeans(feature, klass)
                  feature += 1
                constants(klass) = -0.5 * dot + math.log(math.max(counts(klass) / train.length.toDouble, 1e-300))
                klass += 1

              val fullClasses = labels.distinct
              val classColumns = new Array[Int](fullClasses.length)
              val localIndex = classes.zipWithIndex.map { case (label, index) => label.value -> index }.toMap
              klass = 0
              while klass < fullClasses.length do
                classColumns(klass) = localIndex(fullClasses(klass).value)
                klass += 1

              RidgeFoldFit(classes, invSigmaMeans, constants, classColumns)
            }
        }

  private def scoreSwiftRow(
      data: PatternMatrix,
      positions: Array[Int],
      sample: Int,
      fit: SwiftFoldFit,
      scores: Array[Double]
  ): Either[MvpaError, Unit] =
    var klass = 0
    while klass < fit.classes.length do
      var dot = 0.0
      var feature = 0
      while feature < positions.length do
        val value = data.value(sample, positions(feature))
        if !value.isFinite then return Left(MvpaError.InvalidClassifierInput("test data contains non-finite values"))
        val scaled = (value - fit.means(feature)) / fit.scales(feature)
        dot += scaled * fit.centroids(klass * positions.length + feature)
        feature += 1
      scores(klass) = dot - 0.5 * fit.centroidNorms(klass) + math.log(math.max(fit.priors(klass), 1e-300))
      klass += 1
    Right(())

  private def scoreRidgeRow(
      data: PatternMatrix,
      positions: Array[Int],
      sample: Int,
      fit: RidgeFoldFit,
      scores: Array[Double]
  ): Either[MvpaError, Unit] =
    var klass = 0
    while klass < fit.classes.length do
      var dot = 0.0
      var feature = 0
      while feature < positions.length do
        val value = data.value(sample, positions(feature))
        if !value.isFinite then return Left(MvpaError.InvalidClassifierInput("test data contains non-finite values"))
        dot += value * fit.invSigmaMeans(feature, klass)
        feature += 1
      scores(klass) = dot + fit.linearConstants(klass)
      klass += 1
    Right(())

  private def softmaxInto(scores: Array[Double], out: Array[Double]): Unit =
    var maxScore = scores(0)
    var i = 1
    while i < scores.length do
      maxScore = math.max(maxScore, scores(i))
      i += 1
    var sum = 0.0
    i = 0
    while i < scores.length do
      val value = math.exp(scores(i) - maxScore)
      out(i) = value
      sum += value
      i += 1
    i = 0
    while i < scores.length do
      out(i) /= sum
      i += 1

  private def floorScales(scales: Array[Double]): Unit =
    var i = 0
    while i < scales.length do
      if !scales(i).isFinite || scales(i) <= Eps then scales(i) = 1.0
      i += 1
