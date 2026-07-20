package scalafim.fmri.mvpa

import gale.linalg.{CholeskyOptions, DMat, Matrix}

enum FeaturePredictionDirection:
  case FeaturesToPatterns
  case PatternsToFeatures

  def label: String =
    this match
      case FeaturesToPatterns => "features_to_patterns"
      case PatternsToFeatures => "patterns_to_features"

final case class FeatureModelDesign private (
    items: Vector[String],
    features: DMat,
    featureNames: Vector[String]
):
  require(items.length == features.rows, "feature design item count must match matrix rows")
  require(featureNames.length == features.cols, "feature name count must match matrix columns")

object FeatureModelDesign:
  def apply(
      items: Seq[String],
      features: DMat,
      featureNames: Seq[String] = Seq.empty
  ): Either[MvpaError, FeatureModelDesign] =
    val itemVector = items.map(_.trim).toVector
    val names =
      if featureNames.isEmpty then (0 until features.cols).map(index => s"feature_$index").toVector
      else featureNames.map(_.trim).toVector

    if features.rows < 2 then Left(MvpaError.InvalidFeatureModelInput("feature design requires at least two rows"))
    else if features.cols < 1 then Left(MvpaError.InvalidFeatureModelInput("feature design requires at least one column"))
    else if itemVector.length != features.rows then
      Left(MvpaError.InvalidFeatureModelInput(s"feature design item count ${itemVector.length} != row count ${features.rows}"))
    else if itemVector.exists(_.isEmpty) then Left(MvpaError.InvalidFeatureModelInput("feature design item labels must be non-empty"))
    else if itemVector.distinct.length != itemVector.length then Left(MvpaError.InvalidFeatureModelInput("feature design item labels must be unique"))
    else if names.length != features.cols then
      Left(MvpaError.InvalidFeatureModelInput(s"feature name count ${names.length} != column count ${features.cols}"))
    else if names.exists(_.isEmpty) then Left(MvpaError.InvalidFeatureModelInput("feature names must be non-empty"))
    else if names.distinct.length != names.length then Left(MvpaError.InvalidFeatureModelInput("feature names must be unique"))
    else
      validateFinite(features, "feature design").map(_ => new FeatureModelDesign(itemVector, features, names))

  def unsafe(
      items: Seq[String],
      features: DMat,
      featureNames: Seq[String] = Seq.empty
  ): FeatureModelDesign =
    apply(items, features, featureNames).fold(error => throw new IllegalArgumentException(error.message), identity)

final class FeatureRidgeEstimator private (val penalty: RidgePenalty):
  def lambda: Double =
    penalty.value

object FeatureRidgeEstimator:
  def apply(lambda: Double = 1.0): FeatureRidgeEstimator =
    RidgePenalty(lambda).fold(error => throw new IllegalArgumentException(error.message), value => new FeatureRidgeEstimator(value))

  def fromPenalty(lambda: RidgePenalty): FeatureRidgeEstimator =
    new FeatureRidgeEstimator(lambda)

final case class FeatureModelPrediction(
    direction: FeaturePredictionDirection,
    items: Vector[String],
    targetNames: Vector[String],
    predicted: DMat,
    observed: DMat
):
  require(predicted.rows == observed.rows, "predicted and observed rows must match")
  require(predicted.cols == observed.cols, "predicted and observed columns must match")
  require(items.length == predicted.rows, "prediction item count must match rows")
  require(targetNames.length == predicted.cols, "prediction target name count must match columns")

final case class FeatureModelAnalysis(
    design: FeatureModelDesign,
    direction: FeaturePredictionDirection,
    estimator: FeatureRidgeEstimator = FeatureRidgeEstimator(),
    storePrediction: Boolean = false
) extends FoldRequiredDenseRoiAnalysis:
  override def name: String = s"feature_model_${direction.label}_ridge"
  override val minFeatures: Int = 1
  override def missingFoldsError: MvpaError =
    MvpaError.InvalidFeatureModelInput("feature model analysis requires a fold plan")

  override def evaluateFolded(roi: PatternMatrix, context: FoldedRoiContext): Either[MvpaError, RoiAnalysisResult] =
    for
      _ <- validateInputs(roi, context.foldPlan)
      prediction <- FeatureModelAnalysis.crossValidate(roi, design, direction, estimator, context.foldPlan)
      metrics <- FeatureModelMetrics.compute(prediction).map(_.withEstimator(estimator.lambda))
    yield
      val payload =
        if storePrediction then Some(RoiPayload.FeatureModel(prediction))
        else None
      RoiAnalysisResult(metrics, payload)

  private def validateInputs(roi: PatternMatrix, folds: FoldPlan): Either[MvpaError, Unit] =
    if design.features.rows != roi.samples then
      Left(MvpaError.InvalidFeatureModelInput(s"feature design rows ${design.features.rows} != ROI samples ${roi.samples}"))
    else if folds.samples != roi.samples then
      Left(MvpaError.ResponseLengthMismatch(roi.samples, folds.samples))
    else
      validateFinite(roi.value, "ROI pattern matrix")

object FeatureModelAnalysis:
  private def crossValidate(
      roi: PatternMatrix,
      design: FeatureModelDesign,
      direction: FeaturePredictionDirection,
      estimator: FeatureRidgeEstimator,
      folds: FoldPlan
  ): Either[MvpaError, FeatureModelPrediction] =
    val source =
      direction match
        case FeaturePredictionDirection.FeaturesToPatterns => design.features
        case FeaturePredictionDirection.PatternsToFeatures => roi.value
    val target =
      direction match
        case FeaturePredictionDirection.FeaturesToPatterns => roi.value
        case FeaturePredictionDirection.PatternsToFeatures => design.features
    val targetNames =
      direction match
        case FeaturePredictionDirection.FeaturesToPatterns => roi.featureIndices.map(index => s"pattern_${index.value}")
        case FeaturePredictionDirection.PatternsToFeatures => design.featureNames

    val testRows = folds.folds.flatMap(_.test.map(_.value)).distinct.sorted
    if testRows.isEmpty then Left(MvpaError.InvalidFeatureModelInput("fold plan produced no test samples"))
    else
      val rowToOutput = testRows.zipWithIndex.toMap
      val predicted = Matrix.newBuilder(testRows.length, target.cols)
      val observed = Matrix.newBuilder(testRows.length, target.cols)
      val counts = Array.fill(testRows.length)(0)

      var foldIndex = 0
      while foldIndex < folds.folds.length do
        val fold = folds.folds(foldIndex)
        val trainRows = fold.train.map(_.value)
        val test = fold.test.map(_.value)
        val foldResult =
          for
            sourceTrain <- selectRows(source, trainRows)
            targetTrain <- selectRows(target, trainRows)
            sourceTest <- selectRows(source, test)
            fit <- StandardizedRidgeMap.fit(sourceTrain, targetTrain, estimator.lambda)
            foldPredicted <- fit.predict(sourceTest)
          yield
            var localRow = 0
            while localRow < test.length do
              val outRow = rowToOutput(test(localRow))
              var col = 0
              while col < target.cols do
                predicted(outRow, col) = predicted(outRow, col) + foldPredicted(localRow, col)
                observed(outRow, col) = target(test(localRow), col)
                col += 1
              counts(outRow) += 1
              localRow += 1
        foldResult match
          case Left(error) => return Left(error)
          case Right(()) =>
        foldIndex += 1

      val missing = counts.indexWhere(_ == 0)
      if missing >= 0 then Left(MvpaError.InvalidFeatureModelInput("some test samples were never predicted"))
      else
        var row = 0
        while row < testRows.length do
          var col = 0
          while col < target.cols do
            predicted(row, col) = predicted(row, col) / counts(row)
            col += 1
          row += 1
        Right(
          FeatureModelPrediction(
            direction,
            testRows.map(index => design.items(index)).toVector,
            targetNames,
            predicted.result(),
            observed.result()
          )
        )

  private def selectRows(matrix: DMat, rows: IndexedSeq[Int]): Either[MvpaError, DMat] =
    if rows.isEmpty then Left(MvpaError.InvalidFeatureModelInput("feature model fold has no rows"))
    else rows.find(row => row < 0 || row >= matrix.rows) match
      case Some(row) => Left(MvpaError.FoldIndexOutOfBounds("feature_model", row, matrix.rows))
      case None =>
        val out = Matrix.newBuilder(rows.length, matrix.cols)
        var row = 0
        while row < rows.length do
          var col = 0
          while col < matrix.cols do
            out(row, col) = matrix(rows(row), col)
            col += 1
          row += 1
        Right(out.result())

private final case class StandardizedRidgeMap(
    sourceMeans: Array[Double],
    sourceScales: Array[Double],
    targetMeans: Array[Double],
    targetScales: Array[Double],
    coefficients: DMat
):
  def predict(source: DMat): Either[MvpaError, DMat] =
    if source.cols != sourceMeans.length then
      Left(MvpaError.InvalidFeatureModelInput(s"prediction source column count ${source.cols} != fitted source column count ${sourceMeans.length}"))
    else
      validateFinite(source, "feature model prediction source").map { _ =>
        val standardized = StandardizedRidgeMap.standardize(source, sourceMeans, sourceScales)
        val predicted = standardized * coefficients
        val out = Matrix.newBuilder(predicted.rows, predicted.cols)
        var row = 0
        while row < predicted.rows do
          var col = 0
          while col < predicted.cols do
            out(row, col) = predicted(row, col) * targetScales(col) + targetMeans(col)
            col += 1
          row += 1
        out.result()
      }

private object StandardizedRidgeMap:
  def fit(source: DMat, target: DMat, lambda: Double): Either[MvpaError, StandardizedRidgeMap] =
    if source.rows != target.rows then Left(MvpaError.InvalidFeatureModelInput(s"source rows ${source.rows} != target rows ${target.rows}"))
    else if source.rows < 2 then Left(MvpaError.InvalidFeatureModelInput("ridge feature model requires at least two training rows"))
    else if source.cols < 1 || target.cols < 1 then Left(MvpaError.InvalidFeatureModelInput("ridge feature model requires non-empty source and target columns"))
    else if !lambda.isFinite || lambda <= 0.0 then Left(MvpaError.InvalidFeatureModelInput("ridge lambda must be positive and finite"))
    else
      for
        _ <- validateFinite(source, "feature model source")
        _ <- validateFinite(target, "feature model target")
        sourceStats <- ColumnStats.from(source)
        targetStats <- ColumnStats.from(target)
        coefficients <- solve(source, target, sourceStats, targetStats, lambda)
      yield
        StandardizedRidgeMap(
          sourceStats.means,
          sourceStats.scales,
          targetStats.means,
          targetStats.scales,
          coefficients
        )

  private def solve(
      source: DMat,
      target: DMat,
      sourceStats: ColumnStats,
      targetStats: ColumnStats,
      lambda: Double
  ): Either[MvpaError, DMat] =
    val x = standardize(source, sourceStats.means, sourceStats.scales)
    val y = standardize(target, targetStats.means, targetStats.scales)
    val gram = x.t * x
    val gramBuilder = Matrix.newBuilder(gram.rows, gram.cols)
    var row = 0
    while row < gram.rows do
      var col = 0
      while col < gram.cols do
        gramBuilder(row, col) = gram(row, col) + (if row == col then lambda else 0.0)
        col += 1
      row += 1
    val xty = x.t * y
    gramBuilder.result().cholesky(CholeskyOptions(1e-12))
      .left
      .map(error => MvpaError.InvalidFeatureModelInput(s"ridge solve failed: ${error.getMessage}"))
      .flatMap(_.solve(xty).left.map(error => MvpaError.InvalidFeatureModelInput(s"ridge solve failed: ${error.getMessage}")))

  def standardize(matrix: DMat, means: Array[Double], scales: Array[Double]): DMat =
    val out = Matrix.newBuilder(matrix.rows, matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row, col) = (matrix(row, col) - means(col)) / scales(col)
        col += 1
      row += 1
    out.result()

private final case class ColumnStats(means: Array[Double], scales: Array[Double])

private object ColumnStats:
  private val Eps = 1e-12

  def from(matrix: DMat): Either[MvpaError, ColumnStats] =
    val means = new Array[Double](matrix.cols)
    val scales = new Array[Double](matrix.cols)
    var col = 0
    while col < matrix.cols do
      var sum = 0.0
      var row = 0
      while row < matrix.rows do
        sum += matrix(row, col)
        row += 1
      val mean = sum / matrix.rows
      means(col) = mean

      var ss = 0.0
      row = 0
      while row < matrix.rows do
        val centered = matrix(row, col) - mean
        ss += centered * centered
        row += 1
      val variance =
        if matrix.rows > 1 then ss / (matrix.rows - 1)
        else 0.0
      val scale = math.sqrt(math.max(variance, 0.0))
      scales(col) =
        if !scale.isFinite || scale <= Eps then 1.0
        else scale
      col += 1
    Right(ColumnStats(means, scales))

private final case class FeatureModelMetricSet(
    patternCorrelation: Double,
    patternDiscrimination: Double,
    patternRankPercentile: Double,
    rdmCorrelation: Double,
    targetCorrelation: Double,
    mse: Double,
    rSquared: Double,
    meanTargetwiseCorrelation: Double,
    observations: Int,
    targetColumns: Int
):
  def withEstimator(lambda: Double): MetricVector =
    MetricVector.from(
      Vector(
        "PatternCorrelation" -> patternCorrelation,
        "PatternDiscrimination" -> patternDiscrimination,
        "PatternRankPercentile" -> patternRankPercentile,
        "RdmCorrelation" -> rdmCorrelation,
        "TargetCorrelation" -> targetCorrelation,
        "Mse" -> mse,
        "RSquared" -> rSquared,
        "MeanTargetwiseCorrelation" -> meanTargetwiseCorrelation,
        "Observations" -> observations.toDouble,
        "TargetColumns" -> targetColumns.toDouble,
        "RidgeLambda" -> lambda
      )
    )

private object FeatureModelMetrics:
  def compute(prediction: FeatureModelPrediction): Either[MvpaError, FeatureModelMetricSet] =
    val predicted = prediction.predicted
    val observed = prediction.observed
    if predicted.rows != observed.rows || predicted.cols != observed.cols then
      Left(MvpaError.InvalidFeatureModelInput("predicted and observed matrices must have identical shape"))
    else
      for
        _ <- validateFinite(predicted, "feature model predictions")
        _ <- validateFinite(observed, "feature model observations")
      yield
        val matrixMetrics = patternMetrics(predicted, observed)
        FeatureModelMetricSet(
          patternCorrelation = matrixMetrics.patternCorrelation,
          patternDiscrimination = matrixMetrics.patternDiscrimination,
          patternRankPercentile = matrixMetrics.patternRankPercentile,
          rdmCorrelation = rdmCorrelation(predicted, observed),
          targetCorrelation = globalCorrelation(predicted, observed),
          mse = mse(predicted, observed),
          rSquared = rSquared(predicted, observed),
          meanTargetwiseCorrelation = meanColumnCorrelation(predicted, observed),
          observations = predicted.rows,
          targetColumns = predicted.cols
        )

  private final case class PatternMetrics(
      patternCorrelation: Double,
      patternDiscrimination: Double,
      patternRankPercentile: Double
  )

  private def patternMetrics(predicted: DMat, observed: DMat): PatternMetrics =
    if predicted.rows < 2 || predicted.cols < 2 then PatternMetrics(Double.NaN, Double.NaN, Double.NaN)
    else
      val cor = new Array[Double](predicted.rows * predicted.rows)
      var row = 0
      while row < predicted.rows do
        var col = 0
        while col < predicted.rows do
          cor(row * predicted.rows + col) = rowCorrelation(predicted, row, observed, col)
          col += 1
        row += 1

      var diagSum = 0.0
      var diagN = 0
      var offSum = 0.0
      var offN = 0
      var rankSum = 0.0
      var rankN = 0
      row = 0
      while row < predicted.rows do
        val diag = cor(row * predicted.rows + row)
        if diag.isFinite then
          diagSum += diag
          diagN += 1
          var lessOrEqual = 0
          var finite = 0
          var col = 0
          while col < predicted.rows do
            val value = cor(row * predicted.rows + col)
            if value.isFinite then
              finite += 1
              if value <= diag then lessOrEqual += 1
            if col != row then
              if value.isFinite then
                offSum += value
                offN += 1
            col += 1
          if finite > 1 then
            rankSum += (lessOrEqual - 1).toDouble / (finite - 1)
            rankN += 1
        row += 1

      val patternCorrelation =
        if diagN == 0 then Double.NaN else diagSum / diagN
      val offMean =
        if offN == 0 then Double.NaN else offSum / offN
      val patternDiscrimination =
        if patternCorrelation.isFinite && offMean.isFinite then patternCorrelation - offMean else Double.NaN
      val rank =
        if rankN == 0 then Double.NaN else rankSum / rankN
      PatternMetrics(patternCorrelation, patternDiscrimination, rank)

  private def rdmCorrelation(predicted: DMat, observed: DMat): Double =
    if predicted.rows < 3 || predicted.cols < 2 then Double.NaN
    else
      val result =
        for
          predictedRdm <- Rdm.correlation(predicted)
          observedRdm <- Rdm.correlation(observed)
          score <- RdmScorer.Spearman.score(predictedRdm, observedRdm)
        yield score
      result.getOrElse(Double.NaN)

  private def globalCorrelation(predicted: DMat, observed: DMat): Double =
    var predictedMean = 0.0
    var observedMean = 0.0
    var i = 0
    while i < predicted.rows * predicted.cols do
      predictedMean += predicted(i / predicted.cols, i % predicted.cols)
      observedMean += observed(i / observed.cols, i % observed.cols)
      i += 1
    predictedMean /= (predicted.rows * predicted.cols)
    observedMean /= (observed.rows * observed.cols)

    var numerator = 0.0
    var predictedSs = 0.0
    var observedSs = 0.0
    i = 0
    while i < predicted.rows * predicted.cols do
      val px = predicted(i / predicted.cols, i % predicted.cols) - predictedMean
      val oy = observed(i / observed.cols, i % observed.cols) - observedMean
      numerator += px * oy
      predictedSs += px * px
      observedSs += oy * oy
      i += 1
    val denom = math.sqrt(predictedSs * observedSs)
    if denom <= 0.0 then Double.NaN else numerator / denom

  private def meanColumnCorrelation(predicted: DMat, observed: DMat): Double =
    if predicted.rows < 2 then Double.NaN
    else
      var sum = 0.0
      var n = 0
      var col = 0
      while col < predicted.cols do
        val value = columnCorrelation(predicted, observed, col)
        if value.isFinite then
          sum += value
          n += 1
        col += 1
      if n == 0 then Double.NaN else sum / n

  private def mse(predicted: DMat, observed: DMat): Double =
    var sum = 0.0
    var i = 0
    while i < predicted.rows * predicted.cols do
      val diff = predicted(i / predicted.cols, i % predicted.cols) - observed(i / observed.cols, i % observed.cols)
      sum += diff * diff
      i += 1
    sum / (predicted.rows * predicted.cols)

  private def rSquared(predicted: DMat, observed: DMat): Double =
    var mean = 0.0
    var i = 0
    while i < observed.rows * observed.cols do
      mean += observed(i / observed.cols, i % observed.cols)
      i += 1
    mean /= (observed.rows * observed.cols)

    var rss = 0.0
    var tss = 0.0
    i = 0
    while i < observed.rows * observed.cols do
      val residual = observed(i / observed.cols, i % observed.cols) - predicted(i / predicted.cols, i % predicted.cols)
      val centered = observed(i / observed.cols, i % observed.cols) - mean
      rss += residual * residual
      tss += centered * centered
      i += 1
    if tss <= 0.0 then Double.NaN else 1.0 - rss / tss

  private def rowCorrelation(left: DMat, leftRow: Int, right: DMat, rightRow: Int): Double =
    val leftMean = rowMean(left, leftRow)
    val rightMean = rowMean(right, rightRow)
    var numerator = 0.0
    var leftSs = 0.0
    var rightSs = 0.0
    var col = 0
    while col < left.cols do
      val x = left(leftRow, col) - leftMean
      val y = right(rightRow, col) - rightMean
      numerator += x * y
      leftSs += x * x
      rightSs += y * y
      col += 1
    val denom = math.sqrt(leftSs * rightSs)
    if denom <= 0.0 then Double.NaN else numerator / denom

  private def columnCorrelation(left: DMat, right: DMat, col: Int): Double =
    var leftMean = 0.0
    var rightMean = 0.0
    var row = 0
    while row < left.rows do
      leftMean += left(row, col)
      rightMean += right(row, col)
      row += 1
    leftMean /= left.rows
    rightMean /= right.rows

    var numerator = 0.0
    var leftSs = 0.0
    var rightSs = 0.0
    row = 0
    while row < left.rows do
      val x = left(row, col) - leftMean
      val y = right(row, col) - rightMean
      numerator += x * y
      leftSs += x * x
      rightSs += y * y
      row += 1
    val denom = math.sqrt(leftSs * rightSs)
    if denom <= 0.0 then Double.NaN else numerator / denom

  private def rowMean(matrix: DMat, row: Int): Double =
    var sum = 0.0
    var col = 0
    while col < matrix.cols do
      sum += matrix(row, col)
      col += 1
    sum / matrix.cols

private def validateFinite(matrix: DMat, label: String): Either[MvpaError, Unit] =
  var row = 0
  while row < matrix.rows do
    var col = 0
    while col < matrix.cols do
      if !matrix(row, col).isFinite then
        return Left(MvpaError.InvalidFeatureModelInput(s"$label contains non-finite values"))
      col += 1
    row += 1
  Right(())
