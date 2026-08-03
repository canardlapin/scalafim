package scalafim.fmri.mvpa

import gale.linalg.{DMat, Matrix}

enum OperatorRdmExecution:
  case FoldwiseSufficientStatistics

/** Allocation and execution evidence for an operator-native RDM.
  *
  * The bound covers storage owned by this computation, excluding the supplied
  * operator and Gale's primitive apply scratch. It is independent of the
  * number of folds because each fold is reduced before the next is visited.
  */
final case class OperatorRdmReceipt(
    execution: OperatorRdmExecution,
    samples: Int,
    features: Int,
    conditions: Int,
    folds: Int,
    peakOwnedDoublesUpperBound: Long,
    avoidedTrialPatternDoubles: Long
):
  require(samples > 1, "operator RDM requires at least two samples")
  require(features > 0, "operator RDM requires at least one feature")
  require(conditions > 1, "operator RDM requires at least two conditions")
  require(folds > 1, "operator RDM requires at least two folds")
  require(peakOwnedDoublesUpperBound > 0L, "operator RDM working-set bound must be positive")
  require(avoidedTrialPatternDoubles == samples.toLong * features, "avoided pattern size must be samples by features")

  val trialByFeatureMaterializations: Int = 0

final case class OperatorCrossvalidatedGeometry private (
    classes: Vector[ClassLabel],
    crossvalidatedGram: DMat,
    observed: LabeledRdm,
    receipt: OperatorRdmReceipt
):
  require(classes.length == crossvalidatedGram.rows, "classes must match Gram rows")
  require(crossvalidatedGram.rows == crossvalidatedGram.cols, "crossvalidated Gram must be square")
  require(observed.rdm.items == classes.length, "observed RDM must match classes")
  require(receipt.conditions == classes.length, "receipt conditions must match classes")

/** Crossvalidated condition geometry derived from a pattern operator.
  *
  * For fold `r`, let `A_r` average samples into conditions and let `B` be the
  * sample-by-feature operator. This implementation obtains `(A_r B)^T` by one
  * adjoint application and immediately reduces it into
  *
  * `G = sum_{r != s} (A_r B)(A_s B)^T / (R(R - 1))`.
  *
  * Neither `B` nor a trial-by-feature coefficient table is materialized.
  */
object OperatorCrossvalidatedGeometry:
  def compute(
      patterns: PatternOperator,
      response: Response,
      folds: FoldPlan,
      normalizeByFeatures: Boolean = true
  ): Either[MvpaError, OperatorCrossvalidatedGeometry] =
    if folds.folds.length < 2 then
      Left(MvpaError.InvalidRdmInput("operator crossnobis requires at least two folds"))
    else if folds.samples != patterns.samples then
      Left(MvpaError.ResponseLengthMismatch(patterns.samples, folds.samples))
    else
      for
        labels <- Classification.categorical(response, patterns.samples)
        geometry <- accumulate(patterns, labels, folds, normalizeByFeatures)
      yield geometry

  private def accumulate(
      patterns: PatternOperator,
      labels: Vector[ClassLabel],
      foldPlan: FoldPlan,
      normalizeByFeatures: Boolean
  ): Either[MvpaError, OperatorCrossvalidatedGeometry] =
    val classes = labels.distinct
    val classIndex = classes.zipWithIndex.map((label, index) => label -> index).toMap
    val conditions = classes.length
    val foldCount = foldPlan.folds.length
    val summedPatterns = new Array[Double](patterns.features * conditions)
    val withinGram = new Array[Double](conditions * conditions)

    var foldIndex = 0
    while foldIndex < foldCount do
      val fold = foldPlan.folds(foldIndex)
      val counts = new Array[Int](conditions)
      var row = 0
      while row < fold.test.length do
        val sample = fold.test(row).value
        counts(classIndex(labels(sample))) += 1
        row += 1

      var condition = 0
      while condition < conditions do
        if counts(condition) == 0 then
          return Left(
            MvpaError.InvalidRdmInput(
              s"crossnobis fold '${fold.id}' has no samples for class '${classes(condition).value}'"
            )
          )
        condition += 1

      val weights = Matrix.newBuilder(patterns.samples, conditions)
      row = 0
      while row < fold.test.length do
        val sample = fold.test(row).value
        val classPosition = classIndex(labels(sample))
        weights(sample, classPosition) += 1.0 / counts(classPosition)
        row += 1

      patterns.transposeApplyTo(weights.result()) match
        case Left(error) =>
          return Left(error)
        case Right(foldPatterns) =>
          var feature = 0
          while feature < patterns.features do
            condition = 0
            while condition < conditions do
              summedPatterns(feature * conditions + condition) += foldPatterns(feature, condition)
              condition += 1
            feature += 1

          var left = 0
          while left < conditions do
            var right = 0
            while right < conditions do
              var dot = 0.0
              feature = 0
              while feature < patterns.features do
                dot += foldPatterns(feature, left) * foldPatterns(feature, right)
                feature += 1
              withinGram(left * conditions + right) += dot
              right += 1
            left += 1
      foldIndex += 1

    val gram = Matrix.newBuilder(conditions, conditions)
    val foldPairs = foldCount.toDouble * (foldCount - 1).toDouble
    var left = 0
    while left < conditions do
      var right = 0
      while right < conditions do
        var total = 0.0
        var feature = 0
        while feature < patterns.features do
          total += summedPatterns(feature * conditions + left) * summedPatterns(feature * conditions + right)
          feature += 1
        gram(left, right) = (total - withinGram(left * conditions + right)) / foldPairs
        right += 1
      left += 1

    val crossvalidatedGram = gram.result()
    val pairs = Rdm.pairIndices(conditions)
    val distances = new Array[Double](pairs.length)
    var pair = 0
    while pair < pairs.length do
      val (row, col) = pairs(pair)
      val raw = crossvalidatedGram(row, row) +
        crossvalidatedGram(col, col) -
        2.0 * crossvalidatedGram(row, col)
      distances(pair) = if normalizeByFeatures then raw / patterns.features else raw
      if !distances(pair).isFinite then
        return Left(MvpaError.InvalidRdmInput("operator crossnobis produced a non-finite distance"))
      pair += 1

    val rdm = RdmVector.unsafe(conditions, distances.toVector)
    val observed = LabeledRdm.unsafe(classes.map(_.value), rdm)
    val samples = patterns.samples.toLong
    val features = patterns.features.toLong
    val conditionCount = conditions.toLong
    val receipt = OperatorRdmReceipt(
      execution = OperatorRdmExecution.FoldwiseSufficientStatistics,
      samples = patterns.samples,
      features = patterns.features,
      conditions = conditions,
      folds = foldCount,
      peakOwnedDoublesUpperBound = samples * conditionCount +
        2L * features * conditionCount +
        2L * conditionCount * conditionCount,
      avoidedTrialPatternDoubles = samples * features
    )
    Right(new OperatorCrossvalidatedGeometry(classes, crossvalidatedGram, observed, receipt))

final case class OperatorCrossnobisAnalysis(
    normalizeByFeatures: Boolean = true,
    storeRdm: Boolean = true
) extends FoldRequiredOperatorRoiAnalysis:
  override def name: String =
    if normalizeByFeatures then "operator_crossnobis_normalized" else "operator_crossnobis"

  override val minFeatures: Int = 1
  override def missingFoldsError: MvpaError =
    MvpaError.InvalidRdmInput("operator crossnobis analysis requires a fold plan")

  override def evaluateFolded(
      roi: PatternOperator,
      context: FoldedRoiContext
  ): Either[MvpaError, RoiAnalysisResult] =
    OperatorCrossvalidatedGeometry
      .compute(roi, context.response, context.foldPlan, normalizeByFeatures)
      .map: geometry =>
        val payload =
          if storeRdm then Some(RoiPayload.Rdm(geometry.observed))
          else None
        RoiAnalysisResult(operatorMetrics(geometry), payload)

final case class OperatorCrossnobisRsaAnalysis(
    models: Vector[RdmModel],
    scorer: RdmScorer = RdmScorer.Pearson,
    normalizeByFeatures: Boolean = true,
    storeObservedRdm: Boolean = false
) extends FoldRequiredOperatorRoiAnalysis:
  require(models.nonEmpty, "operator RSA analysis requires at least one model")
  require(models.map(_.name).distinct.length == models.length, "operator RSA model names must be unique")

  override def name: String = s"operator_crossnobis_rsa_${scorer.name.toLowerCase}"
  override val minFeatures: Int = 1
  override def missingFoldsError: MvpaError =
    MvpaError.InvalidRdmInput("operator crossnobis RSA requires a fold plan")

  override def evaluateFolded(
      roi: PatternOperator,
      context: FoldedRoiContext
  ): Either[MvpaError, RoiAnalysisResult] =
    for
      geometry <- OperatorCrossvalidatedGeometry
        .compute(roi, context.response, context.foldPlan, normalizeByFeatures)
      scores <- RsaAnalysisSupport.scoreModels(models, scorer, geometry.observed)
    yield
      val scoreMetrics = scores.map(score => s"${score.modelName}.${scorer.name}" -> score.value)
      val metrics = MetricVector.from(operatorMetricPairs(geometry) ++ scoreMetrics)
      val observed = if storeObservedRdm then Some(geometry.observed) else None
      RoiAnalysisResult(metrics, Some(RoiPayload.Rsa(observed, scores)))

private def operatorMetrics(geometry: OperatorCrossvalidatedGeometry): MetricVector =
  MetricVector.from(operatorMetricPairs(geometry))

private def operatorMetricPairs(geometry: OperatorCrossvalidatedGeometry): Vector[(String, Double)] =
  RdmAnalysisSupport.rdmMetricPairs(geometry.observed.rdm, geometry.receipt.features) ++
    Vector(
      "Folds" -> geometry.receipt.folds.toDouble,
      "TrialPatternMaterializations" -> geometry.receipt.trialByFeatureMaterializations.toDouble,
      "PeakOwnedDoublesUpperBound" -> geometry.receipt.peakOwnedDoublesUpperBound.toDouble
    )
