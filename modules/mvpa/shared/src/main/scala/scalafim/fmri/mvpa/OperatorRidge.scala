package scalafim.fmri.mvpa

import gale.linalg.{DMat, DoubleLinearOperator, LinAlgError, LinearOperator, Matrix, MutableDVec}
import gale.solvers.{SolverConfig, ToleranceMode, lsqr}

opaque type OperatorRidgeTolerance = Double

object OperatorRidgeTolerance:
  def apply(value: Double): Either[OperatorRidgeError, OperatorRidgeTolerance] =
    if !value.isFinite || value <= 0.0 then Left(OperatorRidgeError.InvalidTolerance(value))
    else Right(value)

  private[mvpa] def unsafe(value: Double): OperatorRidgeTolerance =
    value

  extension (tolerance: OperatorRidgeTolerance)
    inline def value: Double = tolerance

opaque type OperatorRidgeIterationLimit = Int

object OperatorRidgeIterationLimit:
  def apply(value: Int): Either[OperatorRidgeError, OperatorRidgeIterationLimit] =
    if value <= 0 then Left(OperatorRidgeError.InvalidIterationLimit(value))
    else Right(value)

  private[mvpa] def unsafe(value: Int): OperatorRidgeIterationLimit =
    value

  extension (limit: OperatorRidgeIterationLimit)
    inline def value: Int = limit

enum OperatorRidgeError:
  case InvalidPenalty(value: Double)
  case InvalidTolerance(value: Double)
  case InvalidIterationLimit(value: Int)
  case TargetLengthMismatch(expected: Int, actual: Int)
  case InsufficientTrainingSamples(fitId: String, actual: Int)
  case MissingTrainingClass(fitId: String, classLabel: ClassLabel)
  case FoldSampleMismatch(expected: Int, actual: Int)
  case NoTestSamples
  case MissingPrediction(sample: SampleIndex)
  case PredictionShapeMismatch(detail: String)
  case FeatureAxisMismatch(expected: Vector[FeatureIndex], actual: Vector[FeatureIndex])
  case PatternFailure(detail: String)
  case NumericalFailure(fitId: String, cause: LinAlgError)
  case DidNotConverge(fitId: String, classLabel: ClassLabel, iterations: Int, normalResidual: Double)
  case NonFiniteResult(fitId: String, stage: String)

  def message: String =
    this match
      case InvalidPenalty(value) =>
        s"ridge penalty must be positive and finite, got $value"
      case InvalidTolerance(value) =>
        s"LSQR tolerance must be positive and finite, got $value"
      case InvalidIterationLimit(value) =>
        s"LSQR iteration limit must be positive, got $value"
      case TargetLengthMismatch(expected, actual) =>
        s"ridge target length mismatch: expected $expected, got $actual"
      case InsufficientTrainingSamples(fitId, actual) =>
        s"ridge fit '$fitId' requires at least two training samples, got $actual"
      case MissingTrainingClass(fitId, classLabel) =>
        s"ridge fit '$fitId' has no training membership mass for class ${classLabel.value}"
      case FoldSampleMismatch(expected, actual) =>
        s"ridge fold sample count mismatch: expected $expected, got $actual"
      case NoTestSamples =>
        "ridge fold plan produced no test samples"
      case MissingPrediction(sample) =>
        s"ridge fold plan never predicted sample ${sample.value}"
      case PredictionShapeMismatch(detail) =>
        detail
      case FeatureAxisMismatch(expected, actual) =>
        s"ridge model feature axis ${expected.map(_.value)} != prediction axis ${actual.map(_.value)}"
      case PatternFailure(detail) =>
        detail
      case NumericalFailure(fitId, cause) =>
        s"ridge fit '$fitId' failed numerically: ${cause.getMessage}"
      case DidNotConverge(fitId, classLabel, iterations, normalResidual) =>
        s"ridge fit '$fitId' for class ${classLabel.value} did not converge after $iterations iteration(s); " +
          s"normal residual=$normalResidual"
      case NonFiniteResult(fitId, stage) =>
        s"ridge fit '$fitId' produced non-finite values in $stage"

final class OperatorRidgeConfig private (
    val penalty: RidgePenalty,
    val tolerance: OperatorRidgeTolerance,
    val maxIterations: OperatorRidgeIterationLimit
)

object OperatorRidgeConfig:
  val Default: OperatorRidgeConfig =
    new OperatorRidgeConfig(
      RidgePenalty.unsafe(1.0),
      OperatorRidgeTolerance.unsafe(1e-10),
      OperatorRidgeIterationLimit.unsafe(2000)
    )

  def apply(
      penalty: Double = 1.0,
      tolerance: Double = 1e-10,
      maxIterations: Int = 2000
  ): Either[OperatorRidgeError, OperatorRidgeConfig] =
    for
      ridge <- RidgePenalty(penalty).left.map(_ => OperatorRidgeError.InvalidPenalty(penalty))
      solverTolerance <- OperatorRidgeTolerance(tolerance)
      iterationLimit <- OperatorRidgeIterationLimit(maxIterations)
    yield new OperatorRidgeConfig(ridge, solverTolerance, iterationLimit)

  private[mvpa] def unsafe(
      penalty: Double = 1.0,
      tolerance: Double = 1e-10,
      maxIterations: Int = 2000
  ): OperatorRidgeConfig =
    apply(penalty, tolerance, maxIterations)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

enum OperatorRidgeExecutionMode:
  case OperatorProducts

final case class OperatorRidgeClassReceipt(
    classLabel: ClassLabel,
    iterations: Int,
    normalResidual: Double
):
  require(iterations >= 0, "ridge iterations must be non-negative")
  require(normalResidual.isFinite && normalResidual >= 0.0, "ridge residual must be finite and non-negative")

final case class OperatorRidgeFitReceipt(
    trainingSamples: Int,
    features: Int,
    penalty: RidgePenalty,
    tolerance: OperatorRidgeTolerance,
    maxIterations: OperatorRidgeIterationLimit,
    patternProvenance: PatternOperatorProvenance,
    classFits: Vector[OperatorRidgeClassReceipt],
    forwardApplications: Int,
    transposeApplications: Int
):
  require(trainingSamples >= 2, "ridge receipt requires at least two training samples")
  require(features > 0, "ridge receipt requires at least one feature")
  require(classFits.length >= 2, "ridge receipt requires at least two class fits")
  require(forwardApplications >= 0, "forward application count must be non-negative")
  require(transposeApplications >= 0, "transpose application count must be non-negative")

final case class OperatorRidgeFoldReceipt(
    foldId: String,
    testSamples: Int,
    fit: OperatorRidgeFitReceipt
):
  require(foldId.trim.nonEmpty, "ridge fold receipt id must be non-empty")
  require(testSamples > 0, "ridge fold receipt requires test samples")

final case class OperatorRidgeCrossValidationReceipt(
    targetKind: ClassMembershipKind,
    executionMode: OperatorRidgeExecutionMode,
    folds: Vector[OperatorRidgeFoldReceipt]
):
  require(folds.nonEmpty, "ridge cross-validation receipt requires folds")

  def totalIterations: Int =
    folds.flatMap(_.fit.classFits).map(_.iterations).sum

  def maxNormalResidual: Double =
    folds.flatMap(_.fit.classFits).map(_.normalResidual).max

  def forwardApplications: Int =
    folds.map(_.fit.forwardApplications).sum

  def transposeApplications: Int =
    folds.map(_.fit.transposeApplications).sum

final case class OperatorRidgePrediction(
    classes: Vector[ClassLabel],
    scores: DMat,
    sampleIndices: Vector[SampleIndex]
):
  require(classes.length >= 2, "ridge prediction requires at least two classes")
  require(scores.cols == classes.length, "ridge score columns must match classes")
  require(scores.rows == sampleIndices.length, "ridge score rows must match sample indices")

  def predicted: Vector[ClassLabel] =
    val out = Vector.newBuilder[ClassLabel]
    out.sizeHint(scores.rows)
    var sample = 0
    while sample < scores.rows do
      var bestClass = 0
      var bestScore = scores(sample, 0)
      var klass = 1
      while klass < scores.cols do
        val candidate = scores(sample, klass)
        if candidate > bestScore then
          bestScore = candidate
          bestClass = klass
        klass += 1
      out += classes(bestClass)
      sample += 1
    out.result()

final case class OperatorRidgeCrossValidatedResult(
    prediction: OperatorRidgePrediction,
    targetMse: Double,
    targetArgmaxAccuracy: Double,
    receipt: OperatorRidgeCrossValidationReceipt
):
  require(targetMse.isFinite && targetMse >= 0.0, "ridge target MSE must be finite and non-negative")
  require(
    targetArgmaxAccuracy.isFinite && targetArgmaxAccuracy >= 0.0 && targetArgmaxAccuracy <= 1.0,
    "ridge target argmax accuracy must be finite and in [0, 1]"
  )

final case class OperatorRidgePayload(
    prediction: Option[OperatorRidgePrediction],
    receipt: OperatorRidgeCrossValidationReceipt
)

final class OperatorRidgeModel private[mvpa] (
    val classes: Vector[ClassLabel],
    val featureIndices: Vector[FeatureIndex],
    val coefficients: DMat,
    val intercepts: Vector[Double],
    val receipt: OperatorRidgeFitReceipt
):
  require(classes.length >= 2, "ridge model requires at least two classes")
  require(coefficients.rows == featureIndices.length, "coefficient rows must match features")
  require(coefficients.cols == classes.length, "coefficient columns must match classes")
  require(intercepts.length == classes.length, "intercepts must match classes")

  def predict(test: PatternOperator): Either[OperatorRidgeError, OperatorRidgePrediction] =
    if test.featureIndices != featureIndices then
      Left(OperatorRidgeError.FeatureAxisMismatch(featureIndices, test.featureIndices))
    else
      test
        .applyTo(coefficients)
        .left
        .map(error => OperatorRidgeError.PatternFailure(error.message))
        .flatMap: rawScores =>
          val scores = Matrix.newBuilder(rawScores.rows, rawScores.cols)
          var sample = 0
          var finite = true
          while sample < rawScores.rows && finite do
            var klass = 0
            while klass < rawScores.cols && finite do
              val score = rawScores(sample, klass) + intercepts(klass)
              if !score.isFinite then finite = false
              else scores(sample, klass) = score
              klass += 1
            sample += 1
          if finite then Right(OperatorRidgePrediction(classes, scores.result(), test.sampleIndices))
          else Left(OperatorRidgeError.NonFiniteResult("prediction", "class scores"))

object OperatorRidge:
  private val MembershipMassTolerance = 1e-12

  def fit(
      train: PatternOperator,
      targets: ClassMembership,
      config: OperatorRidgeConfig = OperatorRidgeConfig.Default
  ): Either[OperatorRidgeError, OperatorRidgeModel] =
    fitBlock(train, targets.classes, targets.values, config, "fit")

  def crossValidate(
      data: PatternOperator,
      targets: ClassMembership,
      folds: FoldPlan,
      config: OperatorRidgeConfig = OperatorRidgeConfig.Default
  ): Either[OperatorRidgeError, OperatorRidgeCrossValidatedResult] =
    if targets.samples != data.samples then
      Left(OperatorRidgeError.TargetLengthMismatch(data.samples, targets.samples))
    else if folds.samples != data.samples then
      Left(OperatorRidgeError.FoldSampleMismatch(data.samples, folds.samples))
    else
      val testRows = folds.folds.flatMap(_.test.map(_.value)).distinct.sorted
      if testRows.isEmpty then Left(OperatorRidgeError.NoTestSamples)
      else
        val rowToOutput = testRows.zipWithIndex.toMap
        val scoreSums = Matrix.newBuilder(testRows.length, targets.classCount)
        val predictionCounts = Array.fill(testRows.length)(0)
        val receipts = Vector.newBuilder[OperatorRidgeFoldReceipt]

        def processFold(fold: Fold): Either[OperatorRidgeError, Unit] =
          val trainPositions = fold.train.map(_.value)
          val testPositions = fold.test.map(_.value)
          val trainTargets = targets.selectPositions(trainPositions)
          missingClass(trainTargets, targets.classes) match
            case Some(classLabel) =>
              Left(OperatorRidgeError.MissingTrainingClass(fold.id, classLabel))
            case None =>
              for
                train <- data
                  .selectRows(fold.train)
                  .left
                  .map(error => OperatorRidgeError.PatternFailure(error.message))
                test <- data
                  .selectRows(fold.test)
                  .left
                  .map(error => OperatorRidgeError.PatternFailure(error.message))
                model <- fitBlock(train, targets.classes, trainTargets, config, fold.id)
                prediction <- model.predict(test)
                _ <- validateFoldPrediction(prediction, testPositions.length, targets.classes)
              yield
                var localRow = 0
                while localRow < testPositions.length do
                  val outputRow = rowToOutput(testPositions(localRow))
                  var klass = 0
                  while klass < targets.classCount do
                    scoreSums(outputRow, klass) =
                      scoreSums(outputRow, klass) + prediction.scores(localRow, klass)
                    klass += 1
                  predictionCounts(outputRow) += 1
                  localRow += 1
                receipts += OperatorRidgeFoldReceipt(fold.id, testPositions.length, model.receipt)

        var foldIndex = 0
        var failure = Option.empty[OperatorRidgeError]
        while foldIndex < folds.folds.length && failure.isEmpty do
          processFold(folds.folds(foldIndex)) match
            case Left(error) => failure = Some(error)
            case Right(())   =>
          foldIndex += 1

        failure match
          case Some(error) => Left(error)
          case None =>
            val missing = predictionCounts.indexWhere(_ == 0)
            if missing >= 0 then
              Left(OperatorRidgeError.MissingPrediction(SampleIndex(testRows(missing))))
            else
              var outputRow = 0
              while outputRow < testRows.length do
                var klass = 0
                while klass < targets.classCount do
                  scoreSums(outputRow, klass) = scoreSums(outputRow, klass) / predictionCounts(outputRow)
                  klass += 1
                outputRow += 1
              val scores = scoreSums.result()
              finiteMatrix(scores, "cross-validated class scores", "cross-validation") match
                case Left(error) => Left(error)
                case Right(()) =>
                  val prediction = OperatorRidgePrediction(
                    classes = targets.classes,
                    scores = scores,
                    sampleIndices = testRows.map(position => data.sampleIndices(position)).toVector
                  )
                  val (mse, accuracy) = scoreMetrics(prediction, targets, testRows)
                  Right(
                    OperatorRidgeCrossValidatedResult(
                      prediction = prediction,
                      targetMse = mse,
                      targetArgmaxAccuracy = accuracy,
                      receipt = OperatorRidgeCrossValidationReceipt(
                        targetKind = targets.kind,
                        executionMode = OperatorRidgeExecutionMode.OperatorProducts,
                        folds = receipts.result()
                      )
                    )
                  )

  private def fitBlock(
      train: PatternOperator,
      classes: Vector[ClassLabel],
      targetValues: DMat,
      config: OperatorRidgeConfig,
      fitId: String
  ): Either[OperatorRidgeError, OperatorRidgeModel] =
    if targetValues.rows != train.samples then
      Left(OperatorRidgeError.TargetLengthMismatch(train.samples, targetValues.rows))
    else if targetValues.cols != classes.length then
      Left(
        OperatorRidgeError.PredictionShapeMismatch(
          s"ridge target columns ${targetValues.cols} do not match class count ${classes.length}"
        )
      )
    else if train.samples < 2 then
      Left(OperatorRidgeError.InsufficientTrainingSamples(fitId, train.samples))
    else
      missingClass(targetValues, classes) match
        case Some(classLabel) => Left(OperatorRidgeError.MissingTrainingClass(fitId, classLabel))
        case None =>
          try
            val counter = new OperatorApplicationCounter
            val checked = checkedOperator(train.linear, counter, fitId)
            val featureMeans = columnMeans(checked)
            val targetMeans = columnMeans(targetValues)
            val centered = centeredOperator(checked, featureMeans)
            val augmented = augmentedRidgeOperator(centered, math.sqrt(config.penalty.value))
            val coefficients = Matrix.newBuilder(train.features, classes.length)
            val intercepts = new Array[Double](classes.length)
            val classReceipts = Vector.newBuilder[OperatorRidgeClassReceipt]
            val solverConfig = SolverConfig(
              tolerance = config.tolerance.value,
              maxIterations = config.maxIterations.value
            )

            var klass = 0
            while klass < classes.length do
              val rhs = MutableDVec.zeros(train.samples + train.features)
              var sample = 0
              while sample < train.samples do
                rhs(sample) = targetValues(sample, klass) - targetMeans(klass)
                sample += 1

              val solved = lsqr(
                augmented,
                rhs.asVec,
                solverConfig,
                ToleranceMode.RelativeToRhs
              )
              if !solved.converged then
                return Left(
                  OperatorRidgeError.DidNotConverge(
                    fitId,
                    classes(klass),
                    solved.iterations,
                    solved.residual
                  )
                )
              if !solved.residual.isFinite then
                return Left(OperatorRidgeError.NonFiniteResult(fitId, "LSQR residual"))

              var feature = 0
              var meanContribution = 0.0
              while feature < train.features do
                val coefficient = solved.x(feature)
                if !coefficient.isFinite then
                  return Left(OperatorRidgeError.NonFiniteResult(fitId, "ridge coefficients"))
                coefficients(feature, klass) = coefficient
                meanContribution += featureMeans(feature) * coefficient
                feature += 1
              val intercept = targetMeans(klass) - meanContribution
              if !intercept.isFinite then
                return Left(OperatorRidgeError.NonFiniteResult(fitId, "ridge intercepts"))
              intercepts(klass) = intercept
              classReceipts += OperatorRidgeClassReceipt(classes(klass), solved.iterations, solved.residual)
              klass += 1

            val receipt = OperatorRidgeFitReceipt(
              trainingSamples = train.samples,
              features = train.features,
              penalty = config.penalty,
              tolerance = config.tolerance,
              maxIterations = config.maxIterations,
              patternProvenance = train.provenance,
              classFits = classReceipts.result(),
              forwardApplications = counter.forwardApplications,
              transposeApplications = counter.transposeApplications
            )
            Right(
              new OperatorRidgeModel(
                classes = classes,
                featureIndices = train.featureIndices,
                coefficients = coefficients.result(),
                intercepts = intercepts.toVector,
                receipt = receipt
              )
            )
          catch
            case cause: LinAlgError =>
              Left(OperatorRidgeError.NumericalFailure(fitId, cause))

  private def checkedOperator(
      source: DoubleLinearOperator,
      counter: OperatorApplicationCounter,
      fitId: String
  ): DoubleLinearOperator =
    LinearOperator.fromFunctions(source.rows, source.cols)(
      (input, output) =>
        counter.forwardApplications += 1
        source.applyTo(input, output)
        requireFinite(output, s"ridge fit '$fitId' forward operator output"),
      (input, output) =>
        counter.transposeApplications += 1
        source.transposeApplyTo(input, output)
        requireFinite(output, s"ridge fit '$fitId' transpose operator output")
    )

  private def centeredOperator(
      source: DoubleLinearOperator,
      featureMeans: Array[Double]
  ): DoubleLinearOperator =
    LinearOperator.fromFunctions(source.rows, source.cols)(
      (input, output) =>
        source.applyTo(input, output)
        var meanScore = 0.0
        var feature = 0
        while feature < source.cols do
          meanScore += featureMeans(feature) * input(feature)
          feature += 1
        var sample = 0
        while sample < source.rows do
          output(sample) = output(sample) - meanScore
          sample += 1,
      (input, output) =>
        source.transposeApplyTo(input, output)
        var sampleSum = 0.0
        var sample = 0
        while sample < source.rows do
          sampleSum += input(sample)
          sample += 1
        var feature = 0
        while feature < source.cols do
          output(feature) = output(feature) - featureMeans(feature) * sampleSum
          feature += 1
    )

  private def augmentedRidgeOperator(
      centered: DoubleLinearOperator,
      sqrtPenalty: Double
  ): DoubleLinearOperator =
    LinearOperator.fromFunctions(centered.rows + centered.cols, centered.cols)(
      (input, output) =>
        val centeredOutput = MutableDVec.zeros(centered.rows)
        centered.applyTo(input, centeredOutput)
        var sample = 0
        while sample < centered.rows do
          output(sample) = centeredOutput(sample)
          sample += 1
        var feature = 0
        while feature < centered.cols do
          output(centered.rows + feature) = sqrtPenalty * input(feature)
          feature += 1,
      (input, output) =>
        val centeredInput = MutableDVec.zeros(centered.rows)
        var sample = 0
        while sample < centered.rows do
          centeredInput(sample) = input(sample)
          sample += 1
        centered.transposeApplyTo(centeredInput.asVec, output)
        var feature = 0
        while feature < centered.cols do
          output(feature) = output(feature) + sqrtPenalty * input(centered.rows + feature)
          feature += 1
    )

  private def columnMeans(operator: DoubleLinearOperator): Array[Double] =
    val scaledOnes = MutableDVec.zeros(operator.rows)
    var sample = 0
    while sample < operator.rows do
      scaledOnes(sample) = 1.0 / operator.rows
      sample += 1
    val output = MutableDVec.zeros(operator.cols)
    operator.transposeApplyTo(scaledOnes.asVec, output)
    val means = new Array[Double](operator.cols)
    var feature = 0
    while feature < operator.cols do
      means(feature) = output(feature)
      feature += 1
    means

  private def columnMeans(matrix: DMat): Array[Double] =
    val means = new Array[Double](matrix.cols)
    var col = 0
    while col < matrix.cols do
      var total = 0.0
      var row = 0
      while row < matrix.rows do
        total += matrix(row, col)
        row += 1
      means(col) = total / matrix.rows
      col += 1
    means

  private def missingClass(
      memberships: DMat,
      classes: Vector[ClassLabel]
  ): Option[ClassLabel] =
    var klass = 0
    while klass < memberships.cols do
      var mass = 0.0
      var sample = 0
      while sample < memberships.rows do
        mass += memberships(sample, klass)
        sample += 1
      if mass <= MembershipMassTolerance then return Some(classes(klass))
      klass += 1
    None

  private def validateFoldPrediction(
      prediction: OperatorRidgePrediction,
      expectedSamples: Int,
      expectedClasses: Vector[ClassLabel]
  ): Either[OperatorRidgeError, Unit] =
    if prediction.scores.rows != expectedSamples then
      Left(
        OperatorRidgeError.PredictionShapeMismatch(
          s"ridge prediction rows ${prediction.scores.rows} do not match fold test rows $expectedSamples"
        )
      )
    else if prediction.classes != expectedClasses then
      Left(OperatorRidgeError.PredictionShapeMismatch("ridge prediction class axis changed across folds"))
    else Right(())

  private def scoreMetrics(
      prediction: OperatorRidgePrediction,
      targets: ClassMembership,
      targetPositions: Vector[Int]
  ): (Double, Double) =
    var squaredError = 0.0
    var correct = 0
    val predicted = prediction.predicted
    var row = 0
    while row < targetPositions.length do
      val targetRow = targetPositions(row)
      var klass = 0
      while klass < targets.classCount do
        val difference = prediction.scores(row, klass) - targets.values(targetRow, klass)
        squaredError += difference * difference
        klass += 1
      if predicted(row) == targets.argmaxLabelAt(targetRow) then correct += 1
      row += 1
    val mse = squaredError / (targetPositions.length * targets.classCount)
    val accuracy = correct.toDouble / targetPositions.length
    (mse, accuracy)

  private def finiteMatrix(
      matrix: DMat,
      stage: String,
      fitId: String
  ): Either[OperatorRidgeError, Unit] =
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        if !matrix(row, col).isFinite then
          return Left(OperatorRidgeError.NonFiniteResult(fitId, stage))
        col += 1
      row += 1
    Right(())

  private def requireFinite(vector: MutableDVec, stage: String): Unit =
    var index = 0
    while index < vector.length do
      if !vector(index).isFinite then throw LinAlgError.InvalidArgument(s"$stage contains non-finite values")
      index += 1

  private final class OperatorApplicationCounter:
    var forwardApplications: Int = 0
    var transposeApplications: Int = 0

final case class CrossValidatedOperatorRidgeAnalysis(
    config: OperatorRidgeConfig = OperatorRidgeConfig.Default,
    storePredictions: Boolean = false
) extends FoldRequiredOperatorRoiAnalysis:
  override val name: String = "cv_operator_ridge"
  override val minFeatures: Int = 1
  override val missingFoldsError: MvpaError = MvpaError.MissingFoldPlan(name)

  override def evaluateFolded(
      roi: PatternOperator,
      context: FoldedRoiContext
  ): Either[MvpaError, RoiAnalysisResult] =
    for
      targets <- ClassMembership.fromResponse(context.response)
      evaluated <- OperatorRidge
        .crossValidate(roi, targets, context.foldPlan, config)
        .left
        .map(MvpaError.OperatorRidgeFailed.apply)
    yield
      RoiAnalysisResult(
        metrics = MetricVector(
          "TargetMse" -> evaluated.targetMse,
          "TargetArgmaxAccuracy" -> evaluated.targetArgmaxAccuracy,
          "TestedSamples" -> evaluated.prediction.scores.rows.toDouble,
          "FoldCount" -> evaluated.receipt.folds.length.toDouble,
          "SolverIterations" -> evaluated.receipt.totalIterations.toDouble,
          "MaxNormalResidual" -> evaluated.receipt.maxNormalResidual
        ),
        payload = Some(
          RoiPayload.OperatorRidge(
            OperatorRidgePayload(
              prediction = if storePredictions then Some(evaluated.prediction) else None,
              receipt = evaluated.receipt
            )
          )
        )
      )
