package scalafim.fmri.mvpa.fit

import scalafim.multivar.core.*
import scalafim.multivar.family.canonical.*

import gale.linalg.{CholeskyOptions, DMat, Matrix}
import scalafim.fmri.mvpa.*

enum SoftLdaComponents:
  case Maximum
  case Fixed(count: ComponentCount)

enum SoftLdaExecutionMode:
  case PulledBackOperatorProducts

final case class SoftLdaConfig(
    withinPolicy: WithinScatterPolicy,
    objective: LdaObjective = LdaObjective.FisherRayleigh,
    components: SoftLdaComponents = SoftLdaComponents.Maximum,
    trialNuisance: Option[TrialNuisanceDesign] = None
)

enum SoftLdaError:
  case TargetLengthMismatch(expected: Int, actual: Int)
  case FoldSampleMismatch(expected: Int, actual: Int)
  case TrialNuisanceLengthMismatch(expected: Int, actual: Int)
  case MissingPrediction(sample: SampleIndex)
  case InvalidTrainingFold(foldId: String, detail: String)
  case PatternFailure(foldId: String, detail: String)
  case SemanticFailure(foldId: String, detail: String)
  case LdaFailure(foldId: String, cause: MultivarError)
  case NumericalFailure(foldId: String, detail: String)

  def message: String =
    this match
      case TargetLengthMismatch(expected, actual) =>
        s"soft-LDA target length mismatch: expected $expected, got $actual"
      case FoldSampleMismatch(expected, actual) =>
        s"soft-LDA fold sample count mismatch: expected $expected, got $actual"
      case TrialNuisanceLengthMismatch(expected, actual) =>
        s"trial-level nuisance row count mismatch: expected $expected, got $actual"
      case MissingPrediction(sample) =>
        s"soft-LDA fold plan never predicted sample ${sample.value}"
      case InvalidTrainingFold(foldId, detail) =>
        s"soft-LDA fold '$foldId' is invalid: $detail"
      case PatternFailure(foldId, detail) =>
        s"soft-LDA pattern operator failed in fold '$foldId': $detail"
      case SemanticFailure(foldId, detail) =>
        s"soft-LDA operator adaptation failed in fold '$foldId': $detail"
      case LdaFailure(foldId, cause) =>
        s"soft-LDA fit failed in fold '$foldId': ${cause.message}"
      case NumericalFailure(foldId, detail) =>
        s"soft-LDA prediction failed in fold '$foldId': $detail"

final case class SoftLdaFoldReceipt(
    foldId: String,
    trainingSamples: Vector[SampleIndex],
    testSamples: Vector[SampleIndex],
    patternProvenance: PatternOperatorProvenance,
    composedPatternInput: Boolean,
    trialNuisanceColumns: Int,
    fit: LdaOperatorFit[?, ?, ?]
):
  require(foldId.trim.nonEmpty, "soft-LDA fold id must be non-empty")
  require(trainingSamples.nonEmpty, "soft-LDA receipt requires training samples")
  require(testSamples.nonEmpty, "soft-LDA receipt requires test samples")
  require(trialNuisanceColumns >= 0, "trial nuisance column count must be non-negative")

final case class SoftLdaCrossValidationReceipt(
    targetKind: ClassMembershipKind,
    executionMode: SoftLdaExecutionMode,
    objective: LdaObjective,
    folds: Vector[SoftLdaFoldReceipt]
):
  require(folds.nonEmpty, "soft-LDA cross-validation receipt requires folds")

final case class SoftLdaCrossValidatedResult(
    prediction: ClassificationPrediction,
    targetMse: Double,
    targetArgmaxAccuracy: Double,
    receipt: SoftLdaCrossValidationReceipt
):
  require(targetMse.isFinite && targetMse >= 0.0, "soft-LDA target MSE must be finite and non-negative")
  require(
    targetArgmaxAccuracy.isFinite && targetArgmaxAccuracy >= 0.0 && targetArgmaxAccuracy <= 1.0,
    "soft-LDA target argmax accuracy must be finite and in [0, 1]"
  )

/** Fold-local soft LDA over a sample-by-feature linear operator.
  *
  * The trial table is adapted directly to `multivar.OpTable`; class and
  * nuisance row relations are then pulled back with `secondOrder`. No
  * trial-by-feature matrix is requested by this path.
  */
object SoftLda:
  private val MassTolerance = 1e-12

  def crossValidate(
      data: PatternOperator,
      targets: ClassMembership,
      folds: FoldPlan,
      config: SoftLdaConfig
  ): Either[SoftLdaError, SoftLdaCrossValidatedResult] =
    if targets.samples != data.samples then Left(SoftLdaError.TargetLengthMismatch(data.samples, targets.samples))
    else if folds.samples != data.samples then Left(SoftLdaError.FoldSampleMismatch(data.samples, folds.samples))
    else if config.trialNuisance.exists(_.samples != data.samples) then
      Left(SoftLdaError.TrialNuisanceLengthMismatch(data.samples, config.trialNuisance.fold(0)(_.samples)))
    else
      val testRows = folds.folds.flatMap(_.test.map(_.value)).distinct.sorted
      val rowToOutput = testRows.zipWithIndex.toMap
      val probabilitySums = Matrix.newBuilder(testRows.length, targets.classCount)
      val predictionCounts = Array.fill(testRows.length)(0)
      val receipts = Vector.newBuilder[SoftLdaFoldReceipt]

      def processFold(fold: Fold, ordinal: Int): Either[SoftLdaError, Unit] =
        val trainPositions = fold.train.map(_.value)
        val testPositions = fold.test.map(_.value)
        val trainMembership = selectRows(targets.values, trainPositions)
        for
          _ <- validateMasses(trainMembership, targets.classes, fold.id)
          train <- data.selectRows(fold.train).left.map(error => SoftLdaError.PatternFailure(fold.id, error.message))
          test <- data.selectRows(fold.test).left.map(error => SoftLdaError.PatternFailure(fold.id, error.message))
          incidence <- ClassIncidence.fromSimplex(trainMembership).left.map(SoftLdaError.LdaFailure(fold.id, _))
          nuisance <- selectNuisance(config.trialNuisance, trainPositions, fold.id)
          rows <- SpaceRef
            .of(s"soft-lda-fold-$ordinal-train", SpaceRole.Samples, train.samples)
            .left
            .map(SoftLdaError.LdaFailure(fold.id, _))
          features <- SpaceRef
            .of(s"soft-lda-fold-$ordinal-features", SpaceRole.Observed, train.features)
            .left
            .map(SoftLdaError.LdaFailure(fold.id, _))
          trainTable <- adapt(train, rows.evidence, features.evidence, fold.id, "train")
          problem <- LdaProblem
            .fromTable(
              rows.evidence,
              features.evidence,
              trainTable,
              incidence,
              config.withinPolicy,
              nuisance,
              SemanticProvenance.source("mvpa-soft-lda")
            )
            .left
            .map(SoftLdaError.LdaFailure(fold.id, _))
          componentCount <- components(config.components, problem.maximumComponents, fold.id)
          fit <- problem.fit(componentCount, config.objective).left.map(SoftLdaError.LdaFailure(fold.id, _))
          testRowsSpace <- SpaceRef
            .of(s"soft-lda-fold-$ordinal-test", SpaceRole.Samples, test.samples)
            .left
            .map(SoftLdaError.LdaFailure(fold.id, _))
          testTable <- adapt(test, testRowsSpace.evidence, features.evidence, fold.id, "test")
          trainScores <- fit.scores(problem.table).toDense.left.map(error => SoftLdaError.SemanticFailure(fold.id, error.message))
          testScores <- fit.scores(testTable).toDense.left.map(error => SoftLdaError.SemanticFailure(fold.id, error.message))
          probabilities <- classify(trainScores, testScores, trainMembership, fit, nuisance.fold(0)(_.columns), fold.id)
        yield
          var localRow = 0
          while localRow < testPositions.length do
            val outputRow = rowToOutput(testPositions(localRow))
            var klass = 0
            while klass < targets.classCount do
              probabilitySums(outputRow, klass) = probabilitySums(outputRow, klass) + probabilities(localRow, klass)
              klass += 1
            predictionCounts(outputRow) += 1
            localRow += 1
          receipts += SoftLdaFoldReceipt(
            fold.id,
            train.sampleIndices,
            test.sampleIndices,
            train.provenance,
            composedPatternInput = train.provenance.origin == PatternOperatorOrigin.Composed,
            nuisance.fold(0)(_.columns),
            fit
          )

      var foldIndex = 0
      var failure = Option.empty[SoftLdaError]
      while foldIndex < folds.folds.length && failure.isEmpty do
        processFold(folds.folds(foldIndex), foldIndex) match
          case Left(error) => failure = Some(error)
          case Right(())   =>
        foldIndex += 1

      failure match
        case Some(error) => Left(error)
        case None =>
          val missing = predictionCounts.indexWhere(_ == 0)
          if missing >= 0 then Left(SoftLdaError.MissingPrediction(SampleIndex(testRows(missing))))
          else
            var row = 0
            while row < testRows.length do
              var klass = 0
              while klass < targets.classCount do
                probabilitySums(row, klass) = probabilitySums(row, klass) / predictionCounts(row)
                klass += 1
              row += 1
            val probabilities = probabilitySums.result()
            val prediction = ClassificationPrediction(
              targets.classes,
              probabilities,
              testRows.map(position => data.sampleIndices(position)).toVector
            )
            val (mse, accuracy) = metrics(prediction, targets, testRows)
            Right(
              SoftLdaCrossValidatedResult(
                prediction,
                mse,
                accuracy,
                SoftLdaCrossValidationReceipt(
                  targets.kind,
                  SoftLdaExecutionMode.PulledBackOperatorProducts,
                  config.objective,
                  receipts.result()
                )
              )
            )

  private def adapt[Rows <: SemanticSpace, Feature <: SemanticSpace](
      patterns: PatternOperator,
      rows: SpaceEvidence[Rows],
      features: SpaceEvidence[Feature],
      foldId: String,
      partition: String
  ): Either[SoftLdaError, OpTable[Rows, Feature, UncheckedEvidence]] =
    Op.fromLinearMap(
      patterns.linear,
      CoordinateEvidence.dual(features),
      CoordinateEvidence.primal(rows),
      OperatorRoleWitness.table,
      ValueIdentity.source(ValueId.unsafe(s"soft-lda-$partition-table")),
      SemanticProvenance.source("mvpa-pattern-operator")
    ).left.map(error => SoftLdaError.SemanticFailure(foldId, error.message))

  private def components(
      requested: SoftLdaComponents,
      maximum: Int,
      foldId: String
  ): Either[SoftLdaError, ComponentCount] =
    requested match
      case SoftLdaComponents.Maximum => ComponentCount(maximum).left.map(SoftLdaError.LdaFailure(foldId, _))
      case SoftLdaComponents.Fixed(count) if count.value <= maximum => Right(count)
      case SoftLdaComponents.Fixed(count) =>
        Left(SoftLdaError.LdaFailure(foldId, MultivarError.InvalidComponentRequest(count.value, maximum)))

  private def selectNuisance(
      nuisance: Option[TrialNuisanceDesign],
      positions: IndexedSeq[Int],
      foldId: String
  ): Either[SoftLdaError, Option[TrialNuisanceDesign]] =
    nuisance match
      case None => Right(None)
      case Some(design) =>
        TrialNuisanceDesign
          .from(selectRows(design.values, positions))
          .left
          .map(SoftLdaError.LdaFailure(foldId, _))
          .map(Some.apply)

  private def classify(
      trainScores: DMat,
      testScores: DMat,
      membership: DMat,
      fit: LdaOperatorFit[?, ?, ?],
      nuisanceColumns: Int,
      foldId: String
  ): Either[SoftLdaError, DMat] =
    val masses = new Array[Double](membership.cols)
    val centroids = Matrix.newBuilder(membership.cols, trainScores.cols)
    var sample = 0
    while sample < membership.rows do
      var klass = 0
      while klass < membership.cols do
        val weight = membership(sample, klass)
        masses(klass) += weight
        var component = 0
        while component < trainScores.cols do
          centroids(klass, component) = centroids(klass, component) + weight * trainScores(sample, component)
          component += 1
        klass += 1
      sample += 1
    var klass = 0
    while klass < membership.cols do
      var component = 0
      while component < trainScores.cols do
        centroids(klass, component) = centroids(klass, component) / masses(klass)
        component += 1
      klass += 1
    val classMeans = centroids.result()

    for
      within <- fit.realizedWithin.toDense.left.map(error => SoftLdaError.SemanticFailure(foldId, error.message))
      weights <- fit.functionalFrame.weights.toDense.left.map(error => SoftLdaError.SemanticFailure(foldId, error.message))
      projected = weights.t * within * weights
      degreesOfFreedom = math.max(1, trainScores.rows - membership.cols - nuisanceColumns)
      covariance = scale(projected, 1.0 / degreesOfFreedom)
      factor <- covariance
        .cholesky(CholeskyOptions(1e-12))
        .left
        .map(error => SoftLdaError.NumericalFailure(foldId, error.getMessage))
      coefficients <- factor
        .solve(classMeans.t)
        .left
        .map(error => SoftLdaError.NumericalFailure(foldId, error.getMessage))
    yield
      val raw = testScores * coefficients
      val shifted = Matrix.newBuilder(raw.rows, raw.cols)
      var row = 0
      while row < raw.rows do
        klass = 0
        while klass < raw.cols do
          var quadratic = 0.0
          var component = 0
          while component < classMeans.cols do
            quadratic += classMeans(klass, component) * coefficients(component, klass)
            component += 1
          val prior = masses(klass) / membership.rows
          shifted(row, klass) = raw(row, klass) - 0.5 * quadratic + Math.log(math.max(prior, 1e-300))
          klass += 1
        row += 1
      Classification.softmax(shifted.result())

  private def validateMasses(
      membership: DMat,
      classes: Vector[ClassLabel],
      foldId: String
  ): Either[SoftLdaError, Unit] =
    var klass = 0
    while klass < membership.cols do
      var mass = 0.0
      var row = 0
      while row < membership.rows do
        mass += membership(row, klass)
        row += 1
      if mass <= MassTolerance then
        return Left(SoftLdaError.InvalidTrainingFold(foldId, s"class ${classes(klass).value} has no training mass"))
      klass += 1
    Right(())

  private def selectRows(values: DMat, positions: IndexedSeq[Int]): DMat =
    val out = Matrix.newBuilder(positions.length, values.cols)
    var row = 0
    while row < positions.length do
      var col = 0
      while col < values.cols do
        out(row, col) = values(positions(row), col)
        col += 1
      row += 1
    out.result()

  private def metrics(
      prediction: ClassificationPrediction,
      targets: ClassMembership,
      positions: Vector[Int]
  ): (Double, Double) =
    val predicted = prediction.predicted
    val expected = targets.argmaxLabels
    var squaredError = 0.0
    var correct = 0
    var row = 0
    while row < positions.length do
      val targetRow = positions(row)
      var klass = 0
      while klass < targets.classCount do
        val difference = prediction.probabilities(row, klass) - targets.values(targetRow, klass)
        squaredError += difference * difference
        klass += 1
      if predicted(row) == expected(targetRow) then correct += 1
      row += 1
    (squaredError / (positions.length * targets.classCount), correct.toDouble / positions.length)

  private def scale(value: DMat, factor: Double): DMat =
    val out = Matrix.newBuilder(value.rows, value.cols)
    var row = 0
    while row < value.rows do
      var col = 0
      while col < value.cols do
        out(row, col) = factor * value(row, col)
        col += 1
      row += 1
    out.result()

final case class CrossValidatedSoftLdaAnalysis(
    config: SoftLdaConfig,
    storePredictions: Boolean = false
) extends FoldRequiredOperatorRoiAnalysis:
  override val name: String = "cv_soft_lda"
  override val minFeatures: Int = 1
  override val missingFoldsError: MvpaError = MvpaError.MissingFoldPlan(name)

  override def evaluateFolded(
      roi: PatternOperator,
      context: FoldedRoiContext
  ): Either[MvpaError, RoiAnalysisResult] =
    for
      targets <- ClassMembership.fromResponse(context.response)
      evaluated <- SoftLda
        .crossValidate(roi, targets, context.foldPlan, config)
        .left
        .map(error => MvpaError.AnalysisFailed(context.featureSet.id, error.message))
    yield
      RoiAnalysisResult(
        MetricVector(
          "TargetMse" -> evaluated.targetMse,
          "TargetArgmaxAccuracy" -> evaluated.targetArgmaxAccuracy,
          "TestedSamples" -> evaluated.prediction.probabilities.rows.toDouble,
          "FoldCount" -> evaluated.receipt.folds.length.toDouble
        ),
        if storePredictions then Some(RoiPayload.Classification(evaluated.prediction)) else None
      )
