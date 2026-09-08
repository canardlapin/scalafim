package scalafim.fmri.mvpa

import gale.linalg.CholeskyOptions
import gale.linalg.DMat
import gale.linalg.Matrix
import multivar.core.SemanticSpace
import resample4s.core.Reindexing
import resample4s.core.UnitKey

opaque type FeatureModelPenalty = Double

object FeatureModelPenalty:
  def apply(value: Double): Either[FeatureModelError, FeatureModelPenalty] =
    if !value.isFinite || value <= 0.0 then Left(FeatureModelError.InvalidPenalty(value))
    else Right(value)

  private[mvpa] def unsafe(value: Double): FeatureModelPenalty =
    value

  extension (penalty: FeatureModelPenalty) inline def value: Double = penalty

enum FeatureModelDirection:
  case Encoding
  case Decoding

  def label: String =
    this match
      case Encoding => "model-features-to-neural-patterns"
      case Decoding => "neural-patterns-to-model-features"

enum FeatureModelSourceError:
  case Identity(error: ScientificIdentityError)
  case SampleAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case SampleWitnessMismatch
  case InvalidFeaturePurpose(actual: AxisPurpose)

  def message: String =
    this match
      case Identity(error)                      => error.message
      case SampleAxisMismatch(expected, actual) =>
        s"feature-model table samples ${actual.value} do not match observations ${expected.value}"
      case SampleWitnessMismatch =>
        "feature-model table and observations use different nominal sample witnesses"
      case InvalidFeaturePurpose(actual) =>
        s"feature-model coordinates require purpose '${AxisPurpose.Covariates.value}', obtained '${actual.value}'"

/** Neural observations and a model-feature table sharing one exact sample axis. Direction belongs to the estimand, not
  * to the source container.
  */
final class FeatureModelSource[
    S <: SemanticSpace,
    N <: SemanticSpace,
    F <: SemanticSpace,
    NK,
    FK
] private (
    val observations: Observations[S, N, NK],
    val features: EvidenceTable[S, F, SampleId, FK],
    val featureAxisName: ScientificAxisName,
    val identity: ScientificSourceIdentity
) extends ScientificSource:
  override type Neural = N
  override type NeuralKey = NK

  def samples: AxisRef.Aux[SampleId, S] = observations.samples
  def featureAxis: AxisRef.Aux[FK, F] = features.columns
  def sampleAxisName: ScientificAxisName = observations.sampleAxisName
  def neuralAxisName: ScientificAxisName = observations.neuralAxisName

  override def neuralAxis: AxisRef.Aux[NK, N] = observations.neuralAxis

  def encode(
      penalty: FeatureModelPenalty
  ): FeatureModelEstimand[S, N, F, NK, FK] =
    FeatureModel.estimand(this, FeatureModelDirection.Encoding, penalty)

  def decode(
      penalty: FeatureModelPenalty
  ): FeatureModelEstimand[S, N, F, NK, FK] =
    FeatureModel.estimand(this, FeatureModelDirection.Decoding, penalty)

object FeatureModelSource:
  private val Protocol = "scalafim-feature-model-source/v1"

  def apply[
      S <: SemanticSpace,
      N <: SemanticSpace,
      F <: SemanticSpace,
      NK,
      FK
  ](
      observations: Observations[S, N, NK],
      features: EvidenceTable[S, F, SampleId, FK],
      featureAxisName: ScientificAxisName = ScientificAxisName.unsafe("model-features")
  ): Either[
    FeatureModelSourceError,
    FeatureModelSource[S, N, F, NK, FK]
  ] =
    if features.rows.identity != observations.samples.identity then
      Left(
        FeatureModelSourceError.SampleAxisMismatch(
          observations.samples.identity.fingerprint,
          features.rows.identity.fingerprint
        )
      )
    else if !(features.rows.evidence eq observations.samples.evidence) then
      Left(FeatureModelSourceError.SampleWitnessMismatch)
    else if features.columns.identity.purpose != AxisPurpose.Covariates then
      Left(
        FeatureModelSourceError.InvalidFeaturePurpose(
          features.columns.identity.purpose
        )
      )
    else
      ScientificSourceIdentity(
        ScientificSourceKind.unsafe("feature-model-observations"),
        Vector(
          ScientificSourceAxis(
            observations.sampleAxisName,
            observations.samples.identity
          ),
          ScientificSourceAxis(
            observations.neuralAxisName,
            observations.neuralAxis.identity
          ),
          ScientificSourceAxis(featureAxisName, features.columns.identity)
        ),
        Vector(
          "features-value" -> features.table.valueIdentity.stableKey,
          "observations" -> observations.identity.fingerprint.value,
          "protocol" -> Protocol
        )
      ).left
        .map(FeatureModelSourceError.Identity.apply)
        .map: identity =>
          new FeatureModelSource(
            observations,
            features,
            featureAxisName,
            identity
          )

enum FeatureModelBindRejection:
  case SampleAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case SampleWitnessMismatch
  case InvalidSchedule(error: BoundScheduleError)
  case InsufficientAnalysisRows(unit: UnitKey, actual: Int)
  case EmptyAssessment(unit: UnitKey)

  def message: String =
    this match
      case SampleAxisMismatch(expected, actual) =>
        s"feature-model design samples ${actual.value} do not match source samples ${expected.value}"
      case SampleWitnessMismatch =>
        "feature-model design and source use different nominal sample witnesses"
      case InvalidSchedule(error)                 => error.message
      case InsufficientAnalysisRows(unit, actual) =>
        s"feature-model unit $unit requires at least two analysis rows, obtained $actual"
      case EmptyAssessment(unit) =>
        s"feature-model unit $unit has no assessment rows"

enum FeatureModelError:
  case Evidence(error: EvidenceTableError)
  case Axis(error: AxisRefError)
  case Design(error: PredictiveDesignError)
  case Schedule(error: BoundScheduleError)
  case ExecutionEvidence(error: ExecutionReceiptError)
  case InvalidPenalty(value: Double)
  case ShapeMismatch(
      label: String,
      expectedRows: Int,
      expectedColumns: Int,
      actualRows: Int,
      actualColumns: Int
  )
  case EmptyRows(label: String)
  case InsufficientTrainingRows(actual: Int)
  case NonFiniteValue(label: String, row: Int, column: Int, value: Double)
  case RidgeSolve(detail: String)
  case MissingPrediction(sample: SampleId)

  def message: String =
    this match
      case Evidence(error)          => error.message
      case Axis(error)              => error.message
      case Design(error)            => error.message
      case Schedule(error)          => error.message
      case ExecutionEvidence(error) => error.message
      case InvalidPenalty(value)    =>
        s"feature-model ridge penalty must be positive and finite, obtained $value"
      case ShapeMismatch(label, expectedRows, expectedColumns, actualRows, actualColumns) =>
        s"$label expected ${expectedRows}x$expectedColumns, obtained ${actualRows}x$actualColumns"
      case EmptyRows(label)                 => s"$label contains no rows"
      case InsufficientTrainingRows(actual) =>
        s"feature-model ridge requires at least two training rows, obtained $actual"
      case NonFiniteValue(label, row, column, value) =>
        s"$label contains non-finite value $value at ($row,$column)"
      case RidgeSolve(detail)        => s"feature-model ridge solve failed: $detail"
      case MissingPrediction(sample) =>
        s"feature-model exact-once design produced no prediction for '${sample.value}'"

final class FeatureModelEstimand[
    S <: SemanticSpace,
    N <: SemanticSpace,
    F <: SemanticSpace,
    NK,
    FK
] private[mvpa] (
    val direction: FeatureModelDirection,
    val penalty: FeatureModelPenalty,
    val identity: EstimandIdentity
) extends Estimand[
      FeatureModelSource[S, N, F, NK, FK],
      CrossFitDesign[S, ?]
    ]:
  override type Result = MeasuredFeatureModel[S]
  override type Rejection = FeatureModelBindRejection
  override type Failure = FeatureModelError

  override val defaultBoundaries: RequestedBoundaries = FeatureModel.Boundaries

  override def rejectionMessage(value: FeatureModelBindRejection): String =
    value.message

  override def failureMessage(value: FeatureModelError): String = value.message

final case class FeatureModelFoldReceipt(
    unit: UnitKey,
    analysisSamples: AxisIdentity,
    assessmentSamples: AxisIdentity
)

final class FeatureModelComputationReceipt private[mvpa] (
    val folds: Vector[FeatureModelFoldReceipt]
)

final case class FeatureModelMetrics(
    patternCorrelation: Option[Double],
    patternDiscrimination: Option[Double],
    patternRankPercentile: Option[Double],
    rdmCorrelation: Option[Double],
    targetCorrelation: Option[Double],
    meanSquaredError: Double,
    rSquared: Option[Double],
    meanTargetwiseCorrelation: Option[Double]
)

/** Exact-once sample-keyed predictions. Target identity is retained as the measured neural axis for encoding or the
  * declared model-feature axis for decoding; no generated string labels stand in for coordinates.
  */
final class MeasuredFeatureModel[S <: SemanticSpace] private[mvpa] (
    val samples: AxisRef.Aux[SampleId, S],
    val target: AxisIdentity,
    val predicted: DMat,
    val observed: DMat,
    val metrics: FeatureModelMetrics,
    val computation: FeatureModelComputationReceipt
)

object FeatureModel:
  private val Kind = EstimandKind.unsafe("cross-fitted-feature-model")

  val RidgeSolver: SolverIdentity =
    SolverIdentity.trusted(
      SolverId.unsafe("feature-model-standardized-ridge-cholesky"),
      Vector(
        "implementation" -> "scalafim-feature-model",
        "linear-kernel" -> "gale-cholesky"
      )
    )

  private[mvpa] val Boundaries =
    RequestedBoundaries.trusted(
      Vector(
        OutputBoundaryIdentity.trusted(
          OutputBoundaryId.unsafe("out-of-fold-feature-predictions")
        ),
        OutputBoundaryIdentity.trusted(
          OutputBoundaryId.unsafe("feature-model-summaries")
        )
      )
    )

  private[mvpa] def estimand[
      S <: SemanticSpace,
      N <: SemanticSpace,
      F <: SemanticSpace,
      NK,
      FK
  ](
      source: FeatureModelSource[S, N, F, NK, FK],
      direction: FeatureModelDirection,
      penalty: FeatureModelPenalty
  ): FeatureModelEstimand[S, N, F, NK, FK] =
    val identity = EstimandIdentity.trusted(
      Kind,
      Vector(
        "direction" -> direction.label,
        "fit" -> "fold-local-standardized-multivariate-ridge",
        "penalty" -> java.lang.Double.toHexString(penalty.value),
        "source" -> source.identity.fingerprint.value
      )
    )
    new FeatureModelEstimand(direction, penalty, identity)

  given compiler[
      S <: SemanticSpace,
      N <: SemanticSpace,
      F <: SemanticSpace,
      NK,
      FK,
      FoldUnit,
      R
  ]: Compile[
    FeatureModelSource[S, N, F, NK, FK],
    CrossFitDesign[S, FoldUnit],
    FeatureModelEstimand[S, N, F, NK, FK],
    R
  ] with
    override type Prepared = Unit

    override def prepare(
        specification: ScientificSpecification[
          FeatureModelSource[S, N, F, NK, FK],
          CrossFitDesign[S, FoldUnit],
          FeatureModelEstimand[S, N, F, NK, FK],
          R
        ]
    ): Either[specification.Rejection, Unit] =
      val source = specification.source
      val design = specification.design
      if source.samples.identity != design.validation.schedule.axis.identity then
        Left(
          FeatureModelBindRejection.SampleAxisMismatch(
            source.samples.identity.fingerprint,
            design.validation.schedule.axis.identity.fingerprint
          )
        )
      else if !(source.samples.evidence eq design.validation.schedule.axis.evidence) then
        Left(FeatureModelBindRejection.SampleWitnessMismatch)
      else
        val iterator = design.validation.schedule.iterator
        var rejection: Option[FeatureModelBindRejection] = None
        while iterator.hasNext && rejection.isEmpty do
          val (key, bound) = iterator.next()
          bound match
            case Left(error) =>
              rejection = Some(FeatureModelBindRejection.InvalidSchedule(error))
            case Right(unit) =>
              val analysis = design.validation.roles.analysis(unit)
              val assessment = design.validation.roles.assessment(unit)
              if analysis.size < 2 then
                rejection = Some(
                  FeatureModelBindRejection.InsufficientAnalysisRows(
                    key,
                    analysis.size
                  )
                )
              else if assessment.size == 0 then rejection = Some(FeatureModelBindRejection.EmptyAssessment(key))
        rejection match
          case Some(value) => Left(value)
          case None        => Right(())

  given task[
      S <: SemanticSpace,
      N <: SemanticSpace,
      F <: SemanticSpace,
      NK,
      FK,
      FoldUnit,
      R
  ]: MeasurementTask[
    FeatureModelSource[S, N, F, NK, FK],
    CrossFitDesign[S, FoldUnit],
    FeatureModelEstimand[S, N, F, NK, FK],
    R,
    Unit
  ] with
    override def validate(
        strategy: ExecutionStrategy
    ): Either[ExecutionPlanError, Unit] =
      if strategy.representation != ExecutionRepresentation.Dense then
        Left(
          ExecutionPlanError.UnsupportedRepresentation(
            strategy.representation,
            Vector(ExecutionRepresentation.Dense)
          )
        )
      else if strategy.precision != NumericPrecision.Binary64 then
        Left(
          ExecutionPlanError.UnsupportedPrecision(
            strategy.precision,
            Vector(NumericPrecision.Binary64)
          )
        )
      else if strategy.solver != SolverChoice.Selected(RidgeSolver) then
        Left(ExecutionPlanError.UnsupportedSolver(strategy.solver))
      else
        strategy.materialization match
          case MaterializationPolicy.Reject =>
            Left(
              ExecutionPlanError.MaterializationRequired(
                "feature modelling consumes explicitly budgeted neural and model-feature tables"
              )
            )
          case MaterializationPolicy.Allow(_) => Right(())

    override def execute(
        plan: BoundScientificPlan[
          FeatureModelSource[S, N, F, NK, FK],
          CrossFitDesign[S, FoldUnit],
          FeatureModelEstimand[S, N, F, NK, FK],
          R,
          Unit
        ]
    )(
        entry: MeasurementEntry[N, NK, ?, R],
        context: TaskContext
    ): Either[
      TaskReportError,
      TaskReport[plan.Result, plan.Rejection, plan.Failure]
    ] =
      evaluate(
        plan.specification.source,
        plan.specification.design,
        plan.specification.estimand,
        entry.measurement,
        context.strategy.materialization
      ) match
        case Left(error)      => TaskReport.failed(error, context.strategy.target)
        case Right(execution) =>
          val scope = ExecutionScope.Measurement(entry.measurement.identity.id)
          val neural = ExecutionMaterialization(
            scope,
            execution.neuralMaterialization,
            "feature modelling requires the declared local neural table"
          )
          val features = ExecutionMaterialization(
            scope,
            execution.featureMaterialization,
            "feature modelling requires the declared model-feature table"
          )
          (neural, features) match
            case (Right(neuralReceipt), Right(featureReceipt)) =>
              TaskReport.success(
                execution.result,
                context.strategy.target,
                operatorApplications = execution.operatorApplications,
                materializations = Vector(neuralReceipt, featureReceipt)
              )
            case (Left(error), _) =>
              TaskReport.failed(
                FeatureModelError.ExecutionEvidence(error),
                context.strategy.target,
                operatorApplications = execution.operatorApplications
              )
            case (_, Left(error)) =>
              TaskReport.failed(
                FeatureModelError.ExecutionEvidence(error),
                context.strategy.target,
                operatorApplications = execution.operatorApplications
              )

  private final case class FeatureModelExecution[S <: SemanticSpace](
      result: MeasuredFeatureModel[S],
      neuralMaterialization: MaterializationReceipt,
      featureMaterialization: MaterializationReceipt,
      operatorApplications: Long
  )

  private def evaluate[
      S <: SemanticSpace,
      N <: SemanticSpace,
      F <: SemanticSpace,
      NK,
      FK,
      FoldUnit,
      L
  ](
      source: FeatureModelSource[S, N, F, NK, FK],
      design: CrossFitDesign[S, FoldUnit],
      estimand: FeatureModelEstimand[S, N, F, NK, FK],
      measurement: Measurement[N, NK, L],
      policy: MaterializationPolicy
  ): Either[FeatureModelError, FeatureModelExecution[S]] =
    for
      measured <- source.observations.evidence
        .measureColumns(measurement)
        .left
        .map(FeatureModelError.Evidence.apply)
      neural <- measured
        .materialize(policy)
        .left
        .map(FeatureModelError.Evidence.apply)
      features <- source.features
        .materialize(policy)
        .left
        .map(FeatureModelError.Evidence.apply)
      _ <- design.outOfFold.left.map(FeatureModelError.Design.apply)
      prediction <- crossValidate(
        source,
        design,
        estimand,
        neural.value,
        features.value,
        measurement.local.identity
      )
      metrics = FeatureModelMetricKernel.compute(
        prediction.predicted,
        prediction.observed
      )
    yield FeatureModelExecution(
      new MeasuredFeatureModel(
        source.samples,
        prediction.target,
        prediction.predicted,
        prediction.observed,
        metrics,
        new FeatureModelComputationReceipt(prediction.folds)
      ),
      neural.receipt,
      features.receipt,
      measurement.local.size.toLong + source.featureAxis.size.toLong
    )

  private final case class CrossValidatedPrediction(
      target: AxisIdentity,
      predicted: DMat,
      observed: DMat,
      folds: Vector[FeatureModelFoldReceipt]
  )

  private def crossValidate[
      S <: SemanticSpace,
      N <: SemanticSpace,
      F <: SemanticSpace,
      NK,
      FK,
      FoldUnit
  ](
      source: FeatureModelSource[S, N, F, NK, FK],
      design: CrossFitDesign[S, FoldUnit],
      estimand: FeatureModelEstimand[S, N, F, NK, FK],
      neural: DMat,
      features: DMat,
      measuredNeuralAxis: AxisIdentity
  ): Either[FeatureModelError, CrossValidatedPrediction] =
    val (input, target, targetIdentity) = estimand.direction match
      case FeatureModelDirection.Encoding =>
        (features, neural, measuredNeuralAxis)
      case FeatureModelDirection.Decoding =>
        (neural, features, source.featureAxis.identity)
    val predicted = Matrix.newBuilder(source.samples.size, target.cols)
    val observed = Matrix.newBuilder(source.samples.size, target.cols)
    val written = Array.fill(source.samples.size)(false)
    val foldReceipts = Vector.newBuilder[FeatureModelFoldReceipt]
    val iterator = design.validation.schedule.iterator
    while iterator.hasNext do
      val (key, bound) = iterator.next()
      val unit = bound.left.map(FeatureModelError.Schedule.apply) match
        case Left(error)  => return Left(error)
        case Right(value) => value
      val analysis = design.validation.roles.analysis(unit)
      val assessment = design.validation.roles.assessment(unit)
      val fit = for
        sourceTrain <- selectRows(input, analysis)
        targetTrain <- selectRows(target, analysis)
        sourceTest <- selectRows(input, assessment)
        fitted <- StandardizedRidgeMap.fit(
          sourceTrain,
          targetTrain,
          estimand.penalty
        )
        foldPrediction <- fitted.predict(sourceTest)
      yield foldPrediction
      fit match
        case Left(error)           => return Left(error)
        case Right(foldPrediction) =>
          var assessmentPosition = 0
          while assessmentPosition < assessment.size do
            val sourcePosition = assessment
              .sourcePositionAt(assessmentPosition)
              .left
              .map(FeatureModelError.Axis.apply) match
              case Left(error)  => return Left(error)
              case Right(value) => value
            var column = 0
            while column < target.cols do
              predicted(sourcePosition, column) = foldPrediction(assessmentPosition, column)
              observed(sourcePosition, column) = target(sourcePosition, column)
              column += 1
            written(sourcePosition) = true
            assessmentPosition += 1
      foldReceipts += FeatureModelFoldReceipt(
        key,
        analysis.child.identity,
        assessment.child.identity
      )

    var sourcePosition = 0
    while sourcePosition < written.length do
      if !written(sourcePosition) then
        return Left(
          FeatureModelError.MissingPrediction(source.samples.keys(sourcePosition))
        )
      sourcePosition += 1

    Right(
      CrossValidatedPrediction(
        targetIdentity,
        predicted.result(),
        observed.result(),
        foldReceipts.result()
      )
    )

  private def selectRows[
      S <: SemanticSpace,
      K,
      C,
      R <: Reindexing
  ](
      matrix: DMat,
      selection: ReindexingLeg[S, K, C, R]
  ): Either[FeatureModelError, DMat] =
    if selection.size == 0 then Left(FeatureModelError.EmptyRows("feature-model selection"))
    else
      val output = Matrix.newBuilder(selection.size, matrix.cols)
      var row = 0
      while row < selection.size do
        val sourcePosition = selection
          .sourcePositionAt(row)
          .left
          .map(FeatureModelError.Axis.apply) match
          case Left(error)  => return Left(error)
          case Right(value) => value
        var column = 0
        while column < matrix.cols do
          output(row, column) = matrix(sourcePosition, column)
          column += 1
        row += 1
      Right(output.result())

private final class StandardizedRidgeMap(
    sourceMeans: Array[Double],
    sourceScales: Array[Double],
    targetMeans: Array[Double],
    targetScales: Array[Double],
    coefficients: DMat
):
  def predict(source: DMat): Either[FeatureModelError, DMat] =
    if source.cols != sourceMeans.length then
      Left(
        FeatureModelError.ShapeMismatch(
          "feature-model prediction source",
          source.rows,
          sourceMeans.length,
          source.rows,
          source.cols
        )
      )
    else
      for
        _ <- FeatureModelMatrix.validateFinite(source, "feature-model prediction source")
        standardized = FeatureModelMatrix.standardize(
          source,
          sourceMeans,
          sourceScales
        )
        raw = standardized * coefficients
      yield
        val output = Matrix.newBuilder(raw.rows, raw.cols)
        var row = 0
        while row < raw.rows do
          var column = 0
          while column < raw.cols do
            output(row, column) = raw(row, column) * targetScales(column) + targetMeans(column)
            column += 1
          row += 1
        output.result()

private object StandardizedRidgeMap:
  def fit(
      source: DMat,
      target: DMat,
      penalty: FeatureModelPenalty
  ): Either[FeatureModelError, StandardizedRidgeMap] =
    if source.rows != target.rows then
      Left(
        FeatureModelError.ShapeMismatch(
          "feature-model source and target row agreement",
          source.rows,
          target.cols,
          target.rows,
          target.cols
        )
      )
    else if source.rows < 2 then Left(FeatureModelError.InsufficientTrainingRows(source.rows))
    else
      for
        _ <- FeatureModelMatrix.validateFinite(source, "feature-model source")
        _ <- FeatureModelMatrix.validateFinite(target, "feature-model target")
        sourceStats = FeatureModelMatrix.statistics(source)
        targetStats = FeatureModelMatrix.statistics(target)
        x = FeatureModelMatrix.standardize(
          source,
          sourceStats.means,
          sourceStats.scales
        )
        y = FeatureModelMatrix.standardize(
          target,
          targetStats.means,
          targetStats.scales
        )
        gram = x.t * x
        penalized =
          val builder = Matrix.newBuilder(gram.rows, gram.cols)
          var row = 0
          while row < gram.rows do
            var column = 0
            while column < gram.cols do
              builder(row, column) = gram(row, column) +
                (if row == column then penalty.value else 0.0)
              column += 1
            row += 1
          builder.result()
        coefficients <- penalized
          .cholesky(CholeskyOptions(1e-12))
          .left
          .map(error => FeatureModelError.RidgeSolve(error.getMessage))
          .flatMap(
            _.solve(x.t * y).left
              .map(error => FeatureModelError.RidgeSolve(error.getMessage))
          )
      yield new StandardizedRidgeMap(
        sourceStats.means,
        sourceStats.scales,
        targetStats.means,
        targetStats.scales,
        coefficients
      )

private final case class FeatureModelColumnStats(
    means: Array[Double],
    scales: Array[Double]
)

private object FeatureModelMatrix:
  private val ScaleTolerance = 1e-12

  def validateFinite(
      matrix: DMat,
      label: String
  ): Either[FeatureModelError, Unit] =
    var row = 0
    while row < matrix.rows do
      var column = 0
      while column < matrix.cols do
        val value = matrix(row, column)
        if !value.isFinite then
          return Left(
            FeatureModelError.NonFiniteValue(label, row, column, value)
          )
        column += 1
      row += 1
    Right(())

  def statistics(matrix: DMat): FeatureModelColumnStats =
    val means = new Array[Double](matrix.cols)
    val scales = new Array[Double](matrix.cols)
    var column = 0
    while column < matrix.cols do
      var sum = 0.0
      var row = 0
      while row < matrix.rows do
        sum += matrix(row, column)
        row += 1
      means(column) = sum / matrix.rows.toDouble
      var sumSquares = 0.0
      row = 0
      while row < matrix.rows do
        val centered = matrix(row, column) - means(column)
        sumSquares += centered * centered
        row += 1
      val scale = math.sqrt(sumSquares / (matrix.rows - 1).toDouble)
      scales(column) =
        if scale.isFinite && scale > ScaleTolerance then scale else 1.0
      column += 1
    FeatureModelColumnStats(means, scales)

  def standardize(
      matrix: DMat,
      means: Array[Double],
      scales: Array[Double]
  ): DMat =
    val output = Matrix.newBuilder(matrix.rows, matrix.cols)
    var row = 0
    while row < matrix.rows do
      var column = 0
      while column < matrix.cols do
        output(row, column) = (matrix(row, column) - means(column)) / scales(column)
        column += 1
      row += 1
    output.result()

private object FeatureModelMetricKernel:
  def compute(predicted: DMat, observed: DMat): FeatureModelMetrics =
    val patterns = patternMetrics(predicted, observed)
    FeatureModelMetrics(
      patterns._1,
      patterns._2,
      patterns._3,
      rdmCorrelation(predicted, observed),
      globalCorrelation(predicted, observed),
      meanSquaredError(predicted, observed),
      rSquared(predicted, observed),
      meanColumnCorrelation(predicted, observed)
    )

  private def patternMetrics(
      predicted: DMat,
      observed: DMat
  ): (Option[Double], Option[Double], Option[Double]) =
    if predicted.rows < 2 || predicted.cols < 2 then (None, None, None)
    else
      val correlations = new Array[Double](predicted.rows * predicted.rows)
      val defined = Array.fill(predicted.rows * predicted.rows)(false)
      var row = 0
      while row < predicted.rows do
        var column = 0
        while column < predicted.rows do
          val position = row * predicted.rows + column
          rowCorrelation(predicted, row, observed, column) match
            case Some(value) =>
              correlations(position) = value
              defined(position) = true
            case None => ()
          column += 1
        row += 1
      val diagonals = Vector.tabulate(predicted.rows): position =>
        val index = position * predicted.rows + position
        Option.when(defined(index))(correlations(index))
      val offDiagonal = Vector.newBuilder[Double]
      var position = 0
      while position < predicted.rows * predicted.rows do
        val row = position / predicted.rows
        val column = position % predicted.rows
        if row != column && defined(position) then offDiagonal += correlations(position)
        position += 1
      val patternCorrelation = mean(diagonals.flatten)
      val discrimination = for
        diagonal <- patternCorrelation
        off <- mean(offDiagonal.result())
      yield diagonal - off
      val ranks = Vector.tabulate(predicted.rows): row =>
        val diagonalPosition = row * predicted.rows + row
        val rowValues = Vector.tabulate(predicted.rows): column =>
          val index = row * predicted.rows + column
          Option.when(defined(index))(correlations(index))
        val available = rowValues.flatten
        if !defined(diagonalPosition) || available.length < 2 then None
        else
          val diagonal = correlations(diagonalPosition)
          Some(
            (available.count(_ <= diagonal) - 1).toDouble /
              (available.length - 1).toDouble
          )
      (patternCorrelation, discrimination, mean(ranks.flatten))

  private def rdmCorrelation(
      predicted: DMat,
      observed: DMat
  ): Option[Double] =
    if predicted.rows < 3 || predicted.cols < 2 then None
    else
      val predictedDistances = correlationDistances(predicted)
      val observedDistances = correlationDistances(observed)
      for
        left <- predictedDistances
        right <- observedDistances
        correlation <- vectorCorrelation(averageRanks(left), averageRanks(right))
      yield correlation

  private def correlationDistances(matrix: DMat): Option[Vector[Double]] =
    val output = Vector.newBuilder[Double]
    var first = 0
    while first < matrix.rows - 1 do
      var second = first + 1
      while second < matrix.rows do
        rowCorrelation(matrix, first, matrix, second) match
          case None        => return None
          case Some(value) => output += 1.0 - value
        second += 1
      first += 1
    Some(output.result())

  private def averageRanks(values: Vector[Double]): Vector[Double] =
    val sorted = values.zipWithIndex.sortBy(_._1)
    val output = new Array[Double](values.length)
    var start = 0
    while start < sorted.length do
      var end = start + 1
      while end < sorted.length && sorted(end)._1 == sorted(start)._1 do end += 1
      val rank = (start + 1 + end).toDouble / 2.0
      var position = start
      while position < end do
        output(sorted(position)._2) = rank
        position += 1
      start = end
    output.toVector

  private def globalCorrelation(
      predicted: DMat,
      observed: DMat
  ): Option[Double] =
    val left = Vector.tabulate(predicted.rows * predicted.cols): position =>
      predicted(position / predicted.cols, position % predicted.cols)
    val right = Vector.tabulate(observed.rows * observed.cols): position =>
      observed(position / observed.cols, position % observed.cols)
    vectorCorrelation(left, right)

  private def meanColumnCorrelation(
      predicted: DMat,
      observed: DMat
  ): Option[Double] =
    if predicted.rows < 2 then None
    else
      val correlations = Vector.tabulate(predicted.cols): column =>
        val left = Vector.tabulate(predicted.rows)(row => predicted(row, column))
        val right = Vector.tabulate(observed.rows)(row => observed(row, column))
        vectorCorrelation(left, right)
      mean(correlations.flatten)

  private def meanSquaredError(predicted: DMat, observed: DMat): Double =
    var sum = 0.0
    var position = 0
    while position < predicted.rows * predicted.cols do
      val difference =
        predicted(position / predicted.cols, position % predicted.cols) -
          observed(position / observed.cols, position % observed.cols)
      sum += difference * difference
      position += 1
    sum / (predicted.rows * predicted.cols).toDouble

  private def rSquared(predicted: DMat, observed: DMat): Option[Double] =
    var mean = 0.0
    var position = 0
    while position < observed.rows * observed.cols do
      mean += observed(position / observed.cols, position % observed.cols)
      position += 1
    mean /= (observed.rows * observed.cols).toDouble
    var residual = 0.0
    var total = 0.0
    position = 0
    while position < observed.rows * observed.cols do
      val actual = observed(position / observed.cols, position % observed.cols)
      val fitted = predicted(position / predicted.cols, position % predicted.cols)
      residual += (actual - fitted) * (actual - fitted)
      total += (actual - mean) * (actual - mean)
      position += 1
    if total == 0.0 then None else Some(1.0 - residual / total)

  private def rowCorrelation(
      left: DMat,
      leftRow: Int,
      right: DMat,
      rightRow: Int
  ): Option[Double] =
    val leftMean =
      Vector.tabulate(left.cols)(column => left(leftRow, column)).sum /
        left.cols.toDouble
    val rightMean =
      Vector.tabulate(right.cols)(column => right(rightRow, column)).sum /
        right.cols.toDouble
    var numerator = 0.0
    var leftSumSquares = 0.0
    var rightSumSquares = 0.0
    var column = 0
    while column < left.cols do
      val x = left(leftRow, column) - leftMean
      val y = right(rightRow, column) - rightMean
      numerator += x * y
      leftSumSquares += x * x
      rightSumSquares += y * y
      column += 1
    val denominator = math.sqrt(leftSumSquares * rightSumSquares)
    if denominator == 0.0 then None else Some(numerator / denominator)

  private def vectorCorrelation(
      left: Vector[Double],
      right: Vector[Double]
  ): Option[Double] =
    if left.length != right.length || left.length < 2 then None
    else
      val leftMean = left.sum / left.length.toDouble
      val rightMean = right.sum / right.length.toDouble
      var numerator = 0.0
      var leftSumSquares = 0.0
      var rightSumSquares = 0.0
      var position = 0
      while position < left.length do
        val x = left(position) - leftMean
        val y = right(position) - rightMean
        numerator += x * y
        leftSumSquares += x * x
        rightSumSquares += y * y
        position += 1
      val denominator = math.sqrt(leftSumSquares * rightSumSquares)
      if denominator == 0.0 then None else Some(numerator / denominator)

  private def mean(values: Vector[Double]): Option[Double] =
    if values.isEmpty then None else Some(values.sum / values.length.toDouble)
