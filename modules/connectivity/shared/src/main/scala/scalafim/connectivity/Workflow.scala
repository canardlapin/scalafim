package scalafim.connectivity

enum NumericalWarning:
  case NearSingularMatrix(conditionEstimate: Double)
  case ClippedCorrelation(edgeIndex: Int, value: Double)
  case NonPositiveVariance(nodeIndex: Int)

  def message: String =
    this match
      case NearSingularMatrix(conditionEstimate) =>
        s"near-singular matrix, condition estimate ${ConnectivityText.double(conditionEstimate)}"
      case ClippedCorrelation(edgeIndex, value) =>
        s"correlation at edge $edgeIndex was clipped from ${ConnectivityText.double(value)}"
      case NonPositiveVariance(nodeIndex) =>
        s"node $nodeIndex has non-positive variance"

final case class ConvergenceReport(
    converged: Boolean,
    iterations: Int,
    tolerance: Double,
    objective: Option[Double] = None
):
  require(iterations >= 0, "iterations must be non-negative")
  require(tolerance.isFinite && tolerance >= 0.0, "tolerance must be finite and non-negative")
  require(objective.forall(_.isFinite), "objective must be finite when present")

  def description: String =
    val objectivePart = objective.map(value => s":objective=${ConnectivityText.double(value)}").getOrElse("")
    s"converged=$converged:iterations=$iterations:tolerance=${ConnectivityText.double(tolerance)}$objectivePart"

final case class ConnectivityDiagnostics(
    warnings: Vector[NumericalWarning],
    convergence: Option[ConvergenceReport],
    metrics: Map[String, Double]
):
  require(metrics.values.forall(_.isFinite), "diagnostic metrics must be finite")

  def hasWarnings: Boolean =
    warnings.nonEmpty

  def description: String =
    val metricPart = metrics.toVector.sortBy(_._1).map((key, value) => s"$key=${ConnectivityText.double(value)}").mkString("[", ",", "]")
    val warningPart = warnings.map(_.message).mkString("[", "|", "]")
    val convPart = convergence.map(_.description).getOrElse("none")
    s"metrics=$metricPart;warnings=$warningPart;convergence=$convPart"

object ConnectivityDiagnostics:
  val empty: ConnectivityDiagnostics =
    ConnectivityDiagnostics(Vector.empty, None, Map.empty)

final case class PreprocessReceipt(
    plan: PreprocessPlan,
    inputSamples: Int,
    outputSamples: Int,
    notes: Vector[String] = Vector.empty
):
  require(inputSamples > 0, "input samples must be positive")
  require(outputSamples > 0, "output samples must be positive")

  def description: String =
    s"preprocess:${plan.description}:samples=${inputSamples}->${outputSamples}:notes=${notes.sorted.mkString("[", ",", "]")}"

final case class EstimatorReceipt(
    estimator: EstimatorSpec,
    output: EstimatorOutput,
    edgeSpaceDescription: String,
    diagnostics: ConnectivityDiagnostics
):
  require(estimator.output == output, "estimator receipt output must match estimator declaration")

  def description: String =
    s"estimator:${estimator.description}:edge-space=$edgeSpaceDescription:${diagnostics.description}"

sealed trait PostprocessStep:
  def description: String
  def transform(measure: ConnectivityMeasure): Either[ConnectivityError, ConnectivityMeasure]

object PostprocessStep:
  case object FisherZ extends PostprocessStep:
    def description: String =
      "fisher-z"

    def transform(measure: ConnectivityMeasure): Either[ConnectivityError, ConnectivityMeasure] =
      measure.scale match
        case ConnectivityValueScale.CorrelationR =>
          measure.withScale("fisher-z", ConnectivityValueScale.FisherZ)
        case other =>
          Left(ConnectivityError.InvalidWorkflow(s"fisher-z requires correlation-r input, got ${other.label}"))

  case object FisherR extends PostprocessStep:
    def description: String =
      "fisher-r"

    def transform(measure: ConnectivityMeasure): Either[ConnectivityError, ConnectivityMeasure] =
      measure.scale match
        case ConnectivityValueScale.FisherZ =>
          measure.withScale("fisher-r", ConnectivityValueScale.CorrelationR)
        case other =>
          Left(ConnectivityError.InvalidWorkflow(s"fisher-r requires fisher-z input, got ${other.label}"))

  case object ZeroDiagonal extends PostprocessStep:
    def description: String =
      "zero-diagonal"

    def transform(measure: ConnectivityMeasure): Either[ConnectivityError, ConnectivityMeasure] =
      Right(measure)

  private final case class ThresholdAbsoluteSpec(value: Double) extends PostprocessStep:
    require(value.isFinite && value >= 0.0, "absolute threshold must be finite and non-negative")

    def description: String =
      s"threshold-abs:${ConnectivityText.double(value)}"

    def transform(measure: ConnectivityMeasure): Either[ConnectivityError, ConnectivityMeasure] =
      Right(measure)

  def thresholdAbsolute(value: Double): Either[ConnectivityError, PostprocessStep] =
    if value.isFinite && value >= 0.0 then Right(ThresholdAbsoluteSpec(value))
    else Left(ConnectivityError.InvalidScalar("absolute threshold", value, "must be finite and non-negative"))

  def transformAll(
      input: ConnectivityMeasure,
      steps: Iterable[PostprocessStep]
  ): Either[ConnectivityError, ConnectivityMeasure] =
    steps.foldLeft[Either[ConnectivityError, ConnectivityMeasure]](Right(input)): (state, step) =>
      state.flatMap(step.transform)

final class StaticConnectivityWorkflow private (
    val preprocess: PreprocessPlan,
    val estimator: EstimatorSpec,
    val postprocess: Vector[PostprocessStep],
    val outputMeasure: ConnectivityMeasure
):
  def validateRun(run: RunTimeSeries): Either[ConnectivityError, RunTimeSeries] =
    WorkflowInputValidation.validate(preprocess, estimator, run).map(_ => run)

  def description: String =
    s"static-workflow:${preprocess.description}:${estimator.description}:output=${outputMeasure.description}:${postprocess.map(_.description).mkString("[", ",", "]")}"

object StaticConnectivityWorkflow:
  def from(
      preprocess: PreprocessPlan,
      estimator: EstimatorSpec,
      postprocess: Iterable[PostprocessStep] = Vector.empty
  ): Either[ConnectivityError, StaticConnectivityWorkflow] =
    if estimator.output.isDynamic then
      Left(ConnectivityError.InvalidWorkflow(s"static workflow cannot use dynamic estimator '${estimator.name}'"))
    else
      val steps = postprocess.toVector
      val unmet = estimator.unmetRequirements(preprocess)
      if unmet.nonEmpty then
        Left(ConnectivityError.InvalidWorkflow(s"static workflow estimator '${estimator.name}' has unmet requirements: ${unmet.map(_.label).mkString(",")}"))
      else
        PostprocessStep.transformAll(estimator.measure, steps).map: outputMeasure =>
          new StaticConnectivityWorkflow(preprocess, estimator, steps, outputMeasure)

final class DynamicConnectivityWorkflow private (
    val preprocess: PreprocessPlan,
    val estimator: EstimatorSpec,
    val window: WindowSpec,
    val postprocess: Vector[PostprocessStep],
    val outputMeasure: ConnectivityMeasure
):
  def windowLength: Int =
    window.length

  def windowStep: Int =
    window.step

  def windowAxis(timeAxis: TimeAxis): Either[ConnectivityError, WindowAxis] =
    WindowAxis.sliding(timeAxis, window)

  def validateRun(run: RunTimeSeries): Either[ConnectivityError, RunTimeSeries] =
    WorkflowInputValidation.validate(preprocess, estimator, run).flatMap: _ =>
      windowAxis(run.series.timeAxis).map(_ => run)

  def description: String =
    s"dynamic-workflow:${preprocess.description}:${estimator.description}:output=${outputMeasure.description}:${window.description}:${postprocess.map(_.description).mkString("[", ",", "]")}"

object DynamicConnectivityWorkflow:
  def from(
      preprocess: PreprocessPlan,
      estimator: EstimatorSpec,
      windowLength: Int,
      windowStep: Int,
      postprocess: Iterable[PostprocessStep] = Vector.empty
  ): Either[ConnectivityError, DynamicConnectivityWorkflow] =
    if !estimator.output.isDynamic then
      Left(ConnectivityError.InvalidWorkflow(s"dynamic workflow cannot use static estimator '${estimator.name}'"))
    else
      WindowSpec.from(windowLength, windowStep).flatMap: window =>
        val steps = postprocess.toVector
        val unmet = estimator.unmetRequirements(preprocess)
        if unmet.nonEmpty then
          Left(ConnectivityError.InvalidWorkflow(s"dynamic workflow estimator '${estimator.name}' has unmet requirements: ${unmet.map(_.label).mkString(",")}"))
        else
          PostprocessStep.transformAll(estimator.measure, steps).map: outputMeasure =>
            new DynamicConnectivityWorkflow(preprocess, estimator, window, steps, outputMeasure)

private[connectivity] object WorkflowInputValidation:
  def validate(
      preprocess: PreprocessPlan,
      estimator: EstimatorSpec,
      run: RunTimeSeries
  ): Either[ConnectivityError, Unit] =
    val unmet = estimator.unmetRequirements(preprocess)
    if unmet.nonEmpty then
      Left(ConnectivityError.InvalidWorkflow(s"estimator '${estimator.name}' has unmet requirements: ${unmet.map(_.label).mkString(",")}"))
    else if preprocess.frameWeightPolicy == InputPolicy.Required && run.frameWeights.isEmpty then
      Left(ConnectivityError.InvalidWorkflow(s"run '${run.runId.value}' is missing required frame weights"))
    else if preprocess.nuisancePolicy == InputPolicy.Required && run.nuisance.isEmpty then
      Left(ConnectivityError.InvalidWorkflow(s"run '${run.runId.value}' is missing required nuisance matrix"))
    else Right(())
