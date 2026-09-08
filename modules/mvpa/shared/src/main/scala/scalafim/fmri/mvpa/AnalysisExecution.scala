package scalafim.fmri.mvpa

import scala.collection.mutable.ArrayBuffer

enum AnalysisExecutionError:
  case UnsupportedParallelism(value: Int)
  case StreamingDeliveryRequired(actual: ResultDelivery)
  case InvalidTaskReport(measurement: MeasurementId, error: TaskReportError)
  case InvalidMeasurementReceipt(measurement: MeasurementId, error: ExecutionReceiptError)
  case InvalidWork(error: ExecutionReceiptError)
  case InvalidCounts(error: TraversalCountError)
  case InvalidExecutionReceipt(error: ExecutionReceiptError)
  case InvalidAnalysisResult(error: AnalysisResultError)
  case IncompleteCollectedResult(counts: TraversalCounts)

  def message: String =
    this match
      case UnsupportedParallelism(value) =>
        s"portable analysis traversal currently requires serial scheduling, obtained parallelism $value"
      case StreamingDeliveryRequired(actual) =>
        s"visitor and fold execution require streaming delivery, obtained '${actual.label}'"
      case InvalidTaskReport(measurement, error) =>
        s"measurement '${measurement.value}' returned an invalid task report: ${error.message}"
      case InvalidMeasurementReceipt(measurement, error) =>
        s"measurement '${measurement.value}' returned invalid execution evidence: ${error.message}"
      case InvalidWork(error)                => error.message
      case InvalidCounts(error)              => error.message
      case InvalidExecutionReceipt(error)    => error.message
      case InvalidAnalysisResult(error)      => error.message
      case IncompleteCollectedResult(counts) =>
        s"collected result visited ${counts.visited} of ${counts.expected} measurements"

final class AnalysisExecution[
    Source <: ScientificSource,
    Design <: EvidenceDesign,
    E <: Estimand[Source, Design],
    Rendition,
    Prepared
] private (
    val plan: ExecutionPlan[Source, Design, E, Rendition, Prepared]
):
  type Result = plan.scientific.Result
  type Rejection = plan.scientific.Rejection
  type Failure = plan.scientific.Failure
  type Value = MeasurementValue[Result, Rejection, Failure, Rendition]

  def visit[State](
      initial: State
  )(
      step: (State, Value) => VisitDecision[State]
  ): Either[AnalysisExecutionError, TraversalResult[State]] =
    if plan.strategy.delivery != ResultDelivery.Streaming then
      Left(AnalysisExecutionError.StreamingDeliveryRequired(plan.strategy.delivery))
    else traverse(initial, allowStop = true)(step)

  def foldLeft[State](
      initial: State
  )(
      step: (State, Value) => State
  ): Either[AnalysisExecutionError, TraversalResult[State]] =
    visit(initial): (state, value) =>
      VisitDecision.Continue(step(state, value))

  def collect: Either[
    AnalysisExecutionError,
    AnalysisResult[Result, Rejection, Failure, Rendition]
  ] =
    val buffer = ArrayBuffer.empty[Value]
    traverse(buffer, allowStop = false): (values, value) =>
      values += value
      VisitDecision.Continue(values)
    .flatMap: traversal =>
      if traversal.counts.completion != TraversalCompletion.Exhausted then
        Left(AnalysisExecutionError.IncompleteCollectedResult(traversal.counts))
      else
        AnalysisResult
          .fromExecution(this)(
            traversal.state.toVector,
            traversal.counts,
            traversal.receipt
          )
          .left
          .map(AnalysisExecutionError.InvalidAnalysisResult.apply)

  private def traverse[State](
      initial: State,
      allowStop: Boolean
  )(
      step: (State, Value) => VisitDecision[State]
  ): Either[AnalysisExecutionError, TraversalResult[State]] =
    val entries = plan.scientific.specification.frame.entries
    val receipts = Vector.newBuilder[MeasurementExecutionReceipt]
    var state = initial
    var visited = 0
    var succeeded = 0
    var rejected = 0
    var failed = 0
    var operatorApplications = 0L
    var materializedCells = 0L
    var running = true

    while running && visited < entries.length do
      val entry = entries(visited)
      val measurementId = entry.measurement.identity.id
      val context = new TaskContext(plan.identity, visited, entries.length, plan.strategy)
      val report = plan.task
        .execute(plan.scientific)(entry, context)
        .left
        .map(error => AnalysisExecutionError.InvalidTaskReport(measurementId, error)) match
        case Left(error)  => return Left(error)
        case Right(value) => value
      val receipt = MeasurementExecutionReceipt
        .fromTask(
          plan,
          visited,
          report.outcome.status,
          report.actual,
          report.operatorApplications,
          report.materializations,
          report.fallbacks,
          report.convergence
        )
        .left
        .map(error => AnalysisExecutionError.InvalidMeasurementReceipt(measurementId, error)) match
        case Left(error)  => return Left(error)
        case Right(value) => value
      val outcome: MeasurementOutcome[Result, Rejection, Failure] =
        report.outcome match
          case TaskOutcome.Success(value)  => MeasurementOutcome.Success(value, receipt)
          case TaskOutcome.Rejected(value) => MeasurementOutcome.Rejected(value, receipt)
          case TaskOutcome.Failed(value)   => MeasurementOutcome.Failed(value, receipt)
      val value = MeasurementValue(
        entry.measurement.identity,
        entry.rendition,
        outcome
      )

      receipts += receipt
      operatorApplications += receipt.operatorApplications
      materializedCells += receipt.materializedCells
      receipt.status match
        case MeasurementStatus.Succeeded => succeeded += 1
        case MeasurementStatus.Rejected  => rejected += 1
        case MeasurementStatus.Failed    => failed += 1
      visited += 1

      step(state, value) match
        case VisitDecision.Continue(next) => state = next
        case VisitDecision.Stop(next)     =>
          state = next
          if allowStop && visited < entries.length then running = false

    val completion =
      if visited == entries.length then TraversalCompletion.Exhausted
      else TraversalCompletion.StoppedEarly
    val counts = TraversalCounts(
      entries.length,
      visited,
      succeeded,
      rejected,
      failed,
      completion
    ).left.map(AnalysisExecutionError.InvalidCounts.apply) match
      case Left(error)  => return Left(error)
      case Right(value) => value
    val work = ExecutionWork(
      planned = entries.length,
      attempted = visited,
      succeeded = succeeded,
      rejected = rejected,
      failed = failed,
      operatorApplications = operatorApplications,
      materializedCells = materializedCells
    ).left.map(AnalysisExecutionError.InvalidWork.apply) match
      case Left(error)  => return Left(error)
      case Right(value) => value
    val receipt = ExecutionReceipt
      .fromTraversal(plan, receipts.result(), work)
      .left
      .map(AnalysisExecutionError.InvalidExecutionReceipt.apply) match
      case Left(error)  => return Left(error)
      case Right(value) => value

    Right(TraversalResult(state, counts, receipt))

object AnalysisExecution:
  def apply[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition,
      Prepared
  ](
      plan: ExecutionPlan[Source, Design, E, Rendition, Prepared]
  ): Either[
    AnalysisExecutionError,
    AnalysisExecution[Source, Design, E, Rendition, Prepared]
  ] =
    if plan.strategy.scheduling.parallelism != 1 then
      Left(
        AnalysisExecutionError.UnsupportedParallelism(
          plan.strategy.scheduling.parallelism
        )
      )
    else Right(new AnalysisExecution(plan))
