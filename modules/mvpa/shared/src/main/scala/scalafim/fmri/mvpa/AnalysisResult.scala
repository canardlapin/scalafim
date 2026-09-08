package scalafim.fmri.mvpa

enum TaskReportError:
  case NegativeOperatorApplications(value: Long)

  def message: String =
    this match
      case NegativeOperatorApplications(value) =>
        s"task report operator applications must be non-negative, obtained $value"

enum TaskOutcome[+A, +Rejection, +Failure]:
  case Success(value: A) extends TaskOutcome[A, Nothing, Nothing]
  case Rejected(reason: Rejection) extends TaskOutcome[Nothing, Rejection, Nothing]
  case Failed(error: Failure) extends TaskOutcome[Nothing, Nothing, Failure]

  def status: MeasurementStatus =
    this match
      case Success(_)  => MeasurementStatus.Succeeded
      case Rejected(_) => MeasurementStatus.Rejected
      case Failed(_)   => MeasurementStatus.Failed

final class TaskReport[+A, +Rejection, +Failure] private (
    val outcome: TaskOutcome[A, Rejection, Failure],
    val actual: ExecutionTarget,
    val operatorApplications: Long,
    val materializations: Vector[ExecutionMaterialization],
    val fallbacks: Vector[FallbackReceipt],
    val convergence: Vector[IterativeConvergence]
)

object TaskReport:
  def success[A](
      value: A,
      actual: ExecutionTarget,
      operatorApplications: Long = 0L,
      materializations: Seq[ExecutionMaterialization] = Vector.empty,
      fallbacks: Seq[FallbackReceipt] = Vector.empty,
      convergence: Seq[IterativeConvergence] = Vector.empty
  ): Either[TaskReportError, TaskReport[A, Nothing, Nothing]] =
    create(
      TaskOutcome.Success(value),
      actual,
      operatorApplications,
      materializations,
      fallbacks,
      convergence
    )

  def rejected[Rejection](
      reason: Rejection,
      actual: ExecutionTarget,
      operatorApplications: Long = 0L,
      materializations: Seq[ExecutionMaterialization] = Vector.empty,
      fallbacks: Seq[FallbackReceipt] = Vector.empty,
      convergence: Seq[IterativeConvergence] = Vector.empty
  ): Either[TaskReportError, TaskReport[Nothing, Rejection, Nothing]] =
    create(
      TaskOutcome.Rejected(reason),
      actual,
      operatorApplications,
      materializations,
      fallbacks,
      convergence
    )

  def failed[Failure](
      error: Failure,
      actual: ExecutionTarget,
      operatorApplications: Long = 0L,
      materializations: Seq[ExecutionMaterialization] = Vector.empty,
      fallbacks: Seq[FallbackReceipt] = Vector.empty,
      convergence: Seq[IterativeConvergence] = Vector.empty
  ): Either[TaskReportError, TaskReport[Nothing, Nothing, Failure]] =
    create(
      TaskOutcome.Failed(error),
      actual,
      operatorApplications,
      materializations,
      fallbacks,
      convergence
    )

  private def create[A, Rejection, Failure](
      outcome: TaskOutcome[A, Rejection, Failure],
      actual: ExecutionTarget,
      operatorApplications: Long,
      materializations: Seq[ExecutionMaterialization],
      fallbacks: Seq[FallbackReceipt],
      convergence: Seq[IterativeConvergence]
  ): Either[TaskReportError, TaskReport[A, Rejection, Failure]] =
    if operatorApplications < 0L then Left(TaskReportError.NegativeOperatorApplications(operatorApplications))
    else
      Right(
        new TaskReport(
          outcome,
          actual,
          operatorApplications,
          materializations.toVector,
          fallbacks.toVector,
          convergence.toVector
        )
      )

final class TaskContext private[mvpa] (
    val execution: ExecutionPlanIdentity,
    val measurementOrdinal: Int,
    val measurementCount: Int,
    val strategy: ExecutionStrategy
)

enum MeasurementOutcome[+A, +Rejection, +Failure]:
  case Success(value: A, override val receipt: MeasurementExecutionReceipt)
      extends MeasurementOutcome[A, Nothing, Nothing]
  case Rejected(reason: Rejection, override val receipt: MeasurementExecutionReceipt)
      extends MeasurementOutcome[Nothing, Rejection, Nothing]
  case Failed(error: Failure, override val receipt: MeasurementExecutionReceipt)
      extends MeasurementOutcome[Nothing, Nothing, Failure]

  def receipt: MeasurementExecutionReceipt =
    this match
      case Success(_, receipt)  => receipt
      case Rejected(_, receipt) => receipt
      case Failed(_, receipt)   => receipt

  def status: MeasurementStatus =
    this match
      case Success(_, _)  => MeasurementStatus.Succeeded
      case Rejected(_, _) => MeasurementStatus.Rejected
      case Failed(_, _)   => MeasurementStatus.Failed

final case class MeasurementValue[+A, +Rejection, +Failure, +Rendition](
    measurement: MeasurementIdentity,
    rendition: Rendition,
    outcome: MeasurementOutcome[A, Rejection, Failure]
)

enum TraversalCompletion:
  case Exhausted
  case StoppedEarly

enum TraversalCountError:
  case NegativeCount(label: String, value: Int)
  case VisitedExceedsExpected(expected: Int, visited: Int)
  case OutcomeCountMismatch(visited: Int, outcomes: Int)
  case CompletionMismatch(
      expected: Int,
      visited: Int,
      completion: TraversalCompletion
  )

  def message: String =
    this match
      case NegativeCount(label, value) =>
        s"traversal $label count must be non-negative, obtained $value"
      case VisitedExceedsExpected(expected, visited) =>
        s"traversal visited $visited measurements, exceeding expected count $expected"
      case OutcomeCountMismatch(visited, outcomes) =>
        s"traversal visited $visited measurements, but outcome counts sum to $outcomes"
      case CompletionMismatch(expected, visited, completion) =>
        s"traversal completion $completion is inconsistent with $visited of $expected measurements visited"

final class TraversalCounts private (
    val expected: Int,
    val visited: Int,
    val succeeded: Int,
    val rejected: Int,
    val failed: Int,
    val completion: TraversalCompletion
):
  override def equals(other: Any): Boolean =
    other match
      case that: TraversalCounts =>
        expected == that.expected &&
        visited == that.visited &&
        succeeded == that.succeeded &&
        rejected == that.rejected &&
        failed == that.failed &&
        completion == that.completion
      case _ => false

  override def hashCode(): Int =
    (expected, visited, succeeded, rejected, failed, completion).hashCode

object TraversalCounts:
  def apply(
      expected: Int,
      visited: Int,
      succeeded: Int,
      rejected: Int,
      failed: Int,
      completion: TraversalCompletion
  ): Either[TraversalCountError, TraversalCounts] =
    val values = Vector(
      "expected" -> expected,
      "visited" -> visited,
      "succeeded" -> succeeded,
      "rejected" -> rejected,
      "failed" -> failed
    )
    values.find(_._2 < 0) match
      case Some((label, value))       => Left(TraversalCountError.NegativeCount(label, value))
      case None if visited > expected =>
        Left(TraversalCountError.VisitedExceedsExpected(expected, visited))
      case None if succeeded + rejected + failed != visited =>
        Left(
          TraversalCountError.OutcomeCountMismatch(
            visited,
            succeeded + rejected + failed
          )
        )
      case None =>
        val validCompletion = completion match
          case TraversalCompletion.Exhausted    => visited == expected
          case TraversalCompletion.StoppedEarly => visited < expected
        if !validCompletion then Left(TraversalCountError.CompletionMismatch(expected, visited, completion))
        else
          Right(
            new TraversalCounts(
              expected,
              visited,
              succeeded,
              rejected,
              failed,
              completion
            )
          )

enum VisitDecision[+State]:
  case Continue(state: State)
  case Stop(state: State)

final case class TraversalResult[State](
    state: State,
    counts: TraversalCounts,
    receipt: ExecutionReceipt
)

enum AnalysisResultError:
  case ReceiptExecutionMismatch(
      expected: ExecutionPlanFingerprint,
      actual: ExecutionPlanFingerprint
  )
  case ExpectedCountMismatch(expected: Int, actual: Int)
  case VisitedCountMismatch(expected: Int, actual: Int)
  case ReceiptCountMismatch(expected: Int, actual: Int)
  case StatusCountMismatch(
      expectedSucceeded: Int,
      expectedRejected: Int,
      expectedFailed: Int,
      actualSucceeded: Int,
      actualRejected: Int,
      actualFailed: Int
  )
  case MeasurementMismatch(
      position: Int,
      expected: MeasurementFingerprint,
      actual: MeasurementFingerprint
  )
  case OutcomeReceiptMismatch(position: Int)
  case OutcomeStatusMismatch(
      position: Int,
      expected: MeasurementStatus,
      actual: MeasurementStatus
  )

  def message: String =
    this match
      case ReceiptExecutionMismatch(expected, actual) =>
        s"analysis result receipt belongs to execution ${actual.value}, expected ${expected.value}"
      case ExpectedCountMismatch(expected, actual) =>
        s"analysis result expected $actual measurements, but the execution plan contains $expected"
      case VisitedCountMismatch(expected, actual) =>
        s"analysis result contains $actual values, but traversal counts report $expected visited measurements"
      case ReceiptCountMismatch(expected, actual) =>
        s"analysis result contains $expected values, but its receipt contains $actual measurement records"
      case StatusCountMismatch(
            expectedSucceeded,
            expectedRejected,
            expectedFailed,
            actualSucceeded,
            actualRejected,
            actualFailed
          ) =>
        s"analysis result traversal counts ($expectedSucceeded,$expectedRejected,$expectedFailed) do not match outcomes ($actualSucceeded,$actualRejected,$actualFailed)"
      case MeasurementMismatch(position, expected, actual) =>
        s"analysis result value $position names measurement ${actual.value}, expected ${expected.value}"
      case OutcomeReceiptMismatch(position) =>
        s"analysis result value $position does not carry the aggregate receipt's measurement record"
      case OutcomeStatusMismatch(position, expected, actual) =>
        s"analysis result value $position has outcome status $actual, but its receipt records $expected"

final class AnalysisResult[+A, +Rejection, +Failure, +Rendition] private (
    val plan: ScientificPlanIdentity,
    val values: Vector[MeasurementValue[A, Rejection, Failure, Rendition]],
    val counts: TraversalCounts,
    val receipt: ExecutionReceipt
)

object AnalysisResult:
  /** Admit a collected result only from an already validated execution and recheck that values, traversal counts, and
    * execution receipts describe the same ordered measurement traversal.
    */
  def fromExecution[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition,
      Prepared
  ](
      execution: AnalysisExecution[Source, Design, E, Rendition, Prepared]
  )(
      values: Seq[
        MeasurementValue[
          execution.Result,
          execution.Rejection,
          execution.Failure,
          Rendition
        ]
      ],
      counts: TraversalCounts,
      receipt: ExecutionReceipt
  ): Either[
    AnalysisResultError,
    AnalysisResult[
      execution.Result,
      execution.Rejection,
      execution.Failure,
      Rendition
    ]
  ] =
    val valueVector = values.toVector
    val plan = execution.plan
    val expectedMeasurements = plan.scientific.specification.frame.entries
    if receipt.execution != plan.identity then
      Left(
        AnalysisResultError.ReceiptExecutionMismatch(
          plan.identity.fingerprint,
          receipt.execution.fingerprint
        )
      )
    else if counts.expected != expectedMeasurements.length then
      Left(
        AnalysisResultError.ExpectedCountMismatch(
          expectedMeasurements.length,
          counts.expected
        )
      )
    else if counts.visited != valueVector.length then
      Left(
        AnalysisResultError.VisitedCountMismatch(
          counts.visited,
          valueVector.length
        )
      )
    else if receipt.measurements.length != valueVector.length then
      Left(
        AnalysisResultError.ReceiptCountMismatch(
          valueVector.length,
          receipt.measurements.length
        )
      )
    else
      val actualSucceeded = valueVector.count(_.outcome.status == MeasurementStatus.Succeeded)
      val actualRejected = valueVector.count(_.outcome.status == MeasurementStatus.Rejected)
      val actualFailed = valueVector.count(_.outcome.status == MeasurementStatus.Failed)
      if counts.succeeded != actualSucceeded ||
        counts.rejected != actualRejected ||
        counts.failed != actualFailed
      then
        Left(
          AnalysisResultError.StatusCountMismatch(
            counts.succeeded,
            counts.rejected,
            counts.failed,
            actualSucceeded,
            actualRejected,
            actualFailed
          )
        )
      else
        var ordinal = 0
        while ordinal < valueVector.length do
          val value = valueVector(ordinal)
          val expected = expectedMeasurements(ordinal).measurement.identity
          if value.measurement != expected then
            return Left(
              AnalysisResultError.MeasurementMismatch(
                ordinal,
                expected.fingerprint,
                value.measurement.fingerprint
              )
            )
          val measurementReceipt = receipt.measurements(ordinal)
          if value.outcome.receipt != measurementReceipt then
            return Left(AnalysisResultError.OutcomeReceiptMismatch(ordinal))
          if value.outcome.status != measurementReceipt.status then
            return Left(
              AnalysisResultError.OutcomeStatusMismatch(
                ordinal,
                measurementReceipt.status,
                value.outcome.status
              )
            )
          ordinal += 1
        Right(
          new AnalysisResult(
            plan.scientific.identity,
            valueVector,
            counts,
            receipt
          )
        )

trait ResultView[-A, +B]:
  def apply(value: A): B

final case class DerivedMeasurementValue[+B, +Rendition](
    measurement: MeasurementIdentity,
    rendition: Rendition,
    value: B
)

extension [A, Rejection, Failure, Rendition](
    result: AnalysisResult[A, Rejection, Failure, Rendition]
)
  def view[B](using projection: ResultView[A, B]): Vector[DerivedMeasurementValue[B, Rendition]] =
    result.values.collect:
      case MeasurementValue(measurement, rendition, MeasurementOutcome.Success(value, _)) =>
        DerivedMeasurementValue(measurement, rendition, projection(value))
