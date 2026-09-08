package scalafim.fmri.mvpa

enum MeasurementStatus:
  case Succeeded
  case Rejected
  case Failed

enum ExecutionReceiptError:
  case InvalidWork(detail: String)
  case InvalidConvergence(iterations: Int, residualNorm: Double)
  case InvalidReason(detail: String)
  case InvalidFallbackTransition(target: ExecutionTarget)
  case PlannedWorkMismatch(expected: Int, actual: Int)
  case AttemptedWorkMismatch(expected: Int, actual: Int)
  case StatusWorkMismatch(
      succeeded: Int,
      rejected: Int,
      failed: Int,
      actualSucceeded: Int,
      actualRejected: Int,
      actualFailed: Int
  )
  case OperatorWorkMismatch(expected: Long, actual: Long)
  case MaterializedWorkMismatch(expected: Long, actual: Long)
  case ReceiptExecutionMismatch(
      expected: ExecutionPlanFingerprint,
      actual: ExecutionPlanFingerprint
  )
  case ReceiptOrdinalMismatch(expected: Int, actual: Int)
  case ReceiptOrdinalOutOfBounds(position: Int, measurementCount: Int)
  case ReceiptMeasurementMismatch(
      position: Int,
      expected: MeasurementFingerprint,
      actual: MeasurementFingerprint
  )
  case UnexpectedConvergence
  case ConvergenceSolverMismatch(expected: SolverIdentity, actual: SolverIdentity)
  case MaterializationForbidden(scope: ExecutionScope)
  case InvalidMaterializationReceipt(rows: Int, columns: Int, elements: Long)
  case MaterializationBudgetExceeded(scope: ExecutionScope, cells: Long, maximum: Long)
  case UnrecordedFallback(expected: ExecutionTarget, actual: ExecutionTarget)
  case UnexpectedFallback(expected: ExecutionTarget)
  case BrokenFallbackChain(expectedFrom: ExecutionTarget, actualFrom: ExecutionTarget)
  case FallbackNotPermitted(target: ExecutionTarget)
  case FallbackTargetMismatch(expected: ExecutionTarget, actual: ExecutionTarget)

  def message: String =
    this match
      case InvalidWork(detail)                          => s"invalid execution work receipt: $detail"
      case InvalidConvergence(iterations, residualNorm) =>
        s"convergence evidence requires non-negative iterations and finite non-negative residual; obtained $iterations and $residualNorm"
      case InvalidReason(detail)             => s"invalid execution reason: $detail"
      case InvalidFallbackTransition(target) =>
        s"fallback receipt must change execution target; both sides are ${target.label}"
      case PlannedWorkMismatch(expected, actual) =>
        s"execution receipt planned $actual tasks, expected $expected measurements"
      case AttemptedWorkMismatch(expected, actual) =>
        s"execution receipt reports $actual attempted tasks, but contains $expected measurement receipts"
      case StatusWorkMismatch(succeeded, rejected, failed, actualSucceeded, actualRejected, actualFailed) =>
        s"execution status counts ($succeeded,$rejected,$failed) do not match measurement receipts ($actualSucceeded,$actualRejected,$actualFailed)"
      case OperatorWorkMismatch(expected, actual) =>
        s"execution receipt records $actual operator applications, expected $expected from measurement receipts"
      case MaterializedWorkMismatch(expected, actual) =>
        s"execution receipt records $actual materialized cells, expected $expected from measurement receipts"
      case ReceiptExecutionMismatch(expected, actual) =>
        s"measurement receipt belongs to execution ${actual.value}, expected ${expected.value}"
      case ReceiptOrdinalMismatch(expected, actual) =>
        s"measurement receipt has ordinal $actual, expected $expected"
      case ReceiptOrdinalOutOfBounds(position, measurementCount) =>
        s"measurement receipt ordinal $position is outside [0, $measurementCount)"
      case ReceiptMeasurementMismatch(position, expected, actual) =>
        s"measurement receipt $position names ${actual.value}, expected ${expected.value}"
      case UnexpectedConvergence =>
        "execution receipt contains iterative convergence evidence without a selected solver"
      case ConvergenceSolverMismatch(expected, actual) =>
        s"convergence receipt names solver '${actual.id.value}', expected '${expected.id.value}'"
      case MaterializationForbidden(scope) =>
        s"materialization at ${scope.label} was forbidden"
      case InvalidMaterializationReceipt(rows, columns, elements) =>
        s"materialization receipt shape ${rows}x$columns is inconsistent with $elements recorded cells"
      case MaterializationBudgetExceeded(scope, cells, maximum) =>
        s"materialization at ${scope.label} used $cells cells, exceeding budget $maximum"
      case UnrecordedFallback(expected, actual) =>
        s"execution changed from ${expected.label} to ${actual.label} without a fallback receipt"
      case UnexpectedFallback(expected) =>
        s"execution recorded fallback from ${expected.label}, but finished at the requested target"
      case BrokenFallbackChain(expectedFrom, actualFrom) =>
        s"fallback chain expected ${expectedFrom.label}, but the next receipt starts at ${actualFrom.label}"
      case FallbackNotPermitted(target) =>
        s"fallback to ${target.label} was not permitted by the execution strategy"
      case FallbackTargetMismatch(expected, actual) =>
        s"fallback chain ends at ${actual.label}, but execution reports ${expected.label}"

enum ExecutionScope:
  case WholePlan
  case Measurement(id: MeasurementId)

  def label: String =
    this match
      case WholePlan       => "whole-plan"
      case Measurement(id) => s"measurement:${id.value}"

final class ExecutionMaterialization private (
    val scope: ExecutionScope,
    val receipt: MaterializationReceipt,
    val reason: String
):
  def cells: Long = receipt.elements

  override def equals(other: Any): Boolean =
    other match
      case that: ExecutionMaterialization =>
        scope == that.scope && receipt == that.receipt && reason == that.reason
      case _ => false

  override def hashCode(): Int =
    (scope, receipt, reason).hashCode

object ExecutionMaterialization:
  def apply(
      scope: ExecutionScope,
      receipt: MaterializationReceipt,
      reason: String
  ): Either[ExecutionReceiptError, ExecutionMaterialization] =
    validatedReason(reason).map(new ExecutionMaterialization(scope, receipt, _))

  private[mvpa] def validatedReason(
      reason: String
  ): Either[ExecutionReceiptError, String] =
    AxisText
      .exact("execution reason", reason)
      .left
      .map(error => ExecutionReceiptError.InvalidReason(error.message))

final class FallbackReceipt private (
    val from: ExecutionTarget,
    val to: ExecutionTarget,
    val reason: String
):
  override def equals(other: Any): Boolean =
    other match
      case that: FallbackReceipt =>
        from == that.from && to == that.to && reason == that.reason
      case _ => false

  override def hashCode(): Int =
    (from, to, reason).hashCode

object FallbackReceipt:
  def apply(
      from: ExecutionTarget,
      to: ExecutionTarget,
      reason: String
  ): Either[ExecutionReceiptError, FallbackReceipt] =
    if from == to then Left(ExecutionReceiptError.InvalidFallbackTransition(from))
    else
      ExecutionMaterialization
        .validatedReason(reason)
        .map(new FallbackReceipt(from, to, _))

final class ExecutionWork private (
    val planned: Int,
    val attempted: Int,
    val succeeded: Int,
    val rejected: Int,
    val failed: Int,
    val operatorApplications: Long,
    val materializedCells: Long
):
  override def equals(other: Any): Boolean =
    other match
      case that: ExecutionWork =>
        planned == that.planned &&
        attempted == that.attempted &&
        succeeded == that.succeeded &&
        rejected == that.rejected &&
        failed == that.failed &&
        operatorApplications == that.operatorApplications &&
        materializedCells == that.materializedCells
      case _ => false

  override def hashCode(): Int =
    (planned, attempted, succeeded, rejected, failed, operatorApplications, materializedCells).hashCode

object ExecutionWork:
  def apply(
      planned: Int,
      attempted: Int,
      succeeded: Int,
      rejected: Int,
      failed: Int,
      operatorApplications: Long,
      materializedCells: Long
  ): Either[ExecutionReceiptError, ExecutionWork] =
    val counts = Vector(planned, attempted, succeeded, rejected, failed)
    if counts.exists(_ < 0) then Left(ExecutionReceiptError.InvalidWork("task counts must be non-negative"))
    else if attempted > planned then Left(ExecutionReceiptError.InvalidWork("attempted tasks exceed planned tasks"))
    else if succeeded + rejected + failed != attempted then
      Left(
        ExecutionReceiptError.InvalidWork(
          "succeeded, rejected, and failed tasks must sum to attempted tasks"
        )
      )
    else if operatorApplications < 0L || materializedCells < 0L then
      Left(
        ExecutionReceiptError.InvalidWork(
          "operator applications and materialized cells must be non-negative"
        )
      )
    else
      Right(
        new ExecutionWork(
          planned,
          attempted,
          succeeded,
          rejected,
          failed,
          operatorApplications,
          materializedCells
        )
      )

enum ConvergenceOutcome:
  case Converged
  case DidNotConverge

final class IterativeConvergence private (
    val scope: ExecutionScope,
    val solver: SolverIdentity,
    val outcome: ConvergenceOutcome,
    val iterations: Int,
    val residualNorm: Double
):
  override def equals(other: Any): Boolean =
    other match
      case that: IterativeConvergence =>
        scope == that.scope &&
        solver == that.solver &&
        outcome == that.outcome &&
        iterations == that.iterations &&
        residualNorm == that.residualNorm
      case _ => false

  override def hashCode(): Int =
    (scope, solver, outcome, iterations, residualNorm).hashCode

object IterativeConvergence:
  def apply(
      scope: ExecutionScope,
      solver: SolverIdentity,
      outcome: ConvergenceOutcome,
      iterations: Int,
      residualNorm: Double
  ): Either[ExecutionReceiptError, IterativeConvergence] =
    if iterations < 0 || !residualNorm.isFinite || residualNorm < 0.0 then
      Left(ExecutionReceiptError.InvalidConvergence(iterations, residualNorm))
    else Right(new IterativeConvergence(scope, solver, outcome, iterations, residualNorm))

final class MeasurementExecutionReceipt private (
    val execution: ExecutionPlanIdentity,
    val measurement: MeasurementIdentity,
    val ordinal: Int,
    val status: MeasurementStatus,
    val requested: ExecutionTarget,
    val actual: ExecutionTarget,
    val operatorApplications: Long,
    val materializations: Vector[ExecutionMaterialization],
    val fallbacks: Vector[FallbackReceipt],
    val convergence: Vector[IterativeConvergence]
):
  def materializedCells: Long =
    materializations.foldLeft(0L)(_ + _.cells)

  override def equals(other: Any): Boolean =
    other match
      case that: MeasurementExecutionReceipt =>
        execution == that.execution &&
        measurement == that.measurement &&
        ordinal == that.ordinal &&
        status == that.status &&
        requested == that.requested &&
        actual == that.actual &&
        operatorApplications == that.operatorApplications &&
        materializations == that.materializations &&
        fallbacks == that.fallbacks &&
        convergence == that.convergence
      case _ => false

  override def hashCode(): Int =
    (execution, measurement, ordinal, status, actual).hashCode

object MeasurementExecutionReceipt:
  /** Record one task only against an admitted execution plan. The execution identity, strategy, and ordered measurement
    * identity are derived from the plan rather than accepted as forgeable arguments.
    */
  def fromTask[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition,
      Prepared
  ](
      plan: ExecutionPlan[Source, Design, E, Rendition, Prepared],
      ordinal: Int,
      status: MeasurementStatus,
      actual: ExecutionTarget,
      operatorApplications: Long,
      materializations: Seq[ExecutionMaterialization],
      fallbacks: Seq[FallbackReceipt],
      convergence: Seq[IterativeConvergence]
  ): Either[ExecutionReceiptError, MeasurementExecutionReceipt] =
    val measurementCount = plan.measurementCount
    val materializationValues = materializations.toVector
    val fallbackValues = fallbacks.toVector
    val convergenceValues = convergence.toVector
    if ordinal < 0 || ordinal >= measurementCount then
      Left(ExecutionReceiptError.ReceiptOrdinalOutOfBounds(ordinal, measurementCount))
    else if operatorApplications < 0L then
      Left(ExecutionReceiptError.InvalidWork("operator applications must be non-negative"))
    else
      val strategy = plan.strategy
      val measurement = plan.scientific.specification.frame.entries(ordinal).measurement.identity
      for
        _ <- ExecutionReceiptValidation.materializations(
          strategy.materialization,
          materializationValues
        )
        _ <- ExecutionReceiptValidation.fallbacks(strategy, actual, fallbackValues)
        _ <- ExecutionReceiptValidation.convergence(strategy.solver, convergenceValues)
      yield new MeasurementExecutionReceipt(
        plan.identity,
        measurement,
        ordinal,
        status,
        strategy.target,
        actual,
        operatorApplications,
        materializationValues,
        fallbackValues,
        convergenceValues
      )

final class ExecutionReceipt private (
    val execution: ExecutionPlanIdentity,
    val requested: ExecutionTarget,
    val precision: NumericPrecision,
    val solver: SolverChoice,
    val randomStreams: Vector[RandomStream],
    val scheduling: Scheduling,
    val delivery: ResultDelivery,
    val measurements: Vector[MeasurementExecutionReceipt],
    val work: ExecutionWork
):
  def actualTargets: Vector[ExecutionTarget] =
    measurements.map(_.actual)

  def materializations: Vector[ExecutionMaterialization] =
    measurements.flatMap(_.materializations)

  def fallbacks: Vector[FallbackReceipt] =
    measurements.flatMap(_.fallbacks)

  def convergence: Vector[IterativeConvergence] =
    measurements.flatMap(_.convergence)

  override def equals(other: Any): Boolean =
    other match
      case that: ExecutionReceipt =>
        execution == that.execution &&
        requested == that.requested &&
        precision == that.precision &&
        solver == that.solver &&
        randomStreams == that.randomStreams &&
        scheduling == that.scheduling &&
        delivery == that.delivery &&
        measurements == that.measurements &&
        work == that.work
      case _ => false

  override def hashCode(): Int =
    (execution, measurements, work).hashCode

object ExecutionReceipt:
  /** Reconcile aggregate execution evidence against an admitted plan. */
  def fromTraversal[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition,
      Prepared
  ](
      plan: ExecutionPlan[Source, Design, E, Rendition, Prepared],
      measurements: Seq[MeasurementExecutionReceipt],
      work: ExecutionWork
  ): Either[ExecutionReceiptError, ExecutionReceipt] =
    val values = measurements.toVector
    val expectedMeasurements = plan.scientific.specification.frame.entries
    for
      _ <- ExecutionReceiptValidation.work(expectedMeasurements.length, values, work)
      _ <- ExecutionReceiptValidation.measurementOrder(
        plan.identity,
        expectedMeasurements.map(_.measurement.identity),
        values
      )
    yield new ExecutionReceipt(
      plan.identity,
      plan.strategy.target,
      plan.strategy.precision,
      plan.strategy.solver,
      plan.strategy.randomStreams,
      plan.strategy.scheduling,
      plan.strategy.delivery,
      values,
      work
    )

private[mvpa] object ExecutionReceiptValidation:
  def work(
      measurementCount: Int,
      measurements: Vector[MeasurementExecutionReceipt],
      work: ExecutionWork
  ): Either[ExecutionReceiptError, Unit] =
    val succeeded = measurements.count(_.status == MeasurementStatus.Succeeded)
    val rejected = measurements.count(_.status == MeasurementStatus.Rejected)
    val failed = measurements.count(_.status == MeasurementStatus.Failed)
    val operatorApplications = measurements.foldLeft(0L)(_ + _.operatorApplications)
    val materializedCells = measurements.foldLeft(0L)(_ + _.materializedCells)
    if work.planned != measurementCount then
      Left(ExecutionReceiptError.PlannedWorkMismatch(measurementCount, work.planned))
    else if work.attempted != measurements.length then
      Left(ExecutionReceiptError.AttemptedWorkMismatch(measurements.length, work.attempted))
    else if work.succeeded != succeeded || work.rejected != rejected || work.failed != failed then
      Left(
        ExecutionReceiptError.StatusWorkMismatch(
          work.succeeded,
          work.rejected,
          work.failed,
          succeeded,
          rejected,
          failed
        )
      )
    else if work.operatorApplications != operatorApplications then
      Left(
        ExecutionReceiptError.OperatorWorkMismatch(
          operatorApplications,
          work.operatorApplications
        )
      )
    else if work.materializedCells != materializedCells then
      Left(
        ExecutionReceiptError.MaterializedWorkMismatch(
          materializedCells,
          work.materializedCells
        )
      )
    else Right(())

  def measurementOrder(
      execution: ExecutionPlanIdentity,
      expected: Vector[MeasurementIdentity],
      actual: Vector[MeasurementExecutionReceipt]
  ): Either[ExecutionReceiptError, Unit] =
    var ordinal = 0
    while ordinal < actual.length do
      val receipt = actual(ordinal)
      if receipt.execution != execution then
        return Left(
          ExecutionReceiptError.ReceiptExecutionMismatch(
            execution.fingerprint,
            receipt.execution.fingerprint
          )
        )
      if receipt.ordinal != ordinal then
        return Left(ExecutionReceiptError.ReceiptOrdinalMismatch(ordinal, receipt.ordinal))
      val expectedMeasurement = expected(ordinal).fingerprint
      if receipt.measurement.fingerprint != expectedMeasurement then
        return Left(
          ExecutionReceiptError.ReceiptMeasurementMismatch(
            ordinal,
            expectedMeasurement,
            receipt.measurement.fingerprint
          )
        )
      ordinal += 1
    Right(())

  def convergence(
      solver: SolverChoice,
      convergence: Vector[IterativeConvergence]
  ): Either[ExecutionReceiptError, Unit] =
    solver match
      case SolverChoice.NotApplicable =>
        if convergence.isEmpty then Right(())
        else Left(ExecutionReceiptError.UnexpectedConvergence)
      case SolverChoice.Selected(expected) =>
        convergence.find(_.solver != expected) match
          case None        => Right(())
          case Some(value) =>
            Left(
              ExecutionReceiptError.ConvergenceSolverMismatch(
                expected,
                value.solver
              )
            )

  def materializations(
      policy: MaterializationPolicy,
      materializations: Vector[ExecutionMaterialization]
  ): Either[ExecutionReceiptError, Unit] =
    val iterator = materializations.iterator
    while iterator.hasNext do
      val materialization = iterator.next()
      val receipt = materialization.receipt
      val expectedCells = receipt.rows.toLong * receipt.columns.toLong
      if receipt.rows <= 0 || receipt.columns <= 0 || receipt.elements != expectedCells then
        return Left(
          ExecutionReceiptError.InvalidMaterializationReceipt(
            receipt.rows,
            receipt.columns,
            receipt.elements
          )
        )
      else if receipt.elements > receipt.budget.maxElements then
        return Left(
          ExecutionReceiptError.MaterializationBudgetExceeded(
            materialization.scope,
            receipt.elements,
            receipt.budget.maxElements
          )
        )
      else
        policy match
          case MaterializationPolicy.Reject =>
            return Left(ExecutionReceiptError.MaterializationForbidden(materialization.scope))
          case MaterializationPolicy.Allow(budget) =>
            if materialization.cells > budget.maxElements then
              return Left(
                ExecutionReceiptError.MaterializationBudgetExceeded(
                  materialization.scope,
                  materialization.cells,
                  budget.maxElements
                )
              )
    Right(())

  def fallbacks(
      strategy: ExecutionStrategy,
      actual: ExecutionTarget,
      fallbacks: Vector[FallbackReceipt]
  ): Either[ExecutionReceiptError, Unit] =
    if fallbacks.isEmpty then
      if actual == strategy.target then Right(())
      else Left(ExecutionReceiptError.UnrecordedFallback(strategy.target, actual))
    else if actual == strategy.target then Left(ExecutionReceiptError.UnexpectedFallback(strategy.target))
    else
      var expectedFrom = strategy.target
      val iterator = fallbacks.iterator
      while iterator.hasNext do
        val fallback = iterator.next()
        if fallback.from != expectedFrom then
          return Left(
            ExecutionReceiptError.BrokenFallbackChain(expectedFrom, fallback.from)
          )
        if !strategy.fallback.permits(fallback.to) then
          return Left(ExecutionReceiptError.FallbackNotPermitted(fallback.to))
        expectedFrom = fallback.to
      if expectedFrom != actual then Left(ExecutionReceiptError.FallbackTargetMismatch(actual, expectedFrom))
      else Right(())
