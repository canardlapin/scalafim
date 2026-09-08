package scalafim.fmri.mvpa

import resample4s.core.IndexSpace
import resample4s.core.Injection

class AnalysisResultSuite extends munit.FunSuite:
  import ExecutionPlanFixtures.*
  import LabelCountCompiler.given
  import ScientificPlanFixtures.*

  private type ToyBound = BoundScientificPlan[
    LabeledToySource,
    ToyEvidenceDesign,
    LabelCountEstimand.type,
    String,
    PreparedLabelCount
  ]

  private type ToyPlan = ExecutionPlan[
    LabeledToySource,
    ToyEvidenceDesign,
    LabelCountEstimand.type,
    String,
    PreparedLabelCount
  ]

  private val mixedTask = new MeasurementTask[
    LabeledToySource,
    ToyEvidenceDesign,
    LabelCountEstimand.type,
    String,
    PreparedLabelCount
  ]:
    override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
      Right(())

    override def execute(
        plan: ToyBound
    )(
        measurement: MeasurementEntry[
          plan.specification.source.Neural,
          plan.specification.source.NeuralKey,
          ?,
          String
        ],
        context: TaskContext
    ): Either[
      TaskReportError,
      TaskReport[plan.Result, plan.Rejection, plan.Failure]
    ] =
      measurement.measurement.identity.id.value match
        case "alpha" =>
          TaskReport.success(
            plan.prepared.classCount + context.measurementOrdinal,
            context.strategy.target,
            operatorApplications = 1L
          )
        case "beta" =>
          val denseTarget = ExecutionTarget(DenseBackend, ExecutionRepresentation.Dense)
          val fallback = FallbackReceipt(
            context.strategy.target,
            denseTarget,
            "beta uses its declared dense fallback"
          ).toOption.get
          TaskReport.rejected(
            LabelCountRejection.OneClass("beta-null"),
            denseTarget,
            operatorApplications = 2L,
            fallbacks = Vector(fallback)
          )
        case _ =>
          TaskReport.failed(
            LabelCountFailure.Kernel("gamma numerical failure"),
            context.strategy.target,
            operatorApplications = 3L
          )

  private def frame(
      source: LabeledToySource
  ): MeasurementFrame[source.Neural, source.NeuralKey, String] =
    val space = IndexSpace.of(source.features.size).toOption.get
    val specifications = Vector(
      ("gamma", Vector(3)),
      ("alpha", Vector(0, 1)),
      ("beta", Vector(2))
    )
    val entries = specifications.map: (id, positions) =>
      val injection = Injection
        .from(IArray.unsafeFromArray(positions.toArray), space)
        .toOption
        .get
      val measurement = Measurement
        .hardSelection(source.features, MeasurementId.unsafe(id), injection)
        .toOption
        .get
      MeasurementEntry(measurement, s"rendition-$id")
    MeasurementFrame(source.features)(entries).toOption.get

  private def bound(): ToyBound =
    val sourceValue = sourceWithLabels()
    val specification = ScientificSpecification(sourceValue)(
      design(sourceValue),
      frame(sourceValue),
      LabelCountEstimand
    ).toOption.get
    Mvpa.bind(specification).toOption.get

  private def executionPlan(
      delivery: ResultDelivery,
      scheduling: Scheduling = Scheduling.serial,
      task: MeasurementTask[
        LabeledToySource,
        ToyEvidenceDesign,
        LabelCountEstimand.type,
        String,
        PreparedLabelCount
      ] = mixedTask
  ): ToyPlan =
    val denseTarget = ExecutionTarget(DenseBackend, ExecutionRepresentation.Dense)
    val strategyValue = strategy(
      ExecutionRepresentation.Operator,
      delivery,
      fallback = FallbackPolicy.explicit(Vector(denseTarget)).toOption.get,
      scheduling = scheduling
    )
    ExecutionPlan(bound(), strategyValue)(using task).toOption.get

  test("collected results contain exactly one typed outcome and receipt per measurement"):
    val planValue = executionPlan(ResultDelivery.Collected)
    val result = AnalysisExecution(planValue).toOption.get.collect.toOption.get

    assertEquals(
      result.values.map(_.measurement.id.value),
      Vector("alpha", "beta", "gamma")
    )
    assertEquals(
      result.values.map(_.rendition),
      Vector("rendition-alpha", "rendition-beta", "rendition-gamma")
    )
    assert(result.values(0).outcome match
      case MeasurementOutcome.Success(2, receipt) =>
        receipt.status == MeasurementStatus.Succeeded
      case _ => false)
    assert(result.values(1).outcome match
      case MeasurementOutcome.Rejected(
            LabelCountRejection.OneClass("beta-null"),
            receipt
          ) =>
        receipt.status == MeasurementStatus.Rejected
      case _ => false)
    assert(result.values(2).outcome match
      case MeasurementOutcome.Failed(
            LabelCountFailure.Kernel("gamma numerical failure"),
            receipt
          ) =>
        receipt.status == MeasurementStatus.Failed
      case _ => false)
    assert(result.values.forall: value =>
      value.outcome.receipt.measurement == value.measurement)
    assertEquals(
      result.counts,
      TraversalCounts(3, 3, 1, 1, 1, TraversalCompletion.Exhausted).toOption.get
    )
    assertEquals(result.receipt.work.operatorApplications, 6L)
    assertEquals(
      result.receipt.actualTargets.map(_.representation),
      Vector(
        ExecutionRepresentation.Operator,
        ExecutionRepresentation.Dense,
        ExecutionRepresentation.Operator
      )
    )
    assertEquals(result.receipt.fallbacks.length, 1)
    assertEquals(result.plan, planValue.scientific.identity)

  test("streaming visitor, fold, and collected views preserve order and identity"):
    val planValue = executionPlan(ResultDelivery.Streaming)
    val execution = AnalysisExecution(planValue).toOption.get
    val visited = execution.visit(Vector.empty[execution.Value]): (values, value) =>
      VisitDecision.Continue(values :+ value)
    val collected = execution.collect
    val folded = execution.foldLeft(Vector.empty[String]): (ids, value) =>
      ids :+ value.measurement.id.value

    val visitedValue = visited.toOption.get
    val collectedValue = collected.toOption.get
    assertEquals(visitedValue.state, collectedValue.values)
    assertEquals(visitedValue.counts, collectedValue.counts)
    assertEquals(visitedValue.receipt, collectedValue.receipt)
    assertEquals(folded.toOption.get.state, Vector("alpha", "beta", "gamma"))
    assertEquals(
      folded.toOption.get.receipt.measurements.map(_.measurement.fingerprint),
      collectedValue.values.map(_.measurement.fingerprint)
    )

  test("early stop is a reconciled prefix, never a partial AnalysisResult"):
    val execution = AnalysisExecution(
      executionPlan(ResultDelivery.Streaming)
    ).toOption.get
    val traversed = execution.visit(Vector.empty[String]): (ids, value) =>
      val next = ids :+ value.measurement.id.value
      if next.length == 2 then VisitDecision.Stop(next)
      else VisitDecision.Continue(next)
    val result = traversed.toOption.get

    assertEquals(result.state, Vector("alpha", "beta"))
    assertEquals(
      result.counts,
      TraversalCounts(3, 2, 1, 1, 0, TraversalCompletion.StoppedEarly).toOption.get
    )
    assertEquals(result.receipt.measurements.map(_.ordinal), Vector(0, 1))
    assertEquals(result.receipt.work.planned, 3)
    assertEquals(result.receipt.work.attempted, 2)

  test("delivery and scheduling contracts fail before traversal"):
    val collectedExecution = AnalysisExecution(
      executionPlan(ResultDelivery.Collected)
    ).toOption.get
    assert(
      collectedExecution
        .visit(0): (count, _) =>
          VisitDecision.Continue(count + 1)
        .left
        .exists:
          case AnalysisExecutionError.StreamingDeliveryRequired(ResultDelivery.Collected) => true
          case _                                                                          => false
    )

    val parallel = Scheduling
      .parallel(2, 1, CompletionOrder.MeasurementOrder)
      .toOption
      .get
    assert(
      AnalysisExecution(
        executionPlan(ResultDelivery.Streaming, scheduling = parallel)
      ).left.exists:
        case AnalysisExecutionError.UnsupportedParallelism(2) => true
        case _                                                => false
    )

  test("malformed task and telemetry reports cannot become method failures"):
    val invalidReportTask = new MeasurementTask[
      LabeledToySource,
      ToyEvidenceDesign,
      LabelCountEstimand.type,
      String,
      PreparedLabelCount
    ]:
      override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
        Right(())

      override def execute(plan: ToyBound)(
          measurement: MeasurementEntry[
            plan.specification.source.Neural,
            plan.specification.source.NeuralKey,
            ?,
            String
          ],
          context: TaskContext
      ): Either[TaskReportError, TaskReport[plan.Result, plan.Rejection, plan.Failure]] =
        Left(TaskReportError.NegativeOperatorApplications(-1L))

    val missingFallbackTask = new MeasurementTask[
      LabeledToySource,
      ToyEvidenceDesign,
      LabelCountEstimand.type,
      String,
      PreparedLabelCount
    ]:
      override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
        Right(())

      override def execute(plan: ToyBound)(
          measurement: MeasurementEntry[
            plan.specification.source.Neural,
            plan.specification.source.NeuralKey,
            ?,
            String
          ],
          context: TaskContext
      ): Either[TaskReportError, TaskReport[plan.Result, plan.Rejection, plan.Failure]] =
        TaskReport.success(
          2,
          ExecutionTarget(DenseBackend, ExecutionRepresentation.Dense)
        )

    val invalidExecution = AnalysisExecution(
      executionPlan(ResultDelivery.Collected, task = invalidReportTask)
    ).toOption.get
    assert(invalidExecution.collect.left.exists:
      case AnalysisExecutionError.InvalidTaskReport(
            id,
            TaskReportError.NegativeOperatorApplications(-1L)
          ) =>
        id.value == "alpha"
      case _ => false)

    val invalidTelemetry = AnalysisExecution(
      executionPlan(ResultDelivery.Collected, task = missingFallbackTask)
    ).toOption.get
    assert(invalidTelemetry.collect.left.exists:
      case AnalysisExecutionError.InvalidMeasurementReceipt(
            id,
            ExecutionReceiptError.UnrecordedFallback(_, _)
          ) =>
        id.value == "alpha"
      case _ => false)

  test("generic summaries are derived success-only views, not result storage"):
    val result = AnalysisExecution(
      executionPlan(ResultDelivery.Collected)
    ).toOption.get.collect.toOption.get
    given ResultView[Int, String] with
      override def apply(value: Int): String = s"classes=$value"

    val derived = result.view[String]
    assertEquals(derived.map(_.measurement.id.value), Vector("alpha"))
    assertEquals(derived.map(_.value), Vector("classes=2"))
    assertEquals(result.values.length, 3)

  test("result admission rejects values detached from the executor's ordered frame"):
    val execution = AnalysisExecution(
      executionPlan(ResultDelivery.Collected)
    ).toOption.get
    val result = execution.collect.toOption.get
    val detached = result.values.updated(
      0,
      result.values.head.copy(measurement = result.values(1).measurement)
    )

    assert(
      AnalysisResult
        .fromExecution(execution)(detached, result.counts, result.receipt)
        .left
        .exists:
          case AnalysisResultError.MeasurementMismatch(0, expected, actual) =>
            expected != actual
          case _ => false
    )

  test("count construction rejects every inconsistent state"):
    assert(TraversalCounts(2, 3, 3, 0, 0, TraversalCompletion.Exhausted).isLeft)
    assert(TraversalCounts(3, 2, 1, 0, 0, TraversalCompletion.StoppedEarly).isLeft)
    assert(TraversalCounts(3, 2, 1, 1, 0, TraversalCompletion.Exhausted).isLeft)
    assert(TraversalCounts(3, 3, 1, 1, 1, TraversalCompletion.StoppedEarly).isLeft)
