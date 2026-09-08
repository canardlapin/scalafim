package scalafim.fmri.mvpa

private[mvpa] object ExecutionPlanFixtures:
  import LabelCountCompiler.given
  import ScientificPlanFixtures.*

  val SharedBackend: BackendId = BackendId.unsafe("gale-shared")
  val DenseBackend: BackendId = BackendId.unsafe("gale-dense")

  val Solver: SolverIdentity =
    SolverIdentity(
      SolverId.unsafe("portable-reference"),
      Vector("tolerance" -> "1e-10", "version" -> "1")
    ).toOption.get

  def sourceWithLabels(
      labels: Vector[String] = Vector("face", "place", "face", "place")
  ): LabeledToySource =
    val raw = source()
    val target = Column(raw.samples, labels).toOption.get
    new LabeledToySource(raw.samples, raw.features, raw.identity, target)

  def bound(): BoundScientificPlan[
    LabeledToySource,
    ToyEvidenceDesign,
    LabelCountEstimand.type,
    NoRendition.type,
    PreparedLabelCount
  ] =
    val sourceValue = sourceWithLabels()
    val specification = ScientificSpecification(sourceValue)(
      design(sourceValue),
      frame(sourceValue),
      LabelCountEstimand
    ).toOption.get
    Mvpa.bind(specification).toOption.get

  def strategy(
      representation: ExecutionRepresentation,
      delivery: ResultDelivery,
      backend: BackendId = SharedBackend,
      solver: SolverChoice = SolverChoice.Selected(Solver),
      materialization: MaterializationPolicy = MaterializationPolicy.Reject,
      fallback: FallbackPolicy = FallbackPolicy.forbidden,
      scheduling: Scheduling = Scheduling.serial,
      streams: Vector[RandomStream] = Vector(
        RandomStream(RandomStreamId.unsafe("folds"), 91L),
        RandomStream(RandomStreamId.unsafe("solver"), 27L)
      ),
      fields: Vector[(String, String)] = Vector("kernel" -> "toy")
  ): ExecutionStrategy =
    ExecutionStrategy(
      backend,
      representation,
      NumericPrecision.Binary64,
      solver,
      streams,
      scheduling,
      materialization,
      fallback,
      delivery,
      fields
    ).toOption.get

  val UniversalTask: MeasurementTask[
    LabeledToySource,
    ToyEvidenceDesign,
    LabelCountEstimand.type,
    NoRendition.type,
    PreparedLabelCount
  ] =
    new MeasurementTask[
      LabeledToySource,
      ToyEvidenceDesign,
      LabelCountEstimand.type,
      NoRendition.type,
      PreparedLabelCount
    ]:
      override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
        Right(())

      override def execute(
          plan: BoundScientificPlan[
            LabeledToySource,
            ToyEvidenceDesign,
            LabelCountEstimand.type,
            NoRendition.type,
            PreparedLabelCount
          ]
      )(
          measurement: MeasurementEntry[
            plan.specification.source.Neural,
            plan.specification.source.NeuralKey,
            ?,
            NoRendition.type
          ],
          context: TaskContext
      ): Either[
        TaskReportError,
        TaskReport[plan.Result, plan.Rejection, plan.Failure]
      ] =
        TaskReport.success(
          plan.prepared.classCount,
          context.strategy.target,
          operatorApplications = 1L
        )

  def plan(
      strategyValue: ExecutionStrategy
  ): ExecutionPlan[
    LabeledToySource,
    ToyEvidenceDesign,
    LabelCountEstimand.type,
    NoRendition.type,
    PreparedLabelCount
  ] =
    ExecutionPlan(bound(), strategyValue)(using UniversalTask).toOption.get

class ExecutionPlanSuite extends munit.FunSuite:
  import ExecutionPlanFixtures.*

  test("dense, operator, fused, and sufficient-statistic strategies share one task contract"):
    val strategies =
      for
        representation <- ExecutionRepresentation.values.toVector
        delivery <- ResultDelivery.values.toVector
      yield strategy(representation, delivery)

    val plans = strategies.map(strategyValue => plan(strategyValue))
    assertEquals(plans.map(_.measurementCount).distinct, Vector(1))
    assertEquals(
      plans.map(_.scientific.identity.fingerprint).distinct,
      Vector(plans.head.scientific.identity.fingerprint)
    )
    assertEquals(plans.map(_.identity.fingerprint).distinct.length, strategies.length)

  test("backend and scheduling changes preserve science but change execution identity"):
    val serial = plan(strategy(ExecutionRepresentation.Operator, ResultDelivery.Streaming))
    val parallelScheduling = Scheduling
      .parallel(4, 2, CompletionOrder.AsCompleted)
      .toOption
      .get
    val parallel = plan(
      strategy(
        ExecutionRepresentation.Operator,
        ResultDelivery.Streaming,
        backend = DenseBackend,
        scheduling = parallelScheduling
      )
    )

    assertEquals(serial.scientific.identity, parallel.scientific.identity)
    assertNotEquals(serial.strategy.fingerprint, parallel.strategy.fingerprint)
    assertNotEquals(serial.identity.fingerprint, parallel.identity.fingerprint)

  test("strategy identity canonically orders fields and random streams"):
    val forward = strategy(
      ExecutionRepresentation.Operator,
      ResultDelivery.Collected,
      streams = Vector(
        RandomStream(RandomStreamId.unsafe("solver"), 27L),
        RandomStream(RandomStreamId.unsafe("folds"), 91L)
      ),
      fields = Vector("version" -> "1", "kernel" -> "toy")
    )
    val reverse = strategy(
      ExecutionRepresentation.Operator,
      ResultDelivery.Collected,
      streams = Vector(
        RandomStream(RandomStreamId.unsafe("folds"), 91L),
        RandomStream(RandomStreamId.unsafe("solver"), 27L)
      ),
      fields = Vector("kernel" -> "toy", "version" -> "1")
    )

    assertEquals(forward.fingerprint, reverse.fingerprint)
    assertEquals(forward.randomStreams.map(_.id.value), Vector("folds", "solver"))

  test("unsupported strategies fail before any numerical task"):
    val denseOnly = new MeasurementTask[
      LabeledToySource,
      ToyEvidenceDesign,
      LabelCountEstimand.type,
      NoRendition.type,
      PreparedLabelCount
    ]:
      override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
        if strategy.representation == ExecutionRepresentation.Dense then Right(())
        else
          Left(
            ExecutionPlanError.UnsupportedRepresentation(
              strategy.representation,
              Vector(ExecutionRepresentation.Dense)
            )
          )

      override def execute(
          plan: BoundScientificPlan[
            LabeledToySource,
            ToyEvidenceDesign,
            LabelCountEstimand.type,
            NoRendition.type,
            PreparedLabelCount
          ]
      )(
          measurement: MeasurementEntry[
            plan.specification.source.Neural,
            plan.specification.source.NeuralKey,
            ?,
            NoRendition.type
          ],
          context: TaskContext
      ): Either[
        TaskReportError,
        TaskReport[plan.Result, plan.Rejection, plan.Failure]
      ] =
        TaskReport.success(plan.prepared.classCount, context.strategy.target)

    val result = ExecutionPlan(
      bound(),
      strategy(ExecutionRepresentation.Operator, ResultDelivery.Collected)
    )(using denseOnly)

    assert(result.left.exists:
      case ExecutionPlanError.UnsupportedRepresentation(
            ExecutionRepresentation.Operator,
            Vector(ExecutionRepresentation.Dense)
          ) =>
        true
      case _ => false)

  test("malformed scheduling, streams, and fallback policies fail closed"):
    val target = ExecutionTarget(SharedBackend, ExecutionRepresentation.Dense)
    val duplicateStreams = Vector(
      RandomStream(RandomStreamId.unsafe("folds"), 1L),
      RandomStream(RandomStreamId.unsafe("folds"), 2L)
    )

    assert(Scheduling.parallel(0, 1, CompletionOrder.MeasurementOrder).isLeft)
    assert(Scheduling.parallel(1, 0, CompletionOrder.MeasurementOrder).isLeft)
    assert(FallbackPolicy.explicit(Vector.empty).isLeft)
    assert(FallbackPolicy.explicit(Vector(target, target)).isLeft)
    assert(
      ExecutionStrategy(
        SharedBackend,
        ExecutionRepresentation.Dense,
        NumericPrecision.Binary64,
        SolverChoice.NotApplicable,
        Vector.empty,
        Scheduling.serial,
        MaterializationPolicy.Reject,
        FallbackPolicy.explicit(Vector(target)).toOption.get,
        ResultDelivery.Collected
      ).left.exists:
        case ExecutionPlanError.FallbackTargetMatchesRequested(value) => value == target
        case _                                                        => false
    )
    assert(
      ExecutionStrategy(
        SharedBackend,
        ExecutionRepresentation.Dense,
        NumericPrecision.Binary64,
        SolverChoice.NotApplicable,
        duplicateStreams,
        Scheduling.serial,
        MaterializationPolicy.Reject,
        FallbackPolicy.forbidden,
        ResultDelivery.Collected
      ).left.exists:
        case ExecutionPlanError.DuplicateRandomStream(id) => id.value == "folds"
        case _                                            => false
    )
