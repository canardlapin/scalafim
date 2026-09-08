package scalafim.fmri.mvpa

import multivar.core.ValueId

class ExecutionReceiptSuite extends munit.FunSuite:
  import ExecutionPlanFixtures.*

  private type ToyPlan = ExecutionPlan[
    LabeledToySource,
    ToyEvidenceDesign,
    LabelCountEstimand.type,
    NoRendition.type,
    PreparedLabelCount
  ]

  private def materializationReceipt(
      budget: MaterializationBudget = MaterializationBudget.unsafe(64L)
  ): MaterializationReceipt =
    val sourceValue = sourceWithLabels()
    val matrix = GaleTestMatrix.fromRows(
      Vector.tabulate(sourceValue.samples.size): row =>
        Vector.tabulate(sourceValue.features.size): column =>
          (row * sourceValue.features.size + column + 1).toDouble
    )
    EvidenceTable
      .dense(
        sourceValue.samples,
        sourceValue.features,
        matrix,
        ValueId.unsafe("receipt-fixture")
      )
      .toOption
      .get
      .materialize(MaterializationPolicy.Allow(budget))
      .toOption
      .get
      .receipt

  private def work(
      planned: Int = 1,
      attempted: Int = 1,
      succeeded: Int = 1,
      rejected: Int = 0,
      failed: Int = 0,
      operatorApplications: Long = 0L,
      materializedCells: Long = 0L
  ): ExecutionWork =
    ExecutionWork(
      planned,
      attempted,
      succeeded,
      rejected,
      failed,
      operatorApplications,
      materializedCells
    ).toOption.get

  private def measurementReceipt(
      planValue: ToyPlan,
      status: MeasurementStatus = MeasurementStatus.Succeeded,
      actual: Option[ExecutionTarget] = None,
      operatorApplications: Long = 0L,
      materializations: Vector[ExecutionMaterialization] = Vector.empty,
      fallbacks: Vector[FallbackReceipt] = Vector.empty,
      convergence: Vector[IterativeConvergence] = Vector.empty
  ): Either[ExecutionReceiptError, MeasurementExecutionReceipt] =
    MeasurementExecutionReceipt.fromTask(
      planValue,
      ordinal = 0,
      status,
      actual.getOrElse(planValue.strategy.target),
      operatorApplications,
      materializations,
      fallbacks,
      convergence
    )

  test("receipt records per-measurement backend, materialization, solver work, convergence, and fallback"):
    val denseTarget = ExecutionTarget(DenseBackend, ExecutionRepresentation.Dense)
    val strategyValue = strategy(
      ExecutionRepresentation.Operator,
      ResultDelivery.Streaming,
      materialization = MaterializationPolicy.Allow(MaterializationBudget.unsafe(64L)),
      fallback = FallbackPolicy.explicit(Vector(denseTarget)).toOption.get
    )
    val planValue = plan(strategyValue)
    val materialization = ExecutionMaterialization(
      ExecutionScope.Measurement(MeasurementId.unsafe("language-roi")),
      materializationReceipt(),
      "operator backend requested an explicit dense solve"
    ).toOption.get
    val fallback = FallbackReceipt(
      strategyValue.target,
      denseTarget,
      "operator solver capability unavailable"
    ).toOption.get
    val convergence = IterativeConvergence(
      ExecutionScope.Measurement(MeasurementId.unsafe("language-roi")),
      Solver,
      ConvergenceOutcome.Converged,
      iterations = 12,
      residualNorm = 1e-9
    ).toOption.get
    val measurement = measurementReceipt(
      planValue,
      actual = Some(denseTarget),
      operatorApplications = 7L,
      materializations = Vector(materialization),
      fallbacks = Vector(fallback),
      convergence = Vector(convergence)
    ).toOption.get
    val receipt = ExecutionReceipt
      .fromTraversal(
        planValue,
        Vector(measurement),
        work(operatorApplications = 7L, materializedCells = materialization.cells)
      )
      .toOption
      .get

    assertEquals(receipt.execution, planValue.identity)
    assertEquals(receipt.requested, strategyValue.target)
    assertEquals(receipt.actualTargets, Vector(denseTarget))
    assertEquals(receipt.precision, NumericPrecision.Binary64)
    assertEquals(receipt.solver, SolverChoice.Selected(Solver))
    assertEquals(receipt.randomStreams.map(_.id.value), Vector("folds", "solver"))
    assertEquals(receipt.scheduling, Scheduling.serial)
    assertEquals(receipt.delivery, ResultDelivery.Streaming)
    assertEquals(receipt.work.materializedCells, 16L)
    assertEquals(receipt.convergence.map(_.iterations), Vector(12))

  test("densification and fallback can never be silent"):
    val denseTarget = ExecutionTarget(DenseBackend, ExecutionRepresentation.Dense)
    val strictPlan = plan(
      strategy(ExecutionRepresentation.Operator, ResultDelivery.Collected)
    )
    val materialization = ExecutionMaterialization(
      ExecutionScope.WholePlan,
      materializationReceipt(),
      "explicit test materialization"
    ).toOption.get
    val authorizedPlan = plan(
      strategy(
        ExecutionRepresentation.Operator,
        ResultDelivery.Collected,
        fallback = FallbackPolicy.explicit(Vector(denseTarget)).toOption.get
      )
    )
    val fallback = FallbackReceipt(
      authorizedPlan.strategy.target,
      denseTarget,
      "explicit test fallback"
    ).toOption.get

    assert(measurementReceipt(strictPlan, actual = Some(denseTarget)).left.exists:
      case ExecutionReceiptError.UnrecordedFallback(expected, actual) =>
        expected == strictPlan.strategy.target && actual == denseTarget
      case _ => false)
    assert(
      measurementReceipt(
        strictPlan,
        materializations = Vector(materialization)
      ).left.exists:
        case ExecutionReceiptError.MaterializationForbidden(ExecutionScope.WholePlan) => true
        case _                                                                        => false
    )
    assert(
      measurementReceipt(
        authorizedPlan,
        actual = Some(denseTarget),
        fallbacks = Vector(fallback)
      ).isRight
    )

  test("receipt rejects unpermitted and broken fallback chains"):
    val denseTarget = ExecutionTarget(DenseBackend, ExecutionRepresentation.Dense)
    val fusedTarget = ExecutionTarget(SharedBackend, ExecutionRepresentation.Fused)
    val planValue = plan(
      strategy(
        ExecutionRepresentation.Operator,
        ResultDelivery.Collected,
        fallback = FallbackPolicy.explicit(Vector(denseTarget)).toOption.get
      )
    )
    val unpermitted = FallbackReceipt(
      planValue.strategy.target,
      fusedTarget,
      "unpermitted test target"
    ).toOption.get
    val wrongStart = FallbackReceipt(
      fusedTarget,
      denseTarget,
      "broken test chain"
    ).toOption.get

    assert(
      measurementReceipt(
        planValue,
        actual = Some(fusedTarget),
        fallbacks = Vector(unpermitted)
      ).left.exists:
        case ExecutionReceiptError.FallbackNotPermitted(target) => target == fusedTarget
        case _                                                  => false
    )
    assert(
      measurementReceipt(
        planValue,
        actual = Some(denseTarget),
        fallbacks = Vector(wrongStart)
      ).left.exists:
        case ExecutionReceiptError.BrokenFallbackChain(expected, actual) =>
          expected == planValue.strategy.target && actual == fusedTarget
        case _ => false
    )

  test("aggregate work and convergence evidence enforce exact invariants"):
    assert(ExecutionWork(1, 1, 1, 1, 0, 0L, 0L).isLeft)
    assert(ExecutionWork(1, 2, 2, 0, 0, 0L, 0L).isLeft)
    assert(
      IterativeConvergence(
        ExecutionScope.WholePlan,
        Solver,
        ConvergenceOutcome.Converged,
        iterations = -1,
        residualNorm = Double.NaN
      ).isLeft
    )

    val planValue = plan(
      strategy(ExecutionRepresentation.Operator, ResultDelivery.Collected)
    )
    val measurement = measurementReceipt(planValue).toOption.get
    assert(
      ExecutionReceipt
        .fromTraversal(
          planValue,
          Vector(measurement),
          work(planned = 2)
        )
        .left
        .exists:
          case ExecutionReceiptError.PlannedWorkMismatch(1, 2) => true
          case _                                               => false
    )

    val noSolverPlan = plan(
      strategy(
        ExecutionRepresentation.Dense,
        ResultDelivery.Collected,
        solver = SolverChoice.NotApplicable
      )
    )
    val convergence = IterativeConvergence(
      ExecutionScope.WholePlan,
      Solver,
      ConvergenceOutcome.Converged,
      iterations = 1,
      residualNorm = 0.0
    ).toOption.get
    assert(
      measurementReceipt(
        noSolverPlan,
        convergence = Vector(convergence)
      ).left.exists(_ == ExecutionReceiptError.UnexpectedConvergence)
    )

    assert(
      MeasurementExecutionReceipt
        .fromTask(
          noSolverPlan,
          ordinal = noSolverPlan.measurementCount,
          MeasurementStatus.Succeeded,
          noSolverPlan.strategy.target,
          0L,
          Vector.empty,
          Vector.empty,
          Vector.empty
        )
        .left
        .exists:
          case ExecutionReceiptError.ReceiptOrdinalOutOfBounds(position, count) =>
            position == count
          case _ => false
    )

  test("materialization work and budget must agree with evidence receipts"):
    val materialization = ExecutionMaterialization(
      ExecutionScope.WholePlan,
      materializationReceipt(),
      "budget consistency court"
    ).toOption.get
    val malformed = ExecutionMaterialization(
      ExecutionScope.WholePlan,
      materializationReceipt().copy(elements = 15L),
      "malformed shape court"
    ).toOption.get
    val smallBudgetPlan = plan(
      strategy(
        ExecutionRepresentation.Operator,
        ResultDelivery.Collected,
        materialization = MaterializationPolicy.Allow(MaterializationBudget.unsafe(8L))
      )
    )
    assert(
      measurementReceipt(
        smallBudgetPlan,
        materializations = Vector(materialization)
      ).left.exists:
        case ExecutionReceiptError.MaterializationBudgetExceeded(
              ExecutionScope.WholePlan,
              16L,
              8L
            ) =>
          true
        case _ => false
    )
    assert(
      measurementReceipt(
        smallBudgetPlan,
        materializations = Vector(malformed)
      ).left.exists:
        case ExecutionReceiptError.InvalidMaterializationReceipt(4, 4, 15L) => true
        case _                                                              => false
    )

    val allowedPlan = plan(
      strategy(
        ExecutionRepresentation.Operator,
        ResultDelivery.Collected,
        materialization = MaterializationPolicy.Allow(MaterializationBudget.unsafe(64L))
      )
    )
    val measurement = measurementReceipt(
      allowedPlan,
      materializations = Vector(materialization)
    ).toOption.get
    assert(
      ExecutionReceipt
        .fromTraversal(
          allowedPlan,
          Vector(measurement),
          work(materializedCells = 0L)
        )
        .left
        .exists:
          case ExecutionReceiptError.MaterializedWorkMismatch(16L, 0L) => true
          case _                                                       => false
    )
