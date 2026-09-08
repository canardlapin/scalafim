package scalafim.fmri.mvpa

/** Simulates a downstream compilation unit that spoofs the library package. Package-qualified visibility is therefore
  * not an admission boundary: every attack below must fail because construction is companion-private or because the
  * public factory requires an already admitted plan/execution.
  */
class AdmissionSpoofSuite extends munit.FunSuite:
  test("same-package downstream code cannot admit a prepared scientific plan"):
    val factoryErrors = compileErrors("""
      import scalafim.fmri.mvpa.*

      def forge[
          S <: ScientificSource,
          D <: EvidenceDesign,
          E <: Estimand[S, D],
          R,
          P
      ](
          specification: ScientificSpecification[S, D, E, R],
          prepared: P
      ) = BoundScientificPlan.admitted(specification, prepared)
    """)
    val constructorErrors = compileErrors("""
      import scalafim.fmri.mvpa.*

      def forge[
          S <: ScientificSource,
          D <: EvidenceDesign,
          E <: Estimand[S, D],
          R,
          P
      ](
          specification: ScientificSpecification[S, D, E, R],
          prepared: P
      ) = new BoundScientificPlan(specification, prepared)
    """)

    assert(factoryErrors.nonEmpty, clue(factoryErrors))
    assert(factoryErrors.contains("admitted"), clue(factoryErrors))
    assert(constructorErrors.nonEmpty, clue(constructorErrors))

  test("same-package downstream code cannot construct results without an executor"):
    val errors = compileErrors("""
      import scalafim.fmri.mvpa.*

      def forge[A, R, F, Rendition](
          plan: ScientificPlanIdentity,
          values: Vector[MeasurementValue[A, R, F, Rendition]],
          counts: TraversalCounts,
          receipt: ExecutionReceipt
      ) = new AnalysisResult(plan, values, counts, receipt)
    """)

    assert(errors.nonEmpty, clue(errors))

  test("same-package downstream code cannot construct receipts without an admitted plan"):
    val measurementErrors = compileErrors("""
      import scalafim.fmri.mvpa.*

      def forge(
          execution: ExecutionPlanIdentity,
          strategy: ExecutionStrategy,
          measurement: MeasurementIdentity,
          actual: ExecutionTarget
      ) = MeasurementExecutionReceipt.fromTask(
        execution,
        strategy,
        measurement,
        0,
        MeasurementStatus.Succeeded,
        actual,
        0L,
        Vector.empty,
        Vector.empty,
        Vector.empty
      )
    """)
    val aggregateErrors = compileErrors("""
      import scalafim.fmri.mvpa.*

      def forge(
          execution: ExecutionPlanIdentity,
          requested: ExecutionTarget,
          work: ExecutionWork
      ) = new ExecutionReceipt(
        execution,
        requested,
        NumericPrecision.Binary64,
        SolverChoice.NotApplicable,
        Vector.empty,
        Scheduling.serial,
        ResultDelivery.Collected,
        Vector.empty,
        work
      )
    """)

    assert(measurementErrors.nonEmpty, clue(measurementErrors))
    assert(aggregateErrors.nonEmpty, clue(aggregateErrors))

  test("same-package downstream code cannot forge a column or materialized evidence"):
    val columnErrors = compileErrors("""
      import multivar.core.*
      import scalafim.fmri.mvpa.*

      def forge[S <: SemanticSpace, A](
          identity: AxisIdentity,
          rows: SpaceEvidence[S],
          values: IArray[A]
      ) = Column.fromOwned(identity, rows, values)
    """)
    val materializedErrors = compileErrors("""
      import gale.linalg.DMat
      import multivar.core.*
      import scalafim.fmri.mvpa.*

      def forge[S <: SemanticSpace, T <: SemanticSpace](
          rowIdentity: AxisIdentity,
          columnIdentity: AxisIdentity,
          rows: SpaceEvidence[S],
          columns: SpaceEvidence[T],
          value: DMat,
          receipt: MaterializationReceipt
      ) = new MaterializedEvidence(
        rowIdentity,
        columnIdentity,
        rows,
        columns,
        value,
        receipt
      )
    """)

    assert(columnErrors.nonEmpty, clue(columnErrors))
    assert(materializedErrors.nonEmpty, clue(materializedErrors))
