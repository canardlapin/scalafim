package external.consumer

import multivar.core.SpaceRole
import resample4s.core.Seed
import scalafim.fmri.mvpa.AxisRef
import scalafim.fmri.mvpa.analysis.*
import scalafim.fmri.mvpa.execution.*
import scalafim.fmri.mvpa.measurement.MeasurementId
import scalafim.fmri.mvpa.pipeline.PipelineInterpreter

/** Uses public constructors across the optional module boundary. */
class PipelineInterpreterSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private val axis = right(AxisRef.fromStableKeys(
    "pipeline-consumer", SpaceRole.Observed, Vector("a", "b"), "samples", "one", "raw"
  ))

  private given ScientificInputs[Unit, Unit, Unit] with
    def sourceAxes(source: Unit) = Vector(axis.descriptor.coordinateSignature)
    def sourceIdentity(source: Unit) = "consumer-source-v1"
    def sourceCapabilities(source: Unit) = CapabilitySet.empty
    def designAxis(design: Unit) = axis.descriptor.coordinateSignature
    def designIdentity(design: Unit) = "consumer-design-v1"
    def frameAxis(frame: Unit) = axis.descriptor.coordinateSignature
    def frameIdentity(frame: Unit) = "consumer-frame-v1"

  private object SumEstimand extends Estimand[Unit, Unit, Unit]:
    type Result = Int
    type Rejection = String
    type Failure = String
    val id = EstimandId("pipeline-consumer-sum")
    val requiredCapabilities = Set.empty[CapabilityId]

  private val plan = AnalysisSpecification.from(
    (), (), (), SumEstimand, "sum work contributions", Vector.empty, Vector.empty, "sum"
  ).plan

  private val sum = new Reduction[Int, Int]:
    def empty: Int = 0
    def add(current: Int, contribution: Int): Int = current + contribution

  private final class Fixture:
    var events = Vector.empty[String]

    def work(index: Int, computeFailure: Boolean = false, closeFailure: Boolean = false): WorkUnit[Int, Seed] =
      new WorkUnit[Int, Seed]:
        val address = WorkAddress(
          plan, right(SplitCoordinate(0)), right(ReplicateCoordinate(0)),
          right(StageCoordinate(0)), right(MeasurementId(s"unit-$index")),
          right(MeasurementCoordinate(index))
        )
        def acquire(): Either[UnitError, ExecutionResource] =
          events :+= s"acquire-$index"
          Right(new ExecutionResource:
            def close(): Unit =
              events :+= s"close-$index"
              if closeFailure then throw IllegalStateException("close-fault")
          )
        def compute(resource: ExecutionResource, seed: Seed): Either[UnitError, UnitEvaluation[Int, Seed]] =
          events :+= s"compute-$index"
          if computeFailure then throw IllegalStateException("compute-fault")
          else Right(UnitEvaluation.Complete(UnitContribution(s"unit-$index", index + 1, Vector(seed))))

  test("pipeline interprets the supplied evaluation once and preserves typed refusal"):
    var calls = 0
    val expected: Either[UnitError, UnitEvaluation[Int, Seed]] = Left(UnitError.Compute("refused"))
    assertEquals(PipelineInterpreter.evaluate(() =>
      calls += 1
      expected
    ), expected)
    assertEquals(calls, 1)

  test("pipeline and direct execution preserve retry order, seeds, commits and cleanup"):
    def run(interpreter: UnitInterpreter) =
      val fixture = new Fixture
      val a = fixture.work(0)
      val b = fixture.work(1)
      val result = right(LocalExecution.run(
        Vector(a, b), Seed.fromLong(31), sum, Some(Vector(b, a, b)), interpreter = interpreter
      ))
      result match
        case FamilyState.Complete(value) =>
          assertEquals(value.reduction, 3)
          assertEquals(value.coverage, Coverage(2, 2, Vector.empty))
          assertEquals(value.committed.map(_.address), Vector(a.address, b.address))
          assertEquals(value.outOfFold, Vector(WorkAddress.seed(Seed.fromLong(31), a.address), WorkAddress.seed(Seed.fromLong(31), b.address)))
        case other => fail(s"expected complete family: $other")
      assertEquals(fixture.events, Vector("acquire-1", "compute-1", "close-1", "acquire-0", "compute-0", "close-0", "acquire-1", "compute-1", "close-1"))
      result
    assertEquals(run(PipelineInterpreter), run(UnitInterpreter.direct))

  test("pipeline preserves compute and close faults and stops before the next unit"):
    for interpreter <- Vector(UnitInterpreter.direct, PipelineInterpreter) do
      val fixture = new Fixture
      val broken = fixture.work(0, computeFailure = true, closeFailure = true)
      var commitChecks = 0
      val before = new BeforeCommit:
        def check(address: WorkAddress): Either[UnitError, Unit] =
          commitChecks += 1
          Right(())
      right(LocalExecution.run(Vector(broken, fixture.work(1)), Seed.fromLong(2), sum,
        beforeCommit = before, interpreter = interpreter)) match
        case FamilyState.Failed(value, failures) =>
          assertEquals(value.coverage.committed, 0)
          assertEquals(failures, Vector(broken.address -> UnitError.ComputeAndClose(
            UnitError.Compute("compute-fault"), UnitError.Close("close-fault")
          )))
        case other => fail(s"lost failure: $other")
      assertEquals(fixture.events, Vector("acquire-0", "compute-0", "close-0"))
      assertEquals(commitChecks, 0)

  test("pipeline retries after pre-commit refusal without duplicating a contribution"):
    val fixture = new Fixture
    val unit = fixture.work(0)
    var checks = 0
    val before = new BeforeCommit:
      def check(address: WorkAddress): Either[UnitError, Unit] =
        assertEquals(fixture.events.last, "close-0")
        checks += 1
        if checks == 1 then Left(UnitError.BeforeCommit("injected")) else Right(())
    right(LocalExecution.run(Vector(unit), Seed.fromLong(5), sum, Some(Vector(unit, unit, unit)),
      beforeCommit = before, interpreter = PipelineInterpreter)) match
      case FamilyState.Complete(value) =>
        assertEquals(value.reduction, 1)
        assertEquals(value.outOfFold.size, 1)
        assertEquals(value.coverage, Coverage(1, 1, Vector.empty))
      case other => fail(s"retry failed: $other")
    assertEquals(checks, 3)
    assertEquals(fixture.events.count(_ == "close-0"), 3)

  test("pipeline cancellation preserves committed coverage without acquiring remaining work"):
    val fixture = new Fixture
    val a = fixture.work(0)
    val b = fixture.work(1)
    var checks = 0
    right(LocalExecution.run(Vector(a, b), Seed.fromLong(8), sum,
      cancellationRequested = () =>
        checks += 1
        checks == 2,
      interpreter = PipelineInterpreter
    )) match
      case FamilyState.Cancelled(value) =>
        assertEquals(value.coverage, Coverage(2, 1, Vector(b.address)))
      case other => fail(s"lost cancellation: $other")
    assertEquals(fixture.events, Vector("acquire-0", "compute-0", "close-0"))

  test("pipeline infrastructure failures are typed interpreter failures"):
    PipelineInterpreter.evaluate[Int, Seed](() => throw IllegalStateException("interpreter-fault")) match
      case Left(UnitError.Interpreter(detail)) => assert(detail.contains("interpreter-fault"))
      case other => fail(s"lost interpreter failure: $other")
