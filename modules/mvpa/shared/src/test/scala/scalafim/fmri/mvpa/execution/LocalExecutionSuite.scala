package scalafim.fmri.mvpa.analysis

import resample4s.core.Seed
import scalafim.fmri.mvpa.AxisSignature
import scalafim.fmri.mvpa.execution.*
import scalafim.fmri.mvpa.measurement.MeasurementId

class LocalExecutionSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private val plan: PlanId =
    PlanId.derived(
      EstimandId("execution-test"),
      Vector(AxisSignature.unsafe("a" * 64)),
      AxisSignature.unsafe("b" * 64),
      AxisSignature.unsafe("c" * 64),
      "source-v1", "design-v1", "frame-v1", "test", Vector.empty,
      Vector.empty, "sum", Vector.empty, Set.empty
    )

  private def address(split: Int, replicate: Int, stage: Int, measurement: String, coordinate: Int): WorkAddress =
    WorkAddress(
      plan,
      right(SplitCoordinate(split)),
      right(ReplicateCoordinate(replicate)),
      right(StageCoordinate(stage)),
      MeasurementId.unsafe(measurement),
      right(MeasurementCoordinate(coordinate))
    )

  private final class Resource(onClose: () => Unit) extends ExecutionResource:
    def close(): Unit = onClose()

  private final class TestWorkUnit(
      val address: WorkAddress,
      result: Seed => Either[UnitError, UnitEvaluation[Int, String]],
      resource: () => ExecutionResource
  ) extends WorkUnit[Int, String]:
    def acquire(): Either[UnitError, ExecutionResource] = Right(resource())
    def compute(resource: ExecutionResource, seed: Seed): Either[UnitError, UnitEvaluation[Int, String]] = result(seed)

  private val sum = new Reduction[Int, Int]:
    def empty: Int = 0
    def add(current: Int, contribution: Int): Int = current + contribution

  private def complete(address: WorkAddress, fingerprint: String, value: Int, row: String, resource: () => ExecutionResource = () => Resource(() => ())): WorkUnit[Int, String] =
    new TestWorkUnit(address, _ => Right(UnitEvaluation.Complete(UnitContribution(fingerprint, value, Vector(row)))), resource)

  test("addresses derive stable domain-separated streams regardless of scheduling order"):
    val first = complete(address(2, 1, 0, "right", 1), "right", 2, "row-right")
    val second = complete(address(0, 1, 0, "left", 0), "left", 1, "row-left")
    val root = Seed.fromLong(9182L)
    val direct = Vector(first, second).map(unit => unit.address -> WorkAddress.seed(root, unit.address)).toMap
    val reordered = Vector(second, first).map(unit => unit.address -> WorkAddress.seed(root, unit.address)).toMap
    assertEquals(direct, reordered)
    assertNotEquals(direct(first.address), direct(second.address))


  test("duplicate retry with the same contribution commits one reducer and OOF row"):
    val unit = complete(address(0, 0, 0, "whole", 0), "same", 4, "oof")
    val result = right(LocalExecution.run(Vector(unit), Seed.fromLong(7L), sum, attempts = Some(Vector(unit, unit))))
    result match
      case FamilyState.Complete(value) =>
        assertEquals(value.coverage.committed, 1)
        assertEquals(value.reduction, 4)
        assertEquals(value.outOfFold, Vector("oof"))
      case other => fail(s"expected completion, got $other")


  test("conflicting retry is terminal and never double-counts a contribution"):
    val location = address(0, 0, 0, "whole", 0)
    val first = complete(location, "first", 4, "oof-first")
    val conflicting = complete(location, "second", 99, "oof-second")
    val result = right(LocalExecution.run(Vector(first), Seed.fromLong(7L), sum, attempts = Some(Vector(first, conflicting))))
    result match
      case FamilyState.Failed(value, failures) =>
        assertEquals(value.coverage.committed, 1)
        assertEquals(value.committed.head.contribution.reduction, 4)
        assertEquals(failures.length, 1)
      case other => fail(s"expected conflicting retry failure, got $other")


  test("failure immediately before commit leaves no contribution and closes the owned resource once"):
    var computed = 0
    var closed = 0
    val unit = new TestWorkUnit(
      address(0, 0, 0, "whole", 0),
      _ =>
        computed += 1
        Right(UnitEvaluation.Complete(UnitContribution("computed", 3, Vector("oof")))) ,
      () => Resource(() => closed += 1)
    )
    val reject = new BeforeCommit:
      def check(address: WorkAddress): Either[UnitError, Unit] = Left(UnitError.BeforeCommit("injected"))
    val result = right(LocalExecution.run(Vector(unit), Seed.fromLong(1L), sum, beforeCommit = reject))
    result match
      case FamilyState.Failed(value, failures) =>
        assertEquals(computed, 1)
        assertEquals(closed, 1)
        assertEquals(value.coverage.committed, 0)
        assertEquals(value.coverage.missing, Vector(unit.address))
        assertEquals(failures, Vector(unit.address -> UnitError.BeforeCommit("injected")))
      case other => fail(s"expected pre-commit failure, got $other")


  test("partial and cancellation remain incomplete, and compute plus close failures are both preserved"):
    val partial = new TestWorkUnit(
      address(0, 0, 0, "partial", 0),
      _ => Right(UnitEvaluation.Partial("provider paused")),
      () => Resource(() => ())
    )
    right(LocalExecution.run(Vector(partial), Seed.fromLong(2L), sum)) match
      case FamilyState.Partial(value) => assertEquals(value.coverage.committed, 0)
      case other => fail(s"expected partial family, got $other")

    var closed = 0
    val broken = new TestWorkUnit(
      address(0, 0, 0, "broken", 0),
      _ => Left(UnitError.Compute("compute fault")),
      () => Resource(() => { closed += 1; throw IllegalStateException("close fault") })
    )
    right(LocalExecution.run(Vector(broken), Seed.fromLong(2L), sum)) match
      case FamilyState.Failed(_, Vector((_, UnitError.ComputeAndClose(UnitError.Compute("compute fault"), UnitError.Close("close fault"))))) =>
        assertEquals(closed, 1)
      case other => fail(s"expected preserved compute/close faults, got $other")

    val cancelled = right(LocalExecution.run(Vector(complete(address(0, 0, 0, "cancelled", 0), "x", 1, "x")), Seed.fromLong(2L), sum, cancellationRequested = () => true))
    cancelled match
      case FamilyState.Cancelled(value) => assertEquals(value.coverage.committed, 0)
      case other => fail(s"expected cancelled family, got $other")


  test("omitted expected units never form a complete family"):
    val a = complete(address(0,0,0,"a",0),"a",1,"a")
    val b = complete(address(0,0,0,"b",1),"b",2,"b")
    for attempts <- Vector(Vector.empty[WorkUnit[Int,String]],Vector(a)) do
      right(LocalExecution.run(Vector(a,b),Seed.fromLong(5L),sum,Some(attempts))) match
        case FamilyState.Partial(value) =>
          assertEquals(value.coverage.expected,2)
          assertEquals(value.coverage.committed,attempts.size)
          assertEquals(value.coverage.missing.size,2-attempts.size)
        case other => fail(s"missing units became complete: $other")

  test("identical declared fingerprint cannot hide a conflicting contribution"):
    val a = complete(address(0,0,0,"a",0),"same",1,"a")
    val changed = complete(a.address,"same",2,"changed-row")
    right(LocalExecution.run(Vector(a),Seed.fromLong(5L),sum,Some(Vector(a,changed)))) match
      case FamilyState.Failed(value,failures) =>
        assertEquals(value.committed.map(_.contribution.reduction),Vector(1))
        assertEquals(failures.size,1)
      case other => fail(s"conflicting payload was ignored: $other")

  test("plan identity and measurement identity separate complete stream paths"):
    val a = address(0,0,0,"a",0)
    val otherPlan = PlanId.derived(EstimandId("different"),Vector(AxisSignature.unsafe("a"*64)),
      AxisSignature.unsafe("b"*64),AxisSignature.unsafe("c"*64),"source","design","frame","q",
      Vector.empty,Vector.empty,"sum",Vector.empty,Set.empty)
    val root = Seed.fromLong(5L)
    assertNotEquals(WorkAddress.seed(root,a),WorkAddress.seed(root,a.copy(plan=otherPlan)))
    assertNotEquals(WorkAddress.seed(root,a),WorkAddress.seed(root,a.copy(measurement=MeasurementId.unsafe("b"))))
    val first = complete(a,"a",1,"a")
    val bad = complete(a.copy(measurement=MeasurementId.unsafe("b")),"b",2,"b")
    assert(LocalExecution.run(Vector(first,bad),root,sum).isLeft)

  test("explicit schedule reorder preserves canonical reducer and OOF order"):
    val a = complete(address(0,0,0,"a",0),"a",1,"row-a")
    val b = complete(address(0,0,0,"b",1),"b",2,"row-b")
    val forward = right(LocalExecution.run(Vector(a,b),Seed.fromLong(5L),sum,Some(Vector(a,b))))
    val reverse = right(LocalExecution.run(Vector(a,b),Seed.fromLong(5L),sum,Some(Vector(b,a))))
    assertEquals(forward,reverse)

  test("cancellation callback failure is typed and acquires no resources"):
    var acquired = 0
    val a = complete(address(0,0,0,"a",0),"a",1,"a",() =>
      acquired += 1
      Resource(() => ()))
    right(LocalExecution.run(Vector(a),Seed.fromLong(5L),sum,cancellationRequested=() => throw IllegalStateException("control fault"))) match
      case FamilyState.Failed(_,failures) => assertEquals(failures,Vector(a.address -> UnitError.Control("control fault")))
      case other => fail(s"unexpected callback failure: $other")
    assertEquals(acquired,0)

  test("reduction faults retain committed coverage including an empty family"):
    val a = complete(address(0,0,0,"a",0),"a",1,"a")
    val emptyFault = new Reduction[Int,Int]:
      def empty: Int = throw IllegalStateException("empty-fault")
      def add(current: Int, contribution: Int): Int = current + contribution
    val addFault = new Reduction[Int,Int]:
      def empty: Int = 0
      def add(current: Int, contribution: Int): Int = throw IllegalStateException("add-fault")
    right(LocalExecution.run(Vector.empty[WorkUnit[Int,String]],Seed.fromLong(1L),emptyFault)) match
      case FamilyState.ReductionFailed(value,"empty-fault") => assertEquals(value.coverage.committed,0)
      case other => fail(s"unexpected empty reduction result: $other")
    right(LocalExecution.run(Vector(a),Seed.fromLong(1L),addFault)) match
      case FamilyState.ReductionFailed(value,"add-fault") => assertEquals(value.coverage.committed,1)
      case other => fail(s"unexpected reduction result: $other")

  test("a close-failing retry cannot be healed by an earlier or later commit"):
    val a = complete(address(0,0,0,"a",0),"a",1,"a")
    val broken = complete(a.address,"a",1,"a",() => Resource(() => throw IllegalStateException("close-fault")))
    for attempts <- Vector(Vector(a,broken),Vector(broken,a)) do
      right(LocalExecution.run(Vector(a),Seed.fromLong(1L),sum,Some(attempts))) match
        case FamilyState.Failed(_,Vector((_,UnitError.Close("close-fault")))) => ()
        case other => fail(s"close fault disappeared: $other")

  test("conflict survives a later cancellation and stops further work"):
    val a = complete(address(0,0,0,"a",0),"a",1,"a")
    val conflict = complete(a.address,"a",2,"changed")
    val b = complete(address(0,0,0,"b",1),"b",2,"b")
    var calls = 0
    val result = right(LocalExecution.run(Vector(a,b),Seed.fromLong(1L),sum,Some(Vector(a,conflict,b)),() =>
      calls += 1
      calls >= 3
    ))
    assertEquals(calls,2)
    result match
      case FamilyState.Failed(value,Vector((_,UnitError.BeforeCommit(_)))) => assertEquals(value.coverage.committed,1)
      case other => fail(s"conflict disappeared: $other")

  test("throwing acquire and before-commit preserve their exact typed cause"):
    val a = complete(address(0,0,0,"a",0),"a",1,"a",() => throw IllegalStateException("acquire-fault"))
    right(LocalExecution.run(Vector(a),Seed.fromLong(1L),sum)) match
      case FamilyState.Failed(_,Vector((_,UnitError.Acquire("acquire-fault")))) => ()
      case other => fail(s"lost acquire fault: $other")
    val good = complete(a.address,"a",1,"a")
    val before = new BeforeCommit:
      def check(address: WorkAddress): Either[UnitError,scala.Unit] = throw IllegalStateException("before-fault")
    right(LocalExecution.run(Vector(good),Seed.fromLong(1L),sum,beforeCommit = before)) match
      case FamilyState.Failed(_,Vector((_,UnitError.BeforeCommit("before-fault")))) => ()
      case other => fail(s"lost before-commit fault: $other")
