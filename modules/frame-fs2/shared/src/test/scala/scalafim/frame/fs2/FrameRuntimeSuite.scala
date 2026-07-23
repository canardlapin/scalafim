package scalafim.frame.fs2

import cats.effect.Deferred
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import scalafim.frame.*

class FrameRuntimeSuite extends munit.FunSuite:
  type Input = (id: Int)

  private val schema = summon[SchemaDescriptor[Input]].schema

  private def storage[A](result: Either[StorageError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def reference: SourceRef =
    SourceRef.values("input", "input").fold(error => fail(error.message), identity)

  private def frame: Frame[Input] =
    Frame.values[Input](reference).fold(error => fail(error.message), identity)

  private def table(tracker: BufferTracker, values: Vector[Array[Int]]): Table[Input] =
    val batches = values.map: input =>
      storage:
        RecordBatch(
          schema,
          Vector(storage(ColumnArray.int32(input, tracker = tracker)))
        )
    storage(Table[Input](batches))

  test("stream scopes each batch and preserves source ownership"):
    val tracker = new BufferTracker
    val input = table(tracker, Vector(Array(1, 2), Array(3)))
    val runtime = FrameRuntime[IO](ReferenceSources.empty.bind(reference, input))

    runtime
      .stream(frame)
      .evalMap: batch =>
        IO:
          val column = storage(batch.column("id"))
          Vector.tabulate(batch.rowCount)(index => storage(column.scalar(index)))
      .compile
      .toVector
      .map(_.flatten)
      .flatMap: values =>
        IO:
          assertEquals(values, Vector(1, 2, 3).map(ScalarValue.Int32.apply))
          assertEquals(tracker.snapshot.activeOwners, 2)
          assertEquals(tracker.snapshot.activeViews, 2)
          input.close()
          assertEquals(tracker.snapshot.activeOwners, 0)
      .unsafeToFuture()

  test("early termination and cancellation release cursor and batch leases"):
    val tracker = new BufferTracker
    val input = table(tracker, Vector(Array(1), Array(2)))
    val runtime = FrameRuntime[IO](ReferenceSources.empty.bind(reference, input))

    val canceled = for
      started <- Deferred[IO, Unit]
      fiber <- runtime
        .stream(frame)
        .evalMap(_ => started.complete(()).void *> IO.never)
        .compile
        .drain
        .start
      _ <- started.get
      _ <- fiber.cancel
    yield ()

    (
      runtime.stream(frame).take(1).compile.drain *>
        IO(assertEquals(tracker.snapshot.activeViews, 2)) *>
        canceled *>
        IO:
          assertEquals(tracker.snapshot.activeOwners, 2)
          assertEquals(tracker.snapshot.activeViews, 2)
          input.close()
          assertEquals(tracker.snapshot.activeOwners, 0)
    ).unsafeToFuture()

  test("collect exposes Table only inside Resource scope"):
    val tracker = new BufferTracker
    val input = table(tracker, Vector(Array(1, 2), Array(3)))
    val runtime = FrameRuntime[IO](ReferenceSources.empty.bind(reference, input))
    var materialized: Option[Table[Input]] = None

    runtime
      .collect(frame)
      .use: output =>
        IO:
          materialized = Some(output)
          assertEquals(output.rowCount, 3L)
          assert(!output.isClosed)
      .flatMap: _ =>
        IO:
          assert(materialized.exists(_.isClosed))
          assertEquals(tracker.snapshot.activeOwners, 2)
          input.close()
          assertEquals(tracker.snapshot.activeOwners, 0)
      .unsafeToFuture()

  test("failed collection closes acquired output and source leases"):
    val tracker = new BufferTracker
    val input = table(tracker, Vector(Array(Int.MaxValue), Array(2)))
    val runtime = FrameRuntime[IO](ReferenceSources.empty.bind(reference, input))
    val query = frame.withColumn("next")(_.col("id") + Expr.literal(1))

    runtime
      .collect(query)
      .use(_ => IO.unit)
      .attempt
      .flatMap: attempted =>
        IO:
          attempted match
            case Left(ExecutionFailure(ExecutionError.IntegerOverflow(_, BinaryOperator.Add))) =>
              ()
            case other => fail(s"expected structured overflow failure, found $other")

          assertEquals(tracker.snapshot.activeOwners, 2)
          assertEquals(tracker.snapshot.activeViews, 2)
          input.close()
          assertEquals(tracker.snapshot.activeOwners, 0)
      .unsafeToFuture()

  test("physical explain names the selected backend and forbids fallback"):
    val tracker = new BufferTracker
    val input = table(tracker, Vector(Array(1)))
    val runtime = FrameRuntime[IO](ReferenceSources.empty.bind(reference, input))

    assertEquals(
      runtime.physicalExplain(frame),
      "ReferenceExecution(mode=streaming, blocking=none, estimatedRows=unknown, fallback=none)"
    )
    input.close()
