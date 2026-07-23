package scalafim.frame.fs2

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import scalafim.frame.*

class ArrowIpcSuite extends munit.FunSuite:
  type Input = (
    flag: Option[Boolean],
    i32: Int,
    i64: Long,
    f32: Float,
    f64: Option[Double],
    text: String,
    time: TimestampMicros
  )

  private val schema = summon[SchemaDescriptor[Input]].schema

  private def storage[A](result: Either[StorageError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def batch: RecordBatch =
    storage:
      RecordBatch(
        schema,
        Vector(
          storage(ColumnArray.bool(Array(true, false), Array(true, false))),
          storage(ColumnArray.int32(Array(1, 2))),
          storage(ColumnArray.int64(Array(3L, 4L))),
          storage(ColumnArray.float32(Array(1.25f, Float.NaN))),
          storage(ColumnArray.float64(Array(2.5, 0.0), Array(true, false))),
          storage(ColumnArray.utf8(Array("arrow", "λ"))),
          storage(
            ColumnArray.timestamp(
              Array(1000L, 2000L),
              TimeUnit.Microsecond
            )
          )
        )
      )

  private def scalars(batch: RecordBatch, name: String): Vector[ScalarValue] =
    val column = storage(batch.column(name))
    Vector.tabulate(batch.rowCount)(index => storage(column.scalar(index)))

  test("Arrow IPC stream roundtrip preserves schema, values, nulls, and timestamp units"):
    val input = batch
    val sink = new ArrowIpcFrameSink[IO]()
    val written = sink
      .write(schema, Stream.emit(input))
      .unsafeRunSync()
      .fold(error => fail(error.message), identity)
    var acquired: Option[ArrowIpcFrameSource[IO]] = None
    val observed = ArrowIpcFrameSource
      .resource[IO](written.bytes)
      .use: source =>
        acquired = Some(source)
        source
          .plan(ScanRequest())
          .flatMap:
            case Left(error) => IO(fail(error.message))
            case Right(scan) =>
              scan.batches
                .evalMap: output =>
                  IO:
                    (
                      scalars(output, "flag"),
                      scalars(output, "f32"),
                      scalars(output, "f64"),
                      scalars(output, "text"),
                      scalars(output, "time")
                    )
                .compile
                .lastOrError
      .unsafeRunSync()

    assertEquals(written.receipt.rows, 2L)
    assert(written.bytes.nonEmpty)
    assertEquals(observed._1, Vector(ScalarValue.Bool(true), ScalarValue.Null))
    observed._2.last match
      case ScalarValue.Float32(value) => assert(value.isNaN)
      case other => fail(s"expected Float32 NaN, found $other")
    assertEquals(observed._3, Vector(ScalarValue.Float64(2.5), ScalarValue.Null))
    assertEquals(observed._4, Vector("arrow", "λ").map(ScalarValue.Utf8.apply))
    assertEquals(
      observed._5,
      Vector(
        ScalarValue.Timestamp(1000L, TimeUnit.Microsecond),
        ScalarValue.Timestamp(2000L, TimeUnit.Microsecond)
      )
    )

    acquired.get.inspect.unsafeRunSync() match
      case Left(SourceError.Open(_)) => ()
      case other => fail(s"expected finalized Arrow source, found $other")
    input.close()

  test("malformed Arrow IPC acquisition is structured and releases native resources"):
    val malformed = Array[Byte](1, 2, 3, 4)
    var attempt = 0
    while attempt < 20 do
      ArrowIpcFrameSource
        .resource[IO](malformed)
        .use(_ => IO.unit)
        .attempt
        .unsafeRunSync() match
          case Left(SourceFailure(SourceError.Open(detail))) => assert(detail.nonEmpty)
          case other => fail(s"expected structured Arrow open failure, found $other")
      attempt += 1
