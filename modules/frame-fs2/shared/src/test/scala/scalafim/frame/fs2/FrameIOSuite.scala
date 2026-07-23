package scalafim.frame.fs2

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import scalafim.frame.*

class FrameIOSuite extends munit.FunSuite:
  type Input = (id: Int, label: String, score: Option[Double])

  private val schema = summon[SchemaDescriptor[Input]].schema

  private def storage[A](result: Either[StorageError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def batch(tracker: BufferTracker): RecordBatch =
    storage:
      RecordBatch(
        schema,
        Vector(
          storage(ColumnArray.int32(Array(1, 2, 3), tracker = tracker)),
          storage(ColumnArray.utf8(Array("a", "b", "c"), tracker = tracker)),
          storage(
            ColumnArray.float64(
              Array(1.5, 0.0, Double.NaN),
              Array(true, false, true),
              tracker
            )
          )
        )
      )

  private def scalar(batch: RecordBatch, name: String): Vector[ScalarValue] =
    val column = storage(batch.column(name))
    Vector.tabulate(batch.rowCount)(index => storage(column.scalar(index)))

  test("in-memory scans accept supported pushdown and preserve unsupported residuals"):
    val tracker = new BufferTracker
    val input = batch(tracker)
    val source = InMemoryFrameSource[IO](schema, Vector(input))
    val request = ScanRequest(
      columns = Vector("label"),
      predicate = Some(PortablePredicate.IsNull("score")),
      limit = Some(2),
      batchSize = Some(1)
    )

    source
      .plan(request)
      .flatMap:
        case Left(error) => IO(fail(error.message))
        case Right(scan) =>
          scan.batches
            .evalMap(batch => IO(scalar(batch, "label")))
            .compile
            .toVector
            .map(_.flatten)
            .flatMap: labels =>
            IO:
              assertEquals(scan.schema.fields.map(_.name), Vector("label"))
              assertEquals(
                scan.receipt.accepted,
                Vector(PushdownFeature.Projection, PushdownFeature.Limit)
              )
              assertEquals(
                scan.receipt.residual,
                Vector(PushdownFeature.Predicate, PushdownFeature.BatchSize)
              )
              assertEquals(labels, Vector("a", "b").map(ScalarValue.Utf8.apply))
      .flatMap: _ =>
        IO:
          assertEquals(tracker.snapshot.activeOwners, 5)
          assertEquals(tracker.snapshot.activeViews, 5)
          input.close()
          assertEquals(tracker.snapshot.activeOwners, 0)
      .unsafeToFuture()

  test("CSV ingestion and writing preserve explicit schema, nulls, quotes, and NaN"):
    val input =
      """id,label,score
        |1,"a,b",1.5
        |2,missing,
        |3,nan,NaN
        |""".stripMargin
    val options = CsvReadOptions(schema, batchSize = 2)
    val sink = new CsvFrameSink[IO]()
    var acquired: Option[CsvFrameSource[IO]] = None

    CsvFrameSource
      .resource[IO](input, options)
      .use: source =>
        acquired = Some(source)
        source
          .plan(ScanRequest())
          .flatMap:
            case Left(error) => IO(fail(error.message))
            case Right(scan) =>
              scan.batches
                .evalMap: batch =>
                  IO((batch.rowCount, scalar(batch, "label"), scalar(batch, "score")))
                .compile
                .toVector
                .flatMap: observed =>
                IO:
                  assertEquals(observed.map(_._1), Vector(2, 1))
                  assertEquals(
                    observed.flatMap(_._2),
                    Vector("a,b", "missing", "nan").map(ScalarValue.Utf8.apply)
                  )
                  assertEquals(
                    observed.flatMap(_._3).take(2),
                    Vector(ScalarValue.Float64(1.5), ScalarValue.Null)
                  )
                *> source.plan(ScanRequest()).flatMap:
                  case Left(error) => IO(fail(error.message))
                  case Right(writeScan) =>
                    sink.write(schema, writeScan.batches).flatMap:
                      case Left(error) => IO(fail(error.message))
                      case Right(result) =>
                        IO:
                          assertEquals(result.receipt.rows, 3L)
                          assert(result.text.contains("\"a,b\""))
                          assert(result.text.contains("3,nan,NaN"))
      .flatMap: _ =>
        acquired.get.inspect.map:
          case Left(SourceError.Open(_)) => ()
          case other => fail(s"expected finalized CSV source, found $other")
      .unsafeToFuture()

  test("CSV decoding failures are structured and row-addressed"):
    CsvFrameSource
      .resource[IO](
        "id,label,score\nnot-an-int,a,1.0\n",
        CsvReadOptions(schema)
      )
      .use(_ => IO.unit)
      .attempt
      .map:
        case Left(SourceFailure(SourceError.Decode(2, 1, "not-an-int", DataType.Int32))) =>
          ()
        case other => fail(s"expected structured CSV decode error, found $other")
      .unsafeToFuture()
