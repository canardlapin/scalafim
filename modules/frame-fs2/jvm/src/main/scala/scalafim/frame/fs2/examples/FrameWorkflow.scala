package scalafim.frame.fs2.examples

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Resource
import fs2.Stream
import scalafim.frame.*
import scalafim.frame.fs2.*

object FrameWorkflow extends IOApp.Simple:
  type People = (id: Int, team: String, score: Option[Double])
  type Teams = (team: String, region: String)
  type Result = (region: String, n: Long, meanScore: Option[Double])
  type LeftResult = (id: Int, region: Option[String])

  private val peopleSchema = summon[SchemaDescriptor[People]].schema
  private val teamsSchema = summon[SchemaDescriptor[Teams]].schema
  private val peopleRef = SourceRef.values("people", "people").toOption.get
  private val teamsRef = SourceRef.values("teams", "teams").toOption.get

  private def storage[A](result: Either[StorageError, A]): A =
    result.fold(error => throw new IllegalArgumentException(error.message), identity)

  private def frame[A](result: Either[FrameError, A]): A =
    result.fold(error => throw new IllegalArgumentException(error.message), identity)

  private def inputs: (Table[People], Table[Teams]) =
    val peopleBatch = storage:
      RecordBatch(
        peopleSchema,
        Vector(
          storage(ColumnArray.int32(Array(1, 2, 3))),
          storage(ColumnArray.utf8(Array("a", "b", "a"))),
          storage(
            ColumnArray.float64(
              Array(2.0, 0.0, 4.0),
              Array(true, false, true)
            )
          )
        )
      )
    val teamsBatch = storage:
      RecordBatch(
        teamsSchema,
        Vector(
          storage(ColumnArray.utf8(Array("a", "b"))),
          storage(ColumnArray.utf8(Array("north", "south")))
        )
      )
    (
      storage(Table[People](Vector(peopleBatch))),
      storage(Table[Teams](Vector(teamsBatch)))
    )

  def run: IO[Unit] =
    Resource
      .make(IO(inputs)): (people, teams) =>
        IO:
          people.close()
          teams.close()
      .use: (peopleTable, teamsTable) =>
        val inspected = frame(DynamicFrame.scan(peopleRef, peopleSchema.fields))
        val people = frame(inspected.typed[People])
        val teams = frame(Frame.values[Teams](teamsRef))
        val query: Frame[Result] = people
          .filter: row =>
            row.col("score").isNull ||
              (row.col("score") > Expr.literal(Option(1.0))).isTrue
          .withColumn("nextId")(row => row.col("id") + Expr.literal(1))
          .innerJoinUsing(teams, "team")
          .groupBy(row => Tuple1(row.col("region").as("region")))
          .aggregate: row =>
            (
              Aggregate.count.as("n"),
              Aggregate.mean(row.col("score")).as("meanScore")
            )
        val (normalized, normalization) = query.normalized
        val leftQuery: Frame[LeftResult] = people
          .leftJoinUsing(teams, "team")
          .select: row =>
            (
              row.col("id").as("id"),
              row.col("region").as("region")
            )
        val sources = ReferenceSources.empty
          .bind(peopleRef, peopleTable)
          .bind(teamsRef, teamsTable)
        val runtime = FrameRuntime[IO](sources)
        val sink = new CsvFrameSink[IO]()

        for
          _ <- IO.println(s"dynamic schema: ${inspected.schema}")
          _ <- IO.println(normalized.explain)
          _ <- IO.println(s"normalization rules: ${normalization.rules.mkString(", ")}")
          _ <- runtime
            .stream(normalized)
            .evalMap(batch => IO.println(s"streamed ${batch.rowCount} rows"))
            .compile
            .drain
          _ <- runtime.collect(normalized).use: table =>
            sink.write(table.schema, Stream.emits(table.batches)).flatMap:
              case Left(error) => IO.raiseError(new RuntimeException(error.message))
              case Right(output) => IO.println(output.text)
          _ <- IO.println(leftQuery.explain)
          _ <- runtime.collect(leftQuery).use: table =>
            sink.write(table.schema, Stream.emits(table.batches)).flatMap:
              case Left(error) => IO.raiseError(new RuntimeException(error.message))
              case Right(output) => IO.println(output.text)
        yield ()
