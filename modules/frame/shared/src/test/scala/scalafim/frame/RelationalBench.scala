package scalafim.frame

object RelationalBench:
  type Facts = (id: Int, group: String, value: Option[Double])
  type Groups = (group: String, label: String)
  type Projected = (id: Int, value: Option[Double])
  type Aggregated = (group: String, n: Long, mean: Option[Double])

  private def nanos[A](operation: => A): (A, Long) =
    val started = System.nanoTime()
    val result = operation
    (result, System.nanoTime() - started)

  private def storage[A](result: Either[StorageError, A]): A =
    result.fold(error => throw new IllegalStateException(error.message), identity)

  private def frame[A](result: Either[FrameError, A]): A =
    result.fold(error => throw new IllegalStateException(error.message), identity)

  private def execution[A](result: Either[ExecutionError, A]): A =
    result.fold(error => throw new IllegalStateException(error.message), identity)

  private def collect[S <: scala.NamedTuple.AnyNamedTuple](
      query: Frame[S],
      sources: ReferenceSources
  )(using SchemaDescriptor[S]): Table[S] =
    execution(ReferenceInterpreter.prepare(query.plan, sources).collect[S])

  def main(arguments: Array[String]): Unit =
    val length = 100000
    val tracker = new BufferTracker
    val factsRef = frame(SourceRef.values("bench-facts", "bench-facts"))
    val groupsRef = frame(SourceRef.values("bench-groups", "bench-groups"))
    val factsSchema = summon[SchemaDescriptor[Facts]].schema
    val groupsSchema = summon[SchemaDescriptor[Groups]].schema
    val factGroups = Array.tabulate(length)(index => s"g${index % 10}")
    val factValues = Array.tabulate(length)(index => index.toDouble / 10.0)
    val factValidity = Array.tabulate(length)(index => index % 11 != 0)
    val facts = storage:
      Table[Facts](
        Vector(
          storage:
            RecordBatch(
              factsSchema,
              Vector(
                storage(ColumnArray.int32(Array.tabulate(length)(identity), tracker = tracker)),
                storage(ColumnArray.utf8(factGroups, tracker = tracker)),
                storage(ColumnArray.float64(factValues, factValidity, tracker))
              )
            )
        )
      )
    val groups = storage:
      Table[Groups](
        Vector(
          storage:
            RecordBatch(
              groupsSchema,
              Vector(
                storage(ColumnArray.utf8(Array.tabulate(10)(index => s"g$index"), tracker = tracker)),
                storage(ColumnArray.utf8(Array.tabulate(10)(index => s"group-$index"), tracker = tracker))
              )
            )
        )
      )
    val factsFrame = frame(Frame.values[Facts](factsRef))
    val groupsFrame = frame(Frame.values[Groups](groupsRef))
    val sources = ReferenceSources.empty
      .bind(factsRef, facts)
      .bind(groupsRef, groups)

    val (scanned, scanNanos) = nanos(collect(factsFrame, sources))
    val scanRows = scanned.rowCount
    scanned.close()

    val projected = factsFrame
      .select(row => (row.col("id").as("id"), row.col("value").as("value")))
      .filter(row => row.col("id") >= Expr.literal(length / 2))
    val (normalizedProjected, normalization) = projected.normalized
    val (projectedOutput, projectNanos) = nanos(collect(normalizedProjected, sources))
    val projectRows = projectedOutput.rowCount
    projectedOutput.close()

    val aggregated = factsFrame
      .groupBy(row => Tuple1(row.col("group").as("group")))
      .aggregate: row =>
        (
          Aggregate.count.as("n"),
          Aggregate.mean(row.col("value")).as("mean")
        )
    val (aggregateOutput, aggregateNanos) = nanos(collect(aggregated, sources))
    val aggregateRows = aggregateOutput.rowCount
    aggregateOutput.close()

    val joined = factsFrame.innerJoinUsing(groupsFrame, "group")
    val (joinOutput, joinNanos) = nanos(collect(joined, sources))
    val joinRows = joinOutput.rowCount
    joinOutput.close()

    val utf8 = storage(facts.batches.head.column("group"))
    val (utf8Checksum, utf8Nanos) = nanos:
      var total = 0L
      var index = 0
      while index < length do
        storage(utf8.scalar(index)) match
          case ScalarValue.Utf8(value) => total += value.length.toLong
          case other => throw new IllegalStateException(s"unexpected UTF-8 value $other")
        index += 1
      total

    println(s"rows=$length scan_rows=$scanRows project_rows=$projectRows aggregate_rows=$aggregateRows join_rows=$joinRows")
    println(s"normalization_rules=${normalization.rules.mkString(",")}")
    println(s"utf8_checksum=$utf8Checksum")
    println(f"scan_ms=${scanNanos / 1000000.0}%.3f")
    println(f"filter_project_ms=${projectNanos / 1000000.0}%.3f")
    println(f"group_aggregate_ms=${aggregateNanos / 1000000.0}%.3f")
    println(f"inner_join_ms=${joinNanos / 1000000.0}%.3f")
    println(f"utf8_scan_ms=${utf8Nanos / 1000000.0}%.3f")
    println(s"before_close=${tracker.snapshot}")

    facts.close()
    groups.close()
    println(s"after_close=${tracker.snapshot}")
