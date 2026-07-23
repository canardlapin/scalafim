package scalafim.frame

class ReferenceInterpreterSuite extends munit.FunSuite:
  type Input = (
    id: Int,
    value: Option[Double],
    flag: Option[Boolean],
    label: String
  )
  type People = (id: Int, team: String, score: Option[Double])
  type Teams = (team: String, region: String)
  type WorkflowResult = (region: String, n: Long, meanScore: Option[Double])

  private val schema = summon[SchemaDescriptor[Input]].schema

  private def storage[A](result: Either[StorageError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def execution[A](result: Either[ExecutionError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def batch(
      ids: Array[Int],
      values: Array[Double],
      valueValid: Array[Boolean],
      flags: Array[Boolean],
      flagValid: Array[Boolean],
      labels: Array[String]
  ): RecordBatch =
    storage:
      RecordBatch(
        schema,
        Vector(
          storage(ColumnArray.int32(ids)),
          storage(ColumnArray.float64(values, valueValid)),
          storage(ColumnArray.bool(flags, flagValid)),
          storage(ColumnArray.utf8(labels))
        )
      )

  private def fixture(chunked: Boolean): Table[Input] =
    val batches =
      if chunked then
        Vector(
          batch(
            Array(1, 2),
            Array(1.0, 0.0),
            Array(true, false),
            Array(true, false),
            Array(true, false),
            Array("a", "b")
          ),
          batch(
            Array(3, 4),
            Array(Double.NaN, 1.0),
            Array(true, true),
            Array(false, true),
            Array(true, true),
            Array("c", "d")
          )
        )
      else
        Vector(
          batch(
            Array(1, 2, 3, 4),
            Array(1.0, 0.0, Double.NaN, 1.0),
            Array(true, false, true, true),
            Array(true, false, false, true),
            Array(true, false, true, true),
            Array("a", "b", "c", "d")
          )
        )
    storage(Table[Input](batches))

  private def reference: SourceRef =
    SourceRef.values("fixture", "fixture").fold(error => fail(error.message), identity)

  private def source: Frame[Input] =
    Frame.values[Input](reference).fold(error => fail(error.message), identity)

  private def collect[S <: scala.NamedTuple.AnyNamedTuple](
      frame: Frame[S],
      table: Table[Input]
  )(using SchemaDescriptor[S]): Table[S] =
    val sources = ReferenceSources.empty.bind(reference, table)
    execution(ReferenceInterpreter.prepare(frame.plan, sources).collect[S])

  private def scalars(table: Table[?], column: String): Vector[ScalarValue] =
    table.batches.flatMap: batch =>
      val values = storage(batch.column(column))
      Vector.tabulate(batch.rowCount)(index => storage(values.scalar(index)))

  test("dynamic-to-typed analytical workflow executes on every platform"):
    val peopleSchema = summon[SchemaDescriptor[People]].schema
    val teamsSchema = summon[SchemaDescriptor[Teams]].schema
    val peopleRef = SourceRef.values("workflow-people", "workflow-people").toOption.get
    val teamsRef = SourceRef.values("workflow-teams", "workflow-teams").toOption.get
    val peopleInput = storage:
      Table[People](
        Vector(
          storage:
            RecordBatch(
              peopleSchema,
              Vector(
                storage(ColumnArray.int32(Array(1, 2, 3))),
                storage(ColumnArray.utf8(Array("a", "b", "a"))),
                storage(ColumnArray.float64(Array(2.0, 0.0, 4.0), Array(true, false, true)))
              )
            )
        )
      )
    val teamsInput = storage:
      Table[Teams](
        Vector(
          storage:
            RecordBatch(
              teamsSchema,
              Vector(
                storage(ColumnArray.utf8(Array("a", "b"))),
                storage(ColumnArray.utf8(Array("north", "south")))
              )
            )
        )
      )
    val dynamic = DynamicFrame
      .scan(peopleRef, peopleSchema.fields)
      .fold(error => fail(error.message), identity)
    val people = dynamic.typed[People].fold(error => fail(error.message), identity)
    val teams = Frame.values[Teams](teamsRef).fold(error => fail(error.message), identity)
    val query: Frame[WorkflowResult] = people
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
    val sources = ReferenceSources.empty
      .bind(peopleRef, peopleInput)
      .bind(teamsRef, teamsInput)
    val output = execution(ReferenceInterpreter.prepare(query.normalized._1.plan, sources).collect[WorkflowResult])

    assertEquals(
      scalars(output, "region"),
      Vector(ScalarValue.Utf8("north"), ScalarValue.Utf8("south"))
    )
    assertEquals(scalars(output, "n"), Vector(ScalarValue.Int64(2L), ScalarValue.Int64(1L)))
    assertEquals(
      scalars(output, "meanScore"),
      Vector(ScalarValue.Float64(3.0), ScalarValue.Null)
    )

    output.close()
    peopleInput.close()
    teamsInput.close()

  test("project, withColumn, filter, and limit execute without whole-input fallback"):
    val input = fixture(chunked = true)
    val query = source
      .filter(row => row.col("flag").isTrue)
      .withColumn("nextId")(row => row.col("id") + Expr.literal(1))
      .select(row => (row.col("label").as("label"), row.col("nextId").as("nextId")))
      .limit(1)
      .fold(error => fail(error.message), identity)

    val physical = ReferenceInterpreter.prepare(query.plan, ReferenceSources.empty.bind(reference, input))
    val output = execution(physical.collect[(label: String, nextId: Int)])

    assertEquals(scalars(output, "label"), Vector(ScalarValue.Utf8("a")))
    assertEquals(scalars(output, "nextId"), Vector(ScalarValue.Int32(2)))
    assertEquals(physical.shape.streaming, true)
    assertEquals(physical.physicalExplain, "ReferenceExecution(mode=streaming, blocking=none, estimatedRows=1, fallback=none)")

    output.close()
    input.close()

  test("three-valued filters retain only true and null-safe equality is total"):
    val input = fixture(chunked = false)
    val matching = source.filter: row =>
      (row.col("value") === Expr.literal(Option(1.0))).isTrue
    val missing = source
      .select: row =>
        Tuple1(row.col("value").nullSafeEq(Expr.literal(Option.empty[Double])).as("missing"))

    val matched = collect(matching, input)
    val missingValues = collect(missing, input)

    assertEquals(scalars(matched, "id"), Vector(ScalarValue.Int32(1), ScalarValue.Int32(4)))
    assertEquals(
      scalars(missingValues, "missing"),
      Vector(
        ScalarValue.Bool(false),
        ScalarValue.Bool(true),
        ScalarValue.Bool(false),
        ScalarValue.Bool(false)
      )
    )

    matched.close()
    missingValues.close()
    input.close()

  test("filter composition and projection identity are semantic laws"):
    val input = fixture(chunked = true)
    val composed = source
      .filter(row => row.col("id") > Expr.literal(1))
      .filter(row => row.col("id") < Expr.literal(4))
    val fused = source.filter: row =>
      (row.col("id") > Expr.literal(1)) && (row.col("id") < Expr.literal(4))
    val identityProjection = source.select: row =>
      (
        row.col("id").as("id"),
        row.col("value").as("value"),
        row.col("flag").as("flag"),
        row.col("label").as("label")
      )

    val composedOutput = collect(composed, input)
    val fusedOutput = collect(fused, input)
    val projectedOutput = collect(identityProjection, input)

    assertEquals(scalars(composedOutput, "id"), scalars(fusedOutput, "id"))
    assertEquals(scalars(projectedOutput, "id"), Vector(1, 2, 3, 4).map(ScalarValue.Int32.apply))
    scalars(projectedOutput, "value").zip(scalars(input, "value")).foreach:
      case (ScalarValue.Float64(left), ScalarValue.Float64(right)) if left.isNaN && right.isNaN => ()
      case (left, right) => assertEquals(left, right)

    composedOutput.close()
    fusedOutput.close()
    projectedOutput.close()
    input.close()

  test("results are invariant to source chunk boundaries"):
    val oneBatch = fixture(chunked = false)
    val twoBatches = fixture(chunked = true)
    val query = source
      .filter(row => row.col("id") >= Expr.literal(2))
      .select(row => (row.col("id").as("id"), row.col("label").as("label")))

    val first = collect(query, oneBatch)
    val second = collect(query, twoBatches)

    assertEquals(scalars(first, "id"), scalars(second, "id"))
    assertEquals(scalars(first, "label"), scalars(second, "label"))
    assertEquals(first.batches.map(_.rowCount).sum, second.batches.map(_.rowCount).sum)

    first.close()
    second.close()
    oneBatch.close()
    twoBatches.close()

  test("integer overflow is checked and returned as a structured execution error"):
    val overflowBatch = batch(
      Array(Int.MaxValue),
      Array(0.0),
      Array(false),
      Array(false),
      Array(false),
      Array("overflow")
    )
    val input = storage(Table[Input](Vector(overflowBatch)))
    val query = source.withColumn("overflow")(row => row.col("id") + Expr.literal(1))
    val result = ReferenceInterpreter
      .prepare(query.plan, ReferenceSources.empty.bind(reference, input))
      .collect[(
        id: Int,
        value: Option[Double],
        flag: Option[Boolean],
        label: String,
        overflow: Int
      )]

    result match
      case Left(ExecutionError.IntegerOverflow(_, BinaryOperator.Add)) => ()
      case other => fail(s"expected checked addition overflow, found $other")
    input.close()

  test("order guarantees are explicit and preserved only by streaming operators"):
    val unordered = SourceRef
      .scan("scan", "scan")
      .fold(error => fail(error.message), identity)
    val scan = Frame.scan[Input](unordered)
    val projected = scan.select(row => Tuple1(row.col("id").as("id"))).limit(2).toOption.get

    assertEquals(scan.plan.order, OrderGuarantee.Unspecified)
    assertEquals(projected.plan.order, OrderGuarantee.Unspecified)
    assertEquals(source.plan.order, OrderGuarantee.Stable)

  test("grouped aggregates define null, empty, and floating reduction policies"):
    type Sales = (group: Option[String], amount: Option[Double], item: Int)
    type Result = (
      group: Option[String],
      n: Long,
      total: Option[Double],
      mean: Option[Double],
      variance: Option[Double],
      minimum: Option[Double],
      maximum: Option[Double]
    )
    val salesSchema = summon[SchemaDescriptor[Sales]].schema
    val salesRef = SourceRef.values("sales", "sales").toOption.get
    val salesBatch = storage:
      RecordBatch(
        salesSchema,
        Vector(
          storage(ColumnArray.utf8(
            Array("a", "", "a", "", "a"),
            Array(true, false, true, false, true)
          )),
          storage(ColumnArray.float64(
            Array(1.0, 5.0, 0.0, 7.0, 3.0),
            Array(true, true, false, true, true)
          )),
          storage(ColumnArray.int32(Array(1, 2, 3, 4, 5)))
        )
      )
    val salesTable = storage(Table[Sales](Vector(salesBatch)))
    val sales = Frame.values[Sales](salesRef).toOption.get
    val query: Frame[Result] = sales
      .groupBy(row => Tuple1(row.col("group").as("group")))
      .aggregate: row =>
        (
          Aggregate.count.as("n"),
          Aggregate.sum(row.col("amount")).as("total"),
          Aggregate.mean(row.col("amount")).as("mean"),
          Aggregate.variance(row.col("amount")).as("variance"),
          Aggregate.min(row.col("amount")).as("minimum"),
          Aggregate.max(row.col("amount")).as("maximum")
        )

    val output = execution:
      ReferenceInterpreter
        .prepare(query.plan, ReferenceSources.empty.bind(salesRef, salesTable))
        .collect[Result]

    assertEquals(scalars(output, "group"), Vector(ScalarValue.Utf8("a"), ScalarValue.Null))
    assertEquals(scalars(output, "n"), Vector(ScalarValue.Int64(3), ScalarValue.Int64(2)))
    assertEquals(scalars(output, "total"), Vector(ScalarValue.Float64(4.0), ScalarValue.Float64(12.0)))
    assertEquals(scalars(output, "mean"), Vector(ScalarValue.Float64(2.0), ScalarValue.Float64(6.0)))
    assertEquals(scalars(output, "variance"), Vector(ScalarValue.Float64(1.0), ScalarValue.Float64(1.0)))
    assertEquals(scalars(output, "minimum"), Vector(ScalarValue.Float64(1.0), ScalarValue.Float64(5.0)))
    assertEquals(scalars(output, "maximum"), Vector(ScalarValue.Float64(3.0), ScalarValue.Float64(7.0)))
    output.close()
    salesTable.close()

  test("an empty global aggregate emits count zero and null reductions"):
    type EmptyInput = (value: Option[Double])
    type Result = (n: Long, total: Option[Double], mean: Option[Double])
    val emptyRef = SourceRef.values("empty", "empty").toOption.get
    val empty = storage(Table[EmptyInput](Vector.empty))
    val source = Frame.values[EmptyInput](emptyRef).toOption.get
    val query: Frame[Result] = source
      .groupBy(_ => EmptyTuple)
      .aggregate: row =>
        (
          Aggregate.count.as("n"),
          Aggregate.sum(row.col("value")).as("total"),
          Aggregate.mean(row.col("value")).as("mean")
        )

    val output = execution:
      ReferenceInterpreter
        .prepare(query.plan, ReferenceSources.empty.bind(emptyRef, empty))
        .collect[Result]

    assertEquals(scalars(output, "n"), Vector(ScalarValue.Int64(0)))
    assertEquals(scalars(output, "total"), Vector(ScalarValue.Null))
    assertEquals(scalars(output, "mean"), Vector(ScalarValue.Null))
    output.close()
    empty.close()

  test("inner and left joins obey null-key and duplicate-key semantics"):
    type Left = (id: Int, key: Option[Int])
    type Right = (rightKey: Option[Int], label: String)
    val leftSchema = summon[SchemaDescriptor[Left]].schema
    val rightSchema = summon[SchemaDescriptor[Right]].schema
    val leftRef = SourceRef.values("left", "left").toOption.get
    val rightRef = SourceRef.values("right", "right").toOption.get
    val leftBatch = storage:
      RecordBatch(
        leftSchema,
        Vector(
          storage(ColumnArray.int32(Array(1, 2, 3, 4))),
          storage(ColumnArray.int32(Array(1, 0, 1, 2), Array(true, false, true, true)))
        )
      )
    val rightBatch = storage:
      RecordBatch(
        rightSchema,
        Vector(
          storage(ColumnArray.int32(Array(1, 1, 0, 3), Array(true, true, false, true))),
          storage(ColumnArray.utf8(Array("x", "y", "null", "z")))
        )
      )
    val leftTable = storage(Table[Left](Vector(leftBatch)))
    val rightTable = storage(Table[Right](Vector(rightBatch)))
    val left = Frame.values[Left](leftRef).toOption.get
    val right = Frame.values[Right](rightRef).toOption.get
    val inner = left.innerJoin(right): (lhs, rhs) =>
      (lhs.col("key") === rhs.col("rightKey")).isTrue
    val outer = left.leftJoin(right): (lhs, rhs) =>
      (lhs.col("key") === rhs.col("rightKey")).isTrue
    val sources = ReferenceSources.empty.bind(leftRef, leftTable).bind(rightRef, rightTable)

    val innerOutput = execution(ReferenceInterpreter.prepare(inner.plan, sources).collect[
      (id: Int, key: Option[Int], rightKey: Option[Int], label: String)
    ])
    val outerOutput = execution(ReferenceInterpreter.prepare(outer.plan, sources).collect[
      (id: Int, key: Option[Int], rightKey: Option[Int], label: Option[String])
    ])

    assertEquals(scalars(innerOutput, "id"), Vector(1, 1, 3, 3).map(ScalarValue.Int32.apply))
    assertEquals(
      scalars(outerOutput, "id"),
      Vector(1, 1, 2, 3, 3, 4).map(ScalarValue.Int32.apply)
    )
    assertEquals(
      scalars(outerOutput, "label"),
      Vector(
        ScalarValue.Utf8("x"),
        ScalarValue.Utf8("y"),
        ScalarValue.Null,
        ScalarValue.Utf8("x"),
        ScalarValue.Utf8("y"),
        ScalarValue.Null
      )
    )

    innerOutput.close()
    outerOutput.close()
    leftTable.close()
    rightTable.close()

  test("using joins execute over the coalesced output schema"):
    type Left = (key: Option[Int], leftLabel: String)
    type Right = (key: Option[Int], rightLabel: String)
    type Inner = (key: Option[Int], leftLabel: String, rightLabel: String)
    type Outer = (key: Option[Int], leftLabel: String, rightLabel: Option[String])
    val leftRef = SourceRef.values("using-left", "using-left").toOption.get
    val rightRef = SourceRef.values("using-right", "using-right").toOption.get
    val leftBatch = storage:
      RecordBatch(
        summon[SchemaDescriptor[Left]].schema,
        Vector(
          storage(ColumnArray.int32(Array(1, 0, 2), Array(true, false, true))),
          storage(ColumnArray.utf8(Array("a", "null", "b")))
        )
      )
    val rightBatch = storage:
      RecordBatch(
        summon[SchemaDescriptor[Right]].schema,
        Vector(
          storage(ColumnArray.int32(Array(1, 1, 0), Array(true, true, false))),
          storage(ColumnArray.utf8(Array("x", "y", "null")))
        )
      )
    val leftTable = storage(Table[Left](Vector(leftBatch)))
    val rightTable = storage(Table[Right](Vector(rightBatch)))
    val left = Frame.values[Left](leftRef).toOption.get
    val right = Frame.values[Right](rightRef).toOption.get
    val sources = ReferenceSources.empty.bind(leftRef, leftTable).bind(rightRef, rightTable)

    val inner = execution:
      ReferenceInterpreter
        .prepare(left.innerJoinUsing(right, "key").plan, sources)
        .collect[Inner]
    val outer = execution:
      ReferenceInterpreter
        .prepare(left.leftJoinUsing(right, "key").plan, sources)
        .collect[Outer]

    assertEquals(scalars(inner, "leftLabel"), Vector("a", "a").map(ScalarValue.Utf8.apply))
    assertEquals(
      scalars(outer, "rightLabel"),
      Vector(
        ScalarValue.Utf8("x"),
        ScalarValue.Utf8("y"),
        ScalarValue.Null,
        ScalarValue.Null
      )
    )
    inner.close()
    outer.close()
    leftTable.close()
    rightTable.close()

  test("sort is stable with explicit null placement and a declared blocking shape"):
    type SortInput = (key: Option[Int], label: String)
    val sortSchema = summon[SchemaDescriptor[SortInput]].schema
    val sortRef = SourceRef.values("sort", "sort").toOption.get
    val sortBatch = storage:
      RecordBatch(
        sortSchema,
        Vector(
          storage(ColumnArray.int32(Array(2, 0, 1, 1), Array(true, false, true, true))),
          storage(ColumnArray.utf8(Array("a", "b", "c", "d")))
        )
      )
    val sortTable = storage(Table[SortInput](Vector(sortBatch)))
    val source = Frame.values[SortInput](sortRef).toOption.get
    val query = source.sortBy(SortDirection.Ascending, NullPlacement.First)(_.col("key"))
    val prepared = ReferenceInterpreter.prepare(
      query.plan,
      ReferenceSources.empty.bind(sortRef, sortTable)
    )
    val output = execution(prepared.collect[SortInput])

    assertEquals(
      scalars(output, "label"),
      Vector("b", "c", "d", "a").map(ScalarValue.Utf8.apply)
    )
    assertEquals(prepared.shape.streaming, false)
    assertEquals(prepared.shape.blockingNodes, Vector("Sort"))
    assert(query.plan.order.isInstanceOf[OrderGuarantee.Sorted])

    output.close()
    sortTable.close()

  test("normalization rewrites are deterministic and preserve null and NaN semantics"):
    val input = fixture(chunked = true)
    val original = source
      .filter(row => row.col("value").isNull || (row.col("id") > Expr.literal(0)))
      .filter(row => row.col("id") < Expr.literal(5))
      .select: row =>
        (
          row.col("id").as("id"),
          row.col("value").as("value"),
          row.col("label").as("label")
        )
      .select: row =>
        (row.col("value").as("value"), row.col("label").as("label"))
      .limit(3)
      .toOption
      .get
      .limit(2)
      .toOption
      .get
    val (normalized, receipt) = original.normalized
    val sources = ReferenceSources.empty.bind(reference, input)
    val first = execution(ReferenceInterpreter.prepare(original.plan, sources).collect[
      (value: Option[Double], label: String)
    ])
    val second = execution(ReferenceInterpreter.prepare(normalized.plan, sources).collect[
      (value: Option[Double], label: String)
    ])

    assert(receipt.rules.contains(NormalizationRule.FuseFilters))
    assert(receipt.rules.contains(NormalizationRule.FuseProjects))
    assert(receipt.rules.contains(NormalizationRule.CollapseLimits))
    assertEquals(scalars(first, "label"), scalars(second, "label"))
    scalars(first, "value").zip(scalars(second, "value")).foreach:
      case (ScalarValue.Float64(left), ScalarValue.Float64(right))
          if left.isNaN && right.isNaN => ()
      case (left, right) => assertEquals(left, right)
    assertEquals(
      PlanNormalizer.normalize(original.plan).receipt.normalizedExplain,
      receipt.normalizedExplain
    )

    first.close()
    second.close()
    input.close()
