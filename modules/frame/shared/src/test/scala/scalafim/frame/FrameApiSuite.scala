package scalafim.frame

import scala.compiletime.testing.typeCheckErrors

class FrameApiSuite extends munit.FunSuite:
  type People = (
    id: Int,
    name: String,
    score: Option[Double],
    active: Option[Boolean]
  )

  type Departments = (
    departmentId: Int,
    departmentName: String
  )

  private def people: Frame[People] =
    Frame.source[People]("people").fold(error => fail(error.message), identity)

  private def departments: Frame[Departments] =
    Frame.source[Departments]("departments").fold(error => fail(error.message), identity)

  test("typed source derives an explicit runtime schema"):
    assertEquals(
      people.schema.fields,
      Vector(
        DynamicFrame.field("id", DataType.Int32),
        DynamicFrame.field("name", DataType.Utf8),
        DynamicFrame.field("score", DataType.Float64, nullable = true),
        DynamicFrame.field("active", DataType.Bool, nullable = true)
      )
    )

  test("select and withColumn compute named-tuple output schemas"):
    val selected: Frame[(id: Int, name: String)] =
      people.select: row =>
        (row.col("id").as("id"), row.col("name").as("name"))

    val extended: Frame[(id: Int, name: String, nextId: Int)] =
      selected.withColumn("nextId"): row =>
        row.col("id") + Expr.literal(1)

    assertEquals(extended.schema.fields.map(_.name), Vector("id", "name", "nextId"))
    assert(extended.explain.startsWith("Project[id, name, nextId]"))

  test("filter requires a total boolean while nullable booleans are explicit"):
    val filtered = people.filter: row =>
      row.col("active").isTrue && (row.col("id") > Expr.literal(0))

    assertEquals(filtered.schema, people.schema)
    assert(filtered.explain.startsWith("Filter"))

  test("nullable comparisons remain nullable until explicitly made total"):
    val filtered = people.filter: row =>
      val comparison: Expr[Option[Boolean]] =
        row.col("score") === Expr.literal(Option(1.0))
      comparison.isTrue

    assert(filtered.explain.startsWith("Filter"))

  test("grouping and core aggregates compute their result schema"):
    val grouped: Frame[(name: String, n: Long, maxId: Int)] =
      people
        .groupBy: row =>
          Tuple1(row.col("name").as("name"))
        .aggregate: row =>
          (
            Aggregate.count.as("n"),
            Aggregate.max(row.col("id")).as("maxId")
          )

    assertEquals(grouped.schema.fields.map(_.name), Vector("name", "n", "maxId"))
    assert(grouped.explain.startsWith("Aggregate[name, n, maxId]"))

  test("inner and left joins preserve typed field ownership and outer nullability"):
    val inner: Frame[(
      id: Int,
      name: String,
      score: Option[Double],
      active: Option[Boolean],
      departmentId: Int,
      departmentName: String
    )] = people.innerJoin(departments): (person, department) =>
      person.col("id") === department.col("departmentId")

    val left: Frame[(
      id: Int,
      name: String,
      score: Option[Double],
      active: Option[Boolean],
      departmentId: Option[Int],
      departmentName: Option[String]
    )] = people.leftJoin(departments): (person, department) =>
      person.col("id") === department.col("departmentId")

    assertEquals(inner.schema.fields.last.nullable, false)
    assertEquals(left.schema.fields.drop(4).map(_.nullable), Vector(true, true))

  test("using joins coalesce only the requested key and share typed and dynamic plans"):
    type Employees = (id: Int, employee: String)
    type Offices = (id: Int, office: String)
    val employees = Frame.source[Employees]("employees").toOption.get
    val offices = Frame.source[Offices]("offices").toOption.get

    val typed: Frame[(id: Int, employee: String, office: String)] =
      employees.innerJoinUsing(offices, "id")
    val left: Frame[(id: Int, employee: String, office: Option[String])] =
      employees.leftJoinUsing(offices, "id")
    val dynamic = employees.dynamic
      .innerJoinUsing(offices.dynamic, "id")
      .fold(error => fail(error.message), identity)

    assertEquals(typed.schema.fields.map(_.name), Vector("id", "employee", "office"))
    assertEquals(left.schema.fields.map(_.nullable), Vector(false, false, true))
    assertEquals(dynamic.schema, typed.schema)
    assertEquals(dynamic.explain, typed.explain)

  test("dynamic frames promote only after exact typed schema binding"):
    val dynamic = DynamicFrame
      .source(
        "people",
        Vector(
          DynamicFrame.field("id", DataType.Int32),
          DynamicFrame.field("name", DataType.Utf8),
          DynamicFrame.field("score", DataType.Float64, nullable = true),
          DynamicFrame.field("active", DataType.Bool, nullable = true)
        )
      )
      .fold(error => fail(error.message), identity)

    assert(dynamic.typed[People].isRight)

    type Wrong = (id: Long, name: String, score: Option[Double], active: Option[Boolean])
    dynamic.typed[Wrong] match
      case Left(FrameError.SchemaMismatch(issues)) =>
        assert(issues.exists(_.isInstanceOf[BindingIssue.FieldType]))
      case other => fail(s"expected a structured schema mismatch, found $other")

  test("dynamic expression construction validates type, nullability, and scope"):
    val dynamic = people.dynamic
    val id = dynamic.col("id").fold(error => fail(error.message), identity)
    val name = dynamic.col("name").fold(error => fail(error.message), identity)
    val active = dynamic.col("active").fold(error => fail(error.message), identity)

    assertEquals(id === name, Left(FrameError.ExpressionType(DataType.Int32, DataType.Utf8)))
    assertEquals(dynamic.filter(active), Left(FrameError.NullablePredicate(active.id)))

    val total = active.isTrue.fold(error => fail(error.message), identity)
    val filtered = dynamic.filter(total).fold(error => fail(error.message), identity)
    val selected = filtered
      .select("personId" -> id, "personName" -> name)
      .fold(error => fail(error.message), identity)

    assertEquals(selected.schema.fields.map(_.name), Vector("personId", "personName"))
    assert(selected.explain.startsWith("Project[personId, personName]"))

  test("typed erasure and promotion preserve the exact resolved plan without execution"):
    val typed = people.filter(row => row.col("id") > Expr.literal(0))
    val erased = typed.dynamic
    val rebound = erased.typed[People].fold(error => fail(error.message), identity)

    assert(typed.plan.asInstanceOf[AnyRef] eq erased.plan.asInstanceOf[AnyRef])
    assert(erased.plan.asInstanceOf[AnyRef] eq rebound.plan.asInstanceOf[AnyRef])
    assert(typed.schema.asInstanceOf[AnyRef] eq erased.schema.asInstanceOf[AnyRef])

  test("scan and values nodes carry immutable stable source references"):
    val valuesRef = SourceRef
      .values("people-fixture", "people fixture")
      .fold(error => fail(error.message), identity)
    val values = Frame.values[People](valuesRef).fold(error => fail(error.message), identity)

    assertEquals(values.plan.nodeName, "Values")
    assertEquals(values.plan.children, Vector.empty)
    assert(values.explain.startsWith("Values[people fixture; id=people-fixture]"))

  test("resolved IDs and explain output are deterministic across constructions"):
    val first = people.filter(row => row.col("id") > Expr.literal(0))
    val second = Frame
      .source[People]("people")
      .fold(error => fail(error.message), identity)
      .filter(row => row.col("id") > Expr.literal(0))

    assertEquals(first.schema.fields.map(_.id.value), Vector(
      "column:id",
      "column:name",
      "column:score",
      "column:active"
    ))
    assertEquals(first.explain, second.explain)
    assertEquals(
      first.explain,
      "Filter[expr:GreaterThan(expr:column:current:column:id,expr:literal:Int32(0))] " +
        "Schema(id: Int32, name: Utf8, score: Float64?, active: Bool?)\n" +
        "  Scan[people; id=people] Schema(id: Int32, name: Utf8, score: Float64?, active: Bool?)"
    )

  test("invalid dynamic schemas and limits return structured errors"):
    val duplicate = DynamicFrame.source(
      "duplicate",
      Vector(
        DynamicFrame.field("id", DataType.Int32),
        DynamicFrame.field("id", DataType.Int64)
      )
    )
    assertEquals(
      duplicate,
      Left(FrameError.InvalidSchema(SchemaError.DuplicateFieldName("id")))
    )
    assertEquals(people.limit(-1), Left(FrameError.InvalidLimit(-1)))
    val scanRef = SourceRef.scan("not-values", "not-values").toOption.get
    assertEquals(
      Frame.values[People](scanRef),
      Left(FrameError.NotValuesSource(scanRef.id, SourceKind.Scan))
    )

  test("missing and mistyped columns are rejected at compile time"):
    val missing = typeCheckErrors("""
      import scalafim.frame.*
      type S = (id: Int, name: String)
      Frame.source[S]("source").map(_.filter(_.col("missing") === Expr.literal(1)))
    """)
    val mistyped = typeCheckErrors("""
      import scalafim.frame.*
      type S = (id: Int, name: String)
      Frame.source[S]("source").map(_.filter { row =>
        val wrong: Expr[String] = row.col("id")
        wrong === Expr.literal("1")
      })
    """)
    val invalidOperator = typeCheckErrors("""
      import scalafim.frame.*
      type S = (id: Int, name: String)
      Frame.source[S]("source").map(_.filter(row => row.col("id") === row.col("name")))
    """)
    assert(missing.nonEmpty)
    assert(mistyped.nonEmpty)
    assert(invalidOperator.nonEmpty)

  test("nullable predicates, duplicate additions, and colliding joins are rejected"):
    val nullablePredicate = typeCheckErrors("""
      import scalafim.frame.*
      type S = (active: Option[Boolean])
      Frame.source[S]("source").map(_.filter(_.col("active")))
    """)
    val duplicateAddition = typeCheckErrors("""
      import scalafim.frame.*
      type S = (id: Int)
      Frame.source[S]("source").map(_.withColumn("id")(_ => Expr.literal(2)))
    """)
    val collidingJoin = typeCheckErrors("""
      import scalafim.frame.*
      type L = (id: Int)
      type R = (id: Int, label: String)
      val left = Frame.source[L]("left").toOption.get
      val right = Frame.source[R]("right").toOption.get
      left.innerJoin(right)((l, r) => l.col("id") === r.col("id"))
    """)
    val mismatchedUsingKey = typeCheckErrors("""
      import scalafim.frame.*
      type L = (id: Int, label: String)
      type R = (id: Long, value: Double)
      val left = Frame.source[L]("left").toOption.get
      val right = Frame.source[R]("right").toOption.get
      left.innerJoinUsing(right, "id")
    """)
    val usingCollision = typeCheckErrors("""
      import scalafim.frame.*
      type L = (id: Int, label: String)
      type R = (id: Int, label: String)
      val left = Frame.source[L]("left").toOption.get
      val right = Frame.source[R]("right").toOption.get
      left.innerJoinUsing(right, "id")
    """)
    val widenedAlias = typeCheckErrors("""
      import scalafim.frame.*
      val name: String = "widened"
      val widened: NamedExpr[String, Int] = NamedExpr(name, Expr.literal(1))
    """)
    assert(nullablePredicate.nonEmpty)
    assert(duplicateAddition.nonEmpty)
    assert(collidingJoin.nonEmpty)
    assert(mismatchedUsingKey.nonEmpty)
    assert(usingCollision.nonEmpty)
    assert(widenedAlias.nonEmpty)

  test("a wide named-tuple schema derives and resolves its final column"):
    type Wide = (
      f01: Int, f02: Int, f03: Int, f04: Int,
      f05: Int, f06: Int, f07: Int, f08: Int,
      f09: Int, f10: Int, f11: Int, f12: Int,
      f13: Int, f14: Int, f15: Int, f16: Int,
      f17: Int, f18: Int, f19: Int, f20: Int,
      f21: Int, f22: Int, f23: Int, f24: Int,
      f25: Int, f26: Int, f27: Int, f28: Int,
      f29: Int, f30: Int, f31: Int, f32: Int
    )
    val wide = Frame.source[Wide]("wide").fold(error => fail(error.message), identity)
    val finalColumn: Frame[(f32: Int)] = wide.select: row =>
      Tuple1(row.col("f32").as("f32"))

    assertEquals(wide.schema.size, 32)
    assertEquals(finalColumn.schema.fields.map(_.name), Vector("f32"))
