package frameconsumer

import scalafim.frame.*
import scala.compiletime.testing.typeCheckErrors

class PublicApiSuite extends munit.FunSuite:
  type Input = (id: Int, label: String)

  test("the typed query surface is usable outside scalafim.frame"):
    val source = Frame.source[Input]("input").fold(error => fail(error.message), identity)
    val query: Frame[(label: String, nextId: Int)] =
      source
        .filter(row => row.col("id") > Expr.literal(0))
        .select(row => Tuple1(row.col("label").as("label")))
        .withColumn("nextId")(_ => Expr.literal(1))

    assertEquals(query.schema.fields.map(_.name), Vector("label", "nextId"))

  test("resolved plan node constructors are not part of the public API"):
    val errors = typeCheckErrors("""
      import scalafim.frame.*
      type S = (id: Int)
      val schema = summon[SchemaDescriptor[S]].schema
      val source = SourceRef.scan("id", "source").toOption.get
      LogicalPlan.Source(source, schema)
    """)
    assert(errors.nonEmpty)
