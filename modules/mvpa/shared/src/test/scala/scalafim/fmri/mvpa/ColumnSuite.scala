package scalafim.fmri.mvpa

import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import scala.compiletime.testing.typeCheckErrors

class ColumnSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def axis(keys: String*): AxisRef[String] =
    right(
      AxisRef.fromStableKeys(
        "trials",
        SpaceRole.Samples,
        keys.toVector,
        "native",
        "trial",
        "unscaled",
        Vector("source:run-1")
      )
    )

  private def valueId(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))

  test("columns bind values to the full ordered sample axis"):
    val samples = axis("trial-91", "trial-7", "trial-32")
    val column = right(Column.fromValues(samples, Vector(10.0, 20.0, 30.0), valueId("target-v1")))
    assertEquals(column.size, 3)
    assertEquals(right(column(1)), 20.0)
    assertEquals(column.stableRowKeys, Vector("trial-91", "trial-7", "trial-32"))
    assertEquals(column.toRecord.rows, samples.toRecord)
    assertEquals(column.identity.values, valueId("target-v1"))

    val reordered = axis("trial-32", "trial-7", "trial-91")
    assert(Column.decode(reordered, column.toRecord).isLeft)
    assert(Column.bind(samples, reordered.toRecord, column.values, valueId("target-v1")).isLeft)

  test("stable keys, rather than implementation ordinals, survive selection-shaped export"):
    val selected = axis("trial-32", "trial-91")
    val selectedColumn = right(Column.fromValues(selected, Vector(30.0, 10.0), valueId("selected-target-v1")))
    val record = selectedColumn.toRecord
    assertEquals(record.rows.stableKeys, Vector("trial-32", "trial-91"))
    assertEquals(record.values, Vector(30.0, 10.0))
    assertEquals(right(AxisRef.restore(record.rows)).toRecord, record.rows)

  test("categorical, scalar and metadata columns remain ordinary distinct Scala types"):
    final case class Condition(value: String)
    final case class RunId(value: String)
    val samples = axis("a", "b")
    val categorical: Column[samples.Id, Condition] =
      right(Column.fromValues(samples, Vector(Condition("face"), Condition("house")), valueId("condition-v1")))
    val continuous: Column[samples.Id, Double] =
      right(Column.fromValues(samples, Vector(1.5, -2.0), valueId("score-v1")))
    val metadata: Column[samples.Id, RunId] =
      right(Column.fromValues(samples, Vector(RunId("run-1"), RunId("run-2")), valueId("run-v1")))
    assertEquals(categorical.values.map(_.value), Vector("face", "house"))
    assertEquals(continuous.values, Vector(1.5, -2.0))
    assertEquals(metadata.values.map(_.value), Vector("run-1", "run-2"))

  test("column length and nominal row boundaries fail at construction or compilation"):
    val samples = axis("a", "b")
    assert(Column.fromValues(samples, Vector(1.0), valueId("short")).isLeft)
    assertEquals(Column.fromValues(samples, Vector(1.0, 2.0), valueId("valid")).isRight, true)

    val errors = typeCheckErrors("""import scalafim.fmri.mvpa.*
import multivar.core.*
def invalid[S <: SemanticSpace, T <: SemanticSpace, N <: SemanticSpace, A](
  observations: Observations[S,N], target: Column[T,A]
) = Supervised(observations, target)
""")
    assertEquals(errors.length, 1)
    assert(errors.head.message.contains("Column[S, Y]"))
