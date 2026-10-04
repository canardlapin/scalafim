package scalafim.fmri.mvpa

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{Draw, IndexSpace, Injection, Permutation, Reindexing, Selection}
import scala.compiletime.testing.typeCheckErrors
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class ReindexingLegSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def ints(values: Int*): IArray[Int] =
    IArray.unsafeFromArray(values.toArray)

  private def axis(keys: String*): AxisRef[String] =
    right(
      AxisRef.fromStableKeys(
        "trials",
        SpaceRole.Samples,
        keys.toVector,
        "acquisition-order",
        "trial",
        "unscaled",
        Vector("source:run-1")
      )
    )

  private def valueId(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))

  private def source(value: String): EvidenceSource =
    val id = SourceId.unsafe(value)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$value-root"), id)))

  private final class Counted(matrix: DMat) extends DoubleLinearOperator:
    val rows: Int = matrix.rows
    val cols: Int = matrix.cols
    var reads = 0

    def applyTo(input: DVec, output: MutableDVec): Unit =
      reads += 1
      matrix.applyTo(input, output)

    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      reads += 1
      matrix.transposeApplyTo(input, output)

  test("selection, injection, draw and permutation retain distinct axis identity"):
    val parent = axis("a", "b", "c", "d")
    val space = right(IndexSpace.of(parent.size))
    val selection = right(ReindexingLeg.bind(parent, right(Selection.from(ints(0, 2), space))))
    val injection = right(ReindexingLeg.bind(parent, right(Injection.from(ints(2, 0), space))))
    val draw = right(ReindexingLeg.bind(parent, right(Draw.from(ints(2, 0, 2), space))))
    val permutation = right(ReindexingLeg.bind(parent, right(Permutation.from(ints(3, 2, 1, 0)))))

    assertEquals(selection.kind, ReindexingKind.Selection)
    assertEquals(injection.kind, ReindexingKind.Injection)
    assertEquals(draw.kind, ReindexingKind.Draw)
    assertEquals(permutation.kind, ReindexingKind.Permutation)
    assertEquals(selection.members.map(_.parentKey), Vector("a", "c"))
    assertEquals(injection.members.map(_.parentKey), Vector("c", "a"))
    assertEquals(draw.members.map(_.parentKey), Vector("c", "a", "c"))
    assertEquals(permutation.members.map(_.parentKey), Vector("d", "c", "b", "a"))
    assertEquals(draw.members.map(_.occurrence.toVector), Vector(Vector(0), Vector(1), Vector(2)))
    assertEquals(draw.responseSelection.values, Vector(2, 0, 2))
    assertEquals(
      Vector(selection, injection, draw, permutation).map(_.child.descriptor.coordinateSignature).distinct.length,
      4
    )

    val gather = right(draw.leg(DMat.eye(parent.size)))
    val expectedGather = DMat.dense(3, 4, Vector(0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0))
    for row <- 0 until gather.rows; column <- 0 until gather.cols do
      assertEqualsDouble(gather(row, column), expectedGather(row, column), 0.0)

    val scatter = right(draw.leg.star(DMat.eye(draw.size)))
    val expectedScatter = DMat.dense(4, 3, Vector(0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0))
    for row <- 0 until scatter.rows; column <- 0 until scatter.cols do
      assertEqualsDouble(scatter(row, column), expectedScatter(row, column), 0.0)

  test("nested draws preserve root keys and occurrence paths through composition"):
    val parent = axis("a", "b", "c", "d")
    val first = right(
      ReindexingLeg.bind(parent, right(Draw.from(ints(3, 1, 3), right(IndexSpace.of(parent.size)))))
    )
    val second = right(
      first.continue(right(Draw.from(ints(2, 2), right(IndexSpace.of(first.size)))))
    )
    val composed = right(first.andThen(second))

    assertEquals(second.members.map(_.parentKey), Vector("d", "d"))
    assertEquals(second.members.map(_.parentStableKey), Vector("d", "d"))
    assertEquals(second.members.map(_.occurrence.toVector), Vector(Vector(2, 0), Vector(2, 1)))
    assertEquals(composed.ordinals.toVector, Vector(3, 3))
    assertEquals(composed.steps, first.steps ++ second.steps)
    assertEquals(composed.child.toRecord, second.child.toRecord)

    val values = right(Column.fromValues(parent, Vector(10.0, 20.0, 30.0, 40.0), valueId("target-v1")))
    val staged = values.reindex(first).reindex(second)
    val direct = values.reindex(composed)
    assertEquals(staged.values, Vector(40.0, 40.0))
    assertEquals(direct.values, staged.values)
    assertEquals(direct.toRecord.rows, staged.toRecord.rows)

  test("composition is associative and agrees with an independent direct ordinal oracle"):
    val parent = axis("a", "b", "c", "d", "e")
    val first = right(
      ReindexingLeg.bind(parent, right(Injection.from(ints(4, 1, 3, 0), right(IndexSpace.of(parent.size)))))
    )
    val second = right(
      first.continue(right(Selection.from(ints(0, 2, 3), right(IndexSpace.of(first.size)))))
    )
    val third = right(
      second.continue(right(Permutation.from(ints(2, 0, 1))))
    )
    val leftAssociated = right(right(first.andThen(second)).andThen(third))
    val rightAssociated = right(first.andThen(right(second.andThen(third))))
    val firstMap = Vector(4, 1, 3, 0)
    val secondMap = Vector(0, 2, 3)
    val thirdMap = Vector(2, 0, 1)
    val independent = thirdMap.map(secondMap).map(firstMap)
    val expected = Vector(0, 4, 3)

    assertEquals(independent, expected)
    assertEquals(leftAssociated.ordinals.toVector, expected)
    assertEquals(rightAssociated.ordinals.toVector, expected)
    assertEquals(leftAssociated.members.map(_.parentKey), Vector("a", "e", "d"))
    assertEquals(rightAssociated.members.map(_.parentKey), Vector("a", "e", "d"))
    assertEquals(leftAssociated.identity, rightAssociated.identity)
    assertEquals(leftAssociated.child.toRecord, third.child.toRecord)

  test("columns, observations and multiresponse targets restrict without reading matrix-free evidence"):
    val samples = axis("a", "b", "c", "d")
    val neural = right(
      AxisRef.fromStableKeys(
        "brain",
        SpaceRole.Observed,
        Vector("u", "v"),
        "native",
        "percent-signal-change",
        "unscaled"
      )
    )
    val features = right(
      AxisRef.fromStableKeys(
        "targets",
        SpaceRole.Observed,
        Vector("shape", "texture"),
        "native",
        "score",
        "unscaled"
      )
    )
    val raw = DMat.dense(4, 2, Vector(1.0, 2.0, 3.0, 5.0, 7.0, 11.0, 13.0, 17.0))
    val counted = new Counted(raw)
    val observations = right(
      Observations.fromOperator(samples, neural, counted, valueId("bold-v1"), source("bold"))
    )
    val targets = right(
      MultiResponse.fromDense(samples, features, raw, valueId("targets-v1"), source("targets"))
    )
    val leg = right(
      ReindexingLeg.bind(samples, right(Injection.from(ints(3, 0, 2), right(IndexSpace.of(samples.size)))))
    )
    val selectedObservations = observations.reindex(leg)
    val selectedTargets = targets.reindex(leg)
    assertEquals(counted.reads, 0)

    val actualObservations = right(selectedObservations.patterns(DMat.eye(2)))
    val actualTargets = right(selectedTargets.targets(DMat.eye(2)))
    val expected = Vector(Vector(13.0, 17.0), Vector(1.0, 2.0), Vector(7.0, 11.0))
    for row <- expected.indices; column <- expected(row).indices do
      assertEqualsDouble(actualObservations(row, column), expected(row)(column), 1e-12)
      assertEqualsDouble(actualTargets(row, column), expected(row)(column), 1e-12)
    assertEquals(counted.reads, 2)
    assertEquals(selectedObservations.sampleAxis, leg.child.descriptor)
    assertEquals(selectedTargets.sampleAxis, leg.child.descriptor)

  test("foreign row legs and abstract reindexings fail at the public type boundary"):
    val wrongRows = typeCheckErrors("""import scalafim.fmri.mvpa.*
import multivar.core.*
import resample4s.core.*
def invalid[S <: SemanticSpace,T <: SemanticSpace,K,R <: Reindexing](
  column: Column[S,Double], leg: ReindexingLeg[T,K,R]
) = column.reindex(leg)
""")
    val abstractMap = typeCheckErrors("""import scalafim.fmri.mvpa.*
import resample4s.core.*
def invalid[K](axis: AxisRef[K], mapping: Reindexing) =
  ReindexingLeg.bind(axis, mapping)
""")
    assertEquals(wrongRows.length, 1)
    assert(wrongRows.head.message.contains("ReindexingLeg[S"))
    assertEquals(abstractMap.length, 1)
    assert(abstractMap.head.message.contains("Keep the value typed as resample4s.core.Selection"))
