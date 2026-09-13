package scalafim.umvpaspike

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.*
import resample4s.core.{Draw, IndexSpace, Injection, Permutation, Selection}
import scala.compiletime.testing.typeCheckErrors
import IdentityPrototype.*

class IdentityPrototypeSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def axis(name: String, keys: String*): Axis =
    right(Axis.load(DecodedAxis(name, keys.toVector.map(Key(_)), "native", "percent", "unscaled")))

  private def valueId(label: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(label))
  private val revision = SourceRevision("fixture:brain", "v1")

  private final class Counted(val rows: Int, val cols: Int, values: Option[DMat]) extends DoubleLinearOperator:
    var reads = 0
    def applyTo(x: DVec, into: MutableDVec): Unit =
      reads += 1
      values match
        case Some(matrix) => matrix.applyTo(x, into)
        case None         => throw new IllegalStateException("poison neural read")
    override def transposeApplyTo(x: DVec, into: MutableDVec): Unit =
      reads += 1
      values match
        case Some(matrix) => matrix.transposeApplyTo(x, into)
        case None         => throw new IllegalStateException("poison neural read")

  test("runtime identity checks full ordered metadata before touching a poison neural source"):
    val samples = axis("trials", "trial-91", "trial-7", "trial-32")
    val neural = axis("brain", "voxel-8", "voxel-3")
    val poison = new Counted(3, 2, None)
    val variants = Vector(
      neural.decoded.copy(keys = neural.decoded.keys.reverse),
      neural.decoded.copy(basis = "aligned"),
      neural.decoded.copy(units = "raw-signal"),
      neural.decoded.copy(scale = "zscore-training-v2"),
      neural.decoded.copy(namespace = "other-brain"),
      neural.decoded.copy(derivation = Vector("reordered"))
    )
    variants.foreach { declared =>
      assertEquals(
        bind(samples, neural, samples.decoded, declared, poison, revision),
        Left(Error.AxisMismatch("neural coordinates"))
      )
      val different = right(Axis.load(declared))
      assert(different.evidence.descriptor != neural.evidence.descriptor)
    }
    assert(
      bind(
        samples,
        neural,
        samples.decoded.copy(keys = samples.decoded.keys.reverse),
        neural.decoded,
        poison,
        revision
      ).isLeft
    )
    assertEquals(poison.reads, 0)

  test("coordinate identity is independent of source revision and dense/operator realization"):
    val samples = axis("trials", "a", "b")
    val brain = axis("brain", "u", "v")
    val reloaded = right(Axis.load(samples.decoded))
    assertEquals(reloaded.evidence.descriptor, samples.evidence.descriptor)
    val raw = DMat.dense(2, 2, Vector(2.0, 3.0, 5.0, 7.0))
    val counted = new Counted(2, 2, Some(raw))
    val dense = right(bind(samples, brain, samples.decoded, brain.decoded, raw, revision))
    val operator = right(bind(samples, brain, samples.decoded, brain.decoded, counted, revision))
    val revised = right(bind(samples, brain, samples.decoded, brain.decoded, raw, revision.copy(revision = "v2")))
    // LinDescriptor also includes representation: it is an inspection record,
    // not a suitable scientific identity/cache key in its entirety.
    assertEquals(dense.table.descriptor.domain, operator.table.descriptor.domain)
    assertEquals(dense.table.descriptor.codomain, operator.table.descriptor.codomain)
    assert(dense.table.descriptor.representation != operator.table.descriptor.representation)
    assertEquals(dense.table.valueIdentity, operator.table.valueIdentity)
    assert(dense.table.valueIdentity != revised.table.valueIdentity)
    assertEquals(counted.reads, 0)
    val actual = right(operator.table(DMat.eye(2)))
    for row <- 0 until 2; col <- 0 until 2 do assertEqualsDouble(actual(row, col), raw(row, col), 1e-12)
    assertEquals(counted.reads, 2)

  test("arbitrary reordering and repeated draws preserve parent keys and distinct occurrence addresses"):
    val parent = axis("trials", "trial-91", "trial-7", "trial-32")
    val space = right(IndexSpace.of(3))
    val injection = right(restrict(parent, right(Injection.from(IArray(2, 0), space))))
    val draw = right(restrict(parent, right(Draw.from(IArray(2, 0, 2), space))))
    assertEquals(injection.child.decoded.keys.map(_.value), Vector("trial-32", "trial-91"))
    assertEquals(draw.child.decoded.keys.map(_.value), Vector("trial-32", "trial-91", "trial-32"))
    assertEquals(draw.child.decoded.keys.map(_.occurrence), Vector(Vector(0), Vector(1), Vector(2)))
    assertEquals(draw.child.decoded.keys.distinct.length, 3)
    assertEquals(draw.responseSelection.values.toVector, Vector(2, 0, 2))
    assertEquals(draw.responseSelection.domain, parent.responseDomain)
    val target = right(column(parent, parent.decoded, Vector(91.0, 7.0, 32.0)))
    assertEquals(target.reindex(draw).values, Vector(32.0, 91.0, 32.0))
    val nested = right(restrict(draw.child, right(Draw.from(IArray(2, 2), right(IndexSpace.of(3))))))
    assertEquals(nested.child.decoded.keys.map(_.value), Vector("trial-32", "trial-32"))
    assertEquals(nested.child.decoded.keys.map(_.occurrence), Vector(Vector(2, 0), Vector(2, 1)))

  test("existing locus owners supply typed ordinal access to the same ordered keys without a registry"):
    val a = axis("trials", "trial-91", "trial-7")
    val b = right(Axis.load(a.decoded))
    assertEquals(a.keyAt(right(a.locus.value.index(1))), Key("trial-7"))
    assertEquals(a.locus.value.size, a.size)
    assert(!a.locus.value.isPersistable)
    assert(!a.locus.value.sameRuntimeOwnerAs(b.locus.value))
    assertEquals(a.evidence.descriptor, b.evidence.descriptor)
    assert(a.locus.value.index(2).isLeft)
    val wrongOwner = typeCheckErrors("""import scalafim.umvpaspike.IdentityPrototype.*
def invalid(a: Axis, b: Axis, point: locus4s.Index[b.locus.S]) = a.keyAt(point)
""")
    assert(wrongOwner.nonEmpty)

  test("reindexing kind participates in lineage even for identical ordinal mappings"):
    val parent = axis("trials", "a", "b", "c")
    val space = right(IndexSpace.of(3))
    val maps = Vector(
      right(restrict(parent, right(Selection.from(IArray(0, 1, 2), space)))),
      right(restrict(parent, right(Injection.from(IArray(0, 1, 2), space)))),
      right(restrict(parent, right(Draw.from(IArray(0, 1, 2), space)))),
      right(restrict(parent, right(Permutation.from(IArray(0, 1, 2)))))
    )
    assertEquals(maps.map(_.kind).distinct.length, 4)
    assertEquals(maps.map(_.child.evidence.descriptor).distinct.length, 4)
    assert(maps.forall(_.ordinals == Vector(0, 1, 2)))

  test("nested typed restrictions equal independently indexed multiresponse target values"):
    val parent = axis("trials", "a", "b", "c", "d")
    val features = axis("stimulus", "texture", "shape")
    val raw = DMat.dense(4, 2, Vector(2.0, -1.0, 8.0, 4.0, -3.0, 5.0, 6.0, 9.0))
    val target = right(
      responses(parent, features, parent.decoded, features.decoded, raw, SourceRevision("fixture:features", "v1"))
    )
    val first = right(restrict(parent, right(Injection.from(IArray(3, 0, 2), right(IndexSpace.of(4))))))
    val second = right(restrict(first.child, right(Selection.from(IArray(0, 2), right(IndexSpace.of(3))))))
    val nested = target.reindex(first).reindex(second)
    val actual = right(nested.table(DMat.eye(2)))
    val direct = target.table.andThen(first.leg.andThen(second.leg))
    val composed = right(direct(DMat.eye(2)))
    for (source, row) <- Vector(3, 2).zipWithIndex; col <- 0 until 2 do
      assertEqualsDouble(actual(row, col), raw(source, col), 1e-12)
      assertEqualsDouble(composed(row, col), raw(source, col), 1e-12)
    assert(
      responses(
        parent,
        features,
        parent.decoded.copy(keys = parent.decoded.keys.reverse),
        features.decoded,
        raw,
        revision
      ).isLeft
    )
    assert(
      responses(
        parent,
        features,
        parent.decoded,
        features.decoded.copy(keys = features.decoded.keys.reverse),
        raw,
        revision
      ).isLeft
    )

  test("heterogeneous local spaces package dependent measured tables without erased orientation"):
    val samples = axis("trials", "a", "b")
    val brain = axis("brain", "u", "v", "w")
    val one = axis("roi", "u")
    val two = axis("weighted-basis", "u-plus-v", "w-minus-v")
    val raw = DMat.dense(2, 3, Vector(1.0, 2.0, 5.0, 3.0, -1.0, 4.0))
    val counted = new Counted(2, 3, Some(raw))
    val evidence = right(bind(samples, brain, samples.decoded, brain.decoded, counted, revision))
    val frame: Vector[Measurement[brain.Id]] = Vector(
      right(measurement(brain, one, DMat.dense(1, 3, Vector(1.0, 0.0, 0.0)), valueId("roi-u"))),
      right(measurement(brain, two, DMat.dense(2, 3, Vector(1.0, 1.0, 0.0, 0.0, -1.0, 1.0)), valueId("basis-v1")))
    )
    val measured: Vector[Measured[samples.Id]] = frame.map(measure(evidence, _))
    assertEquals(counted.reads, 0)
    assertEquals(measured.map(_.local.size), Vector(1, 2))
    val values = measured.map(m => right(m.table(DMat.eye(m.local.size))))
    assertEqualsDouble(values(0)(0, 0), 1.0, 1e-12)
    assertEqualsDouble(values(0)(1, 0), 3.0, 1e-12)
    val expected = Vector(Vector(3.0, 3.0), Vector(2.0, 5.0))
    for row <- 0 until 2; col <- 0 until 2 do assertEqualsDouble(values(1)(row, col), expected(row)(col), 1e-12)
    assertEquals(counted.reads, 3)

  test("bounded metadata inspection and composition never read the neural payload"):
    val samples = axis("trials", "a", "b", "c")
    val brain = axis("brain", "u", "v")
    val local = axis("roi", "v")
    val poison = new Counted(3, 2, None)
    val evidence = right(bind(samples, brain, samples.decoded, brain.decoded, poison, revision))
    val inspected = right(evidence.inspect(1))
    assertEquals(inspected.preview, Vector(Key("a")))
    assertEquals(inspected.omitted, 2)
    assertEquals(inspected.neuralMoments, ValueKnowledge.Unknown)
    assertEquals(inspected.payloadVerification, PayloadVerification.CallerDeclared)
    assertEquals(right(evidence.inspect(0)).preview, Vector.empty)
    assert(evidence.inspect(-1).isLeft)
    assert(evidence.inspect(65).isLeft)
    val leg = right(measurement(brain, local, DMat.dense(1, 2, Vector(0.0, 1.0)), valueId("roi-v")))
    val measured = measure(evidence, leg)
    assertEquals(measured.table.rows, 3)
    assertEquals(measured.table.cols, 1)
    assertEquals(poison.reads, 0)
    interceptMessage[IllegalStateException]("poison neural read"):
      measured.table(DMat.eye(1))
    assertEquals(poison.reads, 1)

  test("malformed axes, targets, shapes and foreign ordinal domains fail at construction"):
    val a = axis("trials", "a", "b")
    assert(Axis.load(a.decoded.copy(keys = Vector.empty)).isLeft)
    assert(Axis.load(a.decoded.copy(keys = Vector.fill(2)(Key("a")))).isLeft)
    assert(Axis.load(a.decoded.copy(keys = Vector(Key("a", Vector(-1))))).isLeft)
    assert(Axis.load(a.decoded.copy(keys = Vector(Key("a", Vector.fill(33)(0))))).isLeft)
    assert(Axis.load(a.decoded.copy(keys = Vector.tabulate(65)(i => Key(i.toString)))).isLeft)
    assert(Axis.load(a.decoded.copy(units = "")).isLeft)
    assert(column(a, a.decoded.copy(keys = a.decoded.keys.reverse), Vector(1.0, 2.0)).isLeft)
    assert(column(a, a.decoded, Vector(1.0)).isLeft)
    assert(restrict(a, right(Selection.from(IArray(0), right(IndexSpace.of(3))))).isLeft)
    assert(bind(a, a, a.decoded, a.decoded, DMat.eye(3), revision).isLeft)
    assert(bind(a, a, a.decoded, a.decoded, DMat.eye(2), revision.copy(revision = "")).isLeft)
    assert(bind(a, a, a.decoded, a.decoded, DMat.eye(2), revision.copy(locator = "x" * 1025)).isLeft)

  test("positive and negative compile controls protect target and measurement axis boundaries"):
    val positive = typeCheckErrors("""import scalafim.umvpaspike.IdentityPrototype.*
import multivar.core.*
def valid[S <: SemanticSpace,F <: SemanticSpace](x: Responses[S,F], r: Restriction[S]): Table[r.child.Id,F] =
  x.reindex(r).table
def frame[S <: SemanticSpace,N <: SemanticSpace](x: Evidence[S,N], ms: Vector[Measurement[N]]): Vector[Measured[S]] =
  ms.map(m => measure(x,m))
""")
    val wrongColumn = typeCheckErrors("""import scalafim.umvpaspike.IdentityPrototype.*
import multivar.core.*
def invalid[S <: SemanticSpace,T <: SemanticSpace](x: Column[S], r: Restriction[T]) = x.reindex(r)
""")
    val wrongTarget = typeCheckErrors("""import scalafim.umvpaspike.IdentityPrototype.*
import multivar.core.*
def invalid[S <: SemanticSpace,T <: SemanticSpace,F <: SemanticSpace](x: Responses[S,F], r: Restriction[T]) = x.reindex(r)
""")
    val wrongFrame = typeCheckErrors("""import scalafim.umvpaspike.IdentityPrototype.*
import multivar.core.*
def invalid[S <: SemanticSpace,N <: SemanticSpace,O <: SemanticSpace](x: Evidence[S,N], m: Measurement[O]) = measure(x,m)
""")
    assertEquals(positive, Nil)
    assert(wrongColumn.nonEmpty)
    assert(wrongTarget.nonEmpty)
    assert(wrongFrame.nonEmpty)
