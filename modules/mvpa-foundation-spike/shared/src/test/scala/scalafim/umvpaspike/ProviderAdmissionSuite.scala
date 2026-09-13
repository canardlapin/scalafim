package scalafim.umvpaspike

import alder.kernel.*
import alder.data.{Split as AlderSplit, *}
import cats.Id
import cats.data.EitherT
import gale.linalg.DMat
import multivar.core.*
import resample4s.core.{Coverage, DigestAlgorithm, IndexSpace, Labels, Selection}
import resample4s.designs.{KFold, LeaveOneGroupOut}
import scala.compiletime.testing.typeCheckErrors

/** External-package fixtures: no access to alder's private protocol factories. */
object ProviderFixtures:
  val descriptor: ComponentDescriptor = ComponentDescriptor(
    ComponentId("scalafim.spike.visibility"),
    ComponentVersion("1"),
    AuditValue.record(),
    BackendFingerprint("analytic-spike", "1", AuditValue.record())
  )

  final class VisibilityEncoder extends FoldEncoder[Id, Int, Int, Unit, (Int, Boolean)]:
    type State = Set[Int]
    type FitError = Nothing
    type RunError = Nothing
    var encoded: Vector[(Int, Boolean)] = Vector.empty

    def fit[U <: Use.Fit](data: NonEmptyData[U, Example[Int, Int, Unit]])(using
        context: FitContext
    ): FitResult[Id, FitError, Trained[State]] =
      val targets = data.data.foldRows(Set.empty[Int])((seen, _, row) => seen + row.target)
      EitherT.rightT[Id, Failure[Nothing]](context.complete(targets, data, descriptor))

    def encode(state: State, input: Int): Either[Failure[Nothing], (Int, Boolean)] =
      val result = (input, state.contains(input))
      encoded = encoded :+ result
      Right(result)

  final class InputPair extends Transform[Id, Int, (Int, Boolean)]:
    type FitError = Nothing
    type RunError = Nothing
    type Fitted = Pipe[Int, Nothing, (Int, Boolean)]
    def fit[U <: Use.Fit](data: NonEmptyData[U, Int])(using
        context: FitContext
    ): FitResult[Id, FitError, Prepared[Preparation.Reusable, U, Fitted, (Int, Boolean)]] =
      val pipe: Fitted = Pipe.total(input => (input, false))
      EitherT.fromEither[Id](context.completeTransform(pipe, data, descriptor))

  final class RichModel(val seen: Vector[(Long, Int, Boolean)]) extends Pipe[(Int, Boolean), Nothing, Int]:
    val trainingKeys: Vector[Int] = seen.map(_._2)
    def run(value: (Int, Boolean)): Either[Failure[Nothing], Int] = Right(value._1)

  final class CollectLearner extends Learner[Id, (Int, Boolean), Int, Unit, Int]:
    type FitError = Nothing
    type RunError = Nothing
    type Model = RichModel

    def fit[U <: Use.Fit](data: NonEmptyData[U, Example[(Int, Boolean), Int, Unit]])(using
        context: FitContext
    ): FitResult[Id, FitError, Trained[Model]] =
      val rows = data.data.foldRows(Vector.empty[(Long, Int, Boolean)]) { (seen, id, row) =>
        seen :+ ((id.value, row.input._1, row.input._2))
      }
      EitherT.rightT[Id, Failure[Nothing]](context.complete(new RichModel(rows), data, descriptor))

class ProviderAdmissionSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def identityOf(label: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(label))

  test("typed row restriction and neural measurement match independent explicit multiplication"):
    val samples = right(SpaceRef.of("trials-a", SpaceRole.Samples, 4))
    val selected = right(SpaceRef.of("trials-a-selected", SpaceRole.Samples, 2))
    val brain = right(SpaceRef.of("voxels-a", SpaceRole.Observed, 3))
    val local = right(SpaceRef.of("roi-a", SpaceRole.Observed, 2))
    val raw = DMat.dense(4, 3, Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, -1.0, 8.0, 2.0, 9.0, 3.0, 7.0))
    val x: Table[samples.Id, brain.Id] = right(
      Lin.fromDenseMatrix(
        raw,
        CoordinateEvidence.dual(brain.evidence),
        CoordinateEvidence.primal(samples.evidence),
        identityOf("x")
      )
    )
    val l = right(
      Lin.fromDenseMatrix(
        DMat.dense(2, 3, Vector(1.0, 0.0, 0.0, 0.0, 0.5, -1.0)),
        CoordinateEvidence.primal(brain.evidence),
        CoordinateEvidence.primal(local.evidence),
        identityOf("l")
      )
    )
    val r = right(
      Lin.fromDenseMatrix(
        DMat.dense(2, 4, Vector(0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
        CoordinateEvidence.primal(samples.evidence),
        CoordinateEvidence.primal(selected.evidence),
        identityOf("r")
      )
    )
    val measured: Table[selected.Id, local.Id] = l.star.andThen(x).andThen(r)
    val value = right(measured(DMat.eye(2)))
    val sourceRows = Vector(2, 0)
    sourceRows.zipWithIndex.foreach { (source, target) =>
      assertEqualsDouble(value(target, 0), raw(source, 0), 1e-12)
      assertEqualsDouble(value(target, 1), raw(source, 1) * 0.5 - raw(source, 2), 1e-12)
    }
    val reversed = right(measured.star(DMat.eye(2)))
    for row <- 0 until 2; col <- 0 until 2 do assertEqualsDouble(reversed(row, col), value(col, row), 1e-12)
    assertEquals(measured.star.star.valueIdentity, measured.valueIdentity)

  test("nominal spaces and primal/dual orientation reject invalid composition at compile time"):
    val positive = typeCheckErrors("""import multivar.core.*
def valid[S <: SemanticSpace, N <: SemanticSpace, L <: SemanticSpace](
  x: Table[S,N], measurement: Lin[Primal[N],Primal[L]]
): Table[S,L] = measurement.star.andThen(x)
""")
    val wrongOrientation = typeCheckErrors("""import multivar.core.*
def invalid[S <: SemanticSpace, N <: SemanticSpace, L <: SemanticSpace](
  x: Table[S,N], measurement: Lin[Primal[N],Primal[L]]
) = measurement.andThen(x)
""")
    val wrongSpace = typeCheckErrors("""import multivar.core.*
def invalid[S <: SemanticSpace, N <: SemanticSpace, Other <: SemanticSpace, L <: SemanticSpace](
  x: Table[S,N], measurement: Lin[Primal[Other],Primal[L]]
) = measurement.star.andThen(x)
""")
    assertEquals(positive, Nil)
    assert(wrongOrientation.nonEmpty)
    assert(wrongSpace.nonEmpty)

  test("runtime decoder checks declared descriptor before accepting the same-shaped kernel"):
    val a = right(SpaceRef.of("axis-a", SpaceRole.Observed, 2))
    val b = right(SpaceRef.of("axis-b", SpaceRole.Observed, 2))
    val domain = CoordinateEvidence.primal(a.evidence)
    val other = CoordinateEvidence.primal(b.evidence)
    val result = Lin.decode(
      DMat.eye(2),
      domain,
      domain,
      other.descriptor,
      domain.descriptor,
      identityOf("foreign"),
      SemanticProvenance.source("decoded")
    )
    assert(result.isLeft)
    assert(Lin.fromDenseMatrix(DMat.eye(3), domain, domain, identityOf("bad-shape")).isLeft)

  test("grouped exact-once resampling preserves independent membership and deterministic receipts"):
    val groups = right(Labels.dense(IArray(2, 2, 0, 1, 0, 1), 6))
    val compiled = right(
      LeaveOneGroupOut(groups).compile(
        right(IndexSpace.of(6)),
        resample4s.core.Seed.fromLong(31L)
      )
    )
    val seen = Vector.newBuilder[Int]
    compiled.plan.iterator.foreach { (_, split) =>
      val assessment = split.assessment.toVector
      val analysis = split.analysis.toVector
      seen ++= assessment
      assertEquals(assessment.map(groups.toIArray(_)).distinct.length, 1)
      assertEquals(assessment.toSet.intersect(analysis.toSet), Set.empty[Int])
      assertEquals((assessment ++ analysis).sorted, Vector(0, 1, 2, 3, 4, 5))
    }
    assertEquals(seen.result().sorted, Vector(0, 1, 2, 3, 4, 5))
    val population = right(resample4s.core.SourceIdentity.of("fixture:trials", "v1"))
    val a = right(compiled.receipt(population)(using DigestAlgorithm.fnv1a64))
    val b = right(compiled.receipt(population)(using DigestAlgorithm.fnv1a64))
    assertEquals(a.assignment, b.assignment)
    val complete: CompleteResampler[Int] = Resample4sResampler.complete(compiled.plan, a)
    assert(complete.fingerprint.digest.nonEmpty)

  test("public batched data retains stable row keys and matrix views without copying brain rows"):
    val matrix = DMat.dense(3, 2, Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))
    final case class RowView(key: String, ordinal: Int):
      def at(column: Int): Double = matrix(ordinal, column)
    val rows = Vector(RowView("trial-20", 0), RowView("trial-9", 1), RowView("trial-80", 2))
    val source = InMemoryData.unsplit(rows, "matrix-v1")
    val observed = Vector.newBuilder[(Long, String, Double)]
    val sizes = Vector.newBuilder[Int]
    source.foreachBatch(BatchSize.const(2)) { batch =>
      sizes += batch.length
      for i <- 0 until batch.length do
        val value = batch.value(i)
        assert(value eq rows(value.ordinal))
        observed += ((batch.rowId(i).value, value.key, value.at(1)))
    }
    assertEquals(sizes.result(), Vector(2, 1))
    assertEquals(observed.result(), Vector((0L, "trial-20", 2.0), (1L, "trial-9", 4.0), (2L, "trial-80", 6.0)))

  test("stage-bound Alder cross-fitting excludes own targets in root and composed workflows"):
    val values = Vector.tabulate(10)(i => Example(i * 7 + 3, i * 7 + 3, ()))
    val split =
      right(AlderSplit.holdout(InMemoryData.unsplit(values, "keyed-targets-v1"), right(HoldoutSpec.rows(1)), Seed(91L)))
    val data = split.train
    val resampler =
      right(Resample4sResampler.fromDesign[Example[Int, Int, Unit]](KFold(3))(using DigestAlgorithm.fnv1a64))
    val encoder = new ProviderFixtures.VisibilityEncoder
    val feature = FeatureMap.crossFitted(encoder, resampler)
    given FitContext = FitContext.root(
      Seed(17L),
      PlanFingerprint("spike-crossfit"),
      SchemaFingerprint("keyed-int"),
      NumericMode.Deterministic
    )
    val prepared = right(feature.fit(data).value)
    val expected = data.data.foldRows(Vector.empty[(Long, Int)])((acc, id, row) => acc :+ ((id.value, row.input)))
    assertEquals(encoder.encoded.map(_._1).sorted, expected.map(_._2).sorted)
    assert(encoder.encoded.forall(!_._2))
    // Positive control: the serving state is fitted on all training targets.
    assertEquals(right(prepared.artifact.run(expected.head._2)), (expected.head._2, true))
    val wrongSeed = FitContext.root(
      Seed(18L),
      PlanFingerprint("spike-crossfit"),
      SchemaFingerprint("keyed-int"),
      NumericMode.Deterministic
    )
    assert(feature.fit(data)(using wrongSeed).value.isRight)

    val composedEncoder = new ProviderFixtures.VisibilityEncoder
    val composed = FeatureMap
      .crossFitted(composedEncoder, resampler)
      .learnWith(new ProviderFixtures.CollectLearner)
    val trained = right(composed.fit(data).value)
    val terminal = right(composed.terminalModel(trained))
    assertEquals(terminal.artifact.seen.map(v => (v._1, v._2)), expected)
    assert(terminal.artifact.seen.forall(!_._3))
    val lineage = trained.audit.preparation.crossFit.getOrElse(fail("missing cross-fit lineage"))
    val receipt = lineage.resample4s.getOrElse(fail("missing Resample4s receipt"))
    assert(lineage.seed != Seed(17L))
    assertEquals(receipt.planSeed, lineage.seed)

    val compiled = right(KFold(3).compile(right(IndexSpace.of(data.size.toInt)), resample4s.core.Seed.fromLong(17L)))
    val population = right(Resample4sResampler.populationFingerprint(data.fingerprint))
    val strict = right(
      Resample4sResampler.fromCompiled[Example[Int, Int, Unit]](compiled, population)(using DigestAlgorithm.fnv1a64)
    )
    val strictWorkflow = FeatureMap
      .crossFitted(
        new ProviderFixtures.VisibilityEncoder,
        strict
      )
      .learnWith(new ProviderFixtures.CollectLearner)
    strictWorkflow.fit(data).value match
      case Left(failure) =>
        failure.cause match
          case DataError.Resample4sSeedMismatch(expectedSeed, actualSeed) =>
            assertEquals(expectedSeed, 17L)
            assert(actualSeed != expectedSeed)
            assertEquals(failure.stage.segments, Vector(0))
          case other => fail("unexpected provider failure: " + other)
      case Right(_) => fail("strict prebound plan should reject the composed child seed")

  test("public target-blind preparation composes with a learner and preserves its rich fitted artifact"):
    val values = Vector.tabulate(10)(i => Example(i * 7 + 3, i * 7 + 3, ()))
    val data = right(
      AlderSplit.holdout(InMemoryData.unsplit(values, "rich-artifact-v1"), right(HoldoutSpec.rows(1)), Seed(91L))
    ).train
    val workflow = new ProviderFixtures.InputPair().learnWith(new ProviderFixtures.CollectLearner)
    given FitContext = FitContext.root(
      Seed(17L),
      PlanFingerprint("spike-rich-artifact"),
      SchemaFingerprint("keyed-int"),
      NumericMode.Deterministic
    )
    val trained = right(workflow.fit(data).value)
    val terminal = right(workflow.terminalModel(trained))
    val expected = data.data.foldRows(Vector.empty[(Long, Int)])((acc, id, row) => acc :+ ((id.value, row.input)))
    assertEquals(terminal.artifact.seen.map(v => (v._1, v._2)), expected)
    assert(terminal.artifact.seen.forall(!_._3))
    assertEquals(terminal.artifact.trainingKeys, expected.map(_._2))
    assertEquals(right(trained.artifact.run(1000)), 1000)
    assert(trained.audit.children.nonEmpty)

  test("external consumers cannot fit test rows, forge prepared values, or fit preprocessing after OOF preparation"):
    val testFit = typeCheckErrors("""import alder.kernel.*
import cats.Id
def invalid(t: Transform[Id,Double,Double], data: NonEmptyData[Use.Test,Double])(using FitContext) = t.fit(data)
""")
    val preparedRows = typeCheckErrors("""import alder.kernel.*
def invalid[S <: Preparation, U <: Use.Fit,A,B](x: Prepared[S,U,A,B]) = x.rows
""")
    val afterOof = typeCheckErrors("""import alder.kernel.*
import cats.Id
def invalid(f: FeatureMap[Id,Int,Int,Unit,Double], t: Transform[Id,Double,Double]) = f.andThen(t)
""")
    assert(testFit.nonEmpty)
    assert(preparedRows.nonEmpty)
    assert(afterOof.nonEmpty)
