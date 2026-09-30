package scalafim.fmri.mvpa.dataset.predictive

import multivar.core.SpaceRole
import munit.FunSuite
import scalafim.fmri.mvpa.AxisRef
import alder.kernel.DataFingerprint
import alder.data.*
import alder.kernel.*
import cats.Id
import cats.data.EitherT
import gale.linalg.DMat
import scala.compiletime.testing.typeCheckErrors
import resample4s.core.{DigestAlgorithm, Labels}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.{CrossFitDesign, ScientificSeed}

class AlderPredictiveAdmissionSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A =
    value.fold(error => fail(error.toString), identity)

  test("native mapping retains the complete ordered scientific key map") {
    val axis = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, Vector("run-b:2", "run-a:1"), "trial", "none", "one"))
    val mapping = right(NativeAxisMapping.fromAxis(axis, Vector(91L, 7L), DataFingerprint.external("fixture-v1")))

    assertEquals(mapping.entriesByOrdinal.map(_.stableKey), Vector("run-b:2", "run-a:1"))
    assertEquals(mapping.entriesByOrdinal.map(_.nativeId), Vector(91L, 7L))
    assertEquals(mapping.axis.coordinateSignature, axis.descriptor.coordinateSignature)
    assertEquals(right(NativeAxisMapping.verify(axis.descriptor, mapping, DataFingerprint.external("fixture-v1"))), ())
  }

  test("duplicate native ids are refused before a provider root exists") {
    val axis = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, Vector("s-1", "s-2"), "trial", "none", "one"))
    assert(NativeAxisMapping.fromAxis(axis, Vector(9L, 9L), DataFingerprint.external("fixture-v1")).isLeft)
  }

  test("materialization is budgeted before a matrix read") {
    val budget = right(MaterializationBudget(6L))
    assertEquals(right(MaterializationBudget.authorize(budget, 2, 2, 1)), MaterializationReceipt(2, 2, 1, 6L, 0L))

    val refused = MaterializationBudget.authorize(budget, 3, 2, 1)
    assert(refused.isLeft)
  }

  private final class TrackingPipe extends Pipe[Array[Double], Nothing, Int]:
    def run(input: Array[Double]): Either[Failure[Nothing], Int] = Right(input.length)

  private final class TrackingLearner extends Learner[Id, Array[Double], Array[Double], String, Int]:
    var observed: Vector[Long] = Vector.empty
    type FitError = Nothing
    type RunError = Nothing
    type Model = TrackingPipe
    def fit[U <: Use.Fit](data: NonEmptyData[U, Example[Array[Double], Array[Double], String]])(using context: FitContext): FitResult[Id, Nothing, Trained[TrackingPipe]] =
      observed = data.data.foldRows(Vector.empty[Long])((ids, id, _) => ids :+ id.value)
      EitherT.right(context.complete(
        new TrackingPipe,
        data,
        ComponentDescriptor(ComponentId("scalafim.test.tracking"), ComponentVersion("1"), AuditValue.record(), BackendFingerprint("test", "1", AuditValue.record()))
      ))

  test("fixed training fits a public Alder artifact with only training ids in its audit") {
    val axis = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, Vector("a", "b", "c", "d"), "trial", "none", "one"))
    val source = DataFingerprint.external("native-revision-1")
    val mapping = right(NativeAxisMapping.fromAxis(axis, Vector(30L, 10L, 40L, 20L), source))
    val budget = right(MaterializationBudget(16L))
    val rows = right(AlderPredictiveAdmission.materialized(
      axis.descriptor,
      DMat.dense(4, 1, Vector(1.0, 2.0, 3.0, 4.0)),
      DMat.dense(4, 1, Vector(10.0, 20.0, 30.0, 40.0)),
      Vector("train", "validation", "test", "train"), mapping, budget
    ))
    val split = right(rows.fixedTrainValidationTest(Vector(30L, 20L), Vector(10L), Vector(40L), FixedCoverage.Exhaustive))
    val learner = new TrackingLearner
    val context = FitContext.root(Seed(7L), PlanFingerprint("fixed-role-test"), SchemaFingerprint("array-input"), NumericMode.Deterministic)
    val trained = learner.fit(split.train)(using context).toEither.fold(error => fail(error.toString), identity)

    assertEquals(learner.observed, Vector(30L, 20L))
    assertEquals(trained.audit.data.policy, split.train.fingerprint.policy)
    assertEquals(trained.audit.data.digest, split.train.fingerprint.digest)
    assertNotEquals(trained.audit.data.digest, split.validation.fingerprint.digest)
    assertNotEquals(trained.audit.data.digest, split.test.fingerprint.digest)
    assertEquals(trained.artifact.run(Array(1.0)), Right(1))
  }

  test("test-role data cannot typecheck as a learner fitting population") {
    val errors = typeCheckErrors("""
import alder.data.*
import alder.kernel.*
import cats.Id
def invalid[A](held: FixedHoldout[Example[A, A, String]], learner: Learner[Id, A, A, String, Int])(using FitContext) =
  learner.fit(held.test)
""")
    assert(errors.nonEmpty)
  }

  private final class IdentityTransform extends Transform[Id, Array[Double], Array[Double]]:
    type FitError = Nothing
    type RunError = Nothing
    type Fitted = Pipe[Array[Double], Nothing, Array[Double]]
    def fit[U <: Use.Fit](data: NonEmptyData[U, Array[Double]])(using context: FitContext): FitResult[Id, Nothing, Prepared[Preparation.Reusable, U, Fitted, Array[Double]]] =
      EitherT.fromEither(context.completeTransform(
        Pipe.total(identity), data,
        ComponentDescriptor(ComponentId("scalafim.test.target-blind"), ComponentVersion("1"), AuditValue.record(), BackendFingerprint("test", "1", AuditValue.record()))
      ))

  private final class TraceEncoder extends FoldEncoder[Id, Array[Double], Array[Double], String, Set[Double]]:
    val fittedTargetSets = scala.collection.mutable.ArrayBuffer.empty[Set[Double]]
    type State = Set[Double]
    type FitError = Nothing
    type RunError = Nothing
    def fit[U <: Use.Fit](data: NonEmptyData[U, Example[Array[Double], Array[Double], String]])(using context: FitContext): FitResult[Id, Nothing, Trained[Set[Double]]] =
      val targets = data.data.foldRows(Set.empty[Double])((seen, _, example) => seen + example.target(0))
      fittedTargetSets += targets
      EitherT.right(context.complete(
        targets, data,
        ComponentDescriptor(ComponentId("scalafim.test.trace-encoder"), ComponentVersion("1"), AuditValue.record(), BackendFingerprint("test", "1", AuditValue.record()))
      ))
    def encode(state: Set[Double], input: Array[Double]): Either[Failure[Nothing], Set[Double]] = Right(state)

  private final class RecordingLearner extends Learner[Id, Set[Double], Array[Double], String, Set[Double]]:
    var observed = Vector.empty[(Long,Set[Double],Double,String)]
    type FitError = Nothing
    type RunError = Nothing
    type Model = Pipe[Set[Double],Nothing,Set[Double]]
    def fit[U <: Use.Fit](data: NonEmptyData[U,Example[Set[Double],Array[Double],String]])(using context: FitContext): FitResult[Id,Nothing,Trained[Model]] =
      observed = data.data.foldRows(Vector.empty[(Long,Set[Double],Double,String)])((rows,id,value) => rows :+ ((id.value,value.input,value.target(0),value.meta)))
      EitherT.right(context.complete(Pipe.total(identity),data,
        ComponentDescriptor(ComponentId("scalafim.test.recording"),ComponentVersion("1"),AuditValue.record(),BackendFingerprint("test","1",AuditValue.record()))))

  test("public transform is target-blind and cross-fitted feature preparation excludes each held target") {
    given DigestAlgorithm = DigestAlgorithm.fnv1a64
    val axis = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, Vector("a", "b", "c", "d"), "trial", "none", "one"))
    val source = DataFingerprint.external("native-revision-crossfit")
    val mapping = right(NativeAxisMapping.fromAxis(axis, Vector(30L, 10L, 40L, 20L), source))
    val rows = right(AlderPredictiveAdmission.materialized(axis.descriptor, DMat.dense(4, 1, Vector(1.0, 2.0, 3.0, 4.0)), DMat.dense(4, 1, Vector(10.0, 20.0, 30.0, 40.0)), Vector("a", "b", "c", "d"), mapping, right(MaterializationBudget(16L))))
    val train = right(rows.root.training(Vector(30L, 10L, 40L, 20L)))
    val labels = right(Labels.retained(IArray.unsafeFromArray(Array(0, 1, 0, 1))))
    val design = right(FixedPartitions.once(labels))
    val crossFit = right(CrossFitDesign.bind(axis, design, ScientificSeed.fromLong(17L)))
    val bridge = right(AlderPredictiveAdmission.crossFit(rows, crossFit))
    val transform = new IdentityTransform
    val transformed = FeatureMap.inputOnly[Id, Array[Double], Array[Double], String, Array[Double], transform.type](transform)
      .fit(train)(using FitContext.root(Seed(17L), PlanFingerprint("target-blind"), SchemaFingerprint("array"), NumericMode.Deterministic)).toEither
      .fold(error => fail(error.toString), identity)
    assertEquals(transformed.fitted.audit.data.digest, train.fingerprint.digest)

    val encoder = new TraceEncoder
    val feature = FeatureMap.crossFitted(encoder, bridge.resampler)
    val terminal = new RecordingLearner
    val learned = feature.learnWith(terminal)
    val fitted = learned.fit(train)(using FitContext.root(Seed(17L), PlanFingerprint("native-crossfit"), SchemaFingerprint("array"), NumericMode.Deterministic)).toEither.fold(error => fail(error.toString), identity)
    assertEquals(terminal.observed,Vector(
      (30L,Set(20.0,40.0),10.0,"a"),
      (10L,Set(10.0,30.0),20.0,"b"),
      (40L,Set(20.0,40.0),30.0,"c"),
      (20L,Set(10.0,30.0),40.0,"d")
    ))
    terminal.observed.foreach((_,encoded,target,_) => assert(!encoded.contains(target)))
    assertEquals(encoder.fittedTargetSets.take(2).toSet,Set(Set(20.0,40.0),Set(10.0,30.0)))
    assert(fitted.audit.children.nonEmpty)
    assertEquals(right(fitted.artifact.run(Array(1.0))),Set(10.0,20.0,30.0,40.0))
    val reordered = right(rows.root.training(Vector(10L,30L,40L,20L)))
    assert(feature.fit(reordered)(using FitContext.root(Seed(17L),PlanFingerprint("foreign"),SchemaFingerprint("array"),NumericMode.Deterministic)).toEither.isLeft)

  }

  test("target-blind transform inputs have no target member at compile time") {
    val errors = typeCheckErrors("""
import alder.kernel.*
def invalid[U <: Use.Fit](data: NonEmptyData[U, Array[Double]]) =
  data.data.foldRows(0)((_, _, input) => input.target.length)
""")
    assert(errors.nonEmpty)
  }

  test("large dimensions use Long budget arithmetic and actual materialized binding refuses overspend"):
    assert(MaterializationBudget.authorize(right(MaterializationBudget(10L)),Int.MaxValue,Int.MaxValue,Int.MaxValue).isLeft)
    val axis = right(AxisRef.fromStableKeys("samples",SpaceRole.Samples,Vector("a","b"),"trial","none","one"))
    val mapping = right(NativeAxisMapping.fromAxis(axis,Vector(7L,9L),DataFingerprint.external("budget")))
    assert(AlderPredictiveAdmission.materialized(axis.descriptor,DMat.eye(2),DMat.eye(2),Vector("a","b"),mapping,right(MaterializationBudget(7L))).isLeft)
    val admitted = right(AlderPredictiveAdmission.materialized(axis.descriptor,DMat.eye(2),DMat.eye(2),Vector("a","b"),mapping,right(MaterializationBudget(8L))))
    assertEquals(admitted.receipt.copiedCells,8L)

  test("native-table budget refusal precedes poison evidence reads"):
    import gale.linalg.{DoubleLinearOperator,DVec,MutableDVec}
    import multivar.core.{ValueIdentity,ValueId}
    import scalafim.fmri.mvpa.{EvidenceSource,Observations,MultiResponse}
    import scalafim.response.{SourceId,ProvenanceId,Provenance}
    val samples = right(AxisRef.fromStableKeys("samples",SpaceRole.Samples,Vector("a","b"),"trial","none","one"))
    val features = right(AxisRef.fromStableKeys("features",SpaceRole.Observed,Vector("u","v"),"native","none","one"))
    var reads = 0
    val poison = new DoubleLinearOperator:
      val rows = 2
      val cols = 2
      def applyTo(input: DVec,output: MutableDVec): Unit =
        reads += 1
        throw IllegalStateException("poison")
    val sourceId = SourceId.unsafe("poison-source")
    val source = right(EvidenceSource(sourceId,Provenance.source(ProvenanceId.unsafe("poison-root"),sourceId)))
    val observations = right(Observations.fromOperator(samples,features,poison,ValueIdentity.source(ValueId.unsafe("poison-values")),source))
    val targets = right(MultiResponse.fromDense(samples,features,DMat.eye(2),ValueIdentity.source(ValueId.unsafe("target-values")),source))
    val mapping = right(NativeAxisMapping.fromAxis(samples,Vector(10L,20L),DataFingerprint.external("poison-declaration")))
    val policy = right(NativeReadPolicy(2,right(MaterializationBudget(1L))))
    AlderPredictiveAdmission.nativeTables(observations,targets,Vector("a","b"),DataFingerprint.external("poison-metadata"),mapping,policy) match
      case Left(AlderPredictiveAdmissionError.MaterializationOverBudget(_,_,1L)) => ()
      case other => fail(s"expected native budget refusal, got $other")
    assertEquals(reads,0)
