package scalafim.fmri.mvpa.analysis

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import munit.FunSuite
import multivar.core.{SemanticSpace, SpaceEvidence, SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{Coverage, DigestAlgorithm, IndexSpace, Injection}
import resample4s.designs.KFold
import scalafim.fmri.mvpa.{AxisRef, AxisSignature, EvidenceSource, Observations, ScientificSeed, ValidationDesign}
import scalafim.fmri.mvpa.measurement.{MeasurementFrame, MeasurementFrameDeclaration, MeasurementId, MeasurementLeg, PackedMeasurementEntry}
import scalafim.response.{OperationId, Provenance, ProvenanceId, ProvenanceOperation, SourceId}
import scala.compiletime.testing.typeCheckErrors

final class CompilationSuite extends FunSuite:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private val sourceAxis = AxisSignature.unsafe("a" * 64)
  private val designAxisValue = AxisSignature.unsafe("b" * 64)
  private val frameAxisValue = AxisSignature.unsafe("c" * 64)

  private final case class TestSource(revision: String, capabilities: CapabilitySet)
  private final case class TestDesign(realizedSplits: String, seed: String)
  private final case class TestFrame(mapFingerprint: String)

  private given ScientificInputs[TestSource, TestDesign, TestFrame] with
    def sourceAxes(source: TestSource) = Vector(sourceAxis)
    def sourceIdentity(source: TestSource) = s"source:${source.revision}"
    def sourceCapabilities(source: TestSource) = source.capabilities
    def designAxis(design: TestDesign) = designAxisValue
    def designIdentity(design: TestDesign) = s"splits:${design.realizedSplits}|seed:${design.seed}"
    def frameAxis(frame: TestFrame) = frameAxisValue
    def frameIdentity(frame: TestFrame) = s"map:${frame.mapFingerprint}"

  private object TestEstimand extends Estimand[TestSource, TestDesign, TestFrame]:
    type Result = Double
    type Rejection = String
    type Failure = String
    val id: EstimandId = EstimandId("test-estimand")
    val requiredCapabilities: Set[CapabilityId] = Set(CapabilityId("replay"))
    override val parameters = Vector("target-coding" -> "condition", "metric" -> "accuracy")

  private given AnalysisCompiler[TestSource, TestDesign, TestFrame, TestEstimand.type] with
    type Bound = String
    type Program = String
    def bind(specification: AnalysisSpecification[TestSource, TestDesign, TestFrame, TestEstimand.type], available: CapabilitySet) =
      Right(s"${specification.source.revision}:${specification.design.realizedSplits}")
    def numerical(scientific: BoundScientificPlan[TestSource, TestDesign, TestFrame, TestEstimand.type, String, String]) =
      NumericalProgram(scientific, "mean", "test-mean", Vector("sum", "count"))

  private val replay = Capability(CapabilityId("replay"), "actual repeatable evidence source")

  private def specification(
      source: TestSource = TestSource("bold-v1", CapabilitySet.from(Vector(replay))),
      design: TestDesign = TestDesign("folds-v1", "seed-1"),
      frame: TestFrame = TestFrame("weights-v1")
  ) = AnalysisSpecification.from(source, design, frame, TestEstimand, "mean response", Vector("finite values"), Vector("center within run"), "pooled trial")

  private def receipt(scientific: BoundScientificPlan[TestSource, TestDesign, TestFrame, TestEstimand.type, String, String]) =
    ExecutionReceipt(scientific.receipt, EvidenceReceipt("source-v1", Vector("block-0"), None), RealizationReceipt("portable", "test-kernel", "binary64", Vector("seed-1"), Vector.empty, "serial"), 1, 2)

  test("an open test-only estimand binds and compiles without a registry"):
    val scientific = AnalysisCompiler.bind(specification(), CapabilitySet.from(Vector(replay))).toOption.get
    val program = AnalysisCompiler.compile(scientific)
    assertEquals(scientific.receipt.estimand, TestEstimand.id)
    assertEquals(program.program, "mean")
    assertEquals(program.algorithm, "test-mean")

  test("actual source capability is required; a caller label cannot admit a poison source"):
    val poison = specification(source = TestSource("poison-v1", CapabilitySet.empty))
    assertEquals(AnalysisCompiler.bind(poison, CapabilitySet.from(Vector(replay))), Left(BindError.Unsupported(Set(CapabilityId("replay")))))

  test("source revision, realized splits, map weights, and estimand parameters all change the plan"):
    val base = specification()
    val sourceChanged = specification(source = TestSource("bold-v2", CapabilitySet.from(Vector(replay))))
    val splitsChanged = specification(design = TestDesign("folds-v2", "seed-1"))
    val seedChanged = specification(design = TestDesign("folds-v1", "seed-2"))
    val mapChanged = specification(frame = TestFrame("weights-v2"))
    object AlternateParameters extends Estimand[TestSource, TestDesign, TestFrame]:
      type Result = Double
      type Rejection = String
      type Failure = String
      val id = TestEstimand.id
      val requiredCapabilities = TestEstimand.requiredCapabilities
      override val parameters = Vector("target-coding" -> "condition", "metric" -> "balanced-accuracy")
    val parametersChanged = AnalysisSpecification.from(TestSource("bold-v1", CapabilitySet.from(Vector(replay))), TestDesign("folds-v1", "seed-1"), TestFrame("weights-v1"), AlternateParameters, "mean response", Vector("finite values"), Vector("center within run"), "pooled trial")
    Vector(sourceChanged, splitsChanged, seedChanged, mapChanged).foreach(changed => assertNotEquals(base.plan, changed.plan))
    assertNotEquals(base.plan, parametersChanged.plan)

  test("schedule changes preserve scientific identity"):
    val scientific = AnalysisCompiler.bind(specification(), CapabilitySet.from(Vector(replay))).toOption.get
    val program = AnalysisCompiler.compile(scientific)
    val serial = ExecutionPlan(program, ExecutionStrategy("jvm", "serial", 1, () => false))
    val parallel = ExecutionPlan(program, ExecutionStrategy("js", "parallel", 32, () => false))
    assertEquals(serial.numerical.scientific.specification.plan, parallel.numerical.scientific.specification.plan)

  test("result completion is bound to the estimand result type and receipt"):
    val scientific = AnalysisCompiler.bind(specification(), CapabilitySet.from(Vector(replay))).toOption.get
    val result: AnalysisResult[Double] = AnalysisResult.complete(scientific, 0.0, receipt(scientific))
    assertEquals(result.value, 0.0)

  test("result completion rejects String for a Double estimand at compile time"):
    val errors = typeCheckErrors("""
      import scalafim.fmri.mvpa.analysis.*
      object WrongResult:
        final case class Source(capabilities: CapabilitySet)
        given ScientificInputs[Source, Unit, Unit] with
          def sourceAxes(source: Source) = Vector(scalafim.fmri.mvpa.AxisSignature.unsafe("a" * 64))
          def sourceIdentity(source: Source) = "source-v1"
          def sourceCapabilities(source: Source) = source.capabilities
          def designAxis(design: Unit) = scalafim.fmri.mvpa.AxisSignature.unsafe("b" * 64)
          def designIdentity(design: Unit) = "splits-v1|seed-1"
          def frameAxis(frame: Unit) = scalafim.fmri.mvpa.AxisSignature.unsafe("c" * 64)
          def frameIdentity(frame: Unit) = "weights-v1"
        object E extends Estimand[Source, Unit, Unit]:
          type Result = Double
          type Rejection = String
          type Failure = String
          val id = EstimandId("wrong-result")
          val requiredCapabilities = Set.empty[CapabilityId]
        given AnalysisCompiler[Source, Unit, Unit, E.type] with
          type Bound = Unit
          type Program = Unit
          def bind(specification: AnalysisSpecification[Source, Unit, Unit, E.type], available: CapabilitySet) = Right(())
          def numerical(scientific: BoundScientificPlan[Source, Unit, Unit, E.type, Unit, Unit]) = NumericalProgram(scientific, (), "test", Vector.empty)
        val plan = AnalysisCompiler.bind(AnalysisSpecification.from(Source(CapabilitySet.empty), (), (), E, "q", Vector("a"), Vector.empty, "none"), CapabilitySet.empty).toOption.get
        AnalysisResult.complete(plan, "not-a-double", null)
    """)
    assertEquals(errors.length, 1)
    assert(errors.head.message.contains("String"))
    assert(errors.head.message.contains("Double") || errors.head.message.contains("estimand.Result"), errors.head.message)
    assert(errors.head.lineContent.contains("AnalysisResult.complete"), errors.head.lineContent)

  test("dependent entries retain distinct nominal measurement spaces without casts"):
    def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)
    val neural = right(AxisRef.fromStableKeys("neural", SpaceRole.Observed, Vector("a", "b", "c"), "voxel", "psc", "raw", Vector("fixture")))
    val whole = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("whole")))
    val domain = right(IndexSpace.of(neural.size))
    val tail = right(MeasurementLeg.hardSelection(neural, MeasurementId.unsafe("tail"), right(Injection.from(IArray(2), domain))))
    final case class LocalArtifact[L <: SemanticSpace](label: String, local: SpaceEvidence[L])
    def entry[L <: SemanticSpace](id: String, leg: MeasurementLeg[neural.Id, String, L]) =
      FrameEntry[String, String, LocalArtifact, L, String, String, String](id, leg.descriptor.semanticId, ProductState.Valid(LocalArtifact[L](id, leg.local.evidence)))
    def failed[L <: SemanticSpace](id: String, leg: MeasurementLeg[neural.Id, String, L]) =
      FrameEntry[String, String, LocalArtifact, L, String, String, String](id, leg.descriptor.semanticId, ProductState.Failed("measurement failed"))
    val entries: Vector[FrameEntry[String, String, LocalArtifact, String, String, String]] = Vector(entry("whole", whole), entry("tail", tail), failed("failed-tail", tail))
    assertNotEquals(whole.local.descriptor, tail.local.descriptor)
    entries.head.state match
      case ProductState.Valid(artifact) => assertEquals(artifact.local.dimension, 3)
      case other => fail(s"expected typed local payload, got $other")
    entries(1).state match
      case ProductState.Valid(artifact) => assertEquals(artifact.local.dimension, 1)
      case other => fail(s"expected second typed local payload, got $other")
    assertEquals(entries(2).state, ProductState.Failed("measurement failed"))

  test("native metadata distinguishes structural value and provenance lineage without reading a one-shot operator"):
    def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)
    final class Poison extends DoubleLinearOperator:
      val rows = 2
      val cols = 3
      var applications = 0
      def applyTo(input: DVec, output: MutableDVec): Unit =
        applications += 1
        throw new IllegalStateException("one-shot payload was read")
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = applyTo(input, output)
    def axis(namespace: String, role: SpaceRole, keys: Vector[String]) =
      right(AxisRef.fromStableKeys(namespace, role, keys, "native", "psc", "raw", Vector("fixture")))
    def source(id: String) =
      right(EvidenceSource(SourceId.unsafe(id), Provenance.source(ProvenanceId.unsafe(s"$id-root"), SourceId.unsafe(id))))
    val samples = axis("samples", SpaceRole.Samples, Vector("s1", "s2"))
    val neural = axis("neural", SpaceRole.Observed, Vector("a", "b", "c"))
    val firstOperator = new Poison
    val secondOperator = new Poison
    val direct = right(Observations.fromOperator(samples, neural, firstOperator, ValueIdentity.source(ValueId.unsafe("star-x")), source("native-source")))
    val adjoint = right(Observations.fromOperator(samples, neural, secondOperator, ValueIdentity.Adjoint(ValueIdentity.source(ValueId.unsafe("x"))), source("native-source")))
    val provenanceRoot = Provenance.source(ProvenanceId.unsafe("lineage-root"), SourceId.unsafe("native-source"))
    val derivedProvenance = right(Provenance.derive(provenanceRoot, ProvenanceId.unsafe("lineage-derived"), ProvenanceOperation.Derived(OperationId.unsafe("adapter"))))
    val lineageChanged = right(EvidenceSource(SourceId.unsafe("native-source"), derivedProvenance))
    val thirdOperator = new Poison
    val derived = right(Observations.fromOperator(samples, neural, thirdOperator, ValueIdentity.source(ValueId.unsafe("star-x")), lineageChanged))
    val designOne = right(ValidationDesign.bind(samples, KFold.ordered(2), ScientificSeed.fromLong(11L)))
    val designTwo = right(ValidationDesign.bind(samples, KFold.ordered(2), ScientificSeed.fromLong(11L)))
    val changedSeed = right(ValidationDesign.bind(samples, KFold.ordered(2), ScientificSeed.fromLong(12L)))
    val whole = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("whole")))
    val frame = right(MeasurementFrame(neural, Vector(PackedMeasurementEntry(whole, "whole"))))
    val inputs = summon[ScientificInputs[Observations[samples.Id, neural.Id], ValidationDesign[samples.Id, String, Coverage.ExactOnce], MeasurementFrame[neural.Id, String, String]]]
    assertNotEquals(inputs.sourceIdentity(direct), inputs.sourceIdentity(adjoint))
    assertNotEquals(inputs.sourceIdentity(direct), inputs.sourceIdentity(derived))
    assertEquals(inputs.designIdentity(designOne), inputs.designIdentity(designTwo))
    assertNotEquals(inputs.designIdentity(designOne), inputs.designIdentity(changedSeed))
    val duplicateNameForward = MeasurementFrame.lazyFrame(
      neural,
      MeasurementFrameDeclaration("duplicate-parameters", "v1", Vector("radius" -> "3", "radius" -> "4"))
    )(Iterator.empty[PackedMeasurementEntry[neural.Id, String, String]])
    val duplicateNameReverse = MeasurementFrame.lazyFrame(
      neural,
      MeasurementFrameDeclaration("duplicate-parameters", "v1", Vector("radius" -> "4", "radius" -> "3"))
    )(Iterator.empty[PackedMeasurementEntry[neural.Id, String, String]])
    assertEquals(inputs.frameIdentity(duplicateNameForward), inputs.frameIdentity(duplicateNameReverse))
    assertEquals(firstOperator.applications, 0)
    assertEquals(secondOperator.applications, 0)
    assertEquals(thirdOperator.applications, 0)

  test("native one-shot observations cannot be admitted as replayable without applying the operator"):
    def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)
    final class Poison extends DoubleLinearOperator:
      val rows = 2
      val cols = 3
      var applications = 0
      def applyTo(input: DVec, output: MutableDVec): Unit =
        applications += 1
        throw new IllegalStateException("one-shot payload was read")
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = applyTo(input, output)
    val samples = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, Vector("s1", "s2"), "native", "psc", "raw", Vector("fixture")))
    val neural = right(AxisRef.fromStableKeys("neural", SpaceRole.Observed, Vector("a", "b", "c"), "native", "psc", "raw", Vector("fixture")))
    val poison = new Poison
    val source = right(EvidenceSource(SourceId.unsafe("one-shot"), Provenance.source(ProvenanceId.unsafe("one-shot-root"), SourceId.unsafe("one-shot"))))
    val observations = right(Observations.fromOperator(samples, neural, poison, ValueIdentity.source(ValueId.unsafe("one-shot-values")), source))
    val design = right(ValidationDesign.bind(samples, KFold.ordered(2), ScientificSeed.fromLong(3L)))
    val whole = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("whole")))
    val frame = right(MeasurementFrame(neural, Vector(PackedMeasurementEntry(whole, "whole"))))
    object ReplayEstimand extends Estimand[Observations[samples.Id, neural.Id], ValidationDesign[samples.Id, String, Coverage.ExactOnce], MeasurementFrame[neural.Id, String, String]]:
      type Result = Double
      type Rejection = String
      type Failure = String
      val id = EstimandId("native-replay")
      val requiredCapabilities = Set(CapabilityId("replay"))
    var bound = false
    given AnalysisCompiler[Observations[samples.Id, neural.Id], ValidationDesign[samples.Id, String, Coverage.ExactOnce], MeasurementFrame[neural.Id, String, String], ReplayEstimand.type] with
      type Bound = Unit
      type Program = Unit
      def bind(specification: AnalysisSpecification[Observations[samples.Id, neural.Id], ValidationDesign[samples.Id, String, Coverage.ExactOnce], MeasurementFrame[neural.Id, String, String], ReplayEstimand.type], available: CapabilitySet) =
        bound = true
        Right(())
      def numerical(scientific: BoundScientificPlan[Observations[samples.Id, neural.Id], ValidationDesign[samples.Id, String, Coverage.ExactOnce], MeasurementFrame[neural.Id, String, String], ReplayEstimand.type, Unit, Unit]) =
        NumericalProgram(scientific, (), "unused", Vector.empty)
    val specification = AnalysisSpecification.from(observations, design, frame, ReplayEstimand, "q", Vector("a"), Vector.empty, "none")
    assertEquals(AnalysisCompiler.bind(specification, CapabilitySet.from(Vector(replay))), Left(BindError.Unsupported(Set(CapabilityId("replay")))))
    assert(!bound)
    assertEquals(poison.applications, 0)

  test("pooled-trial and mean-run accuracy retain distinct denominators"):
    val runs = Vector(RunAccuracy("short", 1, 1), RunAccuracy("long", 50, 100), RunAccuracy("empty", 0, 0))
    val pooled = AccuracyReduction.pooledTrial(runs) match
      case AccuracyReductionResult.Valid(value) => value
      case other => fail(s"expected valid pooled result, got $other")
    val mean = AccuracyReduction.meanRun(runs) match
      case AccuracyReductionResult.Valid(value) => value
      case other => fail(s"expected valid mean-run result, got $other")
    assertEqualsDouble(pooled.value, 51.0 / 101.0, 1e-12)
    assertEqualsDouble(mean.value, 0.75, 1e-12)
    assertEquals(pooled.evidence.exactNumerator, Some(BigInt(51)))
    assertEquals(pooled.evidence.exactDenominator, BigInt(101))

  test("pooled reduction retains exact totals beyond Long range"):
    AccuracyReduction.pooledTrial(Vector(RunAccuracy("a", Long.MaxValue, Long.MaxValue), RunAccuracy("b", 1, 1))) match
      case AccuracyReductionResult.Valid(value) =>
        assertEquals(value.evidence.exactDenominator, BigInt(Long.MaxValue) + 1)
        assertEquals(value.evidence.exactNumerator, Some(BigInt(Long.MaxValue) + 1))
        assertEqualsDouble(value.value, 1.0, 0.0)
      case other => fail(s"expected exact reduction, got $other")

  test("empty and duplicate reductions are not numerical results"):
    assert(AccuracyReduction.pooledTrial(Vector(RunAccuracy("empty", 0, 0))).isInstanceOf[AccuracyReductionResult.NonEstimable[?, ?]])
    assertEquals(AccuracyReduction.meanRun(Vector(RunAccuracy("same", 1, 1), RunAccuracy("same", 1, 1))), AccuracyReductionResult.DuplicateContributor("same"))
