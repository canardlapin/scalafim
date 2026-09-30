package scalafim.fmri.mvpa.analysis

import gale.linalg.{DVec, DoubleLinearOperator, MutableDVec}
import munit.FunSuite
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{Coverage, DigestAlgorithm}
import resample4s.designs.KFold
import scalafim.fmri.mvpa.{AxisRef, AxisSignature, EvidenceSource, Observations, ScientificSeed, ValidationDesign}
import scalafim.fmri.mvpa.measurement.{MeasurementFrame, MeasurementId, MeasurementLeg, PackedMeasurementEntry}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

final class DiagnosticsSuite extends FunSuite:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private final class PoisonSource:
    var reads = 0
    var capabilities = CapabilitySet.empty
    def readValues(): Nothing =
      reads += 1
      throw new IllegalStateException("values must not be read by diagnostics")

  private final case class Design(identity: String)
  private final case class Frame(identity: String)

  private given ScientificInputs[PoisonSource, Design, Frame] with
    def sourceAxes(source: PoisonSource) = Vector(AxisSignature.unsafe("a" * 64))
    def sourceIdentity(source: PoisonSource) = "declared-source-v1"
    def sourceCapabilities(source: PoisonSource) = source.capabilities
    def designAxis(design: Design) = AxisSignature.unsafe("b" * 64)
    def designIdentity(design: Design) = design.identity
    def frameAxis(frame: Frame) = AxisSignature.unsafe("c" * 64)
    def frameIdentity(frame: Frame) = frame.identity

  private object EstimandUnderTest extends Estimand[PoisonSource, Design, Frame]:
    type Result = Double
    type Rejection = String
    type Failure = String
    val id = EstimandId("diagnostic-test")
    val requiredCapabilities = Set(CapabilityId("replay"))

  private def specification(design: String = "folds-v1", frame: String = "frame-v1") =
    AnalysisSpecification.from(new PoisonSource, Design(design), Frame(frame), EstimandUnderTest, "mean response", Vector("finite values"), Vector("center within run"), "pooled trial")

  private val replayAvailable = CapabilitySet.from(Vector(Capability(CapabilityId("replay"), "test replay")))

  test("describe and explain only inspect cached metadata"):
    val poison = new PoisonSource
    val plan = AnalysisSpecification.from(poison, Design("folds-v1"), Frame("frame-v1"), EstimandUnderTest, "mean response", Vector("finite values"), Vector("center within run"), "pooled trial")
    val description = Diagnostics.describe(plan, CapabilitySet.empty)
    val explanation = Diagnostics.explain(plan, CapabilitySet.empty)
    assertEquals(description.sourceIdentity, "declared-source-v1")
    assertEquals(description.unknowns.length, 1)
    assertEquals(explanation.blockers.map(_.code), Vector("analysis.missing-source-capability"))
    assert(explanation.nextOperations.exists(_.contains("capability")))
    assertEquals(poison.reads, 0)

  test("native diagnostic binding does not apply a poison evidence operator"):
    def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)
    final class PoisonOperator extends DoubleLinearOperator:
      val rows = 2
      val cols = 3
      var applications = 0
      def applyTo(input: DVec, output: MutableDVec): Unit =
        applications += 1
        throw new IllegalStateException("diagnostics must not apply evidence")
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = applyTo(input, output)
    val samples = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, Vector("s1", "s2"), "native", "psc", "raw"))
    val neural = right(AxisRef.fromStableKeys("neural", SpaceRole.Observed, Vector("n1", "n2", "n3"), "native", "psc", "raw"))
    object NativeEstimand extends Estimand[Observations[samples.Id, neural.Id], ValidationDesign[samples.Id, String, Coverage.ExactOnce], MeasurementFrame[neural.Id, String, String]]:
      type Result = Double
      type Rejection = String
      type Failure = String
      val id = EstimandId("native-diagnostic")
      val requiredCapabilities = Set.empty[CapabilityId]
    val operator = new PoisonOperator
    val source = right(EvidenceSource(SourceId.unsafe("poison-source"), Provenance.source(ProvenanceId.unsafe("poison-root"), SourceId.unsafe("poison-source"))))
    val observations = right(Observations.fromOperator(samples, neural, operator, ValueIdentity.source(ValueId.unsafe("poison-values")), source))
    val design = right(ValidationDesign.bind(samples, KFold.ordered(2), ScientificSeed.fromLong(11L)))
    val whole = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("whole")))
    val frame = right(MeasurementFrame(neural, Vector(PackedMeasurementEntry(whole, "whole"))))
    val plan = AnalysisSpecification.from(observations, design, frame, NativeEstimand, "q", Vector("finite"), Vector.empty, "pooled")
    Diagnostics.describe(plan, CapabilitySet.empty)
    Diagnostics.explain(plan, CapabilitySet.empty)
    assertEquals(operator.applications, 0)

  test("large axis inspection is bounded and continuation-driven"):
    def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)
    val keys = Vector.tabulate(10000)(index => f"voxel-$index%05d")
    val axis = right(AxisRef.fromStableKeys("large", SpaceRole.Observed, keys, "voxel", "psc", "raw"))
    val first = AxisPage.inspect(axis, 128).toOption.get
    val second = AxisPage.inspect(axis, 128, first.next).toOption.get
    assertEquals(first.entries.length, 128)
    assertEquals(first.entries.head, 0 -> "voxel-00000")
    assert(first.next.nonEmpty)
    assertEquals(second.entries.head, 128 -> "voxel-00128")
    assertEquals(AxisPage.inspect(axis, 0), Left(DiagnosticError.InvalidLimit(0)))
    assertEquals(AxisPage.inspect(axis, Int.MaxValue), Left(DiagnosticError.LimitExceedsMaximum(Int.MaxValue, 1024)))
    val other = right(AxisRef.fromStableKeys("other", SpaceRole.Observed, Vector("x", "y"), "voxel", "psc", "raw"))
    assertEquals(AxisPage.inspect(other, 1, first.next), Left(DiagnosticError.ContinuationTargetMismatch))

  test("identity evidence never overclaims complete verification"):
    assertEquals(Diagnostics.inspectIdentity(EvidenceReceipt("declared-v1", Vector.empty, None)), ContentIdentity.Declared("declared-v1"))
    assertEquals(Diagnostics.inspectIdentity(EvidenceReceipt("declared-v1", Vector("block-2"), None)), ContentIdentity.ProviderReportedBlocks("declared-v1", Vector("block-2")))
    assertEquals(Diagnostics.inspectIdentity(EvidenceReceipt("declared-v1", Vector("block-2"), Some("sha256:complete"))), ContentIdentity.ProviderReportedComplete("declared-v1", Vector("block-2"), "sha256:complete"))

  test("structural plan differences require rebind and repairs preserve scientific scope"):
    val base = specification()
    val changedDesign = specification(design = "folds-v2")
    val changedFrame = specification(frame = "frame-v2")
    val designDiff = Diagnostics.diff(base, changedDesign)
    val frameDiff = Diagnostics.diff(base, changedFrame)
    assertEquals(designDiff.changes, Set(PlanChange.DesignIdentity))
    assertEquals(frameDiff.changes, Set(PlanChange.FrameIdentity))
    assert(designDiff.rebindRequired)
    assertEquals(RepairAdvice.forKind(RepairKind.EquivalentExecution).rebindRequired, false)
    assert(RepairAdvice.forKind(RepairKind.ChangedEstimator).rebindRequired)
    assert(!RepairAdvice.forKind(RepairKind.RoiShrink).legal)

  test("open estimand metadata is captured once and never re-entered by diagnostics"):
    final class MutableEstimand extends Estimand[PoisonSource, Design, Frame]:
      type Result = Double
      type Rejection = String
      type Failure = String
      var frozen = false
      var idCalls = 0
      var capabilityCalls = 0
      def id: EstimandId =
        if frozen then throw new IllegalStateException("id callback after construction")
        idCalls += 1
        EstimandId("snapshot")
      def requiredCapabilities: Set[CapabilityId] =
        if frozen then throw new IllegalStateException("capability callback after construction")
        capabilityCalls += 1
        Set(CapabilityId("replay"))
    val estimand = new MutableEstimand
    val plan = AnalysisSpecification.from(new PoisonSource, Design("d"), Frame("f"), estimand, "q", Vector.empty, Vector.empty, "pooled")
    estimand.frozen = true
    assertEquals(Diagnostics.describe(plan, CapabilitySet.empty).estimand, EstimandId("snapshot"))
    assertEquals(Diagnostics.explain(plan, CapabilitySet.empty).blockerCount, 1)
    assertEquals(Diagnostics.diff(plan, plan).changes, Set.empty[PlanChange])
    assertEquals(estimand.idCalls, 1)
    assertEquals(estimand.capabilityCalls, 1)

  test("capability changes require renewed admission and explain legal next operations"):
    val source = new PoisonSource
    val blocked = AnalysisSpecification.from(source, Design("d"), Frame("f"), EstimandUnderTest, "q", Vector.empty, Vector.empty, "pooled")
    source.capabilities = CapabilitySet.from(Vector(Capability(CapabilityId("replay"), "owned replay")))
    val supported = AnalysisSpecification.from(source, Design("d"), Frame("f"), EstimandUnderTest, "q", Vector.empty, Vector.empty, "pooled")
    val difference = Diagnostics.diff(blocked, supported)
    assertEquals(difference.changes, Set(PlanChange.SourceCapabilities))
    assert(difference.rebindRequired)
    assertEquals(blocked.plan, supported.plan)
    assertEquals(Diagnostics.explain(supported, replayAvailable).blockerCount, 0)
    assert(Diagnostics.explain(supported, replayAvailable).nextOperations.exists(_.contains("method binding")))
    assertEquals(Diagnostics.explain(supported, CapabilitySet.empty).blockerCount, 1)
    assertEquals(Diagnostics.explain(supported, CapabilitySet.empty).blockers.head.observed, "capability absent from caller available capabilities")
    assertEquals(Diagnostics.explain(blocked, replayAvailable).blockers.head.observed, "capability absent from captured source metadata")
    assertEquals(source.reads, 0)

  test("large scientific metadata pages retain all entries through continuations"):
    val assumptions = Vector.tabulate(1000)(i => s"assumption-$i")
    val plan = AnalysisSpecification.from(new PoisonSource, Design("d"), Frame("f"), EstimandUnderTest, "q", assumptions, Vector.empty, "pooled")
    val first = Diagnostics.describe(plan, CapabilitySet.empty)
    assertEquals(first.assumptions, assumptions.take(128))
    val second = Diagnostics.inspect(plan, CapabilitySet.empty, 128, first.nextMetadata).toOption.get
    assertEquals(second.assumptions, assumptions.slice(128, 256))
    assertEquals(Diagnostics.inspect(plan, CapabilitySet.empty, Int.MaxValue), Left(DiagnosticError.LimitExceedsMaximum(Int.MaxValue, 1024)))
    assertEquals(Diagnostics.inspect(plan, CapabilitySet.empty, 0), Left(DiagnosticError.InvalidLimit(0)))
