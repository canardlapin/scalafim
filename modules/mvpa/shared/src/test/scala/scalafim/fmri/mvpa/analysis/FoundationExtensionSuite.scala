package scalafim.fmri.mvpa.analysis

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{Coverage, DigestAlgorithm}
import resample4s.designs.KFold
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.measurement.*
import scalafim.fmri.mvpa.relation.*
import scalafim.scenarios.{ScenarioHarness, ScenarioResult, ScenarioTolerance}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

/** A method-local extension rehearsal on the physically retired foundation.
  * The new result/program types appear only in this test, never in a registry. */
class FoundationExtensionSuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)

  private def extensionScenario(): ScenarioResult =
    val samples = right(AxisRef.fromStableKeys("extension-samples", SpaceRole.Samples,
      Vector("s0", "s1", "s2", "s3"), "trial", "one", "raw"))
    val neural = right(AxisRef.fromStableKeys("extension-neural", SpaceRole.Observed,
      Vector("left", "right"), "voxel", "one", "raw"))
    val sourceId = SourceId.unsafe("extension-owned-fixture")
    val source = right(EvidenceSource(sourceId, Provenance.source(ProvenanceId.unsafe("extension-root"), sourceId)))
    val observations = right(Observations.fromDense(samples, neural,
      DMat.dense(4, 2, Vector(1.0, 10.0, -1.0, 20.0, 3.0, 30.0, -3.0, 40.0)),
      ValueIdentity.source(ValueId.unsafe("extension-values-v1")), source))
    val design = right(ValidationDesign.bind(samples, KFold.ordered(2), ScientificSeed.fromLong(41)))
    val whole = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("extension-whole")))
    val frame = right(MeasurementFrame(neural, Vector(PackedMeasurementEntry(whole, "whole"))))

    final case class SignedColumnSummary(mean: Double, positiveRows: Int, stableFeatureKey: String)
    enum SummaryRefusal:
      case ForeignAxis
    enum SummaryFailure:
      case NonFinite
    object SignedColumnEstimand extends Estimand[
        Observations[samples.Id, neural.Id], ValidationDesign[samples.Id, String, Coverage.ExactOnce],
        MeasurementFrame[neural.Id, String, String]]:
      type Result = SignedColumnSummary
      type Rejection = SummaryRefusal
      type Failure = SummaryFailure
      val id = EstimandId("test-only-signed-column-summary")
      val requiredCapabilities = Set.empty[CapabilityId]
      override val parameters = Vector("feature" -> "left", "estimand" -> "arithmetic-mean-and-positive-count")

    given AnalysisCompiler[Observations[samples.Id, neural.Id], ValidationDesign[samples.Id, String, Coverage.ExactOnce],
        MeasurementFrame[neural.Id, String, String], SignedColumnEstimand.type] with
      type Bound = Unit
      type Program = () => SignedColumnSummary
      def bind(specification: AnalysisSpecification[Observations[samples.Id, neural.Id],
          ValidationDesign[samples.Id, String, Coverage.ExactOnce], MeasurementFrame[neural.Id, String, String],
          SignedColumnEstimand.type], available: CapabilitySet) = Right(())
      def numerical(scientific: BoundScientificPlan[Observations[samples.Id, neural.Id],
          ValidationDesign[samples.Id, String, Coverage.ExactOnce], MeasurementFrame[neural.Id, String, String],
          SignedColumnEstimand.type, Unit, () => SignedColumnSummary]) =
        val calculate = () =>
          val column = right(scientific.specification.source.patterns(DMat.dense(2, 1, Vector(1.0, 0.0))))
          var sum = 0.0
          var positives = 0
          var row = 0
          while row < column.rows do
            sum += column(row, 0)
            if column(row, 0) > 0.0 then positives += 1
            row += 1
          SignedColumnSummary(sum / column.rows, positives, "left")
        NumericalProgram(scientific, calculate, "test-only-single-column", Vector("sum", "positive-count"))

    val specification = AnalysisSpecification.from(observations, design, frame, SignedColumnEstimand,
      "signed summary", Vector("finite owned synthetic matrix"), Vector.empty, "these four trials")
    val bound = right(AnalysisCompiler.bind(specification, CapabilitySet.empty))
    val program = AnalysisCompiler.compile(bound)
    val result = program.program()
    val receipt = ExecutionReceipt(bound.receipt, EvidenceReceipt("extension-owned-v1", Vector("column-left"), None),
      RealizationReceipt("portable", "test-only-single-column", "binary64", Vector("seed-41"), Vector.empty, "serial"), 1, 4)
    val completed: AnalysisResult[SignedColumnSummary] = AnalysisResult.complete(bound, result, receipt)

    val changedFolds = right(ValidationDesign.bind(samples, KFold.ordered(4), ScientificSeed.fromLong(41)))
    val changed = AnalysisSpecification.from(observations, changedFolds, frame, SignedColumnEstimand,
      "signed summary", Vector("finite owned synthetic matrix"), Vector.empty, "these four trials")
    ScenarioHarness.result("umvpa-open-estimand-extension", Vector(
      ScenarioHarness.scalar("mean", completed.value.mean, 0.0, ScenarioTolerance.absolute(1e-12)),
      ScenarioHarness.fact("positive-count", completed.value.positiveRows == 2, s"count=${completed.value.positiveRows}"),
      ScenarioHarness.fact("feature", completed.value.stableFeatureKey == "left", completed.value.stableFeatureKey),
      ScenarioHarness.fact("estimand", bound.receipt.estimand == SignedColumnEstimand.id, bound.receipt.estimand.toString),
      ScenarioHarness.fact("fold-change", specification.plan != changed.plan, "two folds changed to four"),
      ScenarioHarness.fact("rebind", Diagnostics.diff(specification, changed).rebindRequired, "fold change requires rebinding")
    ))

  private def repairScenario(): ScenarioResult =
    def axis(keys: Vector[String]) = right(AxisRef.fromStableKeys("repair-samples", SpaceRole.Samples, keys, "trial", "one", "raw"))
    val samples = axis(Vector("s0", "s1", "s2", "s3"))
    val reordered = axis(Vector("s3", "s2", "s1", "s0"))
    val imported = right(Column.fromValues(reordered, Vector("b", "b", "a", "a"), ValueIdentity.source(ValueId.unsafe("imported-group-order"))))
    val foreignRejected = Column.decode(samples, imported.toRecord).isLeft
    val inspected = right(AxisPage.inspect(samples, 4)).entries.map(_._2)
    // Explicit keyed metadata repair, not a positional relabel or a cast.
    val keyed = imported.stableRowKeys.zip(imported.values).toMap
    val repaired = right(Column.fromValues(samples, samples.toRecord.stableKeys.map(keyed), ValueIdentity.source(ValueId.unsafe("keyed-group-repair-v1"))))
    val validation = right(LeaveOneGroupOutDesign.bind(samples, repaired, ScientificSeed.fromLong(41))(identity)).validation

    val neural = right(AxisRef.fromStableKeys("repair-neural", SpaceRole.Observed, Vector("left", "right"), "voxel", "one", "raw"))
    var reads = 0
    val poison = new DoubleLinearOperator:
      val rows = 4
      val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        throw IllegalStateException("metadata diagnostics touched payload")
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = applyTo(input, output)
    val sourceId = SourceId.unsafe("repair-pattern-source")
    val source = right(EvidenceSource(sourceId, Provenance.source(ProvenanceId.unsafe("repair-root"), sourceId)))
    val observations = right(Observations.fromOperator(samples, neural, poison, ValueIdentity.source(ValueId.unsafe("repair-poison-values")), source))
    val frame = right(MeasurementFrame(neural, Vector(PackedMeasurementEntry(right(MeasurementLeg.identity(neural, MeasurementId.unsafe("repair-whole"))), "whole"))))
    object RequiresReplay extends Estimand[Observations[samples.Id, neural.Id],
        ValidationDesign[samples.Id, String, Coverage.ExactOnce], MeasurementFrame[neural.Id, String, String]]:
      type Result = Vector[Double]
      type Rejection = String
      type Failure = String
      val id = EstimandId("repair-replay-diagnostic")
      val requiredCapabilities = Set(CapabilityId("replay"))
    val specification = AnalysisSpecification.from(observations, validation, frame, RequiresReplay,
      "repaired keyed metadata", Vector("explicit keyed correspondence"), Vector.empty, "these trials")
    val explanation = Diagnostics.explain(specification, CapabilitySet.empty)
    ScenarioHarness.result("umvpa-keyed-axis-repair", Vector(
      ScenarioHarness.fact("foreign-order-refused", foreignRejected, "reversed metadata cannot be decoded positionally"),
      ScenarioHarness.fact("axis-inspection", inspected == Vector("s0", "s1", "s2", "s3"), inspected.mkString(",")),
      ScenarioHarness.fact("imported-order", imported.stableRowKeys == Vector("s3", "s2", "s1", "s0"), imported.stableRowKeys.mkString(",")),
      ScenarioHarness.fact("repaired-groups", validation.keys.size == 2, s"units=${validation.keys.size}"),
      ScenarioHarness.fact("missing-replay", explanation.blockerCount == 1, s"blockers=${explanation.blockerCount}"),
      ScenarioHarness.fact("actionable-diagnostic", explanation.nextOperations.exists(_.contains("capability")), explanation.nextOperations.mkString(",")),
      ScenarioHarness.fact("zero-payload-reads", reads == 0, s"reads=$reads")
    ))

  private def reuseScenario(): ScenarioResult =
    def axis(name: String, role: SpaceRole, keys: Vector[String]) =
      right(AxisRef.fromStableKeys(name, role, keys, "rsa-workflow", "one", "raw"))
    val samples = axis("reuse-samples", SpaceRole.Samples, Vector.tabulate(6)(i => s"s$i"))
    val partitions = axis("reuse-runs", SpaceRole.Samples, Vector("run-a", "run-b"))
    val effects = axis("reuse-effects", SpaceRole.Latent, Vector("a", "b", "c"))
    val neural = axis("reuse-neural", SpaceRole.Observed, Vector("x", "y"))
    def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
    val run = right(Column.fromValues(samples, Vector("run-a", "run-a", "run-a", "run-b", "run-b", "run-b"), value("runs-v1")))
    val condition = right(Column.fromValues(samples, Vector("a", "b", "c", "a", "b", "c"), value("conditions-v1")))
    val plan = right(ObservationMeanPlan(samples, partitions, effects, run, condition))
    val sourceId = SourceId.unsafe("reuse-source")
    val source = right(EvidenceSource(sourceId, Provenance.source(ProvenanceId.unsafe("reuse-root"), sourceId)))
    val relations = right(ObservationMeanRelations.fromOwnedDense(plan, neural,
      DMat.dense(6, 2, Vector(0.0, 0.0, 1.0, 0.0, 0.0, 2.0, 0.0, 0.0, 1.0, 0.0, 0.0, 2.0)),
      source, RelationSource("reuse-acquisition", "reuse-response", "owned", "none", "none"), "reuse-owned", ObservationMeanBudget(10000)))
    val edge = right(RelationRdm.allDistinctOrdered(partitions)).edges.head
    val geometry = right(OrdinaryRelationGeometry.compute(relations, edge.left, RdmMethod.SquaredEuclidean(), RelationConsumerBudget(32, 10000)))
    val budget = QueryReuseBudget(8, 32, 1000)
    val geometryId = QueryProductId("actual-geometry")
    def program(model: String) = right(QueryReuseProgram(Vector(
      QueryNode(geometryId, Vector.empty, Vector(QueryDependency.relation(relations.relations.head)), "binary64", "portable-ordinary"),
      QueryNode(QueryProductId("comparison"), Vector(geometryId), Vector(QueryDependency.Parameter("rsa-model", model)), "binary64", "portable-comparison")
    ), budget))
    val before = program("model-a")
    val after = program("model-b")
    val decisions = after.assess(before).toMap
    val retained = right(RetainedQuery(geometryId, before, geometry, 3L, budget))
    var callbacks = 0
    var sameProduct = false
    val reused = right(retained.use(after, budget): cached =>
      callbacks += 1
      sameProduct = cached eq geometry
      cached.observed.rdm.values.sum)
    ScenarioHarness.result("umvpa-rsa-geometry-reuse",
      ScenarioHarness.values("squared-distance", geometry.observed.rdm.values.sorted, Vector(1.0, 4.0, 5.0), ScenarioTolerance.absolute(1e-12)) ++ Vector(
        ScenarioHarness.fact("geometry-admitted", decisions(geometryId) == QueryReuseDecision.Admitted, decisions(geometryId).toString),
        ScenarioHarness.fact("comparison-invalidated", decisions(QueryProductId("comparison")).isInstanceOf[QueryReuseDecision.Rejected], decisions(QueryProductId("comparison")).toString),
        ScenarioHarness.fact("actual-product-retained", sameProduct, "callback received the same geometry object"),
        ScenarioHarness.scalar("comparison-sum", reused, 10.0, ScenarioTolerance.absolute(1e-12)),
        ScenarioHarness.fact("one-callback", callbacks == 1, s"callbacks=$callbacks")
      ))

  test("a new native signed-column estimand returns its own typed record"):
    val result = extensionScenario()
    assert(result.ciPass, result.render)

  test("an analyst repairs metadata order and explains missing capability without reading patterns"):
    val result = repairScenario()
    assert(result.ciPass, result.render)

  test("an analyst changes RSA comparison and reuses actual native geometry"):
    val result = reuseScenario()
    assert(result.ciPass, result.render)
