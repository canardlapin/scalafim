package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import munit.FunSuite
import scalafim.fmri.mvpa.{AxisRef, EvidenceSource, MultiResponse, Observations}
import scalafim.fmri.mvpa.analysis.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class AlderExposureReadsSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)

  private final class Poison(values: DMat, fail: Boolean = false) extends DoubleLinearOperator:
    val rows = values.rows
    val cols = values.cols
    var calls = 0
    def applyTo(input: DVec, output: MutableDVec): Unit =
      calls += 1
      if fail then throw IllegalStateException("provider failure")
      var selected = 0
      while selected < input.length && input(selected) == 0.0 do selected += 1
      var row = 0
      while row < rows do
        output(row) = values(row, selected)
        row += 1

  private def source(name: String): EvidenceSource =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))

  private def fixture(fail: Boolean = false) =
    val samples = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, Vector("train", "holdout"), "trial", "none", "one"))
    val inputs = right(AxisRef.fromStableKeys("inputs", SpaceRole.Observed, Vector("i1"), "native", "none", "one"))
    val responses = right(AxisRef.fromStableKeys("responses", SpaceRole.Observed, Vector("y1"), "native", "none", "one"))
    val poison = new Poison(DMat.dense(2, 1, Vector(1.0, 2.0)), fail)
    val target = new Poison(DMat.dense(2, 1, Vector(3.0, 4.0)), fail)
    val observations = right(Observations.fromOperator(samples, inputs, poison, ValueIdentity.source(ValueId.unsafe("x")), source("x-source")))
    val targets = right(MultiResponse.fromOperator(samples, responses, target, ValueIdentity.source(ValueId.unsafe("y")), source("y-source")))
    val mapping = right(NativeAxisMapping.fromAxis(samples, Vector(10L, 20L), DataFingerprint.external("declared")))
    (observations, targets, mapping, poison, target)

  /* The opaque PlanId is produced by analysis compilation in production. A
   * fixture only needs a stable serialized value to exercise this adapter. */
  private def exposure[SK <: multivar.core.SemanticSpace, NK <: multivar.core.SemanticSpace, FK <: multivar.core.SemanticSpace](observations: Observations[SK, NK], targets: MultiResponse[SK, FK], mapping: NativeAxisMapping) =
    EvidenceExposure.external(AlderExposureReads.referenceFor(("0" * 64).asInstanceOf[PlanId], observations, targets, mapping, ResultIdentity("result")))

  private def request(scope: ExposureScope, cells: Long = 64L, assurance: ExposureAssurance = ExposureAssurance.Instrumented) =
    ExposureRequest(ExposurePurpose.PayloadRead, ExposureActorRole.Service, scope, ExposurePayload.Payload, assurance, cells)

  test("training-only native probe is refused before poison parent operators run"):
    val (observations, targets, mapping, inputs, outputs) = fixture()
    val current = exposure(observations, targets, mapping)
    val required = request(ExposureScope.Training)
    val permit = ExposureControl.permit(current, request(ExposureScope.WholePopulation)).toOption.get
    val result = AlderExposureReads.nativeTables(observations, targets, Vector("a", "b"), DataFingerprint.external("metadata"), mapping, right(NativeReadPolicy(1, right(MaterializationBudget(64L)))), current, permit, required)
    assert(result.isInstanceOf[AlderExposureReadResult.Refused])
    assertEquals(inputs.calls, 0)
    assertEquals(outputs.calls, 0)

  test("approved wider read is budgeted and preserves exposure after provider failure"):
    val (observations, targets, mapping, inputs, _) = fixture(fail = true)
    val current = exposure(observations, targets, mapping)
    val required = request(ExposureScope.WholePopulation)
    val permit = ExposureControl.permit(current, required).toOption.get
    val result = AlderExposureReads.nativeTables(observations, targets, Vector("a", "b"), DataFingerprint.external("metadata"), mapping, right(NativeReadPolicy(1, right(MaterializationBudget(64L)))), current, permit, required)
    result match
      case AlderExposureReadResult.ReadFailed(_, next) =>
        assertEquals(next.events.head.request.scope, ExposureScope.WholePopulation)
        assertEquals(next.events.head.request.maximumCells, 64L)
      case other => fail(s"expected recorded failure, got $other")
    assert(inputs.calls > 0)

  test("native reads reject caller constrained assurance and foreign references before callbacks"):
    val (observations, targets, mapping, inputs, outputs) = fixture()
    val current = exposure(observations, targets, mapping)
    val constrained = request(ExposureScope.WholePopulation, assurance = ExposureAssurance.Constrained)
    val constrainedPermit = ExposureControl.permit(current, constrained).toOption.get
    val policy = right(NativeReadPolicy(1, right(MaterializationBudget(64L))))
    assertEquals(
      AlderExposureReads.nativeTables(observations, targets, Vector("a", "b"), DataFingerprint.external("metadata"), mapping, policy, current, constrainedPermit, constrained),
      AlderExposureReadResult.Refused(ExposureError.NativeAssuranceUnsupported, current)
    )
    val foreign = EvidenceExposure.external(ExposureReference(("0" * 64).asInstanceOf[PlanId], "foreign", "foreign", ResultIdentity("result")))
    val approved = request(ExposureScope.WholePopulation)
    val foreignPermit = ExposureControl.permit(foreign, approved).toOption.get
    assertEquals(
      AlderExposureReads.nativeTables(observations, targets, Vector("a", "b"), DataFingerprint.external("metadata"), mapping, policy, foreign, foreignPermit, approved),
      AlderExposureReadResult.Refused(ExposureError.ReferenceMismatch, foreign)
    )
    assertEquals(inputs.calls, 0)
    assertEquals(outputs.calls, 0)
