package scalafim.fmri.mvpa.analysis

import munit.FunSuite
import scalafim.fmri.mvpa.AxisSignature

class EvidenceExposureSuite extends FunSuite:
  private val plan = PlanId.derived(
    EstimandId("exposure-suite"), Vector(AxisSignature.unsafe("0" * 64)), AxisSignature.unsafe("1" * 64),
    AxisSignature.unsafe("2" * 64), "evidence", "design", "frame", "question", Vector.empty,
    Vector.empty, "reduction", Vector.empty, Set.empty
  )

  private def exposure = EvidenceExposure.external(ExposureReference(plan, "evidence-v1", "provenance-v1", ResultIdentity("result-v1")))

  private def read(scope: ExposureScope, payload: ExposurePayload = ExposurePayload.Payload, purpose: ExposurePurpose = ExposurePurpose.PayloadRead) =
    ExposureRequest(purpose, ExposureActorRole.Analyst, scope, payload, ExposureAssurance.Instrumented, 12L)

  test("unknown external evidence refuses a training-only probe without calling the callback"):
    var calls = 0
    val current = exposure
    val request = read(ExposureScope.Training)
    assertEquals(ExposureControl.permit(current, request), Left(ExposureError.UnknownFootprintForTrainingProbe))
    assertEquals(calls, 0)

  test("failure retains an approved whole-population exposure and its budget"):
    val current = exposure
    val request = read(ExposureScope.WholePopulation)
    val permit = ExposureControl.permit(current, request).toOption.get
    ExposureControl.read(current, permit, request)(Left("native provider failed")) match
      case ExposureAttempt.Failed(_, next) =>
        assertEquals(next.events.size, 1)
        assertEquals(next.events.head.request.maximumCells, 12L)
        assertEquals(next.events.head.request.scope, ExposureScope.WholePopulation)
      case other => fail(s"expected retained failed exposure, got $other")

  test("score selection and handoff round trip retain adaptive dependence"):
    val scoreRequest = read(ExposureScope.Holdout, ExposurePayload.DerivedScore, ExposurePurpose.DerivedScoreView)
    val base = EvidenceExposure.internal(ExposureReference(plan, "evidence-v1", "provenance-v1", ResultIdentity("result-v1")))
    val scored = ExposureControl.read(base, ExposureControl.permit(base, scoreRequest).toOption.get, scoreRequest)(Right(0.9)) match
      case ExposureAttempt.Completed(_, next) => next
      case other => fail(s"score read failed: $other")
    val selectRequest = read(ExposureScope.Holdout, ExposurePayload.DerivedScore, ExposurePurpose.ModelSelection)
    val selected = ExposureControl.read(scored, ExposureControl.permit(scored, selectRequest).toOption.get, selectRequest)(Right(())) match
      case ExposureAttempt.Completed(_, next) => next
      case other => fail(s"selection failed: $other")
    assertEquals(selected.events.last.adaptiveDependency, AdaptiveSelectionDependency.DependsOnObservedScores)
    val rebuilt = EvidenceExposureDto.reconstruct(EvidenceExposureDto.from(selected)).toOption.get
    assertEquals(rebuilt.identity, selected.identity)
    assertEquals(ExposureControl.untouchedConfirmation(rebuilt, ExposureScope.Holdout), Left(AdaptiveSelectionDependency.DependsOnObservedScores))

  test("whole-population payload access denies an untouched holdout confirmation even after failure"):
    val current = EvidenceExposure.internal(ExposureReference(plan, "evidence-v1", "provenance-v1", ResultIdentity("result-v1")))
    val request = read(ExposureScope.WholePopulation)
    val advanced = ExposureControl.read(current, ExposureControl.permit(current, request).toOption.get, request)(Left("partial native attempt")) match
      case ExposureAttempt.Failed(_, next) => next
      case other => fail(s"expected failed access record, got $other")
    assertEquals(ExposureControl.untouchedConfirmation(advanced, ExposureScope.Holdout), Left(AdaptiveSelectionDependency.DependsOnObservedScores))

  test("permits are stale after an immutable record advances"):
    val current = exposure
    val request = read(ExposureScope.WholePopulation)
    val permit = ExposureControl.permit(current, request).toOption.get
    val next = ExposureControl.read(current, permit, request)(Right(())).asInstanceOf[ExposureAttempt.Completed[Unit]].exposure
    assertEquals(ExposureControl.authorize(next, permit, request), Left(ExposureError.StalePermit(permit.record, next.identity)))

  test("diagnostic exposure inspection is metadata only"):
    val current = exposure
    val description = Diagnostics.inspectExposure(current, 1).toOption.get
    assertEquals(description.events, Vector.empty)
    assertEquals(description.identity, current.identity)
