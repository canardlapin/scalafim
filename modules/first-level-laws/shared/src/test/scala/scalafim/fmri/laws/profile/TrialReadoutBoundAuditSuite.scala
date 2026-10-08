package scalafim.fmri.laws.profile

import scalafim.fmri.fit.profile.ProfileTrialReadoutMode
import scalafim.scenarios.ScenarioStatus

class TrialReadoutBoundAuditSuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")

  test("longer support preserves the schedule and improves the tail control within unchanged compiler limits"):
    val result = TrialReadoutBoundAudit.run(variants = Vector(
      TrialReadoutBoundAudit.Variant(48.0, 32), TrialReadoutBoundAudit.Variant(96.0, 24),
      TrialReadoutBoundAudit.Variant(192.0, 15, 48)), shapes = Vector(0))
    assertEquals(result.records.length, 4)
    assertEquals(result.geometry.map(_.onsets).distinct.length, 1)
    assertEquals(result.preparationRefusals.length, 1)
    assertEqualsDouble(result.preparationRefusals.head.variant.horizon, 192.0, 0.0)
    val exact = result.records.filter(_.mode == ProfileTrialReadoutMode.ExactShape)
    assert(exact.last.originalQrMaxError < 0.5 * exact.head.originalQrMaxError)
    assert(exact.forall(r => r.preparedQrMaxError < 1e-8 && r.residualGateEmits))
    assert(result.records.forall(r => r.originalQrError2 <= r.finiteCoefficientError2Upper &&
      r.signedQueryError <= r.signedQueryErrorUpper))
    assert(result.scenarios.forall(s => s.status == ScenarioStatus.Fail && !s.ciPass))

  test("finite observation bounds enclose signed coefficients and queries without claiming original certification"):
    val result = TrialReadoutBoundAudit.run(variants = Vector(TrialReadoutBoundAudit.Variant(48.0, 32)), shapes = Vector(0, 3))
    assertEquals(result.records.length, 4)
    assert(result.scenarios.forall(s => s.status == ScenarioStatus.Fail && !s.ciPass))
    result.records.foreach: r =>
      assert(r.finiteCoefficientError2Upper.isFinite && r.finiteSigmaLower > 0.0)
      assert(r.originalQrError2 <= r.finiteCoefficientError2Upper, r.toString)
      assert(r.signedQueryError <= r.signedQueryErrorUpper, r.toString)
      assertEquals(r.queryRetainedTrialValues, 0)
      if r.mode == ProfileTrialReadoutMode.ExactShape then
        assert(r.preparedQrMaxError < 1e-8)
        assert(r.residualGateEmits)
        assertEquals(r.exactFactorAttempts, 1L)
      else
        assert(!r.residualGateEmits)
        assertEquals(r.referenceInverseAttempts, 3L)
        assertEquals(r.exactFactorAttempts, 0L)
    assert(result.records.exists(r => r.mode == ProfileTrialReadoutMode.ExactShape && r.originalQrMaxError > 1e-4))
