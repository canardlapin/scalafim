package scalafim.fmri.laws.profile

import scalafim.fmri.fit.profile.ProfileTrialReadoutMode
import scalafim.scenarios.ScenarioStatus

/** Numerical oracle checks. Scientific errors are measurements, never hidden by tolerances. */
class TrialQualificationAuditSuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(15, "min")
  private lazy val result = TrialQualificationAudit.run(TrialQualificationAudit.fixture())

  test("expanded original-family audit preserves all attempts and separates exact from corrected errors"):
    assertEquals(result.records.length, 48)
    assertEquals(result.geometry.length, 12)
    assertEquals(result.scenarios.length, result.records.length)
    assert(result.scenarios.forall(s => s.status == ScenarioStatus.Fail && !s.ciPass))
    assert(result.geometry.forall(_.finiteOriginalAugmentedSigmaLower > 0.0))
    result.records.foreach: r =>
      assert(r.energy.isFinite)
      assert(r.shapeErrorInChartUnits.isFinite)
      assertEquals(r.emitted, r.preparedQrMaxError.nonEmpty)
      if r.emitted && r.mode == ProfileTrialReadoutMode.ExactShape then
        assert(r.preparedQrMaxError.get < 1e-7, s"${r.cell}: ${r.preparedQrMaxError}")
        assert(r.preparedResidual.get < 1e-8)
      Vector(r.preparedQrMaxError, r.originalQrMaxError, r.signedQueryAbsoluteError,
        r.amplitudeTruthRelativeError, r.preparedResidual, r.originalResidual, r.preparedTrialQrMaxError,
        r.originalTrialQrMaxError, r.originalNuisanceQrMaxError).flatten.foreach(x => assert(x.isFinite))
    val matched = result.records.filter(r => r.cell.endsWith("matched") && r.mode == ProfileTrialReadoutMode.ExactShape)
    assertEquals(matched.length, 8)
    matched.foreach: r =>
      assert(r.emitted, r.toString)
      assert(r.shapeErrorInChartUnits < 1e-4, r.toString)
    result.records.groupBy(r => (r.cell, r.voxel)).values.foreach: pair =>
      assertEquals(pair.length, 2)
      assertEquals(pair.head.status, pair.last.status)
      pair.head.coordinates.zip(pair.last.coordinates).foreach((a, b) => assertEqualsDouble(a, b, 0.0))

  test("original-family tail discrepancy remains visible separately from prepared-basis residuals"):
    assert(result.geometry.forall(g => g.relativeBasisErrorWithinHorizon.isFinite && g.relativeTailDesignError.isFinite))
    assert(result.geometry.exists(_.relativeTailDesignError > 0.01))
    assert(result.records.exists(r => r.emitted && r.mode == ProfileTrialReadoutMode.ExactShape &&
      r.originalResidual.get > 100.0 * math.max(1e-10, r.preparedResidual.get)))
