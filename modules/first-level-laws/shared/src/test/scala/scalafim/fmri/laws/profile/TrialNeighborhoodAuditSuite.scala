package scalafim.fmri.laws.profile

class TrialNeighborhoodAuditSuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")

  test("reference probes retain all attempts, exact anchors, one correction and query-only storage"):
    val result = TrialNeighborhoodAudit.run(trials = 30, freshCount = 4,
      selectedRadii = Vector(0.0, 0.003, 0.03), nodes = Vector(0, 7))
    assertEquals(result.geometry.references, 8)
    assertEquals(result.coverage.map(_.attempted), Vector(4, 4))
    assertEquals(result.records.length, 2 * (2 * 9 + 4 * 8))
    assert(result.records.forall(r => r.inverseApplications == 3 && r.corrections == 1 && r.exactFactors == 0 && r.retainedQueryTrials == 0))
    assert(result.records.filter(_.point.radius == 0.0).forall(_.preparedRelativeError < 1e-9))
    assert(result.records.forall(r => r.originalRelativeError.isFinite && r.preparedRelativeError.isFinite))
    assert(result.coverage.forall(c => c.nearestTwoAmplitudePasses <= c.anyReferenceAmplitudePasses))
