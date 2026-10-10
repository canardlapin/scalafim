package scalafim.fmri.laws.profile

class TrialObservationEnclosureSuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")

  test("original impulse observation certificate encloses QR errors and admits a nonzero conditioning box"):
    val result = TrialNeighborhoodCertificateAudit.run(
      nodes = Vector(7),
      radii = Vector(0.0, 0.001, 0.1),
      boxRadii = Vector(1e-4, 1e-3)
    )
    result.points
      .take(2)
      .foreach: p =>
        assert(p.refusal.isEmpty, p.toString)
        assert(p.originalRelativeError <= p.relativeBound.get, p.toString)
        assert(p.originalQueryError <= p.queryBound.get, p.toString)
        assert(p.etaUpper.get < 1.0)
        assert(p.relativeBound.get <= TrialNeighborhoodAudit.amplitudeTolerance, p.toString)
    assert(result.boxes.head.etaUpper.exists(_ < 1.0), result.boxes.head.toString)
    assert(result.boxes.last.refusal.nonEmpty)
    assert(result.points.last.refusal.nonEmpty)
    assert(result.points.head.preparedRelativeError < 1e-9)

  test("a signed query has a useful original-observation certificate near a reference"):
    val result = TrialNeighborhoodCertificateAudit.run(
      horizon = 96.0,
      nodes = Vector(0),
      radii = Vector(0.001),
      boxRadii = Vector.empty
    )
    val p = result.points.head
    assert(p.refusal.isEmpty, p.toString)
    assert(p.originalQueryError <= p.queryBound.get, p.toString)
    assert(p.queryBound.get <= TrialNeighborhoodAudit.queryTolerance, p.toString)
    assert(p.originalRelativeError <= p.relativeBound.get, p.toString)
