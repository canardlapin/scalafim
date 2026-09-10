package scalafim.examples.workflows

import scalafim.fmri.fit.profile.DecodeStatus

class ProfileHrfConditionWorkflowSuite extends munit.FunSuite:
  test("the condition workflow fits every voxel with typed outputs") {
    val rows = ProfileHrfConditionWorkflows.run()
    assertEquals(rows.map(_.voxel), (0 until 12).toVector)
    assert(rows.count(_.status == DecodeStatus.Accepted) >= 10, rows.map(_.status).toString)
    rows.foreach { r =>
      assert(r.peakLatencySeconds >= 3.0 && r.peakLatencySeconds <= 8.0)
      assert(r.amplitudes.length == 3)
      assertEqualsDouble(r.contrastAminusB, r.amplitudes(0) - r.amplitudes(1), 1e-12)
    }
    // the generating amplitudes have signs (+, -, +): the signed readout keeps them
    assert(rows.count(r => r.amplitudes(0) > 0 && r.amplitudes(1) < 0 && r.amplitudes(2) > 0) >= 10)
  }
