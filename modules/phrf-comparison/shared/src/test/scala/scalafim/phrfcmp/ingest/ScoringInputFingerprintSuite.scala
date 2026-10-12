package scalafim.phrfcmp.ingest

import scalafim.phrfcmp.exec.ScoringInputFingerprint
import scalafim.phrfcmp.score.PilotCell as ScoreCell

class ScoringInputFingerprintSuite extends munit.FunSuite:
  test("scorer input identity binds trial stimulus labels and the entire noise pool without reading truth"):
    val b = ScoringSynth.bound(ScoreCell.TTXFast, 0)
    val original = ScoringInputFingerprint.of(b.fit)
    val stimulus = b.fit.copy(evStim = b.fit.evStim.updated(0, 99))
    assertNotEquals(ScoringInputFingerprint.of(stimulus), original)
    val pool = scalafim.phrfcmp.ingest.Matrix.of(1, b.fit.y.cols, Array.fill(b.fit.y.cols)(1.0)).toOption.get
    val pooled = b.fit.copy(yPool = Some(pool))
    val before = ScoringInputFingerprint.of(pooled)
    assertNotEquals(before, original)
    pool.data(0) = 2.0
    assertNotEquals(ScoringInputFingerprint.of(pooled), before)
    b.truth.condCoef.data(0) = 999.0
    assertEquals(ScoringInputFingerprint.of(b.fit), original)
