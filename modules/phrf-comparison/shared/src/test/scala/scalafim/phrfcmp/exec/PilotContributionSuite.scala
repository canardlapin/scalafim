package scalafim.phrfcmp.exec

import scalafim.phrfcmp.ingest.ScoringSynth
import scalafim.phrfcmp.score.*
import scalafim.phrfcmp.score.PilotCell as ScoreCell

class PilotContributionSuite extends munit.FunSuite:
  private val unit = ScoringSynth.unit(ScoreCell.CTG5, 2, "phrf-gauss")
  private def ok[A](e: Either[String, A]): A = e.fold(fail(_), identity)

  test("canonical contributions preserve all voxels and exact finite doubles on both platforms"):
    val raw = ScoringSynth.contribution(unit)
    val decoded = ok(PilotContribution.decode(raw))
    assertEquals(decoded.unit, unit)
    val ContributionData.Condition(vs) = decoded.data: @unchecked
    assertEquals(vs.length, ScoringSynth.voxels)
    val e = vs.head.toOption.get
    assertEqualsDouble(e.gaussian.get.tau, 4.886, 1e-14)
    assertEqualsDouble(e.gaussian.get.sd.head, 0.08, 0.0)
    assertEquals(e.lags.length, 321)
    assertEquals(e.curves.length, 3)
    assertEquals(raw.toVector, ScoringSynth.contribution(unit).toVector)
    assertEquals(decoded.toString, "DecodedContribution(<redacted>)")

  test("malformed, noncanonical, nonfinite, truncated and wrong-dimension bytes fail with redacted tokens"):
    val raw = ScoringSynth.contribution(unit)
    assertEquals(PilotContribution.decode(raw.dropRight(1)), Left("malformed_contribution"))
    assertEquals(PilotContribution.decode(raw ++ Array[Byte](32)), Left("malformed_contribution"))
    for mutate <- Vector[ujson.Value => Unit](
      j => j(0) = "foreign-schema",
      j => j(1)(1) = 0.5,
      j => j(2) = "wrong-input",
      j => j(4)(0)(1)(0)(1) = "7ff8000000000000",
      j => j(4)(0)(1)(2)(2)(0) = "0000000000000000",
      j => { j(4)(0)(1)(1)(0).arr.remove(0): Unit }
    ) do
      val j = ujson.read(new String(raw, "UTF-8")); mutate(j)
      assertEquals(PilotContribution.decode(ujson.write(j).getBytes("UTF-8")), Left("malformed_contribution"))

  test("bound generator truth uses condition coefficients, trial ordering and immutable copies"):
    val b = ScoringSynth.bound(ScoreCell.CTG5, 2)
    val t = ok(PilotScoreTruth.fromBound(b))
    val expected = b.truth.condCoef(0, 1) * b.truth.truthKernel(0, 50)
    assertEqualsDouble(t.responses.head.curves(1)(50), expected, 1e-15)
    b.truth.condCoef.data(1) = 1000.0
    assertEqualsDouble(t.responses.head.curves(1)(50), expected, 1e-15)
    val tr = ScoringSynth.bound(ScoreCell.TTXFast, 0)
    val truth = ok(PilotScoreTruth.fromBound(tr)).trial.get
    assertEquals(truth.conditionOfTrial, ScoringSynth.conditions)
    assertEqualsDouble(truth.amplitudes(2)(7), tr.truth.trialBeta.get(2, 7), 0.0)

  test("coverage target is explicit; the signed peak error averages conditionwise errors"):
    val e = new ConditionEstimate(Vector(0.0, 1.0), Vector(Vector(1.0, 2.0), Vector(-1.0, -2.0)),
      Some(new GaussianEstimate(5.1, 0.8, Vector(0.2, 0.1), Vector(4.0, -1.0))))
    val truth = (5.0, 0.4, Vector(2.0, -2.0))
    val tau = ok(PilotCoverage.observation(e, truth, GaussianCoverageTarget.Tau))
    val both = ok(PilotCoverage.observation(e, truth, GaussianCoverageTarget.BothShapeCoordinates))
    assertEqualsDouble(tau.signedRelativePeakError, 0.75, 1e-15)
    assertEqualsDouble(tau.tauError, 0.1, 1e-14)
    assert(tau.covered && !both.covered)
    assert(PilotCoverage.observation(new ConditionEstimate(e.lags, e.curves, None), truth, GaussianCoverageTarget.Tau).isLeft)

  test("failed and refused voxels are retained, not replaced with estimates"):
    val u = ScoringSynth.unit(ScoreCell.TTXFast, 0, "lsa")
    val vs = Vector(VoxelOutcome.Refused, VoxelOutcome.Failed, VoxelOutcome.Estimated(TrialEstimate.of(Vector(1.0, -2.0)).toOption.get))
    val raw = ok(PilotContribution.trial(u, "a" * 64, vs))
    assertEquals(new String(raw, "UTF-8"),
      """["phrf-pilot-contribution-v1",["T-TX-fast",0,"lsa"],"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","trial",[["r"],["f"],["e",["3ff0000000000000","c000000000000000"]]],null]""")
    val ContributionData.Trial(xs) = ok(PilotContribution.decode(raw)).data: @unchecked
    assertEquals(xs.take(2), vs.take(2))
    assertEquals(xs.last.toOption.get.values, Vector(1.0, -2.0))
    assertEquals(ok(PilotContribution.decode(PilotContribution.unavailable(u))).data, ContributionData.Unavailable)
