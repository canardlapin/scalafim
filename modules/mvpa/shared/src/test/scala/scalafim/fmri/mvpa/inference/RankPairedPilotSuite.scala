package scalafim.fmri.mvpa.inference

import multivar.inference.CanonicalRankMethod
import scalafim.fmri.mvpa.analysis.CalibrationBindings
import scala.concurrent.duration.*

/** Current public adapters over identical archived inputs; no confirmation is
  * accepted and no numerical kernel lives in this harness.
  */
class RankPairedPilotSuite extends munit.FunSuite:
  override val munitTimeout: Duration = 15.minutes

  test("opt-in paired exploratory cell retains every assigned method result"):
    val list = CalibrationPlatform.environment("SCALAFIM_RANK_PAIRED_LIST")
    assume(list.isDefined, "paired input inventory absent")
    val output = CalibrationPlatform.environment("SCALAFIM_RANK_PAIRED_OUTPUT").getOrElse(fail("paired output required"))
    val paths = CalibrationPlatform.readText(list.get).linesIterator.filter(_.nonEmpty).toVector
    assert(paths.nonEmpty)
    val cases = paths.map(path => CalibrationProtocolSupport.readCase(CalibrationPlatform.readText(path),
      "scalafim/umvpa/rank-method-comparison/v2").fold(fail(_), identity))
    assert(cases.forall(c => Set("fixture", "pilot").contains(c.phase) && c.draws == 199))
    assertEquals(cases.map(c => (c.scenario, c.ordinal)).distinct.length, cases.length)
    if cases.exists(_.phase == "pilot") then
      assertEquals(cases.map(_.phase).distinct, Vector("pilot"))
      assertEquals(cases.map(_.scenario).distinct.length, 1)
      assertEquals(cases.map(_.ordinal), (0 until 200).toVector)
    val source = RankRecordProvenance.SourceSnapshot.environment().fold(fail(_), identity)
    cases.foreach: input =>
      Vector(CanonicalRankMethod.ScoreOrthogonalPermutationV2, CanonicalRankMethod.GaussianInterlacingWilksV1).foreach: method =>
        val started = System.nanoTime()
        def prefix = "{\"scenario_id\":" + CalibrationProtocolSupport.jsonString(input.scenario) +
          ",\"phase\":" + CalibrationProtocolSupport.jsonString(input.phase) +
          ",\"dataset_index\":" + input.ordinal + ",\"root_seed64\":\"" + input.rootSeed +
          "\",\"rank_method\":" + CalibrationProtocolSupport.jsonString(method.identity) +
          ",\"elapsed_seconds\":" + ((System.nanoTime() - started) / 1e9) + source.jsonFields(input)
        val record = CalibrationProtocolSupport.retainEvaluation(prefix):
          CalibrationBindings.rank(input.x, input.y, input.nuisance, input.rootSeed, input.draws,
            input.scenario, input.ordinal, method).map: result =>
            val verified = RankRecordProvenance.verify(input, result).fold(fail(_), identity)
            val arithmetic = result.candidateArithmetic
            val metrics = CalibrationProtocolSupport.rankMetrics(arithmetic.receipts.map(_.pValue.value),
              arithmetic.adjustedPValues.map(_.value), arithmetic.receipts.map(_.exceedances), input.draws).fold(fail(_), identity)
            assert(result.admittedDetectableRank.isLeft)
            prefix + ",\"status\":\"evaluated\",\"completed_draws\":199,\"residual_rows\":" + result.residualRows +
              ",\"planned_owned_cells\":" + result.plannedOwnedCells + ",\"completed_compact_fits\":" +
              arithmetic.completedCompactFits + ",\"observed_roots\":[" + arithmetic.correlations.mkString(",") +
              "]" + metrics.jsonFields(verified) + "}\n"
        source.verify(RankRecordProvenance.readSource).fold(fail(_), identity)
        CalibrationPlatform.appendText(output, record)
