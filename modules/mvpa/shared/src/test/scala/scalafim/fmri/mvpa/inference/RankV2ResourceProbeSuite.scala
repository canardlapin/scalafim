package scalafim.fmri.mvpa.inference

import multivar.inference.CanonicalRankMethod
import scalafim.fmri.mvpa.analysis.CalibrationBindings
import scala.concurrent.duration.*

/** Eight explicitly assigned fixture datasets, never a rate or confirmation study. */
class RankV2ResourceProbeSuite extends munit.FunSuite:
  override val munitTimeout: Duration = 10.minutes

  test("opt-in complete-adapter resource probe preserves each assigned method result"):
    val list = CalibrationPlatform.environment("SCALAFIM_RANK_V2_PROBE_LIST")
    assume(list.isDefined,"resource fixture inventory absent")
    val output = CalibrationPlatform.environment("SCALAFIM_RANK_V2_PROBE_OUTPUT").getOrElse(fail("probe output required"))
    val paths = CalibrationPlatform.readText(list.get).linesIterator.filter(_.nonEmpty).toVector
    assertEquals(paths.size,8)
    val cases = paths.map(path => CalibrationProtocolSupport.readCase(CalibrationPlatform.readText(path),
      "scalafim/umvpa/rank-method-comparison/v2").fold(fail(_),identity))
    assertEquals(cases.map(c => (c.x.rows,c.x.cols,c.y.cols,c.nuisance.cols)).toSet,
      (for n <- Set(80,640); (p,q) <- Set((4,6),(6,4)); z <- Set(1,3) yield (n,p,q,z)))
    cases.foreach: input =>
      assertEquals(input.phase,"fixture")
      assertEquals(input.draws,199)
      assertEquals(input.populationCorrelations.size,4)
      input.populationCorrelations.zip(Vector(.5,.3,.2,0.0)).foreach((a,b) => assertEqualsDouble(a,b,0.0))
      Vector(CanonicalRankMethod.ScoreOrthogonalPermutationV2,CanonicalRankMethod.GaussianInterlacingWilksV1).foreach: method =>
        val started = System.nanoTime()
        def prefix = "{\"scenario_id\":" + CalibrationProtocolSupport.jsonString(input.scenario) +
          ",\"phase\":\"fixture\",\"dataset_index\":0,\"root_seed64\":\"" + input.rootSeed +
          "\",\"rank_method\":" + CalibrationProtocolSupport.jsonString(method.identity) +
          ",\"elapsed_seconds\":" + ((System.nanoTime()-started)/1e9)
        val record = CalibrationProtocolSupport.retainEvaluation(prefix):
          CalibrationBindings.rank(input.x,input.y,input.nuisance,input.rootSeed,input.draws,input.scenario,0,method).map: result =>
            val arithmetic = result.candidateArithmetic
            val metrics = CalibrationProtocolSupport.rankMetrics(arithmetic.receipts.map(_.pValue.value),
              arithmetic.adjustedPValues.map(_.value),arithmetic.receipts.map(_.exceedances),input.draws).fold(fail(_),identity)
            assert(arithmetic.receipts.forall(_.consumed.value==199))
            assert(result.admittedDetectableRank.isLeft)
            prefix + ",\"status\":\"evaluated\",\"completed_draws\":199,\"residual_rows\":" + result.residualRows +
              ",\"planned_owned_cells\":" + result.plannedOwnedCells + ",\"completed_compact_fits\":" +
              arithmetic.completedCompactFits + metrics.jsonFields + "}\n"
        CalibrationPlatform.appendText(output,record)
