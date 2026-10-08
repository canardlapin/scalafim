package scalafim.fmri.mvpa.inference

import munit.FunSuite
import multivar.inference.CanonicalRankMethod
import scalafim.fmri.mvpa.analysis.CalibrationBindings
import scala.concurrent.duration.*

/** Explicitly opt-in external case bridge; default owning-module gates do not
  * run a pilot or consume study confirmation streams. */
class RankConfirmationSuite extends FunSuite:
  override val munitTimeout: Duration = 15.minutes

  test("opt-in case uses the production rank procedure and reports actual completed B"):
    val single = CalibrationPlatform.environment("SCALAFIM_CALIBRATION_CASE_FILE")
    val batch = CalibrationPlatform.environment("SCALAFIM_CALIBRATION_CASE_LIST")
    assert(!(single.isDefined && batch.isDefined), "choose one case or one streaming batch")
    val paths = single.toVector ++ batch.toVector.flatMap(path =>
      CalibrationPlatform.readText(path).linesIterator.filter(_.nonEmpty).toVector)
    assume(paths.nonEmpty,
      "explicit campaign case input absent; no pilot or confirmation is run by default")
    paths.foreach: path =>
      val input = CalibrationProtocolSupport.readCase(CalibrationPlatform.readText(path))
        .fold(error => fail(error), identity)
      assert(Set("fixture", "pilot").contains(input.phase),
        "this initial adapter cannot consume simulator or confirmation studies")
      val started = System.nanoTime()
      val method = CanonicalRankMethod.GaussianInterlacingWilksV1
      val resamplingSeed = CalibrationProtocolSupport.child(input.rootSeed, 103, method.ordinal)
        .fold(error => fail(error), identity)
      def elapsed = (System.nanoTime() - started) / 1e9
      val output = CalibrationPlatform.environment("SCALAFIM_CALIBRATION_OUTPUT").getOrElse(fail("case output path is required"))
      def prefix = "{\"phase\":" + CalibrationProtocolSupport.jsonString(input.phase) +
        ",\"scenario_id\":" + CalibrationProtocolSupport.jsonString(input.scenario) +
        ",\"dataset_index\":" + input.ordinal + ",\"root_seed64\":\"" + input.rootSeed +
        "\",\"resampling_seed64\":\"" + resamplingSeed + "\",\"child_seed_algorithm\":\"seed-path/v1\",\"elapsed_seconds\":" + elapsed
      val record = CalibrationProtocolSupport.retainEvaluation(prefix):
        CalibrationBindings.rank(input.x, input.y, input.nuisance, input.rootSeed, input.draws,
          input.scenario, input.ordinal, method).map: value =>
          val arithmetic = value.candidateArithmetic
          assert(arithmetic.receipts.forall(_.consumed.value == input.draws))
          val metrics = CalibrationProtocolSupport.rankMetrics(arithmetic.receipts.map(_.pValue.value),
            arithmetic.adjustedPValues.map(_.value), arithmetic.receipts.map(_.exceedances), input.draws)
            .fold(error => fail(error), identity)
          val nulls = input.populationCorrelations.indices.map(k => input.populationCorrelations.drop(k).forall(_ == 0.0))
          val members = metrics.raw.indices.map(k => "\"rank-" + (k + 1) + "\"")
          val line = prefix + ",\"rank_method\":" + CalibrationProtocolSupport.jsonString(arithmetic.method.identity) + ",\"status\":\"evaluated\",\"family_complete\":true,\"completed_draws\":" + input.draws +
            ",\"member_ids\":[" + members.mkString(",") + "]" + metrics.jsonFields +
            ",\"declared_population_null\":[" + nulls.mkString(",") +
            "],\"actual_conditional_null\":[" + nulls.mkString(",") + "],\"completed_compact_fits\":" +
            arithmetic.completedCompactFits + ",\"planned_owned_cells\":" + value.plannedOwnedCells + "}\n"
          line
      CalibrationPlatform.appendText(output, record)
