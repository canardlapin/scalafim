package scalafim.fmri.mvpa.inference

import munit.FunSuite
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
      val resamplingSeed = CalibrationProtocolSupport.child(input.rootSeed, 103, 0)
        .fold(error => fail(error), identity)
      val result = CalibrationBindings.rank(input.x, input.y, input.nuisance, input.rootSeed, input.draws,
        input.scenario + "-" + input.ordinal)
      val elapsed = (System.nanoTime() - started) / 1e9
      val output = CalibrationPlatform.environment("SCALAFIM_CALIBRATION_OUTPUT").getOrElse(fail("case output path is required"))
      val prefix = "{\"phase\":\"" + input.phase + "\",\"scenario_id\":\"" + input.scenario +
        "\",\"dataset_index\":" + input.ordinal + ",\"root_seed64\":\"" + input.rootSeed +
        "\",\"resampling_seed64\":\"" + resamplingSeed + "\",\"child_seed_algorithm\":\"seed-path/v1\",\"elapsed_seconds\":" + elapsed
      result match
        case Left(reason) =>
          val escaped = reason.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")
          CalibrationPlatform.appendText(output, prefix + ",\"status\":\"failed\",\"reason\":\"" + escaped + "\",\"p_values\":null,\"reject\":null}\n")
        case Right(value) =>
          val arithmetic = value.candidateArithmetic
          assert(arithmetic.receipts.forall(_.consumed.value == input.draws))
          val p = arithmetic.adjustedPValues.map(_.value)
          val decisions = p.map(_ <= .05)
          val nulls = input.populationCorrelations.indices.map(k => input.populationCorrelations.drop(k).forall(_ == 0.0))
          val members = p.indices.map(k => "\"rank-" + (k + 1) + "\"")
          val line = prefix + ",\"status\":\"evaluated\",\"family_complete\":true,\"completed_draws\":" + input.draws +
            ",\"member_ids\":[" + members.mkString(",") + "],\"p_values\":[" + p.mkString(",") +
            "],\"reject\":[" + decisions.mkString(",") + "],\"declared_population_null\":[" + nulls.mkString(",") +
            "],\"actual_conditional_null\":[" + nulls.mkString(",") + "],\"completed_compact_fits\":" +
            arithmetic.completedCompactFits + ",\"planned_owned_cells\":" + value.plannedOwnedCells + "}\n"
          CalibrationPlatform.appendText(output, line)
