package scalafim.fmri.threshold.scenarios

import scala.concurrent.duration.Duration

import scalafim.fmri.threshold.*
import scalafim.scenarios.{ScenarioHarness, ScenarioResult}

import SignFlipWorld.*

/** Region-level FWER calibration of the reference-aligned HierScan.
  *
  * Implements docs/verification/threshold-hierscan-calibration-protocol-20260930.md.
  * The protocol, not this file, is authoritative for conditions and acceptance.
  */
class HierScanCalibrationScenarioSuite extends munit.FunSuite:

  override val munitTimeout: Duration = Duration(60, "min")

  private val profile = CalibrationProfile.current
  private val baseSeed = profile match
    case CalibrationProfile.PullRequest => 20260931L
    case CalibrationProfile.Calibration => 5550930L
  private val alpha = Alpha.unsafe(0.05)

  conditions.foreach: condition =>
    test(s"${condition.id} HierScan controls region-level FWER (${profile.label}, R=${profile.replicates})"):
      val result = run(condition)
      println(result.render)
      assert(result.ciPass, result.render)

  private def run(condition: Condition): ScenarioResult =
    val rng = scala.util.Random(baseSeed + 1000L * condition.index)
    val r = profile.replicates
    val config = HierScanConfig(alpha = alpha, alternative = condition.alternative)
    var failures = Vector.empty[String]
    var errors = 0
    var detections = 0

    var replicate = 0
    while replicate < r do
      val world = SignFlipWorld.replicate(rng, condition)
      HierScan.run(volume(world.observed), world.nullDraw, config = config) match
        case Right(scan) =>
          val regions = scan.significantRegions.map(_.maskSpaceIndices)
          // Complete null: every region is false. Partial null: a region is
          // false iff it contains no signal voxel.
          if regions.exists(region => !region.exists(condition.isSignal)) then errors += 1
          if regions.exists(region => region.exists(condition.isSignal)) then detections += 1
        case Left(err) =>
          failures :+= s"replicate $replicate: ${err.message}"
      replicate += 1

    val observations =
      Vector(
        ScenarioHarness.fact(
          "replicates.complete",
          failures.isEmpty,
          if failures.isEmpty then s"$r replicates" else failures.take(3).mkString("; ")
        ),
        notLiberal("hierScan.regionFwer", errors, r, alpha)
      ) ++
        (if condition.partial then
           Vector(ScenarioHarness.fact("hierScan.power.recorded", true, f"detections=$detections/$r rate=${detections.toDouble / r}%.4f"))
         else Vector.empty)
    ScenarioHarness.result(s"threshold.hierscan-calibration.${condition.id}.v2", observations)
