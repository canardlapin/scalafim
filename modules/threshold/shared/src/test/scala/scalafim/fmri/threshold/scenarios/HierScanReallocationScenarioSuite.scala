package scalafim.fmri.threshold.scenarios

import scala.concurrent.duration.Duration

import scalafim.fmri.threshold.*
import scalafim.scenarios.{ScenarioHarness, ScenarioResult}

import SignFlipWorld.*

/** HierScan region-level FWER when several rejected non-null siblings share the
  * descendant budget by prior mass.
  *
  * Implements docs/verification/threshold-hierscan-reallocation-protocol-20260930.md.
  */
class HierScanReallocationScenarioSuite extends munit.FunSuite:

  override val munitTimeout: Duration = Duration(60, "min")

  private val profile = CalibrationProfile.current
  private val baseSeed = profile match
    case CalibrationProfile.PullRequest => 20260932L
    case CalibrationProfile.Calibration => 9190930L
  private val alpha = Alpha.unsafe(0.05)

  private val reallocationConditions: Vector[Condition] =
    val cross =
      for
        smooth <- Vector(false, true)
        alternative <- Vector(ThresholdAlternative.Greater, ThresholdAlternative.TwoSided)
      yield (smooth, alternative)
    cross.zipWithIndex.map { case ((smooth, alternative), index) =>
      Condition(index, exact = false, smooth, alternative, partial = true, twoBlocks = true)
    }

  reallocationConditions.foreach: condition =>
    test(s"${condition.id}.two-blocks HierScan controls region-level FWER (${profile.label}, R=${profile.replicates})"):
      val result = run(condition)
      println(result.render)
      assert(result.ciPass, result.render)

  private def run(condition: Condition): ScenarioResult =
    val rng = scala.util.Random(baseSeed + 1000L * condition.index)
    val r = profile.replicates
    val config = HierScanConfig(alpha = alpha, alternative = condition.alternative, priorEta = 1.0)
    val prior = condition.prior.map(volume)
    var failures = Vector.empty[String]
    var errors = 0
    var detectA = 0
    var detectB = 0
    var detectBoth = 0

    var replicate = 0
    while replicate < r do
      val world = SignFlipWorld.replicate(rng, condition)
      HierScan.run(volume(world.observed), world.nullDraw, prior = prior, config = config) match
        case Right(scan) =>
          val regions = scan.significantRegions.map(_.maskSpaceIndices)
          if regions.exists(region => !region.exists(condition.isSignal)) then errors += 1
          val a = regions.exists(_.exists(condition.inBlockA))
          val b = regions.exists(_.exists(condition.inBlockB))
          if a then detectA += 1
          if b then detectB += 1
          if a && b then detectBoth += 1
        case Left(err) =>
          failures :+= s"replicate $replicate: ${err.message}"
      replicate += 1

    def rate(k: Int) = f"$k/$r rate=${k.toDouble / r}%.4f"
    ScenarioHarness.result(
      s"threshold.hierscan-reallocation.${condition.id}.two-blocks.v3",
      Vector(
        ScenarioHarness.fact(
          "replicates.complete",
          failures.isEmpty,
          if failures.isEmpty then s"$r replicates" else failures.take(3).mkString("; ")
        ),
        notLiberal("hierScan.regionFwer", errors, r, alpha),
        ScenarioHarness.fact("hierScan.power.blockA.recorded", true, rate(detectA)),
        ScenarioHarness.fact("hierScan.power.blockB.recorded", true, rate(detectB)),
        ScenarioHarness.fact("hierScan.reallocation.bothBlocks.recorded", true, rate(detectBoth))
      )
    )
