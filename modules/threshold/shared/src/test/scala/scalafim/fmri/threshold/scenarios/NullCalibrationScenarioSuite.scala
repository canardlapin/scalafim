package scalafim.fmri.threshold.scenarios

import scala.concurrent.duration.Duration

import gale.linalg.Matrix
import scalafim.fmri.threshold.*
import scalafim.image.valueAtCanonicalOrdinal
import scalafim.scenarios.{ScenarioHarness, ScenarioResult}

import SignFlipWorld.*

/** FWER calibration of the family-wise procedures under sign-flip nulls.
  *
  * Implements docs/verification/threshold-null-calibration-protocol-20260930.md.
  * The protocol, not this file, is authoritative for conditions and acceptance.
  */
class NullCalibrationScenarioSuite extends munit.FunSuite:

  override val munitTimeout: Duration = Duration(60, "min")

  private val profile = CalibrationProfile.current
  private val baseSeed = profile match
    case CalibrationProfile.PullRequest => 20260930L
    case CalibrationProfile.Calibration => 7310930L
  private val alpha = Alpha.unsafe(0.05)

  conditions.foreach: condition =>
    test(s"${condition.id} controls FWER (${profile.label}, R=${profile.replicates})"):
      val result = run(condition)
      println(result.render)
      assert(result.ciPass, result.render)

  private def run(condition: Condition): ScenarioResult =
    val rng = scala.util.Random(baseSeed + 1000L * condition.index)
    val r = profile.replicates
    var failures = Vector.empty[String]
    var maxTErrors = 0
    var wyErrors = 0
    var hierErrors = 0
    var disagreements = 0

    var replicate = 0
    while replicate < r do
      val world = SignFlipWorld.replicate(rng, condition)
      val nullDraw = world.nullDraw

      val maxT = MaxT.runMap(world.statistic, nullDraw, None, alpha, condition.alternative)
      val wy = WestfallYoung.stepDown(
        world.observed,
        Matrix.tabulate(world.rows.length, voxels)((row, col) => world.rows(row)(col)),
        alpha,
        condition.alternative,
        condition.reference
      )
      (maxT, wy) match
        case (Right(map), Right(tests)) =>
          val mapRejected = (0 until voxels).filter(map.reject.valueAtCanonicalOrdinal).toSet
          val wyRejected = tests.filter(_.rejected).map(_.testIndex).toSet
          val maxTError = mapRejected.exists(v => !condition.isSignal(v))
          val wyError = wyRejected.exists(v => !condition.isSignal(v))
          if maxTError then maxTErrors += 1
          if wyError then wyErrors += 1
          if !condition.partial && mapRejected.nonEmpty != wyRejected.nonEmpty then disagreements += 1
        case (maxTOut, wyOut) =>
          failures :+= s"replicate $replicate: maxT=${maxTOut.left.map(_.message)} wy=${wyOut.left.map(_.message)}"

      if !condition.partial then
        HierScan.run(volume(world.observed), nullDraw, config = HierScanConfig(alpha = alpha, alternative = condition.alternative)) match
          case Right(scan) => if scan.significantRegions.nonEmpty then hierErrors += 1
          case Left(err)   => failures :+= s"replicate $replicate: hierscan=${err.message}"
      replicate += 1

    val observations =
      Vector(
        ScenarioHarness.fact(
          "replicates.complete",
          failures.isEmpty,
          if failures.isEmpty then s"$r replicates" else failures.take(3).mkString("; ")
        ),
        notLiberal("maxT.fwer", maxTErrors, r, alpha),
        notLiberal("westfallYoung.fwer", wyErrors, r, alpha)
      ) ++
        condition.exactSize(alpha).toVector.flatMap(size =>
          Vector(notConservative("maxT.fwer", maxTErrors, r, size), notConservative("westfallYoung.fwer", wyErrors, r, size))
        ) ++
        (if condition.partial then Vector.empty
         else
           Vector(
             notLiberal("hierScan.fwer", hierErrors, r, alpha),
             ScenarioHarness.fact("maxT.equals.westfallYoung", disagreements == 0, s"$disagreements disagreeing replicates")
           ))
    ScenarioHarness.result(s"threshold.null-calibration.${condition.id}.v1", observations)
