package scalafim.fmri.threshold.scenarios

import scala.concurrent.duration.Duration

import gale.linalg.Matrix
import scalafim.fmri.threshold.*
import scalafim.image.{PrimitiveBuffers, SampleSpaces, SomeScalarVolume, valueAtCanonicalOrdinal}
import scalafim.scenarios.{ScenarioHarness, ScenarioObservation, ScenarioResult}

/** FWER calibration of the family-wise procedures under sign-flip nulls.
  *
  * Implements docs/verification/threshold-null-calibration-protocol-20260930.md.
  * The protocol, not this file, is authoritative for conditions and acceptance.
  */
class NullCalibrationScenarioSuite extends munit.FunSuite:

  override val munitTimeout: Duration = Duration(60, "min")

  private val profile = CalibrationProfile.current
  private val side = 6
  private val voxels = side * side * side
  private val alpha = Alpha.unsafe(0.05)
  private val blockEffect = 1.2
  private val gate = 0.001

  private val conditions: Vector[Condition] =
    val cross =
      for
        exact <- Vector(true, false)
        smooth <- Vector(false, true)
        alternative <- Vector(ThresholdAlternative.Greater, ThresholdAlternative.TwoSided)
        partial <- Vector(false, true)
      yield (exact, smooth, alternative, partial)
    cross.zipWithIndex.map { case ((exact, smooth, alternative, partial), index) =>
      Condition(index, exact, smooth, alternative, partial)
    }

  conditions.foreach: condition =>
    test(s"${condition.id} controls FWER (${profile.label}, R=${profile.replicates})"):
      val result = run(condition)
      println(result.render)
      assert(result.ciPass, result.render)

  private def run(condition: Condition): ScenarioResult =
    val rng = scala.util.Random(profile.baseSeed + 1000L * condition.index)
    val r = profile.replicates
    var failures = Vector.empty[String]
    var maxTErrors = 0
    var wyErrors = 0
    var hierErrors = 0
    var disagreements = 0

    var replicate = 0
    while replicate < r do
      val data = subjects(rng, condition)
      val flips = flipFamily(rng, condition)
      val observed = tStat(data, flips(0).map(_ => 1.0))
      val rows = flips.map(tStat(data, _))
      val nullDraw = RowsNullDraw(rows, condition.reference)
      val statistic = StatisticMap.z(volume(observed))

      val maxT = MaxT.runMap(statistic, nullDraw, None, alpha, condition.alternative)
      val wy = WestfallYoung.stepDown(
        observed,
        Matrix.tabulate(rows.length, voxels)((row, col) => rows(row)(col)),
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
        HierScan.run(volume(observed), nullDraw, config = HierScanConfig(alpha = alpha, alternative = condition.alternative)) match
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
        notLiberal("maxT", maxTErrors, r),
        notLiberal("westfallYoung", wyErrors, r)
      ) ++
        condition.exactSize(alpha).toVector.flatMap(size => Vector(notConservative("maxT", maxTErrors, r, size), notConservative("westfallYoung", wyErrors, r, size))) ++
        (if condition.partial then Vector.empty
         else
           Vector(
             notLiberal("hierScan", hierErrors, r),
             ScenarioHarness.fact("maxT.equals.westfallYoung", disagreements == 0, s"$disagreements disagreeing replicates")
           ))
    ScenarioHarness.result(s"threshold.null-calibration.${condition.id}.v1", observations)

  private def notLiberal(method: String, errors: Int, r: Int): ScenarioObservation =
    val tail = Binomial.upperTail(r, alpha.value, errors)
    ScenarioHarness.fact(s"$method.fwer.notLiberal", tail >= gate, f"errors=$errors/$r rate=${errors.toDouble / r}%.4f P(X>=k|alpha)=$tail%.3g")

  private def notConservative(method: String, errors: Int, r: Int, size: Double): ScenarioObservation =
    val tail = Binomial.lowerTail(r, size, errors)
    ScenarioHarness.fact(s"$method.fwer.notConservative", tail >= gate, f"errors=$errors/$r size=$size%.4f P(X<=k|size)=$tail%.3g")

  /** Subject maps s_i * e_i + mu, with optional in-grid 3x3x3 mean smoothing of e_i. */
  private def subjects(rng: scala.util.Random, condition: Condition): Array[Array[Double]] =
    Array.fill(condition.n):
      val scale = math.exp(0.5 * rng.nextGaussian())
      val raw = Array.fill(voxels)(rng.nextGaussian())
      val noise = if condition.smooth then smooth(raw) else raw
      Array.tabulate(voxels)(v => scale * noise(v) + (if condition.partial && condition.isSignal(v) then blockEffect else 0.0))

  private def smooth(raw: Array[Double]): Array[Double] =
    Array.tabulate(voxels): v =>
      val (x, y, z) = (v % side, (v / side) % side, v / (side * side))
      var sum = 0.0
      var count = 0
      for dx <- -1 to 1; dy <- -1 to 1; dz <- -1 to 1 do
        val (a, b, c) = (x + dx, y + dy, z + dz)
        if a >= 0 && a < side && b >= 0 && b < side && c >= 0 && c < side then
          sum += raw(a + side * (b + side * c))
          count += 1
      sum / count

  /** Exact: all 2^n flip vectors, identity first. Monte Carlo: B = 99 uniform draws with replacement. */
  private def flipFamily(rng: scala.util.Random, condition: Condition): Vector[Array[Double]] =
    def signs(code: Int): Array[Double] = Array.tabulate(condition.n)(i => if ((code >> i) & 1) == 1 then -1.0 else 1.0)
    if condition.exact then Vector.tabulate(1 << condition.n)(signs)
    else Vector.fill(99)(signs(rng.nextInt(1 << condition.n)))

  private def tStat(data: Array[Array[Double]], flip: Array[Double]): Array[Double] =
    val n = data.length
    Array.tabulate(voxels): v =>
      var sum = 0.0
      var i = 0
      while i < n do
        sum += flip(i) * data(i)(v)
        i += 1
      val mean = sum / n
      var ss = 0.0
      i = 0
      while i < n do
        val d = flip(i) * data(i)(v) - mean
        ss += d * d
        i += 1
      mean / math.sqrt(ss / (n - 1) / n)

  private def volume(data: Array[Double]): SomeScalarVolume[Double] =
    SomeScalarVolume.unsafeCopyFromCanonicalArray(PrimitiveBuffers.fromArray(data), SampleSpaces(Vector(side, side, side)))

  private final case class Condition(
      index: Int,
      exact: Boolean,
      smooth: Boolean,
      alternative: ThresholdAlternative,
      partial: Boolean
  ):
    val n: Int = if exact then 6 else 10

    def reference: NullReference =
      if exact then NullReference.ExactEnumeration else NullReference.MonteCarlo

    def id: String =
      val nullKind = if exact then "exact-n6" else "montecarlo-n10"
      val field = if smooth then "smooth" else "white"
      val hypothesis = if partial then "partial-null" else "complete-null"
      s"$nullKind.$field.${alternative.toString.toLowerCase}.$hypothesis"

    /** The 2x2x2 signal block, present only in the partial null. */
    def isSignal(v: Int): Boolean =
      partial && v % side < 2 && (v / side) % side < 2 && v / (side * side) < 2

    /** Exact test size, where the protocol declares one. */
    def exactSize(alpha: Alpha): Option[Double] =
      if partial || !exact then None
      else
        val b = 1 << n
        alternative match
          case ThresholdAlternative.Greater  => Some(math.floor(alpha.value * b) / b)
          case ThresholdAlternative.TwoSided => Some(2.0 * math.floor(alpha.value * b / 2.0) / b)
          case ThresholdAlternative.Less     => None

  private final class RowsNullDraw(rows: Vector[Array[Double]], override val reference: NullReference) extends NullDraw:
    override val nPermutations: PermutationCount = PermutationCount.unsafe(rows.length)
    override def draw(index: Int): Either[ThresholdError, Array[Double]] = Right(rows(index).clone)

private enum CalibrationProfile(val label: String, val replicates: Int, val baseSeed: Long):
  case PullRequest extends CalibrationProfile("pull-request", 200, 20260930L)
  case Calibration extends CalibrationProfile("calibration", 2000, 7310930L)

private object CalibrationProfile:
  val current: CalibrationProfile =
    sys.env.get("SCALAFIM_THRESHOLD_CALIBRATION") match
      case None | Some("") | Some("pull-request") => PullRequest
      case Some("calibration")                    => Calibration
      case Some(other) =>
        throw new IllegalArgumentException(s"SCALAFIM_THRESHOLD_CALIBRATION must be 'pull-request' or 'calibration', got '$other'")

private object Binomial:
  private def pmf(r: Int, p: Double): Array[Double] =
    val out = new Array[Double](r + 1)
    out(0) = math.pow(1.0 - p, r.toDouble)
    var k = 0
    while k < r do
      out(k + 1) = out(k) * (r - k).toDouble / (k + 1).toDouble * p / (1.0 - p)
      k += 1
    out

  def upperTail(r: Int, p: Double, k: Int): Double =
    math.min(1.0, pmf(r, p).drop(k).sum)

  def lowerTail(r: Int, p: Double, k: Int): Double =
    math.min(1.0, pmf(r, p).take(k + 1).sum)
