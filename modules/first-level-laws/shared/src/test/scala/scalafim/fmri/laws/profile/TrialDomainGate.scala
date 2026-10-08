package scalafim.fmri.laws.profile

import gale.spectral.RealInterval
import scalafim.fmri.design.hrf.{KernelBasisCompilation, TrialDesignLowering}
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.family.{Cascade34Family, NormalizationRule, ShapeChart, ShapeChartError, ShapePoint}
import scalafim.scenarios.*

/** Frozen candidate domain, not a default change or a calibrated physiological prior.
  * The family has different component orders from SPM; only component modes and
  * area ratio anchor this deliberately declared sensitivity envelope.
  */
object TrialDomainProposal:
  val protocolId = "canonical-like-fixed-shape/v1"
  val protocolSha256 = "29fc8e066b28d3d15cf1879868b7096555e059e318d93d868f849cdc2c586aa3"
  val horizon = 96.0
  val chart: ShapeChart = ShapeChart(
    ("logKappaP", math.log(1.0 / 3.0), math.log(0.5)),
    ("logitRateRatio", math.log(0.4 / 0.6), math.log(0.6 / 0.4)),
    ("rho", 0.1, 0.3))
  val family: Cascade34Family = Cascade34Family.make(chart, Seconds(horizon))
    .fold(e => throw new IllegalArgumentException(e.message), identity)
  val references: NodeGrid = NodeGrid(ShapeChart(chart.names.indices.map(i =>
    (chart.names(i), chart.lower(i) + 0.25 * chart.width(i), chart.upper(i) - 0.25 * chart.width(i)))*), Vector(2, 2, 2))

  def coordinates(unit: Vector[Double]): Vector[Double] =
    require(unit.length == 3 && unit.forall(x => x.isFinite && x >= 0.0 && x <= 1.0))
    unit.indices.map(i => chart.clamp(i, chart.lower(i) + unit(i) * chart.width(i))).toVector

  /** Only coordinates enter routing. No objective, response, residual or oracle
    * is evaluated; exactly one reference is subsequently used for readout.
    */
  def route(coordinates: Vector[Double]): Either[ShapeChartError, Int] =
    chart.point(coordinates).map: _ =>
      var best = 0
      var distance = Double.PositiveInfinity
      var node = 0
      while node < references.count do
        val ref = references.point(node)
        val candidate = coordinates.indices.map(i => math.pow((coordinates(i) - ref(i)) / chart.width(i), 2)).sum
        if candidate < distance then
          best = node
          distance = candidate
        node += 1
      best

  /** Uniform absolute L1 bound on omitted HRF tail, using unit-area Erlang
    * survival functions and the triangle inequality. This is not a coefficient,
    * derivative or decoder-error certificate.
    */
  val tailMassUpper: Double =
    def checked(value: Either[gale.linalg.LinAlgError, RealInterval]): RealInterval = value.fold(throw _, identity)
    def point(x: Double) = checked(RealInterval.exact(x))
    val one = point(1.0)
    val positive = checked(point(chart.lower(0)).exp)
    val negativeLogit = checked(point(0.0).subtract(point(chart.lower(1))))
    val ratio = checked(one.divide(checked(one.add(checked(negativeLogit.exp)))))
    val under = checked(positive.multiply(ratio))
    def survival(order: Int, minimumRate: Double): RealInterval =
      val x = checked(point(minimumRate).multiply(point(horizon)))
      var term = one
      var sum = one
      var k = 1
      while k < order do
        term = checked(checked(term.multiply(x)).divide(point(k.toDouble)))
        sum = checked(sum.add(term))
        k += 1
      checked(sum.multiply(checked(checked(point(0.0).subtract(x)).exp)))
    checked(survival(3, positive.lower).add(checked(point(chart.upper(2)).multiply(survival(4, under.lower))))).upper

  def fixture(trials: Int): DecodedTrialCheckpoint.Fixture =
    DecodedTrialCheckpoint.fixture(DecodedTrialCheckpoint.Config(
      DecodedTrialCheckpoint.Geometry.B0Dense, voxels = 1, trials = trials, blockSize = 1,
      compilation = KernelBasisCompilation.BlockedPartial(96),
      trialPreparation = TrialPreparationPolicy(TrialDesignLowering.Blocked(32)),
      horizonSeconds = Some(horizon), basisMaxRank = 24), Some(family))

object TrialDomainGate:
  final case class Sample(id: String, cohort: String, unit: Vector[Double], noiseRatio: Double, responseSeed: Long)
  final case class Record(sample: Sample, reference: Int, coordinates: Vector[Double],
      preparedRelativeError: Double, originalRelativeError: Double, exactOriginalRelativeError: Double,
      originalContrastError: Double, preparedResidual: Double, relativeTailDesignError: Double,
      relativeBasisDesignError: Double, evaluatedReferences: Int, work: ProfileTrialReadoutWork)
  final case class Coverage(cohort: String, noiseRatio: Double, count: Int, correctedPasses: Int,
      exactPasses: Int, preparedPasses: Int, originalP95: Double, preparedP95: Double, exactP95: Double)
  final case class Geometry(trials: Int, rank: Int, bandwidth: Int, preparationScalars: Long,
      sourceBytes: Long, fullReferenceBytes: Long, valueReferenceBytes: Long, workerBytes: Long,
      sharedWithFullBankBytes: Long, sharedWithValueBankBytes: Long, scopedFullBankEightWorkersBytes: Long,
      preparationSeconds: Double)
  final case class ShapeSummary(unit: Vector[Double], peak: Double, fwhm: Double, trough: Option[Double], troughToPeak: Option[Double])
  final case class Result(geometry: Geometry, coverage: Vector[Coverage], records: Vector[Record],
      conditionalScenario: ScenarioResult, productionScenario: ScenarioResult)

  def summaries: Vector[ShapeSummary] = boundaryUnits.map: unit =>
    val value = TrialDomainProposal.family.attainedSummaries(ShapePoint.unsafe(TrialDomainProposal.coordinates(unit)))
    ShapeSummary(unit, value.peakLatency.value, value.fwhm.value, value.undershoot.map(_.latency.value),
      value.undershoot.map(_.ratioToPeak))

  def boundaryUnits: Vector[Vector[Double]] =
    Vector.tabulate(27)(i => Vector((i % 3) / 2.0, ((i / 3) % 3) / 2.0, (i / 9) / 2.0))

  def samples(trials: Int, freshCount: Int, sensitivityCount: Int, boundaries: Boolean): Vector[Sample] =
    val rng = new scala.util.Random(if trials == 30 then 2026100803L else 2026100804L)
    val fresh = Vector.fill(freshCount)(Vector.fill(3)(rng.nextDouble()))
    def make(cohort: String, unit: Vector[Double], index: Int, noise: Double) =
      Sample(s"$cohort-$index-$noise", cohort, unit, noise, 2026100805L + 10000L * trials + index)
    fresh.zipWithIndex.map((u, i) => make("fresh", u, i, 0.1)) ++
      (if boundaries then boundaryUnits.zipWithIndex.map((u, i) => make("boundary", u, freshCount + i, 0.1)) else Vector.empty) ++
      Vector(0.0, 1.0).flatMap(noise => fresh.take(sensitivityCount).zipWithIndex.map((u, i) => make("sensitivity", u, i, noise)))

  private def geometry(f: DecodedTrialCheckpoint.Fixture, bank: TrialBandedObjective, seconds: Double): Geometry =
    val p = bank.preparation
    val shared = bank.estimatedSharedBytes + p.retainedSourceDesignDataBytes
    Geometry(f.trials, p.basisRank, p.bandwidth, p.receipt.retainedDoubles, p.retainedSourceDesignDataBytes,
      bank.estimatedReferenceBytes, bank.estimatedValueReferenceBytes, bank.estimatedWorkerBytes,
      shared, shared - 8L * bank.estimatedReferenceBytes + 8L * bank.estimatedValueReferenceBytes,
      shared + 8L * bank.estimatedWorkerBytes, seconds)

  def preparation(trials: Int): Either[String, Geometry] =
    val start = System.nanoTime()
    try
      val f = TrialDomainProposal.fixture(trials)
      f.prepare.flatMap(_.trialOutputs).left.map(_.message).flatMap: outputs =>
        outputs.axis.preparation.objective(TrialDomainProposal.references).left.map(_.message)
          .map(bank => geometry(f, bank, (System.nanoTime() - start) / 1e9))
    catch case e: IllegalArgumentException => Left(e.getMessage)

  def run(trials: Int, freshCount: Int, sensitivityCount: Int, boundaries: Boolean = true,
      completed: String => Unit = _ => ()): Result =
    val start = System.nanoTime()
    val f = TrialDomainProposal.fixture(trials)
    val outputs = f.prepare.flatMap(_.trialOutputs).fold(e => throw new IllegalArgumentException(e.message), identity)
    val axis = outputs.axis
    val bank = axis.preparation.objective(TrialDomainProposal.references).fold(e => throw new IllegalArgumentException(e.message), identity)
    val g = geometry(f, bank, (System.nanoTime() - start) / 1e9)
    val times = f.dataset.samplingFrame.samples().map(_.value)
    val onsets = TrialNeighborhoodAudit.onsets(f)
    val rows = samples(trials, freshCount, sensitivityCount, boundaries).zipWithIndex.map: (sample, index) =>
      val actual = TrialDomainProposal.coordinates(sample.unit)
      val node = TrialDomainProposal.route(actual).fold(e => throw new IllegalArgumentException(e.toString), identity)
      val design = TrialNeighborhoodAudit.originalDesign(f, actual)
      val raw = TrialNeighborhoodAudit.response(f, design, sample.noiseRatio, sample.responseSeed)
      val oracle = f.copy(rawBlock = raw).oracleDesign(0, design).take(trials)
      val input = ProfileTrialResponse.make(axis, axis.selectedResponseRows, ProfileTrialResponseDomain.Original, raw)
        .fold(e => throw new IllegalArgumentException(e.message), identity)
      def evaluate(mode: ProfileTrialReadoutMode) =
        ProfileTrialReadout.freeze(bank, axis, actual, node, NormalizationRule.Unnormalised, mode)
          .flatMap(_.newWorker().evaluate(input, DecodedTrialCheckpoint.request))
          .fold(e => throw new IllegalArgumentException(e.message), identity)
      val corrected = evaluate(ProfileTrialReadoutMode.CorrectedReference)
      // Explicit diagnostic comparator; it never changes routing or readout.
      val exact = evaluate(ProfileTrialReadoutMode.ExactShape).trialAmplitudes.get
      val amplitudes = corrected.trialAmplitudes.get
      def relative(a: Seq[Double], b: Seq[Double]): Double =
        math.sqrt(a.zip(b).map((x, y) => (x - y) * (x - y)).sum / b.map(x => x * x).sum)
      val truncated = Array.tabulate(design.length)(i =>
        if times(i / trials) - onsets(i % trials) > TrialDomainProposal.horizon then 0.0 else design(i))
      val represented = DecodedTrialCheckpoint.designAt(f.expanded, actual)
      val contrast = amplitudes.zip(oracle).zipWithIndex.map((pair, j) => (if j % 2 == 0 then 1.0 else -1.0) * (pair._1 - pair._2)).sum
      if (index + 1) % 32 == 0 then completed(s"N=$trials processed ${index + 1} fixed-shape requests")
      Record(sample, node, actual, relative(amplitudes, exact), relative(amplitudes, oracle), relative(exact, oracle),
        math.abs(contrast), corrected.evidence.preparedBasisNormalResidualNorm,
        relative(truncated.toIndexedSeq, design.toIndexedSeq), relative(represented.toIndexedSeq, truncated.toIndexedSeq), 1, corrected.work)
    val coverage = rows.groupBy(r => (r.sample.cohort, r.sample.noiseRatio)).toVector.sortBy(_._1).map: (key, group) =>
      def percentile(f: Record => Double): Double = group.map(f).sorted.apply(math.ceil(0.95 * group.length).toInt - 1)
      Coverage(key._1, key._2, group.length, group.count(_.originalRelativeError <= 1e-3),
        group.count(_.exactOriginalRelativeError <= 1e-3), group.count(_.preparedRelativeError <= 1e-3),
        percentile(_.originalRelativeError), percentile(_.preparedRelativeError), percentile(_.exactOriginalRelativeError))
    val primary = coverage.find(_.cohort == "fresh").get
    val conditional = ScenarioResult(s"phrf-domain-conditional-$trials", Vector(
      ScenarioHarness.fact("fresh original amplitude coverage at least 95 percent", primary.correctedPasses.toDouble / primary.count >= 0.95,
        s"${primary.correctedPasses}/${primary.count}"),
      ScenarioHarness.fact("one routed reference, one correction and no exact fallback", rows.forall(r =>
        r.evaluatedReferences == 1 && r.work.residualCorrections == 1 && r.work.referenceInverseAttempts == 3 && r.work.exactReadoutFactorAttempts == 0),
        "oracle and exact comparators are separately charged diagnostics")))
    val production = ScenarioResult(s"phrf-domain-production-$trials", conditional.observations, Vector(
      ScenarioCaveat("experimental-domain", CaveatKind.DiagnosticsGap, CaveatSeverity.Blocking, "first-level-laws", Some("PHRF-14"),
        "declared canonical-like envelope is not a calibrated physiological prior; no decoder/scientific qualification"),
      ScenarioCaveat("certificate-and-resources", CaveatKind.PublicApiGap, CaveatSeverity.Blocking, "fit/profile", Some("PHRF-11"),
        "no production original-equation certificate, live-memory or complete throughput qualification")))
    Result(g, coverage, rows, conditional, production)
