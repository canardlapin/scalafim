package scalafim.group.research.bootstrap

/** Simulation truth for one study. */
final case class StudyTruth(sigma2: Array[Double], tau2: Double, beta: Array[Double])

final case class SimulatedStudy(cell: Cell, phase: Phase, purpose: StudyPurpose, index: Int, data: StudyData, truth: StudyTruth):
  /** Mean of log(v_i / sigma_i^2): the B-fixV known-truth stratifier (§1). */
  def meanLogVarianceRatio: Double =
    data.v.indices.map(i => math.log(data.v(i) / truth.sigma2(i))).sum / data.v.length

/** Model J (declaration §1): y_i = x_i'beta + u_i + e_i, u_i ~ N(0, tau^2),
  * e_i ~ N(0, sigma_i^2), v_i = sigma_i^2 chi^2_nu/nu, y independent of v.
  * Stress laws replace u/e (t3, lognormal) or generate (e, v) from a first-level
  * AR(1) series. Null studies have beta = 0 (so c'beta = b0 = 0); power studies
  * have beta = delta c/|c|^2.
  */
object ModelJ:
  def draw(cell: Cell, phase: Phase, purpose: StudyPurpose, study: Int, design: ResearchDesign): SimulatedStudy =
    val n = cell.n
    val outcome = StreamKey.of(phase, purpose, cell.id, study, StreamKind.Outcome)
    val firstLevel = StreamKey.of(phase, purpose, cell.id, study, StreamKind.FirstLevel)
    val sigma2 = cell.sigma2.getOrElse {
      val lane = firstLevel.lane(Lane.HierarchySigma)
      Array.fill(n)(10.0 * 0.52 / lane.nextChiSquare(10.0))
    }
    val beta = purpose match
      case StudyPurpose.Null => new Array[Double](cell.p)
      case StudyPurpose.Power =>
        val delta = cell.powerDelta.getOrElse(throw new IllegalArgumentException(s"${cell.id.value} has no power alternative"))
        val c = cell.contrast
        val cc = c.map(x => x * x).sum
        c.map(_ * delta / cc)
    val tau2 = cell.tau2.value
    val uLane = outcome.lane(Lane.SubjectU)
    val eLane = outcome.lane(Lane.SubjectE)
    val chiLane = firstLevel.lane(Lane.FirstLevelChi)
    val seriesLane = firstLevel.lane(Lane.FirstLevelSeries)
    val law = cell.errorLaw
    val trueNu = cell.trueNu.value
    val y = new Array[Double](n)
    val v = new Array[Double](n)
    var i = 0
    while i < n do
      var mean = 0.0
      var j = 0
      while j < cell.p do
        mean += design.row(i, j) * beta(j)
        j += 1
      val u = math.sqrt(tau2) * standard(law, uLane)
      cell.firstLevelSeries match
        case Some(series) =>
          val (slope, naive) = arSlope(series, seriesLane)
          y(i) = mean + u + math.sqrt(sigma2(i)) * slope
          v(i) = sigma2(i) * naive
        case None =>
          y(i) = mean + u + math.sqrt(sigma2(i)) * standard(law, eLane)
          v(i) = if trueNu.isInfinite then sigma2(i) else sigma2(i) * chiLane.nextChiSquare(trueNu) / trueNu
      i += 1
    val nu = Array.fill(n)(cell.declaredNu.value)
    SimulatedStudy(cell, phase, purpose, study, StudyData(design, y, v, nu, 0.0), StudyTruth(sigma2, tau2, beta))

  /** A unit-variance, mean-zero variate of the given law. */
  private def standard(law: ErrorLaw, lane: SplitMix64): Double = law match
    case ErrorLaw.Gaussian => lane.nextGaussian()
    case ErrorLaw.StudentT3 =>
      val z = lane.nextGaussian()
      z / math.sqrt(lane.nextChiSquare(3.0) / 3.0) / math.sqrt(3.0)
    case ErrorLaw.Lognormal =>
      val e = math.E
      (math.exp(lane.nextGaussian()) - math.sqrt(e)) / math.sqrt((e - 1.0) * e)

  /** OLS slope of a unit-variance stationary AR(1) series on (1, x), standardized
    * so that under rho = 0 it is N(0, 1); returns (slope, s^2) with s^2 = RSS/(T - 2)
    * the naive residual variance (so v = sigma^2 s^2 has nominal df T - 2).
    */
  private def arSlope(series: FirstLevelSeries, lane: SplitMix64): (Double, Double) =
    val t = series.length
    val x = series.regressor
    val xbar = x.sum / t
    val sxx = x.map(a => (a - xbar) * (a - xbar)).sum
    val eps = new Array[Double](t)
    eps(0) = lane.nextGaussian()
    var k = 1
    while k < t do
      eps(k) = series.rho * eps(k - 1) + math.sqrt(1.0 - series.rho * series.rho) * lane.nextGaussian()
      k += 1
    val ebar = eps.sum / t
    val slope = (0 until t).map(s => (x(s) - xbar) * (eps(s) - ebar)).sum / sxx
    val intercept = ebar - slope * xbar
    val rss = (0 until t).map { s =>
      val r = eps(s) - intercept - slope * x(s)
      r * r
    }.sum
    (slope * math.sqrt(sxx), rss / series.nominalDf)

/** Everything one study yields: every scheme, every baseline, the sign-flip reference. */
final case class StudyRecord(
    study: SimulatedStudy,
    schemes: Vector[(Scheme, Either[StudyFailure, BootstrapOutcome])],
    native: Either[String, NativeBaseline.Column],
    hc3Equal: Either[String, BaselineResult],
    hc3InverseV: Either[String, BaselineResult],
    oracle: Either[String, BaselineResult],
    signFlip: Option[Double]
):
  /** Level accounting (§6): a study failure or an Unresolved p counts as a rejection. */
  def levelReject(scheme: Scheme, alpha: Double): Boolean =
    schemes.find(_._1 == scheme).map(_._2) match
      case Some(Right(outcome)) => outcome.decision(alpha) != StudyDecision.Retain
      case _ => true

  /** Candidate power accounting (§6): a failure or Unresolved p counts as a non-rejection. */
  def powerReject(scheme: Scheme, alpha: Double): Boolean =
    schemes.find(_._1 == scheme).map(_._2) match
      case Some(Right(outcome)) => outcome.decision(alpha) == StudyDecision.Reject
      case _ => false

  /** Comparator power accounting: a native failure counts as a rejection. */
  def nativeReject(alpha: Double): Boolean = native.fold(_ => true, c => c.failed || c.pValue <= alpha)

/** Runs one study through all requested schemes and baselines. Used by the tests
  * at small sizes; the pilot itself is not run by this harness's tests.
  */
object StudyRunner:
  def run(cell: Cell, phase: Phase, purpose: StudyPurpose, study: Int, draws: Int, schemes: Vector[Scheme], engine: BootstrapEngine): StudyRecord =
    val simulated = ModelJ.draw(cell, phase, purpose, study, engine.design)
    val data = simulated.data
    val bootstrapKey = StreamKey.of(phase, purpose, cell.id, study, StreamKind.Bootstrap)
    val results = schemes.map(s => s -> engine.run(data, s, draws, BootstrapEngine.streamVariates(bootstrapKey)))
    val native = NativeBaseline.pmMkh(engine.design, Vector(data.y), Vector(data.v), data.b0).map(_.head)
    val signFlip =
      if engine.design.p != 1 then None
      else
        val statistic = SignFlip.pmStatistic(engine.design, data.v, data.b0)
        if cell.n <= 8 then Some(SignFlip.enumerate(data.y, data.b0, statistic).pValue(0))
        else
          val key = StreamKey.of(phase, purpose, cell.id, study, StreamKind.SignFlip)
          Some(SignFlip.monteCarlo(data.y, data.b0, 999, key.lane(Lane.Signs), statistic).pValue)
    StudyRecord(
      simulated, results, native,
      Hc3.equalWeight(engine.design, data.y, data.b0),
      Hc3.inverseVariance(engine.design, data.y, data.v, data.b0),
      OracleZ.test(engine.design, data.y, simulated.truth.sigma2, simulated.truth.tau2, data.b0),
      signFlip
    )

/** The B-fixV known-truth control (§1): conditional rejection rates by terciles of
  * the realized mean log(v_i/sigma_i^2), against the same terciles' truth.
  */
object FixVTerciles:
  final case class Tercile(lower: Double, upper: Double, studies: Int, rejections: Int):
    def rate: Double = if studies == 0 then Double.NaN else rejections.toDouble / studies

  def tabulate(records: Vector[StudyRecord], scheme: Scheme, alpha: Double): Vector[Tercile] =
    val keyed = records.map(r => r.study.meanLogVarianceRatio -> r.levelReject(scheme, alpha)).sortBy(_._1)
    val n = keyed.length
    Vector(0, 1, 2).map { t =>
      val slice = keyed.slice(t * n / 3, (t + 1) * n / 3)
      Tercile(slice.headOption.fold(Double.NaN)(_._1), slice.lastOption.fold(Double.NaN)(_._1), slice.length, slice.count(_._2))
    }
