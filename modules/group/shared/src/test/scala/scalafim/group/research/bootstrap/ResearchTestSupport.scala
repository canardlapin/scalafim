package scalafim.group.research.bootstrap

/** Shared helpers for the research suites. Heavier Monte Carlo checks run only
  * when the JVM is started with -Dscalafim.group.bootstrapResearch.heavy=true
  * (never on JS, whose system properties are empty).
  */
object ResearchTestSupport:
  def heavy: Boolean = sys.props.get("scalafim.group.bootstrapResearch.heavy").contains("true")

  val howToEnableHeavy = "heavy research checks skipped; pass -Dscalafim.group.bootstrapResearch.heavy=true (JVM) to run them"

  def cell(id: String): Cell =
    Cell.byId(CellId.parse(id).fold(e => throw new IllegalArgumentException(e), identity))
      .getOrElse(throw new IllegalArgumentException(s"unknown cell $id"))

  def study(id: String, index: Int, purpose: StudyPurpose = StudyPurpose.Null, phase: Phase = Phase.Harness): SimulatedStudy =
    val c = cell(id)
    ModelJ.draw(c, phase, purpose, index, c.researchDesign)

  /** The keyed Bootstrap-stream variates of a study, materialized for `draws` draws. */
  def variates(sim: SimulatedStudy, engine: BootstrapEngine, scheme: Scheme, draws: Int): MatrixDrawVariates =
    val eb = if scheme == Scheme.EmpiricalBayes then engine.hyperparameters(sim.data).toOption.flatten else None
    MatrixDrawVariates.materialize(
      StreamKey.of(sim.phase, sim.purpose, sim.cell.id, sim.index, StreamKind.Bootstrap),
      sim.cell.n, draws, engine.chiDf(sim.data, scheme), engine.posteriorDf(sim.data, scheme, eb)
    )

  def value[E, A](e: Either[E, A]): A = e.fold(err => throw new IllegalStateException(err.toString), identity)

  /** Two-sided Clopper-Pearson check that `k` of `r` is consistent with rate `rate` at total tail `alpha`. */
  def consistentWithRate(k: Int, r: Int, rate: Double, alpha: Double): Boolean =
    ClopperPearson.lower(k, r, alpha / 2.0) <= rate && rate <= ClopperPearson.upper(k, r, alpha / 2.0)

  /** Pearson chi-square statistic of ranks 1..m (m = bins * width) grouped into `bins` equal bins. */
  def rankChiSquare(ranks: Array[Int], m: Int, bins: Int): Double =
    require(m % bins == 0, "ranks must split evenly into bins")
    val width = m / bins
    val counts = new Array[Int](bins)
    ranks.foreach(r => counts((r - 1) / width) += 1)
    val expected = ranks.length.toDouble / bins
    counts.map(c => (c - expected) * (c - expected) / expected).sum

  /** Kolmogorov distance between the empirical law of ranks and the discrete uniform on 1..m. */
  def rankKs(ranks: Array[Int], m: Int): Double =
    val counts = new Array[Int](m + 1)
    ranks.foreach(r => counts(r) += 1)
    var cum = 0
    var d = 0.0
    var j = 1
    while j <= m do
      cum += counts(j)
      d = math.max(d, math.abs(cum.toDouble / ranks.length - j.toDouble / m))
      j += 1
    d

  /** qchisq(0.999, 49) from R 4.x; recorded in the reference fixture and re-checked there. */
  val ChiSquare49At999 = 85.35056461

  /** Pre-set KS tolerance: the asymptotic 0.999 Kolmogorov quantile, sqrt(-log(0.0005)/2) = 1.9495, over sqrt(R). */
  def ksTolerance(r: Int): Double = math.sqrt(-math.log(0.0005) / 2.0) / math.sqrt(r.toDouble)
