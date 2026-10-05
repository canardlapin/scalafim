package scalafim.phrfcmp.score

/** Deterministic synthetic corpora for the scorer tests (no generator data needed). */
object ScoreSynth:
  final class Rng(seed: Long):
    private var s = seed
    def nextLong(): Long =
      s += 0x9e3779b97f4a7c15L
      var z = s
      z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
      z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
      z ^ (z >>> 31)
    def uniform(): Double = (nextLong() >>> 11).toDouble / (1L << 53).toDouble
    def normal(): Double = (0 until 12).map(_ => uniform()).sum - 6.0

  def ok[A](e: Either[ScoreError, A]): A = e.fold(x => sys.error(x.message.text), identity)

  val Offsets: Map[Method, Double] = Map(Method.Can -> 0.0, Method.Inf3 -> 0.1, Method.Fir -> -0.1)

  /** lambda_d = planted + e_d for every comparator pair (plus a per-method offset). `compScale` multiplies a
    * comparator's errors; `refuse(method, dataset, voxel)` marks refused voxels.
    */
  def conditionDataset(
      cell: PilotCell,
      d: Int,
      planted: Double,
      voxels: Int = 8,
      compScale: Map[Method, Double] = Map.empty,
      refuse: (Method, Int, Int) => Boolean = (_, _, _) => false,
      failed: (Method, Int, Int) => Boolean = (_, _, _) => false
  ): ConditionDataset =
    val rng = new Rng(1000L * cell.ordinal + d)
    val e = (rng.uniform() - 0.5) * 0.2
    val base = Vector.fill(voxels)(1.0 + rng.uniform())
    val arms = Method.all.take(4).map { m =>
      val vs = base.zipWithIndex.map { (b, v) =>
        if refuse(m, d, v) then VoxelOutcome.Refused
        else if failed(m, d, v) then VoxelOutcome.Failed
        else if m == Method.Phrf then VoxelOutcome.Estimated(b)
        else VoxelOutcome.Estimated(b * math.exp(-(planted + e + Offsets(m))) * compScale.getOrElse(m, 1.0))
      }
      m -> vs
    }.toMap
    ok(ConditionDataset.of(d, 1.0, 32.0, arms))

  private val NoiseSd: Map[Method, Double] =
    Map(Method.Phrf -> 0.5, Method.Lsa -> 1.0, Method.Lss -> 0.8, Method.Rlss -> 0.6, Method.GlmsD -> 0.9)

  def trialDataset(
      cell: PilotCell,
      d: Int,
      voxels: Int = 6,
      refuse: (Method, Int, Int) => Boolean = (_, _, _) => false,
      failed: (Method, Int, Int) => Boolean = (_, _, _) => false,
      constantTruth: Boolean = false
  ): TrialDataset =
    val rng = new Rng(77000L + 1000L * cell.ordinal + d)
    val conds = Vector.tabulate(144)(_ % 3)
    val truth = Vector.fill(voxels)(Vector.fill(144)(rng.normal()))
    val truthShown = if constantTruth then truth.map(_.map(_ => 1.0)) else truth
    val methods = if cell == PilotCell.TTSFast then Family.TTs.gatingArms else Family.TG.gatingArms
    val arms = methods.map { m =>
      m -> truth.zipWithIndex.map { (t, v) =>
        if refuse(m, d, v) then VoxelOutcome.Refused
        else if failed(m, d, v) then VoxelOutcome.Failed
        else VoxelOutcome.Estimated(ok(TrialEstimate.of(t.map(_ + NoiseSd(m) * rng.normal()))))
      }
    }.toMap
    ok(TrialDataset.of(d, ok(TrialTruth.of(conds, truthShown)), arms))

  /** Estimate whose within-condition Pearson correlation with `t` is exactly `r` (Gram-Schmidt construction). */
  def correlated(t: Vector[Double], conds: Vector[Int], r: Double, rng: Rng): Vector[Double] =
    val out = new Array[Double](t.length)
    conds.distinct.foreach { c =>
      val idx = conds.indices.filter(conds(_) == c)
      val tm = idx.map(t(_)).sum / idx.length
      val tc = idx.map(t(_) - tm)
      val nt = math.sqrt(tc.map(x => x * x).sum)
      val th = tc.map(_ / nt)
      val u0 = idx.map(_ => rng.normal())
      val um = u0.sum / u0.length
      val u1 = u0.map(_ - um)
      val proj = u1.zip(th).map((a, b) => a * b).sum
      val u2 = u1.zip(th).map((a, b) => a - proj * b)
      val nu = math.sqrt(u2.map(x => x * x).sum)
      idx.indices.foreach(k => out(idx(k)) = nt * (r * th(k) + math.sqrt(1.0 - r * r) * u2(k) / nu))
    }
    out.toVector

  /** Trial dataset in which arm `m` has voxel-level Fisher z exactly `z(m, voxel)` (every condition), so endpoints are
    * known in closed form. `refuse(m, voxel)` marks refusals.
    */
  def exactTrialDataset(
      cell: PilotCell,
      d: Int,
      z: (Method, Int) => Double,
      refuse: (Method, Int) => Boolean = (_, _) => false,
      methods: Option[Vector[Method]] = None,
      voxels: Int = 4
  ): TrialDataset =
    val rng = new Rng(31000L + 1000L * cell.ordinal + d)
    val conds = Vector.tabulate(144)(_ % 3)
    val truth = Vector.fill(voxels)(Vector.fill(144)(rng.normal()))
    val ms = methods.getOrElse(if cell == PilotCell.TTSFast then Family.TTs.gatingArms else Family.TG.gatingArms)
    val arms = ms.map { m =>
      m -> truth.zipWithIndex.map { (t, v) =>
        if refuse(m, v) then VoxelOutcome.Refused
        else VoxelOutcome.Estimated(ok(TrialEstimate.of(correlated(t, conds, math.tanh(z(m, v)), rng))))
      }
    }.toMap
    ok(TrialDataset.of(d, ok(TrialTruth.of(conds, truth)), arms))

  def bits(d: Double): Vector[Vector[Byte]] =
    val l = java.lang.Double.doubleToLongBits(d)
    val be = Vector.tabulate(8)(i => ((l >>> (8 * (7 - i))) & 0xffL).toByte)
    Vector(be, be.reverse)

  def containsBytes(hay: Array[Byte], needle: Vector[Byte]): Boolean =
    hay.indices.exists(i => i + needle.length <= hay.length && needle.indices.forall(j => hay(i + j) == needle(j)))

  def textHits(text: String, planted: String): Boolean =
    val tokens = """-?[0-9]+(\.[0-9]+)?([eE][-+]?[0-9]+)?""".r.findAllIn(text).toVector
    text.contains("0." + planted) || tokens.exists(t => t.filter(_.isDigit).dropWhile(_ == '0').startsWith(planted))

  /** Coverage proportion about `pct` percent. */
  def coverageDataset(d: Int, pct: Int, voxels: Int = 40): CoverageDataset =
    val rng = new Rng(555000L + d)
    val clusterRel = rng.normal()
    val clusterTau = rng.normal()
    val vs = Vector.tabulate(voxels) { v =>
      VoxelOutcome.Estimated(ok(CoverageObs.of(clusterRel + 0.5 * rng.normal(), clusterTau + 0.5 * rng.normal(), (v * 37 + d * 11) % 100 < pct)))
    }
    ok(CoverageDataset.of(d, vs))

  def timing: TimingSamples =
    ok(
      TimingSamples.of(
        Vector(0.021, 0.023, 0.025),
        Vector(0.4, 0.5, 0.6),
        Vector(Vector(0.5, 0.55, 0.52, 0.5, 0.51, 0.53, 0.5, 0.5, 0.54), Vector(0.6, 0.6, 0.62, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6)),
        Vector(9.0, 10.0, 12.0),
        Vector(0.2, 0.24, 0.3)
      )
    )

  def corpus(
      d: Int = 20,
      planted: Double = 0.123456,
      coveragePct: Int = 50,
      refusals: Boolean = false,
      compScale: Map[Method, Double] = Map.empty,
      totalCpuSeconds: Double = 7200.0,
      withFailures: Boolean = false,
      trialFn: Option[(PilotCell, Int) => TrialDataset] = None,
      constantTruthCell: Option[(PilotCell, Int)] = None
  ): PilotCorpus =
    val refuseC: (Method, Int, Int) => Boolean =
      if refusals then (m, dd, v) => m == Method.Phrf && (v + dd) % 5 == 0 || m == Method.Can && (v * 3 + dd) % 11 == 0 else (_, _, _) => false
    val refuseT: (Method, Int, Int) => Boolean =
      if refusals then (m, dd, v) => m == Method.Phrf && (v + dd) % 4 == 0 || m == Method.Lss && v == 2 && dd % 3 == 0 else (_, _, _) => false
    val failC: (Method, Int, Int) => Boolean = if withFailures then (m, dd, v) => m == Method.Fir && (v + dd) % 3 == 0 else (_, _, _) => false
    val failT: (Method, Int, Int) => Boolean = if withFailures then (m, dd, v) => m == Method.GlmsD && (v + dd) % 2 == 0 else (_, _, _) => false
    def trialOf(c: PilotCell, dd: Int): TrialDataset = trialFn match
      case Some(f) => f(c, dd)
      case None    => trialDataset(c, dd, refuse = refuseT, failed = failT, constantTruth = constantTruthCell.contains((c, dd)))
    ok(
      PilotCorpus.of(
        PilotCell.conditionCells.map(c => c -> Vector.tabulate(d)(conditionDataset(c, _, planted, compScale = compScale, refuse = refuseC, failed = failC))).toMap,
        PilotCell.trialCells.map(c => c -> Vector.tabulate(d)(trialOf(c, _))).toMap,
        Vector.tabulate(d)(coverageDataset(_, coveragePct)),
        timing,
        totalCpuSeconds
      )
    )
