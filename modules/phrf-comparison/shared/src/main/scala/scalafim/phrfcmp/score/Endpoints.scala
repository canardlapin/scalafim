package scalafim.phrfcmp.score

/** One pair's dataset-level quantities, split into what may feed `Centred` and the sealed counts. */
final class PairDatasetResult private[score] (
    /** Primary (worst-in-family imputed) endpoint; `None` if every gating arm failed at every voxel. */
    private[phrfcmp] val endpoint: Option[Double],
    /** Complete-case endpoint (voxels where every gating arm estimated); sealed. */
    private[phrfcmp] val completeCaseEndpoint: Option[Double],
    private[phrfcmp] val imputedVoxels: Int,
    private[phrfcmp] val excludedVoxels: Int,
    private[phrfcmp] val floorUses: Int,
    private[phrfcmp] val zeroVarianceEstimates: Int
):
  override def toString: String = "PairDatasetResult(<redacted>)"

object Endpoints:
  /** The zero-MISE floor `1e-9 * sigma2_noise * H` of protocol section 6 (R3). */
  def miseFloor(noiseVariance: Double, horizonSeconds: Double): Double = 1e-9 * noiseVariance * horizonSeconds

  /** `lambda = log(MISE_phrf / MISE_comp)`; a MISE of exactly 0 is replaced by `floor`. Returns the value and the
    * number of floors applied.
    */
  def lambda(misePhrf: Double, miseComp: Double, floor: Double): (Double, Int) =
    val (p, np) = if misePhrf == 0.0 then (floor, 1) else (misePhrf, 0)
    val (c, nc) = if miseComp == 0.0 then (floor, 1) else (miseComp, 0)
    (math.log(p / c), np + nc)

  private def mean(xs: Vector[Double]): Double =
    var s = 0.0
    xs.foreach(s += _)
    s / xs.length

  /** Condition endpoint for the pair (PHRF, `comp`) on one dataset. */
  def conditionPair(ds: ConditionDataset, pair: GatingPair): PairDatasetResult =
    val pool = pair.family.gatingArms
    val scores = pool.map(m => ds.arms(m).map(_.toOption))
    val imp = Imputation.worstInFamily(scores, Worse.Higher)
    val iP = pool.indexOf(Method.Phrf)
    val iC = pool.indexOf(pair.comparator)
    val floor = miseFloor(ds.noiseVariance, ds.horizonSeconds)
    if imp.values.head.isEmpty then new PairDatasetResult(None, None, 0, imp.excluded, 0, 0)
    else
      val (lam, nf) = lambda(mean(imp.values(iP)), mean(imp.values(iC)), floor)
      val cc = completeCaseSubset(imp, iP, iC)
      val ccLam = cc.map((p, c) => lambda(mean(p), mean(c), floor)._1)
      new PairDatasetResult(Some(lam), ccLam, imp.imputedPerArm(iP) + imp.imputedPerArm(iC), imp.excluded, nf, 0)

  /** Trial endpoint `delta z = z_phrf - z_comp` for the pair on one dataset; `Left(TruthDegenerate)` when the true
    * within-condition variance is zero (the cell would report E-dev MSE only).
    */
  def trialPair(ds: TrialDataset, pair: GatingPair): Either[ScoreError, PairDatasetResult] =
    val pool = pair.family.gatingArms
    var zeroVar = 0
    var err: Option[ScoreError] = None
    val scores: Vector[Vector[Option[Double]]] = pool.map { m =>
      ds.arms(m).zipWithIndex.map { (o, v) =>
        o.toOption.flatMap { est =>
          FisherZ.voxelZ(est.values, ds.truth.amplitudes(v), ds.truth.conditionOfTrial, ds.truth.conditions) match
            case Right((z, zv)) =>
              if m == Method.Phrf || m == pair.comparator then zeroVar += zv
              Some(z)
            case Left(e) =>
              if err.isEmpty then err = Some(e)
              None
        }
      }
    }
    err match
      case Some(ScoreError.TruthDegenerate(_)) => Left(ScoreError.TruthDegenerate(pair.cell))
      case Some(e) => Left(e)
      case None =>
        val imp = Imputation.worstInFamily(scores, Worse.Lower)
        val iP = pool.indexOf(Method.Phrf)
        val iC = pool.indexOf(pair.comparator)
        if imp.values.head.isEmpty then Right(new PairDatasetResult(None, None, 0, imp.excluded, 0, zeroVar))
        else
          val dz = mean(imp.values(iP)) - mean(imp.values(iC))
          val cc = completeCaseSubset(imp, iP, iC).map((p, c) => mean(p) - mean(c))
          Right(new PairDatasetResult(Some(dz), cc, imp.imputedPerArm(iP) + imp.imputedPerArm(iC), imp.excluded, 0, zeroVar))

  private def completeCaseSubset(imp: Imputed, iP: Int, iC: Int): Option[(Vector[Double], Vector[Double])] =
    val keep = imp.completeCase.zipWithIndex.collect { case (true, i) => i }
    if keep.isEmpty then None else Some((keep.map(imp.values(iP)(_)), keep.map(imp.values(iC)(_))))

/** Within-condition Pearson r of estimated against true E-trial, Fisher-z transformed, averaged over conditions. */
object FisherZ:
  /** |r| is clamped to this bound before `atanh` so a perfect correlation does not give an infinity. */
  val RClamp: Double = 1.0 - 1e-12

  /** Relative spread at or below which a sample counts as constant. */
  private val ConstantTol = 1e-12

  private def isConstant(xs: Vector[Double]): Boolean =
    val m = xs.map(math.abs).max
    val mu = xs.sum / xs.length
    val sd = math.sqrt(xs.map(x => (x - mu) * (x - mu)).sum / xs.length)
    sd <= ConstantTol * m

  /** Voxel-level z (mean over conditions) and the number of conditions whose estimate had zero variance (assigned
    * z = 0, which penalizes full shrinkage). `Left(TruthDegenerate)` if the truth is constant within a condition.
    */
  def voxelZ(est: Vector[Double], truth: Vector[Double], condOfTrial: Vector[Int], conditions: Int): Either[ScoreError, (Double, Int)] =
    var total = 0.0
    var zeroVar = 0
    var c = 0
    var bad = false
    while c < conditions && !bad do
      val idx = condOfTrial.zipWithIndex.collect { case (k, i) if k == c => i }
      val e = idx.map(est(_))
      val t = idx.map(truth(_))
      if isConstant(t) then bad = true
      else if isConstant(e) then zeroVar += 1
      else
        val me = e.sum / e.length
        val mt = t.sum / t.length
        var sxy = 0.0
        var sxx = 0.0
        var syy = 0.0
        var i = 0
        while i < e.length do
          val dx = e(i) - me
          val dy = t(i) - mt
          sxy += dx * dy
          sxx += dx * dx
          syy += dy * dy
          i += 1
        val r = math.max(-RClamp, math.min(RClamp, sxy / math.sqrt(sxx * syy)))
        total += 0.5 * math.log((1.0 + r) / (1.0 - r))
      c += 1
    if bad then Left(ScoreError.TruthDegenerate(PilotCell.TTXFast)) else Right((total / conditions, zeroVar))
