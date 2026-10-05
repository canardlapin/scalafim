package scalafim.phrfcmp.score

/** The input contract of the scorer and aggregator (slice S8).
  *
  * Arms from S3 (condition), S4 and S5 (trial) and S6 (GLMsingle) adapt to these types: each arm reports, per scored
  * voxel, either an estimate in the shape below or a refusal or failure. The raw payload types carry redacted
  * `toString`s and have no serializer; they are the in-memory form of what sits in the sealed store.
  */

/** The methods whose voxels are scored. Closed: PHRF-can is a timing probe and is not scored in the pilot. */
enum Method(val code: String):
  case Phrf extends Method("PHRF")
  case Can extends Method("CAN")
  case Inf3 extends Method("INF3")
  case Fir extends Method("FIR")
  case Lsa extends Method("LSA")
  case Lss extends Method("LSS")
  case Rlss extends Method("rLSS")
  case GlmsD extends Method("GLMs-D")

object Method:
  /** Whitelist order (one pooled refusal rate per method). */
  val all: Vector[Method] = Method.values.toVector

/** The seven pilot cells. */
enum PilotCell(val id: String, val isCondition: Boolean):
  case CTX5 extends PilotCell("C-TX-.5", true)
  case CTS1 extends PilotCell("C-TS-1", true)
  case CTS5 extends PilotCell("C-TS-.5", true)
  case CTG5 extends PilotCell("C-TG-.5", true)
  case TTXFast extends PilotCell("T-TX-fast", false)
  case TTXJit extends PilotCell("T-TX-jit", false)
  case TTSFast extends PilotCell("T-TS-fast", false)

object PilotCell:
  val conditionCells: Vector[PilotCell] = PilotCell.values.toVector.filter(_.isCondition)
  val trialCells: Vector[PilotCell] = PilotCell.values.toVector.filterNot(_.isCondition)

/** The family whose gating arms form the worst-in-family imputation pool (protocol section 7). */
enum Family(val gatingArms: Vector[Method]):
  case CTx extends Family(Vector(Method.Phrf, Method.Can, Method.Inf3, Method.Fir))
  case CTs extends Family(Vector(Method.Phrf, Method.Can, Method.Inf3, Method.Fir))
  case CTg extends Family(Vector(Method.Phrf, Method.Can, Method.Inf3, Method.Fir))
  case TTx extends Family(Vector(Method.Phrf, Method.Lsa, Method.Lss, Method.Rlss))
  case TTs extends Family(Vector(Method.Phrf, Method.Lsa, Method.Lss, Method.Rlss))
  /** The T-TX arms plus GLMs-D. */
  case TG extends Family(Vector(Method.Phrf, Method.Lsa, Method.Lss, Method.Rlss, Method.GlmsD))

/** What one arm reports for one voxel. `Refused` is a typed refusal (PHRF `DecodeStatus` other than `Accepted`,
  * dataset-level `ProfileFitError`); `Failed` is a comparator failure (rank deficiency, non-finite fit, GLMsingle
  * exit). The scorer treats them identically; both count as refused-or-failed in the pooled rate.
  */
enum VoxelOutcome[+A]:
  case Estimated(value: A)
  case Refused
  case Failed

  def toOption: Option[A] = this match
    case Estimated(v) => Some(v)
    case _            => None

  /** Not estimated. */
  def isMissing: Boolean = this match
    case Estimated(_) => false
    case _            => true

/** Condition cell, one dataset: per scored (signal) voxel the integrated squared error of the fitted response
  * against the true response, averaged over conditions (data units^2 * seconds; E-resp on the 0.1 s grid over
  * [0, H], from S3's `EResp`). `noiseVariance` is the generator's noise variance and `horizonSeconds` is H; they
  * set the zero-MISE floor `1e-9 * noiseVariance * H`. Arms present: PHRF, CAN, INF3, FIR.
  */
final class ConditionDataset private (
    private[phrfcmp] val dataset: Int,
    private[phrfcmp] val noiseVariance: Double,
    private[phrfcmp] val horizonSeconds: Double,
    private[phrfcmp] val arms: Map[Method, Vector[VoxelOutcome[Double]]]
):
  private[phrfcmp] def voxels: Int = arms.valuesIterator.next().length
  override def toString: String = "ConditionDataset(<redacted>)"

object ConditionDataset:
  def of(
      dataset: Int,
      noiseVariance: Double,
      horizonSeconds: Double,
      arms: Map[Method, Vector[VoxelOutcome[Double]]]
  ): Either[ScoreError, ConditionDataset] =
    val site = ScoreError.Site.ConditionError
    def fin(x: Double) = !x.isNaN && !x.isInfinite
    if arms.isEmpty || dataset < 0 then Left(ScoreError.TooFewValues(site, arms.size, 1))
    else if !fin(noiseVariance) || !fin(horizonSeconds) then Left(ScoreError.NonFiniteValue(site))
    else if noiseVariance < 0.0 || horizonSeconds <= 0.0 then Left(ScoreError.NegativeValue(site))
    else if arms.valuesIterator.map(_.length).toSet.size != 1 || arms.valuesIterator.next().isEmpty then
      Left(ScoreError.VoxelCountMismatch(site))
    else
      val all = arms.valuesIterator.flatMap(_.iterator.flatMap(_.toOption)).toVector
      if !all.forall(fin) then Left(ScoreError.NonFiniteValue(site))
      else if all.exists(_ < 0.0) then Left(ScoreError.NegativeValue(site))
      else Right(new ConditionDataset(dataset, noiseVariance, horizonSeconds, arms))

/** True trial amplitudes (E-trial) for the scored voxels of one trial dataset, with the condition of each trial.
  * Each voxel has one amplitude per trial (the pilot has 144 trials, 3 conditions x 48).
  */
final class TrialTruth private (
    private[phrfcmp] val conditionOfTrial: Vector[Int],
    private[phrfcmp] val conditions: Int,
    private[phrfcmp] val amplitudes: Vector[Vector[Double]]
):
  private[phrfcmp] def trials: Int = conditionOfTrial.length
  private[phrfcmp] def voxels: Int = amplitudes.length
  override def toString: String = "TrialTruth(<redacted>)"

object TrialTruth:
  def of(conditionOfTrial: Vector[Int], amplitudes: Vector[Vector[Double]]): Either[ScoreError, TrialTruth] =
    val site = ScoreError.Site.TrialTruth
    if conditionOfTrial.isEmpty || amplitudes.isEmpty || conditionOfTrial.exists(_ < 0) then Left(ScoreError.TrialLayout(site))
    else
      val k = conditionOfTrial.max + 1
      val perCond = (0 until k).map(c => conditionOfTrial.count(_ == c))
      if perCond.exists(_ < 3) || amplitudes.exists(_.length != conditionOfTrial.length) then Left(ScoreError.TrialLayout(site))
      else if !amplitudes.forall(_.forall(v => !v.isNaN && !v.isInfinite)) then Left(ScoreError.NonFiniteValue(site))
      else Right(new TrialTruth(conditionOfTrial, k, amplitudes))

/** One arm's estimated E-trial amplitudes for one voxel, in input trial order. Non-finite values are rejected at
  * construction (the adapter reports `Failed` instead).
  */
final class TrialEstimate private (private[phrfcmp] val values: Vector[Double]):
  override def toString: String = "TrialEstimate(<redacted>)"

object TrialEstimate:
  def of(values: Vector[Double]): Either[ScoreError, TrialEstimate] =
    if values.isEmpty || !values.forall(v => !v.isNaN && !v.isInfinite) then Left(ScoreError.NonFiniteValue(ScoreError.Site.TrialEstimate))
    else Right(new TrialEstimate(values))

/** Trial cell, one dataset. Arms present: PHRF, LSA, LSS, rLSS, and GLMs-D in the T-TX cells. */
final class TrialDataset private (
    private[phrfcmp] val dataset: Int,
    private[phrfcmp] val truth: TrialTruth,
    private[phrfcmp] val arms: Map[Method, Vector[VoxelOutcome[TrialEstimate]]]
):
  override def toString: String = "TrialDataset(<redacted>)"

object TrialDataset:
  def of(dataset: Int, truth: TrialTruth, arms: Map[Method, Vector[VoxelOutcome[TrialEstimate]]]): Either[ScoreError, TrialDataset] =
    val site = ScoreError.Site.TrialEstimate
    if arms.isEmpty || dataset < 0 then Left(ScoreError.TooFewValues(site, arms.size, 1))
    else if arms.valuesIterator.exists(_.length != truth.voxels) then Left(ScoreError.VoxelCountMismatch(site))
    else if arms.valuesIterator.exists(_.exists(_.toOption.exists(_.values.length != truth.trials))) then Left(ScoreError.TrialLayout(site))
    else Right(new TrialDataset(dataset, truth, arms))

/** PHRF-only per-voxel accuracy observations in C-TG-.5, for `Accepted` voxels: signed relative E-peak error, `tau`
  * error, and the 1-sigma coverage indicator (conditional on `Accepted`).
  */
final class CoverageObs private (
    private[phrfcmp] val signedRelativePeakError: Double,
    private[phrfcmp] val tauError: Double,
    private[phrfcmp] val covered: Boolean
):
  override def toString: String = "CoverageObs(<redacted>)"

object CoverageObs:
  def of(signedRelativePeakError: Double, tauError: Double, covered: Boolean): Either[ScoreError, CoverageObs] =
    if signedRelativePeakError.isNaN || signedRelativePeakError.isInfinite || tauError.isNaN || tauError.isInfinite then
      Left(ScoreError.NonFiniteValue(ScoreError.Site.CoverageObservation))
    else Right(new CoverageObs(signedRelativePeakError, tauError, covered))

/** C-TG-.5, one dataset, PHRF only. Non-`Estimated` voxels (refused, failed) are not clustered. */
final class CoverageDataset private (private[phrfcmp] val dataset: Int, private[phrfcmp] val voxels: Vector[VoxelOutcome[CoverageObs]]):
  override def toString: String = "CoverageDataset(<redacted>)"

object CoverageDataset:
  def of(dataset: Int, voxels: Vector[VoxelOutcome[CoverageObs]]): Either[ScoreError, CoverageDataset] =
    if dataset < 0 || voxels.isEmpty then Left(ScoreError.TooFewValues(ScoreError.Site.CoverageObservation, voxels.length, 1))
    else Right(new CoverageDataset(dataset, voxels))

/** The five timing quantities of protocol section 9 (a)-(e). */
enum TimingQuantity:
  case TrialMlPerVoxel, TrialPreparationPerAlpha, AlphaCacheFlatness, GlmsingleDataset, ColdConditionPreparation

/** Single-thread CPU seconds, one value per sampled unit. `alphaPreparationProfiles` holds, per sampled dataset, the
  * preparation time at each of the nine alphas; the cache report is the pooled median of max/min.
  */
final class TimingSamples private (
    private[phrfcmp] val trialMlCoreSecondsPerVoxel: Vector[Double],
    private[phrfcmp] val trialPreparationCoreSecondsPerAlpha: Vector[Double],
    private[phrfcmp] val alphaPreparationProfiles: Vector[Vector[Double]],
    private[phrfcmp] val glmsingleCoreSecondsPerDataset: Vector[Double],
    private[phrfcmp] val coldConditionPreparationCoreSeconds: Vector[Double]
):
  override def toString: String = "TimingSamples(<redacted>)"

object TimingSamples:
  def of(
      trialMlCoreSecondsPerVoxel: Vector[Double],
      trialPreparationCoreSecondsPerAlpha: Vector[Double],
      alphaPreparationProfiles: Vector[Vector[Double]],
      glmsingleCoreSecondsPerDataset: Vector[Double],
      coldConditionPreparationCoreSeconds: Vector[Double]
  ): Either[ScoreError, TimingSamples] =
    val checks: Vector[(TimingQuantity, Vector[Double])] = Vector(
      TimingQuantity.TrialMlPerVoxel -> trialMlCoreSecondsPerVoxel,
      TimingQuantity.TrialPreparationPerAlpha -> trialPreparationCoreSecondsPerAlpha,
      TimingQuantity.AlphaCacheFlatness -> alphaPreparationProfiles.flatten,
      TimingQuantity.GlmsingleDataset -> glmsingleCoreSecondsPerDataset,
      TimingQuantity.ColdConditionPreparation -> coldConditionPreparationCoreSeconds
    )
    checks.collectFirst { case (q, v) if v.isEmpty => q } match
      case Some(q) => Left(ScoreError.TimingMissing(q))
      case None =>
        if checks.exists(_._2.exists(v => v.isNaN || v.isInfinite)) then Left(ScoreError.NonFiniteValue(ScoreError.Site.Timing))
        else if checks.exists(_._2.exists(_ <= 0.0)) || alphaPreparationProfiles.exists(_.length < 2) then Left(ScoreError.NegativeValue(ScoreError.Site.Timing))
        else
          Right(
            new TimingSamples(
              trialMlCoreSecondsPerVoxel,
              trialPreparationCoreSecondsPerAlpha,
              alphaPreparationProfiles,
              glmsingleCoreSecondsPerDataset,
              coldConditionPreparationCoreSeconds
            )
          )

/** Everything the aggregator reads: the completed datasets of every cell (equal count D after the uniform
  * reverse-index drop of design 5.2, indices 0 to D-1 in order), the PHRF coverage observations of C-TG-.5, the
  * timing samples, and the total CPU seconds of the run.
  */
final class PilotCorpus private (
    private[phrfcmp] val condition: Map[PilotCell, Vector[ConditionDataset]],
    private[phrfcmp] val trial: Map[PilotCell, Vector[TrialDataset]],
    private[phrfcmp] val coverage: Vector[CoverageDataset],
    private[phrfcmp] val timing: TimingSamples,
    private[phrfcmp] val totalCpuSeconds: Double,
    val datasets: Int
):
  override def toString: String = "PilotCorpus(<redacted>)"

object PilotCorpus:
  /** Smallest D the aggregator accepts (df = D - 1 >= 14, design 5.2). */
  val MinDatasets: Int = 15

  def of(
      condition: Map[PilotCell, Vector[ConditionDataset]],
      trial: Map[PilotCell, Vector[TrialDataset]],
      coverage: Vector[CoverageDataset],
      timing: TimingSamples,
      totalCpuSeconds: Double
  ): Either[ScoreError, PilotCorpus] =
    def conditionArms = Family.CTx.gatingArms
    def need(cell: PilotCell, have: Set[Method], req: Vector[Method]): Option[ScoreError] =
      req.find(!have.contains(_)).map(ScoreError.ArmMissing(cell, _))
    val missingCell =
      PilotCell.conditionCells.find(!condition.contains(_)).orElse(PilotCell.trialCells.find(!trial.contains(_)))
    missingCell match
      case Some(c) => Left(ScoreError.CellMissing(c))
      case None =>
        val d = condition(PilotCell.CTX5).length
        val counts: Vector[(PilotCell, Int)] =
          PilotCell.conditionCells.map(c => c -> condition(c).length) ++ PilotCell.trialCells.map(c => c -> trial(c).length)
        val unequal = counts.find(_._2 != d).map((c, n) => ScoreError.UnequalDatasetCounts(c, n, d))
        val covBad = if coverage.length != d then Some(ScoreError.UnequalDatasetCounts(PilotCell.CTG5, coverage.length, d)) else None
        val indexBad =
          PilotCell.conditionCells.flatMap(c => condition(c).zipWithIndex.collectFirst { case (x, i) if x.dataset != i => ScoreError.DatasetIndexMismatch(c, i) }).headOption
            .orElse(PilotCell.trialCells.flatMap(c => trial(c).zipWithIndex.collectFirst { case (x, i) if x.dataset != i => ScoreError.DatasetIndexMismatch(c, i) }).headOption)
            .orElse(coverage.zipWithIndex.collectFirst { case (x, i) if x.dataset != i => ScoreError.DatasetIndexMismatch(PilotCell.CTG5, i) })
        val armBad: Option[ScoreError] =
          PilotCell.conditionCells.flatMap(c => condition(c).flatMap(x => need(c, x.arms.keySet, conditionArms))).headOption
            .orElse(PilotCell.trialCells.flatMap { c =>
              val req = if c == PilotCell.TTSFast then Family.TTs.gatingArms else Family.TG.gatingArms
              trial(c).flatMap(x => need(c, x.arms.keySet, req))
            }.headOption)
        unequal.orElse(covBad).orElse(indexBad).orElse(armBad) match
          case Some(e) => Left(e)
          case None =>
            if d < MinDatasets then Left(ScoreError.TooFewDatasets(d, MinDatasets))
            else if totalCpuSeconds.isNaN || totalCpuSeconds.isInfinite || totalCpuSeconds < 0.0 then Left(ScoreError.NonFiniteValue(ScoreError.Site.Corpus))
            else Right(new PilotCorpus(condition, trial, coverage, timing, totalCpuSeconds, d))
