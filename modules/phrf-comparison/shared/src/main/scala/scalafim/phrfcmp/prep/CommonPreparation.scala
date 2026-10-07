package scalafim.phrfcmp.prep

import scalafim.fmri.ar.{TimeSegment, WhiteningPlan}
import scalafim.fmri.model.FitConfig
import scalafim.phrfcmp.ingest.{CellKind, Digests, FitInputs, Matrix}

/**
  * Exact fingerprint of the raw inputs a preparation was built from: SHA-256 over a canonical byte form of the event
  * onsets, conditions, durations and runs, the sample times and run ids, the nuisance and y. Every double enters as its
  * IEEE-754 bits (`doubleToLongBits`), so any change, however small, changes the hash. It is a hash of INPUTS only: no
  * truth, no result, nothing that is custody-bearing.
  */
object InputFingerprint:

  def of(i: FitInputs): String =
    val parts: Vector[(String, Either[Array[Int], Array[Double]], Int, Int)] = Vector(
      ("ev_onset", Right(i.evOnset), i.evOnset.length, 1),
      ("ev_cond", Left(i.evCond), i.evCond.length, 1),
      ("ev_duration", Right(i.evDuration), i.evDuration.length, 1),
      ("ev_run", Left(i.evRun), i.evRun.length, 1),
      ("sample_time", Right(i.sampleTime), i.sampleTime.length, 1),
      ("run_id", Left(i.runId), i.runId.length, 1),
      ("nuisance", Right(i.nuisance.data), i.nuisance.rows, i.nuisance.cols),
      ("y", Right(i.y.data), i.y.rows, i.y.cols)
    )
    val size = parts.map { (n, a, _, _) =>
      n.length + 12 + (a match
        case Left(xs)  => xs.length * 4
        case Right(xs) => xs.length * 8)
    }.sum
    val out = new Array[Byte](size)
    var o = 0
    def put(b: Int): Unit =
      out(o) = b.toByte
      o += 1
    def put32(v: Int): Unit =
      put(v >>> 24); put(v >>> 16); put(v >>> 8); put(v)
    def put64(v: Long): Unit =
      put32((v >>> 32).toInt); put32(v.toInt)
    parts.foreach { (n, a, rows, cols) =>
      n.foreach(ch => put(ch.toInt))
      put32(rows); put32(cols)
      a match
        case Left(xs)  => put32(xs.length); xs.foreach(put32)
        case Right(xs) => put32(xs.length); xs.foreach(d => put64(java.lang.Double.doubleToLongBits(d)))
    }
    Digests.sha256Hex(out)

/**
  * Everything common to every native arm of one dataset (design 2.0). Built from a [[FitInputs]] only: neither the
  * pre-fit nor the whitening can reach truth.
  *
  * @param baselineNuisance  generator nuisance with the per-run intercepts dropped (feeds the shared baseline)
  * @param whitened          the one whitened array set
  * @param sigma2All         sigma2 from all runs (the final fit); fold values come from [[sigma2Training]]
  * @param inputFingerprint [[InputFingerprint]] of the raw inputs this preparation was built from
  * @param heterogeneity     per-run rho and voxelwise spread; a record only, it does not gate
  */
final case class CommonPrep(
    kind: CellKind,
    rho: Double,
    prefit: PrefitResult,
    split: SplitNuisance,
    segments: Vector[TimeSegment],
    spec: WhiteningSpec,
    whitened: WhitenedArrays,
    sigma2All: Sigma2Result,
    runId: Array[Int],
    inputFingerprint: String
):
  /** True iff `inputs` are bit-for-bit the inputs this preparation was built from (one check for every runner). */
  def matches(inputs: FitInputs): Boolean = InputFingerprint.of(inputs) == inputFingerprint

  /** As [[matches]], as a typed refusal. */
  def requireMatches(inputs: FitInputs): Either[PrepRefusal, Unit] =
    if matches(inputs) then Right(()) else Left(PrepRefusal.InputsDiffer(inputFingerprint, InputFingerprint.of(inputs)))

  def plan: WhiteningPlan = spec.plan
  def phrfConfig: FitConfig = spec.phrfConfig
  def baselineNuisance: Matrix = split.dropped
  def heterogeneity: ArHeterogeneityRecord = prefit.heterogeneity

  /** Provenance of the AR fit behind `rho`: policy, budget, per-run conditioning, phi and gamma. */
  def arFit: ArFitProvenance = prefit.provenance

  /** The arrays handed to `arm`: always the same shared instance. */
  def armInput(arm: NativeArm): ArmInput = ArmInput(arm, whitened)

  def armInputs: Vector[ArmInput] = NativeArm.forKind(kind).map(armInput)

  /** Per-arm derived-input hashes for the ledger; identical across arms by construction. */
  def armHashes: Map[NativeArm, String] = armInputs.map(a => a.arm -> a.sha256).toMap

  /** sigma2 from the training runs only (a LOROCV fold holding out one run). */
  def sigma2Training(trainingRuns: Set[Int]): Either[PrepRefusal, Sigma2Result] =
    Sigma2.pooled(whitened, runId, Some(trainingRuns))

  /** Whiten an arm-specific design (T x k) with the common plan. */
  def whitenDesign(design: Matrix): Either[PrepRefusal, Matrix] = Whiten.columns(plan, design)

object CommonPreparation:

  /** Intercept drop, specified pre-fit, one global AR(1), plan and matching `FitConfig`, whitened arrays, sigma2. */
  def prepare(inputs: FitInputs, kind: CellKind): Either[PrepRefusal, CommonPrep] =
    for
      segments <- Runs.segments(inputs.runId)
      split <- Nuisance.dropIntercepts(inputs.nuisance, inputs.runId)
      fitted <- FirPrefit.prefit(inputs, kind)
      (pre, design) = fitted
      spec <- WhiteningSpec.build(pre.rho, segments)
      wy <- Whiten.series(spec.plan, inputs.y)
      wn <- Whiten.columns(spec.plan, split.dropped)
      wi <- Whiten.columns(spec.plan, split.runIntercepts)
      wd <- Whiten.columns(spec.plan, design)
      whitened = WhitenedArrays(wy, wn, wi, wd)
      s2 <- Sigma2.pooled(whitened, inputs.runId, None)
    yield CommonPrep(kind, pre.rho, pre, split, segments, spec, whitened, s2, inputs.runId, InputFingerprint.of(inputs))
