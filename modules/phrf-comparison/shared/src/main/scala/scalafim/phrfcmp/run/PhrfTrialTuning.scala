package scalafim.phrfcmp.run

import scalafim.phrfcmp.ingest.{Digests, FitInputs, Matrix}
import scalafim.phrfcmp.prep.CommonPrep

/** What a fold-fit engine does for one (training problem, grid alpha): the ML trial fit, or the PHRF-can fixed-shape readout. */
trait PhrfFoldEngine:
  def fit(problem: PhrfTrialProblem, alphaIndex: Int, alpha: Double): Either[PhrfTrialRefusal, (PhrfTrainFit, PhrfStageSeconds)]

/**
  * The record of one fold: the held-out run, the sigma2 of the TRAINING runs, the hash of every training fit (so that a
  * fold's training side can be compared across inputs that differ only in the held-out run), and the nine held-out scores.
  */
final case class PhrfFoldRecord(
    heldOutRun: Int,
    sigma2: Double,
    trainSha256: Vector[String],
    scores: Vector[Double],
    receipts: Vector[PhrfWorkReceipt],
    /** voxels without a finite readout at some alpha of this fold: excluded from this fold's pooled score at EVERY alpha */
    excludedVoxels: Vector[Int] = Vector.empty
)

/** The LOROCV record of the PHRF arm (design 2.2): the shared selection plus one record per fold. */
final case class PhrfTuningRecord(selection: LorocvSelection, folds: Vector[PhrfFoldRecord], alphasFitted: Vector[Double]):

  def canonicalBytes: Array[Byte] =
    val b = new PhrfBytes().str("phrf-cmp-s5-tuning-v1").i32(selection.grid.size)
    selection.grid.alphas.foreach(b.f64)
    b.i32(selection.selected)
    selection.meanScores.foreach(b.f64)
    b.i32(folds.length)
    folds.foreach { f =>
      b.i32(f.heldOutRun).f64(f.sigma2)
      f.trainSha256.foreach(b.str)
      f.scores.foreach(b.f64)
      b.i32(f.excludedVoxels.length)
      f.excludedVoxels.foreach(b.i32)
    }
    b.i32(alphasFitted.length)
    alphasFitted.foreach(b.f64)
    b.bytes

  def sha256: String = Digests.sha256Hex(canonicalBytes)

/** Stage seconds of one dataset; measurements, never part of a hash. */
final case class PhrfTimings(
    foldPrepare: Vector[Vector[Double]],
    foldRun: Vector[Vector[Double]],
    foldSigma2: Vector[Double],
    foldScore: Vector[Double],
    finalPrepare: Double,
    finalRun: Double,
    finalOutputs: Double,
    voxels: Int
):
  def withFinal(s: PhrfTrialRunner.FinalSeconds): PhrfTimings = copy(finalPrepare = s.prepare, finalRun = s.run, finalOutputs = s.outputs)

  /** Protocol section 9 (a): trial ML seconds per voxel, one per fold fit. */
  def trialMlSecondsPerVoxel: Vector[Double] = foldRun.flatten.map(_ / math.max(1, voxels))

  /** Section 9 (b): trial preparation seconds, one per fold fit. */
  def trialPreparationSecondsPerFit: Vector[Double] = foldPrepare.flatten

  /** Section 9 (c): preparation seconds at each alpha, averaged over the folds (the per-alpha profile of one dataset). */
  def alphaPreparationProfile: Vector[Double] =
    if foldPrepare.isEmpty then Vector.empty
    else Vector.tabulate(foldPrepare.head.length)(k => foldPrepare.map(_(k)).sum / foldPrepare.length)

final case class PhrfTuned(record: PhrfTuningRecord, timings: PhrfTimings)

/**
  * LOROCV tuning of the PHRF arm. The folds, the selection rule (largest mean score, ties to the smaller alpha), the
  * fold score ([[FoldScore.predictedR2]]) and the stimulus-wise amplitude predictor ([[AmplitudePrediction.byStimulus]])
  * are S4's, called, not copied: PHRF, PHRF-can and rLSS are scored identically.
  *
  * Per fold, and in this order, the training side reads only the training runs: its rows, events, baseline columns,
  * whitening segments and `sigma2` ([[CommonPrep.sigma2Training]], recomputed from the training runs of the whitened
  * arrays). The held-out run enters only the score, which compares the whitened held-out response (the shared
  * [[TrialNativeInputs]]) with the prediction `sum_i a_i x_i(theta_v)` built from the training fit.
  */
object PhrfTrialTuning:

  /** The ML trial engine (`trial-banded-ml`). */
  def mlEngine(basis: scalafim.fmri.design.hrf.HrfKernelBasis, config: PhrfTrialConfig, clock: PhrfClock): PhrfFoldEngine =
    new PhrfFoldEngine:
      def fit(problem: PhrfTrialProblem, alphaIndex: Int, alpha: Double): Either[PhrfTrialRefusal, (PhrfTrainFit, PhrfStageSeconds)] =
        val _ = alphaIndex
        PhrfTrialRunner.trainFit(problem, basis, alpha, config, clock)

  private def mat(rows: Int, cols: Int, data: Array[Double]): Either[PhrfTrialRefusal, Matrix] =
    Matrix.of(rows, cols, data).left.map(e => PhrfTrialRefusal.Inconsistent(e.message))

  /**
    * The prediction of the held-out whitened response from a training fit: held-out amplitudes by stimulus (the training
    * condition mean for an unseen stimulus), times the held-out WHITENED trial regressors at each voxel's own decoded shape.
    * The product is S4's shared [[FoldScore.predictedSignal]] (regressor matrix as a parameter), called once per voxel
    * with that voxel's regressors; all delivered voxels are pooled, whatever their decode status (the shared voxel rule).
    */
  private[run] def predictWhitened(fit: PhrfTrainFit, train: PhrfTrialProblem, held: PhrfHeldOutDesign, test: TrialNativeInputs): Either[PhrfTrialRefusal, Matrix] =
    val nt = test.trials
    val v = fit.voxels
    AmplitudePrediction
      .byStimulus(train.trialStim, train.trialCond, fit.amplitudes, test.trialStim, test.trialCond)
      .left.map(PhrfTrialRefusal.Cv(_))
      .flatMap { amp =>
        val rows = held.expanded.rows
        val signal = new Array[Double](rows * v)
        var failure: Option[PhrfTrialRefusal] = None
        var j = 0
        while failure.isEmpty && j < v do
          val one = if !fit.isUsable(j) then Right(zero(rows)) else for
            raw <- PhrfEdf.rawRegressors(held.expanded, fit.coordinates(j))
            w <- scalafim.fmri.ar.WhiteningTransform.matrix(held.plan, raw).left.map(e => PhrfTrialRefusal.Setup("prediction", PhrfTrialAssembly.fmt(e)))
            x <- mat(rows, nt, Array.tabulate(rows * nt)(k => w(k / nt, k % nt)))
            a <- mat(nt, 1, Array.tabulate(nt)(i => amp(i, j)))
            s <- FoldScore.predictedSignal(x, a).left.map(PhrfTrialRefusal.Cv(_))
          yield s
          one match
            case Left(e) => failure = Some(e)
            case Right(s) =>
              var r = 0
              while r < rows do
                signal(r * v + j) = s(r, 0)
                r += 1
          j += 1
        failure.toLeft(()).flatMap(_ => mat(rows, v, signal))
      }

  private def zero(rows: Int): Matrix = Matrix.of(rows, 1, new Array[Double](rows)).fold(e => throw new IllegalStateException(e.message), identity)

  private def columns(m: Matrix, keep: Vector[Int]): Either[PhrfTrialRefusal, Matrix] =
    mat(m.rows, keep.length, Array.tabulate(m.rows * keep.length)(k => m(k / keep.length, keep(k % keep.length))))

  /** Score of one prediction against the held-out whitened response on the voxels `keep` (S4's shared fold score). */
  private def scoreOn(predicted: Matrix, test: TrialNativeInputs, keep: Vector[Int], foldIndex: Int): Either[PhrfTrialRefusal, Double] =
    for
      y <- columns(test.y, keep)
      p <- columns(predicted, keep)
      score <- FoldScore.predictedR2(y, p, test.fixed).left.map {
        case LorocvRefusal.DegenerateVariance(_) => PhrfTrialRefusal.Cv(LorocvRefusal.DegenerateVariance(foldIndex))
        case other                               => PhrfTrialRefusal.Cv(other)
      }
    yield score.r2

  private final case class FoldResult(record: PhrfFoldRecord, prepare: Vector[Double], run: Vector[Double], sigma2Seconds: Double, scoreSeconds: Double)

  private def scoreFold(
      inputs: FitInputs,
      prep: CommonPrep,
      native: TrialNativeInputs,
      grid: AlphaGrid,
      config: PhrfTrialConfig,
      clock: PhrfClock,
      engine: PhrfFoldEngine,
      basis: scalafim.fmri.design.hrf.HrfKernelBasis,
      fold: Fold,
      foldIndex: Int,
      fitted: scala.collection.mutable.ArrayBuffer[Double]
  ): Either[PhrfTrialRefusal, FoldResult] =
    val t0 = clock.nanos()
    prep.sigma2Training(fold.trainRuns.toSet).left.map(PhrfTrialRefusal.Prep(_)).flatMap { s2 =>
      val sigmaSeconds = (clock.nanos() - t0) / 1e9
      val testTrials = Array.tabulate(native.trials)(identity).filter(i => native.trialRun(i) == fold.heldOut)
      for
        train <- PhrfTrialAssembly.problem(inputs, prep, fold.trainRuns, s2.sigma2, config)
        held <- PhrfTrialAssembly.heldOut(inputs, prep, fold.heldOut, basis)
        _ <- Either.cond(
          held.trialIndex.sameElements(testTrials) && held.rows.sameElements(fold.testRows),
          (),
          PhrfTrialRefusal.Inconsistent(s"fold $foldIndex: held-out design and held-out rows disagree")
        )
        test <- native.restrict(fold.testRows, testTrials).left.map(e => PhrfTrialRefusal.Inconsistent(e.code))
        rows <- scoreGrid(train, held, test, grid, engine, clock, foldIndex, fitted)
      yield FoldResult(PhrfFoldRecord(fold.heldOut, s2.sigma2, rows.map(_._1), rows.map(_._2), rows.map(_._3), rows.map(_._6).head), rows.map(_._4.prepare), rows.map(_._4.run), sigmaSeconds, rows.map(_._5).sum)
    }

  /**
    * All grid alphas of one fold. POOLING RULE (decision point, receipt): the fold score pools all delivered scored voxels
    * (S4's rule) EXCEPT a voxel that has no finite readout at some alpha of this fold, which is excluded from this fold's
    * pool at every alpha, so that every alpha of the fold is scored on the same voxel set. Such a voxel is Failed (design
    * 2.3), the rest proceed; a fold with no usable voxel is a typed refusal.
    */
  private def scoreGrid(
      train: PhrfTrialProblem,
      held: PhrfHeldOutDesign,
      test: TrialNativeInputs,
      grid: AlphaGrid,
      engine: PhrfFoldEngine,
      clock: PhrfClock,
      foldIndex: Int,
      fitted: scala.collection.mutable.ArrayBuffer[Double]
  ): Either[PhrfTrialRefusal, Vector[(String, Double, PhrfWorkReceipt, PhrfStageSeconds, Double, Vector[Int])]] =
    val fits = Vector.newBuilder[(PhrfTrainFit, PhrfStageSeconds, Matrix, Double)]
    var failure: Option[PhrfTrialRefusal] = None
    var k = 0
    while failure.isEmpty && k < grid.size do
      fitted += grid.alpha(k)
      engine.fit(train, k, grid.alpha(k)) match
        case Left(e) => failure = Some(e)
        case Right((fit, secs)) =>
          val t0 = clock.nanos()
          predictWhitened(fit, train, held, test) match
            case Left(e)  => failure = Some(e)
            case Right(p) => fits += ((fit, secs, p, (clock.nanos() - t0) / 1e9))
      k += 1
    failure match
      case Some(e) => Left(e)
      case None =>
        val all = fits.result()
        val v = all.head._1.voxels
        val excluded = Vector.tabulate(v)(identity).filter(j => all.exists(!_._1.isUsable(j)))
        val keep = Vector.tabulate(v)(identity).filterNot(excluded.contains)
        if keep.isEmpty then Left(PhrfTrialRefusal.Cv(LorocvRefusal.DegenerateVariance(foldIndex)))
        else
          val out = Vector.newBuilder[(String, Double, PhrfWorkReceipt, PhrfStageSeconds, Double, Vector[Int])]
          var bad: Option[PhrfTrialRefusal] = None
          all.foreach { (fit, secs, pred, predSecs) =>
            if bad.isEmpty then
              val t0 = clock.nanos()
              scoreOn(pred, test, keep, foldIndex) match
                case Left(e)  => bad = Some(e)
                case Right(s) => out += ((fit.sha256, s, fit.receipt, secs, predSecs + (clock.nanos() - t0) / 1e9, excluded))
          }
          bad.toLeft(out.result())

  /**
    * Tune `engine` over `grid` by leave-one-run-out. A refusal anywhere stops the tuning and is returned typed; the
    * grid is typed positive, so no alpha 0 is ever handed to the engine.
    */
  def tune(
      inputs: FitInputs,
      prep: CommonPrep,
      native: TrialNativeInputs,
      grid: AlphaGrid,
      config: PhrfTrialConfig,
      clock: PhrfClock,
      engine: PhrfFoldEngine
  ): Either[PhrfTrialRefusal, PhrfTuned] =
    for
      _ <- PhrfTrialRunner.checked(inputs, prep)
      basis <- PhrfTrialRunner.basisOf(config)
      folds <- Lorocv.folds(prep.runId).left.map(PhrfTrialRefusal.Cv(_))
      results = scala.collection.mutable.ArrayBuffer.empty[FoldResult]
      fitted = scala.collection.mutable.ArrayBuffer.empty[Double]
      failure = scala.collection.mutable.ArrayBuffer.empty[PhrfTrialRefusal]
      selection <- Lorocv
        .run(folds, grid) { fold =>
          val index = folds.indexOf(fold)
          scoreFold(inputs, prep, native, grid, config, clock, engine, basis, fold, index, fitted) match
            case Left(e) =>
              failure += e
              Left(LorocvRefusal.Evaluation(index, e.code, e.message))
            case Right(r) =>
              results += r
              Right(r.record.scores)
        }
        .left.map(e => failure.headOption.getOrElse(PhrfTrialRefusal.Cv(e)))
    yield
      val rs = results.toVector
      PhrfTuned(
        PhrfTuningRecord(selection, rs.map(_.record), fitted.toVector),
        PhrfTimings(rs.map(_.prepare), rs.map(_.run), rs.map(_.sigma2Seconds), rs.map(_.scoreSeconds), 0.0, 0.0, 0.0, inputs.y.rows)
      )
