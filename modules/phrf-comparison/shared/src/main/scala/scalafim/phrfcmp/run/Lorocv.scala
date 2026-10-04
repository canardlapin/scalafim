package scalafim.phrfcmp.run

import gale.linalg.{DMat, QROptions, QRPivoting}

import scalafim.phrfcmp.ingest.Matrix

/** Typed reason the LOROCV shell or the fold score refused; no exception crosses this boundary. */
enum LorocvRefusal:
  case TooFewRuns(runs: Int)
  case Inconsistent(detail: String)
  case NonFiniteScore(fold: Int, alphaIndex: Int)
  case DegenerateVariance(fold: Int)
  case NoTrainingCondition(condition: Int)
  case Evaluation(fold: Int, token: String, detail: String)

  def message: String = this match
    case TooFewRuns(n)           => s"leave-one-run-out needs at least two runs, got $n"
    case Inconsistent(d)         => s"LOROCV inputs disagree: $d"
    case NonFiniteScore(f, k)    => s"fold $f alpha index $k produced a non-finite score"
    case DegenerateVariance(f)   => s"fold $f has no held-out variance after removing the nuisance"
    case NoTrainingCondition(c)  => s"condition $c has no training trial, so a held-out trial of it cannot be predicted"
    case Evaluation(f, _, d)     => s"fold $f evaluation failed: $d"

  /** Short status token (letters, digits, `_`), safe for ledger codes. */
  def code: String = this match
    case TooFewRuns(_)           => "lorocv_too_few_runs"
    case Inconsistent(_)         => "lorocv_inconsistent"
    case NonFiniteScore(_, _)    => "lorocv_nonfinite_score"
    case DegenerateVariance(_)   => "lorocv_degenerate_variance"
    case NoTrainingCondition(_)  => "lorocv_no_training_condition"
    case Evaluation(_, t, _)     => t

/**
  * One leave-one-run-out fold: `heldOut` is the single run left out, `trainRuns` every other run. Row sets are over the
  * time axis (ascending); training and test rows partition it.
  */
final case class Fold(heldOut: Int, trainRuns: Vector[Int], trainRows: Array[Int], testRows: Array[Int])

/** The outcome of tuning over a grid. `foldScores(f)(k)` is the held-out R^2 of fold `f` at grid point `k`. */
final case class LorocvSelection(
    grid: AlphaGrid,
    foldScores: Vector[Vector[Double]],
    meanScores: Vector[Double],
    selected: Int
):
  def selectedAlpha: Double = grid.alpha(selected)
  def atLowerEnd: Boolean = selected == 0
  def atUpperEnd: Boolean = selected == grid.size - 1

/** The held-out score of one fold: pooled over voxels, in whitened space, after the held-out nuisance is removed. */
final case class FoldScore(r2: Double, rss: Double, tss: Double)

/**
  * Leave-one-run-out cross-validation shell (design 2.2, O2 default frozen in v0). The shell owns the folds, the
  * selection rule and the shared fold score; an arm supplies only "the nine fold scores of this fold".
  *
  *  - Folds: one per run, each leaving out exactly that run.
  *  - Score of a fold at a grid point: [[FoldScore.predictedR2]].
  *  - Selection: the grid point with the largest mean fold score; on an exact tie the smaller alpha wins (a larger score
  *    must be strictly larger to displace an earlier, smaller-alpha point).
  *  - The scorer sees held-out data only through the callback it is given a [[Fold]] for; nothing here reads data.
  */
object Lorocv:

  /** Folds over the distinct run ids of `runId` (ascending), each leaving one run out. */
  def folds(runId: Array[Int]): Either[LorocvRefusal, Vector[Fold]] =
    val runs = runId.distinct.sorted.toVector
    if runs.length < 2 then Left(LorocvRefusal.TooFewRuns(runs.length))
    else
      Right(runs.map { held =>
        val train = Vector.newBuilder[Int]
        val test = Vector.newBuilder[Int]
        var t = 0
        while t < runId.length do
          if runId(t) == held then test += t else train += t
          t += 1
        Fold(held, runs.filterNot(_ == held), train.result().toArray, test.result().toArray)
      })

  /** The selection rule on a fold-by-grid score table. */
  def select(grid: AlphaGrid, foldScores: Vector[Vector[Double]]): Either[LorocvRefusal, LorocvSelection] =
    if foldScores.isEmpty then Left(LorocvRefusal.Inconsistent("no folds"))
    else if foldScores.exists(_.length != grid.size) then Left(LorocvRefusal.Inconsistent("a fold has the wrong number of scores"))
    else
      val bad = (for f <- foldScores.indices; k <- 0 until grid.size if !foldScores(f)(k).isFinite yield (f, k)).headOption
      bad match
        case Some((f, k)) => Left(LorocvRefusal.NonFiniteScore(f, k))
        case None =>
          val means = Vector.tabulate(grid.size) { k =>
            var s = 0.0
            var f = 0
            while f < foldScores.length do
              s += foldScores(f)(k)
              f += 1
            s / foldScores.length
          }
          var best = 0
          var k = 1
          while k < grid.size do
            if means(k) > means(best) then best = k
            k += 1
          Right(LorocvSelection(grid, foldScores, means, best))

  /** Run `scoreFold` on every fold (in order) and select. Stops at the first refusal. */
  def run(
      folds: Vector[Fold],
      grid: AlphaGrid
  )(scoreFold: Fold => Either[LorocvRefusal, Vector[Double]]): Either[LorocvRefusal, LorocvSelection] =
    val rows = Vector.newBuilder[Vector[Double]]
    var failure: Option[LorocvRefusal] = None
    var f = 0
    while failure.isEmpty && f < folds.length do
      scoreFold(folds(f)) match
        case Left(e)  => failure = Some(e)
        case Right(s) => rows += s
      f += 1
    failure match
      case Some(e) => Left(e)
      case None    => select(grid, rows.result())

object FoldScore:

  /**
    * The predicted held-out signal `regressors * amplitudes`: `regressors` is held-out rows x held-out trials (the arm's
    * whitened single-trial regressors restricted to the held-out run's rows and trials, in the same trial order as
    * `amplitudes`), `amplitudes` is held-out trials x voxels (from [[AmplitudePrediction.byStimulus]]). Shared by every arm
    * (rLSS, PHRF, PHRF-can) so that row, trial-order and nuisance conventions are identical; the result goes to
    * [[predictedR2]] with the held-out run's whitened nuisance.
    *
    * VOXEL POOLING RULE: scores pool over ALL delivered scored voxels of the held-out block, regardless of any per-voxel
    * decode status (a refused PHRF voxel still contributes its delivered prediction); there is no voxel selection.
    */
  def predictedSignal(regressors: Matrix, amplitudes: Matrix): Either[LorocvRefusal, Matrix] =
    if regressors.cols != amplitudes.rows then
      Left(LorocvRefusal.Inconsistent(s"${regressors.cols} held-out regressors but ${amplitudes.rows} predicted amplitudes"))
    else
      val rows = regressors.rows
      val n = regressors.cols
      val v = amplitudes.cols
      val out = new Array[Double](rows * v)
      var r = 0
      while r < rows do
        var i = 0
        while i < n do
          val x = regressors(r, i)
          if x != 0.0 then
            var c = 0
            while c < v do
              out(r * v + c) += x * amplitudes(i, c)
              c += 1
          i += 1
        r += 1
      Matrix.of(rows, v, out).left.map(e => LorocvRefusal.Inconsistent(e.message))

  /**
    * Held-out predicted R^2: `1 - RSS / TSS` with `RSS = |P (y - yhat)|^2`, `TSS = |P y|^2`, summed over rows and voxels
    * (pooled), where `P` removes the held-out run's own nuisance (its columns are never estimated from training data, so
    * the held-out baseline is fitted in-sample, identically for every arm and grid point). `y` and `yhat` are rows x V and
    * `nuisance` is rows x F, all whitened; F may be 0.
    */
  def predictedR2(y: Matrix, predicted: Matrix, nuisance: Matrix): Either[LorocvRefusal, FoldScore] =
    if y.rows != predicted.rows || y.cols != predicted.cols || nuisance.rows != y.rows then
      Left(LorocvRefusal.Inconsistent(s"y ${y.rows}x${y.cols}, prediction ${predicted.rows}x${predicted.cols}, nuisance ${nuisance.rows}x${nuisance.cols}"))
    else if y.rows == 0 || y.cols == 0 then Left(LorocvRefusal.Inconsistent("empty held-out block"))
    else
      val yd = DMat.tabulate(y.rows, y.cols)((r, c) => y(r, c))
      val pd = DMat.tabulate(y.rows, y.cols)((r, c) => predicted(r, c))
      val projected: Either[LorocvRefusal, (DMat, DMat)] =
        if nuisance.cols == 0 then Right((yd, pd))
        else
          val f = DMat.tabulate(nuisance.rows, nuisance.cols)((r, c) => nuisance(r, c))
          val qr = f.qr(QROptions(QRPivoting.Column, Some(1e-10)))
          for
            py <- qr.residualize(yd).left.map(e => LorocvRefusal.Inconsistent(e.toString))
            pp <- qr.residualize(pd).left.map(e => LorocvRefusal.Inconsistent(e.toString))
          yield (py, pp)
      projected.flatMap { (py, pp) =>
        var rss = 0.0
        var tss = 0.0
        var raw = 0.0
        var r = 0
        while r < py.rows do
          var c = 0
          while c < py.cols do
            val a = py(r, c)
            val d = a - pp(r, c)
            tss += a * a
            rss += d * d
            raw += y(r, c) * y(r, c)
            c += 1
          r += 1
        // nothing left after the nuisance: round-off alone must not be scored
        if !(tss > 1e-12 * raw) || !tss.isFinite || !rss.isFinite then Left(LorocvRefusal.DegenerateVariance(-1))
        else Right(FoldScore(1.0 - rss / tss, rss, tss))
      }

/**
  * The held-out amplitude predictor of the shared fold score (design 2.2): each held-out trial is predicted by the mean
  * training amplitude of the SAME stimulus (stimulus-persistent deviations repeat across runs), and by the training
  * condition mean for a stimulus unseen in training. Sums are accumulated in training-trial order (deterministic).
  */
object AmplitudePrediction:

  /**
    * @param trainAmp rows = training trials, cols = voxels (the arm's trial amplitudes from the training-fold fit)
    * @return rows = test trials, same voxels
    */
  def byStimulus(
      trainStim: Array[Int],
      trainCond: Array[Int],
      trainAmp: Matrix,
      testStim: Array[Int],
      testCond: Array[Int]
  ): Either[LorocvRefusal, Matrix] =
    val n = trainStim.length
    val v = trainAmp.cols
    if trainCond.length != n || trainAmp.rows != n || testStim.length != testCond.length then
      Left(LorocvRefusal.Inconsistent("training or test per-trial arrays differ in length"))
    else
      val slotOfStim = scala.collection.mutable.HashMap.empty[Int, Int]
      val stimSum = scala.collection.mutable.ArrayBuffer.empty[Array[Double]]
      val stimCount = scala.collection.mutable.ArrayBuffer.empty[Int]
      val stimCond = scala.collection.mutable.ArrayBuffer.empty[Int]
      val maxCond = math.max(if trainCond.isEmpty then -1 else trainCond.max, if testCond.isEmpty then -1 else testCond.max)
      if (trainCond ++ testCond).exists(_ < 0) then Left(LorocvRefusal.Inconsistent("negative condition id"))
      else
        val condSum = Array.fill(maxCond + 1)(new Array[Double](v))
        val condCount = new Array[Int](maxCond + 1)
        var i = 0
        while i < n do
          val slot = slotOfStim.getOrElseUpdate(
            trainStim(i), {
              stimSum += new Array[Double](v)
              stimCount += 0
              stimCond += trainCond(i)
              stimSum.length - 1
            }
          )
          val s = stimSum(slot)
          val c = condSum(trainCond(i))
          var j = 0
          while j < v do
            val a = trainAmp(i, j)
            s(j) += a
            c(j) += a
            j += 1
          stimCount(slot) = stimCount(slot) + 1
          condCount(trainCond(i)) += 1
          i += 1
        val out = new Array[Double](testStim.length * v)
        var failure: Option[LorocvRefusal] = None
        var t = 0
        while failure.isEmpty && t < testStim.length do
          slotOfStim.get(testStim(t)) match
            case Some(slot) =>
              if stimCond(slot) != testCond(t) then
                failure = Some(LorocvRefusal.Inconsistent(s"stimulus ${testStim(t)} appears in two conditions"))
              else
                var j = 0
                while j < v do
                  out(t * v + j) = stimSum(slot)(j) / stimCount(slot)
                  j += 1
            case None =>
              val c = testCond(t)
              if condCount(c) == 0 then failure = Some(LorocvRefusal.NoTrainingCondition(c))
              else
                var j = 0
                while j < v do
                  out(t * v + j) = condSum(c)(j) / condCount(c)
                  j += 1
          t += 1
        failure match
          case Some(e) => Left(e)
          case None    => Matrix.of(testStim.length, v, out).left.map(e => LorocvRefusal.Inconsistent(e.message))
