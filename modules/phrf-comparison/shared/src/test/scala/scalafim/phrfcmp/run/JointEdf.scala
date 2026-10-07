package scalafim.phrfcmp.run

import gale.linalg.{DMat, QROptions, QRPivoting}

/**
  * A PHRF-style joint effective df computed densely from a design alone (test oracle standing in for S5's PHRF side):
  * `edf(lambda) = tr(W^{-1} X~'X~)`, `W = X~'X~ + lambda (I - M)`, `X~` the trial regressors with the nuisance projected out
  * and `M` the within-condition averaging projector. It is the same functional `tr(S X)` as the rLSS amplitude edf.
  */
object JointEdf:

  def at(in: TrialNativeInputs, lambdas: Vector[Double]): Vector[Double] =
    val x = DMat.tabulate(in.timepoints, in.trials)((r, c) => in.trialDesign(r, c))
    val xt =
      if in.fixed.cols == 0 then x
      else
        val f = DMat.tabulate(in.timepoints, in.fixed.cols)((r, c) => in.fixed(r, c))
        f.qr(QROptions(QRPivoting.Column, Some(1e-10))).residualize(x).fold(e => throw new AssertionError(e.toString), identity)
    val n = in.trials
    val gram = xt.t * xt
    lambdas.map { lambda =>
      val w = DMat.tabulate(n, n) { (i, j) =>
        val same = in.trialCond(i) == in.trialCond(j)
        val count = in.trialCond.count(_ == in.trialCond(i))
        gram(i, j) + lambda * ((if i == j then 1.0 else 0.0) - (if same then 1.0 / count else 0.0))
      }
      val sol = w.cholesky.fold(e => throw new AssertionError(e.toString), identity).solve(gram).fold(e => throw new AssertionError(e.toString), identity)
      (0 until n).map(i => sol(i, i)).sum
    }

  /** Targets for any set of runs: the joint edf of the design restricted to exactly those runs. */
  def targets(in: TrialNativeInputs, grid: AlphaGrid): EdfTargets =
    EdfTargets { runs =>
      val rows = Array.tabulate(in.timepoints)(identity).filter(t => runs.contains(in.runId(t)))
      val trials = Array.tabulate(in.trials)(identity).filter(i => runs.contains(in.trialRun(i)))
      in.restrict(rows, trials).left.map(_.message).map(d => at(d, grid.lambdas))
    }
