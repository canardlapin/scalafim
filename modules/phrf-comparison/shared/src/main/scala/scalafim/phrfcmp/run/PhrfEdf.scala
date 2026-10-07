package scalafim.phrfcmp.run

import gale.linalg.{DMat, QROptions, QRPivoting}

import scalafim.fmri.ar.WhiteningTransform
import scalafim.fmri.design.hrf.ExpandedTrialDesign
import scalafim.fmri.fit.profile.TrialHeldOutPrediction
import scalafim.phrfcmp.ingest.FitInputs
import scalafim.phrfcmp.prep.CommonPrep

/** PHRF's true effective df at each grid alpha (`lambda = 1/alpha`) for one design; `trials` = N, `conditions` = G. */
final case class PhrfEdf(alphas: Vector[Double], lambdas: Vector[Double], edf: Vector[Double], trials: Int, conditions: Int)

/**
  * PHRF's effective degrees of freedom, on the definition of `object DfMapping` (S4, owner decision 2026-10-02):
  * `edf = tr(S X) = sum_i d ahat_i / d a_i`, the trace of the trial-amplitude smoother, condition means included and the
  * nuisance projected out. For PHRF at a fixed shape and `lambda = 1/alpha`,
  * `edf_P(lambda) = tr( W^{-1} X~'X~ )`, `W = X~'X~ + lambda (I - M)`, `X~` the whitened trial regressors at the shape with the
  * whitened nuisance projected out and `M` the within-condition averaging projector. It is `N` at `lambda = 0` and tends to
  * `G` as `lambda` grows. EVERY edf here is the edf AT THE CANONICAL SHAPE (the matched quantity); PHRF decodes a shape per voxel, so
  * the per-voxel decoded-shape edf ([[atShapes]]) is a separate sealed diagnostic. Computed from the design alone (no data), on the same whitened design `TrialBandedPreparation` is
  * built from (same expanded lowering, same whitening plan, same baseline).
  */
object PhrfEdf:

  private val RankTol = 1e-10

  /** `X~'X~` for the whitened regressors `x` (T x N) after projecting out `nuisance` (T x F, may have F = 0). */
  def projectedGram(x: DMat, nuisance: Option[DMat]): Either[String, DMat] =
    val xt = nuisance match
      case None => Right(x)
      case Some(f) =>
        f.qr(QROptions(QRPivoting.Column, Some(RankTol))).residualize(x).left.map(_.toString)
    xt.map(r => r.t * r)

  /** `tr( (G + lambda (I - M))^{-1} G )` for the projected Gram `g` and the trial condition ids. `lambda >= 0`. */
  def amplitudeEdf(g: DMat, conditionOfTrial: Array[Int], lambda: Double): Either[String, Double] =
    val n = g.rows
    if g.cols != n || conditionOfTrial.length != n then Left("Gram and condition ids disagree")
    else if !(lambda >= 0.0 && lambda.isFinite) then Left("lambda must be finite and non-negative")
    else
      val counts = new Array[Int](conditionOfTrial.max + 1)
      conditionOfTrial.foreach(c => counts(c) += 1)
      val w = DMat.tabulate(n, n) { (i, j) =>
        val same = if conditionOfTrial(i) == conditionOfTrial(j) then 1.0 / counts(conditionOfTrial(i)) else 0.0
        g(i, j) + lambda * ((if i == j then 1.0 else 0.0) - same)
      }
      w.qr(QROptions(QRPivoting.Column, Some(1e-14))).solveLeastSquares(g) match
        case Left(e) => Left(e.toString)
        case Right(z) =>
          var t = 0.0
          var i = 0
          while i < n do
            t += z(i, i)
            i += 1
          Right(t)

  /** The unwhitened regressors `x_i(theta)` of an expanded design (T x N): the lowering contracted with `c(theta)`. */
  private[run] def rawRegressors(expanded: ExpandedTrialDesign, coordinates: Vector[Double]): Either[PhrfTrialRefusal, DMat] =
    TrialHeldOutPrediction.basisCoefficients(expanded, coordinates).left.map(e => PhrfTrialRefusal.Setup("edf", PhrfTrialAssembly.fmt(e))).map { c =>
      val n = expanded.trials
      val m = expanded.rank
      val cols = expanded.columns
      val data = expanded.term.data.data
      DMat.tabulate(expanded.rows, n) { (r, i) =>
        var acc = 0.0
        var j = 0
        while j < m do
          acc += c(j) * data(r * cols + j * n + i)
          j += 1
        acc
      }
    }

  /** The whitened regressors `x_i(theta)` of `problem` (T x N), whitened with the problem's plan. */
  private[run] def regressors(problem: PhrfTrialProblem, expanded: ExpandedTrialDesign, coordinates: Vector[Double]): Either[PhrfTrialRefusal, DMat] =
    rawRegressors(expanded, coordinates).flatMap { raw =>
      WhiteningTransform.matrix(problem.spec.plan, raw).left.map(e => PhrfTrialRefusal.Setup("edf", PhrfTrialAssembly.fmt(e)))
    }

  private def refuse(m: String) = PhrfTrialRefusal.Setup("edf", m.take(30).filter(_.isLetterOrDigit))

  private def edfsFor(g: DMat, problem: PhrfTrialProblem, grid: AlphaGrid): Either[PhrfTrialRefusal, Vector[Double]] =
    val out = Vector.newBuilder[Double]
    var failure: Option[PhrfTrialRefusal] = None
    var k = 0
    while failure.isEmpty && k < grid.size do
      amplitudeEdf(g, problem.trialCond, grid.lambda(k)) match
        case Left(m)  => failure = Some(refuse(m))
        case Right(v) => out += v
      k += 1
    failure.toLeft(out.result())

  /** The whitened nuisance of `problem` (None when it has no columns). */
  private def nuisanceOf(problem: PhrfTrialProblem): Either[PhrfTrialRefusal, Option[DMat]] =
    WhiteningTransform
      .matrix(problem.spec.plan, PhrfTrialAssembly.baselineMat(problem))
      .left.map(e => PhrfTrialRefusal.Setup("edf", PhrfTrialAssembly.fmt(e)))
      .map(n => if n.cols == 0 then None else Some(n))

  /** `X~'X~` of `problem` at the shape `coordinates`. */
  def gram(problem: PhrfTrialProblem, basis: scalafim.fmri.design.hrf.HrfKernelBasis, coordinates: Vector[Double]): Either[PhrfTrialRefusal, DMat] =
    for
      expanded <- PhrfTrialAssembly.expandedOf(problem, basis)
      x <- regressors(problem, expanded, coordinates)
      nuis <- nuisanceOf(problem)
      g <- projectedGram(x, nuis).left.map(refuse)
    yield g

  /** PHRF edf at every grid alpha for `problem` at `shape`: EDF AT THE CANONICAL SHAPE when `shape` is the canonical one. */
  def forProblem(problem: PhrfTrialProblem, basis: scalafim.fmri.design.hrf.HrfKernelBasis, shape: PhrfCanonicalShape, grid: AlphaGrid): Either[PhrfTrialRefusal, PhrfEdf] =
    for
      g <- gram(problem, basis, shape.coordinates)
      values <- edfsFor(g, problem, grid)
    yield PhrfEdf(grid.alphas, grid.lambdas, values, problem.trials, problem.conditions)

  /**
    * DIAGNOSTIC ONLY (sealed, never a decision input): PHRF's edf at each voxel's own DECODED shape, per grid alpha. The
    * matching of rLSS to PHRF is at the canonical shape; PHRF decodes a shape per voxel, so this shows how far each voxel's
    * true edf is from the matched one. `None` for a voxel without coordinates in the chart.
    */
  def atShapes(problem: PhrfTrialProblem, basis: scalafim.fmri.design.hrf.HrfKernelBasis, shapes: Vector[Option[Vector[Double]]], grid: AlphaGrid): Either[PhrfTrialRefusal, Vector[Option[Vector[Double]]]] =
    PhrfTrialAssembly.expandedOf(problem, basis).flatMap { expanded =>
      nuisanceOf(problem).flatMap { nuis =>
        val out = Vector.newBuilder[Option[Vector[Double]]]
        var failure: Option[PhrfTrialRefusal] = None
        var j = 0
        while failure.isEmpty && j < shapes.length do
          shapes(j) match
            case None => out += None
            case Some(c) =>
              val r = for
                x <- regressors(problem, expanded, c)
                g <- projectedGram(x, nuis).left.map(refuse)
                e <- edfsFor(g, problem, grid)
              yield e
              r match
                case Left(e)  => failure = Some(e)
                case Right(v) => out += Some(v)
          j += 1
        failure.toLeft(out.result())
      }
    }

  /** The edf targets of the design made of `runs` (a training fold, or all runs for the final fit). */
  def forRuns(inputs: FitInputs, prep: CommonPrep, runs: Vector[Int], grid: AlphaGrid, config: PhrfTrialConfig): Either[PhrfTrialRefusal, PhrfEdf] =
    for
      _ <- PhrfTrialRunner.checked(inputs, prep)
      basis <- PhrfTrialRunner.basisOf(config)
      shape <- PhrfCanonicalShape.derive(basis.family).left.map(m => PhrfTrialRefusal.Setup("canonical_shape", m.take(40).filter(_.isLetterOrDigit)))
      problem <- PhrfTrialAssembly.problem(inputs, prep, runs, prep.sigma2All.sigma2, config)
      edf <- forProblem(problem, basis, shape, grid)
    yield edf
