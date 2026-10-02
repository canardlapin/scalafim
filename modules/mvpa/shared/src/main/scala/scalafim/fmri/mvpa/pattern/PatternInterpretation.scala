package scalafim.fmri.mvpa.pattern

import gale.linalg.DMat
import multivar.core.SemanticSpace
import resample4s.core.Injection
import scalafim.fmri.mvpa.{AxisDigest, AxisRef, Column, EvidenceIdentity, ReindexingIdentity, ReindexingLeg}

enum PatternInterpretationError:
  case Prediction(error: PatternPredictionError)
  case ScopeMismatch(field: String)
  case AssessmentReuse(role: String, ordinals: Vector[Int])
  case InsufficientRows(rows: Int)
  case Budget(requiredCells: BigInt, allowedCells: Long)
  case NonFinite(field: String)

enum InterpretationScore:
  case RawComponents, CalibratedComponents, PosteriorComponents, PosteriorTargets

/** Forward loadings describe task contributions. Filters describe unshrunk
  * calibrated scores; neither is an empirical diagnostic or a significance map.
  */
final class CalibratedPatternInterpretation[N, Q, R] private[pattern] (
    val prediction: PatternPrediction[N, Q, R], val neuralByComponent: DMat,
    val relativePivotTolerance: Double
):
  val neuralAxis: AxisRef[N] = prediction.factors.neuralAxis
  val componentAxis: AxisRef[R] = prediction.factors.componentAxis
  val forwardLoadings: DMat = prediction.factors.neuralByComponent
  val score: InterpretationScore = InterpretationScore.CalibratedComponents
  def scores(values: AxisValues[N]): Either[PatternPredictionError, CalibratedComponentScores[R]] =
    if values.axis.descriptor != neuralAxis.descriptor then Left(PatternPredictionError.AxisMismatch("calibrated interpretation neural input"))
    else
      val centered = prediction.effectiveCentering match
        case CenteringPolicy.CenteredBeforeFit(_, _) => values.values
        case CenteringPolicy.ExplicitIntercept(offset, _) => values.values.zip(offset.values).map((x, mean) => x - mean)
      if centered.exists(!_.isFinite) then Left(PatternPredictionError.Invalid("calibrated interpretation input is nonfinite"))
      else
        val z = neuralByComponent.t * DMat.dense(neuralAxis.size, 1, centered)
        AxisValues(componentAxis, Vector.tabulate(componentAxis.size)(i => z(i, 0)))
          .left.map(PatternPredictionError.Artifact.apply).map(CalibratedComponentScores.apply)

final case class HaufeDiagnosticScope(
    training: ReindexingIdentity, assessment: ReindexingIdentity,
    selection: Vector[ReindexingIdentity], tuning: Vector[ReindexingIdentity]
)

/** Cov(x, score) Cov(score)^-1 on independent declared diagnostic rows.
  * No sparse projection, thresholding, p-value, or model identity is imposed.
  * Scope declarations cannot authenticate unrecorded analyst data access.
  */
final class EmpiricalHaufeDiagnostic[N, S] private[pattern] (
    val neuralAxis: AxisRef[N], val scoreAxis: AxisRef[S],
    val neuralByScore: DMat, val score: InterpretationScore,
    val scope: HaufeDiagnosticScope, val rows: Int, val covarianceDenominator: Int,
    val predictionIdentity: String, val identity: String,
    val relativePivotTolerance: Double
)

object PatternInterpretation:
  def calibrated[N, Q, R](prediction: PatternPrediction[N, Q, R],
      policy: PatternPredictionPolicy = PatternPredictionPolicy.strict
  ): Either[PatternInterpretationError, CalibratedPatternInterpretation[N, Q, R]] =
    val p = prediction.factors.neuralAxis.size
    val r = prediction.factors.componentAxis.size
    val cells = BigInt(8) * p * r + BigInt(8) * r * r
    if cells > policy.maximumWorkspaceCells || BigInt(p) * r > Int.MaxValue then
      Left(PatternInterpretationError.Budget(cells, policy.maximumWorkspaceCells))
    else
      for
        factor <- PatternPrediction.factor(prediction.gram, "calibrated interpretation Gram", policy)
          .left.map(PatternInterpretationError.Prediction.apply)
        transpose <- factor.solve(prediction.rawFilters.neuralByComponent.t)
          .left.map(error => PatternInterpretationError.Prediction(PatternPredictionError.Factorization("calibrated filters", error.toString)))
        filters = transpose.t
        _ <- if ResidualCovariance.finite(filters) then Right(()) else Left(PatternInterpretationError.NonFinite("calibrated filters"))
      yield new CalibratedPatternInterpretation(prediction, filters, policy.relativePivotTolerance)

  def empiricalCalibrated[P <: SemanticSpace, K, N, Q, R](prediction: PatternPrediction[N, Q, R],
      training: ReindexingLeg[P, K, Injection], assessment: ReindexingLeg[P, K, Injection],
      selection: Vector[ReindexingLeg[P, K, Injection]], tuning: Vector[ReindexingLeg[P, K, Injection]]
  )(observations: Column[assessment.Child, AxisValues[N]], policy: PatternPredictionPolicy = PatternPredictionPolicy.strict
  ): Either[PatternInterpretationError, EmpiricalHaufeDiagnostic[N, R]] =
    empirical(prediction, prediction.factors.componentAxis, InterpretationScore.CalibratedComponents,
      training, assessment, selection, tuning)(observations, policy, x => prediction.calibratedScores(x).map(_.values))

  def empiricalRaw[P <: SemanticSpace, K, N, Q, R](prediction: PatternPrediction[N, Q, R],
      training: ReindexingLeg[P, K, Injection], assessment: ReindexingLeg[P, K, Injection],
      selection: Vector[ReindexingLeg[P, K, Injection]], tuning: Vector[ReindexingLeg[P, K, Injection]]
  )(observations: Column[assessment.Child, AxisValues[N]], policy: PatternPredictionPolicy = PatternPredictionPolicy.strict
  ): Either[PatternInterpretationError, EmpiricalHaufeDiagnostic[N, R]] =
    empirical(prediction, prediction.factors.componentAxis, InterpretationScore.RawComponents,
      training, assessment, selection, tuning)(observations, policy, x => prediction.rawScores(x).map(_.values))

  def empiricalPosterior[P <: SemanticSpace, K, N, Q, R](prediction: PatternPrediction[N, Q, R],
      training: ReindexingLeg[P, K, Injection], assessment: ReindexingLeg[P, K, Injection],
      selection: Vector[ReindexingLeg[P, K, Injection]], tuning: Vector[ReindexingLeg[P, K, Injection]]
  )(observations: Column[assessment.Child, AxisValues[N]], policy: PatternPredictionPolicy = PatternPredictionPolicy.strict
  ): Either[PatternInterpretationError, EmpiricalHaufeDiagnostic[N, R]] =
    empirical(prediction, prediction.factors.componentAxis, InterpretationScore.PosteriorComponents,
      training, assessment, selection, tuning)(observations, policy, x => prediction.posteriorScores(x).map(_.values))

  def empiricalTargets[P <: SemanticSpace, K, N, Q, R](prediction: PatternPrediction[N, Q, R],
      training: ReindexingLeg[P, K, Injection], assessment: ReindexingLeg[P, K, Injection],
      selection: Vector[ReindexingLeg[P, K, Injection]], tuning: Vector[ReindexingLeg[P, K, Injection]]
  )(observations: Column[assessment.Child, AxisValues[N]], policy: PatternPredictionPolicy = PatternPredictionPolicy.strict
  ): Either[PatternInterpretationError, EmpiricalHaufeDiagnostic[N, Q]] =
    empirical(prediction, prediction.factors.targetAxis, InterpretationScore.PosteriorTargets,
      training, assessment, selection, tuning)(observations, policy, x => prediction.decode(x).map(_.values))

  private def empirical[P <: SemanticSpace, K, N, Q, R, S](prediction: PatternPrediction[N, Q, R],
      scores: AxisRef[S], definition: InterpretationScore,
      training: ReindexingLeg[P, K, Injection], assessment: ReindexingLeg[P, K, Injection],
      selection: Vector[ReindexingLeg[P, K, Injection]], tuning: Vector[ReindexingLeg[P, K, Injection]]
  )(observations: Column[assessment.Child, AxisValues[N]], policy: PatternPredictionPolicy,
      score: AxisValues[N] => Either[PatternPredictionError, AxisValues[S]]
  ): Either[PatternInterpretationError, EmpiricalHaufeDiagnostic[N, S]] =
    val neural = prediction.factors.neuralAxis
    val n = assessment.child.size
    val p = neural.size
    val r = scores.size
    val cells = BigInt(16) * n * (p + BigInt(r)) + BigInt(16) * p * r + BigInt(16) * r * r
    val roles = Vector("training" -> Vector(training), "selection" -> selection, "tuning" -> tuning)
    if training.child.size == 0 || training.child.descriptor != prediction.artifact.trainingBinding.declaredSampleAxis then
      Left(PatternInterpretationError.ScopeMismatch("training binding"))
    else if observations.rowAxis != assessment.child.descriptor || roles.exists(_._2.exists(_.parentAxis != assessment.parentAxis)) then
      Left(PatternInterpretationError.ScopeMismatch("diagnostic parent or row domain"))
    else if n < 2 then Left(PatternInterpretationError.InsufficientRows(n))
    else if cells > policy.maximumWorkspaceCells || BigInt(n) * p > Int.MaxValue || BigInt(n) * r > Int.MaxValue || BigInt(r) * r > Int.MaxValue then
      Left(PatternInterpretationError.Budget(cells, policy.maximumWorkspaceCells))
    else if observations.values.exists(_.axis.descriptor != neural.descriptor) then
      Left(PatternInterpretationError.ScopeMismatch("neural coordinates"))
    else
      val held = assessment.ordinals.iterator.toSet
      val overlaps = roles.flatMap((role, scopes) => scopes.map(leg => role -> leg.ordinals.iterator.filter(held).toVector))
      overlaps.find(_._2.nonEmpty) match
        case Some((role, ordinals)) => Left(PatternInterpretationError.AssessmentReuse(role, ordinals))
        case None =>
          val outputs = Vector.newBuilder[AxisValues[S]]
          var failure: Option[PatternInterpretationError] = None
          var row = 0
          while row < n && failure.isEmpty do
            score(observations.values(row)) match
              case Left(error) => failure = Some(PatternInterpretationError.Prediction(error))
              case Right(value) => outputs += value
            row += 1
          failure.toLeft(()).flatMap: _ =>
            val z = outputs.result()
            val xMean = Vector.tabulate(p)(col => observations.values.map(_.values(col) / n).sum)
            val zMean = Vector.tabulate(r)(col => z.map(_.values(col) / n).sum)
            val centeredX = DMat.tabulate(n, p)((i, j) => observations.values(i).values(j) - xMean(j))
            val centeredZ = DMat.tabulate(n, r)((i, j) => z(i).values(j) - zMean(j))
            val cross = (centeredX.t * centeredZ) * (1.0 / (n - 1))
            val covariance = (centeredZ.t * centeredZ) * (1.0 / (n - 1))
            if !ResidualCovariance.finite(cross) || !ResidualCovariance.finite(covariance) then
              Left(PatternInterpretationError.NonFinite("empirical covariance"))
            else
              for
                factor <- PatternPrediction.factor(covariance, "empirical score covariance", policy)
                  .left.map(PatternInterpretationError.Prediction.apply)
                transpose <- factor.solve(cross.t).left.map(error => PatternInterpretationError.Prediction(
                  PatternPredictionError.Factorization("empirical Haufe solve", error.toString)))
                patterns = transpose.t
                _ <- if ResidualCovariance.finite(patterns) then Right(()) else Left(PatternInterpretationError.NonFinite("empirical Haufe pattern"))
              yield
                val identity = AxisDigest.sha256Hex: writer =>
                  writer.string("scalafim.empirical-haufe.v1")
                  writer.string(prediction.numericalIdentity)
                  writer.string(definition.toString)
                  writer.string(scores.descriptor.coordinateSignature.value)
                  writer.string(java.lang.Double.toHexString(policy.relativePivotTolerance))
                  def scope(leg: ReindexingLeg[P, K, Injection]): Unit =
                    writer.string(leg.parentAxis.coordinateSignature.value)
                    writer.string(leg.child.descriptor.coordinateSignature.value)
                    writer.intLE(leg.child.size)
                    leg.ordinals.iterator.foreach(writer.intLE)
                  scope(assessment)
                  roles.foreach: (role, legs) =>
                    writer.string(role)
                    writer.intLE(legs.size)
                    legs.foreach(scope)
                  EvidenceIdentity.writeValues(writer, observations.valueIdentity)
                  observations.values.foreach(_.values.foreach(value => writer.string(java.lang.Double.toHexString(value))))
                new EmpiricalHaufeDiagnostic(neural, scores, patterns, definition,
                  HaufeDiagnosticScope(training.identity, assessment.identity, selection.map(_.identity), tuning.map(_.identity)),
                  n, n - 1, prediction.numericalIdentity, identity, policy.relativePivotTolerance)
