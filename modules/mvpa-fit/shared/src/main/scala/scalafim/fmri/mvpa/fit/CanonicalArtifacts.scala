package scalafim.fmri.mvpa.fit

import gale.linalg.{DMat, DVec}
import multivar.core.*
import multivar.family.canonical.*
import scalafim.dataset.RunId
import scalafim.fmri.fit.{RunIndex, TemporalPreparationReceipt, TrainingRunScope}
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest}
import scalafim.fmri.mvpa.analysis.*
import scalafim.fmri.mvpa.decomposition.DecompositionPurpose

final case class CanonicalTrainingReceipt(
    trainingRuns: Vector[RunId], temporalPreparation: Vector[(RunId, TemporalPreparationReceipt)],
    evidenceIdentity: String, momentIdentity: String, admission: NumericResourceAdmission
):
  require(trainingRuns.nonEmpty && trainingRuns.distinct.length == trainingRuns.length)
  require(temporalPreparation.map(_._1) == trainingRuns)
  require(evidenceIdentity.matches("[0-9a-f]{64}") && momentIdentity.matches("[0-9a-f]{64}"))

/** Feature coordinates remain nominally N; only the fitted component domain
  * is existential. These are fits, not averages of cross-validation folds.
  */
final case class CanonicalArtifact[N <: SemanticSpace, C <: SemanticSpace] private[fit] (
    neural: SpaceEvidence[N], neuralAxis: AxisDescriptor,
    fit: CanonicalEffectFit[N, C], receipt: CanonicalTrainingReceipt
):
  val purpose: DecompositionPurpose = DecompositionPurpose.Descriptive
  require(neuralAxis.size == neural.dimension)
final case class NonnegativeCanonicalArtifact[N <: SemanticSpace, C <: SemanticSpace] private[fit] (
    neural: SpaceEvidence[N], neuralAxis: AxisDescriptor,
    fit: ConstrainedCanonicalFit[N, C], receipt: CanonicalTrainingReceipt
):
  val purpose: DecompositionPurpose = DecompositionPurpose.Descriptive
  require(neuralAxis.size == neural.dimension)
final case class CanonicalSpectrumArtifact[N <: SemanticSpace, C <: SemanticSpace] private[fit] (
    neural: SpaceEvidence[N], neuralAxis: AxisDescriptor,
    fit: CanonicalSpectrumFit[N, C], receipt: CanonicalTrainingReceipt
):
  val purpose: DecompositionPurpose = DecompositionPurpose.Descriptive
  require(neuralAxis.size == neural.dimension)

final case class CanonicalAssessmentFold[N <: SemanticSpace](
    training: CanonicalArtifact[N, ? <: SemanticSpace], receipt: CanonicalFoldReceipt, heldOutRoot: Double
):
  require(heldOutRoot.isFinite && heldOutRoot >= 0.0)
final case class CanonicalAssessment[N <: SemanticSpace](
    folds: Vector[CanonicalAssessmentFold[N]], meanHeldOutRoot: Double, rootToCorrelation: Double
):
  require(folds.nonEmpty && meanHeldOutRoot.isFinite && meanHeldOutRoot >= 0.0)
  require(rootToCorrelation.isFinite && rootToCorrelation >= 0.0 && rootToCorrelation <= 1.0)
  val reduction: ReductionEvidence[RunId] = canonicalMeanEvidence(folds.map(_.receipt.heldOutRun))
final case class NonnegativeAssessmentFold[N <: SemanticSpace](
    training: NonnegativeCanonicalArtifact[N, ? <: SemanticSpace], receipt: CanonicalFoldReceipt, heldOutRoot: Double
):
  require(heldOutRoot.isFinite && heldOutRoot >= 0.0)
final case class NonnegativeCanonicalAssessment[N <: SemanticSpace](
    folds: Vector[NonnegativeAssessmentFold[N]], meanHeldOutRoot: Double, rootToCorrelation: Double
):
  require(folds.nonEmpty && meanHeldOutRoot.isFinite && meanHeldOutRoot >= 0.0)
  require(rootToCorrelation.isFinite && rootToCorrelation >= 0.0 && rootToCorrelation <= 1.0)
  val reduction: ReductionEvidence[RunId] = canonicalMeanEvidence(folds.map(_.receipt.heldOutRun))
final case class ManovaAssessmentFold[N <: SemanticSpace](
    training: CanonicalSpectrumArtifact[N, ? <: SemanticSpace], receipt: ManovaFoldReceipt,
    heldOutRoots: CanonicalRootSpectrum, heldOutStatistics: ManovaStatistics
)
final case class ManovaAssessment[N <: SemanticSpace](
    folds: Vector[ManovaAssessmentFold[N]], meanStatistics: ManovaStatistics
):
  require(folds.nonEmpty)
  val reduction: ReductionEvidence[RunId] = canonicalMeanEvidence(folds.map(_.receipt.heldOutRun))
final case class SignedAssessmentFold[N <: SemanticSpace](
    training: CanonicalArtifact[N, ? <: SemanticSpace], receipt: SignedCrossRunReceipt,
    numerator: Double, denominator: Double, statistic: SignedCrossRunRayleigh
):
  require(numerator.isFinite && denominator.isFinite && denominator > 0.0)
final case class SignedCanonicalAssessment[N <: SemanticSpace](
    folds: Vector[SignedAssessmentFold[N]], meanStatistic: SignedCrossRunRayleigh
):
  require(folds.nonEmpty)
  val reduction: ReductionEvidence[RunId] = canonicalMeanEvidence(folds.map(_.receipt.canonical.heldOutRun))

private[fit] def canonicalMeanEvidence(runs: Vector[RunId]): ReductionEvidence[RunId] =
  ReductionEvidence(runs.map(_ -> ReductionWeight(1.0)), Vector.empty, runs.length.toDouble, None, BigInt(runs.length))

/** Domain preparation supplies effect/residual moments; all generic solvers
  * and numerical certificates remain in pinned Multivar/Gale.
  */
object CanonicalGlobal:
  private val unknownBackend = ResourceBound.Unknown("pinned canonical solver live numeric workspace has no provider declaration")

  def fit[P <: SemanticSpace, N <: SemanticSpace](
      source: CanonicalRunSet[P, N, CanonicalGeometrySchedule], training: CanonicalTrainingRuns[P],
      regularization: ResidualRegularization, budget: ResourceBudget,
      backend: ResourceBound = unknownBackend
  ): Either[CanonicalArtifactError, CanonicalArtifact[N, ? <: SemanticSpace]] =
    for
      _ <- validateSelection(source, training)
      admission <- source.admit(budget, backend, contrastWorkspace(source))
      moments <- contrastMoments(source, training.indices, training.scope, budget)
      artifact <- fitContrast(source, training.indices, moments, regularization, admission)
    yield artifact

  def fitNonnegative[P <: SemanticSpace, N <: SemanticSpace](
      source: CanonicalRunSet[P, N, CanonicalGeometrySchedule], training: CanonicalTrainingRuns[P],
      model: NonnegativeCanonicalModelSpec, budget: ResourceBudget,
      backend: ResourceBound = unknownBackend
  ): Either[CanonicalArtifactError, NonnegativeCanonicalArtifact[N, ? <: SemanticSpace]] =
    for
      _ <- validateSelection(source, training)
      admission <- source.admit(budget, backend, contrastWorkspace(source))
      moments <- contrastMoments(source, training.indices, training.scope, budget)
      artifact <- fitConstrained(source, training.indices, moments, model, admission)
    yield artifact

  def assess[P <: SemanticSpace, N <: SemanticSpace](
      source: CanonicalRunSet[P, N, CanonicalGeometrySchedule], regularization: ResidualRegularization,
      budget: ResourceBudget, backend: ResourceBound = unknownBackend
  ): Either[CanonicalArtifactError, CanonicalAssessment[N]] =
    assessment(source, budget, backend): (heldOut, indices, trainingMoments, heldOutMoment, admission) =>
      for
        artifact <- fitContrast(source, indices, trainingMoments, regularization, admission)
        direction <- simpleDirection(source.runs(heldOut).runId, artifact.fit)
        held <- heldOutMoment()
        root <- score(source.runs(heldOut).runId, direction, held, artifact.fit.programFit.identifiability.context.tolerance)
      yield CanonicalAssessmentFold(artifact, foldReceipt(source, heldOut, indices, trainingMoments, held), root)
    .map: folds =>
      val mean = average(folds.map(_.heldOutRoot))
      CanonicalAssessment(folds, mean, math.sqrt(mean / (1.0 + mean)))

  def assessNonnegative[P <: SemanticSpace, N <: SemanticSpace](
      source: CanonicalRunSet[P, N, CanonicalGeometrySchedule], model: NonnegativeCanonicalModelSpec,
      budget: ResourceBudget, backend: ResourceBound = unknownBackend
  ): Either[CanonicalArtifactError, NonnegativeCanonicalAssessment[N]] =
    assessment(source, budget, backend): (heldOut, indices, trainingMoments, heldOutMoment, admission) =>
      for
        artifact <- fitConstrained(source, indices, trainingMoments, model, admission)
        held <- heldOutMoment()
        root <- score(source.runs(heldOut).runId, artifact.fit.direction, held, artifact.fit.programFit.identifiability.context.tolerance)
      yield NonnegativeAssessmentFold(artifact, foldReceipt(source, heldOut, indices, trainingMoments, held), root)
    .map: folds =>
      val mean = average(folds.map(_.heldOutRoot))
      NonnegativeCanonicalAssessment(folds, mean, math.sqrt(mean / (1.0 + mean)))

  def assessSigned[P <: SemanticSpace, N <: SemanticSpace](
      source: CanonicalRunSet[P, N, CanonicalGeometrySchedule], regularization: ResidualRegularization,
      budget: ResourceBudget, backend: ResourceBound = unknownBackend
  ): Either[CanonicalArtifactError, SignedCanonicalAssessment[N]] =
    assessment(source, budget, backend): (heldOut, indices, trainingMoments, heldOutMoment, admission) =>
      for
        artifact <- fitContrast(source, indices, trainingMoments, regularization, admission)
        direction <- simpleDirection(source.runs(heldOut).runId, artifact.fit)
        held <- heldOutMoment()
        result <- signedScore(source.runs(heldOut).runId, direction, trainingMoments, held, artifact.fit)
      yield SignedAssessmentFold(artifact,
        SignedCrossRunReceipt(foldReceipt(source, heldOut, indices, trainingMoments, held), SignedCrossRunEstimator.FrozenTrainingDirection,
          SignedCrossRunOrientation.AgreementSign, SignedCrossRunExchangeability.RunWiseContrastSignFlip), result._1, result._2, result._3)
    .map: folds =>
      SignedCanonicalAssessment(folds, SignedCrossRunRayleigh.unsafe(average(folds.map(_.statistic.value))))

  def fitManova[P <: SemanticSpace, N <: SemanticSpace](
      source: CanonicalRunSet[P, N, ManovaGeometrySchedule], training: CanonicalTrainingRuns[P],
      regularization: ResidualRegularization, budget: ResourceBudget, backend: ResourceBound = unknownBackend
  ): Either[CanonicalArtifactError, CanonicalSpectrumArtifact[N, ? <: SemanticSpace]] =
    for
      _ <- validateSelection(source, training)
      rank <- validateManova(source, training.indices, training.scope)
      admission <- source.admit(budget, backend, manovaWorkspace(source))
      moments <- manovaMoments(source, training.indices, training.scope, budget)
      fit <- fitSpectrum(source.neural, sumMatrices(moments.map(_.effect)), sumMatrices(moments.map(_.residual)), rank, regularization)
    yield CanonicalSpectrumArtifact(source.neural, source.neuralAxis, fit,
      trainingReceipt(source, training.indices, moments.map(_.temporalReceipt), sumMatrices(moments.map(_.effect)), sumMatrices(moments.map(_.residual)), admission))

  def assessManova[P <: SemanticSpace, N <: SemanticSpace](
      source: CanonicalRunSet[P, N, ManovaGeometrySchedule], regularization: ResidualRegularization,
      budget: ResourceBudget, backend: ResourceBound = unknownBackend
  ): Either[CanonicalArtifactError, ManovaAssessment[N]] =
    source.admit(budget, backend, manovaWorkspace(source)).flatMap: admission =>
      val indices = source.runs.indices.toVector
      collect(indices.map(held => source.scope(indices.filter(_ != held)))).flatMap: scopes =>
        collect(scopes.map(scope => validateManova(source, indices, scope))).flatMap: ranks =>
          if ranks.distinct.length != 1 then Left(CanonicalArtifactError.Input("MANOVA contrast rank changes across folds"))
          else traverse(indices): held =>
            val training = indices.filter(_ != held)
            manovaMoments(source, training, scopes(held), budget).flatMap: moments =>
              val effect = sumMatrices(moments.map(_.effect))
              val residual = sumMatrices(moments.map(_.residual))
              for
                fit <- fitSpectrum(source.neural, effect, residual, ranks(held), regularization)
                frame <- fit.denseFrame.left.map(CanonicalArtifactError.MultivarFailure.apply)
                heldMoment <- manovaMoments(source, Vector(held), scopes(held), budget).map(_.head)
                // Held-out fitting is restricted to the frozen training frame.
                // It does not re-estimate a direction in the original N domain.
                component <- SpaceRef.of(s"canonical-held-${momentIdentity(frame)}", SpaceRole.Latent, frame.cols).left.map(CanonicalArtifactError.MultivarFailure.apply)
                heldFit <- fitSpectrum(component.evidence, compress(frame, heldMoment.effect), compress(frame, heldMoment.residual), ranks(held), regularization)
              yield ManovaAssessmentFold(
                CanonicalSpectrumArtifact(source.neural, source.neuralAxis, fit, trainingReceipt(source, training, moments.map(_.temporalReceipt), effect, residual, admission)),
                ManovaFoldReceipt(training.map(source.runs(_).runId), source.runs(held).runId,
                  indices.map(i => source.runs(i).runId -> (training.zip(moments).toMap + (held -> heldMoment))(i).temporalReceipt), ManovaMomentExecution.RunwiseSufficientStatistics),
                heldFit.roots, heldFit.statistics
              )
          .map: folds =>
            ManovaAssessment(folds, ManovaStatistics(average(folds.map(_.heldOutStatistics.royLargestRoot)),
              average(folds.map(_.heldOutStatistics.wilksLambda)), average(folds.map(_.heldOutStatistics.pillaiTrace)),
              average(folds.map(_.heldOutStatistics.hotellingLawleyTrace))))

  private def validateManova[P <: SemanticSpace, N <: SemanticSpace](
      source: CanonicalRunSet[P, N, ManovaGeometrySchedule], indices: Vector[Int], scope: TrainingRunScope
  ): Either[CanonicalArtifactError, Int] =
    collect(indices.map: index =>
      val run = source.runs(index)
      run.geometry.resolve(run.runId, RunIndex.unsafe(index), scope).left.map(CanonicalArtifactError.Temporal.apply).flatMap: geometry =>
        if geometry.preparedDesign.timepoints != run.observations.rows then Left(CanonicalArtifactError.Input("MANOVA temporal design and observation time domains differ"))
        else if BigInt(source.neuralAxis.size) * geometry.preparedDesign.predictors > Int.MaxValue then Left(CanonicalArtifactError.Input("MANOVA response-design moment exceeds primitive array capacity"))
        else Right((geometry.contrastRank, geometry.receipt.contrastName))
    ).flatMap: hypotheses =>
      if hypotheses.distinct.length != 1 then Left(CanonicalArtifactError.Input("MANOVA runs must use the same named contrast subspace"))
      else if hypotheses.head._1 > source.neuralAxis.size then Left(CanonicalArtifactError.Input("MANOVA contrast rank exceeds the neural domain dimension"))
      else Right(hypotheses.head._1)

  private def manovaMoments[P <: SemanticSpace, N <: SemanticSpace](
      source: CanonicalRunSet[P, N, ManovaGeometrySchedule], indices: Vector[Int], scope: TrainingRunScope, budget: ResourceBudget
  ): Either[CanonicalArtifactError, Vector[ManovaRunMoments]] =
    validateManova(source, indices, scope).flatMap: _ =>
      traverse(indices): index =>
        val run = source.runs(index)
        for
          geometry <- run.geometry.resolve(run.runId, RunIndex.unsafe(index), scope).left.map(CanonicalArtifactError.Temporal.apply)
          response <- run.read(budget)
          prepared <- geometry.prepareResponse(response).left.map(error => CanonicalArtifactError.Temporal(OneShotMvpaError.TemporalPreparationFailure(run.runId, error)))
          moments <- ManovaMoments.accumulate(prepared, geometry, Array.tabulate(source.neuralAxis.size)(identity)).left.map(CanonicalArtifactError.Temporal.apply)
        yield moments

  private def fitSpectrum[N <: SemanticSpace](space: SpaceEvidence[N], effect: DMat, residual: DMat, rank: Int,
      regularization: ResidualRegularization): Either[CanonicalArtifactError, CanonicalSpectrumFit[N, ? <: SemanticSpace]] =
    for
      e <- covariance(space, effect, "manova-effect")
      r <- covariance(space, residual, "manova-temporal-residual")
      problem <- CanonicalEffectProblem.fromOperators(space, e, r, regularization).left.map(CanonicalArtifactError.MultivarFailure.apply)
      fit <- problem.fitSpectrum(rank).left.map(CanonicalArtifactError.MultivarFailure.apply)
    yield fit

  private def compress(frame: DMat, values: DMat): DMat =
    DMat.tabulate(frame.cols, frame.cols): (left, right) =>
      var total = 0.0
      var row = 0
      while row < values.rows do
        var column = 0
        while column < values.cols do
          total += frame(row, left) * values(row, column) * frame(column, right)
          column += 1
        row += 1
      total

  private def assessment[P <: SemanticSpace, N <: SemanticSpace, A](
      source: CanonicalRunSet[P, N, CanonicalGeometrySchedule], budget: ResourceBudget, backend: ResourceBound
  )(evaluate: (Int, Vector[Int], Vector[CanonicalRunMoments], () => Either[CanonicalArtifactError, CanonicalRunMoments], NumericResourceAdmission) => Either[CanonicalArtifactError, A]): Either[CanonicalArtifactError, Vector[A]] =
    source.admit(budget, backend, contrastWorkspace(source)).flatMap: admission =>
      // Resolve every fold schedule before the first acquisition or response read.
      val indices = source.runs.indices.toVector
      val scopes = collect(indices.map(held => source.scope(indices.filter(_ != held))))
      scopes.flatMap: trainingScopes =>
        collect(trainingScopes.map(scope => validateContrast(source, indices, scope))).flatMap: _ =>
          traverse(indices): held =>
            val training = indices.filter(_ != held)
            contrastMoments(source, training, trainingScopes(held), budget).flatMap: moments =>
              val readHeld = () => contrastMoments(source, Vector(held), trainingScopes(held), budget).map(_.head)
              evaluate(held, training, moments, readHeld, admission)


  private def validateContrast[P <: SemanticSpace, N <: SemanticSpace](
      source: CanonicalRunSet[P, N, CanonicalGeometrySchedule], indices: Vector[Int], scope: TrainingRunScope
  ): Either[CanonicalArtifactError, Unit] =
    collect(indices.map: index =>
      val run = source.runs(index)
      run.geometry.resolve(run.runId, RunIndex.unsafe(index), scope).left.map(CanonicalArtifactError.Temporal.apply).flatMap: geometry =>
        val p = BigInt(source.neuralAxis.size)
        if geometry.preparedDesign.timepoints != run.observations.rows then
          Left(CanonicalArtifactError.Input("temporal design and observation time domains differ"))
        else if p * geometry.preparedDesign.predictors > Int.MaxValue then
          Left(CanonicalArtifactError.Input("response-design moment exceeds primitive array capacity"))
        else Right(())
    ).map(_ => ())

  private def contrastMoments[P <: SemanticSpace, N <: SemanticSpace](
      source: CanonicalRunSet[P, N, CanonicalGeometrySchedule], indices: Vector[Int], scope: TrainingRunScope, budget: ResourceBudget
  ): Either[CanonicalArtifactError, Vector[CanonicalRunMoments]] =
    validateContrast(source, indices, scope).flatMap: _ =>
      traverse(indices): index =>
        val run = source.runs(index)
        for
          geometry <- run.geometry.resolve(run.runId, RunIndex.unsafe(index), scope).left.map(CanonicalArtifactError.Temporal.apply)
          response <- run.read(budget)
          prepared <- geometry.prepareResponse(response).left.map(error => CanonicalArtifactError.Temporal(OneShotMvpaError.TemporalPreparationFailure(run.runId, error)))
          moments <- CanonicalMoments.accumulate(prepared, geometry, Array.tabulate(source.neuralAxis.size)(identity)).left.map(CanonicalArtifactError.Temporal.apply)
        yield moments

  private def fitContrast[P <: SemanticSpace, N <: SemanticSpace](
      source: CanonicalRunSet[P, N, CanonicalGeometrySchedule], indices: Vector[Int], moments: Vector[CanonicalRunMoments],
      regularization: ResidualRegularization, admission: NumericResourceAdmission
  ): Either[CanonicalArtifactError, CanonicalArtifact[N, ? <: SemanticSpace]] =
    val effect = sumMatrices(moments.map(_.effect))
    val residual = sumMatrices(moments.map(_.residual))
    for
      e <- covariance(source.neural, effect, "effect")
      r <- covariance(source.neural, residual, "temporal-residual")
      problem <- CanonicalEffectProblem.fromOperators(source.neural, e, r, regularization).left.map(CanonicalArtifactError.MultivarFailure.apply)
      fit <- problem.fit.left.map(CanonicalArtifactError.MultivarFailure.apply)
    yield CanonicalArtifact(source.neural, source.neuralAxis, fit, trainingReceipt(source, indices, moments.map(_.temporalReceipt), effect, residual, admission))

  private def fitConstrained[P <: SemanticSpace, N <: SemanticSpace](
      source: CanonicalRunSet[P, N, CanonicalGeometrySchedule], indices: Vector[Int], moments: Vector[CanonicalRunMoments],
      model: NonnegativeCanonicalModelSpec, admission: NumericResourceAdmission
  ): Either[CanonicalArtifactError, NonnegativeCanonicalArtifact[N, ? <: SemanticSpace]] =
    val effect = sumMatrices(moments.map(_.effect))
    val residual = sumMatrices(moments.map(_.residual))
    for
      e <- covariance(source.neural, effect, "effect")
      r <- covariance(source.neural, residual, "temporal-residual")
      problem <- ConstrainedCanonicalProblem.fromOperators(source.neural, e, r, model.regularization, model.constraint, model.solver).left.map(CanonicalArtifactError.MultivarFailure.apply)
      fit <- problem.fit.left.map(CanonicalArtifactError.MultivarFailure.apply)
    yield NonnegativeCanonicalArtifact(source.neural, source.neuralAxis, fit, trainingReceipt(source, indices, moments.map(_.temporalReceipt), effect, residual, admission))

  private def trainingReceipt[P <: SemanticSpace, N <: SemanticSpace, G](
      source: CanonicalRunSet[P, N, G], indices: Vector[Int], preparations: Vector[TemporalPreparationReceipt],
      effect: DMat, residual: DMat, admission: NumericResourceAdmission
  ): CanonicalTrainingReceipt =
    val trainingEvidence = AxisDigest.sha256Hex: writer =>
      writer.string("canonical-training-evidence-v1")
      writer.string(source.neuralAxis.stableKey)
      writer.intLE(indices.length)
      indices.foreach(index => writer.string(source.runs(index).identity))
    CanonicalTrainingReceipt(indices.map(source.runs(_).runId), indices.zip(preparations).map((index, receipt) => source.runs(index).runId -> receipt),
      trainingEvidence, momentIdentity(effect, residual), admission)

  private def foldReceipt[P <: SemanticSpace, N <: SemanticSpace](
      source: CanonicalRunSet[P, N, CanonicalGeometrySchedule], held: Int, training: Vector[Int], moments: Vector[CanonicalRunMoments], heldMoment: CanonicalRunMoments
  ): CanonicalFoldReceipt =
    val indexed = training.zip(moments).toMap + (held -> heldMoment)
    CanonicalFoldReceipt(training.map(source.runs(_).runId), source.runs(held).runId,
      source.runs.indices.map(index => source.runs(index).runId -> indexed(index).temporalReceipt).toVector, CanonicalMomentExecution.RunwiseSufficientStatistics)

  private[fit] def covariance[N <: SemanticSpace](space: SpaceEvidence[N], values: DMat, label: String): Either[CanonicalArtifactError, OpCovariance[N, CertifiedPsd]] =
    val id = ValueIdentity.source(ValueId.unsafe(s"canonical-${space.id.value}-$label-${momentIdentity(values)}"))
    for
      context <- CertificateContext.from(CertificateTolerance.strict, CertificateNorm.Frobenius, s"canonical-$label-psd", "gale", NumericalPrecision.Float64).left.map(CanonicalArtifactError.SemanticFailure.apply)
      linear <- Lin.fromDenseMatrix(values, CoordinateEvidence.dual(space), CoordinateEvidence.primal(space), id).left.map(CanonicalArtifactError.SemanticFailure.apply)
      certificate <- FormCertificates.psd(linear, context).left.map(CanonicalArtifactError.SemanticFailure.apply)
      result <- Op.certifiedPsd(Op.fromLin(linear, OperatorRoleWitness.covariance), certificate).left.map(CanonicalArtifactError.SemanticFailure.apply)
    yield result

  private[fit] def momentIdentity(values: DMat*): String = AxisDigest.sha256Hex: writer =>
    writer.string("canonical-dense-moments-v1")
    writer.intLE(values.length)
    values.foreach: matrix =>
      writer.intLE(matrix.rows)
      writer.intLE(matrix.cols)
      var row = 0
      while row < matrix.rows do
        var column = 0
        while column < matrix.cols do
          writer.string(java.lang.Double.toHexString(matrix(row, column)))
          column += 1
        row += 1

  private def simpleDirection[N <: SemanticSpace](run: RunId, fit: CanonicalEffectFit[N, ? <: SemanticSpace]): Either[CanonicalArtifactError, DVec] =
    fit.solution match
      case CanonicalEffectSolution.Simple(direction, _) => Right(direction)
      case CanonicalEffectSolution.LeadingSubspace(_, _, multiplicity) => Left(CanonicalArtifactError.Temporal(OneShotMvpaError.NonIdentifiableHeldOutDirection(run, multiplicity)))

  private def score(run: RunId, direction: DVec, held: CanonicalRunMoments, tolerance: CertificateTolerance): Either[CanonicalArtifactError, Double] =
    val denominator = quadratic(direction, held.residual)
    val root = quadratic(direction, held.effect) / denominator
    if !denominator.isFinite || denominator <= tolerance.threshold(matrixFrobenius(held.residual)) then
      Left(CanonicalArtifactError.Temporal(OneShotMvpaError.NonPositiveHeldOutDenominator(run, denominator)))
    else if root.isFinite && root >= -1e-10 then Right(math.max(0.0, root))
    else Left(CanonicalArtifactError.Temporal(OneShotMvpaError.InvalidHeldOutRoot(run, root)))

  private def signedScore[N <: SemanticSpace](run: RunId, direction: DVec, training: Vector[CanonicalRunMoments], held: CanonicalRunMoments,
      fit: CanonicalEffectFit[N, ? <: SemanticSpace]): Either[CanonicalArtifactError, (Double, Double, SignedCrossRunRayleigh)] =
    def dot(values: DVec): Double =
      var sum = 0.0
      var i = 0
      while i < direction.length do
        sum += direction(i) * values(i)
        i += 1
      sum
    val numerator = training.map(m => dot(m.contrastEstimate)).sum * dot(held.contrastEstimate) / held.contrastVariance
    val residual = sumMatrices(training.map(_.residual))
    val denominator = quadratic(direction, residual) + fit.regularization.ridgeAmount * dot(direction)
    val threshold = fit.programFit.identifiability.context.tolerance.threshold(matrixFrobenius(residual) + fit.regularization.ridgeAmount * math.sqrt(direction.length.toDouble))
    if !numerator.isFinite then Left(CanonicalArtifactError.Temporal(OneShotMvpaError.NonFiniteCrossRunNumerator(run, numerator)))
    else if !denominator.isFinite || denominator <= threshold then Left(CanonicalArtifactError.Temporal(OneShotMvpaError.NonPositiveCrossRunDenominator(run, denominator)))
    else SignedCrossRunRayleigh(numerator / denominator).left.map(CanonicalArtifactError.Temporal.apply).map(value => (numerator, denominator, value))

  private[fit] def collect[A](values: Vector[Either[CanonicalArtifactError, A]]): Either[CanonicalArtifactError, Vector[A]] =
    values.foldLeft[Either[CanonicalArtifactError, Vector[A]]](Right(Vector.empty))((result, value) => result.flatMap(found => value.map(found :+ _)))

  private def contrastWorkspace[P <: SemanticSpace, N <: SemanticSpace](source: CanonicalRunSet[P, N, CanonicalGeometrySchedule]): BigInt =
    source.runs.map(run => BigInt(source.neuralAxis.size) * run.geometry.geometries.map(_.preparedDesign.predictors).max * 4).sum

  private def manovaWorkspace[P <: SemanticSpace, N <: SemanticSpace](source: CanonicalRunSet[P, N, ManovaGeometrySchedule]): BigInt =
    source.runs.map(run => BigInt(source.neuralAxis.size) * run.geometry.geometries.map(g => g.preparedDesign.predictors + g.contrastRank).max * 4).sum

  private def traverse[I, A](inputs: Vector[I])(task: I => Either[CanonicalArtifactError, A]): Either[CanonicalArtifactError, Vector[A]] =
    inputs.foldLeft[Either[CanonicalArtifactError, Vector[A]]](Right(Vector.empty))((result, input) => result.flatMap(found => task(input).map(found :+ _)))

  private def average(values: Vector[Double]): Double =
    val scale = values.map(math.abs).max
    if scale == 0.0 then 0.0
    else (values.map(_ / scale).sum / values.length) * scale

  private def validateSelection[P <: SemanticSpace, N <: SemanticSpace, G](source: CanonicalRunSet[P, N, G], training: CanonicalTrainingRuns[P]): Either[CanonicalArtifactError, Unit] =
    if training.partitionAxis == source.partitionAxis && training.indices.map(source.runs(_).runId) == training.runIds then Right(())
    else Left(CanonicalArtifactError.Input("training selection belongs to another run partition"))
