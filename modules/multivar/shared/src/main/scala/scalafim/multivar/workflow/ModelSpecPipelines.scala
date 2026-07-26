package scalafim.multivar
package workflow

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*
import scalafim.multivar.family.spectral.*
import scalafim.multivar.family.canonical.*

import gale.linalg.DMat

/** Fold-local exact quadratic GPCA.
  *
  * Candidate values are `components` and `quadraticWeight`. Statistical
  * operators, the penalty pullback, requested/lowered programs, and the exact
  * spectral rewrite are rebuilt independently inside every training scope.
  */
final class ExactQuadraticGpcaFoldPipeline(
    val placement: QuadraticPlacement = QuadraticPlacement.ObjectiveRidge
) extends FoldPipeline:
  val family: QuadraticFamily = QuadraticFamily.Ridge

  def fit(
      context: TrainingContext,
      training: ProcessedStudy,
      candidate: HyperparameterCandidate
  ): Either[ModelSpecError, FoldPipelineFit] =
    for
      components <- modelComponents(candidate)
      rawWeight <- candidate.value("quadraticWeight")
      weight <- PenaltyWeight(rawWeight).left.map(ModelSpecError.Program.apply)
      rowSpace <- MvSpace
        .of(s"${context.split.stringValue}.quadratic.rows", SpaceRole.Samples, training.values.rows)
        .left
        .map(ModelSpecError.Multivar.apply)
      rowMetric <- MetricSpec.identity(training.values.rows, Some(rowSpace)).left.map(ModelSpecError.Multivar.apply)
      featureMetric <- MetricSpec
        .identity(training.values.cols, Some(training.featureSpace))
        .left
        .map(ModelSpecError.Multivar.apply)
      prepared <- DynamicGpcaProblem
        .from(
          training.values,
          rowSpace,
          training.featureSpace,
          rowMetric,
          featureMetric,
          ValueIdentity.derived("modelspec-quadratic-gpca", training.sourceIdentity),
          training.provenance
        )
        .left
        .map(ModelSpecError.Multivar.apply)
      problem = prepared.value
      targetOperator <- Op
        .fromDense(
          DMat.eye(problem.featureSpace.dimension),
          CoordinateEvidence.dual(problem.featureSpace),
          CoordinateEvidence.primal(problem.featureSpace),
          OperatorRoleWitness.cross,
          ValueIdentity.derived("modelspec-quadratic-target", training.sourceIdentity),
          training.provenance
        )
        .left
        .map(error => ModelSpecError.InvalidDefinition(error.message))
      parameterId = ParameterId.unsafe(s"${problem.featureSpace.id.value}.quadratic-gpca-frame")
      target <- TargetExpression
        .linear(parameterId, "modelspec-quadratic-feature-target", targetOperator)
        .left
        .map(ModelSpecError.Program.apply)
      term = PenaltyTerm(
        target,
        FunctionalKind.SquaredNorm(problem.featureMetric.valueIdentity),
        weight
      )
      lowering <- QuadraticPullback
        .lower(term, targetOperator, problem.featureMetric, family, placement)
        .left
        .map(ModelSpecError.Quadratic.apply)
      exact <- ExactSpectralPrograms
        .gpcaQuadratic(problem, lowering, components)
        .left
        .map(ModelSpecError.ExactSpectral.apply)
      effectiveNumerator = placement match
        case QuadraticPlacement.ObjectiveRidge => QuadraticPullback.effective(problem.covariance, lowering)
        case QuadraticPlacement.DenominatorLoading => problem.covariance
      effectiveDenominator = placement match
        case QuadraticPlacement.ObjectiveRidge => problem.featureCometric
        case QuadraticPlacement.DenominatorLoading => QuadraticPullback.effective(problem.featureCometric, lowering)
      numeratorSnapshot <- OperatorSnapshot
        .from("effective-numerator", DerivedOperatorKind.SecondOrder, effectiveNumerator)
        .left
        .map(ModelSpecError.Multivar.apply)
      denominatorSnapshot <- OperatorSnapshot
        .from("effective-denominator", DerivedOperatorKind.SecondOrder, effectiveDenominator)
        .left
        .map(ModelSpecError.Multivar.apply)
      proofDiagnostic <- FitDiagnostic
        .from("exact-rewrite-residual", exact.proof.residual, Some(exact.proof.tolerance.threshold(1.0)))
        .left
        .map(ModelSpecError.Multivar.apply)
      effectiveOperators = Vector(numeratorSnapshot, denominatorSnapshot)
      bundle <- OperatorFitBundle
        .from(exact.programFit, effectiveOperators, Vector(proofDiagnostic), exact.provenance)
        .left
        .map(ModelSpecError.Multivar.apply)
      solverExecution <- SolverExecutionRecord.from(
        "gale-exact-quadratic-eigen",
        "exact quadratic generalized symmetric-definite eigen",
        Vector(
          "components" -> components.value.toString,
          "family" -> family.toString,
          "placement" -> placement.toString,
          "quadraticWeight" -> weight.value.toString,
          "rewriteExact" -> exact.proof.exact.toString
        ),
        exact.programFit.solverAttestation
      )
      events = Vector(
        LifecycleEvent.fit(
          context,
          LifecycleStage.StatisticalEstimation,
          "quadratic-gpca-statistics",
          problem.provenance
        ),
        LifecycleEvent.fit(
          context,
          LifecycleStage.ProgramBuild,
          "quadratic-gpca-program",
          exact.requestedProgram.provenance
        ),
        LifecycleEvent.fit(
          context,
          LifecycleStage.Lowering,
          "exact-quadratic-rewrite",
          exact.provenance
        ),
        LifecycleEvent.fit(
          context,
          LifecycleStage.Solve,
          "gale-exact-quadratic-eigen",
          exact.programFit.provenance
        )
      )
      result <- FoldPipelineFit.from(
        context,
        training,
        exact.requestedProgram,
        exact.loweredProgram,
        bundle,
        Vector.empty,
        effectiveOperators,
        Vector.empty,
        None,
        solverExecution,
        events,
        exact.provenance
      )
    yield result

/** Fold-local coordinate-constrained generalized-Rayleigh model.
  *
  * Candidate value `ridge` controls trace-scaled residual regularization. The
  * nonnegative cone is part of the fitted program, and the returned guarantee
  * remains `StationaryPoint`; the projected solver is never relabeled as an
  * unconstrained spectral optimum.
  */
final class NonnegativeCanonicalFoldPipeline(
    val solver: ConstrainedCanonicalSolverSpec = ConstrainedCanonicalSolverSpec.default
) extends FoldPipeline:
  def fit(
      context: TrainingContext,
      training: ProcessedStudy,
      candidate: HyperparameterCandidate
  ): Either[ModelSpecError, FoldPipelineFit] =
    for
      rawRidge <- candidate.value("ridge")
      ridge <- TraceRidgeFraction(rawRidge).left.map(ModelSpecError.Multivar.apply)
      dense <- training.values.toDense(StoragePolicy.AllowDense).left.map(ModelSpecError.Multivar.apply)
      feature = SpaceRef(training.featureSpace)
      effectValues = dense.t * dense
      residualValues = DMat.eye(training.values.cols)
      effect <- certifiedTrainingCovariance(
        feature.evidence,
        effectValues,
        ValueIdentity.derived("modelspec-constrained-effect", training.sourceIdentity),
        training.provenance
      )
      residual <- certifiedTrainingCovariance(
        feature.evidence,
        residualValues,
        ValueIdentity.derived("modelspec-constrained-residual", training.sourceIdentity),
        training.provenance
      )
      problem <- ConstrainedCanonicalProblem
        .fromOperators(
          feature.evidence,
          effect,
          residual,
          ResidualRegularization.TraceScaled(ridge),
          CanonicalFrameConstraint.Nonnegative,
          solver,
          CertificateTolerance.strict,
          training.provenance
        )
        .left
        .map(ModelSpecError.Multivar.apply)
      fitted <- problem.fit.left.map(ModelSpecError.Multivar.apply)
      effectSnapshot <- OperatorSnapshot
        .from("constrained-effect", DerivedOperatorKind.SecondOrder, effect)
        .left
        .map(ModelSpecError.Multivar.apply)
      residualSnapshot <- OperatorSnapshot
        .from("regularized-residual", DerivedOperatorKind.SecondOrder, fitted.regularizedResidual)
        .left
        .map(ModelSpecError.Multivar.apply)
      stationarity <- FitDiagnostic
        .from("projected-stationarity", fitted.diagnostics.stationarityResidual, Some(solver.tolerance))
        .left
        .map(ModelSpecError.Multivar.apply)
      feasibility <- FitDiagnostic
        .from("constraint-violation", fitted.diagnostics.constraintViolation, Some(solver.tolerance))
        .left
        .map(ModelSpecError.Multivar.apply)
      normalization <- FitDiagnostic
        .from("normalization-error", fitted.diagnostics.normalizationError, Some(solver.tolerance))
        .left
        .map(ModelSpecError.Multivar.apply)
      effectiveOperators = Vector(effectSnapshot, residualSnapshot)
      bundle <- OperatorFitBundle
        .from(
          fitted.programFit,
          effectiveOperators,
          Vector(stationarity, feasibility, normalization),
          fitted.provenance.semantic
        )
        .left
        .map(ModelSpecError.Multivar.apply)
      solverExecution <- SolverExecutionRecord.from(
        "gale-projected-rayleigh",
        "gale.optim projected generalized-Rayleigh",
        Vector(
          "constraint" -> CanonicalFrameConstraint.Nonnegative.toString,
          "ridge" -> ridge.value.toString,
          "tolerance" -> solver.tolerance.toString,
          "maxIterations" -> solver.maxIterations.toString,
          "iterations" -> fitted.diagnostics.iterations.toString,
          "termination" -> fitted.diagnostics.termination.toString
        ),
        fitted.programFit.solverAttestation
      )
      events = Vector(
        LifecycleEvent.fit(
          context,
          LifecycleStage.StatisticalEstimation,
          "constrained-canonical-moments",
          training.provenance
        ),
        LifecycleEvent.fit(
          context,
          LifecycleStage.ProgramBuild,
          "nonnegative-canonical-program",
          fitted.programFit.program.provenance
        ),
        LifecycleEvent.fit(
          context,
          LifecycleStage.Lowering,
          "projected-rayleigh-lowering",
          fitted.provenance.semantic
        ),
        LifecycleEvent.fit(
          context,
          LifecycleStage.Solve,
          "gale-projected-rayleigh",
          fitted.provenance.semantic
        )
      )
      result <- FoldPipelineFit.from(
        context,
        training,
        fitted.programFit.program,
        fitted.programFit.program,
        bundle,
        Vector.empty,
        effectiveOperators,
        Vector.empty,
        None,
        solverExecution,
        events,
        fitted.provenance.semantic
      )
    yield result

private def modelComponents(candidate: HyperparameterCandidate): Either[ModelSpecError, ComponentCount] =
  candidate.value("components").flatMap: raw =>
    if raw.isValidInt && raw == raw.toInt.toDouble && raw > 0.0 then
      Right(ComponentCount.unsafe(raw.toInt))
    else Left(ModelSpecError.InvalidHyperparameter("components", raw, "must be a positive integer"))

private def certifiedTrainingCovariance[Feature <: SemanticSpace](
    feature: SpaceEvidence[Feature],
    values: DMat,
    identity: ValueIdentity,
    provenance: SemanticProvenance
): Either[ModelSpecError, OpCovariance[Feature, CertifiedPsd]] =
  for
    _ <- MatrixOps.checkFinite("fold-local covariance", values).left.map(ModelSpecError.Multivar.apply)
    context <- CertificateContext
      .from(
        CertificateTolerance.strict,
        CertificateNorm.Frobenius,
        "modelspec-fold-local-psd",
        "gale",
        NumericalPrecision.Float64
      )
      .left
      .map(error => ModelSpecError.InvalidDefinition(error.message))
    linear <- Lin
      .fromDenseMatrix(
        values,
        CoordinateEvidence.dual(feature),
        CoordinateEvidence.primal(feature),
        identity,
        provenance
      )
      .left
      .map(error => ModelSpecError.InvalidDefinition(error.message))
    certificate <- FormCertificates
      .psd(linear, context)
      .left
      .map(error => ModelSpecError.InvalidDefinition(error.message))
    certified <- Op
      .certifiedPsd(Op.fromLin(linear, OperatorRoleWitness.covariance), certificate)
      .left
      .map(error => ModelSpecError.InvalidDefinition(error.message))
  yield certified
