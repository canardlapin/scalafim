package scalafim.multivar
package family.canonical

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*

import gale.linalg.{DMat, DVec, Matrix}
import gale.optim.{ProjectedRayleigh, ProjectedRayleighConfig, ProjectedRayleighResult, ProjectedRayleighTermination}

enum CanonicalFrameConstraint:
  case Nonnegative

  def feasibleSet: FeasibleSetKind =
    this match
      case Nonnegative => FeasibleSetKind.NonnegativeOrthant

/** A coordinate constraint removes the ordinary sign gauge. The coordinate
  * system is therefore part of the scientific estimand, not a display choice.
  */
enum ConstrainedCanonicalGauge:
  case CoordinateIdentified

final class ConstrainedCanonicalSolverSpec private (
    val tolerance: Double,
    val maxIterations: Int
):
  private[multivar] def galeConfig: ProjectedRayleighConfig =
    ProjectedRayleighConfig(tolerance = tolerance, maxIterations = maxIterations)

object ConstrainedCanonicalSolverSpec:
  def from(tolerance: Double, maxIterations: Int): Either[MultivarError, ConstrainedCanonicalSolverSpec] =
    if !tolerance.isFinite || tolerance <= 0.0 then
      Left(MultivarError.InvalidTolerance("constrained canonical solver tolerance", tolerance))
    else if maxIterations <= 0 then Left(MultivarError.InvalidDimension("constrained canonical iteration budget", maxIterations))
    else Right(new ConstrainedCanonicalSolverSpec(tolerance, maxIterations))

  val default: ConstrainedCanonicalSolverSpec =
    new ConstrainedCanonicalSolverSpec(1e-10, 5000)

final case class ConstrainedCanonicalDiagnostics(
    stationarityResidual: Double,
    constraintViolation: Double,
    normalizationError: Double,
    objectiveChange: Double,
    iterations: Int,
    termination: ProjectedRayleighTermination
):
  require(stationarityResidual.isFinite && stationarityResidual >= 0.0)
  require(constraintViolation.isFinite && constraintViolation >= 0.0)
  require(normalizationError.isFinite && normalizationError >= 0.0)
  require(objectiveChange.isFinite && objectiveChange >= 0.0)
  require(iterations >= 0)

final case class ConstrainedCanonicalFit[
    Feature <: SemanticSpace,
    Component <: SemanticSpace
](
    root: CanonicalRoot,
    direction: DVec,
    constraint: CanonicalFrameConstraint,
    gauge: ConstrainedCanonicalGauge,
    functionalFrame: FunctionalFrame[Feature, Component, UncheckedEvidence],
    programFit: OperatorProgramFit,
    regularizedResidual: OpCovariance[Feature, CertifiedSpd],
    regularization: ResidualRegularizationFit,
    diagnostics: ConstrainedCanonicalDiagnostics,
    solverResult: ProjectedRayleighResult,
    provenance: CanonicalEffectProvenance
):
  require(direction.length == functionalFrame.weights.codomain.descriptor.dimension)

/** A typed coordinate-constrained generalized-Rayleigh problem.
  *
  * This is a distinct estimand from [[CanonicalEffectProblem]]: the declared
  * feature coordinates and feasible cone are part of the model. Construction
  * remains free of folds, contrasts, and ROIs; those layers provide effect and
  * residual operators without changing the numerical problem.
  */
final class ConstrainedCanonicalProblem[Feature <: SemanticSpace] private (
    val canonical: CanonicalEffectProblem[Feature],
    val constraint: CanonicalFrameConstraint,
    val solver: ConstrainedCanonicalSolverSpec
):
  val featureSpace: SpaceEvidence[Feature] = canonical.featureSpace

  def fit: Either[MultivarError, ConstrainedCanonicalFit[Feature, ? <: SemanticSpace]] =
    for
      effectDense <- constrainedSemantic(canonical.effect.toDense)
      residualDense <- constrainedSemantic(canonical.residual.toDense)
      prepared <- prepareCanonicalResidual(
        residualDense,
        featureSpace.dimension,
        canonical.regularization
      )
      (regularizedDense, regularizationFit) = prepared
      regularized <- certifyCanonicalResidual(
        featureSpace,
        canonical.residual,
        regularizedDense,
        canonical.regularization,
        canonical.tolerance,
        canonical.provenance
      )
      solved <- solve(effectDense, regularizedDense)
      root <- CanonicalRoot(solved.root)
      component <- SpaceRef.of(s"${featureSpace.id.value}.constrained-canonical", SpaceRole.Latent, 1)
      fit <- assemble(component, regularized, regularizationFit, solved, root, effectDense, regularizedDense)
    yield fit

  private def solve(effect: DMat, residual: DMat): Either[MultivarError, ProjectedRayleighResult] =
    val result =
      constraint match
        case CanonicalFrameConstraint.Nonnegative =>
          ProjectedRayleigh.solveNonnegative(effect, residual, solver.galeConfig)
    result.left.map(error => MultivarError.SolverFailed(error.message)).flatMap: solved =>
      if solved.converged then Right(solved)
      else
        solved.termination match
          case ProjectedRayleighTermination.IterationLimit =>
            Left(
              MultivarError.IterationLimitExceeded(
                "gale projected generalized-Rayleigh",
                solver.maxIterations,
                solved.certificate.stationarityResidual
              )
            )
          case other => Left(MultivarError.SolverFailed(s"gale projected generalized-Rayleigh stopped at $other"))

  private def assemble(
      component: SpaceRef,
      regularizedResidual: OpCovariance[Feature, CertifiedSpd],
      regularizationFit: ResidualRegularizationFit,
      solved: ProjectedRayleighResult,
      root: CanonicalRoot,
      effectDense: DMat,
      residualDense: DMat
  ): Either[MultivarError, ConstrainedCanonicalFit[Feature, component.Id]] =
    val parameterId = ParameterId.unsafe(s"${featureSpace.id.value}.constrained-canonical-frame")
    val fitProvenance = canonical.provenance.append(
      SemanticProvenanceEvent.Derived(
        "gale-projected-generalized-rayleigh",
        Vector(canonical.effect.valueIdentity, regularizedResidual.valueIdentity)
      )
    )
    val scale = constrainedMatrixFrobenius(effectDense) + Math.abs(root.value) * constrainedMatrixFrobenius(residualDense)
    val normalizedStationarity = solved.certificate.stationarityResidual / Math.max(1.0, scale)
    val certificateResidual = Math.max(
      normalizedStationarity,
      Math.max(solved.certificate.constraintViolation, solved.certificate.normalizationError)
    )
    for
      variable <- constrainedProgram(FrameVariable.from(parameterId, featureSpace, component.evidence))
      frameOperator <- constrainedSemantic(
        Op.fromDense(
          directionMatrix(solved.direction),
          CoordinateEvidence.primal(component.evidence),
          CoordinateEvidence.dual(featureSpace),
          OperatorRoleWitness.frame,
          ValueIdentity.derived(
            "constrained-canonical-frame",
            canonical.effect.valueIdentity,
            regularizedResidual.valueIdentity
          ),
          fitProvenance
        )
      )
      parameterization = FrameParameterization.identity(variable)
      normalization = FrameNormalization(variable, regularizedResidual)
      target = ParameterExpression[DMat](parameterId, "functional-frame")
      constraintTerm = ConstraintTerm.typed(target, TypedFeasibleSet.nonnegative[DMat])
      program <- constrainedProgram(
        OperatorProgram.from(
          Vector(parameterization),
          BaseObjective.GeneralizedRayleigh(
            SelfCompressionExpression(variable, canonical.effect),
            SelfCompressionExpression(variable, regularizedResidual)
          ),
          Vector(normalization),
          constraints = Vector(constraintTerm),
          provenance = fitProvenance
        )
      )
      context <- constrainedSemantic(
        CertificateContext.from(
          CertificateTolerance.strict,
          CertificateNorm.Euclidean,
          "projected-generalized-rayleigh-kkt",
          "gale",
          NumericalPrecision.Float64,
          Some(regularizationLabel(regularizationFit))
        )
      )
      functionalFrame = FunctionalFrame(frameOperator)
      programFit <- constrainedProgram(
        OperatorProgramFit.stationary(
          program,
          Vector(FittedFrame(variable, functionalFrame)),
          root.value,
          NumericalIdentifiability(1, Vector(Vector(0)), certificateResidual, context),
          fitProvenance
        )
      )
    yield
      ConstrainedCanonicalFit(
        root,
        solved.direction,
        constraint,
        ConstrainedCanonicalGauge.CoordinateIdentified,
        functionalFrame,
        programFit,
        regularizedResidual,
        regularizationFit,
        ConstrainedCanonicalDiagnostics(
          solved.certificate.stationarityResidual,
          solved.certificate.constraintViolation,
          solved.certificate.normalizationError,
          solved.certificate.objectiveChange,
          solved.iterations,
          solved.termination
        ),
        solved,
        CanonicalEffectProvenance(
          canonical.effect.valueIdentity,
          canonical.residual.valueIdentity,
          regularizedResidual.valueIdentity,
          "gale.optim.ProjectedRayleigh.solveNonnegative",
          fitProvenance
        )
      )

object ConstrainedCanonicalProblem:
  def fromOperators[Feature <: SemanticSpace](
      featureSpace: SpaceEvidence[Feature],
      effect: OpCovariance[Feature, CertifiedPsd],
      residual: OpCovariance[Feature, CertifiedPsd],
      regularization: ResidualRegularization,
      constraint: CanonicalFrameConstraint = CanonicalFrameConstraint.Nonnegative,
      solver: ConstrainedCanonicalSolverSpec = ConstrainedCanonicalSolverSpec.default,
      tolerance: CertificateTolerance = CertificateTolerance.strict,
      provenance: SemanticProvenance = SemanticProvenance.source("constrained-canonical-operators")
  ): Either[MultivarError, ConstrainedCanonicalProblem[Feature]] =
    CanonicalEffectProblem
      .fromOperators(featureSpace, effect, residual, regularization, tolerance, provenance)
      .map(new ConstrainedCanonicalProblem(_, constraint, solver))

  def fromDense[Feature <: SemanticSpace](
      featureSpace: SpaceEvidence[Feature],
      effect: DMat,
      residual: DMat,
      regularization: ResidualRegularization,
      constraint: CanonicalFrameConstraint = CanonicalFrameConstraint.Nonnegative,
      solver: ConstrainedCanonicalSolverSpec = ConstrainedCanonicalSolverSpec.default,
      tolerance: CertificateTolerance = CertificateTolerance.strict,
      provenance: SemanticProvenance = SemanticProvenance.source("constrained-canonical-dense")
  ): Either[MultivarError, ConstrainedCanonicalProblem[Feature]] =
    CanonicalEffectProblem
      .fromDense(featureSpace, effect, residual, regularization, tolerance, provenance)
      .map(new ConstrainedCanonicalProblem(_, constraint, solver))

private def directionMatrix(direction: DVec): DMat =
  val out = Matrix.newBuilder(direction.length, 1)
  var row = 0
  while row < direction.length do
    out(row, 0) = direction(row)
    row += 1
  out.result()

private def constrainedMatrixFrobenius(matrix: DMat): Double =
  var squared = 0.0
  var row = 0
  while row < matrix.rows do
    var column = 0
    while column < matrix.cols do
      val value = matrix(row, column)
      squared += value * value
      column += 1
    row += 1
  Math.sqrt(squared)

private def regularizationLabel(fit: ResidualRegularizationFit): String =
  fit.specification match
    case ResidualRegularization.Unregularized => "unregularized"
    case ResidualRegularization.TraceScaled(fraction) => s"trace-scaled-${fraction.value}"

private def constrainedSemantic[A](value: Either[SemanticError, A]): Either[MultivarError, A] =
  value.left.map(error => MultivarError.SolverFailed(error.message))

private def constrainedProgram[A](value: Either[ProgramError, A]): Either[MultivarError, A] =
  value.left.map(error => MultivarError.SolverFailed(error.message))
