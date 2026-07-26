package scalafim.multivar
package family.multiblock

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*
import scalafim.multivar.solver.*
import scalafim.multivar.family.spectral.*

enum ExactMultiblockError:
  case InvalidDefinition(detail: String)
  case DirectSum(error: DirectSumError)
  case Spectral(error: ExactSpectralError)

  def message: String =
    this match
      case InvalidDefinition(detail) => detail
      case DirectSum(error)          => error.message
      case Spectral(error)           => error.message

/** Exact spectral execution specialized to direct-sum multiblock studies.
  *
  * Keeping this adapter with the multiblock family preserves a one-way
  * dependency: multiblock may reuse the spectral substrate, while the
  * spectral family remains independent of multiblock structure.
  */
object ExactMultiblockPrograms:
  def quadratic(
      study: DirectSumStudy,
      associationProblem: MaximizeAssociation[study.rowSpace.Id],
      disagreement: ConstraintPenalty[study.rowSpace.Id],
      weight: PenaltyWeight,
      components: ComponentCount,
      rankTolerance: SpectralRankTolerance = SpectralRankTolerance.default,
      solver: GeneralizedEigenSolver = DenseSolvers.generalizedEigen
  ): Either[ExactMultiblockError, ExactSpectralProgramFit[study.featureSpace.Id, ? <: SemanticSpace]] =
    for
      association <- DirectSumFeatureOperators
        .association(study, associationProblem.objective)
        .left
        .map(ExactMultiblockError.DirectSum.apply)
      cometric <- study.columnGeometry.cometric.toRight(
        ExactMultiblockError.InvalidDefinition(
          "multiset quadratic execution requires a certified direct-sum cometric"
        )
      )
      parameterId = ParameterId.unsafe(s"${study.featureSpace.descriptor.id.value}.quadratic-multiset-frame")
      target <- TargetExpression
        .linear(parameterId, "multiset-score-table", study.table)
        .left
        .map(error => ExactMultiblockError.Spectral(ExactSpectralError.Program(error)))
      term = PenaltyTerm(
        target,
        FunctionalKind.SquaredNorm(disagreement.operator.valueIdentity),
        weight
      )
      lowering <- QuadraticPullback
        .lower(
          term,
          study.table,
          disagreement.operator,
          QuadraticFamily.MultisetDisagreement,
          QuadraticPlacement.ObjectiveRidge
        )
        .left
        .map(error => ExactMultiblockError.Spectral(ExactSpectralError.Quadratic(error)))
      fit <- ExactSpectralPrograms
        .quadratic(
          study.featureSpace.evidence,
          association.operator,
          cometric,
          lowering,
          components,
          parameterId,
          "quadratic-multiset",
          rankTolerance,
          solver
        )
        .left
        .map(ExactMultiblockError.Spectral.apply)
    yield fit

  def hardEquality[
      ConstraintSpace <: SemanticSpace
  ](
      study: DirectSumStudy,
      associationProblem: MaximizeAssociation[study.rowSpace.Id],
      equality: HardScoreConstraint[study.rowSpace.Id, ConstraintSpace],
      components: ComponentCount,
      tolerance: CertificateTolerance = CertificateTolerance.strict,
      rankTolerance: SpectralRankTolerance = SpectralRankTolerance.default,
      solver: GeneralizedEigenSolver = DenseSolvers.generalizedEigen
  ): Either[ExactMultiblockError, ExactSpectralProgramFit[study.featureSpace.Id, ? <: SemanticSpace]] =
    for
      association <- DirectSumFeatureOperators
        .association(study, associationProblem.objective)
        .left
        .map(ExactMultiblockError.DirectSum.apply)
      cometric <- study.columnGeometry.cometric.toRight(
        ExactMultiblockError.InvalidDefinition(
          "hard multiset agreement requires a certified direct-sum cometric"
        )
      )
      scoreConstraint = study.table.andThen(equality.constraint.operator)
      fit <- ExactSpectralPrograms
        .hardEquality(
          study.featureSpace.evidence,
          association.operator,
          cometric,
          scoreConstraint,
          components,
          ParameterId.unsafe(s"${study.featureSpace.descriptor.id.value}.hard-agreement-frame"),
          "hard-multiset-agreement",
          tolerance,
          rankTolerance,
          solver
        )
        .left
        .map(ExactMultiblockError.Spectral.apply)
    yield fit
