package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

enum ExactSpectralRewriteKind:
  case ObjectiveQuadratic(family: QuadraticFamily)
  case DenominatorLoading(family: QuadraticFamily)
  case NullSpaceEquality

final case class ExactSpectralRewriteProof(
    kind: ExactSpectralRewriteKind,
    exact: Boolean,
    inputOperators: Vector[ValueIdentity],
    outputOperators: Vector[ValueIdentity],
    residual: Double,
    tolerance: CertificateTolerance,
    quadratic: Option[QuadraticEquivalenceProof],
    nullSpace: Option[NullSpaceProof]
):
  require(residual.isFinite && residual >= 0.0, "rewrite residual must be finite and non-negative")

enum ExactSpectralError:
  case InvalidDefinition(detail: String)
  case Multivar(error: MultivarError)
  case Program(error: ProgramError)
  case Semantic(error: SemanticError)
  case Quadratic(error: QuadraticLoweringError)
  case Parameterization(error: ParameterizationError)
  case DirectSum(error: DirectSumError)

  def message: String =
    this match
      case InvalidDefinition(detail) => detail
      case Multivar(error) => error.message
      case Program(error) => error.message
      case Semantic(error) => error.message
      case Quadratic(error) => error.message
      case Parameterization(error) => error.message
      case DirectSum(error) => error.message

final case class ExactSpectralProgramFit[
    Feature <: SemanticSpace,
    Component <: SemanticSpace
](
    requestedProgram: OperatorProgram,
    loweredProgram: OperatorProgram,
    programFit: OperatorProgramFit,
    functionalFrame: FunctionalFrame[Feature, Component, UncheckedEvidence],
    eigenvalues: DVec,
    solvedNumerator: DMat,
    solvedDenominator: DMat,
    proof: ExactSpectralRewriteProof,
    provenance: SemanticProvenance
)

object ExactSpectralPrograms:
  def gpcaQuadratic[Rows <: SemanticSpace, Feature <: SemanticSpace](
      problem: GpcaProblem[Rows, Feature],
      lowering: QuadraticLowering[Feature],
      components: ComponentCount,
      rankTolerance: SpectralRankTolerance = SpectralRankTolerance.default,
      solver: GeneralizedEigenSolver = DenseSolvers.generalizedEigen
  ): Either[ExactSpectralError, ExactSpectralProgramFit[Feature, ? <: SemanticSpace]] =
    quadratic(
      problem.featureSpace,
      problem.covariance,
      problem.featureCometric,
      lowering,
      components,
      ParameterId.unsafe(s"${problem.featureSpace.id.value}.quadratic-gpca-frame"),
      "quadratic-gpca",
      rankTolerance,
      solver
    )

  def multisetQuadratic(
      study: DirectSumStudy,
      associationProblem: MaximizeAssociation[study.rowSpace.Id],
      disagreement: ConstraintPenalty[study.rowSpace.Id],
      weight: PenaltyWeight,
      components: ComponentCount,
      rankTolerance: SpectralRankTolerance = SpectralRankTolerance.default,
      solver: GeneralizedEigenSolver = DenseSolvers.generalizedEigen
  ): Either[ExactSpectralError, ExactSpectralProgramFit[study.featureSpace.Id, ? <: SemanticSpace]] =
    for
      association <- DirectSumFeatureOperators
        .association(study, associationProblem.objective)
        .left
        .map(ExactSpectralError.DirectSum.apply)
      cometric <- study.columnGeometry.cometric.toRight(
        ExactSpectralError.InvalidDefinition("multiset quadratic execution requires a certified direct-sum cometric")
      )
      parameterId = ParameterId.unsafe(s"${study.featureSpace.descriptor.id.value}.quadratic-multiset-frame")
      target <- TargetExpression
        .linear(parameterId, "multiset-score-table", study.table)
        .left
        .map(ExactSpectralError.Program.apply)
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
        .map(ExactSpectralError.Quadratic.apply)
      fit <- quadratic(
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
    yield fit

  def multisetHardEquality[
      ConstraintSpace <: SemanticSpace
  ](
      study: DirectSumStudy,
      associationProblem: MaximizeAssociation[study.rowSpace.Id],
      equality: HardScoreConstraint[study.rowSpace.Id, ConstraintSpace],
      components: ComponentCount,
      tolerance: CertificateTolerance = CertificateTolerance.strict,
      rankTolerance: SpectralRankTolerance = SpectralRankTolerance.default,
      solver: GeneralizedEigenSolver = DenseSolvers.generalizedEigen
  ): Either[ExactSpectralError, ExactSpectralProgramFit[study.featureSpace.Id, ? <: SemanticSpace]] =
    for
      association <- DirectSumFeatureOperators
        .association(study, associationProblem.objective)
        .left
        .map(ExactSpectralError.DirectSum.apply)
      cometric <- study.columnGeometry.cometric.toRight(
        ExactSpectralError.InvalidDefinition("hard multiset agreement requires a certified direct-sum cometric")
      )
      scoreConstraint = study.table.andThen(equality.constraint.operator)
      fit <- hardEquality(
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
    yield fit

  private def quadratic[
      Feature <: SemanticSpace,
      RN <: OperatorRoleTag,
      EN <: OperatorEvidence
  ](
      featureSpace: SpaceEvidence[Feature],
      baseNumerator: Op[Dual[Feature], Primal[Feature], RN, EN],
      baseDenominator: OpCometric[Feature, CertifiedSpd],
      lowering: QuadraticLowering[Feature],
      components: ComponentCount,
      parameterId: ParameterId,
      method: String,
      rankTolerance: SpectralRankTolerance,
      solver: GeneralizedEigenSolver
  ): Either[ExactSpectralError, ExactSpectralProgramFit[Feature, ? <: SemanticSpace]] =
    val effectiveNumerator = lowering.placement match
      case QuadraticPlacement.ObjectiveRidge => QuadraticPullback.effective(baseNumerator, lowering)
      case QuadraticPlacement.DenominatorLoading => baseNumerator
    for
      numerator <- effectiveNumerator.toDense.left.map(ExactSpectralError.Semantic.apply)
      denominator <- lowering.placement match
        case QuadraticPlacement.ObjectiveRidge => Right(baseDenominator)
        case QuadraticPlacement.DenominatorLoading =>
          val raw = QuadraticPullback.effective(baseDenominator, lowering)
          raw.toDense
            .left
            .map(ExactSpectralError.Semantic.apply)
            .flatMap(dense => certifyCometric(raw, dense))
      denominatorDense <- denominator.toDense.left.map(ExactSpectralError.Semantic.apply)
      rayleigh <- GeneralizedRayleighRitz
        .solve(numerator, denominatorDense, components, rankTolerance, solver = solver)
        .left
        .map(ExactSpectralError.Multivar.apply)
      component <- SpaceRef
        .of(s"${featureSpace.id.value}.$method-components", SpaceRole.Latent, rayleigh.values.length)
        .left
        .map(ExactSpectralError.Multivar.apply)
      variable <- FrameVariable
        .from(parameterId, featureSpace, component.evidence)
        .left
        .map(ExactSpectralError.Program.apply)
      identityParameterization = FrameParameterization.identity(variable)
      requestedNormalization = FrameNormalization(variable, baseDenominator)
      requested <- OperatorProgram
        .from(
          Vector(identityParameterization),
          BaseObjective.MaximizeTrace(SelfCompressionExpression(variable, baseNumerator)),
          Vector(requestedNormalization),
          penalties = Vector(lowering.original),
          provenance = SemanticProvenance.source(s"$method-requested-program")
        )
        .left
        .map(ExactSpectralError.Program.apply)
      loweredNormalization = FrameNormalization(variable, denominator)
      lowered <- OperatorProgram
        .from(
          Vector(identityParameterization),
          BaseObjective.MaximizeTrace(SelfCompressionExpression(variable, effectiveNumerator)),
          Vector(loweredNormalization),
          provenance = SemanticProvenance.source(s"$method-lowered-program")
        )
        .left
        .map(ExactSpectralError.Program.apply)
      provenance = (baseNumerator.provenance ++ lowering.pulledBack.provenance).append(
        SemanticProvenanceEvent.Derived(
          s"exact-${lowering.placement.toString.toLowerCase}-spectral-rewrite",
          Vector(baseNumerator.valueIdentity, lowering.pulledBack.valueIdentity, denominator.valueIdentity)
        )
      )
      frameOperator <- Op
        .fromDense(
          rayleigh.vectors,
          CoordinateEvidence.primal(component.evidence),
          CoordinateEvidence.dual(featureSpace),
          OperatorRoleWitness.frame,
          ValueIdentity.derived(s"$method-frame", effectiveNumerator.valueIdentity, denominator.valueIdentity),
          provenance
        )
        .left
        .map(ExactSpectralError.Semantic.apply)
      frame = FunctionalFrame(frameOperator, Some(denominator))
      programFit <- fitProgram(
        lowered,
        variable,
        frame,
        rayleigh,
        sum(rayleigh.values),
        method,
        provenance
      )
      kind = lowering.placement match
        case QuadraticPlacement.ObjectiveRidge => ExactSpectralRewriteKind.ObjectiveQuadratic(lowering.family)
        case QuadraticPlacement.DenominatorLoading => ExactSpectralRewriteKind.DenominatorLoading(lowering.family)
    yield
      ExactSpectralProgramFit(
        requested,
        lowered,
        programFit,
        frame,
        rayleigh.values,
        numerator,
        denominatorDense,
        ExactSpectralRewriteProof(
          kind,
          exact = true,
          Vector(baseNumerator.valueIdentity, baseDenominator.valueIdentity, lowering.pulledBack.valueIdentity),
          Vector(effectiveNumerator.valueIdentity, denominator.valueIdentity),
          0.0,
          lowering.proof.tolerance,
          Some(lowering.proof),
          None
        ),
        provenance
      )

  private def hardEquality[
      Feature <: SemanticSpace,
      ConstraintSpace <: SemanticSpace,
      RN <: OperatorRoleTag,
      EN <: OperatorEvidence,
      RC <: OperatorRoleTag,
      EC <: OperatorEvidence
  ](
      featureSpace: SpaceEvidence[Feature],
      numerator: Op[Dual[Feature], Primal[Feature], RN, EN],
      denominator: OpCometric[Feature, CertifiedSpd],
      constraint: Op[Dual[Feature], Primal[ConstraintSpace], RC, EC],
      components: ComponentCount,
      parameterId: ParameterId,
      method: String,
      tolerance: CertificateTolerance,
      rankTolerance: SpectralRankTolerance,
      solver: GeneralizedEigenSolver
  ): Either[ExactSpectralError, ExactSpectralProgramFit[Feature, ? <: SemanticSpace]] =
    for
      constraintDense <- constraint.toDense.left.map(ExactSpectralError.Semantic.apply)
      basisValues <- nullSpaceBasis(constraintDense, components, tolerance)
      free <- SpaceRef
        .of(s"${featureSpace.id.value}.$method-free", SpaceRole.Observed, basisValues.cols)
        .left
        .map(ExactSpectralError.Multivar.apply)
      basis <- Op
        .fromDense(
          basisValues,
          CoordinateEvidence.dual(free.evidence),
          CoordinateEvidence.dual(featureSpace),
          OperatorRoleWitness.coefficient,
          ValueIdentity.derived("verified-null-space-basis", constraint.valueIdentity),
          constraint.provenance
        )
        .left
        .map(ExactSpectralError.Semantic.apply)
      component <- SpaceRef
        .of(s"${featureSpace.id.value}.$method-components", SpaceRole.Latent, components.value)
        .left
        .map(ExactSpectralError.Multivar.apply)
      variable <- FrameVariable
        .from(parameterId, featureSpace, component.evidence)
        .left
        .map(ExactSpectralError.Program.apply)
      parameterization <- LinearFrameParameterization
        .nullSpace(variable, free.evidence, basis, constraint, tolerance)
        .left
        .map(ExactSpectralError.Parameterization.apply)
      reducedNumerator = basis.andThen(numerator).andThen(basis.dual)
      reducedDenominator = basis.andThen(denominator).andThen(basis.dual)
      numeratorDense <- reducedNumerator.toDense.left.map(ExactSpectralError.Semantic.apply)
      denominatorDense <- reducedDenominator.toDense.left.map(ExactSpectralError.Semantic.apply)
      rayleigh <- GeneralizedRayleighRitz
        .solve(numeratorDense, denominatorDense, components, rankTolerance, solver = solver)
        .left
        .map(ExactSpectralError.Multivar.apply)
      weights <- basis(rayleigh.vectors).left.map(ExactSpectralError.Semantic.apply)
      target <- TargetExpression
        .linear(parameterId, "hard-linear-equality", constraint)
        .left
        .map(ExactSpectralError.Program.apply)
      normalization = FrameNormalization(variable, denominator)
      requested <- OperatorProgram
        .from(
          Vector(FrameParameterization.identity(variable)),
          BaseObjective.MaximizeTrace(SelfCompressionExpression(variable, numerator)),
          Vector(normalization),
          constraints = Vector(ConstraintTerm(target, FeasibleSetKind.ZeroSubspace)),
          provenance = SemanticProvenance.source(s"$method-requested-program")
        )
        .left
        .map(ExactSpectralError.Program.apply)
      lowered <- OperatorProgram
        .from(
          Vector(parameterization.descriptor),
          BaseObjective.MaximizeTrace(SelfCompressionExpression(variable, numerator)),
          Vector(normalization),
          provenance = SemanticProvenance.source(s"$method-lowered-program")
        )
        .left
        .map(ExactSpectralError.Program.apply)
      provenance = (numerator.provenance ++ constraint.provenance).append(
        SemanticProvenanceEvent.Derived(
          "exact-null-space-equality-rewrite",
          Vector(numerator.valueIdentity, denominator.valueIdentity, constraint.valueIdentity, basis.valueIdentity)
        )
      )
      frameOperator <- Op
        .fromDense(
          weights,
          CoordinateEvidence.primal(component.evidence),
          CoordinateEvidence.dual(featureSpace),
          OperatorRoleWitness.frame,
          ValueIdentity.derived(s"$method-frame", numerator.valueIdentity, basis.valueIdentity),
          provenance
        )
        .left
        .map(ExactSpectralError.Semantic.apply)
      frame = FunctionalFrame(frameOperator, Some(denominator))
      residualValues <- constraint(weights).left.map(ExactSpectralError.Semantic.apply)
      residual = maxAbs(residualValues)
      _ <-
        if residual <= tolerance.threshold(Math.max(1.0, maxAbs(weights))) then Right(())
        else Left(ExactSpectralError.InvalidDefinition(s"hard equality residual $residual exceeds its declared tolerance"))
      programFit <- fitProgram(
        lowered,
        variable,
        frame,
        rayleigh,
        sum(rayleigh.values),
        method,
        provenance
      )
      nullProof <- parameterization.nullSpaceProof.toRight(
        ExactSpectralError.InvalidDefinition("null-space lowering returned no verification proof")
      )
    yield
      ExactSpectralProgramFit(
        requested,
        lowered,
        programFit,
        frame,
        rayleigh.values,
        numeratorDense,
        denominatorDense,
        ExactSpectralRewriteProof(
          ExactSpectralRewriteKind.NullSpaceEquality,
          exact = true,
          Vector(numerator.valueIdentity, denominator.valueIdentity, constraint.valueIdentity),
          Vector(basis.valueIdentity, reducedNumerator.valueIdentity, reducedDenominator.valueIdentity),
          residual,
          tolerance,
          None,
          Some(nullProof)
        ),
        provenance
      )

  private def nullSpaceBasis(
      constraint: DMat,
      components: ComponentCount,
      tolerance: CertificateTolerance
  ): Either[ExactSpectralError, DMat] =
    val gram = constraint.t * constraint
    DenseSolvers.symmetricEigen.decompose(gram).left.map: error =>
      ExactSpectralError.Multivar(LinalgErrorAdapter.toMultivarError(error))
    .flatMap: eigen =>
      val leading = if eigen.values.length == 0 then 0.0 else Math.max(0.0, eigen.values(0))
      val threshold = tolerance.threshold(leading)
      val indices = eigen.values.toVector.zipWithIndex.collect:
        case (value, index) if Math.abs(value) <= threshold => index
      if indices.length < components.value then
        Left(
          ExactSpectralError.InvalidDefinition(
            s"hard equality leaves ${indices.length} free direction(s), fewer than ${components.value} requested component(s)"
          )
        )
      else Right(GaleNumerics.selectColumns(eigen.vectors, indices))

  private def certifyCometric[Feature <: SemanticSpace](
      raw: Op[Dual[Feature], Primal[Feature], CometricOperatorRole, UncheckedEvidence],
      dense: DMat
  ): Either[ExactSpectralError, OpCometric[Feature, CertifiedSpd]] =
    for
      linear <- Lin
        .fromDenseMatrix(dense, raw.domain, raw.codomain, raw.valueIdentity, raw.provenance)
        .left
        .map(ExactSpectralError.Semantic.apply)
      certificate <- FormCertificates.spd(linear).left.map(ExactSpectralError.Semantic.apply)
      certified <- Op.certifiedSpd(raw, certificate).left.map(ExactSpectralError.Semantic.apply)
    yield certified

  private def fitProgram[
      Feature <: SemanticSpace,
      Component <: SemanticSpace
  ](
      program: OperatorProgram,
      variable: FrameVariable[Feature, Component],
      frame: FunctionalFrame[Feature, Component, UncheckedEvidence],
      rayleigh: RayleighRitzResult,
      objective: Double,
      method: String,
      provenance: SemanticProvenance
  ): Either[ExactSpectralError, OperatorProgramFit] =
    for
      context <- CertificateContext
        .from(
          CertificateTolerance.strict,
          CertificateNorm.Frobenius,
          method,
          "gale",
          NumericalPrecision.Float64
        )
        .left
        .map(ExactSpectralError.Semantic.apply)
      fit <- OperatorProgramFit
        .from(
          program,
          Vector(FittedFrame(variable, frame)),
          objective,
          NumericalIdentifiability(
            rayleigh.values.length,
            rayleigh.diagnostics.spectralClusters,
            Math.max(rayleigh.diagnostics.generalizedResidual, rayleigh.diagnostics.normalizationResidual),
            context
          ),
          provenance
        )
        .left
        .map(ExactSpectralError.Program.apply)
    yield fit

  private def sum(values: DVec): Double =
    var result = 0.0
    var index = 0
    while index < values.length do
      result += values(index)
      index += 1
    result

  private def maxAbs(values: DMat): Double =
    var result = 0.0
    var row = 0
    while row < values.rows do
      var column = 0
      while column < values.cols do
        result = Math.max(result, Math.abs(values(row, column)))
        column += 1
      row += 1
    result
