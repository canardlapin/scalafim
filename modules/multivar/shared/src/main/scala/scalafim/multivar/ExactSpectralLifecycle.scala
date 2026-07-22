package scalafim.multivar

/** Family-indexed declared view of an exact-spectral operator program. */
final class ExactSpectralModelProgram private (
    val operatorProgram: OperatorProgram,
    val id: ProgramId[ExactSpectralFamily],
    val contract: FamilyContract[ExactSpectralFamily]
) extends ModelProgram[ExactSpectralFamily]:
  val family: ExactSpectralFamily = MathematicalModelFamily.ExactSpectralFrame
  def requestedClaim: RequestedOptimizationClaim = operatorProgram.resultSemantics.requestedClaim
  def provenance: SemanticProvenance = operatorProgram.provenance

object ExactSpectralModelProgram:
  def from(program: OperatorProgram): Either[ModelLifecycleError, ExactSpectralModelProgram] =
    if program.resultSemantics.requestedClaim != RequestedOptimizationClaim.ExactGlobal then
      Left(ModelLifecycleError.InvalidDefinition("an exact-spectral model program must request exact global optimization"))
    else
      FamilyContract
        .from[ExactSpectralFamily](MathematicalContractCatalog.exactSpectralFrame)
        .map(contract => new ExactSpectralModelProgram(program, ProgramId.from(program.valueIdentity), contract))

/** Execution-ready exact-spectral plan retaining its declared and lowered programs. */
final class ExactSpectralCompiledModel private[multivar] (
    val program: ExactSpectralModelProgram,
    val loweredProgram: OperatorProgram,
    val proof: ExactSpectralRewriteProof,
    val id: CompiledId[ExactSpectralFamily],
    val solverPlan: SolverPlan[ExactSpectralFamily],
    val provenance: SemanticProvenance
) extends CompiledModel[ExactSpectralFamily, ExactSpectralModelProgram]:
  def executionProgramIdentity: ValueIdentity = loweredProgram.valueIdentity

object ExactSpectralLifecycle:
  def declare(program: OperatorProgram): Either[ModelLifecycleError, ExactSpectralModelProgram] =
    ExactSpectralModelProgram.from(program)

  def compile(
      program: ExactSpectralModelProgram,
      loweredProgram: OperatorProgram,
      proof: ExactSpectralRewriteProof,
      method: String
  ): Either[ModelLifecycleError, ExactSpectralCompiledModel] =
    if loweredProgram.resultSemantics.requestedClaim != RequestedOptimizationClaim.ExactGlobal then
      Left(ModelLifecycleError.InvalidDefinition("an exact-spectral compiled plan must retain an exact-global request"))
    else if !proof.exact then
      Left(ModelLifecycleError.InvalidDefinition("an exact-spectral compiled plan requires an exact rewrite proof"))
    else
      for
        plan <- SolverPlan.from(program.id, method, program.requestedClaim)
        compiledId = CompiledId.derive(
          program.id,
          Vector(loweredProgram.valueIdentity) ++ proof.inputOperators ++ proof.outputOperators
        )
      yield
        new ExactSpectralCompiledModel(
          program,
          loweredProgram,
          proof,
          compiledId,
          plan,
          loweredProgram.provenance
        )

  def bundle[
      Feature <: SemanticSpace,
      Component <: SemanticSpace
  ](
      fit: ExactSpectralProgramFit[Feature, Component]
  ): Either[ModelLifecycleError, OperatorFitBundle] =
    for
      residual <- FitDiagnostic
        .from("spectral-residual", fit.programFit.identifiability.residual)
        .left
        .map(ModelLifecycleError.Multivar.apply)
      result <- OperatorFitBundle
        .from(fit.programFit, Vector.empty, Vector(residual), fit.provenance)
        .left
        .map(ModelLifecycleError.Multivar.apply)
    yield result

  def solve(
      compiled: ExactSpectralCompiledModel,
      payload: OperatorFitBundle,
      scope: TrainingScopeId
  ): Either[
    ModelLifecycleError,
    FittedModel[ExactSpectralFamily, ExactSpectralModelProgram, OperatorFitBundle]
  ] =
    val programFit = payload.programFit
    val achieved = programFit.achievedGuarantee
    val semanticBindings = achieved.semanticEvidence.bindings
    if programFit.program.valueIdentity != compiled.executionProgramIdentity then
      Left(
        ModelLifecycleError.BindingMismatch(
          "spectral payload execution program",
          compiled.executionProgramIdentity,
          programFit.program.valueIdentity
        )
      )
    else if achieved.claimClass != OptimizationClaimClass.ExactGlobal then
      Left(ModelLifecycleError.RequestedClaimMismatch(OptimizationClaimClass.ExactGlobal, achieved.claimClass))
    else
      val resultId = ResultId.from[ExactSpectralFamily](semanticBindings.result)
      val binding = FitBinding.from(
        compiled.program.id,
        compiled.id,
        compiled.executionProgramIdentity,
        semanticBindings.data,
        scope,
        resultId
      )
      val certificate = programFit.solverAttestation.certificate
      val receipt = ExactSpectralReceipt.from(
        binding,
        compiled.solverPlan,
        programFit.identifiability,
        certificate
      )
      for
        certificates <- NonEmptyCertificates.from(Vector(certificate))
        solver <- SolverEvidence.from(
          binding,
          compiled.solverPlan,
          receipt,
          achieved,
          certificates,
          payload.provenance
        )
        fitted <- FittedModel.from(
          compiled.program,
          compiled,
          payload,
          semanticBindings.result,
          binding,
          solver,
          scope,
          payload.provenance
        )
      yield fitted

  def adapt[
      Feature <: SemanticSpace,
      Component <: SemanticSpace
  ](
      fit: ExactSpectralProgramFit[Feature, Component]
  ): Either[
    ModelLifecycleError,
    FittedModel[ExactSpectralFamily, ExactSpectralModelProgram, OperatorFitBundle]
  ] =
    val method = fit.programFit.solverAttestation.certificate.context.method
    val dataIdentity = fit.programFit.achievedGuarantee.semanticEvidence.bindings.data
    for
      program <- declare(fit.requestedProgram)
      compiled <- compile(program, fit.loweredProgram, fit.proof, method)
      payload <- bundle(fit)
      fitted <- solve(compiled, payload, TrainingScopeId.standalone(dataIdentity))
    yield fitted

extension [Feature <: SemanticSpace, Component <: SemanticSpace](
    fit: ExactSpectralProgramFit[Feature, Component]
)
  def toLifecycleFit: Either[
    ModelLifecycleError,
    FittedModel[ExactSpectralFamily, ExactSpectralModelProgram, OperatorFitBundle]
  ] =
    ExactSpectralLifecycle.adapt(fit)
