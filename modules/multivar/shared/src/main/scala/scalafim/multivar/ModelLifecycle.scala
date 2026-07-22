package scalafim.multivar

enum ModelLifecycleError:
  case InvalidDefinition(detail: String)
  case ContractFamilyMismatch(expected: MathematicalModelFamily, actual: MathematicalModelFamily)
  case BindingMismatch(field: String, expected: ValueIdentity, actual: ValueIdentity)
  case RequestedClaimMismatch(expected: OptimizationClaimClass, actual: OptimizationClaimClass)
  case EmptyCertificates
  case Multivar(error: MultivarError)

  def message: String =
    this match
      case InvalidDefinition(detail) => detail
      case ContractFamilyMismatch(expected, actual) =>
        s"model contract family is $actual, expected $expected"
      case BindingMismatch(field, expected, actual) =>
        s"$field identity ${actual.stableKey} does not match ${expected.stableKey}"
      case RequestedClaimMismatch(expected, actual) =>
        s"achieved optimization claim is $actual, but the solver plan requested $expected"
      case EmptyCertificates => "solver evidence must retain at least one numerical certificate"
      case Multivar(error) => error.message

/** Runtime witness for an enum-case singleton used as a model-family type. */
trait ModelFamilyWitness[F <: MathematicalModelFamily]:
  def family: F

object ModelFamilyWitness:
  given anchorRegularizedFrame: ModelFamilyWitness[MathematicalModelFamily.AnchorRegularizedFrame.type] with
    def family: MathematicalModelFamily.AnchorRegularizedFrame.type =
      MathematicalModelFamily.AnchorRegularizedFrame

  given exactSpectralFrame: ModelFamilyWitness[MathematicalModelFamily.ExactSpectralFrame.type] with
    def family: MathematicalModelFamily.ExactSpectralFrame.type =
      MathematicalModelFamily.ExactSpectralFrame

  given jointSparseFunctionalFactorization
      : ModelFamilyWitness[MathematicalModelFamily.JointSparseFunctionalFactorization.type] with
    def family: MathematicalModelFamily.JointSparseFunctionalFactorization.type =
      MathematicalModelFamily.JointSparseFunctionalFactorization

  given generalizedLowRankModel: ModelFamilyWitness[MathematicalModelFamily.GeneralizedLowRankModel.type] with
    def family: MathematicalModelFamily.GeneralizedLowRankModel.type =
      MathematicalModelFamily.GeneralizedLowRankModel

  given convexifiedLowRankMatrix: ModelFamilyWitness[MathematicalModelFamily.ConvexifiedLowRankMatrix.type] with
    def family: MathematicalModelFamily.ConvexifiedLowRankMatrix.type =
      MathematicalModelFamily.ConvexifiedLowRankMatrix

  given structuredMultiblockFactorization
      : ModelFamilyWitness[MathematicalModelFamily.StructuredMultiblockFactorization.type] with
    def family: MathematicalModelFamily.StructuredMultiblockFactorization.type =
      MathematicalModelFamily.StructuredMultiblockFactorization

/** A mathematical contract whose runtime family has been checked against `F`. */
final class FamilyContract[F <: MathematicalModelFamily] private (
    val value: MathematicalModelContract,
    val family: F
)

object FamilyContract:
  def from[F <: MathematicalModelFamily](
      value: MathematicalModelContract
  )(using witness: ModelFamilyWitness[F]): Either[ModelLifecycleError, FamilyContract[F]] =
    if value.family == witness.family then Right(new FamilyContract(value, witness.family))
    else Left(ModelLifecycleError.ContractFamilyMismatch(witness.family, value.family))

final class ProgramId[F <: MathematicalModelFamily] private (val valueIdentity: ValueIdentity)

object ProgramId:
  private[multivar] def from[F <: MathematicalModelFamily](value: ValueIdentity): ProgramId[F] =
    new ProgramId(value)

final class CompiledId[F <: MathematicalModelFamily] private (val valueIdentity: ValueIdentity)

object CompiledId:
  private[multivar] def derive[F <: MathematicalModelFamily](
      program: ProgramId[F],
      inputs: Vector[ValueIdentity]
  ): CompiledId[F] =
    new CompiledId(ValueIdentity.derived("compiled-model", (program.valueIdentity +: inputs)*))

final class ResultId[F <: MathematicalModelFamily] private (val valueIdentity: ValueIdentity)

object ResultId:
  private[multivar] def from[F <: MathematicalModelFamily](value: ValueIdentity): ResultId[F] =
    new ResultId(value)

final class TrainingScopeId private (val valueIdentity: ValueIdentity)

object TrainingScopeId:
  def from(scope: TrainingScope): TrainingScopeId =
    new TrainingScopeId(scope.valueIdentity)

  def standalone(dataIdentity: ValueIdentity): TrainingScopeId =
    new TrainingScopeId(ValueIdentity.derived("standalone-training-scope", dataIdentity))

/** One exact binding shared by the compiled plan, receipt, guarantee, and fit. */
final class FitBinding[F <: MathematicalModelFamily] private (
    val program: ProgramId[F],
    val compiled: CompiledId[F],
    val compiledProgram: ValueIdentity,
    val data: ValueIdentity,
    val scope: TrainingScopeId,
    val result: ResultId[F]
)

object FitBinding:
  private[multivar] def from[F <: MathematicalModelFamily](
      program: ProgramId[F],
      compiled: CompiledId[F],
      compiledProgram: ValueIdentity,
      data: ValueIdentity,
      scope: TrainingScopeId,
      result: ResultId[F]
  ): FitBinding[F] =
    new FitBinding(program, compiled, compiledProgram, data, scope, result)

trait ModelCandidate:
  def valueIdentity: ValueIdentity

trait ModelProgram[F <: MathematicalModelFamily]:
  def family: F
  def id: ProgramId[F]
  def contract: FamilyContract[F]
  def requestedClaim: RequestedOptimizationClaim
  def provenance: SemanticProvenance

final class SolverPlan[F <: MathematicalModelFamily] private (
    val method: String,
    val requestedClaim: RequestedOptimizationClaim,
    val valueIdentity: ValueIdentity
)

object SolverPlan:
  private[multivar] def from[F <: MathematicalModelFamily](
      program: ProgramId[F],
      method: String,
      requestedClaim: RequestedOptimizationClaim
  ): Either[ModelLifecycleError, SolverPlan[F]] =
    val clean = method.trim
    if clean.isEmpty then Left(ModelLifecycleError.InvalidDefinition("solver plan method must be non-empty"))
    else
      Right(
        new SolverPlan(
          clean,
          requestedClaim,
          ValueIdentity.derived(s"solver-plan-$clean", program.valueIdentity)
        )
      )

trait CompiledModel[
    F <: MathematicalModelFamily,
    P <: ModelProgram[F]
]:
  def program: P
  def id: CompiledId[F]
  def executionProgramIdentity: ValueIdentity
  def solverPlan: SolverPlan[F]
  def provenance: SemanticProvenance

/** Associated types keep every family-specific stage in one workflow path. */
trait ModelWorkflow:
  type Family <: MathematicalModelFamily
  type TrainingData
  type Candidate <: ModelCandidate
  type Program <: ModelProgram[Family]
  type Compiled <: CompiledModel[Family, Program]
  type FitPayload
  type ApplicationInput
  type Application

  def declare(training: TrainingData, candidate: Candidate): Either[ModelLifecycleError, Program]
  def compile(program: Program): Either[ModelLifecycleError, Compiled]
  def solve(
      training: TrainingData,
      scope: TrainingScopeId,
      compiled: Compiled
  ): Either[ModelLifecycleError, FittedModel[Family, Program, FitPayload]]
  def apply(
      fitted: FittedModel[Family, Program, FitPayload],
      input: ApplicationInput
  ): Either[ModelLifecycleError, Application]

final class NonEmptyCertificates private (
    val head: NumericalCertificate,
    val tail: Vector[NumericalCertificate]
):
  def values: Vector[NumericalCertificate] = head +: tail

object NonEmptyCertificates:
  def from(values: Vector[NumericalCertificate]): Either[ModelLifecycleError, NonEmptyCertificates] =
    values.headOption match
      case None => Left(ModelLifecycleError.EmptyCertificates)
      case Some(head) => Right(new NonEmptyCertificates(head, values.tail))

sealed trait SolverReceipt[F <: MathematicalModelFamily]:
  def program: ProgramId[F]
  def compiled: CompiledId[F]
  def compiledProgram: ValueIdentity
  def planIdentity: ValueIdentity
  def data: ValueIdentity
  def result: ResultId[F]
  def certificateIdentities: Vector[ValueIdentity]

final class SolverEvidence[F <: MathematicalModelFamily] private (
    val binding: FitBinding[F],
    val plan: SolverPlan[F],
    val receipt: SolverReceipt[F],
    val achieved: AchievedOptimizationGuarantee,
    val certificates: NonEmptyCertificates,
    val provenance: SemanticProvenance
)

object SolverEvidence:
  private[multivar] def from[F <: MathematicalModelFamily](
      binding: FitBinding[F],
      plan: SolverPlan[F],
      receipt: SolverReceipt[F],
      achieved: AchievedOptimizationGuarantee,
      certificates: NonEmptyCertificates,
      provenance: SemanticProvenance
  ): Either[ModelLifecycleError, SolverEvidence[F]] =
    val evidence = achieved.semanticEvidence
    val retainedCertificates = certificates.values
    if receipt.program.valueIdentity != binding.program.valueIdentity then
      Left(ModelLifecycleError.BindingMismatch("receipt program", binding.program.valueIdentity, receipt.program.valueIdentity))
    else if receipt.compiled.valueIdentity != binding.compiled.valueIdentity then
      Left(ModelLifecycleError.BindingMismatch("receipt compiled plan", binding.compiled.valueIdentity, receipt.compiled.valueIdentity))
    else if receipt.compiledProgram != binding.compiledProgram then
      Left(ModelLifecycleError.BindingMismatch("receipt execution program", binding.compiledProgram, receipt.compiledProgram))
    else if receipt.planIdentity != plan.valueIdentity then
      Left(ModelLifecycleError.BindingMismatch("receipt solver plan", plan.valueIdentity, receipt.planIdentity))
    else if receipt.data != binding.data then
      Left(ModelLifecycleError.BindingMismatch("receipt data", binding.data, receipt.data))
    else if receipt.result.valueIdentity != binding.result.valueIdentity then
      Left(ModelLifecycleError.BindingMismatch("receipt result", binding.result.valueIdentity, receipt.result.valueIdentity))
    else if receipt.certificateIdentities != retainedCertificates.map(_.valueIdentity) then
      Left(ModelLifecycleError.InvalidDefinition("solver receipt and retained certificate identities differ"))
    else if evidence.bindings.program != binding.compiledProgram then
      Left(ModelLifecycleError.BindingMismatch("achieved execution program", binding.compiledProgram, evidence.bindings.program))
    else if evidence.bindings.data != binding.data then
      Left(ModelLifecycleError.BindingMismatch("achieved data", binding.data, evidence.bindings.data))
    else if evidence.bindings.result != binding.result.valueIdentity then
      Left(ModelLifecycleError.BindingMismatch("achieved result", binding.result.valueIdentity, evidence.bindings.result))
    else if !evidence.numericalCertificates.forall(retainedCertificates.contains) then
      Left(ModelLifecycleError.InvalidDefinition("achieved guarantee references a certificate absent from solver evidence"))
    else if achieved.claimClass != OptimizationClaimClass.Unresolved &&
        achieved.claimClass != plan.requestedClaim.claimClass
    then Left(ModelLifecycleError.RequestedClaimMismatch(plan.requestedClaim.claimClass, achieved.claimClass))
    else Right(new SolverEvidence(binding, plan, receipt, achieved, certificates, provenance))

final class FittedModel[
    F <: MathematicalModelFamily,
    P <: ModelProgram[F],
    R
] private (
    val program: P,
    val payload: R,
    val binding: FitBinding[F],
    val solver: SolverEvidence[F],
    val scope: TrainingScopeId,
    val provenance: SemanticProvenance
)

object FittedModel:
  private[multivar] def from[
      F <: MathematicalModelFamily,
      P <: ModelProgram[F],
      R
  ](
      program: P,
      compiled: CompiledModel[F, P],
      payload: R,
      payloadResultIdentity: ValueIdentity,
      binding: FitBinding[F],
      solver: SolverEvidence[F],
      scope: TrainingScopeId,
      provenance: SemanticProvenance
  ): Either[ModelLifecycleError, FittedModel[F, P, R]] =
    if compiled.program.id.valueIdentity != program.id.valueIdentity then
      Left(ModelLifecycleError.BindingMismatch("compiled program", program.id.valueIdentity, compiled.program.id.valueIdentity))
    else if binding.program.valueIdentity != program.id.valueIdentity then
      Left(ModelLifecycleError.BindingMismatch("fit program", program.id.valueIdentity, binding.program.valueIdentity))
    else if binding.compiled.valueIdentity != compiled.id.valueIdentity then
      Left(ModelLifecycleError.BindingMismatch("fit compiled plan", compiled.id.valueIdentity, binding.compiled.valueIdentity))
    else if binding.compiledProgram != compiled.executionProgramIdentity then
      Left(
        ModelLifecycleError.BindingMismatch(
          "fit execution program",
          compiled.executionProgramIdentity,
          binding.compiledProgram
        )
      )
    else if binding.result.valueIdentity != payloadResultIdentity then
      Left(ModelLifecycleError.BindingMismatch("fit payload result", binding.result.valueIdentity, payloadResultIdentity))
    else if binding.scope.valueIdentity != scope.valueIdentity then
      Left(ModelLifecycleError.BindingMismatch("fit training scope", binding.scope.valueIdentity, scope.valueIdentity))
    else if solver.binding ne binding then
      Left(ModelLifecycleError.InvalidDefinition("fitted model must retain the exact binding admitted by solver evidence"))
    else if solver.plan.valueIdentity != compiled.solverPlan.valueIdentity then
      Left(
        ModelLifecycleError.BindingMismatch(
          "fit solver plan",
          compiled.solverPlan.valueIdentity,
          solver.plan.valueIdentity
        )
      )
    else if solver.achieved.semanticEvidence.bindings.contract != program.contract.value.id then
      Left(ModelLifecycleError.InvalidDefinition("achieved guarantee uses a foreign mathematical contract"))
    else Right(new FittedModel(program, payload, binding, solver, scope, provenance))

trait CanTransform[-Fit, -Input]:
  type Output
  def transform(fit: Fit, input: Input): Either[ModelLifecycleError, Output]

object CanTransform:
  type Aux[Fit, Input, Result] = CanTransform[Fit, Input] { type Output = Result }

  extension [Fit](fit: Fit)
    def transformWith[Input](input: Input)(using capability: CanTransform[Fit, Input]):
        Either[ModelLifecycleError, capability.Output] =
      capability.transform(fit, input)

trait CanEncode[-Fit, -Input]:
  type Output
  def encode(fit: Fit, input: Input): Either[ModelLifecycleError, Output]

object CanEncode:
  type Aux[Fit, Input, Result] = CanEncode[Fit, Input] { type Output = Result }

  extension [Fit](fit: Fit)
    def encodeWith[Input](input: Input)(using capability: CanEncode[Fit, Input]):
        Either[ModelLifecycleError, capability.Output] =
      capability.encode(fit, input)

trait CanReconstruct[-Fit, -Input]:
  type Output
  def reconstruct(fit: Fit, input: Input): Either[ModelLifecycleError, Output]

object CanReconstruct:
  type Aux[Fit, Input, Result] = CanReconstruct[Fit, Input] { type Output = Result }

  extension [Fit](fit: Fit)
    def reconstructWith[Input](input: Input)(using capability: CanReconstruct[Fit, Input]):
        Either[ModelLifecycleError, capability.Output] =
      capability.reconstruct(fit, input)

type ExactSpectralFamily = MathematicalModelFamily.ExactSpectralFrame.type

final class ExactSpectralReceipt private (
    val program: ProgramId[ExactSpectralFamily],
    val compiled: CompiledId[ExactSpectralFamily],
    val compiledProgram: ValueIdentity,
    val planIdentity: ValueIdentity,
    val data: ValueIdentity,
    val result: ResultId[ExactSpectralFamily],
    val certificateIdentities: Vector[ValueIdentity],
    val retainedRank: Int,
    val residual: Double,
    val spectralClusters: Vector[Vector[Int]]
) extends SolverReceipt[ExactSpectralFamily]

object ExactSpectralReceipt:
  private[multivar] def from(
      binding: FitBinding[ExactSpectralFamily],
      plan: SolverPlan[ExactSpectralFamily],
      identifiability: NumericalIdentifiability,
      certificate: NumericalCertificate
  ): ExactSpectralReceipt =
    new ExactSpectralReceipt(
      binding.program,
      binding.compiled,
      binding.compiledProgram,
      plan.valueIdentity,
      binding.data,
      binding.result,
      Vector(certificate.valueIdentity),
      identifiability.retainedRank,
      identifiability.residual,
      identifiability.spectralClusters
    )
