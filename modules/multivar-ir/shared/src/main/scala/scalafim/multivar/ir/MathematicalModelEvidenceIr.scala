package scalafim.multivar.ir

import scalafim.multivar.core.*
import scalafim.multivar.contract.*
import scalafim.multivar.optimization.*
import scalafim.multivar.solver.*
import scalafim.multivar.lifecycle.*
import scalafim.multivar.capability.*
import scalafim.multivar.family.spectral.*
import scalafim.multivar.family.paired.*
import scalafim.multivar.family.canonical.*
import scalafim.multivar.family.cpca.*
import scalafim.multivar.family.sparse.*
import scalafim.multivar.family.glrm.*
import scalafim.multivar.family.multiblock.*
import scalafim.multivar.family.kernel.*
import scalafim.multivar.workflow.*
import scalafim.multivar.validation.*

/** Companion evidence envelope for an extant operator-program document.
  *
  * This IR does not re-encode operators or parameters. It binds their stable
  * identities to the mathematical estimand, theorem assumptions, achieved
  * guarantee, and reproducibility receipt needed for external review.
  */
enum ModelFamilyEvidenceIr:
  case AnchorRegularizedFrame
  case ExactSpectralFrame
  case JointSparseFunctionalFactorization
  case GeneralizedLowRankModel
  case ConvexifiedLowRankMatrix
  case StructuredMultiblockFactorization

  def contract: MathematicalModelContract =
    this match
      case AnchorRegularizedFrame => MathematicalContractCatalog.anchorRegularizedFrame
      case ExactSpectralFrame => MathematicalContractCatalog.exactSpectralFrame
      case JointSparseFunctionalFactorization => MathematicalContractCatalog.jointSparseFunctionalFactorization
      case GeneralizedLowRankModel => MathematicalContractCatalog.generalizedLowRankModel
      case ConvexifiedLowRankMatrix => MathematicalContractCatalog.convexifiedLowRankMatrix
      case StructuredMultiblockFactorization => MathematicalContractCatalog.structuredMultiblockFactorization

enum ModelEstimandEvidenceIr:
  case AnchorCoefficientRefinement
  case GeneralizedSpectralSubspace
  case JointStructuredFactors
  case GeneralizedLatentRepresentation
  case ConvexLowRankMatrix
  case SharedBlockLatentRepresentation

  def family: ModelFamilyEvidenceIr =
    this match
      case AnchorCoefficientRefinement => ModelFamilyEvidenceIr.AnchorRegularizedFrame
      case GeneralizedSpectralSubspace => ModelFamilyEvidenceIr.ExactSpectralFrame
      case JointStructuredFactors => ModelFamilyEvidenceIr.JointSparseFunctionalFactorization
      case GeneralizedLatentRepresentation => ModelFamilyEvidenceIr.GeneralizedLowRankModel
      case ConvexLowRankMatrix => ModelFamilyEvidenceIr.ConvexifiedLowRankMatrix
      case SharedBlockLatentRepresentation => ModelFamilyEvidenceIr.StructuredMultiblockFactorization

enum EntryLossEvidenceIr:
  case HalfSquared
  case Huber(delta: Double)
  case BernoulliLogistic
  case PoissonLogLink
  case CumulativeOrdinal(levels: Int, orderIdentity: String)
  case Softmax(levels: Int)

final case class LossBindingEvidenceIr(
    featureDomainIdentity: String,
    loss: EntryLossEvidenceIr
)

enum MissingnessTargetEvidenceIr:
  case FixedMask
  case McarSimulation(generatorIdentity: String)
  case MarSensitivity(mechanismIdentity: String)
  case MnarSensitivity(selectionModelIdentity: String)

enum ObservationMaskEvidenceIr:
  case Complete(observationIdentity: String)
  case Explicit(
      maskIdentity: String,
      observedCount: Int,
      target: MissingnessTargetEvidenceIr
  )
  case Censored(
      maskIdentity: String,
      observedCount: Int,
      likelihoodIdentity: String,
      target: MissingnessTargetEvidenceIr
  )

enum GeometryRoleEvidenceIr:
  case RowNormalization
  case ColumnNormalization
  case Smoothness
  case Graph
  case BlockAlignment

final case class GeometryBindingEvidenceIr(
    role: GeometryRoleEvidenceIr,
    operatorIdentity: String,
    certificateIdentity: String
)

enum PenaltyOwnerEvidenceIr:
  case RowFactor
  case ColumnFactor
  case SharedRows
  case BlockDecoder(blockIdentity: String)
  case ConvexMatrix

/** Evidence uses the runtime's canonical mathematical identity directly.
  * Family-specific targets and executable capabilities remain in `owner`, the
  * optional operator identity, and the runtime witness that produced this value.
  */
type PenaltyFunctionalEvidenceIr = PenaltyFunctionalIdentity

object PenaltyFunctionalEvidenceIr:
  def from(witness: PenaltyFunctionalWitness): PenaltyFunctionalEvidenceIr =
    witness.functionalIdentity

  def fromStableKey(value: String): Option[PenaltyFunctionalEvidenceIr] =
    PenaltyFunctionalIdentity.fromStableKey(value)

final case class PenaltyBindingEvidenceIr(
    owner: PenaltyOwnerEvidenceIr,
    functional: PenaltyFunctionalEvidenceIr,
    weight: Double,
    operatorIdentity: Option[String]
)

final case class TheoremAssumptionEvidenceIr(
    theoremId: String,
    assumptionId: String,
    witnessIdentity: String
)

enum SolverFamilyEvidenceIr:
  case ExactSpectral
  case ConvexComposite
  case AlternatingBlockCoordinate
  case Palm
  case ConvexNuclear

final case class SolverReceiptEvidenceIr(
    family: SolverFamilyEvidenceIr,
    implementationVersion: String,
    policyIdentity: String,
    traceIdentity: String,
    tolerance: ToleranceIr,
    iterationCount: Int
)

enum AchievedGuaranteeEvidenceIr:
  case ExactGlobal(certificateIdentity: String)
  case EpsilonGlobal(gap: Double, certificateIdentity: String)
  case UniqueMinimizer(distanceBound: Double, certificateIdentity: String)
  case Stationary(residual: Double, certificateIdentity: String)
  case CoordinatewiseStationary(residuals: Vector[Double], certificateIdentity: String)
  case Feasible(residual: Double, certificateIdentity: String)
  case Unresolved(reason: String)

  def claim: OptimizationClaimClass =
    this match
      case ExactGlobal(_) => OptimizationClaimClass.ExactGlobal
      case EpsilonGlobal(_, _) => OptimizationClaimClass.EpsilonGlobal
      case UniqueMinimizer(_, _) => OptimizationClaimClass.UniqueMinimizerWithinBound
      case Stationary(_, _) => OptimizationClaimClass.Stationary
      case CoordinatewiseStationary(_, _) => OptimizationClaimClass.CoordinatewiseStationary
      case Feasible(_, _) => OptimizationClaimClass.Feasible
      case Unresolved(_) => OptimizationClaimClass.Unresolved

  def certificateReference: Option[String] =
    this match
      case ExactGlobal(identity) => Some(identity)
      case EpsilonGlobal(_, identity) => Some(identity)
      case UniqueMinimizer(_, identity) => Some(identity)
      case Stationary(_, identity) => Some(identity)
      case CoordinatewiseStationary(_, identity) => Some(identity)
      case Feasible(_, identity) => Some(identity)
      case Unresolved(_) => None

final case class DependencyVersionEvidenceIr(name: String, version: String)

final case class ReproducibilityReceiptIr(
    generatorIdentity: String,
    seed: Long,
    dependencies: Vector[DependencyVersionEvidenceIr],
    conditionEstimate: Double,
    tolerance: ToleranceIr,
    resultIdentity: String
)

final case class MathematicalModelEvidenceIr(
    id: String,
    contractId: String,
    family: ModelFamilyEvidenceIr,
    estimand: ModelEstimandEvidenceIr,
    operatorProgramSchema: String,
    programId: String,
    dataIdentity: String,
    mask: ObservationMaskEvidenceIr,
    losses: Vector[LossBindingEvidenceIr],
    geometries: Vector[GeometryBindingEvidenceIr],
    penalties: Vector[PenaltyBindingEvidenceIr],
    assumptions: Vector[TheoremAssumptionEvidenceIr],
    solver: SolverReceiptEvidenceIr,
    achievedGuarantee: AchievedGuaranteeEvidenceIr,
    certificateIdentities: Vector[String],
    reproducibility: ReproducibilityReceiptIr
)

final case class MathematicalModelEvidenceDocumentIr(
    schema: String,
    models: Vector[MathematicalModelEvidenceIr]
)

object MathematicalModelEvidenceDocumentIr:
  val schemaV20: String = "scalafim-mathematical-model-evidence-ir/2.0"

object MathematicalModelEvidenceIrValidator:
  def validate(
      document: MathematicalModelEvidenceDocumentIr
  ): Either[IrError, MathematicalModelEvidenceDocumentIr] =
    for
      _ <- requireValue(
        document.schema == MathematicalModelEvidenceDocumentIr.schemaV20,
        RejectionCategory.SchemaVersionMismatch,
        "$.schema",
        s"expected ${MathematicalModelEvidenceDocumentIr.schemaV20}, got ${document.schema}"
      )
      models <- unique(document.models, _.id, "$.models")
      _ <- requireValue(models.nonEmpty, RejectionCategory.Malformed, "$.models", "at least one model evidence record is required")
      _ <- document.models.foldLeft[Either[IrError, Unit]](Right(())): (result, model) =>
        result.flatMap(_ => validateModel(model))
    yield document

  private def validateModel(model: MathematicalModelEvidenceIr): Either[IrError, Unit] =
    val path = s"$$.models.${model.id}"
    val contract = model.family.contract
    for
      _ <- nonEmpty(model.id, s"$path.id")
      _ <- requireValue(
        model.estimand.family == model.family,
        RejectionCategory.Malformed,
        s"$path.estimand",
        s"${model.estimand} belongs to ${model.estimand.family}, not ${model.family}"
      )
      _ <- requireValue(
        model.contractId == contract.id.value,
        RejectionCategory.Malformed,
        s"$path.contract_id",
        s"expected contract ${contract.id.value} for ${model.family}"
      )
      _ <- requireValue(
        model.operatorProgramSchema == OperatorProgramDocumentIr.schemaV02,
        RejectionCategory.SchemaVersionMismatch,
        s"$path.operator_program_schema",
        s"expected ${OperatorProgramDocumentIr.schemaV02}"
      )
      _ <- nonEmpty(model.programId, s"$path.program_id")
      _ <- nonEmpty(model.dataIdentity, s"$path.data_identity")
      _ <- validateMask(model.mask, s"$path.mask")
      _ <- validateLosses(model, path)
      _ <- validateGeometries(model.geometries, s"$path.geometries")
      _ <- validatePenalties(model.penalties, s"$path.penalties")
      _ <- validateSolver(model.solver, s"$path.solver")
      _ <- validateGuarantee(model, contract, path)
      _ <- validateReproducibility(model.reproducibility, s"$path.reproducibility")
    yield ()

  private def validateMask(mask: ObservationMaskEvidenceIr, path: String): Either[IrError, Unit] =
    mask match
      case ObservationMaskEvidenceIr.Complete(identity) => nonEmpty(identity, s"$path.observation_identity")
      case ObservationMaskEvidenceIr.Explicit(identity, count, target) =>
        for
          _ <- nonEmpty(identity, s"$path.mask_identity")
          _ <- positive(count, s"$path.observed_count")
          _ <- validateMissingness(target, path)
        yield ()
      case ObservationMaskEvidenceIr.Censored(identity, count, likelihood, target) =>
        for
          _ <- nonEmpty(identity, s"$path.mask_identity")
          _ <- positive(count, s"$path.observed_count")
          _ <- nonEmpty(likelihood, s"$path.likelihood_identity")
          _ <- validateMissingness(target, path)
        yield ()

  private def validateMissingness(target: MissingnessTargetEvidenceIr, path: String): Either[IrError, Unit] =
    target match
      case MissingnessTargetEvidenceIr.FixedMask => Right(())
      case MissingnessTargetEvidenceIr.McarSimulation(identity) => nonEmpty(identity, s"$path.generator_identity")
      case MissingnessTargetEvidenceIr.MarSensitivity(identity) => nonEmpty(identity, s"$path.mechanism_identity")
      case MissingnessTargetEvidenceIr.MnarSensitivity(identity) => nonEmpty(identity, s"$path.selection_model_identity")

  private def validateLosses(model: MathematicalModelEvidenceIr, path: String): Either[IrError, Unit] =
    val required =
      model.family == ModelFamilyEvidenceIr.GeneralizedLowRankModel ||
        model.family == ModelFamilyEvidenceIr.StructuredMultiblockFactorization
    for
      _ <- requireValue(
        !required || model.losses.nonEmpty,
        RejectionCategory.Malformed,
        s"$path.losses",
        s"${model.family} requires explicit entry losses"
      )
      _ <- unique(model.losses, _.featureDomainIdentity, s"$path.losses").map(_ => ())
      _ <- model.losses.foldLeft[Either[IrError, Unit]](Right(())): (result, binding) =>
        result.flatMap: _ =>
          nonEmpty(binding.featureDomainIdentity, s"$path.losses.feature_domain_identity")
            .flatMap(_ => validateLoss(binding.loss, s"$path.losses.${binding.featureDomainIdentity}"))
    yield ()

  private def validateLoss(loss: EntryLossEvidenceIr, path: String): Either[IrError, Unit] =
    loss match
      case EntryLossEvidenceIr.Huber(delta) => finitePositive(delta, s"$path.delta")
      case EntryLossEvidenceIr.CumulativeOrdinal(levels, order) =>
        positive(levels - 1, s"$path.levels").flatMap(_ => nonEmpty(order, s"$path.order_identity"))
      case EntryLossEvidenceIr.Softmax(levels) => positive(levels - 1, s"$path.levels")
      case _ => Right(())

  private def validateGeometries(values: Vector[GeometryBindingEvidenceIr], path: String): Either[IrError, Unit] =
    for
      _ <- requireValue(values.nonEmpty, RejectionCategory.Malformed, path, "at least one geometry is required")
      _ <- unique(values, _.operatorIdentity, path).map(_ => ())
      _ <- values.foldLeft[Either[IrError, Unit]](Right(())): (result, geometry) =>
        result.flatMap: _ =>
          nonEmpty(geometry.operatorIdentity, s"$path.operator_identity")
            .flatMap(_ => nonEmpty(geometry.certificateIdentity, s"$path.certificate_identity"))
    yield ()

  private def validatePenalties(values: Vector[PenaltyBindingEvidenceIr], path: String): Either[IrError, Unit] =
    values.foldLeft[Either[IrError, Unit]](Right(())): (result, penalty) =>
      result.flatMap: _ =>
        finitePositive(penalty.weight, s"$path.weight").flatMap: _ =>
          penalty.operatorIdentity.fold[Either[IrError, Unit]](Right(()))(nonEmpty(_, s"$path.operator_identity"))

  private def validateSolver(value: SolverReceiptEvidenceIr, path: String): Either[IrError, Unit] =
    for
      _ <- nonEmpty(value.implementationVersion, s"$path.implementation_version")
      _ <- nonEmpty(value.policyIdentity, s"$path.policy_identity")
      _ <- nonEmpty(value.traceIdentity, s"$path.trace_identity")
      _ <- tolerance(value.tolerance, s"$path.tolerance")
      _ <- requireValue(value.iterationCount >= 0, RejectionCategory.Malformed, s"$path.iteration_count", "must be non-negative")
    yield ()

  private def validateGuarantee(
      model: MathematicalModelEvidenceIr,
      contract: MathematicalModelContract,
      path: String
  ): Either[IrError, Unit] =
    val achieved = model.achievedGuarantee
    val certificate = achieved.certificateReference
    val witnesses = model.assumptions.groupBy(_.theoremId)
    for
      _ <- requireValue(
        contract.admissibleClaims.contains(achieved.claim),
        RejectionCategory.Malformed,
        s"$path.achieved_guarantee",
        s"${achieved.claim} is not admitted by ${contract.family}"
      )
      _ <- validateQuantitativeGuarantee(achieved, s"$path.achieved_guarantee")
      _ <- requireValue(
        certificate.forall(model.certificateIdentities.contains),
        RejectionCategory.Malformed,
        s"$path.certificate_identities",
        "achieved guarantee certificate is absent from certificate identities"
      )
      _ <- model.certificateIdentities.foldLeft[Either[IrError, Unit]](Right(()))((result, id) => result.flatMap(_ => nonEmpty(id, s"$path.certificate_identities")))
      _ <- validateAssumptionBindings(model.assumptions, contract, s"$path.assumptions")
      _ <-
        if achieved.claim == OptimizationClaimClass.Feasible || achieved.claim == OptimizationClaimClass.Unresolved then Right(())
        else
          val supported = contract.theorems.filter(_.supportedClaims.contains(achieved.claim))
          val complete = supported.exists: theorem =>
            val supplied = witnesses.getOrElse(theorem.id.value, Vector.empty).map(_.assumptionId).toSet
            theorem.assumptions.map(_.value).toSet.subsetOf(supplied)
          requireValue(
            complete,
            RejectionCategory.Malformed,
            s"$path.assumptions",
            s"${achieved.claim} requires every assumption of one supporting theorem"
          )
    yield ()

  private def validateQuantitativeGuarantee(
      achieved: AchievedGuaranteeEvidenceIr,
      path: String
  ): Either[IrError, Unit] =
    achieved match
      case AchievedGuaranteeEvidenceIr.ExactGlobal(identity) => nonEmpty(identity, s"$path.certificate_identity")
      case AchievedGuaranteeEvidenceIr.EpsilonGlobal(gap, identity) =>
        finiteNonNegative(gap, s"$path.gap").flatMap(_ => nonEmpty(identity, s"$path.certificate_identity"))
      case AchievedGuaranteeEvidenceIr.UniqueMinimizer(distance, identity) =>
        finiteNonNegative(distance, s"$path.distance_bound").flatMap(_ => nonEmpty(identity, s"$path.certificate_identity"))
      case AchievedGuaranteeEvidenceIr.Stationary(residual, identity) =>
        finiteNonNegative(residual, s"$path.residual").flatMap(_ => nonEmpty(identity, s"$path.certificate_identity"))
      case AchievedGuaranteeEvidenceIr.CoordinatewiseStationary(residuals, identity) =>
        requireValue(residuals.nonEmpty, RejectionCategory.Malformed, s"$path.residuals", "must be non-empty")
          .flatMap: _ =>
            residuals.foldLeft[Either[IrError, Unit]](Right(()))((result, residual) => result.flatMap(_ => finiteNonNegative(residual, s"$path.residuals")))
          .flatMap(_ => nonEmpty(identity, s"$path.certificate_identity"))
      case AchievedGuaranteeEvidenceIr.Feasible(residual, identity) =>
        finiteNonNegative(residual, s"$path.residual").flatMap(_ => nonEmpty(identity, s"$path.certificate_identity"))
      case AchievedGuaranteeEvidenceIr.Unresolved(reason) => nonEmpty(reason, s"$path.reason")

  private def validateAssumptionBindings(
      values: Vector[TheoremAssumptionEvidenceIr],
      contract: MathematicalModelContract,
      path: String
  ): Either[IrError, Unit] =
    val declared = contract.theorems.flatMap: theorem =>
      theorem.assumptions.map(assumption => (theorem.id.value, assumption.value))
    values.foldLeft[Either[IrError, Unit]](Right(())): (result, evidence) =>
      result.flatMap: _ =>
        for
          _ <- nonEmpty(evidence.witnessIdentity, s"$path.witness_identity")
          _ <- requireValue(
            declared.contains((evidence.theoremId, evidence.assumptionId)),
            RejectionCategory.Malformed,
            path,
            s"undeclared theorem assumption ${evidence.theoremId}/${evidence.assumptionId}"
          )
        yield ()

  private def validateReproducibility(value: ReproducibilityReceiptIr, path: String): Either[IrError, Unit] =
    for
      _ <- nonEmpty(value.generatorIdentity, s"$path.generator_identity")
      _ <- requireValue(
        value.seed >= 0L && value.seed <= 9007199254740991L,
        RejectionCategory.Malformed,
        s"$path.seed",
        "must be non-negative and exactly representable in JSON"
      )
      dependencies <- unique(value.dependencies, _.name, s"$path.dependencies")
      _ <- requireValue(dependencies.nonEmpty, RejectionCategory.Malformed, s"$path.dependencies", "must be non-empty")
      _ <- value.dependencies.foldLeft[Either[IrError, Unit]](Right(())): (result, dependency) =>
        result.flatMap(_ => nonEmpty(dependency.name, s"$path.dependencies.name"))
          .flatMap(_ => nonEmpty(dependency.version, s"$path.dependencies.version"))
      _ <- requireValue(
        value.conditionEstimate.isFinite && value.conditionEstimate >= 1.0,
        RejectionCategory.Malformed,
        s"$path.condition_estimate",
        "must be finite and at least one"
      )
      _ <- tolerance(value.tolerance, s"$path.tolerance")
      _ <- nonEmpty(value.resultIdentity, s"$path.result_identity")
    yield ()

  private def tolerance(value: ToleranceIr, path: String): Either[IrError, Unit] =
    requireValue(
      value.absolute.isFinite && value.absolute >= 0.0 && value.relative.isFinite && value.relative >= 0.0,
      RejectionCategory.Malformed,
      path,
      "absolute and relative tolerances must be finite and non-negative"
    )

  private def nonEmpty(value: String, path: String): Either[IrError, Unit] =
    requireValue(value.trim.nonEmpty, RejectionCategory.Malformed, path, "must be non-empty")

  private def positive(value: Int, path: String): Either[IrError, Unit] =
    requireValue(value > 0, RejectionCategory.Malformed, path, "must be positive")

  private def finitePositive(value: Double, path: String): Either[IrError, Unit] =
    requireValue(value.isFinite && value > 0.0, RejectionCategory.Malformed, path, "must be finite and positive")

  private def finiteNonNegative(value: Double, path: String): Either[IrError, Unit] =
    requireValue(value.isFinite && value >= 0.0, RejectionCategory.Malformed, path, "must be finite and non-negative")

  private def unique[A](values: Vector[A], key: A => String, path: String): Either[IrError, Map[String, A]] =
    val result = scala.collection.mutable.LinkedHashMap.empty[String, A]
    val checked = values.foldLeft[Either[IrError, Unit]](Right(())): (state, value) =>
      state.flatMap: _ =>
        val id = key(value)
        if result.contains(id) then Left(IrError(RejectionCategory.Malformed, path, s"duplicate identity '$id'"))
        else
          result.update(id, value)
          Right(())
    checked.map(_ => result.toMap)

  private def requireValue(
      condition: Boolean,
      category: RejectionCategory,
      path: String,
      detail: String
  ): Either[IrError, Unit] =
    if condition then Right(()) else Left(IrError(category, path, detail))
